# Import Data
data_chatter <- read.csv(file.choose(),header = TRUE)
head(data_chatter,5)


# Explore Data
str(data_chatter)
data_chatter$Resolved <- as.factor(data_chatter$Resolved)
summary(data_chatter)
cor(data_chatter[1:7])


# Visualize
library(psych)
pairs.panels(data_chatter, # data contains only numeric values
                             gap=0)
par(mfrow = c(3,3), mar = c(4,4,1,1))
for (i in colnames(data_chatter[,1:7])){
     plot(data_chatter[[i]]~data_chatter[["Resolved"]],col = c("blue","red"),ylab=i,xlab="Resolved",main=i)
}
plot(data_chatter$Resolved,col = c("blue","red"))

par(mfrow = c(3,3), mar = c(4,4,1,1))
for (i in colnames(data_chatter[,1:7])){
     plot(density(data_chatter[[i]]),main=i)
}
par(mfrow = c(1,1), mar = c(4,4,1,1))


#install.packages("tree")
library(tree)
library(caret)
library(randomForest)
set.seed(600)
split <- sample(nrow(data_chatter),nrow(data_chatter)*0.8)
train <- data_chatter[split,]
test <- data_chatter[-split,]


# Tree model
set.seed(600)
tree_mod <- tree(Resolved~.,train)
plot(tree_mod);text(tree_mod,pretty=0)
summary(tree_mod)

#Prediction (Unpruned)
tree_pred <- predict(tree_mod,test[,-grep("Resolved",colnames(data_chatter))],type="class")
cm1 = confusionMatrix(table(tree_pred,test$Resolved))
misclas_err_unp <- mean(tree_pred != test$Resolved)
cm1$overall["Accuracy"]

# Improve Prediction (Prune model)
set.seed(600)
cv_tree_mod <- cv.tree(tree_mod,FUN=prune.misclass)
cv_tree_mod
plot(cv_tree_mod$size,cv_tree_mod$dev,type="b",xlab="Size",ylab="Deviance")
prune_data <- prune.misclass(tree_mod,best=8)
summary(prune_data)
plot(prune_data);text(prune_data,pretty=0)

# prediction (pruned)
set.seed(600)
tree_pred <- predict(prune_data,test,type="class")
cm2 <- confusionMatrix(table(tree_pred,test$Resolved))
misclas_err_pr <- mean(tree_pred != test$Resolved)
cm2$overall["Accuracy"]


# Random Forest
control <- trainControl(method = "cv",
                        number = 10,
                        search = "random")

set.seed(600)
rf <- train(Resolved~.,data = train,
            method = "rf",
            trControl= control,tuneLength=7)

rf
rf$bestTune
plot(rf)

set.seed(600)
randf_mod = randomForest(Resolved~.,data = train,mtry = 2,importance=TRUE)
randf_mod

# prediction
rf_pred=predict(randf_mod,test)
cm3 <- confusionMatrix(rf_pred,test$Resolved)
misclas_err_rf <- mean(rf_pred != test$Resolved)
cm3$overall["Accuracy"]


# SVM model
library(e1071)
set.seed(600)
svm_untuned <-svm(Resolved~.,train,kernel="linear")
summary(svm_untuned)
untuned_pred <- predict(svm_untuned,test)
cm4 <- confusionMatrix(untuned_pred,test$Resolved)
cm4$overall["Accuracy"]

# Tuning
set.seed(600)
tune_mod <- tune(svm,Resolved~.,data=train,
                 ranges = list(cost=seq(0.1,2,0.1)),kernel="linear")
plot(tune_mod)
tune_mod

# Best model
set.seed(600)
svm_tuned <- tune_mod$best.model
summary(svm_tuned)

# tuned prediction
tuned_pred <- predict(svm_tuned,test)
cm5 <- confusionMatrix(tuned_pred,test$Resolved)
cm5$overall["Accuracy"]


data.frame(Accuracy = c(unprunnedTree=cm1$overall["Accuracy"],
                        prunedTree=cm2$overall["Accuracy"],
                        randomForest=cm3$overall["Accuracy"],
                        svm_untuned=cm4$overall["Accuracy"],
                        SVM_tuned=cm5$overall["Accuracy"]))

