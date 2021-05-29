#l LOGISTIC MODEL

# Import Data
datan <- read.csv(file.choose(),header = TRUE)
head(datan,5)

# Explore Data
str(datan) # get the structure of the dataset
datan$survive <- as.factor(datan$survive) # change data class to factor for categoricat description
summary(datan) # get summary of all column in dataset


# Find and Drop Missing Values
sum(is.na(data))

pairs(datan[2:9],
      lower.panel=panel.smooth,
      upper.panel = panel.cor)

boxplot(datan[-1])

par(mfrow = c(3,3), mar = c(4,4,1,1))
plot(density(datan[,2],na.rm=T),main = "Body length")
plot(density(datan[,3],na.rm=T),main = "Maximum wingspan")
plot(density(datan[,4],na.rm=T),main = "Body weight")
plot(density(datan[,5],na.rm=T),main = "Lenght of beak and head")
plot(density(datan[,6],na.rm=T),main = "Length of humerous")
plot(density(datan[,7],na.rm=T),main = "Length of femur")
plot(density(datan[,8],na.rm=T),main = "Length of tibiotarsus")
plot(density(datan[,9],na.rm=T),main = "Width of skull")
plot(density(datan[,10],na.rm=T),main = "Length of keel of sternum")
par(mfrow = c(1,1), mar = c(4,4,1,1))

cor(datan[-1])

# Split Data
set.seed(600)
split <- sample(1:nrow(datan),nrow(datan)*0.7,replace = FALSE) # random selection of 70% data.
train <- datan[split,] # 70% training data
test <- datan[-split,] # remaining 30% test data



# Logistic Model
log_mod <- glm(survive~.,data = train,family = "binomial")
summary(log_mod)



# Get training prediction accuracy of model
train_acc <- predict(log_mod,train,type = "response")
train_pred <- ifelse(train_acc>0.5,1,0)
train.conf_mat <- table(Predicted = train_pred, Actual = train$survive);train.conf_mat # training confusion matrix table
train.pred_summary <- confusionMatrix(train.conf_mat);train.pred_summary
train_accuracy <- train.pred_summary$overall["Accuracy"];train_accuracy


# Get testing prediction accuracy of model
test_acc <- predict(log_mod,test,type = "response")
test_pred <- ifelse(test_acc>0.5,1,0)
test.conf_mat <- table(Predicted = test_pred, Actual = test$survive);test.conf_mat # training confusion matrix table
test.pred_summary <- confusionMatrix(test.conf_mat);test.pred_summary
test_accuracy <- test.pred_summary$overall["Accuracy"];test_accuracy


Accuracy <- c(train_accuracy,test_accuracy)
Dataset <- c("Train Accuracy","Test Accuracy")
final_table <- data.frame(cbind(Dataset,Accuracy))
final_table                                                               


##############################################################################################################################################################
##############################################################################################################################################################
##############################################################################################################################################################


# KNN MODEL

# Import Data
dataw <- read.csv(file.choose(),header = TRUE)
head(dataw,5)

# Explore Data
str(dataw) # get the structure of the dataset
dataw$survive <- as.factor(dataw$survive) # change data class to factor for categoricat description
summary(dataw) # get summary of all column in dataset

# Find and Drop Missing Values
sum(is.na(dataw))

set.seed(600)
# split data to train and test set
split <- sample(nrow(dataw),nrow(dataw)*0.7)
train_set <- dataw[split,]
test_set <- dataw[-split,]
names(train_set)


round(sqrt(nrow(train_set)))

# KNN Model
model <- train(survive~.,data=train_set,
               method = "knn",
               tuneLength = 12,
               preProcess = c("center","scale"),
               trControl = trainControl(method="cv"))
model
plot(model,
     xlab = "Number of neigbours(k)",
     main = "Accuracy Comparison against k",
     type = "b",
     pch = 0,
     lwd = 1.8)

# Accuracy
prediction1 <- predict(model,test_set)
prediction1
cm_test <- confusionMatrix(data = prediction1,
                           reference = test$survive)
cm_test
prediction2 = predict(model,train_set)
prediction2
cm_train <- confusionMatrix(data = prediction2,
                            reference = train$survive)
cm_test

test_accuracy <- cm_test$overall["Accuracy"]
train_accuracy <- cm_train$overall["Accuracy"]

Accuracy <- c(train_accuracy,test_accuracy)
Dataset <- c("Train Accuracy","Test Accuracy")
final_table <- data.frame(cbind(Dataset,Accuracy))
final_table 

