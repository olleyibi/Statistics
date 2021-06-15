library(caret)
library(tibble)
setwd("C:/Users/olley/Downloads/Documents/statistics/430 dolphin-pacific")
dolphin <- read.csv("Spinner_dolphin.csv", header=T)
attach(dolphin)
view(dolphin)
str(dolphin)
sum(is.na(dolphin))
dolphin$Whistle <- as.factor(dolphin$Whistle)
str(dolphin)
summary(dolphin)

# Split Data
set.seed(430)
split <- sample(nrow(dolphin),nrow(dolphin)*0.75,replace= F)
train <- dolphin[split,]
test <- dolphin[-split,]


set.seed(430)
pc_mod = prcomp(train[,-1],center=T,scale.=T)
summary(pc_mod)
pairs.panels(pc_mod$x)

pc_train = predict(pc_mod,train)
pc_train = data.frame(pc_train,whistle = train$Whistle)

pc_test = predict(pc_mod,test)
pc_test = data.frame(pc_test,whistle = test$Whistle)


library(ggplot2)
ggplot(pc_train,aes(PC1,PC2, col = whistle,fill=whistle))+
  stat_ellipse(geom="polygon",col="black",alpha=0.5)+
  geom_point(shape=21,col="black")


ggplot(pc_test,aes(PC1,PC2, col = whistle,fill=whistle))+
  stat_ellipse(geom="polygon",col="black",alpha=0.5)+
  geom_point(shape=21,col="black")

#UNING NNET
library(nnet)
set.seed(430)
pcnnet_mod = nnet(whistle~.,data=pc_train,size = 4,maxit=500,decay=0.0001)
summary(pcnnet_mod)
pcpred_net = as.factor(predict(pcnnet_mod,pc_test,type="class"))
confusionMatrix(pcpred_net,pc_test$whistle)


# using svm
library(e1071)
kernels = c("linear","polynomial","radial", "sigmoid")
pcaccuracy = 1
pccost = 1
for (i in kernels){
  set.seed(430)
  pcsvm_tune = tune(svm,whistle~PC1+PC2+PC3+PC4+PC5+PC6,data=pc_train,
                  ranges = list(cost=c(0.01,0.1,0.2,0.5,1,5,10,20,50,100)),kernel = i)
  pctune_model = pcsvm_tune$best.model
  pccost[i] = pctune_model$cost
  set.seed(430)
  print(pctune_model$cost)
  pcmodel = svm(whistle~PC1+PC2+PC3+PC4+PC5+PC6,data = pc_train,cost = pctune_model$cost,kernel = i)
  pcprediction = predict(pcmodel,pc_test)
  pcconf_tab = table(Predicted = pcprediction,Actual = pc_test$whistle)
  pcaccuracy[i] = sum(diag(pcconf_tab))/sum(pcconf_tab)
}
print(pcaccuracy)
print(pccost)
pccomp_model = as.data.frame(cbind(pccost,pcaccuracy))
pccomp_model


set.seed(430)
pcsvm_model = svm(whistle~PC1+PC2+PC3+PC4+PC5+PC6,data = pc_train,cost = 1.0,kernel="radial")
summary(pcsvm_model)
pcpred = predict(pcsvm_model,pc_test)
confusionMatrix(pcpred,pc_test$whistle)$overall["Accuracy"] #svm


# Tree Model
set.seed(430)
pctree = rpart(whistle~PC1+PC2+PC3+PC4+PC5,data=pc_train,method="class")
plot(pctree)
text(pctree,pretty=0)
rpart.plot(pctree,type=3,extra=108)
pctree_pred = predict(pctree,pc_test,type="class") 
pctree_tab = table(pctree_pred,pc_test$whistle)
confusionMatrix(pctree_pred,test$Whistle)$overall["Accuracy"] #tree


#RANDOM FOREST mODEL
set.seed(430)
pct= tuneRF(pc_train[,-9],pc_train[,9],stepFactor = 0.5,plot=T,ntreeTry = 500,
          trace = T,improve = 0.01)


set.seed(430)
pcrf_model = randomForest(whistle~PC1+PC2+PC3+PC4+PC5+PC6+PC7,data=pc_train,ntree=500,
                        mtry=2,importance=T,proximity=T)
print(pcrf_model)
pcrf_pred = predict(pcrf_model,pc_test)
confusionMatrix(pcrf_pred,pc_test$whistle)$overall["Accuracy"] #rf


# NNET
set.seed(430)
pcnnet_mod = nnet(whistle~PC1+PC2+PC3+PC4+PC5,data=pc_train,size = 4,maxit=500,decay=0.0001)
summary(pcnnet_mod)
pcpred_net = as.factor(predict(pcnnet_mod,pc_test,type="class"))
confusionMatrix(pcpred_net,pc_test$whistle)$overall["Accuracy"] #nnet



#KNN
pcac=1
for (i in 1:10){
  set.seed(430)
  pcknn_m = knn3(whistle~.,data=pc_train,k=i)
  summary(pcknn_m)
  pcknn_pred = predict(pcknn_m,pc_test,type="class")
  pcac[i] = confusionMatrix(pcknn_pred,pc_test$whistle)$overall["Accuracy"]
}
plot(pcac~seq(1,10))
lines(pcac~seq(1,10))

set.seed(430)
pcknn_m = knn3(whistle~PC1+PC2+PC3+PC4+PC5+PC6,data=pc_train,k=4)
summary(pcknn_m)
pcknn_pred = predict(pcknn_m,pc_test,type="class")
confusionMatrix(pcknn_pred,pc_test$whistle)$overall["Accuracy"] #knn
