rm(list=ls())
library(caret)
library(psych)
library(randomForest)
library(e1071)
library(tibble)
setwd("C:/Users/olley/Downloads/Documents/statistics/classification method")
dolphin <- read.csv("SDF.csv", header=T)
attach(dolphin)
#view(dolphin)
str(dolphin)
sum(is.na(dolphin))
dolphin$Whistle <- as.factor(dolphin$Whistle)
str(dolphin)
summary(dolphin)

source("Rfunctions.R")
upper.panel<-function(x, y){
  points(x,y, col = dolphin$Whistle)}
pairs(dolphin[,-1], 
      lower.panel = panel.cor,
      upper.panel = upper.panel)


dolphin[,-1] = as.data.frame(scale(dolphin[,-1],center = T))
summary(dolphin)

#check variance
for (i in colnames(dolphin[,-1])){
  print(var(dolphin[[i]]))
}
# Split Data
set.seed(430)
split <- sample(nrow(dolphin),nrow(dolphin)*0.75,replace= F)
train <- dolphin[split,]
test <- dolphin[-split,]


# Using SVM
kernels = c("linear","polynomial","radial", "sigmoid")
accuracy = 1
cost = 1
for (i in kernels){
  set.seed(430)
  svm_tune = tune(svm,Whistle~.,data=train,gamma=0.1,
                  ranges = list(cost=c(0.01,0.1,0.2,0.5,1,5,10,20,50,100)),kernel = i)
  tune_model = svm_tune$best.model
  cost[i] = tune_model$cost
  set.seed(430)
  model = svm(Whistle~.,data = train,cost = tune_model$cost,kernel = i)
  prediction = predict(model,test)
  conf_tab = table(Predicted = prediction,Actual = test$Whistle)
  accuracy[i] = sum(diag(conf_tab))/sum(conf_tab)
}
print(accuracy)
print(cost)
comp_model = as.data.frame(cbind(cost,accuracy))
comp_model

set.seed(430)
svm_model = svm(Whistle~.,data = train,cost = 20,kernel="radial")
summary(svm_model)
pred = predict(svm_model,test)
svm_acc = round(confusionMatrix(pred,test$Whistle)$overall["Accuracy"],4)*100
svm_acc


# Using Random Forest
# Get Tuned model 
set.seed(430)
t= tuneRF(train[,-1],train[,1],stepFactor = 0.5,plot=T,ntreeTry = 300,
          trace = T,improve = 0.5,doBest = T)
# model
set.seed(430)
rf_model = randomForest(Whistle~.,data=train,ntree=300,
                        mtry=4,importance=T)
print(rf_model)
rf_pred = predict(rf_model,test)
rf_acc = round(confusionMatrix(rf_pred,test$Whistle)$overall["Accuracy"],4)*100
rf_acc


# Using KNN
ac=1
for (i in 1:30){
  set.seed(430)
  knn_m = knn3(Whistle~.,data=train,k=i)
  summary(knn_m)
  knn_pred = predict(knn_m,test,type="class")
  ac[i] = confusionMatrix(knn_pred,test$Whistle)$overall["Accuracy"]
}
plot(ac~seq(1,30))
lines(ac~seq(1,30))
#Model
set.seed(430)
knn_m = knn3(Whistle~.,data=train,k=6)
summary(knn_m)
#Prediction
knn_pred = predict(knn_m,test,type="class")
knn_acc = round(confusionMatrix(knn_pred,test$Whistle)$overall["Accuracy"],4)*100
knn_acc
  
