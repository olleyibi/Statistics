# Using SVM
library(caret)
library(psych)
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

upper.panel<-function(x, y){
  points(x,y, col = dolphin$Whistle)}
pairs(dolphin[,-1], 
      lower.panel = panel.cor,
      upper.panel = upper.panel)

# Split Data
set.seed(430)
split <- sample(nrow(dolphin),nrow(dolphin)*0.75,replace= F)
train <- dolphin[split,]
test <- dolphin[-split,]


library(e1071)
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
svm_model = svm(Whistle~.,data = train,cost = 50,kernel="radial")
summary(svm_model)
pred = predict(svm_model,test)
confusionMatrix(pred,test$Whistle)$overall["Accuracy"]