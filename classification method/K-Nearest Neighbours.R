# Knn
library(class)
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

#Scale
dolphin[,-1] = as.data.frame(scale(dolphin[,-1],center = T))
#summary(dolphin)

# Split Data
set.seed(430)
split <- sample(nrow(dolphin),nrow(dolphin)*0.75,replace= F)
train <- dolphin[split,]
test <- dolphin[-split,]

ac=1
for (i in 1:40){
  set.seed(430)
  knn_m = knn3(Whistle~.,data=train,k=i)
  summary(knn_m)
  knn_pred = predict(knn_m,test,type="class")
  ac[i] = confusionMatrix(knn_pred,test$Whistle)$overall["Accuracy"]
}
cbind(ac,seq(1,40))

set.seed(430)
knn_m = knn3(Whistle~.,data=train,k=5)
summary(knn_m)

knn_pred = predict(knn_m,test,type="class")
confusionMatrix(knn_pred,test$Whistle)

