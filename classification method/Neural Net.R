# Using nnet
#install.packages("devtools")
library(devtools)
library(caret)
library(tibble)
library(nnet)
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
summary(dolphin)


# Split Data
set.seed(430)
split <- sample(nrow(dolphin),nrow(dolphin)*0.75,replace= F)
train <- dolphin[split,]
test <- dolphin[-split,]


set.seed(430)
nnet_mod = nnet(Whistle~.,data=train,size = 4,maxit=500,decay=0.0001)
summary(nnet_mod)
pred_net = as.factor(predict(nnet_mod,test,type="class"))
confusionMatrix(pred_net,test$Whistle)
