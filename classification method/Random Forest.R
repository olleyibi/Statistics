# Using Random Forest
library(randomForest)
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

# Tune 
set.seed(430)
t= tuneRF(train[,-1],train[,1],stepFactor = 0.5,plot=T,ntreeTry = 500,
          trace = T,improve = 0.01)


set.seed(430)
rf_model = randomForest(Whistle~.,data=train,ntree=500,
                        mtry=4,importance=T,proximity=T)
print(rf_model)
rf_pred = predict(rf_model,test)
confusionMatrix(rf_pred,test$Whistle)




