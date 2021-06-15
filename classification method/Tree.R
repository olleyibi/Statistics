# Using tree classification
library(rpart)
library(rpart.plot)
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

# Model
set.seed(430)
tree = rpart(Whistle~.,data=train,method="class")
plot(tree)
text(tree,pretty=0)
rpart.plot(tree,type=3,extra=108)

tree_pred = predict(tree,test,type="class") 
tree_tab = table(tree_pred,test$Whistle)
confusionMatrix(tree_pred,test$Whistle)
