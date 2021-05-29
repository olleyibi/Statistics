# norm all

setwd("C:/Users/olley/Downloads/Documents")
toy = read.csv("Toy.csv", header=T)
str(toy)
toy=toy[,-1]
str(toy)
library(caret)
# Normalize dataset
toy2 = as.data.frame(scale(toy,center=T))

set.seed(430)
split <- sample(1:nrow(toy2),nrow(toy2)*0.75,replace = FALSE) # random selection of 75% data.
train <- toy2[split,] # 70% training data
test <- toy2[-split,] # remaining 30% test data


set.seed(430)
netm=neuralnet(Y~.,data=train,hidden=1,stepmax = 1e7)
plot(netm)

# predictions
pred = predict(netm,test[,-10])
cor(pred,test$Y) # correlation coefficient

# mean squared error
library(modelr)
MSE1 = mse(netm,test)
MSE1

# plot of predicted vs actual
plot(pred~test[,10],xlab = "Actual",ylab = "Prediction",main="Actual vs Predicted")
abline(lm(pred~test[,10]), col = "green", lwd = 3)


# Question 2
MSE = 1
R = 1
for (i in 1:7){
  model = neuralnet(Y~.,data=train,hidden=i,stepmax = 1e7)
  prediction = predict(model,test[,-10])
  MSE[i] = mse(model,test)
  R[i] = cor(prediction,test$Y)
}
par(mfrow=c(1,2))
plot(MSE~seq.int(7), xlab= "Hidden Unit",ylab = "Mean Squared Error",main="Hidden Unit vs MSE")
lines(seq.int(7),MSE)
plot(R~seq.int(7), xlab= "Hidden Unit",ylab = "Correlation Coefficient",main="Hidden Unit vs Correlation Coefficient")
lines(seq.int(7),R)
par(mfrow=c(1,1))

d = data.frame(cbind(Hidden_unit = seq.int(7),Mean_squared_error = MSE,Correlation_coeff=R))
d
# Using 6 Hidden unit
set.seed(430)
netm7=neuralnet(Y~.,data=train,hidden=7,stepmax = 1e7)
plot(netm7)
pred7 = predict(netm7,test[,-10])
cor(pred7,test$Y) # correlation coefficient

# mean squared error
library(modelr)
MSE7 = mse(netm7,test)
MSE7

# plot of predicted vs actual
plot(pred7~test[,10],xlab = "Actual",ylab = "Prediction",main="Actual vs Predicted")
abline(lm(pred7~test[,10]), col = "red", lwd = 3)





# Question 3
train_percent = c(20,40,60,80)
train_ratio = train_percent/100
hidden_layer = c(1,7)

par(mfrow=c(1,2))

for (j in hidden_layer){
  R_cor=1
  for (i in seq.int(length(train_ratio))){
    set.seed(430)
    spliting <- sample(1:nrow(toy2),nrow(toy2)*(train_ratio[i]),replace = FALSE)
    training = toy2[spliting,]
    testing = toy2[-spliting,]
    set.seed(430)
    model=neuralnet(Y~.,data=training,hidden=j,stepmax = 1e7)
    preds = predict(model,testing[,-10])
    R_cor[i] = cor(preds,test[,10])
  }
  
  plot(R_cor~train_percent,xlab = "Percentage Training Dataset",ylab = "Correlation Coefficient",main=paste("For Hidden layer of",j))
  lines(R_cor~train_percent)
}


par(mfrow=c(1,1))
