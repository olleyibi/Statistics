#install.packages("readxl")
#install.packages("Rcpp")
#install.packages("caret")
#install.packages('glmnet')
library(tidyverse)
library(Rcpp)
library(caret)
library(glmnet)
library("readxl")

# Import Data
data_body <-  read_xlsx(file.choose())# head(data_body,5)
head(data_body)

# Exploratory Data Analysis
str(data_body) # get structure of the dataset
data_body$Gender<-as.factor(data_body$Gender) # Change gender column to factor data type
summary(data_body)


# Find Missing Values
sum(is.na(data_body))


par(mfrow = c(5,5), mar = c(4,4,1,1))
# Histogram and Density Plot Distribution of Numeric Columns
for (i in colnames(data_body)){
  if (class(data_body[[i]]) == "numeric"){
    hist(data_body[[i]],xlab=i,main = paste("Histogram of ",i),breaks = 10,probability = TRUE)
    lines(density(data_body[[i]]),lwd=3,col="red")
  }
}
# Pie chart representing Gender Column
table<-round(prop.table(table(data_body[["Gender"]]))*100,2)
pie(table,label=c(paste(table[1],"% Female"),paste(table[2],"% Male")))

# Boxplot Distribution of Numeric columns grouped by Gender
for (i in colnames(data_body)){
  if (class(data_body[[i]]) == "numeric"){
    boxplot(data_body[[i]]~data_body[["Gender"]],col=c(2,3),main = paste(i,"Boxplot Distribution"),
                                                                         ylab = i,xlab="Gender",names=c("Female","Male"))
  }
}
par(mfrow = c(1,1), mar = c(4,4,1,1))



# split data to train and test set
split <- sample(nrow(data_body),nrow(data_body)*0.5)
train_set <- data_body[split,]
test_set <- data_body[-split,]


##################################### Forward Model Selection ########################
# minimal model (lower model)
low <- formula(~ 1)
# maximum model (upper model)
up <- formula(~ BA_diam + PB + BI_diam + Chest_dep + Chest_diam + Elbow_diam + 
                Wrist_diam + Knee_diam + Ankle_diam + Shoulder_g + Chest_g + 
                Waist_g + Navel_g + Hip_g + Thigh_g + Bicep_g + Forearm_g + 
                Knee_g + Calf_g + Ankle_g + Wrist_g + Age + Height + Gender)

# forward selection
start <- lm(Weight ~ 1, data=train_set)
fstep <- step(start,
              direction="forward",
              scope=list(lower=low,upper=up))
summary(fstep) #  New significant model
par(mfrow = c(2,2), mar = c(4,4,1,1))
plot(fstep)
par(mfrow = c(1,1), mar = c(4,4,1,1))

# Make Predictions
pred<-predict(fstep,test_set)


# Model performance metrics

data.frame(
  RMSE = RMSE(pred, test_set$Weight),
  Rsquare = R2(pred, test_set$Weight)
)

# Finding Accuracy
actuals_preds <- data.frame(cbind(actuals=test_set$Weight, predicteds=pred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy
# Correlation shows a high value of 98.6 %, so it means that actuals values and predicted values have similar directional movement.


# Actual values and predicted ones seem very close to each other. A good metric to see how much they are close is the min-max 
# accuracy, that considers the average between the minimum and the maximum prediction.
min_max <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
print(min_max)
# The result of 0.975 is a high value, and it means that the accuracy is very good.


# Mean absolute percentage deviation
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
print(mape)
# The result of about 0.025 is very low, and means a very good prediction accuracy.






# Ridge Regression
# Predictor variables
xtrain <- model.matrix(Weight~., train_set)[,-23]
# Outcome variable
ytrain <- train_set$Weight


# Find the best lambda using cross-validation
set.seed(600)
cv <- cv.glmnet(xtrain, ytrain, alpha = 0)
summary(cv)


# produce plot of test MSE by lambda value
plot(cv)
abline(v = log(cv$lambda.1se), col = "red", lty = "dashed")
#The lowest point in the curve indicates the optimal lambda: the log value 
#of lambda that best minimised the error in cross-validation.

# Display the best lambda value
cv$lambda.min #best lambda


# Fit the final model on the training data
model <- glmnet(xtrain, ytrain, alpha = 0, lambda = cv$lambda.min)
# Display regression coefficients
coef(model)

#install.packages("plotmo")
#library(plotmo)
#plot_glmnet(model,xvar="lambda",grid.col="lightgray")
#plot(model,xvar = "lambda",label=TRUE,col=c(1,2,3,4,5,6,7,8,9))

# Make predictions on the test data
xtest <- model.matrix(Weight ~., test_set)[,-23]
predictions <- model %>% predict(xtest) %>% as.vector()


# Model performance metrics

data.frame(
  RMSE = RMSE(predictions, test_set$Weight),
  Rsquare = R2(predictions, test_set$Weight)
)


# Finding Accuracy
actuals_preds2 <- data.frame(cbind(actuals2=test_set$Weight, predicted2=predictions))  # make actuals_predicteds dataframe.
correlation_accuracy2 <- cor(actuals_preds2)
correlation_accuracy2

min_max2 <- mean(apply(actuals_preds2, 1, min) / apply(actuals_preds2, 1, max))  
print(min_max2)

mape2 <- mean(abs((actuals_preds2$predicted2 - actuals_preds2$actuals2))/actuals_preds2$actuals2)
print(mape2)

