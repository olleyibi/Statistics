#Loading required libraries
rm(list=ls())
library(tidyverse)
library(ggplot2)
library(splines)
theme_set(theme_classic())


#Load Environment and  dataset
setwd("C:/Users/olley/Downloads/Documents")
ar_rain <- read.table("ArmidaleRainfall.txt",header=T)


# Split the data into training and test set
set.seed(600)
split <- sample(nrow(ar_rain),nrow(ar_rain)*0.8,replace= F)
train_ar <- ar_rain[split,]
test_ar <- ar_rain[-split,]


#visualize the scatter plot of the Annual vs Year variables
ggplot(train_ar, aes(Year,Annual) ) +
  geom_point() +
  stat_smooth()


#Polynomial regression Modeling
adj_r2 = 1
for (i in 1:15){
  model=lm(Annual~poly(Year,i),data=train_ar)
  adj_r2[i] = summary(model)$adj.r.squared
}
plot(adj_r2~seq.int(1,15),main = "Adjusted R2 vs Degree of polynomial",xlab="Degree of Polynomial",ylab="Annual")
lines(adj_r2~seq.int(1,15))

poly_model=lm(Annual~poly(Year,9),data=train_ar)
summary(poly_model)

#Visualizing Polynomial Regression
ggplot(data=train_ar, aes(Year,Annual)) +
  geom_point() + 
  geom_smooth(method="lm", formula=y~poly(x,9))+ labs(title = "9th-order Polynomial Regression") 


#Model Prediction
prediction_ar = predict(poly_model,test_ar)

# Model Performance
data.frame(
  RMSE = RMSE(prediction_ar, test_ar$Annual),
  R2 = R2(prediction_ar, test_ar$Annual)
)



# Splines
# Build the model
knots <- quantile(train_ar$Year, p = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))
sp_model <- lm (Annual ~ bs(Year, knots = knots), data = train_ar)
summary(sp_model)

# Visualization
ggplot(df, aes(Year, Annual) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3),col = "purple")+labs(title = "Spline regression with three knots")




par(mfrow=c(2,2))
plot(poly_model)
par(mfrow=c(1,1))






#Part b 
# Kernal smoother
plot(df$Year,df$Annual,pch=20,col='grey', xlab="Years",ylab = "Annual",
     main = "Kernal Smoother with bandwidth 1")+lines(ksmooth(df$Year,df$Annual, "normal", bandwidth = 1),
                                                      col = 7862,lwd = 2)

plot(df$Year,df$Annual,pch=20,col='grey', xlab="Years",ylab = "Annual",
     main = "Kernal Smoother with bandwidth 2")+lines(ksmooth(df$Year,df$Annual, "normal", bandwidth = 2),
                                                      col = 765,lwd = 2)





# Local Polynomial
local=loess(Annual ~ Year, df)
summary(local) 
plot(y=df$Annual, x=df$Year, type="p", main="Loess Smoothing and Prediction", xlab="Date", ylab="Unemployment (Median)")+lines(df$Annual, df$Year)


ggplot(data=df, aes(Year,Annual)) +
  geom_point() + 
  geom_smooth(method="loess", formula= y ~ x ,col = "red")+labs(title = "Local Polynomils") 

# GAM
ggplot(df, aes(x = Year, y = Annual)) + geom_point()+ stat_smooth(method = "gam", 
                                                                  formula = y ~s(x), size = 1, se = T, colour = "green")+labs(title = "GAM")



# Comparison on a same plot
m <- ggplot(df, aes(x = Year, y = Annual)) + geom_point()
print(m)
m + stat_smooth(method = "lm", formula = y~poly(x,9), size = 1, se = T,
                colour = "black") + stat_smooth(method = "loess", formula = y ~ x,
                                                size = 1, se = T, colour = "blue") + stat_smooth(method = "lm", 
                                                                                                 formula =  y ~ splines::bs(x, df = 3), size = 1, se = T, colour = "red") + stat_smooth(method = "gam", 
                                                                                                                                                                                        formula = y ~s(x), size = 1, se = T, colour = "green") +labs(title = "Comparison")
