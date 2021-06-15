#Loading required libraries

library(tidyverse)
library(ggplot2)
library(splines)


#Importing our dataset

df <- read.delim("C:/Users/Ma Sha Allah/Downloads/ArmidaleRainfall.txt")


# Part(a)        Polynomial regression
# Modeling
model=lm(Annual~poly(Year,3),data=df)
summary(model)
model=lm(Annual~poly(Year,9),data=df)
summary(model)


#Visualizing Polynomial Regression

ggplot(data=df, aes(Year,Annual)) +
   geom_point() + 
   geom_smooth(method="lm", formula=y~poly(x,9))+ labs(title = "9th-order Polynomial Regression") 

 
 
#Part b 

# Kernal smoother
plot(df$Year,df$Annual,pch=20,col='grey', xlab="Years",ylab = "Annual",main = "Kernal Smoother with bandwidth 1")+lines(ksmooth(df$Year,df$Annual, "normal", bandwidth = 1), col = 7862,lwd = 2)
plot(df$Year,df$Annual,pch=20,col='grey', xlab="Years",ylab = "Annual",main = "Kernal Smoother with bandwidth 2")+lines(ksmooth(df$Year,df$Annual, "normal", bandwidth = 2), col = 765,lwd = 2)

 # Splines
 
# Build the model

knots <- quantile(df$Year, p = c(0.25,0.5,0.75))
model <- lm (Annual ~ bs(Year, knots = knots), data = df)
summary(model) 
# Visualization

ggplot(df, aes(Year, Annual) ) +
    geom_point() +
    stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3),col = "purple")+labs(title = "Spline regression with three knots")
 
 
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
