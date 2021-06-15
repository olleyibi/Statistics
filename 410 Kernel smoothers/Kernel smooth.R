rm(list=ls())
#Loading required libraries

library(tidyverse)
library(ggplot2)
library(splines)
theme_set(theme_classic())


#Importing our dataset
setwd("C:/Users/olley/Downloads/Documents")
df <- read.table("ArmidaleRainfall.txt",header=T)


# Visualize Data
ggplot(data=df, aes(Year,Annual)) +
  geom_point() + 
  geom_smooth()


# Polynomial regression Modeling
adj_r2 = 1
for (i in 1:15){
  model=lm(Annual~poly(Year,i),data=df)
  adj_r2[i] = summary(model)$adj.r.squared
}
plot(adj_r2~seq.int(1,15),xlab="Degree of Polynomial",ylab="Annual",main="Adjusted R2 vs Degree of Polynomial")
lines(adj_r2~seq.int(1,15))

# Final polynomial regression model
poly_model=lm(Annual~poly(Year,9),data=df)
summary(poly_model)

par(mfrow=c(2,2))
plot(poly_model)
par(mfrow=c(1,1))


#Visualizing Polynomial Regression
ggplot(data=df, aes(Year,Annual)) +
  geom_point() + 
  geom_smooth(method="lm", formula=y~poly(x,9))+ labs(title = "9th-order Polynomial Regression") 




# Kernal smoother
par(mfrow=c(2,2))
for (i in 1:4){
  plot(df$Year,df$Annual,pch=20,col='grey', xlab="Years",ylab = "Annual",
       main = paste("Kernal Smoother with bandwidth",i))
  lines(ksmooth(df$Year,df$Annual, "normal", bandwidth = i),
        col = 7862+i,lwd = 2)
}
par(mfrow=c(1,1))



# Local Polynomial
local=loess(Annual ~ Year, df)
summary(local)
# Visualize the smoother
ggplot(data=df, aes(Year,Annual)) +
  geom_point() + 
  geom_smooth(method="loess", formula= y ~ x ,col = "red")+
  labs(title = "Local Polynomials") 


# Splines Model
knots <- quantile(df$Year, p = c(0.25,0.5,0.75))
model <- lm (Annual ~ bs(Year, knots = knots), data = df)
summary(model)
# Visualization
ggplot(df, aes(Year, Annual) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, 3),col = "purple")+
  labs(title = "Spline regression with three knots")



# GAM
ggplot(df, aes(x = Year, y = Annual)) + geom_point()+
  stat_smooth(method = "gam",formula = y ~s(x), size = 1, se = T, colour = "green")+
  labs(title = "GAM")



# Comparison on a same plot
m <- ggplot(df, aes(x = Year, y = Annual)) + geom_point()
print(m)
m + stat_smooth(method = "lm", formula = y~poly(x,9), size = 1, se = T,
                colour = "black") + stat_smooth(method = "loess", formula = y ~ x,
                                                size = 1, se = T, colour = "blue") + stat_smooth(method = "lm", 
                                                                                                 formula =  y ~ splines::bs(x, df = 3), size = 1, se = T, colour = "red") + stat_smooth(method = "gam", 
                                                                                                                                                                                        formula = y ~s(x), size = 1, se = T, colour = "green") +labs(title = "Comparison")
