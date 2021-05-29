# Import the data
data <- read.table(file.choose(),header = TRUE)
data


# Plot the correlation pairs figure
source("Rfunctions.R")
pairs(data[2:6],
      lower.panel=panel.smooth,
      upper.panel = panel.cor)
# With a correlation value of <0.1, we find little to no pattern between correlation pairs (PE,PP) and (PE,PS) as 
# the observations were randomly scattered
# PP has a weak positive linear relationship with PS of correlation value 0.35
# temp has a low correlation value of 0.0046, -0.08957, 0.138 with PS, PP(Negative relationship), PE respectively
# larvae has a negative relationship with PE, PP, PS and temp with correlation value of -0.81(strong correlation),
# -0.52555(moderately correlated), -0.22(weak correlation), and -0.0097(extremely weak correlation) respectively


# Fith the model
mod <- lm(larvae~PE+PP+PS+temp,data)
summary(mod) # Summary table of the model
# larvae = 52.54973 - 0.059817(PE) - 0.206932(PP) - 0.002371(PS) + 0.273804(temp)


# H0: ??PE = ??PP = ??PS = ??temp = 0, Given all predictors have been fitted, they are all not useful
# Given all predictors are fitted, at a 5% significance level, the significant predictors of fish larvae 
# density include PE, PP, and temp, their p-values of 2e-16, 2e-16 and 0.0163 respectively gives enough significant 
# statistical evidence to reject the null hypothesis (H0) which says all predictors are not useful.
mod2 <- lm(larvae~PE+PP+temp,data) # Fit new model with new derived predictors


print(summary(mod2)) # summary table of new model
# larvae = 52.519088 - 0.059837(PE) - 0.208167(PP) + 0.272595(temp)


# Get 95% confidence intervals for the regression coecients
betaCI(mod2)
# The 95% CI for ??PE is (-0.063,-0.056) indicates that provided other predictors are held constant, a unit increase
# in PE(polyethylene microplastics) will result in a decrease in the average larvae between -0.056 and -0.063 µg/m3
# The 95% CI for ??PP is (-0.228,-0.188) indicates that provided other predictors are held constant, a unit increase
# in PP(polypropylene microplastics) will result in a decrease in the average larvae between -0.188 and -0.228 µg/m3
# The 95% CI for ??temp is (0.05,0.495) indicates that provided other predictors are held constant, a unit increase
# in temp(water temperature) will result in an increase in the average larvae between 0.05 and 0.495 Celsius


# Get diagnostic plots
par(mfrow=c(2,2))
plot(mod2)
# Residuals VS Fitted plot
# It can be observed that linearity is fairly reasonably because, the red line seems to move closely with the dashed 
# line. The heteroskedasticity while moving towards the right on the x-axis looks like there is a spread of the 
# residuals which appears to be increasing at some points. Finally, points 131, 60 and 176 may be outliers, 
# with large residual values.
# Normal Q-Q plot
# It shows that the residuals are basically normally distributed but a little skewness can be observed on the right
# of the plot

par(mfrow = c(1,1))

# Estimating mean fish larvae density and the 95% CI
temp <- c(17.5,20.5)
PE <- c(300,300)
PP <- c(50,50)
newd <- data.frame(PE,PP,temp)
newd
predict(mod2, newdata = newd, interval = 'confidence','level' = 0.95)
predict(mod2, newdata = newd, interval = 'prediction','level' = 0.95)


# Conclusion
# It is found out that the percentage of polyethylene and polypropylene microplastics(PE and PP) on aquatic organisms, 
# and the temperature of the water are useful in predicting the larvae density(per 100 m3), while the percentage of 
# the polystyrene microplastics(PS) do not have a significant effect on the larvae density(per 100 m3)
# The final model is E(larvae) = 52.519088 - 0.059837(PE) - 0.208167(PP) + 0.272595(temp)
# The average larvae density will decrease when polyethylene and polypropylene microplastics(PE and PP) increases
# on aquatic organisms provided other predictors remain constant when either PE or PP is being studied. However, 
# provided other predictors are held constant, the average larvae density is higher for increased temperature at 95%
# CI.The model using polyethylene, polypropylene microplastics(PE and PP) and temp(water temperature) explains about
# 89.1% of the variability in larvae density.


n <- ****** # integer datatype
colour <- ******
title <- ****** # title of plot
lab_y <- ****** # label for y-axis
lab_x <- ****** # label for x-axis

plot(density(data$colname,na.rm=T),
     col = colour
     main = title,
     xlab = lab_x,
     ylab = lab_y)







