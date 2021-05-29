# Import Data
data_gas <- read.table(file.choose(),header = TRUE)
head(data_gas,5)


# Explore Data
str(data_gas) # get structure of the dataset
summary(data_gas)


# Find Missing Values
sum(is.na(data_gas))

# pairs plot to visualize correlation of the numeric values
pairs(data_gas[2:ncol(data_gas)],
      lower.panel=panel.smooth,
      upper.panel = panel.cor) # visual plot to show bivariate relationship between all variable pairs (visualizes correlation)


set.seed(600)
# fit model using linear regression
mod <- lm(HEATRATE~INLET+EXH+AIRFLOW,data_gas)
summary(mod) # model summary
round(cor(data_gas[2:(ncol(data_gas)-1)]),3) #correlation between numeric predictors
anova(mod)


# Get VIF of model predictors
# install.packages("car")
library(car)
round(vif(mod),3)


# Using stepwise Regression

# minimal model (lower model)
formL <- formula(~ 1)

# maximum model (upper model)
formU <- formula(~INLET*EXH*AIRFLOW)

# forward selection
start.model <- lm(HEATRATE ~ 1, data=data_gas)
fstep.model <- step(start.model,
                    direction="forward",
                    scope=list(lower= formL,upper=formU))
summary(fstep.model)
anova(fstep.model)

