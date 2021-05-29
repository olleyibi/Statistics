# Import Data
data <- read.csv(file.choose(),header = TRUE)
head(data,5)


# Explore Data
str(data) # get structure of the dataset
data$predation <- as.factor(data$predation) # change data class to factor for categoricat description
data$exposure <- as.factor(data$exposure) # change data class to factor for categoricat description
summary(data) # get summary of all column in dataset


# Find and Drop Missing Values
sum(is.na(data))
dim(data)
library(dbplyr)
data_dropna <-data %>% na.omit()		
dim(data_dropna)




# Get numeric columns
num_var <- data.frame(data_dropna[c("non_dreaming","dreaming","MLE","gestation","log_body","log_brain")]) # select numeric column for description


# Quantify and Visualize numeric variables
summary(num_var) # get summary of all numeric column in dataset
round(cor(num_var),4) # get correlation relationship between of all numeric column in dataset
pairs(num_var,
      lower.panel=panel.smooth,
      upper.panel = panel.cor) # visual plot to show bivariate relationship between all variable pairs (visualizes correlation)
boxplot(num_var,col = c(1,2,3,4,5,6)) # Confirm presence of outliers
# Check the distribution of numaric values
par(mfrow = c(2,3), mar = c(4,4,1,1)) 
hist(num_var$non_dreaming,breaks=10,main='Non_dreaming Distribution',col="violet",xlab = "Non_dreaming")
hist(num_var$dreaming,breaks=10,main='Dreaming Distribution',col="cyan",,xlab = "Dreaming")
hist(num_var$MLE,breaks=10,main='MLE Distribution',col="yellow",xlab = "MLE")
hist(num_var$gestation,breaks=10,main='Gestation Distribution',col="blue",xlab = "Gestation")
hist(num_var$log_body,breaks=10,main='Log_body Distribution',col="red",xlab = "Log_body")
hist(num_var$log_brain,breaks=10,main='Log_brain Distribution',col='green',xlab = "Log_brain")

par(mfrow = c(1,2), mar = c(4,4,1,1))

pred_grp = tapply(data_dropna$MLE, data_dropna$predation, mean)
pred_grp
barplot(pred_grp,beside=TRUE,
        col = c(1,2,3),main = "Average MLE Vs predation type")

exp_grp = tapply(data_dropna$MLE, data_dropna$exposure, mean)
exp_grp
barplot(exp_grp,
        col = c(1,2,3),main = "Average MLE Vs exposure type")

par(mfrow = c(1,1), mar = c(4,4,1,1))

# Fit Original Model
mod <- lm(MLE~non_dreaming+dreaming+gestation+predation+exposure+log_body+log_brain,data=data_dropna)
summary(mod)

#Addressing multicolinearity
# install.packages("faraway")
library(faraway)
vif(mod)

mod2 <- lm(MLE~non_dreaming+dreaming+gestation+predation+exposure,data=data_dropna) # model with neither log_brain nor log_body
mod3 <- lm(MLE~non_dreaming+dreaming+gestation+predation+exposure+log_body,data=data_dropna) # model with only log_body
mod4 <- lm(MLE~non_dreaming+dreaming+gestation+predation+exposure+log_brain,data=data_dropna) # model with only log_brain
summary(mod4)
Model=c("original(mod)","without both(mod2)","with log_body only(mod3)","with log_brain only(mod4)")
R2=c(summary(mod)$r.squared,summary(mod2)$r.squared,summary(mod3)$r.squared,summary(mod4)$r.squared)
Adj_R2=c(summary(mod)$adj.r.squared,summary(mod2)$adj.r.squared,summary(mod3)$adj.r.squared,summary(mod4)$adj.r.squared)
comp_tab=data.frame(cbind(Model,R2,Adj_R2))
comp_tab


