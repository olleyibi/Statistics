setwd("C:/Users/olley/Downloads/Documents/statistics")
folate = read.table("folate.txt", header=T)
str(folate)
folate$ventilation <- as.factor(folate$ventilation)

plot(folate$folate~folate$ventilation,las=1,xlab="Ventilation",ylab="Folate")

df <- as.data.frame(cbind(method=c(1,2,3), mean = round(tapply(folate$folate,folate$ventilation,mean),2)))
df
model <- lm(folate~ventilation,data = folate)
summary(model)
source("Rfunctions.R")

# Interpret Coefficient
betaCI(model)


m2_v_m3 <- C(folate$ventilation,c(0,1,-1),1)
orth_set <- C(folate$ventilation,c(1,-1,-1),1)

new_model <- lm(folate~m2_v_m3+orth_set,data = folate)
betaCI(new_model)


anova(new_model)