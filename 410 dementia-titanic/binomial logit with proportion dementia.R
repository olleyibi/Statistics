odour_score <- c(0,1,2,3,4,5,6,7,8,9,10)
patients_with_dementia <- c(24,20,26,25,27,29,27,29,30,23,18)
total <- c(75,64,86,87,93,96,92,105,115,112,130)
data_odour = data.frame(odour_score,patients_with_dementia,total)

#get proportion
data_odour["proportion"] <- round(data_odour$patients_with_dementia/data_odour$total,3)

str(data_odour)
data_odour


plot(data_odour$odour_score,data_odour$proportion,xlab="Odour Score",ylab = "Proportion")

# model
set.seed(600)
glm_mod <- glm(proportion~odour_score,family = "binomial",weights=total,data=data_odour)
summary(glm_mod)


source("Rfunctions.r")
round(betaCI(glm_mod),3)



#Interpreting the Residual Deviance
# 
pchisq(df=9,q=6.8493,lower.tail = F)
# 


# ANOVA of Devoance Table
anova(glm_mod,test='Chisq')



# Linear Predictor
eta <- glm_mod$linear.predictors
eta

# Predict values with std errors
preds <- predict(glm_mod,se.fit=T,type="response")
fit<-preds$fit
up<-preds$fit+2*preds$se.fit
lw<-preds$fit-2*preds$se.fit

# scatter plot 
plot(data_odour$odour_score,data_odour$proportion,xlab = "Odour Score",ylab="Proportion")
lines(data_odour$odour_score,fit,lty=1)
lines(data_odour$odour_score,lw,lty=2)
lines(data_odour$odour_score,up,lty=2)
  


#given p = 25% == 0.25
(log(0.25/(1-0.25))+0.62011)/-0.07599
