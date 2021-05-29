library(dplyr)
data_titanic <-  read.table(file.choose(),header = T)
head(data_titanic)
str(data_titanic)
data_titanic$class <- as.factor(data_titanic$class)
data_titanic$age <- as.factor(data_titanic$age)
data_titanic$sex <- as.factor(data_titanic$sex)
str(data_titanic)
data_titanic["surv_prop"] <- round(data_titanic$survive/data_titanic$total,3)
data_titanic
str(data_titanic)
data_titanic$class <- relevel(data_titanic$class,ref="first")
summary(data_titanic)


par(mfrow=c(1,3))
for (i in colnames(data_titanic%>% select(class,age,sex))){
  grp_sum <- data.frame(sum = tapply(data_titanic[["survive"]], data_titanic[[i]], sum))
  print(grp_sum)
  grp_sum<-tibble::rownames_to_column(grp_sum,i)
  pct <- paste(round(100*grp_sum$sum/sum(grp_sum$sum), 1),"%")
  pie(grp_sum$sum,labels=pct,col = rainbow(length(grp_sum[[i]])),main=paste("Total Survival grouped by ",i))
  legend("bottomright",legend=grp_sum[[i]],fill=rainbow(length(grp_sum$sum)))
}




for (i in colnames(data_titanic%>% select(class,age,sex))){
  avg_prop = tapply(data_titanic[["surv_prop"]], data_titanic[[i]], mean)
  barplot(avg_prop,col = rainbow(length(avg_prop)),ylab="Average Proportion",xlab=i,main=paste("Average Survival grouped by ",i))
}
  

par(mfrow=c(1,1))



# Stepwise Logistic Regression
set.seed(600)
# minimal model (lower model)
low <- formula(~ 1)
# maximum model (upper model)
up <- formula(~ class*age*sex)

start <- glm(surv_prop ~ 1, data=data_titanic,family="binomial",weights = total)
fstep <- step(start,
              direction="forward",
              scope=list(lower=low,upper=up))

summary(fstep)

par(mfrow=c(2,2))
plot(fstep)
par(mfrow=c(1,1))




anova(fstep,test='Chisq')
round(1-pchisq(1.7,df=3),3)
