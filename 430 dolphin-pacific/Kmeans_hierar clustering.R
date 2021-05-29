setwd("C:/Users/olley/Downloads")

#import Libraries
library(stats)
library(ggfortify)

#import data
data_pi <- read.csv("Pacific_Islands.csv", header=T)
attach(data_pi)
view(data_pi)
str(data_pi)
data_pi$Region <- as.factor(data_pi$Region)
str(data_pi)
summary(data_pi)


# normalize numeric variables
num_var <- data_pi[,2:13] #select numeric data
num_scaled <- scale(num_var)
head(num_scaled)
summary(num_scaled)


# Kmeans Clustering
# chosing optimal k
k<-list()
for (i in 1:15){
  k[[i]]<-kmeans(num_scaled,i)
}

betweenss_totss<-list()
for (i in 1:10){
  betweenss_totss[[i]]<-k[[i]]$betweenss/k[[i]]$totss
}

plot(1:10,betweenss_totss,type="b",
     ylab="Between SS / Total SS",xlab = "Cluster (k)")

set.seed(600)
kmeans_mod <- kmeans(num_scaled,3)


#Evaluate if clusters are distinct
autoplot(kmeans_mod,num_scaled,frame=TRUE)
plot(kmeans_mod$cluster)

par(mfrow =c(3,4))
for (i in colnames(data_pi[,2:13])){
  plot(data_pi[[i]],kmeans_mod$cluster,col=kmeans_mod$cluster,xlab=toupper(i),main=paste(toupper(i)," CLUSTERED"),pch=19 )
}
par(mfrow =c(1,1))

new_df <- cbind.data.frame(PICT=data_pi$PICT,prediction=kmeans_mod$cluster,actual=data_pi$Region)
table(new_df$actual,new_df$prediction)


#Heirarchical Clustering
dist_data <- dist(num_scaled)

for (i in c("single","complete")){
  set.seed(600)
  model<-hclust(dist_data,i)
  plot(model,main = paste(toupper(i),"LINKAGE"),labels=data_pi$PICT,xlab ="Pacific Island Country or Territory" )
  rect.hclust(model,3,border="red")
}

