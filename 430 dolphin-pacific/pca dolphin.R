setwd("C:/Users/olley/Downloads")
data_dolphin <- read.csv("Spinner_dolphin.csv", header=T)
attach(data_dolphin)
view(data_dolphin)
str(data_dolphin)
sum(is.na(data_dolphin))
data_dolphin$Whistle <- as.factor(data_dolphin$Whistle)
str(data_dolphin)
summary(data_dolphin)


pairs(data_dolphin[,-1],
      lower.panel=panel.smooth,
      upper.panel = panel.cor)

boxplot(data_dolphin[,-1])

set.seed(600)
pcmod <- prcomp(data_dolphin[,-1],scale=T)
pcmod
summary(pcmod)
plot(pcmod,type='l')

biplot(pcmod,scale = 0)

# compute proportion of variance explained
pve=summary(pcmod)$importance[2,]

plot(pve , xlab=" Principal Component ", ylab=" Proportion of Variance Explained ", ylim=c(0,1) ,type='b')


plot(cumsum(pve),xlab="Principal Component", ylab ="Cumulative Proportion of Variance Explained ",ylim=c(0,1) ,type='b')



round(cor(data_dolphin[,-1],pcmod$x[,1:2]),2)

pcmod$x
dat <- cbind(data_dolphin,pcmod$x[,1:3])
head(dat)


library(ggplot2)
ggplot(dat,aes(PC1,PC2, col = Whistle,fill=Whistle))+
  stat_ellipse(geom="polygon",col="black",alpha=0.5)+
  geom_point(shape=21,col="black")


