setwd("C:/Users/olley/Downloads/Documents/statistics/410 ANOVA Factorial")
#install.packages("effects")
library(dae)
library(effects)
barley = read.table("Barley.txt", header=T)
str(barley)
barley$BBLOCK = as.factor(barley$BBLOCK)
barley$BSPACE = as.factor(barley$BSPACE)
barley$BVARIETY = as.factor(barley$BVARIETY)
str(barley)

# There are four blocks, three varieties and three different row spacings (blocked 3X3X3 factorial)

# Linear model
lmod <- lm(BYIELD~BBLOCK+BSPACE*BVARIETY, data = barley)
plot(allEffects(lmod))
summary(lmod)
anova(lmod)

interaction.plot(barley$BSPACE,barley$BVARIETY,barley$BYIELD,
                 col= c(1,2,3),lwd=2,xlab="space",ylab="Yield Mean",
                 trace.label = "BVARIETY")

?interaction.plot

print(allEffects(lmod))

par(mfrow=c(2,2))
plot(lmod)
par(mfrow=c(1,1))


boxplot(lmod$residuals)

shapiro.test(lmod$residuals)

mean(lmod$residuals)

