# Import Data
data_virus <- read.table(file.choose(),header = TRUE)
head(data_virus,5)


# Explore Data
str(data_virus) # get structure of the dataset
summary(data_virus)


# Find Missing Values
sum(is.na(data_virus))


# Plot cases against week

total_cases <- data.frame(tapply(data_virus$cases,data_virus$week,sum)) # get the total cases each week
colnames(total_cases)<-"Total"
total_cases

par(mfrow = c(2,2), mar = c(4,4,1,1))

# Barplot
barplot(tapply(data_virus$cases,data_virus$week,sum),col=c(1,2,3,4,5,6),
        xlab = "Week",
        ylab = "Cases")


# Boxplot
boxplot(data_virus$cases~data_virus$week,col=c(1,2,3,4,5,6),
     xlab = "Week",
     ylab = "Cases")


plot(cases~week,data=data_virus,col=week,
     xlab = "Week",
     ylab = "Cases")

par(mfrow = c(1,1), mar = c(4,4,1,1))

# It can be observed that the relationship between the week and the number of cases is positive and the number of cases
# inceases as the number of weeks increases. Also it can be observed that 

# Polynomial Regression
# Order 1
mod <- lm(cases~week,data_virus)
summary(mod)
anova(mod)

# Order 2
mod2 <- lm(cases~week+I(week^2),data_virus)
summary(mod2)
anova(mod2)


# Order 3
mod3 = lm(cases ~week+I(week^2)+I(week^3),data_virus)
summary(mod3)
anova(mod3)


par(mfrow = c(2,2), mar = c(4,4,1,1))
plot(mod2)
par(mfrow = c(1,1), mar = c(4,4,1,1))

library(ggplot2)
ggplot(data = data_virus, aes(x=week,y=cases))+
        geom_point(pch=20,color = "blue",size = 2)+
        geom_smooth(method = "lm", formula = y~poly(x,2), color="red",linetype= 2, se = TRUE)+
        labs(title = "Quadratic polynomial fit",x="Week",y="Cases")
