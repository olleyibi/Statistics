# Import Data
data_aphid <- read.csv(file.choose(),header = TRUE)
head(data_aphid,5)


# Explore Data
str(data_aphid) # get structure of the dataset
summary(data_aphid)


# Find Missing Values
sum(is.na(data_aphid))


# Function to calculate R0 for the population
calc_ro <- function(data){ # function takes in the data as an argument
  r0=1
  j=1
  for (i in 1:ncol(data)){
    if (class(data[,i]) == "integer"){ # checks if data column is of integer class
      total <- sum(data[,i]>0) #total female that survived at each age
      row_tot <- nrow(data) #total female in the observation
      prop <- total/row_tot #proportion of female surviving
      avg <- mean(data[,i]) #average number of offspring at each age
      r0[j]<- avg*prop
      j=j+1
    }
  }
  return(sum(r0))
}


# Estimate Bootstrap Error and 95% cinfidence interval

set.seed((600))
boot_out<-1
R<-100000
for (i in 1:R){
  new_data <- sample(data_aphid[-1],3,replace = TRUE)
  boot_out[i] <- calc_ro(new_data)
}



calc_ro(data_aphid)
mean(boot_out)
sd(boot_out)

# 95% confidence interval
data.frame(interval=c(quantile(boot_out,prob=0.025),quantile(boot_out,prob=0.975)))


# We are 95% confident that the net reproductive rate of this animal is between 8.64 and 11.97


