# Import Data
dataw <- read.csv(file.choose(),header = TRUE)
head(dataw,5)

# Explore Data
str(dataw) # get the structure of the dataset
dataw$survive <- as.factor(dataw$survive) # change data class to factor for categoricat description
summary(dataw) # get summary of all column in dataset

# Find and Drop Missing Values
sum(is.na(dataw))

#normalize data
norma <- function(x){(x-min(x))/(max(x)-min(x))}
dataw_norm = as.data.frame(lapply(dataw[-1],norma))
summary(dataw_norm)


set.seed(600)
# split data to train and test set
split <- sample(nrow(dataw_norm),nrow(dataw_norm)*0.7)
train_set <- dataw_norm[split,]
test_set <- dataw_norm[-split,]
names(train_set)

# create labels for training and test data
y_train <- dataw[split,1]
y_test <- dataw[-split,1]

# KNN model
library(class)
library(caret)

# Get accuracy of model using a standard calculated k value
knn_mod1 <- knn(train = train_set,test = test_set,cl = y_train,k=round(sqrt(nrow(train_set))))
cm_k_sqrt <- confusionMatrix(table(ActualValue = y_test,
                      Predicted = knn_mod1))
round(cm_k_sqrt$overall,3) # prediction summary of model



# Optimizing the value of k
i=1
k.optm=1
for (i in 1:10){
  knn_mod_optm <- knn(train = train_set,test = test_set,cl = y_train,k=i)
  k.optm[i] <- 100 * sum(y_test == knn_mod_optm)/NROW(y_test)
  k=i
  cat(k,'=',k.optm[i],'\n')
}
plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level") # Visualize accuracy agains selected k values


# new model using optimum k value
knn_mod2 <- knn(train = train_set,test = test_set,cl = y_train,k=3)
cm_k_opt <- confusionMatrix(table(ActualValue = y_test,
                      Predicted = knn_mod2))
round(cm_k_opt$overall,3)



cc <- confusionMatrix(table(ActualValue = y_train,
                            Predicted = knn_mod2))
