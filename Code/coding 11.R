############################################################################################
# Prediction
############################################################################################
# read fat dataset
fat <- read.csv('fat.csv')

# random split the data into 80% training and 20% test
set.seed(2022)
index.train <- sample(1:dim(fat)[1], 0.8 * dim(fat)[1])
data.train <- fat[index.train,]
data.test <- fat[-index.train,]

# fit a linear model on the training set
lm.model <- lm(brozek ~ weight + abdom + forearm + wrist, 
               data=data.train)

# predict on the test set
yhat.test <- predict(lm.model, data.test)

# calculate test MSE
y.test <- data.test$brozek
MSE.test <- mean((y.test - yhat.test)^2)
MSE.test

# root MSE
RMSE.test <- sqrt(MSE.test)
RMSE.test

# normalized root MSE
NRMSE.test <- RMSE.test / mean(y.test)
NRMSE.test


############################################################################################
# 5-fold Cross-validation
############################################################################################
# read fat dataset
fat <- read.csv('fat.csv')
set.seed(2022)

# randomly shuffle the index
index.random <- sample(1:dim(fat)[1])

# split the data (index) into 5 folds 
groups <- cut(1:252, 5, labels = FALSE)
index.fold <- split(index.random, groups)

# an empty vector to save individual MSE
MSEs <- c()

# 5-fold cross-validation
for(index.test in index.fold){
  
  # creat training and test set
  data.test <- fat[index.test,]
  data.train <- fat[-index.test,]
  
  # fit a linear model on the training set
  lm.model <- lm(brozek ~ weight + abdom + forearm + wrist, 
                 data=data.train)
  
  # predict on the test set
  yhat.test <- predict(lm.model, data.test)
  
  # calculate test MSE
  y.test <- data.test$brozek
  MSE.test <- mean((y.test - yhat.test)^2)
  MSEs <- c(MSEs, MSE.test)
}
# plot 5 MSEs
plot(1:5, MSEs, type='b', col='red', xlab='Fold', ylab='MSE', ylim=c(10,25))

# Average 5 MSEs
mean(MSEs)

############################################################################################
# Leave-one-out Cross-validation
############################################################################################
# read fat dataset
fat <- read.csv('fat.csv')
set.seed(2022)

# an empty vector to save individual MSE
MSEs <- c()

# LOOCV
for(index.test in 1:dim(fat)[1]){
  
  # creat training and test set
  data.test <- fat[index.test,]
  data.train <- fat[-index.test,]
  
  # fit a linear model on the training set
  lm.model <- lm(brozek ~ weight + abdom + forearm + wrist, 
                 data=data.train)
  
  # predict on the test set
  yhat.test <- predict(lm.model, data.test)
  
  # calculate test MSE
  y.test <- data.test$brozek
  MSE.test <- mean((y.test - yhat.test)^2)
  MSEs <- c(MSEs, MSE.test)
}
# plot n MSEs
plot(1:dim(fat)[1], MSEs, type='b', col='red', xlab='Fold', ylab='MSE')

# Average MSEs
mean(MSEs)
