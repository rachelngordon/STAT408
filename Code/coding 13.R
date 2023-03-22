# ridge regression

meatspec <- read.csv('meatspec.csv')

require(MASS)
rgmod <- lm.ridge(fat ~ ., meatspec, lambda = seq(0, 5e-8, len=21))
matplot(rgmod$lambda, coef(rgmod), type="l", xlab=expression(lambda)
          ,ylab=expression(hat(beta)),col=1)

set.seed(2022)
index.train <- sample(214*0.8)
trainmeat <- meatspec[index.train,]
testmeat <- meatspec[-index.train,]

# regular linear model
modlm <- lm(fat ~ ., trainmeat)
yhat <- predict(modlm, testmeat)
MSE <- mean((yhat - testmeat$fat)^2)
MSE

# select best lamda by cross-validation
rgmod <- lm.ridge(fat ~ ., trainmeat, lambda = seq(0, 5e-8, len=21))
rgmod$GCV
which.min(rgmod$GCV)

# predict using best lamda
yhat.ridge <- cbind(1,as.matrix(testmeat[,-101])) %*% coef(rgmod)[8,]
MSE.ridge <- mean((yhat.ridge - testmeat$fat)^2)
MSE.ridge

# Lasso
require(lars)
state <- read.csv('state.csv')

# lars function requires the matrix of predictors as its first argument, 
# and the vector of response as its second argument
lmod <- lars(as.matrix(state[,-4]),state$Life)
plot(lmod)

# lasso for prediction
trainy <- trainmeat$fat
trainx <- as.matrix(trainmeat[,-101])
lassomod <- lars(trainx,trainy)

# select best penalty by cross-validation
set.seed(2022)
cvout <- cv.lars(trainx,trainy)
cvout$index[which.min(cvout$cv)]

# predict using best penalty
testx <- as.matrix(testmeat[,-101])
yhat <- predict(lassomod,testx,s=0.0101,mode="fraction")
MSE <- mean((yhat$fit - testmeat$fat)^2)
MSE





