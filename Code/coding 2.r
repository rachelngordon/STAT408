# set working directory
setwd("C:/Users/mxi1/OneDrive - Loyola University Chicago/Loyola/STAT 408 Fall 2022")

# read data
pima <- read.csv('pima.csv')

# remove missing values 
pima <- pima[complete.cases(pima), ]
dim(pima)

# multinormial linear regression
lm.model <- lm(insulin~., data = pima)
summary(lm.model)

# design matrix
X <- model.matrix(~pregnant+glucose+diastolic+triceps+bmi+diabetes+age+test, data = pima)

# reponse variable
y <- pima$insulin

XtXi <- solve(t(X)%*%X)
XtXi%*%t(X)%*%y

# create a new variable
pima$new <- pima$bmi+pima$age
lm.model <- lm(insulin~., data = pima)
summary(lm.model)

# read data
pima <- read.csv('pima.csv')

# remove missing values
pima <- pima[complete.cases(pima), ]

# add small perturbation
pima$glucose.p <- pima$glucose + 0.001*(runif(dim(pima))-0.5)

# fit linear model
lm.model <- lm(insulin~., data = pima)
summary(lm.model)





