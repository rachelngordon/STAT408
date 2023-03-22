# set working directory
setwd("C:/Users/mxi1/OneDrive - Loyola University Chicago/Loyola/STAT 408 Fall 2022")

# read data
pima <- read.csv('pima.csv')

# transfer test from character to factor
pima$test <- as.factor(pima$test)
summary(pima)

# correlation coefficient without missing values
cor(pima$insulin, pima$glucose, use='complete.obs')

# visualization
plot(insulin~glucose, data = pima)

# correlation in healthy group
pima.neg <- pima[pima$test=='negative',]
cor(pima.neg$insulin, pima.neg$glucose, use='complete.obs')
plot(insulin~glucose, data = pima.neg)

# correlation in diabetes group
pima.pos <- pima[pima$test=='positive',]
cor(pima.pos$insulin, pima.pos$glucose, use='complete.obs')
plot(insulin~glucose, data = pima.pos)

# compare diabetes pedigree function 
plot(diabetes ~ test, pima)

# two-sample t test 
t.test(pima.neg$diabetes, pima.pos$diabetes)

# simple linear regression with insulin as response and glucose as predictor
lm.model <- lm(insulin~glucose, data = pima)

# show lease square estimation
lm.model

# plot the fitted regression line in the scatter plot
plot(insulin~glucose, data = pima)
abline(lm.model,col='red', lwd=2)

# check goodness of fit and hypothesis test
summary(lm.model)


