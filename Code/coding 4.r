# read data
pima <- read.csv('pima.csv')

# remove missing values 
pima <- pima[complete.cases(pima), ]

# fit linear model
lm.model <- lm(insulin~., data=pima)
summary(lm.model)

# plot residule vs yhat
plot(lm.model$residuals~lm.model$fitted.values, xlab='yhat', ylab='residue')
abline(h=0)

# fit linear model (square root transformation)
lm.model <- lm(sqrt(insulin)~., data=pima)

# plot residule vs yhat 
plot(lm.model$residuals~lm.model$fitted.values, xlab='yhat', ylab='residue')
abline(h=0)
summary(lm.model)

# fit linear model (log transformation)
lm.model <- lm(log(insulin)~., data=pima)

# plot residule vs yhat 
plot(lm.model$residuals~lm.model$fitted.values, xlab='yhat', ylab='residue')
abline(h=0)
summary(lm.model)

# qq plot
qqnorm(lm.model$residuals)
qqline(lm.model$residuals)

# residue correlation between healthy people and patients
residue.neg <- lm.model$residuals[pima$test=='negative']
residue.pos <- lm.model$residuals[pima$test=='positive']
cor(residue.neg[sample(130)], residue.pos)
plot(residue.neg[sample(130)], residue.pos)

# residue correlation between ith and (i+1)th individuals
n <- dim(pima)[1]
cor(tail(lm.model$residuals, n-1), head(lm.model$residuals, n-1))
plot(tail(lm.model$residuals, n-1)~head(lm.model$residuals, n-1))

# studentized residues
lm.model <- lm(insulin~., data=pima)
sr <- rstudent(lm.model)
df <- n - 9 - 1

# use 5% critical value as cutoff
which(abs(sr) > qt(0.975, df))
sum(abs(sr) > qt(0.975, df))

# influential observation
lm.model <- lm(insulin~., data=pima)
beta.change <- dfbeta(lm.model)

# plot the change of glucose parameter after removing each data point 
plot(beta.change[,3])

# model specification
x <- runif(100,0,10)
y <- 3+x+x^2+rnorm(100,0,1)
lm.model <- lm(y~x)
plot(lm.model$residuals ~ lm.model$fitted.values)

plot(lm.model$residuals ~ x)
plot(lm.model$)

