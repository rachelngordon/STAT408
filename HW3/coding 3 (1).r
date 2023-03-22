# read data
pima <- read.csv('pima.csv')

# remove missing values 
pima <- pima[complete.cases(pima), ]

# F test for model comparison
lm.model <- lm(insulin~., data = pima)
null.model <- lm(insulin~1, data=pima)
anova(null.model, lm.model)
summary(lm.model)

# Full RSS
rss.full <- deviance(lm.model)

# Null RSS
rss.null <- deviance(null.model)

# Full df
df.full <- df.residual(lm.model)

# Null df
df.null <- df.residual(null.model)

# manually perform F test
F.stat <- ((rss.null - rss.full)/(df.null - df.full)) / (rss.full/df.full)
p <- 9
q <- 1
n <- 392
p.value <- 1 - pf(F.stat, p-q, n-p)
p.value

# Test a Pair of Predictors
pima <- read.csv('pima.csv')
pima <- pima[complete.cases(pima), ]
lm.model <- lm(insulin~., data = pima)
small.model <- lm(insulin~pregnant+diastolic+triceps+diabetes+age+test, data=pima)
anova(small.model, lm.model)


# Test relationship of two predictors
pima <- read.csv('pima.csv')
pima <- pima[complete.cases(pima), ]
lm.model <- lm(insulin~., data = pima)
small.model <- lm(insulin~I(glucose+bmi)+pregnant+diastolic+triceps+diabetes+age+test, data=pima)
anova(small.model, lm.model)

summary(lm.model)

# Test a subspace
pima <- read.csv('pima.csv')
pima <- pima[complete.cases(pima), ]
lm.model <- lm(insulin~., data = pima)
small.model <- lm(insulin~offset(2*glucose)+bmi+pregnant+diastolic+triceps+diabetes+age+test, data=pima)
anova(small.model, lm.model)



######################################################
# permutation F test
######################################################
# orginal F test
lm.model <- lm(insulin~., data = pima)
summary(lm.model)
summary(lm.model)$fstat
summary(lm.model)$fstat[1]
F.original <- summary(lm.model)$fstat[1]

# set random seed to keep result same
set.seed(123)

# empty vector to save each permutation F statistic
Fs <- c()

# repeat by 4000 times
for(i in 1:4000){
  # linear regression on shuffled response
  lm.model <- lm(sample(insulin)~., data = pima)
  # save permutation F statistic
  Fs[i] <- summary(lm.model)$fstat[1]
}

# Calculate the proportion of less than the original F statistics
mean(Fs > F.original)
hist(Fs, breaks = 100)

######################################################
# permutation T test
######################################################
# orginal t test for tricepts
lm.model <- lm(insulin~., data = pima)
summary(lm.model)
summary(lm.model)$coef[5,]
T.original <- summary(lm.model)$coef[5,3]

# set random seed to keep result same
set.seed(123)

# empty vector to save each permutation F statistic
Ts <- c()

for(i in 1:1000){
  # linear regression on shuffled pregnant
  lm.model <- lm(insulin~glucose+bmi+pregnant+diastolic+sample(triceps)+diabetes+age+test, data = pima)
  # save permutation T statistic
  Ts[i] <-  summary(lm.model)$coef[5,3]
}

# Calculate the proportion of less than the original F statistics
mean(abs(Ts) > abs(T.original))
hist(Ts, breaks = 100)


# confidence interval
pima <- read.csv('pima.csv')
pima <- pima[complete.cases(pima), ]
lm.model <- lm(insulin~., data = pima)
summary(lm.model)

# 95% CI for glucose
2.16735 + c(-1,1) * qnorm(0.975) * 0.18957

# 99% CI for glucose
2.16735 + c(-1,1) * qnorm(0.995) * 0.18957

# 95% CI for bmi
2.11882 + c(-1,1) * qnorm(0.975) * 0.97538

# 99% CI for bmi
2.11882 + c(-1,1) * qnorm(0.995) * 0.97538

# 95% CI for age
1.03011 + c(-1,1) * qnorm(0.975) * 0.69809

# automatic CI
confint(lm.model, level = 0.95)
confint(lm.model, level = 0.99)


# boostrap CI for glucose
set.seed(2022)
beta <- c()
for(i in 1:1000){
  index <- sample(392, replace = T)
  pima.bootstrap <- pima[index,]
  lm.model <- lm(insulin~., data = pima.bootstrap)
  beta[i] <- coef(lm.model)[3]
}
hist(beta, breaks = 100, main='Empirical Distribution of Glucose')
quantile(beta, c(0.025, 0.975))


