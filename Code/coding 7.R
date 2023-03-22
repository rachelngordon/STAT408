chredlin <- read.csv('chredlin.csv')
chmiss <- read.csv('chmiss.csv')

summary(chmiss)
mean(is.na(chmiss))

1 - mean(complete.cases(chmiss))
summary(chmiss)

summary(lm(involact~., data = chmiss))

means <- colMeans(chmiss, na.rm = T)
chmiss.impute <- chmiss
for(i in 1:6){
  chmiss.impute[is.na(chmiss.impute[,i]), i] <- means[i]
}
summary(lm(involact~., data = chmiss.impute))

# impute income
lm.impute <- lm(income~race+fire+age+theft, data = chmiss)
income.impute <- predict(lm.impute, chmiss[is.na(chmiss$income),])
round(income.impute, 2)
chredlin$income[is.na(chmiss$income)]

chmiss.impute <- chmiss
chmiss.impute[is.na(chmiss.impute$income),] <- income.impute

# impute fire
lm.impute <- lm(fire~race+theft+age+income, data = chmiss)
fire.impute <- predict(lm.impute, chmiss[is.na(chmiss$fire),])
chmiss.impute[is.na(chmiss.impute$fire),] <- fire.impute

# impute theft
lm.impute <- lm(theft~race+fire+age+income, data = chmiss)
theft.impute <- predict(lm.impute, chmiss[is.na(chmiss$theft),])
chmiss.impute[is.na(chmiss.impute$theft),] <- theft.impute

# impute age
lm.impute <- lm(age~race+fire+theft+income, data = chmiss)
age.impute <- predict(lm.impute, chmiss[is.na(chmiss$age),])
chmiss.impute[is.na(chmiss.impute$age),] <- age.impute

# impute race
lm.impute <- lm(race~age+fire+theft+income, data = chmiss)
race.impute <- predict(lm.impute, chmiss[is.na(chmiss$race),])
ifelse(race.impute<0,0,race.impute)
chmiss.impute[is.na(chmiss.impute$race),] <- race.impute

summary(lm(involact~., data = chmiss.impute))






