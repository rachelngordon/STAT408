sexab <- read.csv('sexab.csv')
sexab$csa <- as.factor(sexab$csa)
summary(sexab)

plot(ptsd~csa,data = sexab)
plot(ptsd~cpa,pch=as.character(csa),data = sexab)

d1 <- ifelse(sexab$csa=='Abused',1,0)
d2 <- ifelse(sexab$csa=='NotAbused',1,0)

lmod <- lm(ptsd~d1+d2, sexab)
summary(lmod)
model.matrix(lmod)

lmod <- lm(ptsd~csa, sexab)
summary(lmod)
model.matrix(lmod)

lmod <- lm(ptsd~cpa+csa+cpa:csa,sexab)
summary(lmod)

plot(ptsd~cpa, sexab, pch=as.numeric(csa))
abline(3.7, 0.764, lty=2)
abline(10.5571, 0.45)

lmod <- lm(ptsd~cpa+csa,sexab)
summary(lmod)

plot(ptsd~cpa, sexab, pch=as.numeric(csa))
abline(3.9752, 0.5506, lty=2)
abline(10.248, 0.5506)

####################################################
happy <- read.csv('happy.csv')
summary(happy)
lmod <- lm(happy~money+sex+love+work, data = happy)
summary(lmod)
X <- model.matrix(lmod)

happy$love <- as.factor(happy$love)
happy$work <- as.factor(happy$work)
summary(happy)

lmod <- lm(happy~money+sex+love+work, data = happy)
summary(lmod)
X.factor <- model.matrix(lmod)






