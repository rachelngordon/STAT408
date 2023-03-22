
saving <- read.csv("saving.csv")
plot(sr ~ pop15, data = saving, xlab='population under 15', ylab='saving rate')
abline(v=35, lty=5)

# fit two models
lm1 <- lm(sr~pop15, data = saving, subset = (pop15<35))
lm2 <- lm(sr~pop15, data = saving, subset = (pop15>35))

# draw two models
segments(x0 = 20, y0 = lm1$coefficients[1]+lm1$coefficients[2]*20, 
         x1 = 35, y1 = lm1$coefficients[1]+lm1$coefficients[2]*30)
segments(x0 = 35, y0 = lm1$coefficients[1]+lm1$coefficients[2]*35, 
         x1 = 48, y1 = lm1$coefficients[1]+lm1$coefficients[2]*48)

# two base functions
bl <- function(x){
  ifelse(x<35, 35-x, 0)
}

br <- function(x){
  ifelse(x>35, x-35, 0)
}
# fit a segmented model
lm.seg <- lm(sr~bl(pop15)+br(pop15), data = saving)

# plot model
x <- seq(20, 48, by=1)
y <- lm.seg$coefficients[1]+lm.seg$coefficients[2]*bl(x)+lm.seg$coefficients[3]*br(x)
lines(x,y,lty=2)

summary(lm.seg)
summary(lm1)
summary(lm2)

# polynomial regression
lm.model <- lm(sr~pop15+ddpi+I(pop15*ddpi)+I(ddpi^2)+I(pop15^2), data = saving)
summary(lm.model)

# collinearity
seatpos <- read.csv('seatpos.csv')
lm.model <- lm(hipcenter~., data = seatpos)
summary(lm.model)

# correlation matrix
round(cor(seatpos[,-9]), 2)

# regression among predictors
X <- model.matrix(lm.model)[,-1]
for(i in 1:8){
  r2 <- summary(lm(X[,i]~X[,-i]))$r.squared
  cat(colnames(X)[i], '\t', r2, '\n')
}

# instability
lm.model <- lm(hipcenter+rnorm(n=38,mean=0,sd=10)~., data = seatpos)
summary(lm.model)

# remove correlated predictors







