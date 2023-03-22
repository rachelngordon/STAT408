newhamp <- read.csv('newhamp.csv')

colSums(newhamp[newhamp$votesys == 'D', c('Obama','Clinton')])
colSums(newhamp[newhamp$votesys == 'H', c('Obama','Clinton')])

lmod <- lm(pObama ~ votesys, newhamp)
summary(lmod)

lmod <- lm(pObama ~ votesys + Dean, newhamp)
summary(lmod)

lmod <- lm(Dean ~ votesys, newhamp)
summary(lmod)

library(Matching)
newhamp$trt <- ifelse(newhamp$votesys == 'H',1,0)
mm <- GenMatch(newhamp$trt, newhamp$Dean, ties=FALSE)
match <- mm$matches[,1:2]

pdiff <- newhamp$pObama[match[,1]] - newhamp$pObama[match[,2]]
t.test(pdiff)

set.seed(2022)
mm <- GenMatch(newhamp$trt, newhamp$Dean, ties=FALSE)
match <- mm$matches[,1:2]

pdiff <- newhamp$pObama[match[,1]] - newhamp$pObama[match[,2]]
t.test(pdiff)

plot(pdiff ~ newhamp$Dean[match[,1]], xlab="Dean", ylab="Hand-Digital")
abline(h=0)

Dean.compare <- cbind(newhamp$Dean[match[,1]], newhamp$Dean[match[,2]])


