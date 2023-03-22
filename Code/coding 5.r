# read data
state <- read.csv('state.csv')

# Backward selection
# fit linear model with all predictors
lm.model <- lm(Life.Exp~., data=state)
summary(lm.model)

# fit linear model without Area
lm.model <- update(lm.model, .~.-Area)
summary(lm.model)

# fit linear model without Illiteracy
lm.model <- update(lm.model, .~.-Illiteracy)
summary(lm.model)

# fit linear model without Income
lm.model <- update(lm.model, .~.-Income)
summary(lm.model)

# fit linear model without Population
lm.model <- update(lm.model, .~.-Population)
summary(lm.model)

# step AIC
lm.model <- lm(Life.Exp~., data=state)
step(lm.model)
