############################################################################################
# Prediction accuracy in logistic regression
############################################################################################
# read Heart dataset
heart <- read.csv('Heart.csv')

# remove obs with missing value
heart <- heart[complete.cases(heart), ]

# transform character response to 0 or 1
heart$AHD <- ifelse(heart$AHD=="Yes", 1, 0)

# random split the data into 80% training and 20% test
set.seed(2022)
index.train <- sample(1:dim(heart)[1], 0.8 * dim(heart)[1])
data.train <- heart[index.train,]
data.test <- heart[-index.train,]

# fit a logistic regression on training set
logistic.model <- glm(AHD~., data=data.train, family='binomial')

# predict on the test test, obtain the predicted P(Y=1)
p.pred <- predict(logistic.model, data.test, type='response')

# transform to binary response
y.pred <- ifelse(p.pred>=0.5, 1, 0)

# calculate classification accuracy
y.truth <- data.test$AHD
acc.test <- mean(y.pred==y.truth)
acc.test

############################################################################################
# confusion matrix, true positive rate, true negative rate, precision
############################################################################################
# confusion matrix
table(y.pred, y.truth)

# true positive
TP <- intersect(which(y.truth==1), which(y.pred==1))

# true negative
TN <- intersect(which(y.truth==0), which(y.pred==0))

# all positives
AP <- which(data.test$AHD==1)

# all negatives
AN <- which(data.test$AHD==0)

# predicted positives
PP <- which(y.pred==1)

# true postive rate
TPR <- length(TP) / length(AP)
TPR

# true negative rate
TNR <- length(TN) / length(AN)
TNR

# precision
prec <- length(TP) / length(PP)
prec

############################################################################################
# roc curve
############################################################################################
library(PRROC)
plot(roc.curve(scores.class0 = p.pred[y.truth==1], 
               scores.class1 = p.pred[y.truth==0], curve = TRUE),
     ylab='True Postive Rate', xlab='False Negative Rate (1 - True Negative Rate)')




