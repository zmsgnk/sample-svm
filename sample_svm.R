library(kernlab)

data(spam)

set.seed(10)
train.idx <- sample.int(nrow(spam), nrow(spam)/2)
train.d <- spam[train.idx, ]
test.d <- spam[-train.idx, ]

library(e1071)
##-----------------------------------
## RBF kernel
##-----------------------------------
rbf <- svm(type ~ ., data=train.d, probability=TRUE, kernel="radial", cross=10)
rbf.pred <- predict(rbf, newdata=test.d, probability=TRUE, decision.values=TRUE)
table(rbf.pred, test.d$type)

##-----------------------------------
## Linear kernel
##-----------------------------------
linear <- svm(type ~ ., data=train.d, probability=TRUE, kernel="linear", cross=10)
linear.pred <- predict(linear, newdata=test.d, probability=TRUE, decision.values=TRUE)
table(linear.pred, test.d$type)

##-----------------------------------
## Polynomial kernel
##-----------------------------------
poly <- svm(type ~ ., data=train.d, probability=TRUE, kernel="polynomial", cross=10)
poly.pred <- predict(poly, newdata=test.d, probability=TRUE, decision.values=TRUE)
table(poly.pred, test.d$type)

##-----------------------------------
## Sigmoid kernel
##-----------------------------------
sig <- svm(type ~ ., data=train.d, probability=TRUE, kernel="sigmoid", cross=10)
sig.pred <- predict(sig, newdata=test.d, probability=TRUE, decision.values=TRUE)
table(sig.pred, test.d$type)