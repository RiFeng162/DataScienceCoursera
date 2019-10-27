# This file contains the code to complete the Chapter 9 Lab (Support Vector Machine)
# of Statistical Learning

########### Support Vector Classifier #############
 # prepare data
set.seed(1)
x <- matrix(rnorm(2*20), ncol = 2) # values filled column by column
y <- c(rep(-1,10), rep(1,10))
x[y==1,] <- x[y==1,] + 1
plot(x, col = (3 - y)) # decision boundary are not linear
library(e1071) # to use function svm
dat <- data.frame(x = x, y = as.factor(y)) # response should be factor to fill in svm function
svm.fit <- svm(y~., data = dat, kernel = "linear", cost = 0.1, scale = T) # return svm object, smaller the cost, wider the margin
plot(svm.fit, dat)
svm.fit$index # index for support vectors

 # tune parameter with cross-validation
tune.out <- tune(svm, y~., data = dat, kernel = "linear", 
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10 ,100)))
summary(tune.out) # return the parameter and finalModel
svm.best <- tune.out$best.model
summary(svm.best) # contents same as the svm.fit
pred.svm <- predict(svm.best, dat)
table(pred.svm, y)

############# Support Vector Machine #############
 # prepare the data
set.seed((1))
x <- matrix(rnorm(200*2), ncol = 2)
x[1:100,] <- x[1:100,] + 2
x[101:150, ] <- x[101:150, ] - 2
y <- c(rep(1, 150), rep(2,50))
dat <- data.frame(x = x, y = as.factor(y))
train <- sample(1:length(y), length(y)/2)
plot(x, col = y)

 # SVM for non-linear boundary
svm.fit <- svm(y~., data = dat, kernel = "radial", gamma = 1, cost = 1e5)
plot(svm.fit, dat)
svm.tune <- tune(svm, y~., data = dat, kernel = "radial", 
                 ranges = list(cost = c(0.1,1,10,100,1000),
                               gamma = c(0.5,1,2,3,4)))
summary(svm.tune)

############ ROC Curve ############
library(ROCR)
 # create function to simplify ROC curve processes
rocplot <- function(pred, truth, ...) {
  predob <- prediction(pred, truth) # create prediciton-object, the base of ROCR
  perf <- performance(predob, "tpr", "fpr") # create performance-object
  plot(perf, ...)
}

svm.fit.opt <- svm(y~., data = dat[train,], kernel = "radial", gamma = 50,
                   cost = 1, decision.values = T)
fitted <- attributes(predict(svm.fit.opt, dat[train,], decision.values = T))$decision.values
par(mfrow = c(1,2))
rocplot(fitted, dat[train, "y"], add = T, col = "red")
par(old.par)

############# ROC with Multiple Classes #############
 # prepare data
set.seed(1)
x <- rbind(x, matrix(rnorm(50*2), ncol = 2))
y <- c(y, rep(0, 50))
x[y==0,2] <- x[y==0,2] + 2
dat <- data.frame(x = x, y = as.factor(y))
plot(x, col = (y+1))

svm.fit <- svm(y ~ ., data = dat, kernel = "radial", cost = 10, gamma = 1)
plot(svm.fit, dat)

########## Application to Gene Expression Data #########
 # data prepare and explore
library(ISLR)
data("Khan")
names(Khan) # contains "xtrain", "xtest", "ytrain", "ytest"
dim(Khan$xtrain) # 63, 2308
dim(Khan$xtest) # 20 2308
table(Khan$ytrain) # response contains 4 classes
dat <- data.frame(x = Khan$xtrain, y = as.factor(Khan$ytrain))

out <- svm(y~., data = dat, kernel = "linear", cost = 10)
summary(out)
table(out$fitted, dat$y) # perfect prediciton on training set

dat.test <- data.frame(x = Khan$xtest, y = Khan$ytest)
pred.test <- predict(out, newdata = dat.test)
table(pred.test, dat.test$y)


























