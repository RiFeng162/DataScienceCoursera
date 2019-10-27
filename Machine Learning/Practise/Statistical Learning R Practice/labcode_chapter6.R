# This file contains the code of Statistical Learning Chapter 6 Lab code.
library(leaps) # contain function regsubsets
library(ISLR)
data("Hitters")
dim(Hitters) # 322 20
Hitters <- na.omit(Hitters)
dim(Hitters) # 263 20

regfit.full <- regsubsets(Salary~., data = Hitters)
summary(regfit.full)
regfit.full <- regsubsets(Salary~., data = Hitters, nvmax = 19) # set max subset size
full.summary <- summary(regfit.full)
names(full.summary)
full.summary$rsq

par(mfrow = c(2,2))
plot(full.summary$rss, xlab = "Number of Variables", ylab = "RSS")
plot(full.summary$adjr2, ylab = "Adjusted R-Square", type = "l")
which.max(full.summary$adjr2) # 11
points(11, full.summary$adjr2[11], col = "red", pch = 19)
plot(full.summary$cp, ylab = "Cp", type = "l")
x <- which.min(full.summary$cp)
points(x, full.summary$cp[x], col="red", pch=19)
plot(full.summary$bic, ylab="BIC", type = "l")
x <- which.min(full.summary$bic)
points(x, full.summary$bic[x], col="red", pch=19)

# to visualize
par(old.par)
plot(regfit.full, scale = "r2") # show which variables are in the models
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
coef(regfit.full, 6) # extract coefficients of the specified model

############### Forward or Backward #############
regfit.fwd <- regsubsets(Salary~., data = Hitters, method = "forward",
                         nvmax = 19)
summary(regfit.fwd)
regfit.bwd <- regsubsets(Salary~., data = Hitters, method = "backward",
                         nvmax = 19)
summary(regfit.bwd)
plot(regfit.bwd, scale = "r2")

coef(regfit.bwd,7)
coef(regfit.fwd,7)
coef(regfit.full,7)
# optimal models from each strategy are different

############# combine variable selection and cross-validation ##############
set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(Hitters), rep = TRUE)
test <- (!train)

regfit.best <- regsubsets(Salary~., data = Hitters[train,], nvmax = 19)
test.mat <- model.matrix(Salary~., data = Hitters[test,]) # build X matrix for regression
val.errors <- rep(NA, 19)
for (i in 1:19) {
  coefi <- coef(regfit.best, i)
  pred <- test.mat[,names(coefi)]%*%coefi # matrix product, X times beta
  val.errors[i] <- mean((Hitters[test,"Salary"] - pred)^2)
} # create this loop because no predict method for regsubsets
which.min(val.errors) # 7
coef(regfit.best, 7)

predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]]) # call is stored in list, 2nd element is formula
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
} # create predict method for regsubsets

regfit.best <- regsubsets(Salary~., data = Hitters, nvmax = 19)
coef(regfit.best, 7) # we use full data to re-construct models, the steps above
# used to get optimal subset size 7
# best subset model from full data is different from training data model

k <- 10
set.seed(1)
folds <- sample(1:k, nrow(Hitters), replace = T)
cv.errors <- matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))
for (j in 1:k) {
  best.fit <- regsubsets(Salary~., data = Hitters[folds!=j,], nvmax = 19)
  for (i in 1:19) {
    pred <- predict.regsubsets(best.fit, Hitters[folds==j,], id = i)
    cv.errors[j,i] <- mean((Hitters$Salary[folds==j] - pred)^2)
  }
}
mean.cv.errors <- apply(cv.errors,2, mean)
mean.cv.errors
# combine cv with vr selection. Apply subset selection on (k-1) folds and predict
# on 1 fold. It means at each subset, we do a k-fold cv to accurately access its
# metrix
plot(mean.cv.errors, tyoe = "b") # subset with 10 has smallest MSE, instead of 7 

############### Ridge Regression and Lasso ##################
library(glmnet) # to use glmnet function
 # prepare data
grid <- 10^seq(10,-2,length.out = 100)
x <- model.matrix(Salary~., Hitters)[,-1]
y <- Hitters$Salary


ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)
 # alpha = 1 is lasso, alpha = 0 is ridge
 # attention, x, y matrix format instead of formula
 # create elnet, glmnet class -> ?predict.elnet for details
dim(coef(ridge.mod)) # 20 100 (coefficients under each lambda value)
ridge.mod$lambda[50] # 11498
coef(ridge.mod)[, 50]
sqrt(sum(coef(ridge.mod)[-1, 50]^2)) # norm beta, 6.4
ridge.mod$lambda[60] # 705
sqrt(sum(coef(ridge.mod)[-1, 60]^2)) # 57, small lambda with large norm

pred.coef <- predict(ridge.mod, s = 50, type = "coefficients")
pred.coef[1:20,]
 # provide new lambda(s) to predict coefficients

set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
test.y <- y[test]
ridge.mod <- glmnet(training, y[train], alpha = 0, lambda = grid)
pred.ridge <- predict(ridge.mod, s = 4, newx = testing)
mean((pred.ridge-test.y)^2)
mean((mean(y[train])-test.y)^2) # null model RSS
 # use cv to decide lambda
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 0) # cv for glm
plot(cv.out)
best.lambda <- cv.out$lambda.min
best.lambda # 326, this lambda minimize the RSS
ridge.pred <- predict(ridge.mod, s = best.lambda, newx = x[-train,])
mean((ridge.pred - test.y)^2)
out <- glmnet(x,y, alpha = 0)
predict(out, type = "coefficients", s = best.lambda)

 # lasso
lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)

 # with cv
cv.out <- cv.glmnet(x[train,], y[train], alpha = 1)
plot(cv.out) # MSE change trends with lambda
best.lambda <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s = best.lambda, newx = x[test,])
mean((lasso.pred - y[test])^2)
out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients", s = best.lambda)[1:20,]
lasso.coef
lasso.coef[lasso.coef!=0]

############ PCA ############
library(pls)
set.seed(2)
train <- sample(1:nrow(Hitters), nrow(Hitters)/2)

 # create Principal Component Regression
pcr.fit <- pcr(Salary ~ ., data = Hitters, 
               scale = TRUE, validation = "CV") # create mvr object
pcr.fit
summary(pcr.fit) # report Root MSE for each CV
validationplot(pcr.fit, val.type = "MSEP")

pcr.fit <- pcr(Salary ~ ., data = Hitters, subset = train, scale = TRUE,
               validation = "CV")
validationplot(pcr.fit) # get optiaml when ncomp = 6

pred.pcr <- predict(pcr.fit, newdata = Hitters[-train,], ncomp = 7) # ncomp is required
mean((pred.pcr - Hitters$Salary[-train])^2)

########### PLS ##############
 # create Partial Least Square Regression
pls.fit <- plsr(Salary ~ ., data = Hitters, subset = train, scale = T,
               validation = "CV")
summary(pls.fit) # lowest CV Error reached at 2 components
validationplot(pls.fit, val.type = "MSEP")

pred.pls <- predict(pls.fit, newdata = Hitters[-train,], ncomp = 2)
mean((pred.pls - Hitters$Salary[-train])^2) # 103263.1


























