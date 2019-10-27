# This file contains the code to complete Chapter 6 R Exercise questions in Statistical Learning

# question 8
x <- rnorm(100)
eps <- rnorm(100)
y <- x + 2*x^2 + 3*x^3 + eps
 #prepare data
xnames <- paste0("X", 1:10)
dat <- data.frame(Y = y)
for (i in 1:10) {
  xi <- x^i
  dat <- cbind(dat, xi)
  names(dat)[i+1] <- xnames[i]
}
 # best subset
reg.best <- regsubsets(Y~., data = dat, nvmax = 10)
summary.best <- summary(reg.best)
plot(summary.best$rsq, xlab = "R Square", type = "l")
which.max(summary.best$rsq)
points(10, summary.best$rsq[10], col = "red", pch = 19)
coef(reg.best, 3)
# Cp 3,  BIC 3, R2 10

reg.fwd <- regsubsets(Y~., data = dat, nvmax = 10, method = "forward")
sum <- summary(reg.fwd)
plot(sum$rsq, type = "l")
which.min(sum$cp)
which.max(sum$rsq)
coef(reg.fwd, 3)
# Cp 3, BIC 3, R2 10

reg.bwd <- regsubsets(Y~., data = dat, nvmax = 10, method = "backward")
sum <- summary(reg.bwd)
coef(reg.bwd, 8)
# Cp 8, BIC 6, R2 10

 # fit a lasso 
dat1 <- as.matrix(dat) # input for cv.glmnet should be matrix
lambda <- cv.glmnet(x = dat1[,-1], y = dat1[,1], alpha = 1)
plot(lambda)
best.lambda <- lambda$lambda.min # 0.0675
lasso.fit <- glmnet(x = dat1[,-1], y = dat1[,1], alpha = 1, lambda = best.lambda)
plot(lasso.fit, type = "p")
coef(lasso.fit)
pred.lasso <- predict(lasso.fit, newx = dat1[,-1], s = best.lambda)
mean((pred.lasso - dat[,1])^2) # 0.765

 # for new relation and perform best subset and lasso
y <- 7*x^7 + eps
 # best subset
reg.best <- regsubsets(Y ~ ., data = dat, nvmax = 10)
sum <- summary(reg.best)
which.min(sum$cp) # 2, the 2nd model is optimal
coef(reg.best, 2) # b0-0.063, X6-(-0.004), X7-6.99
 # best subset can find good model 
 # lasso 
dat1 <- as.matrix(dat)
lambda <- cv.glmnet(dat1[,-1], dat1[,1], alpha = 1)
best.lambda <- lambda$lambda.min # 38.485
lasso.fit <- glmnet(dat1[,-1], dat1[,1], alpha = 1, lambda = best.lambda)
coef(lasso.fit)

############## question 9 ##############
data("College")
dim(College) # 777 18
sum(is.na(College)) # no NA

 # seperate into training and testing
train <- sample(1:nrow(College), nrow(College)/2)
training <- College[train, ]
testing <- College[-train, ]

 # response is Apps(Applications), and fit a linear model
lm.fit <- lm(Apps ~ ., data = training)
summary(lm.fit) # 17 predictors used, r2 is 0.93, 
pred.lm <- predict(lm.fit, testing)
MSE.lm <- mean((pred.lm - testing[,"Apps"])^2) #1083505

 # fit a ridge regression
x <- model.matrix(Apps ~ ., College)
y <- College$Apps
lambda <- cv.glmnet(x[train,], y[train], alpha = 0)
best.lambda <- lambda$lambda.min # 391.5686
ridge.fit <- glmnet(x[train,], y[train], alpha = 0, lambda = best.lambda)
pred.ridge <- predict(ridge.fit, newx = x[-train,], s = best.lambda)
MSE.ridge <- mean((pred.ridge - y[-train])^2) # 999941.8, less than linear model

 # fit a lasso
lambda <- cv.glmnet(x[train,], y[train], alpha = 1)
best.lambda <- lambda$lambda.min # 14.7423, quite different from ridge
lasso.fit <- glmnet(x[train,], y[train], alpha = 1, lambda = best.lambda)
coef(lasso.fit) # 15 predictors are non-zero
pred.lasso <- predict(lasso.fit, newx = x[-train,])
MSE.lasso <- mean((pred.lasso - y[-train])^2) #1082219, no better than ridge

 # fit a PCA regression
library(pls)
pca.fit <- pcr(Apps ~ ., data = training, scale = TRUE, validation = "CV")
summary(pca.fit)
validationplot(pca.fit) # 17 components reach the optimal
pred.pca <- predict(pca.fit, newdata = testing, ncomp = 17)
MSE.pca <- mean((pred.pca - testing[,"Apps"])^2) # 1195755

 # fit a PLS regression 
pls.fit <- plsr(Apps~., data = College, subset = train, scale = TRUE,
                validation = "CV")
summary(pls.fit)
validationplot(pls.fit) # 13 components reach the optimal
pred.pls <- predict(pls.fit, newdata = testing, ncomp = 13)
MSE.pls <- mean((pred.pls - testing[, "Apps"])^2) #1195822


































