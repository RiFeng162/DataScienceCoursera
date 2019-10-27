# This file include the codes to complete the Quiz 4 of Machine Learning, 
# Data Science courses from Coursera.

############ question 1 ###########
# prepare data
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
vowel.train$y <- as.factor(vowel.train$y) # dim 528 11
vowel.test$y <- as.factor(vowel.test$y) # dim 462 11
set.seed(33833)

# create models
rf.vowel <- train(y~., data = vowel.train, method = "rf")
gbm.vowel <- train(y~., data = vowel.train, method = "gbm", verbose = FALSE)
pred.rf <- predict(rf.vowel, vowel.test)
pred.gbm <- predict(gbm.vowel, vowel.test)
confusionMatrix(pred.gbm, pred.rf)
# accuracy for rf is 0.6017, gbm is 0.5303, for both is 0.6797

############# question 2 #############
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors) # dim 333 131
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

# create models
rf.ad <- train(diagnosis~., data = training, method = "rf")
gbm.ad <- train(diagnosis~., data = training, method = "gbm", verbose = FALSE)
lda.ad <- train(diagnosis~., data = training, method = "lda")
set.seed(62433)
pred.rf <- predict(rf.ad, testing)
pred.gbm <- predict(gbm.ad, testing)
pred.lda <- predict(lda.ad, testing)
confusionMatrix(pred.rf, testing$diagnosis)$overall[1] # 0.9024
confusionMatrix(pred.gbm, testing$diagnosis)$overall[1] # 0.8780
confusionMatrix(pred.lda, testing$diagnosis)$overall[1] # 0.9146

# stacking the models
df.stack <- data.frame(pred.rf, pred.gbm, pred.lda, diagnosis = testing$diagnosis,
                       stringsAsFactors = F)
stack.ad <- train(diagnosis~., data = df.stack, method = "rf")
pred.stack <- predict(stack.ad, df.stack)
confusionMatrix(pred.stack, testing$diagnosis)$overall[1] # 0.9390

########### question 3 ############
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete) # dim 1030 9
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

attach(concrete)
lasso.con <- train(CompressiveStrength~., data = training, method = "lasso")
plot(lasso.con)
sum <- summary(lasso.con)
lasso.enet <- enet(training[,-9], training[,9], lambda = 0)
plot(lasso.enet) 

grid <- 10^seq(10, -2, length.out = 100)
training <- as.matrix(training)
lasso.con <- glmnet(training[,-9], training[,9], lambda = grid, alpha = 1)
plot(lasso.con)
summary(lasso.con)
which(lasso.con$df == 1) # 76 77, find the lambda that only leave 1 predictor
lasso.con$lambda[76:77]
out <- predict(lasso.con, s = 8, type = "coefficients")
out # Cement is the last one
detach(concrete)

############### question 4 ###############
library(lubridate) # For year() function below
dat = read.csv("~/Desktop/gaData.csv", stringsAsFactors = FALSE) # dim 600 3
training = dat[year(dat$date) < 2012,] # 365
testing = dat[(year(dat$date)) > 2011,] # 235
tstrain = ts(training$visitsTumblr)

# Fit a model using the bats() function in the forecast package 
# to the training time series. Then forecast this model for the 
# remaining time points. For how many of the testing points is the true value
# within the 95% prediction interval bounds?

library(forecast)
bats.fc <- bats(tstrain)
summary(bats.fc)

pred.fc <- forecast(bats.fc, h = 235, level = 95)
test.y <- testing$visitsTumblr
mean(test.y >= pred.fc$lower & test.y <= pred.fc$upper) # 0.96

############### question 5 ###############
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

# Set the seed to 325 and fit a support vector machine using the 
# e1071 package to predict Compressive Strength using the default settings. 
# Predict on the testing set. What is the RMSE?

library(e1071)
svm.fit <- svm(CompressiveStrength ~ ., data = training)
pred.test <- predict(svm.fit, newdata = testing)
RMSE <- sqrt(mean((pred.test - testing$CompressiveStrength)^2))
RMSE # 7.96











