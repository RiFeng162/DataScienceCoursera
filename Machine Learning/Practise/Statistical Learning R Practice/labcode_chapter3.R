# This file contains the Lab codes of Chapter 3 (Linear Regression), Statistical
# Learning

########## Simple Linear Regression #########
library(MASS)
library(ISLR)
data("Boston")

 # fit model
lm.fit <- lm(medv ~ lstat, data = Boston)
lm.fit
summary(lm.fit)
names(lm.fit) # check what imformation in the model
coef(lm.fit) # extract coefficients
confint(lm.fit) # confidence interval for coefficients
predict(lm.fit, data.frame(lstat = c(5,10,15)), interval = "confidence") # predict the CI

attach(Boston)
plot(lstat, medv)
abline(lm.fit)
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit)) # rstudent return studentized residuals
plot(hatvalues(lm.fit))

########## Multiple Linear Regression ###########
lm.fit <- lm(medv ~ lstat + age, data = Boston)
summary(lm.fit)
lm.fit <- lm(medv ~ ., data = Boston)
summary(lm.fit) # ?summary.lm to find what conponents are accessible 
library(car)
vif(lm.fit) # calculate vif
 
 # include interaction term
lm.fit <- lm(medv ~ lstat*age, data = Boston) # include main effects and interation
lm.fit <- lm(medv ~ lstat:age, data = Boston) # only interation 
summary(lm.fit)

 # non-linear transformation
lm.fit <- lm(medv ~ lstat + I(lstat^2))
lm.fit <- lm(medv ~ poly(lstat, 5))
summary(lm.fit)
detach(Boston)
 
 # quantitative variables
data("Carseats")
attach(Carseats)
lm.fit <- lm(Sales ~ ., data = Carseats) 
summary(lm.fit) # ShelveLoc is qualitative variables
contrasts(ShelveLoc) # check how R code dummy variable
