# This file contains the Lab code for chapter 4 (Classification) of Statistical Learning

 # prepare data
library(ISLR)
data("Smarket")
names(Smarket)
cor(Smarket[,-9]) # calculate the correlation matrix
attach(Smarket)
plot(Volume)

######### Logistic Regression ###########
glm.fit <- glm(Direction ~ . - Today - Year, data = Smarket, family = "binomial")
  # Generalized Linear Model, binomial specifies it's Logistic Regression
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef
glm.probs <- predict(glm.fit, type = "response")
 # type = "response" return the value in response scale(probability), the default in predictors 
 # scale(logit). The probability is P(Y=1|X), by contrasts() it show Up is coded as 1
contrasts(Direction)
 # transfer probability to class labels
glm.pred <- rep("Down", length(Direction))
glm.pred[glm.probs > 0.5] <- "Up"
table(glm.pred, Direction)
mean(glm.pred == Direction) # compute accuracy

######### Linear Discriminant Analysis ########
library(MASS)
lda.fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket)
lda.fit
plot(lda.fit)
lda.pred <- predict(lda.fit) 
 # contains 3 parts:class-predicted class, posterior-predicted probability(this can
 # be used to create different cut-offs)
 # x-linear discriminants (used in discriminant function)

 # Quadratic Discriminat Analysis
qda.fit <- qda(Direction ~ Lag1 + Lag2, data = Smarket)
qda.fit
qda.pred <- predict(qda.fit)

########## K-Neareat Neighbors #########
library(class)
train <- Year<2005
train.X <- cbind(Lag1, Lag2)[train,]
test.X <- cbind(Lag1, Lag2)[!train,]
train.y <- Direction[train]
 # create knn 
knn.pred <- knn(train = train.X, test = test.X, cl = train.y, k = 3) 
 # this function combinds model construction and prediction as one
 # it returns the predicted values
table(knn.pred, Direction[!train])
library(caret)
confusionMatrix(knn.pred, Direction[!train])
detach(Smarket)





