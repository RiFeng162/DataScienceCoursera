########## chapter 8 ##########
library(tree)
data("Carseats")
?Carseats

attach(Carseats)
High <- ifelse(Sales<=8, "No", "Yes")
Carseats <- cbind(Carseats, High)

# create classification tree
fit.tree <- tree(High ~ . -Sales, data = Carseats)
summary(fit.tree) # access tree information
fit.tree
plot(fit.tree) # visualize the tree
text(fit.tree, pretty = 0) # pretty == 0, let factor variables shows the level values

# use train/test method
train <- sample(1:nrow(Carseats), 200)
test.High <- High[-train]
test.Carseats <- Carseats[-train,]
fit.tree <- tree(High ~ . -Sales, data = Carseats, subset = train)
pred.High <- predict(fit.tree, test.Carseats, type = "class")
# type="class" return the predicted classes, otherwise the probability
table(prediction = pred.High, truth = test.High) # accuracy is 0.72

# use cross-validation
set.seed(2)
cv.carseats <- cv.tree(fit.tree, FUN = prune.misclass) # perform cv to decide complexity level
summary(cv.carseats)
cv.carseats # it's not a tree-object, hence we need other function to return a tree
plot(cv.carseats$k, cv.carseats$dev, type = "b")
prune.tree <- prune.misclass(fit.tree, best = 11) # prune the tree, for regression tree, use prune.tree
plot(prune.tree)
text(prune.tree, pretty = 0)
pred.High <- predict(prune.tree, test.Carseats, type = "class") # accuracy for prune.tree is 0.735
detach(Carseats)

######## Regression Tree ##########
library(MASS)
data("Boston")
train <- sample(1:nrow(Boston), nrow(Boston)/2)
training <- Boston[train,]
testing <- Boston[-train,]
test.y <- testing[,"medv"]
tree.boston <- tree(medv ~ ., data = Boston, subset = train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston, pretty = 0)

# use cross-validation 
cv.boston <- cv.tree(tree.boston)
cv.boston
plot(cv.boston$size, cv.boston$dev, type = "b") # optimal size is 7
prune.boston <- prune.tree(tree.boston, best = 7) # Residual mean deviance 17.96
pred.boston <- predict(tree.boston, testing)
true.boston <- testing[,"medv"]
plot(true.boston, pred.boston)
abline(1,0)
mean((pred.boston - true.boston)^2)

############ Bagging and Random Forest ##########
library(randomForest)
set.seed(1)

# create bagged tree while set mtry = # of predictors
bag.boston <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 13,
                           improtance = TRUE)
bag.boston # MSE is 12.2 less than pruned tree
pred.bag <- predict(bag.boston, testing)
plot(pred.bag, test.y)
abline(0,1)
mean((pred.bag - test.y)^2) # why test MSE less than train?

bag.boston <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 13,
                           ntree = 50) # ntree set B - number of trees to grow

# create random forest with set mtry < p
rf.boston <- randomForest(medv~., data = Boston, subset = train, mtry = 6,
                          importance = TRUE)
pred.boston <- predict(rf.boston, testing)
importance(rf.boston) # importance used to extract importance measures
varImpPlot(rf.boston) # visualize importance - shows lstat, rm are most important

########## Boosting ###########






























