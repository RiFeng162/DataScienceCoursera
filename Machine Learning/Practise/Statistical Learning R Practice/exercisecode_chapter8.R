############### chapter 8 ###############

# Exercise 7
mtry <- c(3,6,9,12)
ntree <- c(10, 50, 100, 500, 1000)

metric <- data.frame("mes"=numeric(),"mtry"=numeric(),"ntree"=numeric())
t <- 0
for (i in mtry) {
  for (j in ntree) {
    rf.tree <- randomForest(medv~., data = Boston, ntree = j, mtry = i)
    t <- t+1
    metric[t,"mes"] <- mean(rf.tree$mse)
    metric[t,"mtry"] <- i
    metric[t,"ntree"] <- j
  }
}
metric <- na.omit(metric)
metric$mtry <- as.factor(metric$mtry)
library(ggplot2)
p <- ggplot(metric, aes(x = ntree, y = mes, color = mtry)) + geom_line()
p
# it shows as trees become larger, the MSE decreases and be steady after 500.
# m = 6 get the optimal instead of the smallest m value.

# Exersice 8
# prepare data sets
data("Carseats")
attach(Carseats)
train <- sample(1:nrow(Carseats), nrow(Carseats)/2)
training <- Carseats[train,]
testing <- Carseats[-train,]
test.Sales <- Sales[-train]

# create regression tree
tree.carseats <- tree(Sales ~ ., data = Carseats, subset = train)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, pretty = 0)
pred.carseats <- predict(tree.carseats, newdata = testing)
mean((pred.carseats - test.Sales)^2) # MSE 5.265

# use cross-validation
cv.carseats <- cv.tree(tree.carseats)
plot(cv.carseats$size, cv.carseats$dev, type = "b")
 # shows cv will not prune the tree.

# use bagging
bag.carseats <- randomForest(Sales~., data = Carseats, subset = train,
                             mtry = 10, importance = TRUE)
bag.carseats # MSE 2.896
pred.carseats <- predict(bag.carseats, testing) # MSE 2.64
importance(bag.carseats) # ShelveLoc is most important

# use random forest
rf.carseats <- randomForest(Sales~., data = Carseats, subset = train, mtry = 4)
rf.carseats
pred.carseats <- predict(rf.carseats, testing) # MSE 2.648 almost the same as bagging
importance(rf.carseats)

metric <- data.frame("m" = integer(), "trainMSE" = numeric(), "testMSE" = numeric())
for(m in 1:10) {
  rf.carseats <- randomForest(Sales~., data = Carseats, subset = train, 
                              mtry = m)
  metric[m, "m"] <- m
  metric[m, "trainMSE"] <- mean(rf.carseats$mse)
  pred.carseats <- predict(rf.carseats, testing)
  metric[m, "testMSE"] <- mean((pred.carseats - test.Sales)^2)
}
attach(metric)
ggplot(metric , aes(x = m)) + geom_line(aes(y=trainMSE),color = "red") + geom_line(aes(y=testMSE))

# Exercise 9
attach(OJ)
sum(is.na(OJ)) # no NA
train <- sample(1:nrow(OJ), 800)
training <- OJ[train,]
testing <-OJ[-train,]
test.Purchase <- Purchase[-train]
tree.fit <- tree(Purchase~., data = training)
summary(tree.fit)
tree.fit # most important variable is LoyalCH
plot(tree.fit)
text(tree.fit, pretty = 0)
cv.oj <- cv.tree(tree.fit, FUN = prune.misclass)
cv.oj # best model with 11 leaves
plot(cv.oj$size, cv.oj$dev, type = "b")
prune.tree <- prune.misclass(tree.fit, best = 5)
summary(prune.tree)
pred.Purchase <- predict(tree.fit, newdata = testing, type = "class")
# accuracy is 0.796
table(prediction = pred.Purchase, truth = test.Purchase)
pred.Purchase <- predict(prune.tree, newdata = testing, type = "class")
