# This file contains code for practicing caretEnsemble tutorial.
# The link is https://cran.r-project.org/web/packages/caretEnsemble/vignettes/caretEnsemble-intro.html

library(caret)
library(mlbench)
library(pROC)
data("Sonar")
dim(Sonar) # 208 61
str(Sonar) # last column is the response Class
set.seed(107)
inTrain <- createDataPartition(Sonar$Class, p = 0.75, list = FALSE)
training <- Sonar[inTrain,]
testing <- Sonar[-inTrain, ]
 # create model list with caretList
train.ctrl <- trainControl(method = "boot",
                           number = 25, 
                           savePredictions = "final",
                           classProbs = TRUE,
                           index = createResample(training$Class, 25),
                           # define row index for each resample, make sure each method is trained with same data
                           summaryFunction = twoClassSummary)

library(rpart)
library(caretEnsemble)
model.list <- caretList(Class ~ ., data = training, 
                        trControl = train.ctrl,
                        methodList = c("glm", "rpart"))
model.pred <- as.data.frame(predict(model.list, testing))
# specify tuning parameters for each model
library(randomForest)
library(nnet)
model.list2 <- caretList(Class ~ ., data = training, 
                         trControl = train.ctrl, 
                         metric = "ROC", 
                         methodList = c("rpart", "glm"), 
                         tuneList = list(
                           rf1 = caretModelSpec(method = "rf", tuneGrid = data.frame(.mtry = 2)),
                           rf2 = caretModelSpec(method = "rf", tuneGrid = data.frame(.mtry = 10), preProcess = "pca"),
                           nn = caretModelSpec(method = "nnet", tuneLength = 2, trace = FALSE)
                           # components in caretModelSpec are passed to train()
                           ))
xyplot(resamples(model.list))
modelCor(resamples(model.list)) # correlation among models
    # models with low correlation is suitable to ensemble

 # combine models with caretEnsemble
greedy_ensemble <- caretEnsemble(model.list, metric = "ROC",
                                 trControl = trainControl(
                                   number = 2, 
                                   summaryFunction = twoClassSummary,
                                   classProbs = TRUE
                                 ))
summary(greedy_ensemble) # ensembled model has higher ROC
# check with testing data
library(caTools)
list.pred <- lapply(model.list, predict, newdata = testing, type = "prob")
list.pred <- lapply(list.pred, function(x) x[,"M"])
list.pred <- data.frame(list.pred)
ensm.pred <- predict(greedy_ensemble, testing, type = "prob")
list.pred$ensemble <- ensm.pred
caTools::colAUC(list.pred, testing$Class)
varImp(greedy_ensemble)

 # combine model with caretStack
# caretStack let us specify the way to combine the models
model.stack <- caretStack(model.list,
                          metric = "ROC", 
                          method = "glm",  # the method to combind models
                          trControl = trainControl(
                            method = "boot",
                            number = 10,
                            savePredictions = "final",
                            classProbs = TRUE,
                            summaryFunction = twoClassSummary
                          ))























