# This file contains the code to implement the practise of caret tutorial.
# The link for the tutorial is : https://www.machinelearningplus.com/machine-learning/caret-package/

data(OJ)
head(OJ[,1:10]) # response is the 1st variable - Purchase
 # split into training and testing
intrain <- createDataPartition(OJ$Purchase, p = 0.9, list = FALSE)
training <- OJ[intrain,]
testing <- OJ[-intrain,]
x <- training[,-1]
y <- training[, 1]

 # make some descriptive analysis
library(skimr)
skimmed <- skim_to_wide(training)
View(skimmed)

 # pre-processing
# For NA, 1. compute mean for quantitative, 2. most frequent
# class for qualitative. 3. KNN-use other predicots
preprocess_missing_model <- preProcess(training, method = "knnImpute")
preprocess_missing_model
training <- predict(preprocess_missing_model, training)
# For qualitative predictors, create dummy variables
dummy_model <- dummyVars(Purchase ~ ., training) # similar to model matrix
training_matrix <- predict(dummy_model, training)
training <- data.frame(training_matrix)
# Transform predictors to be at range 0-1
preprocess_range_model <- preProcess(training, method = "range")
training <- predict(preprocess_range_model, training)
training$Purchase <- y

 # variable selection
# Show the X-mean for each response class
featurePlot(x = training[,1:18], 
            y = training$Purchase,
            plot = "density",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))
# Using rfe(Recursive Feature Elimination) - backward stepwise
set.seed(100)
subsets <- c(1:5, 10, 15, 18) # number of predictors to be contained
ctrl <- rfeControl(functions = rfFuncs,     # use random forest
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)         # controls what algorithm to use and cv
ft.select <- rfe(x = training[,1:18],
                 y = training$Purchase,
                 sizes = subsets,
                 rfeControl = ctrl)
ft.select # a rfe-object
predictors(ft.select) # extract the predictors
plot(ft.select, type = "b") # visulize the model

 # tune and train the model
model.names <- paste(names(getModelInfo()), collapse = ", ")
model.names # access all models in train
View(modelLookup("earth")) # check hyperparameters of specific algorithm
# create MARA model 
model.mars <- train(Purchase ~ ., data = training, method = "earth")
mars.pred <- predict(model.mars)
model.mars
plot(model.mars)
# in train(), other things we can do:
# 1. set pre-process
# 2. set cross-validation method
# 3. tune the hyper parameters
# 4. set metrics to optimize the algorithm

# compute variable importance
varimp.mars <- varImp(model.mars)
plot(varimp.mars)

 # predict on test set
# Test set should be processed as the train set does
testing <- predict(preprocess_missing_model, testing)
testing <- predict(dummy_model, testing)
testing <- predict(preprocess_range_model, testing)
testing <- data.frame(testing)
View(head(testing))
head(testing)
test.pred <- predict(model.mars, testing)
confusionMatrix(reference = OJ[-intrain, "Purchase"], # set true results
                data = test.pred,
                mode = "everything",
                positive = "MM")                      # set positive level

 # Tune hyper parameters
# trainControl can set two things:
# 1. cross-validation method
# 2. results to be summarized
# Two way to tune parameters: 1. tuneLength; 2. tuneGrid
fitControl <- trainControl(method = "cv",               # set cross-validation as K-fold
                           number = 5,                  # K = 5
                           savePredictions = "final",   # only save the final prediction
                           classProbs = T,
                           summaryFunction = twoClassSummary)
set.seed(100)
model.mars2 <- train(Purchase ~ ., data = training,
                     method = "earth",
                     tuneLength = 5,         # how many levels to try hyper parameters
                     metric = "ROC",         # set metric for selecting best model
                     trControl = fitControl)
model.mars2
mars.grid <- expand.grid(nprune = c(2,4,6,8,10),
                         degree = c(1,2,3))
model.mars3 <- train(Purchase ~ ., data = training,
                     method = "earth",
                     tuneGrid = mars.grid,
                     metric = "ROC",
                     trControl = fitControl)
model.mars3
test.pred <- predict(model.mars3, testing)

 # compare performance among multiple algorithms
model.adaboost <- train(Purchase ~ ., data =training, method = "adaboost", tuneLength = 2, trControl = fitControl, metric = "ROC")
model.rf <- train(Purchase ~ ., data =training, method = "rf", tuneLength = 5, trControl = fitControl, metric = "ROC")
# model.xgbDart <- train(Purchase ~ ., data =training, method = "xgbDART", tuneLength = 5, trControl = fitControl, metric = "ROC")
model.svmRadial <- train(Purchase ~ ., data =training, method = "svmRadial", tuneLength = 15, trControl = fitControl, metric = "ROC")
model.compare <- resamples(list(ADABOOST = model.adaboost,
                                RF = model.rf,
                                MARS = model.mars3,
                                SVM = model.svmRadial))
summary(model.compare)
scales <- list(x = list(relation = "free"),
               y = list(relation = "free"))
bwplot(model.compare, scales = scales)
# Ensembling multiple models as one
# the process above can be combined with a simple command
library(caretEnsemble)
tctrl <- trainControl(method = "repeatedcv",
                      number = 10,
                      repeats = 3, 
                      savePredictions = TRUE,
                      classProbs = TRUE)
algorithmList <- c("rf", "adaboost", "earth", "svmRadial")
set.seed(100)
model.ensemble <- caretList(Purchase ~ ., data = training,
                            trControl = tctrl, methodList = algorithmList)
results <- resamples(model.ensemble)
summary(results)
bwplot(results, scales = scales)

 # combine multiple models
set.seed(101)
stack.control <- trainControl(method = "repeatedcv",
                              number = 10, 
                              repeats = 3, 
                              savePredictions = TRUE, 
                              classProbs = TRUE)
stack.glm <- caretStack(model.ensemble, method = "glm",
                        metric = "Accuracy", 
                        trControl = stack.control)
stack.glm # ensembles performs better when models are less correlated
stack.pred <- predict(stack.glm, testing)

























