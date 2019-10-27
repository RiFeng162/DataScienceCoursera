# This file contians the code for Quiz 4 project, Machine Learning of Data Science.

 # import data
library(caret)
data.raw <- read.csv("data/pml-training.csv", 
                     row.names = 1,
                     stringsAsFactors = FALSE)
data.test <- read.csv("data/pml-testing.csv")
dim(data.raw) # 19622 159 (1 is used as row names)
View(sapply(data.raw, class) )
sum(complete.cases(data.raw))   # only 406 observations are complete
    # data can seperate into 2 parts: 38*4(data from forearm,
    # arm, belt, dumbell) + 8(other variables)

###### data preprocessing ######
data.y <- as.factor(data.raw$classe)
    # choose only predictors related with body movement
data.x <- sapply(data.raw[, 7:158], 
                 function(x) if(is.numeric(x)) x else as.numeric(x))
    # in this process, values == #DIV/0! is converted as NA
data.x <- as.data.frame(data.x)

# impute missing data
preprocess.na <- preProcess(data.x, method = "knnImpute")
    # knnImpute cannot be done since not enough data for NA
na_ratio <- sapply(data.x, 
                   function(x) round(sum(is.na(x))/dim(data.x)[1] , 2))
View(na_ratio)
sum(na_ratio > 0.95) # 100 variables have over 95% NAs
data.newx <- data.x[, na_ratio < 0.95] # new data has 52 variables
sum(is.na(data.newx)) # 0, no NA hence no need to impute missing values
    # calculate variable
cor_mat <- cor(data.newx)
sum(cor_mat[lower.tri(cor_mat)]>0.7) # 22 pairs of variables has correlation > 0.7
    # run pca preProcess
preprocess.pca <- preProcess(data.newx, method = "pca", thresh = 0.95 )
preprocess.pca 
data.pcax <- predict(preprocess.pca, data.newx)

###### model fitting ######
# choose lda, rf, svm
training <- data.frame(classe = data.y, data.pcax)
training_1000 <- training[sample(1:dim(training)[1], 1000), ]
training_2000 <- training[sample(1:dim(training)[1], 2000), ]
training_3000 <- training[sample(1:dim(training)[1], 3000), ]
training_4000 <- training[sample(1:dim(training)[1], 4000), ]


fit.ctrl <- trainControl(method = "cv",
                         number = 10,
                         savePredictions = "final",
                         classProbs = TRUE)
tic("running time for RF with 4000 observation")
fit.rf <- train(classe ~ ., data = training_5000, 
                 subset = training_1000,
                method = "rf", #tuneLength = 5,
                trControl = fit.ctrl)
toc()
# (observations, tune) - (time, accuracy)
# (1000,3) - (30,0.78), (2000,3) - (98,0.86), 
# (3000,3) - (120,0.90), (4000,3) - (240,0.91)

tic("running time for svmRadial tune 10")
fit.svmRadial <- train(classe ~ ., data = training_3000,
                       method = "svmRadial",
                       tuneLength = 10,
                       trControl = fit.ctrl)
toc()
# (3000,3) - (82,0.71), (3000,5) - (116,0.76)
# (3000,10) - (209，0.78)
tic("running time for lda default tune")
fit.lda <- train(classe ~ ., data = training_3000,
                 method = "lda",
                 trControl = fit.ctrl)
toc()
# (3000, ) - (1.5, 0.54), (3000, 10) - (1.2, 0.54)
# decision boundary is not linear

fit.compare <- resamples(list(RF = fit.rf,
                              SVM = fit.svmRadial,
                              LDA = fit.lda))
summary(fit.compare)
bwplot(fit.compare)
var.impt <- varImp(fit.rf)
plot(fit.rf) # it's
plot(var.impt)

subsets <- c(5, 10, 15, 20,23:25) # number of predictors to be contained
ctrl <- rfeControl(functions = rfFuncs,     # use random forest
                   method = "cv")         # controls what algorithm to use and cv
tic("running time for rfe variable selection")
ft.select <- rfe(x = training_3000[,-1],
                 y = training_3000[, 1],
                 sizes = subsets,
                 rfeControl = ctrl)
toc()
# 1000 - (111, 23-0.797), 3000 - (432, 24-0.88)

###### apply on testing set ######
  # preprocess on testing set
  # test set has no classe, instead is problem_id
test.x <- sapply(data.test[, 7:158],    # only this subset contains body movement data 
                 function(x) if(is.numeric(x)) x else as.numeric(x))
test.x <- as.data.frame(test.x)
test.newx <- test.x[, na_ratio < 0.95]
test.pcax <- predict(preprocess.pca, test.newx)

 
 
 