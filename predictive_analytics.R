library(caret)
library(e1071)

control <- trainControl(method="repeatedcv", number=10, repeats=3)
set.seed(123)
metric <- "Accuracy"
mtry <- 4
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(as.factor(label)~., data=mHealth.train.sampled, method="rf", metric=metric, tuneGrid=tunegrid)
print(rf_default)


# Source: https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/
customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
   predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
   predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes


tunegrid <- expand.grid(.mtry=c(4), .ntree=c(1000))
rf.1 <- train(as.factor(label)~., data=mHealth.train.sampled, method=customRF, metric=metric, tuneGrid=tunegrid)
print(rf.1)
#12000 samples
#   24 predictor
#   12 classes: '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12' 
#
#No pre-processing
#Resampling: Bootstrapped (25 reps) 
#Summary of sample sizes: 12000, 12000, 12000, 12000, 12000, 12000, ... 
#Resampling results:
#
#  Accuracy   Kappa    
#  0.9787984  0.9768679
#
#Tuning parameter 'mtry' was held constant at a value of 4
#Tuning parameter 'ntree' was held constant at a value of 500

rf.1.varImp <- varImp(rf.1)
plot(rf.1.varImp, main="Random Forest #1")


tunegrid <- expand.grid(.mtry=c(4), .ntree=c(1000))
rf.2 <- train(as.factor(label)~., data=mHealth.train.sampled, method=customRF, metric=metric, tuneGrid=tunegrid)
print(rf.2)
#12000 samples
#   24 predictor
#   12 classes: '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12' 
#
#No pre-processing
#Resampling: Bootstrapped (25 reps) 
#Summary of sample sizes: 12000, 12000, 12000, 12000, 12000, 12000, ... 
#Resampling results:
#
#  Accuracy   Kappa    
#  0.9747887  0.9724929
#
#Tuning parameter 'mtry' was held constant at a value of 10
#Tuning parameter 'ntree' was held constant at a value of 500

plot(rf.2.varImp)
plot(rf.2.varImp, main="Random Forest #2")


rf.3 <- train(as.factor(label)~., data=mHealth.train.sampled, method=customRF, metric=metric, tuneGrid=tunegrid)
print(rf.3)
#12000 samples
#   24 predictor
#   12 classes: '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12' 
#
#No pre-processing
#Resampling: Bootstrapped (25 reps) 
#Summary of sample sizes: 12000, 12000, 12000, 12000, 12000, 12000, ... 
#Resampling results:
#
#  Accuracy   Kappa    
#  0.9755452  0.9733184
#
#Tuning parameter 'mtry' was held constant at a value of 10
#Tuning parameter 'ntree' was held constant at a value of 500

rf.3.varImp <- varImp(rf.3)
plot(rf.3.varImp, main="Random Forest #3")










