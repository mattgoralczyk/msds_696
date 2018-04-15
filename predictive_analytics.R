library(caret)
library(e1071)

#
# Random Forests
#

control <- trainControl(method="repeatedcv", number=10, repeats=3)
set.seed(123)
metric <- "Accuracy"
mtry <- 4
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(as.factor(label)~., data=mHealth.train, method="rf", metric=metric, tuneGrid=tunegrid)
print(rf_default)

library(randomForest)
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


tunegrid <- expand.grid(.mtry=c(4), .ntree=c(500))
rf.1 <- train(as.factor(label)~., data=mHealth.train, method=customRF, metric=metric, tuneGrid=tunegrid)
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
#  0.9792981  0.9774132
#
#Tuning parameter 'mtry' was held constant at a value of 4
#Tuning parameter 'ntree' was held constant at a value of 500

rf.1.varImp <- varImp(rf.1)
plot(rf.1.varImp, main="Random Forest #1")

rf.1.predict <- predict(rf.1, newdata=mHealth.test)
rf.1.prediction <- matrix(rf.1.predict, nrow=12, ncol=length(rf.1.predict)/12)
confusionMatrix(rf.1.prediction, mHealth.test$label)
#Confusion Matrix and Statistics
#
#          Reference
#Prediction    1    2    3    4    5    6    7    8    9   10   11   12
#        1  1000    0    0    0    0    1    0    0    0    0    0    0
#        2     0 1000    0    0    0    0    0    0    0    0    0    0
#        3     0    0 1000    0    0    0    0    0    0    0    0    0
#        4     0    0    0  997   10    0    0    0    0    0    0    1
#        5     0    0    0    3  982    5    0    7    0    0    0    0
#        6     0    0    0    0    0  985    0    5    1    0    0    0
#        7     0    0    0    0    0    5 1000    4    0    0    0    0
#        8     0    0    0    0    2    4    0  984    3    0    0    0
#        9     0    0    0    0    0    0    0    0  996    0    0    0
#        10    0    0    0    0    3    0    0    0    0  917   16    5
#        11    0    0    0    0    0    0    0    0    0   77  984   12
#        12    0    0    0    0    3    0    0    0    0    6    0  982
#
#Overall Statistics
#                                          
#               Accuracy : 0.9856          
#                 95% CI : (0.9833, 0.9876)
#    No Information Rate : 0.0833          
#    P-Value [Acc > NIR] : < 2.2e-16       
#                                          
#                  Kappa : 0.9843          
# Mcnemar's Test P-Value : NA              
#
#Statistics by Class:
#
#                     Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9 Class: 10 Class: 11 Class: 12
#Sensitivity           1.00000  1.00000  1.00000  0.99700  0.98200  0.98500  1.00000  0.98400  0.99600   0.91700   0.98400   0.98200
#Specificity           0.99991  1.00000  1.00000  0.99900  0.99864  0.99945  0.99918  0.99918  1.00000   0.99782   0.99191   0.99918
#Pos Pred Value        0.99900  1.00000  1.00000  0.98909  0.98495  0.99395  0.99108  0.99094  1.00000   0.97450   0.91705   0.99092
#Neg Pred Value        1.00000  1.00000  1.00000  0.99973  0.99836  0.99864  1.00000  0.99855  0.99964   0.99249   0.99854   0.99836
#Prevalence            0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333   0.08333   0.08333   0.08333
#Detection Rate        0.08333  0.08333  0.08333  0.08308  0.08183  0.08208  0.08333  0.08200  0.08300   0.07642   0.08200   0.08183
#Detection Prevalence  0.08342  0.08333  0.08333  0.08400  0.08308  0.08258  0.08408  0.08275  0.08300   0.07842   0.08942   0.08258
#Balanced Accuracy     0.99995  1.00000  1.00000  0.99800  0.99032  0.99223  0.99959  0.99159  0.99800   0.95741   0.98795   0.99059




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
#  0.9797447  0.9779007
#
#Tuning parameter 'mtry' was held constant at a value of 4
#Tuning parameter 'ntree' was held constant at a value of 1000

rf.2.varImp <- varImp(rf.2)
plot(rf.2.varImp, main="Random Forest #2")

rf.2.predict <- predict(rf.2, newdata=mHealth.test)
rf.2.prediction <- matrix(rf.2.predict, nrow=12, ncol=length(rf.1.predict)/12)
confusionMatrix(rf.2.prediction, mHealth.test$label)
#Confusion Matrix and Statistics
#
#          Reference
#Prediction    1    2    3    4    5    6    7    8    9   10   11   12
#        1  1000    0    0    0    0    0    0    0    0    0    0    0
#        2     0 1000    0    0    0    0    0    0    0    0    0    0
#        3     0    0 1000    0    0    0    0    0    0    0    0    0
#        4     0    0    0  997   11    0    0    0    0    0    0    3
#        5     0    0    0    3  980    5    0    6    0    0    0    2
#        6     0    0    0    0    0  987    0    6    1    0    0    0
#        7     0    0    0    0    0    5 1000    1    0    0    0    0
#        8     0    0    0    0    2    3    0  987    1    0    0    0
#        9     0    0    0    0    0    0    0    0  998    0    0    0
#        10    0    0    0    0    4    0    0    0    0  926   16    6
#        11    0    0    0    0    0    0    0    0    0   69  984   11
#        12    0    0    0    0    3    0    0    0    0    5    0  978
#
#Overall Statistics
#                                          
#               Accuracy : 0.9864          
#                 95% CI : (0.9842, 0.9884)
#    No Information Rate : 0.0833          
#    P-Value [Acc > NIR] : < 2.2e-16       
#                                          
#                  Kappa : 0.9852          
# Mcnemar's Test P-Value : NA              
#
#Statistics by Class:
#
#                     Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9 Class: 10 Class: 11 Class: 12
#Sensitivity           1.00000  1.00000  1.00000  0.99700  0.98000  0.98700  1.00000  0.98700  0.99800   0.92600   0.98400   0.97800
#Specificity           1.00000  1.00000  1.00000  0.99873  0.99855  0.99936  0.99945  0.99945  1.00000   0.99764   0.99273   0.99927
#Pos Pred Value        1.00000  1.00000  1.00000  0.98615  0.98394  0.99296  0.99404  0.99396  1.00000   0.97269   0.92481   0.99189
#Neg Pred Value        1.00000  1.00000  1.00000  0.99973  0.99818  0.99882  1.00000  0.99882  0.99982   0.99330   0.99854   0.99800
#Prevalence            0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333   0.08333   0.08333   0.08333
#Detection Rate        0.08333  0.08333  0.08333  0.08308  0.08167  0.08225  0.08333  0.08225  0.08317   0.07717   0.08200   0.08150
#Detection Prevalence  0.08333  0.08333  0.08333  0.08425  0.08300  0.08283  0.08383  0.08275  0.08317   0.07933   0.08867   0.08217
#Balanced Accuracy     1.00000  1.00000  1.00000  0.99786  0.98927  0.99318  0.99973  0.99323  0.99900   0.96182   0.98836   0.98864



tunegrid <- expand.grid(.mtry=c(10), .ntree=c(500))
rf.3 <- train(as.factor(label)~., data=mHealth.train, method=customRF, metric=metric, tuneGrid=tunegrid)
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
#  0.9750191  0.9727443
#
#Tuning parameter 'mtry' was held constant at a value of 10
#Tuning parameter 'ntree' was held constant at a value of 500

rf.3.varImp <- varImp(rf.3)
plot(rf.3.varImp, main="Random Forest #3")


rf.3.predict <- predict(rf.3, newdata=mHealth.test)
rf.3.prediction <- matrix(rf.3.predict, nrow=12, ncol=length(rf.1.predict)/12)
confusionMatrix(rf.3.prediction, mHealth.test$label)
#Confusion Matrix and Statistics
#
#          Reference
#Prediction    1    2    3    4    5    6    7    8    9   10   11   12
#        1  1000    0    0    0    0    1    4    1    0    0    0    0
#        2     0 1000    0    0    0    0    0    0    0    0    0    0
#        3     0    0 1000    0    0    0    0    0    0    0    0    0
#        4     0    0    0  996   15    0    0    0    0    0    0    2
#        5     0    0    0    2  972    8    0    9    0    0    0    2
#        6     0    0    0    1    1  976    1   14    1    0    0    0
#        7     0    0    0    0    0   10  994    8    0    0    0    0
#        8     0    0    0    0    3    5    1  967    3    0    0    0
#        9     0    0    0    0    0    0    0    1  996    0    0    0
#        10    0    0    0    1    4    0    0    0    0  926   21   13
#        11    0    0    0    0    0    0    0    0    0   63  978   13
#        12    0    0    0    0    5    0    0    0    0   11    1  970
#
#Overall Statistics
#                                          
#               Accuracy : 0.9812          
#                 95% CI : (0.9787, 0.9836)
#    No Information Rate : 0.0833          
#    P-Value [Acc > NIR] : < 2.2e-16       
#                                          
#                  Kappa : 0.9795          
# Mcnemar's Test P-Value : NA              
#
#Statistics by Class:
#
#                     Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9 Class: 10 Class: 11 Class: 12
#Sensitivity           1.00000  1.00000  1.00000  0.99600  0.97200  0.97600  0.99400  0.96700  0.99600   0.92600   0.97800   0.97000
#Specificity           0.99945  1.00000  1.00000  0.99845  0.99809  0.99836  0.99836  0.99891  0.99991   0.99645   0.99309   0.99845
#Pos Pred Value        0.99404  1.00000  1.00000  0.98322  0.97885  0.98189  0.98221  0.98774  0.99900   0.95959   0.92789   0.98278
#Neg Pred Value        1.00000  1.00000  1.00000  0.99964  0.99746  0.99782  0.99945  0.99701  0.99964   0.99329   0.99799   0.99728
#Prevalence            0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333   0.08333   0.08333   0.08333
#Detection Rate        0.08333  0.08333  0.08333  0.08300  0.08100  0.08133  0.08283  0.08058  0.08300   0.07717   0.08150   0.08083
#Detection Prevalence  0.08383  0.08333  0.08333  0.08442  0.08275  0.08283  0.08433  0.08158  0.08308   0.08042   0.08783   0.08225
#Balanced Accuracy     0.99973  1.00000  1.00000  0.99723  0.98505  0.98718  0.99618  0.98295  0.99795   0.96123   0.98555   0.98423



#
# xgboost
#

library(xgboost)
library(dplyr)

mHealth.train.xgb.labels <- mHealth.train$label
mHealth.test.xgb.labels <- mHealth.test$label

mHealth.train.xgb.labels[mHealth.train.xgb.labels==1] <- 0
mHealth.train.xgb.labels[mHealth.train.xgb.labels==2] <- 1
mHealth.train.xgb.labels[mHealth.train.xgb.labels==3] <- 2
mHealth.train.xgb.labels[mHealth.train.xgb.labels==4] <- 3
mHealth.train.xgb.labels[mHealth.train.xgb.labels==5] <- 4
mHealth.train.xgb.labels[mHealth.train.xgb.labels==6] <- 5
mHealth.train.xgb.labels[mHealth.train.xgb.labels==7] <- 6
mHealth.train.xgb.labels[mHealth.train.xgb.labels==8] <- 7
mHealth.train.xgb.labels[mHealth.train.xgb.labels==9] <- 8
mHealth.train.xgb.labels[mHealth.train.xgb.labels==10] <- 9
mHealth.train.xgb.labels[mHealth.train.xgb.labels==11] <- 10
mHealth.train.xgb.labels[mHealth.train.xgb.labels==12] <- 11

mHealth.test.xgb.labels[mHealth.test.xgb.labels==1] <- 0
mHealth.test.xgb.labels[mHealth.test.xgb.labels==2] <- 1
mHealth.test.xgb.labels[mHealth.test.xgb.labels==3] <- 2
mHealth.test.xgb.labels[mHealth.test.xgb.labels==4] <- 3
mHealth.test.xgb.labels[mHealth.test.xgb.labels==5] <- 4
mHealth.test.xgb.labels[mHealth.test.xgb.labels==6] <- 5
mHealth.test.xgb.labels[mHealth.test.xgb.labels==7] <- 6
mHealth.test.xgb.labels[mHealth.test.xgb.labels==8] <- 7
mHealth.test.xgb.labels[mHealth.test.xgb.labels==9] <- 8
mHealth.test.xgb.labels[mHealth.test.xgb.labels==10] <- 9
mHealth.test.xgb.labels[mHealth.test.xgb.labels==11] <- 10
mHealth.test.xgb.labels[mHealth.test.xgb.labels==12] <- 11

xgb_params <- list("objective"="multi:softprob", "eval_metric"="mlogloss", "num_class"=12)
mHealth.train.xgb <- xgb.DMatrix(data=as.matrix(mHealth.train[,1:23]), label=mHealth.train.xgb.labels)
mHealth.test.xgb <- xgb.DMatrix(data=as.matrix(mHealth.test[,1:23]), label=mHealth.test.xgb.labels)
xg.1 <- xgb.cv(params=xgb_params, data=mHealth.train.xgb, nrounds=50, nfold=5, verbose=FALSE, prediction=TRUE)

# https://rpubs.com/mharris/multiclass_xgboost

xg1.prediction <- data.frame(xg.1$pred) %>%
mutate(max_prob=max.col(., ties.method="last"), label=mHealth.train.xgb.labels+1)

head(xg1.prediction)
confusionMatrix(factor(xg1.prediction$max_prob), factor(xg1.prediction$label), mode="everything")
#Confusion Matrix and Statistics
#
#          Reference
#Prediction   1   2   3   4   5   6   7   8   9  10  11  12
#        1  995   0   0   0   0   1   5   1   0   0   0   0
#        2    1 999   0   0   0   0   0   0   0   0   0   0
#        3    0   0 999   0   0   0   0   0   0   0   1   1
#        4    0   0   0 989  16   0   0   0   0   0   0   7
#        5    1   1   0   8 961   4   0  12   1   0   0   1
#        6    2   0   0   1   2 986   1   5   0   0   0   1
#        7    1   0   0   0   0   7 992   8   0   0   0   0
#        8    0   0   0   1  13   2   2 971   2   0   0   0
#        9    0   0   0   0   1   0   0   0 996   0   1   0
#        10   0   0   1   0   4   0   0   0   1 951  27  20
#        11   0   0   0   1   1   0   0   0   0  40 963  13
#        12   0   0   0   0   2   0   0   3   0   9   8 957
#
#Overall Statistics
#                                          
#               Accuracy : 0.9799          
#                 95% CI : (0.9772, 0.9824)
#    No Information Rate : 0.0833          
#    P-Value [Acc > NIR] : < 2.2e-16       
#                                          
#                  Kappa : 0.9781          
# Mcnemar's Test P-Value : NA              
#
#Statistics by Class:
#
#                     Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9 Class: 10 Class: 11 Class: 12
#Sensitivity           0.99500  0.99900  0.99900  0.98900  0.96100  0.98600  0.99200  0.97100  0.99600   0.95100   0.96300   0.95700
#Specificity           0.99936  0.99991  0.99982  0.99791  0.99745  0.99891  0.99855  0.99818  0.99982   0.99518   0.99500   0.99800
#Pos Pred Value        0.99301  0.99900  0.99800  0.97727  0.97169  0.98798  0.98413  0.97982  0.99800   0.94721   0.94597   0.97753
#Neg Pred Value        0.99955  0.99991  0.99991  0.99900  0.99646  0.99873  0.99927  0.99737  0.99964   0.99554   0.99663   0.99610
#Precision             0.99301  0.99900  0.99800  0.97727  0.97169  0.98798  0.98413  0.97982  0.99800   0.94721   0.94597   0.97753
#Recall                0.99500  0.99900  0.99900  0.98900  0.96100  0.98600  0.99200  0.97100  0.99600   0.95100   0.96300   0.95700
#F1                    0.99401  0.99900  0.99850  0.98310  0.96631  0.98699  0.98805  0.97539  0.99700   0.94910   0.95441   0.96716
#Prevalence            0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333   0.08333   0.08333   0.08333
#Detection Rate        0.08292  0.08325  0.08325  0.08242  0.08008  0.08217  0.08267  0.08092  0.08300   0.07925   0.08025   0.07975
#Detection Prevalence  0.08350  0.08333  0.08342  0.08433  0.08242  0.08317  0.08400  0.08258  0.08317   0.08367   0.08483   0.08158
#Balanced Accuracy     0.99718  0.99945  0.99941  0.99345  0.97923  0.99245  0.99527  0.98459  0.99791   0.97309   0.97900   0.97750


xgb_params <- list("objective"="multi:softprob", "eval_metric"="mlogloss", "num_class"=12)
xg.1 <- xgb.train(params=xgb_params, data=mHealth.train.xgb, nrounds=2)
mHealth.test.xgb <- xgb.DMatrix(data=as.matrix(mHealth.test[,1:23]), label=mHealth.test.xgb.labels)
xg.1.test.predict <- predict(xg.1, newdata=mHealth.test.xgb)
xg.1.test.prediction <- matrix(xg.1.test.predict, nrow=12, ncol=length(xg.1.test.predict)/12) %>%
t() %>%
data.frame() %>%
mutate(label=mHealth.test.xgb.labels+1, max_prob=max.col(., "last"))
confusionMatrix(factor(xg.1.test.prediction$max_prob), factor(xg.1.test.prediction$label), mode="everything")
#Confusion Matrix and Statistics
#
#          Reference
#Prediction    1    2    3    4    5    6    7    8    9   10   11   12
#        1   978    0    0    2    2    9   21   19    0    0    0    0
#        2     0  996    0    0    5    0    1    0    0    0    0    0
#        4     0    0    0  954  108    1    0   56    0    7    8   17
#        5     1    1    0   23  772   23    2   20    2    6    6    8
#        6     4    0    0    7   23  898   34   49    1    1    0    9
#        7     9    3    0    1    3   22  910   24    0   11   12    5
#        8     8    0    0    2   56   46   20  809   12    3    0   14
#        9     0    0    0    0    4    0    4    2  979    2    2    0
#        10    0    0    0    2   11    0    6   14    1  798   59   78
#        11    0    0    0    6    0    0    0    0    0  113  855   56
#        12    0    0    0    3   16    1    2    7    5   59   58  812
#
#Overall Statistics
#                                          
#               Accuracy : 0.8968          
#                 95% CI : (0.8912, 0.9021)
#    No Information Rate : 0.0833          
#    P-Value [Acc > NIR] : < 2.2e-16       
#                                          
#                  Kappa : 0.8874          
# Mcnemar's Test P-Value : NA              
#
#Statistics by Class:
#
#                     Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9 Class: 10 Class: 11 Class: 12
#Sensitivity           0.97800  0.99600  1.00000  0.95400  0.77200  0.89800  0.91000  0.80900  0.97900   0.79800   0.85500   0.81200
#Specificity           0.99518  0.99945  0.99991  0.98209  0.99164  0.98836  0.99182  0.98536  0.99873   0.98445   0.98409   0.98627
#Pos Pred Value        0.94859  0.99401  0.99900  0.82884  0.89352  0.87524  0.91000  0.83402  0.98590   0.82353   0.83010   0.84320
#Neg Pred Value        0.99799  0.99964  1.00000  0.99576  0.97953  0.99071  0.99182  0.98268  0.99809   0.98169   0.98678   0.98297
#Precision             0.94859  0.99401  0.99900  0.82884  0.89352  0.87524  0.91000  0.83402  0.98590   0.82353   0.83010   0.84320
#Recall                0.97800  0.99600  1.00000  0.95400  0.77200  0.89800  0.91000  0.80900  0.97900   0.79800   0.85500   0.81200
#F1                    0.96307  0.99500  0.99950  0.88703  0.82833  0.88648  0.91000  0.82132  0.98244   0.81056   0.84236   0.82731
#Prevalence            0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333   0.08333   0.08333   0.08333
#Detection Rate        0.08150  0.08300  0.08333  0.07950  0.06433  0.07483  0.07583  0.06742  0.08158   0.06650   0.07125   0.06767
#Detection Prevalence  0.08592  0.08350  0.08342  0.09592  0.07200  0.08550  0.08333  0.08083  0.08275   0.08075   0.08583   0.08025
#Balanced Accuracy     0.98659  0.99773  0.99995  0.96805  0.88182  0.94318  0.95091  0.89718  0.98886   0.89123   0.91955   0.89914

xgb.names <- colnames(mHealth.train[,-1])
xg.1.imp.matrix <- xgb.importance(feature_names=xgb.names, model=xg.1)
head(xg.1.imp.matrix)
#          Feature       Gain      Cover  Frequency
#1:           ec_1 0.16338905 0.06921126 0.07190413
#2:  accel_r_arm_z 0.09167633 0.08697418 0.06790945
#3: gyro_l_ankle_x 0.08221461 0.07509602 0.06391478
#4:  accel_r_arm_y 0.06413648 0.08386557 0.06524634
#5:  mag_l_ankle_y 0.06390988 0.07028279 0.05725699
#6:   gyro_r_arm_y 0.06370102 0.04972227 0.05326232
xgb.ggplot.importance(xg.1.imp.matrix)




#
# SVM
#
svm.1 <- svm(as.factor(mHealth.train$label)~., data=mHealth.train[,1:23])
svm.1.predict <- predict(svm.1, newdata=mHealth.test)
svm.1.prediction <- matrix(svm.1.predict, nrow=12, ncol=length(svm.1.predict)/12)
confusionMatrix(svm.1.matrx, as.factor(mHealth.test$label))






















