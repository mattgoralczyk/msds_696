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
#Random Forest 
#
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
#  0.9794028  0.9775274
#
#Tuning parameter 'mtry' was held constant at a value of 4

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
xg.cv <- xgb.cv(params=xgb_params, data=mHealth.train.xgb, nrounds=50, nfold=5, verbose=FALSE, prediction=TRUE)

# https://rpubs.com/mharris/multiclass_xgboost

xg.cv.prediction <- data.frame(xg.cv$pred) %>%
mutate(max_prob=max.col(., ties.method="last"), label=mHealth.train.xgb.labels+1)

head(xg.cv.prediction)
confusionMatrix(factor(xg.cv.prediction$max_prob), factor(xg.cv.prediction$label), mode="everything")
#Confusion Matrix and Statistics
#
#          Reference
#Prediction   1   2   3   4   5   6   7   8   9  10  11  12
#        1  994   0   0   0   0   2   5   1   0   0   0   0
#        2    0 999   1   0   0   0   0   0   0   0   0   0
#        3    0   0 999   0   0   0   0   0   0   0   0   0
#        4    0   0   0 991  17   0   0   0   0   0   0   5
#        5    1   1   0   8 963   4   0  11   1   0   0   0
#        6    2   0   0   1   2 983   2   7   0   0   0   1
#        7    2   0   0   0   0   6 990   9   0   1   0   0
#        8    1   0   0   0  12   5   3 971   2   0   0   0
#        9    0   0   0   0   0   0   0   0 996   1   1   0
#        10   0   0   0   0   1   0   0   0   1 932  33  16
#        11   0   0   0   0   2   0   0   0   0  53 957  17
#        12   0   0   0   0   3   0   0   1   0  13   9 961
#
#Overall Statistics
#                                          
#               Accuracy : 0.978           
#                 95% CI : (0.9752, 0.9805)
#    No Information Rate : 0.0833          
#    P-Value [Acc > NIR] : < 2.2e-16       
#                                          
#                  Kappa : 0.976           
# Mcnemar's Test P-Value : NA              
#
#Statistics by Class:
#
#                     Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9 Class: 10 Class: 11 Class: 12
#Sensitivity           0.99400  0.99900  0.99900  0.99100  0.96300  0.98300  0.99000  0.97100  0.99600   0.93200   0.95700   0.96100
#Specificity           0.99927  0.99991  1.00000  0.99800  0.99764  0.99864  0.99836  0.99791  0.99982   0.99536   0.99345   0.99764
#Pos Pred Value        0.99202  0.99900  1.00000  0.97828  0.97371  0.98497  0.98214  0.97686  0.99800   0.94812   0.93003   0.97366
#Neg Pred Value        0.99945  0.99991  0.99991  0.99918  0.99664  0.99845  0.99909  0.99737  0.99964   0.99383   0.99608   0.99646
#Precision             0.99202  0.99900  1.00000  0.97828  0.97371  0.98497  0.98214  0.97686  0.99800   0.94812   0.93003   0.97366
#Recall                0.99400  0.99900  0.99900  0.99100  0.96300  0.98300  0.99000  0.97100  0.99600   0.93200   0.95700   0.96100
#F1                    0.99301  0.99900  0.99950  0.98460  0.96833  0.98398  0.98606  0.97392  0.99700   0.93999   0.94332   0.96729
#Prevalence            0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333   0.08333   0.08333   0.08333
#Detection Rate        0.08283  0.08325  0.08325  0.08258  0.08025  0.08192  0.08250  0.08092  0.08300   0.07767   0.07975   0.08008
#Detection Prevalence  0.08350  0.08333  0.08325  0.08442  0.08242  0.08317  0.08400  0.08283  0.08317   0.08192   0.08575   0.08225
#Balanced Accuracy     0.99664  0.99945  0.99950  0.99450  0.98032  0.99082  0.99418  0.98445  0.99791   0.96368   0.97523   0.97932


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



xgb_params <- list("objective"="multi:softprob", "eval_metric"="mlogloss", "num_class"=12)
xg.2 <- xgb.train(params=xgb_params, data=mHealth.train.xgb, nrounds=50)
mHealth.test.xgb <- xgb.DMatrix(data=as.matrix(mHealth.test[,1:23]), label=mHealth.test.xgb.labels)
xg.2.test.predict <- predict(xg.2, newdata=mHealth.test.xgb)
xg.2.test.prediction <- matrix(xg.2.test.predict, nrow=12, ncol=length(xg.2.test.predict)/12) %>%
t() %>%
data.frame() %>%
mutate(label=mHealth.test.xgb.labels+1, max_prob=max.col(., "last"))
confusionMatrix(factor(xg.2.test.prediction$max_prob), factor(xg.2.test.prediction$label), mode="everything")
#Confusion Matrix and Statistics
#
#          Reference
#Prediction    1    2    3    4    5    6    7    8    9   10   11   12
#        1   993    0    0    0    0    1    2    1    0    0    0    0
#        2     2 1000    0    0    0    0    0    0    0    0    0    0
#        3     0    0 1000    0    0    0    0    0    0    0    0    0
#        4     0    0    0  993   13    0    0    0    0    1    0    7
#        5     0    0    0    5  973    3    1    8    0    0    0    0
#        6     5    0    0    1    0  985    4    8    1    0    0    0
#        7     0    0    0    0    0    2  993    1    0    0    0    0
#        8     0    0    0    0    8    9    0  978    2    0    0    4
#        9     0    0    0    0    0    0    0    3  995    1    1    0
#        10    0    0    0    1    5    0    0    1    0  943   30   15
#        11    0    0    0    0    0    0    0    0    0   42  967   11
#        12    0    0    0    0    1    0    0    0    2   13    2  963
#
#Overall Statistics
#                                          
#               Accuracy : 0.9819          
#                 95% CI : (0.9794, 0.9842)
#    No Information Rate : 0.0833          
#    P-Value [Acc > NIR] : < 2.2e-16       
#                                          
#                  Kappa : 0.9803          
# Mcnemar's Test P-Value : NA              
#
#Statistics by Class:
#
#                     Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9 Class: 10 Class: 11 Class: 12
#Sensitivity           0.99300  1.00000  1.00000  0.99300  0.97300  0.98500  0.99300  0.97800  0.99500   0.94300   0.96700   0.96300
#Specificity           0.99964  0.99982  1.00000  0.99809  0.99845  0.99827  0.99973  0.99791  0.99955   0.99527   0.99518   0.99836
#Pos Pred Value        0.99599  0.99800  1.00000  0.97929  0.98283  0.98108  0.99699  0.97702  0.99500   0.94774   0.94804   0.98165
#Neg Pred Value        0.99936  1.00000  1.00000  0.99936  0.99755  0.99864  0.99936  0.99800  0.99955   0.99482   0.99699   0.99664
#Precision             0.99599  0.99800  1.00000  0.97929  0.98283  0.98108  0.99699  0.97702  0.99500   0.94774   0.94804   0.98165
#Recall                0.99300  1.00000  1.00000  0.99300  0.97300  0.98500  0.99300  0.97800  0.99500   0.94300   0.96700   0.96300
#F1                    0.99449  0.99900  1.00000  0.98610  0.97789  0.98303  0.99499  0.97751  0.99500   0.94536   0.95743   0.97224
#Prevalence            0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333   0.08333   0.08333   0.08333
#Detection Rate        0.08275  0.08333  0.08333  0.08275  0.08108  0.08208  0.08275  0.08150  0.08292   0.07858   0.08058   0.08025
#Detection Prevalence  0.08308  0.08350  0.08333  0.08450  0.08250  0.08367  0.08300  0.08342  0.08333   0.08292   0.08500   0.08175
#Balanced Accuracy     0.99632  0.99991  1.00000  0.99555  0.98573  0.99164  0.99636  0.98795  0.99727   0.96914   0.98109   0.98068

xgb.names <- colnames(mHealth.train[,-1])
xg.2.imp.matrix <- xgb.importance(feature_names=xgb.names, model=xg.2)
head(xg.2.imp.matrix)
#          Feature       Gain      Cover  Frequency
#1:           ec_1 0.13876153 0.05627032 0.05586434
#2:  accel_r_arm_z 0.09464612 0.08176027 0.05793688
#3: gyro_l_ankle_x 0.07824334 0.06248727 0.04879887
#4:   gyro_r_arm_y 0.07250447 0.06667253 0.05379180
#5:   gyro_r_arm_z 0.06974321 0.08781868 0.06066886
#6:  accel_r_arm_y 0.06781192 0.06339070 0.04446538
xgb.ggplot.importance(xg.2.imp.matrix)


xgb_params <- list("objective"="multi:softprob", "eval_metric"="mlogloss", "num_class"=12)
xg.3 <- xgb.train(params=xgb_params, data=mHealth.train.xgb, nrounds=50, nfold=5)
mHealth.test.xgb <- xgb.DMatrix(data=as.matrix(mHealth.test[,1:23]), label=mHealth.test.xgb.labels)
xg.3.test.predict <- predict(xg.3, newdata=mHealth.test.xgb)
xg.3.test.prediction <- matrix(xg.3.test.predict, nrow=12, ncol=length(xg.3.test.predict)/12) %>%
t() %>%
data.frame() %>%
mutate(label=mHealth.test.xgb.labels+1, max_prob=max.col(., "last"))
confusionMatrix(factor(xg.3.test.prediction$max_prob), factor(xg.3.test.prediction$label), mode="everything")
#Confusion Matrix and Statistics
#
#          Reference
#Prediction    1    2    3    4    5    6    7    8    9   10   11   12
#        1   993    0    0    0    0    1    2    1    0    0    0    0
#        2     2 1000    0    0    0    0    0    0    0    0    0    0
#        3     0    0 1000    0    0    0    0    0    0    0    0    0
#        4     0    0    0  993   13    0    0    0    0    1    0    7
#        5     0    0    0    5  973    3    1    8    0    0    0    0
#        6     5    0    0    1    0  985    4    8    1    0    0    0
#        7     0    0    0    0    0    2  993    1    0    0    0    0
#        8     0    0    0    0    8    9    0  978    2    0    0    4
#        9     0    0    0    0    0    0    0    3  995    1    1    0
#        10    0    0    0    1    5    0    0    1    0  943   30   15
#        11    0    0    0    0    0    0    0    0    0   42  967   11
#        12    0    0    0    0    1    0    0    0    2   13    2  963
#
#Overall Statistics
#                                          
#               Accuracy : 0.9819          
#                 95% CI : (0.9794, 0.9842)
#    No Information Rate : 0.0833          
#    P-Value [Acc > NIR] : < 2.2e-16       
#                                          
#                  Kappa : 0.9803          
# Mcnemar's Test P-Value : NA              
#
#Statistics by Class:
#
#                     Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9 Class: 10 Class: 11 Class: 12
#Sensitivity           0.99300  1.00000  1.00000  0.99300  0.97300  0.98500  0.99300  0.97800  0.99500   0.94300   0.96700   0.96300
#Specificity           0.99964  0.99982  1.00000  0.99809  0.99845  0.99827  0.99973  0.99791  0.99955   0.99527   0.99518   0.99836
#Pos Pred Value        0.99599  0.99800  1.00000  0.97929  0.98283  0.98108  0.99699  0.97702  0.99500   0.94774   0.94804   0.98165
#Neg Pred Value        0.99936  1.00000  1.00000  0.99936  0.99755  0.99864  0.99936  0.99800  0.99955   0.99482   0.99699   0.99664
#Precision             0.99599  0.99800  1.00000  0.97929  0.98283  0.98108  0.99699  0.97702  0.99500   0.94774   0.94804   0.98165
#Recall                0.99300  1.00000  1.00000  0.99300  0.97300  0.98500  0.99300  0.97800  0.99500   0.94300   0.96700   0.96300
#F1                    0.99449  0.99900  1.00000  0.98610  0.97789  0.98303  0.99499  0.97751  0.99500   0.94536   0.95743   0.97224
#Prevalence            0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333   0.08333   0.08333   0.08333
#Detection Rate        0.08275  0.08333  0.08333  0.08275  0.08108  0.08208  0.08275  0.08150  0.08292   0.07858   0.08058   0.08025
#Detection Prevalence  0.08308  0.08350  0.08333  0.08450  0.08250  0.08367  0.08300  0.08342  0.08333   0.08292   0.08500   0.08175
#Balanced Accuracy     0.99632  0.99991  1.00000  0.99555  0.98573  0.99164  0.99636  0.98795  0.99727   0.96914   0.98109   0.98068

xgb.names <- colnames(mHealth.train[,-1])
xg.3.imp.matrix <- xgb.importance(feature_names=xgb.names, model=xg.3)
head(xg.3.imp.matrix)
#          Feature       Gain      Cover  Frequency
#1:           ec_1 0.13876153 0.05627032 0.05586434
#2:  accel_r_arm_z 0.09464612 0.08176027 0.05793688
#3: gyro_l_ankle_x 0.07824334 0.06248727 0.04879887
#4:   gyro_r_arm_y 0.07250447 0.06667253 0.05379180
#5:   gyro_r_arm_z 0.06974321 0.08781868 0.06066886
#6:  accel_r_arm_y 0.06781192 0.06339070 0.04446538
xgb.ggplot.importance(xg.3.imp.matrix)



xgb_params <- list("objective"="multi:softprob", "eval_metric"="mlogloss", "num_class"=12)
xg.4 <- xgb.train(params=xgb_params, data=mHealth.train.xgb, nrounds=500, nfold=5)
mHealth.test.xgb <- xgb.DMatrix(data=as.matrix(mHealth.test[,1:23]), label=mHealth.test.xgb.labels)
xg.4.test.predict <- predict(xg.4, newdata=mHealth.test.xgb)
xg.4.test.prediction <- matrix(xg.4.test.predict, nrow=12, ncol=length(xg.4.test.predict)/12) %>%
t() %>%
data.frame() %>%
mutate(label=mHealth.test.xgb.labels+1, max_prob=max.col(., "last"))
confusionMatrix(factor(xg.4.test.prediction$max_prob), factor(xg.4.test.prediction$label), mode="everything")
#Confusion Matrix and Statistics
#
#          Reference
#Prediction    1    2    3    4    5    6    7    8    9   10   11   12
#        1   997    0    0    0    0    2    2    0    0    0    0    0
#        2     2 1000    0    0    0    0    0    0    0    0    0    0
#        3     0    0 1000    0    0    0    0    0    0    0    0    0
#        4     0    0    0  994   13    0    0    2    0    2    0    4
#        5     0    0    0    5  979    3    1    7    0    0    0    0
#        6     1    0    0    0    0  989    0    6    1    0    0    0
#        7     0    0    0    0    0    1  997    1    0    0    0    0
#        8     0    0    0    0    5    5    0  979    1    0    0    3
#        9     0    0    0    0    0    0    0    3  996    1    0    0
#        10    0    0    0    1    3    0    0    0    1  944   24    9
#        11    0    0    0    0    0    0    0    0    0   41  975   12
#        12    0    0    0    0    0    0    0    2    1   12    1  972
#
#Overall Statistics
#                                          
#               Accuracy : 0.9852          
#                 95% CI : (0.9828, 0.9873)
#    No Information Rate : 0.0833          
#    P-Value [Acc > NIR] : < 2.2e-16       
#                                          
#                  Kappa : 0.9838          
# Mcnemar's Test P-Value : NA              
#
#Statistics by Class:
#
#                     Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9 Class: 10 Class: 11 Class: 12
#Sensitivity           0.99700  1.00000  1.00000  0.99400  0.97900  0.98900  0.99700  0.97900  0.99600   0.94400   0.97500   0.97200
#Specificity           0.99964  0.99982  1.00000  0.99809  0.99855  0.99927  0.99982  0.99873  0.99964   0.99655   0.99518   0.99855
#Pos Pred Value        0.99600  0.99800  1.00000  0.97931  0.98392  0.99198  0.99800  0.98590  0.99600   0.96130   0.94844   0.98381
#Neg Pred Value        0.99973  1.00000  1.00000  0.99945  0.99809  0.99900  0.99973  0.99809  0.99964   0.99492   0.99772   0.99746
#Precision             0.99600  0.99800  1.00000  0.97931  0.98392  0.99198  0.99800  0.98590  0.99600   0.96130   0.94844   0.98381
#Recall                0.99700  1.00000  1.00000  0.99400  0.97900  0.98900  0.99700  0.97900  0.99600   0.94400   0.97500   0.97200
#F1                    0.99650  0.99900  1.00000  0.98660  0.98145  0.99049  0.99750  0.98244  0.99600   0.95257   0.96154   0.97787
#Prevalence            0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333   0.08333   0.08333   0.08333
#Detection Rate        0.08308  0.08333  0.08333  0.08283  0.08158  0.08242  0.08308  0.08158  0.08300   0.07867   0.08125   0.08100
#Detection Prevalence  0.08342  0.08350  0.08333  0.08458  0.08292  0.08308  0.08325  0.08275  0.08333   0.08183   0.08567   0.08233
#Balanced Accuracy     0.99832  0.99991  1.00000  0.99605  0.98877  0.99414  0.99841  0.98886  0.99782   0.97027   0.98509   0.98527


xgb.names <- colnames(mHealth.train[,-1])
xg.4.imp.matrix <- xgb.importance(feature_names=xgb.names, model=xg.4)
head(xg.4.imp.matrix)
#          Feature       Gain      Cover  Frequency
#1:           ec_1 0.13856572 0.05588976 0.04926370
#2:  accel_r_arm_z 0.09456201 0.08000330 0.05381860
#3: gyro_l_ankle_x 0.07819091 0.06162680 0.04868881
#4:   gyro_r_arm_y 0.07246375 0.06543015 0.05213815
#5:   gyro_r_arm_z 0.06972215 0.08592416 0.05549905
#6:  accel_r_arm_y 0.06774394 0.06179392 0.03940211
xgb.ggplot.importance(xg.4.imp.matrix)


#
# SVM
#
svm.1 <- svm(as.factor(mHealth.train$label)~., data=mHealth.train[,1:23])
svm.1.predict <- predict(svm.1, newdata=mHealth.test)
svm.1.prediction <- matrix(svm.1.predict, nrow=12, ncol=length(svm.1.predict)/12)
confusionMatrix(svm.1.prediction, as.factor(mHealth.test$label))
#Confusion Matrix and Statistics
#
#          Reference
#Prediction    1    2    3    4    5    6    7    8    9   10   11   12
#        1   999    0    0    6    5   92   63   60    0    0    0    0
#        2     0  999    0    0    6    0    0    0    0    0    0    0
#        3     0    0 1000    0    0    0    0    0    0    0    0    0
#        4     1    0    0  970   56    0    0    2    0    0    0    4
#        5     0    1    0   15  863    6    2   34    2    0    0    3
#        6     0    0    0    3    8  864   20   17    0    0    0    1
#        7     0    0    0    0    7    6  899    6    0    0    0    2
#        8     0    0    0    1   49   32   15  881    5    0    0    0
#        9     0    0    0    0    1    0    0    0  993    0    0    0
#        10    0    0    0    0    1    0    0    0    0  922   43    9
#        11    0    0    0    0    1    0    1    0    0   70  955   24
#        12    0    0    0    5    3    0    0    0    0    8    2  957
#
#Overall Statistics
#                                         
#               Accuracy : 0.9418         
#                 95% CI : (0.9375, 0.946)
#    No Information Rate : 0.0833         
#    P-Value [Acc > NIR] : < 2.2e-16      
#                                         
#                  Kappa : 0.9365         
# Mcnemar's Test P-Value : NA             
#
#Statistics by Class:
#
#                     Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9 Class: 10 Class: 11 Class: 12
#Sensitivity           0.99900  0.99900  1.00000  0.97000  0.86300  0.86400  0.89900  0.88100  0.99300   0.92200   0.95500   0.95700
#Specificity           0.97945  0.99945  1.00000  0.99427  0.99427  0.99555  0.99809  0.99073  0.99991   0.99518   0.99127   0.99836
#Pos Pred Value        0.81551  0.99403  1.00000  0.93901  0.93197  0.94633  0.97717  0.89624  0.99899   0.94564   0.90866   0.98154
#Neg Pred Value        0.99991  0.99991  1.00000  0.99726  0.98763  0.98773  0.99088  0.98920  0.99936   0.99293   0.99589   0.99610
#Prevalence            0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333   0.08333   0.08333   0.08333
#Detection Rate        0.08325  0.08325  0.08333  0.08083  0.07192  0.07200  0.07492  0.07342  0.08275   0.07683   0.07958   0.07975
#Detection Prevalence  0.10208  0.08375  0.08333  0.08608  0.07717  0.07608  0.07667  0.08192  0.08283   0.08125   0.08758   0.08125
#Balanced Accuracy     0.98923  0.99923  1.00000  0.98214  0.92864  0.92977  0.94855  0.93586  0.99645   0.95859   0.97314   0.97768



svm.2 <- svm(as.factor(mHealth.train$label)~., data=mHealth.train[,1:23], kernel="radial", gamma=2)
svm.2.predict <- predict(svm.2, newdata=mHealth.test)
svm.2.prediction <- matrix(svm.2.predict, nrow=12, ncol=length(svm.2.predict)/12)
confusionMatrix(svm.2.prediction, as.factor(mHealth.test$label))
#Confusion Matrix and Statistics
#
#          Reference
#Prediction    1    2    3    4    5    6    7    8    9   10   11   12
#        1   977    0    0    0    0    0    0    0    0    0    0    0
#        2     0  989    0    0    0    0    0    0    0    0    0    0
#        3     0    0  988    0    0    0    0    0    0    0    0    0
#        4     0    0    0  570    0    0    0    0    0    0    0    0
#        5     0    0    0    0  348    0    0    0    0    0    0    0
#        6     0    0    0    0    0  721    9    1    0    0    0    0
#        7     0    0    0    0    0    6  669    0    0    0    0    0
#        8     0    0    0    0    0    2    1  646    0    0    0    0
#        9     0    0    0    0    0    0    0    0  732    0    0    0
#        10    0    0    0    0    0    0    0    0    0  219    0    0
#        11   23   11   12  430  652  271  321  353  268  781 1000  817
#        12    0    0    0    0    0    0    0    0    0    0    0  183
#
#Overall Statistics
#                                          
#               Accuracy : 0.6702          
#                 95% CI : (0.6617, 0.6786)
#    No Information Rate : 0.0833          
#    P-Value [Acc > NIR] : < 2.2e-16       
#                                          
#                  Kappa : 0.6402          
# Mcnemar's Test P-Value : NA              
#
#Statistics by Class:
#
#                     Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9 Class: 10 Class: 11 Class: 12
#Sensitivity           0.97700  0.98900  0.98800  0.57000  0.34800  0.72100  0.66900  0.64600  0.73200   0.21900   1.00000   0.18300
#Specificity           1.00000  1.00000  1.00000  1.00000  1.00000  0.99909  0.99945  0.99973  1.00000   1.00000   0.64191   1.00000
#Pos Pred Value        1.00000  1.00000  1.00000  1.00000  1.00000  0.98632  0.99111  0.99538  1.00000   1.00000   0.20247   1.00000
#Neg Pred Value        0.99791  0.99900  0.99891  0.96238  0.94404  0.97524  0.97077  0.96881  0.97622   0.93371   1.00000   0.93086
#Prevalence            0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333   0.08333   0.08333   0.08333
#Detection Rate        0.08142  0.08242  0.08233  0.04750  0.02900  0.06008  0.05575  0.05383  0.06100   0.01825   0.08333   0.01525
#Detection Prevalence  0.08142  0.08242  0.08233  0.04750  0.02900  0.06092  0.05625  0.05408  0.06100   0.01825   0.41158   0.01525
#Balanced Accuracy     0.98850  0.99450  0.99400  0.78500  0.67400  0.86005  0.83423  0.82286  0.86600   0.60950   0.82095   0.59150


svm.3 <- svm(as.factor(mHealth.train$label)~., data=mHealth.train[,1:23], kernel="radial", gamma=1/100)
svm.3.predict <- predict(svm.3, newdata=mHealth.test)
svm.3.prediction <- matrix(svm.3.predict, nrow=12, ncol=length(svm.3.predict)/12)
confusionMatrix(svm.3.prediction, as.factor(mHealth.test$label))
#Confusion Matrix and Statistics
#
#          Reference
#Prediction    1    2    3    4    5    6    7    8    9   10   11   12
#        1   999    0    0   44   37  137  106  165    0    0    0    0
#        2     0  996    0    0   10    0    6    0    0    0    0    1
#        3     0    0 1000    0    0    0    0    0    0    0    0    0
#        4     1    0    0  858  142   15   10   22    0    0    0   14
#        5     0    0    0   67  663   26    5   69    7    0    0   13
#        6     0    0    0    8   17  799    1   36    0    0    0    1
#        7     0    4    0    0    7    4  868    3    0    2    0    6
#        8     0    0    0   13  109   19    3  700   10    0    0    3
#        9     0    0    0    0    1    0    0    5  983    0    0    0
#        10    0    0    0    0    4    0    0    0    0  829   71   53
#        11    0    0    0    1    1    0    0    0    0  132  915   23
#        12    0    0    0    9    9    0    1    0    0   37   14  886
#
#Overall Statistics
#                                          
#               Accuracy : 0.8747          
#                 95% CI : (0.8686, 0.8805)
#    No Information Rate : 0.0833          
#    P-Value [Acc > NIR] : < 2.2e-16       
#                                          
#                  Kappa : 0.8633          
# Mcnemar's Test P-Value : NA              
#
#Statistics by Class:
#
#                     Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9 Class: 10 Class: 11 Class: 12
#Sensitivity           0.99900  0.99600  1.00000  0.85800  0.66300  0.79900  0.86800  0.70000  0.98300   0.82900   0.91500   0.88600
#Specificity           0.95555  0.99845  1.00000  0.98145  0.98300  0.99427  0.99764  0.98573  0.99945   0.98836   0.98573   0.99364
#Pos Pred Value        0.67137  0.98322  1.00000  0.80791  0.78000  0.92691  0.97092  0.81680  0.99393   0.86625   0.85354   0.92678
#Neg Pred Value        0.99990  0.99964  1.00000  0.98702  0.96978  0.98195  0.98811  0.97308  0.99846   0.98452   0.99222   0.98968
#Prevalence            0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333   0.08333   0.08333   0.08333
#Detection Rate        0.08325  0.08300  0.08333  0.07150  0.05525  0.06658  0.07233  0.05833  0.08192   0.06908   0.07625   0.07383
#Detection Prevalence  0.12400  0.08442  0.08333  0.08850  0.07083  0.07183  0.07450  0.07142  0.08242   0.07975   0.08933   0.07967
#Balanced Accuracy     0.97727  0.99723  1.00000  0.91973  0.82300  0.89664  0.93282  0.84286  0.99123   0.90868   0.95036   0.93982



svm.4 <- svm(as.factor(mHealth.train$label)~., data=mHealth.train[,1:23], kernel="radial", cost=2)
svm.4.predict <- predict(svm.4, newdata=mHealth.test)
svm.4.prediction <- matrix(svm.4.predict, nrow=12, ncol=length(svm.4.predict)/12)
confusionMatrix(svm.4.prediction, as.factor(mHealth.test$label))
#Confusion Matrix and Statistics
#
#          Reference
#Prediction    1    2    3    4    5    6    7    8    9   10   11   12
#        1  1000    0    0    4    4   60   52   31    0    0    0    0
#        2     0  999    0    0    2    0    0    0    0    0    0    0
#        3     0    0 1000    0    0    0    0    0    0    0    0    0
#        4     0    0    0  976   46    0    0    2    0    0    0    4
#        5     0    1    0   11  888    4    0   25    2    0    0    3
#        6     0    0    0    4    6  908   16   16    0    0    0    1
#        7     0    0    0    0    4    2  916    9    0    0    0    1
#        8     0    0    0    1   43   26   15  917    6    0    0    0
#        9     0    0    0    0    1    0    0    0  992    0    0    0
#        10    0    0    0    0    2    0    0    0    0  934   44   10
#        11    0    0    0    0    0    0    1    0    0   61  954   25
#        12    0    0    0    4    4    0    0    0    0    5    2  956
#
#Overall Statistics
#                                         
#               Accuracy : 0.9533         
#                 95% CI : (0.9494, 0.957)
#    No Information Rate : 0.0833         
#    P-Value [Acc > NIR] : < 2.2e-16      
#                                         
#                  Kappa : 0.9491         
# Mcnemar's Test P-Value : NA             
#
#Statistics by Class:
#
#                     Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9 Class: 10 Class: 11 Class: 12
#Sensitivity           1.00000  0.99900  1.00000  0.97600  0.88800  0.90800  0.91600  0.91700  0.99200   0.93400   0.95400   0.95600
#Specificity           0.98627  0.99982  1.00000  0.99527  0.99582  0.99609  0.99855  0.99173  0.99991   0.99491   0.99209   0.99864
#Pos Pred Value        0.86881  0.99800  1.00000  0.94942  0.95075  0.95478  0.98283  0.90972  0.99899   0.94343   0.91643   0.98455
#Neg Pred Value        1.00000  0.99991  1.00000  0.99781  0.98988  0.99167  0.99241  0.99245  0.99927   0.99401   0.99580   0.99601
#Prevalence            0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333   0.08333   0.08333   0.08333
#Detection Rate        0.08333  0.08325  0.08333  0.08133  0.07400  0.07567  0.07633  0.07642  0.08267   0.07783   0.07950   0.07967
#Detection Prevalence  0.09592  0.08342  0.08333  0.08567  0.07783  0.07925  0.07767  0.08400  0.08275   0.08250   0.08675   0.08092
#Balanced Accuracy     0.99314  0.99941  1.00000  0.98564  0.94191  0.95205  0.95727  0.95436  0.99595   0.96445   0.97305   0.97732



svm.5 <- svm(as.factor(mHealth.train$label)~., data=mHealth.train[,1:23], kernel="radial", cost=100)
svm.5.predict <- predict(svm.5, newdata=mHealth.test)
svm.5.prediction <- matrix(svm.5.predict, nrow=12, ncol=length(svm.5.predict)/12)
confusionMatrix(svm.5.prediction, as.factor(mHealth.test$label))
#Confusion Matrix and Statistics
#
#          Reference
#Prediction    1    2    3    4    5    6    7    8    9   10   11   12
#        1  1000    0    0    0    2    4    2    0    0    0    0    0
#        2     0 1000    0    0    3    0    0    0    0    0    0    0
#        3     0    0 1000    0    0    0    0    0    0    0    0    0
#        4     0    0    0  987   22    0    0    1    0    0    0    4
#        5     0    0    0    6  949    4    4   15    0    0    0    2
#        6     0    0    0    2    2  977    5   13    1    0    0    1
#        7     0    0    0    0    2    6  986    5    0    0    0    2
#        8     0    0    0    1   13    9    2  966    4    0    0    0
#        9     0    0    0    0    1    0    0    0  995    0    0    0
#        10    0    0    0    0    2    0    0    0    0  944   36   13
#        11    0    0    0    0    0    0    1    0    0   51  962   26
#        12    0    0    0    4    4    0    0    0    0    5    2  952
#
#Overall Statistics
#                                          
#               Accuracy : 0.9765          
#                 95% CI : (0.9736, 0.9791)
#    No Information Rate : 0.0833          
#    P-Value [Acc > NIR] : < 2.2e-16       
#                                          
#                  Kappa : 0.9744          
# Mcnemar's Test P-Value : NA              
#
#Statistics by Class:
#
#                     Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9 Class: 10 Class: 11 Class: 12
#Sensitivity           1.00000  1.00000  1.00000  0.98700  0.94900  0.97700  0.98600  0.96600  0.99500   0.94400   0.96200   0.95200
#Specificity           0.99927  0.99973  1.00000  0.99755  0.99718  0.99782  0.99864  0.99736  0.99991   0.99536   0.99291   0.99864
#Pos Pred Value        0.99206  0.99701  1.00000  0.97337  0.96837  0.97602  0.98501  0.97085  0.99900   0.94874   0.92500   0.98449
#Neg Pred Value        1.00000  1.00000  1.00000  0.99882  0.99537  0.99791  0.99873  0.99691  0.99955   0.99491   0.99653   0.99565
#Prevalence            0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333   0.08333   0.08333   0.08333
#Detection Rate        0.08333  0.08333  0.08333  0.08225  0.07908  0.08142  0.08217  0.08050  0.08292   0.07867   0.08017   0.07933
#Detection Prevalence  0.08400  0.08358  0.08333  0.08450  0.08167  0.08342  0.08342  0.08292  0.08300   0.08292   0.08667   0.08058
#Balanced Accuracy     0.99964  0.99986  1.00000  0.99227  0.97309  0.98741  0.99232  0.98168  0.99745   0.96968   0.97745   0.97532



svm.6 <- svm(as.factor(mHealth.train$label)~., data=mHealth.train[,1:23], kernel="radial", cost=1000)
svm.6.predict <- predict(svm.6, newdata=mHealth.test)
svm.6.prediction <- matrix(svm.6.predict, nrow=12, ncol=length(svm.6.predict)/12)
confusionMatrix(svm.6.prediction, as.factor(mHealth.test$label))
#Confusion Matrix and Statistics
#
#          Reference
#Prediction    1    2    3    4    5    6    7    8    9   10   11   12
#        1  1000    0    0    0    2    3    0    2    0    0    0    0
#        2     0 1000    0    0    3    0    0    0    0    0    0    0
#        3     0    0 1000    0    0    0    0    0    0    0    0    0
#        4     0    0    0  984   28    0    0    3    0    0    0    4
#        5     0    0    0    9  947    5    4   16    0    0    0    2
#        6     0    0    0    2    1  972    5   12    1    0    0    1
#        7     0    0    0    0    2    8  987    7    0    0    0    2
#        8     0    0    0    1   10   12    3  960    4    0    0    0
#        9     0    0    0    0    1    0    0    0  995    0    0    0
#        10    0    0    0    0    2    0    0    0    0  944   36   13
#        11    0    0    0    0    0    0    1    0    0   51  962   26
#        12    0    0    0    4    4    0    0    0    0    5    2  952
#
#Overall Statistics
#                                         
#               Accuracy : 0.9752         
#                 95% CI : (0.9723, 0.978)
#    No Information Rate : 0.0833         
#    P-Value [Acc > NIR] : < 2.2e-16      
#                                         
#                  Kappa : 0.973          
# Mcnemar's Test P-Value : NA             
#
#Statistics by Class:
#
#                     Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9 Class: 10 Class: 11 Class: 12
#Sensitivity           1.00000  1.00000  1.00000  0.98400  0.94700  0.97200  0.98700  0.96000  0.99500   0.94400   0.96200   0.95200
#Specificity           0.99936  0.99973  1.00000  0.99682  0.99673  0.99800  0.99827  0.99727  0.99991   0.99536   0.99291   0.99864
#Pos Pred Value        0.99305  0.99701  1.00000  0.96565  0.96338  0.97787  0.98111  0.96970  0.99900   0.94874   0.92500   0.98449
#Neg Pred Value        1.00000  1.00000  1.00000  0.99854  0.99519  0.99746  0.99882  0.99637  0.99955   0.99491   0.99653   0.99565
#Prevalence            0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333   0.08333   0.08333   0.08333
#Detection Rate        0.08333  0.08333  0.08333  0.08200  0.07892  0.08100  0.08225  0.08000  0.08292   0.07867   0.08017   0.07933
#Detection Prevalence  0.08392  0.08358  0.08333  0.08492  0.08192  0.08283  0.08383  0.08250  0.08300   0.08292   0.08667   0.08058
#Balanced Accuracy     0.99968  0.99986  1.00000  0.99041  0.97186  0.98500  0.99264  0.97864  0.99745   0.96968   0.97745   0.97532


















