#gbm model for floor prediction using WAPs and Building

#preditct building with wifi5 dataset

#load libraries
library(dplyr)
library(ggplot2)
library(caret)
library(grid)
library(gridExtra)

#Load the necessary data
setwd("~/Ubiqum/Project 4/Wifi Task/UJIndoorLoc")
wifi6 <- readRDS("wifi6.rds")


#remove all features other than WAPs, Building, and Floor
FLR <- select(wifi6, WAP006:WAP517, BUILDINGID, FLOOR)


#trying running model on entire dataset to predict building
set.seed(234)

# define an 75%/25% train/test split of the dataset
inTraining2 <- createDataPartition(FLR$FLOOR, p = .75, list = FALSE)
training2 <- FLR[inTraining2,]
testing2 <- FLR[-inTraining2,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 2, repeats = 1)

#train classification model
F_gbm <- train(FLOOR~., data = training2, method = "gbm", trControl=fitControl, verbose = FALSE)

#make predictions
predF_gbm <- predict(F_gbm, testing2)

#performace measurment
postResample(predF_gbm, testing2$FLOOR)
# output
Accuracy     Kappa 
0.9889409    0.9855319

#check confusion matrix
confusionMatrix(predF_gbm, testing2$FLOOR)

Confusion Matrix and Statistics

              Reference
Prediction    0    1    2    3    4
         0 1070    6    0    1    0
         1    9 1235    6    5    0
         2    1    8 1087    6    0
         3    0    0   10 1102    0
         4    0    0    0    0  156

Overall Statistics

Accuracy : 0.9889          
95% CI : (0.9855, 0.9917)
No Information Rate : 0.2656          
P-Value [Acc > NIR] : < 2.2e-16       

Kappa : 0.9855          
Mcnemars Test P-Value : NA              

Statistics by Class:

                     Class: 0 Class: 1 Class: 2 Class: 3 Class: 4
Sensitivity            0.9907   0.9888   0.9855   0.9892  1.00000
Specificity            0.9981   0.9942   0.9958   0.9972  1.00000
Pos Pred Value         0.9935   0.9841   0.9864   0.9910  1.00000
Neg Pred Value         0.9972   0.9959   0.9956   0.9967  1.00000
Prevalence             0.2297   0.2656   0.2346   0.2369  0.03318
Detection Rate         0.2276   0.2627   0.2312   0.2344  0.03318
Detection Prevalence   0.2291   0.2669   0.2344   0.2365  0.03318
Balanced Accuracy      0.9944   0.9915   0.9907   0.9932  1.00000
