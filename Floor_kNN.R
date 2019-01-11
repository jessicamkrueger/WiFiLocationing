#knn model for floor prediction using WAPs and Building

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
F_knn <- train(FLOOR~., data = training2, method = "knn", trControl=fitControl)

#make predictions
predF_knn <- predict(F_knn, testing2)

#performace measurment
postResample(predF_knn, testing2$FLOOR)
# output
Accuracy     Kappa 
0.9982986    0.9977742 

#check confusion matrix
confusionMatrix(predF_knn, testing2$FLOOR)
Confusion Matrix and Statistics

              Reference
Prediction    0    1    2    3    4
         0 1078    2    1    0    0
         1    1 1247    2    0    0
         2    1    0 1099    0    0
         3    0    0    1 1114    0
         4    0    0    0    0  156

Overall Statistics

Accuracy : 0.9983          
95% CI : (0.9967, 0.9993)
No Information Rate : 0.2656          
P-Value [Acc > NIR] : < 2.2e-16       

Kappa : 0.9978          
Mcnemars Test P-Value : NA              

Statistics by Class:

                     Class: 0 Class: 1 Class: 2 Class: 3 Class: 4
Sensitivity            0.9981   0.9984   0.9964   1.0000  1.00000
Specificity            0.9992   0.9991   0.9997   0.9997  1.00000
Pos Pred Value         0.9972   0.9976   0.9991   0.9991  1.00000
Neg Pred Value         0.9994   0.9994   0.9989   1.0000  1.00000
Prevalence             0.2297   0.2656   0.2346   0.2369  0.03318
Detection Rate         0.2293   0.2652   0.2337   0.2369  0.03318
Detection Prevalence   0.2299   0.2658   0.2339   0.2371  0.03318
Balanced Accuracy      0.9987   0.9988   0.9980   0.9999  1.00000