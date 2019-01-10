#preditct building with wifi5 dataset

#load libraries
library(dplyr)
library(ggplot2)
library(caret)
library(grid)
library(gridExtra)

#Load the necessary data
setwd("~/Ubiqum/Project 4/Wifi Task/UJIndoorLoc")
wifi5 <- readRDS("wifi5.rds")


#remove all features other than WAPs and Latitude
B5 <- select(wifi5, WAP006:WAP517, BUILDINGID)


#trying running model on entire dataset to predict building
set.seed(234)

# define an 75%/25% train/test split of the dataset
inTraining2 <- createDataPartition(B5$BUILDINGID, p = .75, list = FALSE)
training2 <- B5[inTraining2,]
testing2 <- B5[-inTraining2,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 2, repeats = 1)

#train kNN classification model
B_gbm <- train(BUILDINGID~., data = training2, method = "gbm", trControl=fitControl, verbose = FALSE)


#make predictions
predB_gbm <- predict(B_gbm, testing2)

#performace measurment
postResample(predB_gbm, testing2$BUILDINGID)
# output
Accuracy     Kappa 
0.9983062 0.9973835 


#check confusion matrix
confusionMatrix(predB_gbm,testing2$BUILDINGID)

Confusion Matrix and Statistics

              Reference
Prediction    0     1     2
          0   1312  0     0
          1   0     1297  8
          2   0     0     2106

Overall Statistics

Accuracy : 0.9983          
95% CI : (0.9967, 0.9993)
No Information Rate : 0.4476          
P-Value [Acc > NIR] : < 2.2e-16       

Kappa : 0.9974          
Mcnemars Test P-Value : NA              

Statistics by Class:

Class: 0 Class: 1 Class: 2
Sensitivity            1.0000   1.0000   0.9962
Specificity            1.0000   0.9977   1.0000
Pos Pred Value         1.0000   0.9939   1.0000
Neg Pred Value         1.0000   1.0000   0.9969
Prevalence             0.2778   0.2746   0.4476
Detection Rate         0.2778   0.2746   0.4459
Detection Prevalence   0.2778   0.2763   0.4459
Balanced Accuracy      1.0000   0.9988   0.9981