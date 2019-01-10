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
inTraining <- createDataPartition(B5$BUILDINGID, p = .75, list = FALSE)
training <- B5[inTraining,]
testing <- B5[-inTraining,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 2, repeats = 1)

#train kNN classification model
B_KNN <- train(BUILDINGID~., data = training, method = "knn", trControl=fitControl)


#make predictions
predB_KNN <- predict(B_KNN, testing)

#performace measurment
postResample(predB_KNN, testing$BUILDINGID)
# output
Accuracy     Kappa 
0.9970358 0.9954154 

#view kNN model
B_KNN


#check confusion matrix
confusionMatrix(predB_KNN,testing$BUILDINGID)

###
Confusion Matrix and Statistics


              Reference
Prediction    0       1       2
        0     1312    0       0
        1     0       1283    0
        2     0       14      2114

Overall Statistics

Accuracy : 0.997          
95% CI : (0.995, 0.9984)
No Information Rate : 0.4476         
P-Value [Acc > NIR] : < 2.2e-16      

Kappa : 0.9954         
Mcnemars Test P-Value : NA             

Statistics by Class:

Class: 0 Class: 1 Class: 2
Sensitivity            1.0000   0.9892   1.0000
Specificity            1.0000   1.0000   0.9946
Pos Pred Value         1.0000   1.0000   0.9934
Neg Pred Value         1.0000   0.9959   1.0000
Prevalence             0.2778   0.2746   0.4476
Detection Rate         0.2778   0.2716   0.4476
Detection Prevalence   0.2778   0.2716   0.4506
Balanced Accuracy      1.0000   0.9946   0.9973

###


