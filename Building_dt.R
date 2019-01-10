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
inTraining1 <- createDataPartition(B5$BUILDINGID, p = .75, list = FALSE)
training1 <- B5[inTraining1,]
testing1 <- B5[-inTraining1,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 2, repeats = 1)

#train kNN classification model
B_dt <- train(BUILDINGID~., data = training1, method = "rpart", trControl=fitControl)


#make predictions
predB_dt <- predict(B_dt, testing1)

#performace measurment
postResample(predB_dt, testing1$BUILDINGID)
# output
Accuracy     Kappa 
0.8702096 0.8033970 


#check confusion matrix
confusionMatrix(predB_dt,testing1$BUILDINGID)

Confusion Matrix and Statistics

              Reference
Prediction    0     1     2
        0     1064  12    0
        1     248   1285  353
        2     0     0     1761

Overall Statistics

Accuracy : 0.8702          
95% CI : (0.8603, 0.8797)
No Information Rate : 0.4476          
P-Value [Acc > NIR] : < 2.2e-16       

Kappa : 0.8034          
Mcnemars Test P-Value : NA              

Statistics by Class:

                     Class: 0 Class: 1 Class: 2
Sensitivity            0.8110   0.9907   0.8330
Specificity            0.9965   0.8246   1.0000
Pos Pred Value         0.9888   0.6813   1.0000
Neg Pred Value         0.9320   0.9958   0.8808
Prevalence             0.2778   0.2746   0.4476
Detection Rate         0.2253   0.2721   0.3729
Detection Prevalence   0.2278   0.3993   0.3729
Balanced Accuracy      0.9037   0.9077   0.9165
