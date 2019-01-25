#create model to predict lat & long using building and floor

#outlier exploration in wifi task

library(dplyr)
library(ggplot2)
library(caret)
library(gridExtra)
library(grid)
library(tidyr)

#Load the necessary data
setwd("~/Ubiqum/Project 4/Wifi Task/UJIndoorLoc")
wifi6 <- readRDS("wifi6.rds")

#create model for building 0
LatB0 <- wifi6 %>%
  filter(BUILDINGID == 0) %>%
  select(WAP006:WAP517, LATITUDE)

LatB1 <- wifi6 %>%
  filter(BUILDINGID == 1) %>%
  select(WAP006:WAP517, LATITUDE)

LatB2 <- wifi6 %>%
  filter(BUILDINGID == 2) %>%
  select(WAP006:WAP517, LATITUDE)

#trying running model on B1F1 subset to predict latitude
set.seed(234)

# define an 75%/25% train/test split of the dataset
inTrainingLatB0 <- createDataPartition(LatB0$LATITUDE, p = .75, list = FALSE)
trainingLatB0 <- LatB0[inTraining,]
testingLatB0 <- LatB0[-inTraining,]

inTrainingLatB1 <- createDataPartition(LatB1$LATITUDE, p = .75, list = FALSE)
trainingLatB1 <- LatB1[inTraining,]
testingLatB1 <- LatB1[-inTraining,]

inTrainingLatB2 <- createDataPartition(LatB2$LATITUDE, p = .75, list = FALSE)
trainingLatB2 <- LatB2[inTraining,]
testingLatB2 <- LatB2[-inTraining,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 2, repeats = 2)

#train kNN classification model
gbm_Model_LatB0 <- train(LATITUDE~., data = trainingLatB0, method = "gbm", trControl=fitControl, verbose = FALSE)
gbm_Model_LatB1 <- train(LATITUDE~., data = trainingLatB1, method = "gbm", trControl=fitControl, verbose = FALSE)
gbm_Model_LatB2 <- train(LATITUDE~., data = trainingLatB2, method = "gbm", trControl=fitControl, verbose = FALSE)

#make predictions
testPred_gbmLatB0 <- predict(gbm_Model_LatB0, testingLatB0)
testPred_gbmLatB1 <- predict(gbm_Model_LatB1, testingLatB1)
testPred_gbmLatB2 <- predict(gbm_Model_LatB2, testingLatB2)

#performace measurment
postResample(testPred_gbmLatB0, testingLatB0$LATITUDE)
postResample(testPred_gbmLatB1, testingLatB1$LATITUDE)
postResample(testPred_gbmLatB2, testingLatB2$LATITUDE)

#Building 0
RMSE      Rsquared       MAE 
5.5654802 0.9705876 4.1947233 

#Building 1
RMSE      Rsquared      MAE 
7.561435  0.956786 5.996530 

#Building 2
RMSE   Rsquared        MAE 
11.7155922  0.7937586  8.5188885 

#plot predicted verses actual
plot(testPred_gbmLatB2,testingLatB0$LATITUDE)
abline(1,1)

plot(testPred_gbmLatB1,testingLatB1$LATITUDE)
abline(1,1)

plot(testPred_gbmLatB2,testingLatB2$LATITUDE)
abline(1,1)