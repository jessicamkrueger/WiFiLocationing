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
LonB0 <- wifi6 %>%
  filter(BUILDINGID == 0) %>%
  select(WAP006:WAP517, LATITUDE, LONGITUDE)

LonB1 <- wifi6 %>%
  filter(BUILDINGID == 1) %>%
  select(WAP006:WAP517, LATITUDE, LONGITUDE)

LonB2 <- wifi6 %>%
  filter(BUILDINGID == 2) %>%
  select(WAP006:WAP517, LATITUDE, LONGITUDE)

#trying running model on B1F1 subset to predict latitude
set.seed(234)

# define an 75%/25% train/test split of the dataset
inTrainingLonB0 <- createDataPartition(LonB0$LATITUDE, p = .75, list = FALSE)
trainingLonB0 <- LonB0[inTraining,]
testingLonB0 <- LonB0[-inTraining,]

inTrainingLonB1 <- createDataPartition(LonB1$LATITUDE, p = .75, list = FALSE)
trainingLonB1 <- LonB1[inTraining,]
testingLonB1 <- LonB1[-inTraining,]

inTrainingLonB2 <- createDataPartition(LonB2$LATITUDE, p = .75, list = FALSE)
trainingLonB2 <- LonB2[inTraining,]
testingLonB2 <- LonB2[-inTraining,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 2, repeats = 2)

#train kNN classification model
knn_Model_LonB0 <- train(LONGITUDE~., data = trainingLonB0, method = "knn", trControl=fitControl)
knn_Model_LonB1 <- train(LONGITUDE~., data = trainingLonB1, method = "knn", trControl=fitControl)
knn_Model_LonB2 <- train(LONGITUDE~., data = trainingLonB2, method = "knn", trControl=fitControl)


saveRDS(knn_Model_LonB0, "Mod_LongB0_Knn.rds")
saveRDS(knn_Model_LonB1, "Mod_LongB1_Knn.rds")
saveRDS(knn_Model_LonB2, "Mod_LongB2_Knn.rds")

#make predictions
testPred_knnLonB0 <- predict(knn_Model_LonB0, testingLonB0)
testPred_knnLonB1 <- predict(knn_Model_LonB1, testingLonB1)
testPred_knnLonB2 <- predict(knn_Model_LonB2, testingLonB2)

#performace measurment
postResample(testPred_knnLonB0, testingLonB0$LONGITUDE)
postResample(testPred_knnLonB1, testingLonB1$LONGITUDE)
postResample(testPred_knnLonB2, testingLonB2$LONGITUDE)

#Building 0
RMSE      Rsquared       MAE 
2.4000328 0.9906585 1.1987502  

#Building 1
RMSE Rsquared      MAE 
2.652013 0.997201 1.139928

#Building 2
RMSE      Rsquared       MAE 
7.9393940 0.9226945   3.9457720 

