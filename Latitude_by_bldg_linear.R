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
lm_Model_LatB0 <- train(LATITUDE~., data = trainingLatB0, method = "lm", trControl=fitControl)
lm_Model_LatB1 <- train(LATITUDE~., data = trainingLatB1, method = "lm", trControl=fitControl)
lm_Model_LatB2 <- train(LATITUDE~., data = trainingLatB2, method = "lm", trControl=fitControl)

#make predictions
testPred_lmLatB0 <- predict(lm_Model_LatB0, testingLatB0)
testPred_lmLatB1 <- predict(lm_Model_LatB1, testingLatB1)
testPred_lmLatB2 <- predict(lm_Model_LatB2, testingLatB2)

#performace measurment
postResample(testPred_lmLatB0, testingLatB0$LATITUDE)
postResample(testPred_lmLatB1, testingLatB1$LATITUDE)
postResample(testPred_lmLatB2, testingLatB2$LATITUDE)

#Building 0
RMSE  Rsquared       MAE 
7.9948643 0.9395171 6.2641075 

#Building 1
RMSE  Rsquared       MAE 
9.7638243 0.9276562 7.5295520 

#Building 2
RMSE   Rsquared        MAE 
17.8756767  0.6133052 12.7397514 

#plot predicted verses actual
plot(testPred_lmLatB0,testingLatB0$LATITUDE)
abline(1,1)

plot(testPred_lmLatB1,testingLatB1$LATITUDE)
abline(1,1)

plot(testPred_lmLatB2,testingLatB2$LATITUDE)
abline(1,1)
