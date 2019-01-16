
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
FLRB0 <- wifi6 %>%
  filter(BUILDINGID == 0) %>%
  select(WAP006:WAP517, FLOOR)

FLRB0$FLOOR <- as.integer(FLRB0$FLOOR)
FLRB0$FLOOR <- as.factor(FLRB0$FLOOR)

FLRB1 <- wifi6 %>%
  filter(BUILDINGID == 1) %>%
  select(WAP006:WAP517, FLOOR)

FLRB1$FLOOR <- as.integer(FLRB1$FLOOR)
FLRB1$FLOOR <- as.factor(FLRB1$FLOOR)

FLRB2 <- wifi6 %>%
  filter(BUILDINGID == 2) %>%
  select(WAP006:WAP517, FLOOR)

#trying running model on entire dataset to predict building
set.seed(234)

# define an 75%/25% train/test split of the dataset
inTraining0 <- createDataPartition(FLRB0$FLOOR, p = .75, list = FALSE)
training0 <- FLRB0[inTraining0,]
testing0 <- FLRB0[-inTraining0,]

inTraining1 <- createDataPartition(FLRB1$FLOOR, p = .75, list = FALSE)
training1 <- FLRB1[inTraining1,]
testing1 <- FLRB1[-inTraining1,]

inTraining2 <- createDataPartition(FLRB2$FLOOR, p = .75, list = FALSE)
training2 <- FLRB2[inTraining2,]
testing2 <- FLRB2[-inTraining2,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 2, repeats = 1)

#train classification model
F_gbmB0 <- train(FLOOR~., data = training0, method = "gbm", trControl=fitControl, verbose = FALSE)
F_gbmB1 <- train(FLOOR~., data = training1, method = "gbm", trControl=fitControl, verbose = FALSE)
F_gbmB2 <- train(FLOOR~., data = training2, method = "gbm", trControl=fitControl, verbose = FALSE)

#make predictions
predF_gbmB0 <- predict(F_gbmB0, testing0)
predF_gbmB1 <- predict(F_gbmB1, testing1)
predF_gbmB2 <- predict(F_gbmB2, testing2)

#performace measurment
postResample(predF_gbmB0, testing0$FLOOR)
postResample(predF_gbmB1, testing1$FLOOR)
postResample(predF_gbmB2, testing2$FLOOR)

# output
#Building 0
Accuracy     Kappa 
0.9954198    0.9938647 
#made 6 errors

#Building 1
Accuracy     Kappa 
0.9930070    0.9905781 
#9 errors

#Building 2
Accuracy    Kappa 
1           1 
#noerrors

#test on validation data

#Load the necessary data
setwd("~/Ubiqum/Project 4/Wifi Task/UJIndoorLoc")

Val <- readRDS("ValClean.rds")
wifi6 <- readRDS("wifi6.rds")

ValB0 <-  Val %>%
  filter(BUILDINGID == 0) %>%
  select(WAP006:WAP517, FLOOR)
ValB0$FLOOR <- as.numeric(ValB0$FLOOR)
ValB0$FLOOR <- as.factor(ValB0$FLOOR)

ValB1 <-  Val %>%
  filter(BUILDINGID == 1) %>%
  select(WAP006:WAP517, FLOOR)
ValB1$FLOOR <- as.numeric(ValB1$FLOOR)
ValB1$FLOOR <- as.factor(ValB1$FLOOR)

ValB2 <- Val %>%
  filter(BUILDINGID == 2) %>%
  select(WAP006:WAP517, FLOOR)

#make predictions lat using wifi3 model
PredFlB0gbm <- predict(F_gbmB0, ValB0)
PredFlB1gbm<- predict(F_gbmB1, ValB1)
PredFlB2gbm <- predict(F_gbmB2, ValB2)

#performace measurment
postResample(PredFlB0gbm, ValB0$FLOOR)
postResample(PredFlB1gbm, ValB1$FLOOR)
postResample(PredFlB2gbm, ValB2$FLOOR)

#building0
Accuracy     Kappa 
0.9738806    0.9631005 
#14 errors
            Reference
Prediction    1   2   3   4
          1  76   1   1   0
          2   2 206   5   0
          3   0   0 157   2
          4   0   1   2  83

#building 1
Accuracy     Kappa 
0.7785016    0.6797865 
   
             Reference
Prediction   1  2  3  4
          1 21  1  0  0
          2  5 94  1  0
          3  3 45 82  5
          4  1  3  4 42


#building 2
Accuracy     Kappa 
0.8731343    0.8269391

             Reference
Prediction    0   1   2   3   4
          0  21   1   0   0   1
          1   3 109   3   0   0
          2   0   1  47   0   0
          3   0   0   4  40  21
          4   0   0   0   0  17