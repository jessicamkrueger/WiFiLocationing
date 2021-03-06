
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
F_knnB0 <- train(FLOOR~., data = training0, method = "knn", trControl=fitControl)
F_knnB1 <- train(FLOOR~., data = training1, method = "knn", trControl=fitControl)
F_knnB2 <- train(FLOOR~., data = training2, method = "knn", trControl=fitControl)

#make predictions
predF_knnB0 <- predict(F_knnB0, testing0)
predF_knnB1 <- predict(F_knnB1, testing1)
predF_knnB2 <- predict(F_knnB2, testing2)

#performace measurment
postResample(predF_knnB0, testing0$FLOOR)
postResample(predF_knnB1, testing1$FLOOR)
postResample(predF_knnB2, testing2$FLOOR)

# output
#Building 0
Accuracy     Kappa 
0.9992366 0.9989777 

#Building 1
Accuracy     Kappa 
0.9976690 0.9968609

#Building 2
Accuracy    Kappa 
0.999049 0.998776 

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
PredFlB0 <- predict(F_knnB0, ValB0)
PredFlB1 <- predict(F_knnB1, ValB1)
PredFlB2 <- predict(F_knnB2, ValB2)

#performace measurment
postResample(PredFlB0, ValB0$FLOOR)
postResample(PredFlB1, ValB1$FLOOR)
postResample(PredFlB2, ValB2$FLOOR)

#building0
Accuracy     Kappa 
0.9738806    0.9630649 

#building 1
Accuracy     Kappa 
0.7687296    0.6681132 

            Reference
Prediction   1  2  3  4
          1 20  4  0  0
          2  4 92  1  0
          3  6 44 79  2
          4  0  3  7 45

#building 2
Accuracy     Kappa 
0.9104478    0.8783569 
#21 errors
             Reference
Prediction    0   1   2   3   4
          0  22   1   0   0   2
          1   2 109   1   0   0
          2   0   1  49   0   0
          3   0   0   4  40  13
          4   0   0   0   0  24