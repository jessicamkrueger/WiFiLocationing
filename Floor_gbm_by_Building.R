
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


#Building 1


#Building 2
 

#test on validation data

#Load the necessary data
setwd("~/Ubiqum/Project 4/Wifi Task/UJIndoorLoc")

Val <- readRDS("ValClean.rds")
wifi6 <- readRDS("wifi6.rds")

ValB0 <- filter(Val, BUILDINGID == 0)
ValB1<- filter(Val, BUILDINGID == 1)
ValB2 <- filter(Val, BUILDINGID == 2)

#deploy model on validation data

FloorValB0 <- select(ValB0, WAP006:WAP517)

#make predictions lat using wifi3 model
PredFlB0gbm <- predict(F_gbmB0, ValB0)
PredFlB1gbm<- predict(F_gbmB1, ValB1)
PredFlB2gbm <- predict(F_gbmB2, ValB2)

#performace measurment
postResample(PredFlB0gbm, ValB0$FLOOR)
postResample(PredFlB1gbm, ValB1$FLOOR)
postResample(PredFlB2gbm, ValB2$FLOOR)

#building0
Accuracy       Kappa 
0.01492537 -0.27199180 ##what?!?!

#building 1


#building 2
