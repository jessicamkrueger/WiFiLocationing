
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
F_rfB0 <- train(FLOOR~., data = training0, method = "rf", trControl=fitControl)
F_rfB1 <- train(FLOOR~., data = training1, method = "rf", trControl=fitControl)
F_rfB2 <- train(FLOOR~., data = training2, method = "rf", trControl=fitControl)

#make predictions
predF_rfB0 <- predict(F_rfB0, testing0)
predF_rfB1 <- predict(F_rfB1, testing1)
predF_rfB2 <- predict(F_rfB2, testing2)

#performace measurment
postResample(predF_rfB0, testing0$FLOOR)
postResample(predF_dtB1, testing1$FLOOR)
postResample(predF_dtB2, testing2$FLOOR)

# output
#Building 0
Accuracy     Kappa 
0.9969466    0.9959100 

#Building 1
Accuracy     Kappa 
0.6099456    0.4626501 

#Building 2
Accuracy     Kappa 
0.4108417    0.2160599

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