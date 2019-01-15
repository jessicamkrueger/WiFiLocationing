#rf model for floor prediction using WAPs and Building

#preditct building with wifi5 dataset

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
FLR <- select(wifi6, WAP006:WAP517, BUILDINGID, FLOOR)

#trying running model on entire dataset to predict building
set.seed(234)

# define an 75%/25% train/test split of the dataset
inTraining2 <- createDataPartition(FLR$FLOOR, p = .75, list = FALSE)
training2 <- FLR[inTraining2,]
testing2 <- FLR[-inTraining2,]

#10 fold cross validation
fitControl2 <- trainControl(method = "repeatedcv", number = 2, repeats = 1)
#putting 1 for number will give you an error, should always be at least 2 :)

#train classification model
F_rf <- train(FLOOR~., data = training2, method = "cforest", trControl=fitControl2)

#make predictions
predF_rf <- predict(F_rf, testing2)

#performace measurment
postResample(predF_rf, testing2$FLOOR)
# output


#check confusion matrix
confusionMatrix(predF_rf, testing2$FLOOR)
