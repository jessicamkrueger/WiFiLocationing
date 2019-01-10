#using new data frame, wifi5 with bad data removed, test KNN model

#load libraries
library(dplyr)
library(ggplot2)
library(caret)
library(grid)
library(gridExtra)

#Load the necessary data
setwd("~/Ubiqum/Project 4/Wifi Task/UJIndoorLoc")
wifi5 <- readRDS("wifi5.rds")

#create a subset of just building 1, floor 1
wifi5B1F1 <- wifi5 %>% 
  filter(BUILDINGID == 1) %>%
  filter(FLOOR == 1)



#remove all features other than WAPs and Latitude
B1F1LAT5 <- select(wifi5B1F1, WAP006:WAP517, LATITUDE)
B1F1LON5 <- select(wifi5B1F1, WAP006:WAP517, LONGITUDE)

#trying running model on B1F1 subset to predict latitude
set.seed(234)

# define an 75%/25% train/test split of the dataset
inTraining13 <- createDataPartition(B1F1LAT5$LATITUDE, p = .75, list = FALSE)
training13 <- B1F1LAT5[inTraining13,]
testing13 <- B1F1LAT5[-inTraining13,]

#10 fold cross validation
fitControl13 <- trainControl(method = "repeatedcv", number = 10, repeats = 2)

#train kNN classification model
kNN_Model13 <- train(LATITUDE~., data = training13, method = "knn", trControl=fitControl13)


#make predictions
testPred_kNN13 <- predict(kNN_Model13, testing13)

#performace measurment
postResample(testPred_kNN13, testing13$LATITUDE)
# output
RMSE  Rsquared       MAE 
5.2696535 0.9819977 2.2217492

#view kNN model
kNN_Model9


#plot predicted verses actual
plot(testPred_kNN13,testing13$LATITUDE)
abline(1,1)



#trying running model on B1F1 subset to predict longitude
set.seed(234)

# define an 75%/25% train/test split of the dataset
inTraining14 <- createDataPartition(B1F1LON5$LONGITUDE, p = .75, list = FALSE)
training14 <- B1F1LON5[inTraining14,]
testing14 <- B1F1LON5[-inTraining14,]

#10 fold cross validation
fitControl14 <- trainControl(method = "repeatedcv", number = 10, repeats = 2)

#train kNN classification model
kNN_Model14<- train(LONGITUDE~., data = training14, method = "knn", trControl=fitControl14)


#make predictions
testPred_kNN14 <- predict(kNN_Model14, testing14)

#performace measurment
postResample(testPred_kNN14, testing14$LONGITUDE)
# output
RMSE  Rsquared       MAE 
3.8665836 0.9931378 2.0348402 

#view kNN model
kNN_Model14


#plot predicted verses actual
plot(testPred_kNN14,testing14$LONGITUDE)
abline(1,1)
