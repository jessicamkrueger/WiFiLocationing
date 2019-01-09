#using new data frame, wifi3 with reduced columns, test KNN model

#load libraries
library(dplyr)
library(ggplot2)
library(caret)

#Load the necessary data
setwd("~/Ubiqum/Project 4/Wifi Task/UJIndoorLoc")
wifi <- read.csv("trainingData.csv")

#create a subset of just building 1, floor 1
wifi4B1F1 <- wifi4 %>% 
  filter(BUILDINGID == 1) %>%
  filter(FLOOR == 1)



#remove all features other than WAPs and Latitude
B1F1LAT4 <- select(wifi4B1F1, WAP006:WAP517, LATITUDE)
B1F1LON4 <- select(wifi4B1F1, WAP006:WAP517, LONGITUDE)

#trying running model on B1F1 subset to predict latitude
set.seed(234)

# define an 75%/25% train/test split of the dataset
inTraining11 <- createDataPartition(B1F1LAT4$LATITUDE, p = .75, list = FALSE)
training11 <- B1F1LAT4[inTraining11,]
testing11 <- B1F1LAT4[-inTraining11,]

#10 fold cross validation
fitControl11 <- trainControl(method = "repeatedcv", number = 10, repeats = 2)

#train kNN classification model
kNN_Model11 <- train(LATITUDE~., data = training11, method = "knn", trControl=fitControl11)

#predictor variables
predictors(kNN_Model11)

#make predictions
testPred_kNN11 <- predict(kNN_Model11, testing11)

#performace measurment
postResample(testPred_kNN11, testing11$LATITUDE)
# output
RMSE  Rsquared       MAE 
5.3323026 0.9816214 2.2532309 

#view kNN model
kNN_Model11


#plot predicted verses actual
plot(testPred_kNN11,testing11$LATITUDE)
abline(1,1)



#trying running model on B1F1 subset to predict longitude
set.seed(234)

# define an 75%/25% train/test split of the dataset
inTraining12 <- createDataPartition(B1F1LON4$LONGITUDE, p = .75, list = FALSE)
training12 <- B1F1LON4[inTraining12,]
testing12 <- B1F1LON4[-inTraining12,]

#10 fold cross validation
fitControl12 <- trainControl(method = "repeatedcv", number = 10, repeats = 2)

#train kNN classification model
kNN_Model12<- train(LONGITUDE~., data = training12, method = "knn", trControl=fitControl12)

#predictor variables
predictors(kNN_Model12)

#make predictions
testPred_kNN12 <- predict(kNN_Model12, testing12)

#performace measurment
postResample(testPred_kNN12, testing12$LONGITUDE)
# output
RMSE  Rsquared       MAE 
3.9363626 0.9928827 2.0936898 

#view kNN model
kNN_Model12


#plot predicted verses actual
plot(testPred_kNN12,testing12$LONGITUDE)
abline(1,1)


#compare predictions to actual values
#deploy model

wifiTEST3 <- select(wifi3B1F1, WAP006:WAP517)

wifiTestLat3 <- predict(kNN_Model9, wifiTEST3)
wifiTEST3$LAT <- wifiTestLat3

wifiTestLon3 <- predict(kNN_Model10, wifiTEST3)
wifiTEST3$LON <- wifiTestLon3

#plot predictions vs. actual
ggplot() +
  geom_point(aes(x=wifiTEST3$LON, y=wifiTEST3$LAT), color = "red") +
  geom_point(aes(wifi3B1F1$LONGITUDE, wifi3B1F1$LATITUDE))