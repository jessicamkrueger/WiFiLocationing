#load libraries
library(dplyr)
library(ggplot2)
library(caret)

#Load the necessary data
setwd("~/Ubiqum/Project 4/Wifi Task/UJIndoorLoc")
wifi <- read.csv("trainingData.csv")

#create a subset of just building 1, floor 1
wifiB1F1 <- wifi %>% 
  filter(BUILDINGID == 1) %>%
  filter(FLOOR == 1)



#remove all features other than WAPs and Latitude
B1F1LAT <- select(wifiB1F1, WAP001:WAP520, LATITUDE)
B1F1LON <- select(wifiB1F1, WAP001:WAP520, LONGITUDE)

#trying running model on B1F1 subset to predict latitude
set.seed(234)

# define an 75%/25% train/test split of the dataset
inTraining8 <- createDataPartition(B1F1LAT$LATITUDE, p = .75, list = FALSE)
training8 <- B1F1LAT[inTraining8,]
testing8 <- B1F1LAT[-inTraining8,]

#10 fold cross validation
fitControl8 <- trainControl(method = "repeatedcv", number = 10, repeats = 2)

#train kNN classification model
rf_Model8 <- train(LATITUDE~., data = training, method = "cforest", trControl=fitControl8)

#predictor variables
predictors(rf_Model8)

#make predictions
testPred_rf8 <- predict(rf_Model8, testing8)

#performace measurment
postResample(testPred_rf8, testing8$LATITUDE)
# output
RMSE  Rsquared       MAE 
6.9021709 0.9691811 4.5144857 

#view kNN model
rf_Model8


#plot predicted verses actual
plot(testPred_rf8,testing8$LATITUDE)
abline(1,1)




#deploy model

wifiTEST2 <- select(wifiB1F1, WAP001:WAP520)

wifiTestLat2 <- predict(lm_Model6, wifiTEST2)
wifiTEST2$LAT <- wifiTestLat2

wifiTestLon2 <- predict(lm_Model7, wifiTEST2)
wifiTEST2$LON <- wifiTestLon2

#plot liner regression predictions against the actual values
ggplot() +
  geom_point(aes(x=wifiTEST2$LON, y=wifiTEST2$LAT), color = "red") +
  geom_point(aes(wifiB1F1$LONGITUDE, wifiB1F1$LATITUDE))

ggplot() +
  geom_point(aes(x=wifiTEST2$LON, y=wifiTEST2$LAT), color = "red") +
  geom_point(aes(wifi$LONGITUDE, wifi$LATITUDE))