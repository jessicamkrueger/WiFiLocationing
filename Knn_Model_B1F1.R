
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

#visualize B1F1
boxplot(wifiB1F1[,1:25])
boxplot(wifiB1F1[,26:50])
boxplot(wifiB1F1[,51:75])       
boxplot(wifiB1F1[,76:100]) 
boxplot(wifiB1F1[,101:150]) 

summary(wifiB1F1$WAP090)

#remove all features other than WAPs and Latitude
B1F1LAT <- select(wifiB1F1, WAP001:WAP520, LATITUDE)
B1F1LON <- select(wifiB1F1, WAP001:WAP520, LONGITUDE)

#trying running model on B1F1 subset to predict latitude
set.seed(234)

# define an 75%/25% train/test split of the dataset
inTraining <- createDataPartition(B1F1LAT$LATITUDE, p = .75, list = FALSE)
training <- B1F1LAT[inTraining,]
testing <- B1F1LAT[-inTraining,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 2)

#train kNN classification model
kNN_Model1 <- train(LATITUDE~., data = training, method = "knn", trControl=fitControl)

#predictor variables
predictors(kNN_Model)

#make predictions
testPred_kNN1 <- predict(kNN_Model1, testing)

#performace measurment
postResample(testPred_kNN1, testing$LATITUDE)
# output
#RMSE Rsquared      MAE 
#6.281632 0.974411 3.509039 

#view kNN model
kNN_Model


#plot predicted verses actual
plot(testPred_kNN1,testing$LATITUDE)
abline(1,1)

summary(testPred_kNN1)
summary(testing$LATITUDE)

#WAP105 listed as most important variable
summary(B1F1$WAP105)


#trying running model on B1F1 subset to predict longitude
set.seed(234)

# define an 75%/25% train/test split of the dataset
inTraining2 <- createDataPartition(B1F1LON$LONGITUDE, p = .75, list = FALSE)
training2 <- B1F1LON[inTraining2,]
testing2 <- B1F1LON[-inTraining2,]

#10 fold cross validation
fitControl2 <- trainControl(method = "repeatedcv", number = 10, repeats = 2)

#train kNN classification model
kNN_Model2<- train(LONGITUDE~., data = training2, method = "knn", trControl=fitControl2)

#predictor variables
predictors(kNN_Model2)

#make predictions
testPred_kNN2 <- predict(kNN_Model2, testing2)

#performace measurment
postResample(testPred_kNN2, testing2$LONGITUDE)
# output
#RMSE  Rsquared       MAE 
#5.4253880 0.9868761 3.3356870 

#view kNN model
kNN_Model2


#plot predicted verses actual
plot(testPred_kNN2,testing2$LONGITUDE)
abline(1,1)

summary(testPred_kNN2)
summary(testing2$LONGITUDE)



