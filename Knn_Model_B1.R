
#load libraries
library(dplyr)
library(ggplot2)
library(caret)

#Load the necessary data
setwd("~/Ubiqum/Project 4/Wifi Task/UJIndoorLoc")
wifi <- read.csv("trainingData.csv")

#create a subset of just building 1, floor 1
wifiB1 <- wifi %>% 
  filter(BUILDINGID == 1)


#remove all features other than WAPs and Latitude
B1LAT <- select(wifiB1, WAP001:WAP520, LATITUDE)
B1LON <- select(wifiB1, WAP001:WAP520, LONGITUDE)
B1FLO <- select(wifiB1, WAP001:WAP520, FLOOR)

#trying running model on B1F1 subset to predict latitude
set.seed(234)

# define an 75%/25% train/test split of the dataset
inTraining <- createDataPartition(B1LAT$LATITUDE, p = .75, list = FALSE)
training3 <- B1LAT[inTraining,]
testing3 <- B1LAT[-inTraining,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 2)

#train kNN classification model
kNN_Model3 <- train(LATITUDE~., data = training3, method = "knn", trControl=fitControl)

#predictor variables
predictors(kNN_Model3)

#make predictions
testPred_kNN3 <- predict(kNN_Model3, testing3)

#performace measurment
postResample(testPred_kNN3, testing3$LATITUDE)
# output
#RMSE Rsquared      MAE 
#6.281632 0.974411 3.509039 

#view kNN model
kNN_Model3


#plot predicted verses actual
plot(testPred_kNN3,testing3$LATITUDE)
abline(1,1)

summary(testPred_kNN3)
summary(testing3$LATITUDE)

varImp(kNN_Model3, scale=FALSE)

hist(B1LAT$WAP173)


#trying running model on B1 subset to predict longitude
set.seed(234)

# define an 75%/25% train/test split of the dataset
inTraining4 <- createDataPartition(B1LON$LONGITUDE, p = .75, list = FALSE)
training4 <- B1LON[inTraining2,]
testing4 <- B1LON[-inTraining2,]

#10 fold cross validation
fitControl4 <- trainControl(method = "repeatedcv", number = 10, repeats = 2)

#train kNN classification model
kNN_Model4<- train(LONGITUDE~., data = training4, method = "knn", trControl=fitControl4)

#predictor variables
predictors(kNN_Model4)

#make predictions
testPred_kNN4 <- predict(kNN_Model4, testing4)

#performace measurment
postResample(testPred_kNN4, testing4$LONGITUDE)
# output
#RMSE   Rsquared        MAE 
#20.5304685  0.8539805 11.4066253 

#view kNN model
kNN_Model4


#plot predicted verses actual
plot(testPred_kNN4,testing4$LONGITUDE)
abline(1,1)

summary(testPred_kNN4)
summary(testing4$LONGITUDE)



#trying running model on B1F subset to predict floor
set.seed(234)
summary(B1FLO$FLOOR)
B1FLO$FLOOR <- as.numeric(B1FLO$FLOOR)
B1FLO$FLOOR <- as.factor(B1FLO$FLOOR)

# define an 75%/25% train/test split of the dataset
inTraining5 <- createDataPartition(B1FLO$FLOOR, p = .75, list = FALSE)
training5 <- B1FLO[inTraining5,]
testing5 <- B1FLO[-inTraining5,]

#10 fold cross validation
fitControl5 <- trainControl(method = "repeatedcv", number = 10, repeats = 2)

#train  model
rf5 <- train(FLOOR~., data = training5, method = "knn", trControl=fitControl5)

#predictor variables
predictors(rf5)

#make predictions
testPred_rf5 <- predict(rf5, testing5)

#performace measurment
postResample(testPred_rf5, testing5$FLOOR)
# output
#RMSE Rsquared      MAE 
#6.281632 0.974411 3.509039 

#view model
rf5


#plot predicted verses actual
plot(testPred_rf5,testing5$FLOOR)
abline(1,1)

summary(testPred_rf5)
summary(testing5$FLOOR)

confusionMatrix(rf5)
