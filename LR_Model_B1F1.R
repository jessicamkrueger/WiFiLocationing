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
inTraining6 <- createDataPartition(B1F1LAT$LATITUDE, p = .75, list = FALSE)
training6 <- B1F1LAT[inTraining6,]
testing6 <- B1F1LAT[-inTraining6,]

#10 fold cross validation
fitControl6 <- trainControl(method = "repeatedcv", number = 10, repeats = 2)

#train kNN classification model
lm_Model6 <- train(LATITUDE~., data = training, method = "lm", trControl=fitControl6)

#predictor variables
predictors(lm_Model6)

#make predictions
testPred_lm6 <- predict(lm_Model6, testing6)

#performace measurment
postResample(testPred_lm6, testing6$LATITUDE)
# output
#RMSE Rsquared      MAE 
#6.281632 0.974411 3.509039 

#view kNN model
lm_Model6


#plot predicted verses actual
plot(testPred_lm6,testing6$LATITUDE)
abline(1,1)



#WAP105 listed as most important variable
summary(B1F1$WAP105)


#trying running model on B1F1 subset to predict longitude
set.seed(234)

# define an 75%/25% train/test split of the dataset
inTraining7 <- createDataPartition(B1F1LON$LONGITUDE, p = .75, list = FALSE)
training7 <- B1F1LON[inTraining7,]
testing7 <- B1F1LON[-inTraining7,]

#10 fold cross validation
fitControl7 <- trainControl(method = "repeatedcv", number = 10, repeats = 2)

#train kNN classification model
lm_Model7<- train(LONGITUDE~., data = training2, method = "lm", trControl=fitControl7)

#predictor variables
predictors(lm_Model7)

#make predictions
testPred_lm7 <- predict(lm_Model7, testing7)

#performace measurment
postResample(testPred_lm7, testing7$LONGITUDE)
# output
#RMSE  Rsquared       MAE 
#5.4253880 0.9868761 3.3356870 

#view kNN model
lm_Model7


#plot predicted verses actual
plot(testPred_lm7,testing7$LONGITUDE)
abline(1,1)

summary(testPred_kNN2)
summary(testing2$LONGITUDE)

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
