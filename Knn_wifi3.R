#using new data frame, wifi3 with reduced columns, test KNN model

#load libraries
library(dplyr)
library(ggplot2)
library(caret)
library(grid)
library(gridExtra)

#Load the necessary data
setwd("~/Ubiqum/Project 4/Wifi Task/UJIndoorLoc")
wifi <- read.csv("trainingData.csv")

#create a subset of just building 1, floor 1
wifi3B1F1 <- wifi3 %>% 
  filter(BUILDINGID == 1) %>%
  filter(FLOOR == 1)



#remove all features other than WAPs and Latitude
B1F1LAT3 <- select(wifi3B1F1, WAP006:WAP517, LATITUDE)
B1F1LON3 <- select(wifi3B1F1, WAP006:WAP517, LONGITUDE)

#trying running model on B1F1 subset to predict latitude
set.seed(234)

# define an 75%/25% train/test split of the dataset
inTraining9 <- createDataPartition(B1F1LAT3$LATITUDE, p = .75, list = FALSE)
training9 <- B1F1LAT3[inTraining9,]
testing9 <- B1F1LAT3[-inTraining9,]

#10 fold cross validation
fitControl9 <- trainControl(method = "repeatedcv", number = 10, repeats = 2)

#train kNN classification model
kNN_Model9 <- train(LATITUDE~., data = training9, method = "knn", trControl=fitControl9)

#predictor variables
predictors(kNN_Model9)

#make predictions
testPred_kNN9 <- predict(kNN_Model9, testing9)

#performace measurment
postResample(testPred_kNN9, testing9$LATITUDE)
# output
RMSE  Rsquared       MAE 
5.2696535 0.9819977 2.2217492

#view kNN model
kNN_Model9


#plot predicted verses actual
plot(testPred_kNN9,testing9$LATITUDE)
abline(1,1)



#trying running model on B1F1 subset to predict longitude
set.seed(234)

# define an 75%/25% train/test split of the dataset
inTraining10 <- createDataPartition(B1F1LON3$LONGITUDE, p = .75, list = FALSE)
training10 <- B1F1LON3[inTraining10,]
testing10 <- B1F1LON3[-inTraining10,]

#10 fold cross validation
fitControl10 <- trainControl(method = "repeatedcv", number = 10, repeats = 2)

#train kNN classification model
kNN_Model10<- train(LONGITUDE~., data = training10, method = "knn", trControl=fitControl10)

#predictor variables
predictors(kNN_Model10)

#make predictions
testPred_kNN10 <- predict(kNN_Model10, testing10)

#performace measurment
postResample(testPred_kNN10, testing10$LONGITUDE)
# output
RMSE  Rsquared       MAE 
3.8665836 0.9931378 2.0348402 

#view kNN model
kNN_Model10


#plot predicted verses actual
plot(testPred_kNN10,testing10$LONGITUDE)
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

#add actual values to df with predictions and calculate size of errors
wifiTEST3$ActLat <- wifi3B1F1$LATITUDE
wifiTEST3$ActLon <- wifi3B1F1$LONGITUDE
colnames(wifiTEST3)

wifi3errors <- wifiTEST3 %>%
  mutate(diffLAT = (LAT - ActLat)) %>%
  mutate(diffLON = (LON - ActLon))

#review distribution of errors
summary(wifi3errors$diffLON) 
summary(wifi3errors$diffLAT)

hist(wifi3errors$diffLON) 
hist(wifi3errors$diffLAT)

boxplot(wifi3errors$diffLON) 
boxplot(wifi3errors$diffLAT)

#subset the error df by selecting the biggest errors to analyze further
errors1 <- filter(wifi3errors, abs(diffLON) > 5)

#remove cols that have all -101s
means101 <- colMeans(errors1) #finds mean of each column
mean101 <- which(means101 == -101) #there are 55 WAPs with mean 100

WAPs101 <- names(mean101)

errors2 <- select(errors1, -WAPs101) #removes all WAPs with only 100s

#create a total number of WAPs each observation is registering on
errors2$Obs_Num <- c(1:181)
errors3 <- gather(errors2, "WAP", "dBM", WAP008:WAP191)

#filter out all -101 values to just show actual signals
errors4 <- filter(errors3, dBM != -101)

errors5 <- errors4 %>%
  group_by(Obs_Num) %>%
  count(Obs_Num)

#how many WAPs do most observations hit on?
wifi$Obs_Num <- c(1:19937)
wifiTidy <- gather(wifi, "WAP", "dBM", WAP001:WAP520)

#filter out all -101 values to just show actual signals
wifiTidy2 <- filter(wifiTidy, dBM != 100)

wifiTidy4 <- filter(wifiTidy2, dBM > -50) #how many obs have dBMs over -50? (11,039)
#take a randomly selected obs, and see what chart of WAP hits looks like
#compare this to the WAP charts of the biggest errors below

p10 <- wifiTidy2 %>%
  filter(Obs_Num == 10314) %>%
  ggplot(aes(WAP, dBM)) +
  geom_col()+
  labs(title = "Obs 10314")

wifiTidy3 <- wifiTidy2 %>%
  group_by(Obs_Num) %>%
  count(Obs_Num)

ggplot(wifiTidy3, aes(n)) +
  geom_bar() +
  labs(title = "Distribution of how many WAPs each observation registers")

p1 <- errors4 %>%
  filter(Obs_Num == 1) %>%
  ggplot(aes(WAP, dBM)) +
  geom_col()+
  labs(title = "Obs 1")

p2 <-errors4 %>%
  filter(Obs_Num == 2) %>%
  ggplot(aes(WAP, dBM)) +
  geom_col()+
  labs(title = "Obs 2")

p3 <-errors4 %>%
  filter(Obs_Num == 3) %>%
  ggplot(aes(WAP, dBM)) +
  geom_col()+
  labs(title = "Obs 3")

p4 <-errors4 %>%
  filter(Obs_Num == 4) %>%
  ggplot(aes(WAP, dBM)) +
  geom_col()+
  labs(title = "Obs 4")

p5 <-errors4 %>%
  filter(Obs_Num == 5) %>%
  ggplot(aes(WAP, dBM)) +
  geom_col()+
  labs(title = "Obs 5")

p6 <-errors4 %>%
  filter(Obs_Num == 6) %>%
  ggplot(aes(WAP, dBM)) +
  geom_col()+
  labs(title = "Obs 6")

p7 <-errors4 %>%
  filter(Obs_Num == 7) %>%
  ggplot(aes(WAP, dBM)) +
  geom_col() +
  labs(title = "Obs 7")

p8 <-errors4 %>%
  filter(Obs_Num == 8) %>%
  ggplot(aes(WAP, dBM)) +
  geom_col() +
  labs(title = "Obs 8")

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol = 4)

#try making a chart of the values of the WAPs from errors2
WAPsum <- colSums(errors2, na.rm = TRUE, dims = 1)
WAPsumsort <- sort(WAPsum, decreasing = TRUE)


#plot actual coordinates of errors2 onto the "map" of the buildings
ggplot() +
  geom_jitter(aes(wifi3B1F1$LONGITUDE, wifi3B1F1$LATITUDE)) +
  geom_jitter(aes(x=errors2$ActLon, y=errors2$ActLat), color = "red")

#compare the amount of the error with the number of WAP hits
#create a total number of WAPs each observation is registering on
wifi3errors$Obs_Num <- c(1:1484)
wifi3errors1<- gather(wifi3errors, "WAP", "dBM", WAP006:WAP517)

#filter out all -101 values to just show actual signals
wifi3errors1 <- filter(wifi3errors1, dBM != -101)

wifi3errors2 <- wifi3errors1 %>%
  group_by(Obs_Num) %>%
  count(Obs_Num)

wifi3errors$WAPcount <- wifi3errors2$n
saveRDS(wifi3errors, "wifi3_errors")

ggplot(wifi3errors, aes(abs(diffLON), WAPcount)) +
  geom_point() +
  labs(title = "Error amount (Longitude) compared to WAP count")

ggplot(wifi3errors, aes(abs(diffLAT), WAPcount)) +
  geom_point() +
  labs(title = "Error amount (Latitude) compared to WAP count")

#compare the amount of the error with the sum of signal strength of all WAP hits
wifi3errors[wifi3errors == -101] <- 0
wifi3errors <- select(wifi3errors, -Strength_total)

strength_total <- rowSums(wifi3errors[1:321])

wifi3errors$stength_tot <- strength_total

ggplot(wifi3errors, aes(abs(diffLAT), stength_tot)) +
  geom_point() +
  labs(title = "Error amount (Latitude) compared to sum of signal strength")

ggplot(wifi3errors, aes(abs(diffLON), stength_tot)) +
  geom_point() +
  labs(title = "Error amount (Longitude) compared to sum of signal strength")
