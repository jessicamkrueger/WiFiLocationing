#create plot of the actual latitude vs longitude

#load libraries
library(dplyr)
library(ggplot2)
library(caret)
library(gridExtra)
library(grid)
library(tidyr)


#Load the necessary data
setwd("~/Ubiqum/Project 4/Wifi Task/UJIndoorLoc")
wifi <- read.csv("trainingData.csv")

#create a subset of just building 1, floor 1
wifiB1F1 <- wifi %>% 
  filter(BUILDINGID == 1) %>%
  filter(FLOOR == 1)

#Plot the actual Latitudes and Longitudes for test set
actual <- data.frame("LONG" = testing2$LONGITUDE, "LAT" = testing$LATITUDE)
ggplot(actual, aes(LONG, LAT)) +
  geom_point() +
  labs(title = "Actual Locations")

#plot the predicted Latitudes and Longitudes for B1F1 based on kNN model
prediction <- data.frame("predictionLO" = testPred_kNN2, "predictionLA" = testPred_kNN1)
ggplot(prediction, aes(predictionLO, predictionLA)) +
  geom_point() +
  labs(title = "Predicted Locations")

#plot predicted and actual values on one chart
actual$predictionLO <- prediction$predictionLO
actual$predictionLA <- prediction$predictionLA
all <- gather(actual, "LoType", "Longitude", predictionLO, LONG)
all <- gather(all, "LaType", "Latitude", predictionLA, LAT)

ggplot(all, aes(Longitude, Latitude)) +
  geom_point(aes(color=LaType))


#plot latitude and longitude of the entire data set along with model predictions
ggplot() +
  geom_point(aes(x=wifiTEST$LON, y=wifiTEST$LAT), color = "red") +
  geom_point(aes(wifi$LONGITUDE, wifi$LATITUDE))

#plot predictions against actual values for B1F1
ggplot() +
  geom_point(aes(x=wifiTEST$LON, y=wifiTEST$LAT), color = "red") +
  geom_point(aes(wifiB1F1$LONGITUDE, wifiB1F1$LATITUDE))
  
