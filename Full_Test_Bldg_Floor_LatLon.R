#use validation data and models to predict building, floor, then lat and long

library(dplyr)
library(tidyr)
library(ggplot2)
options(scipen=999)

#load all final models
setwd("~/Ubiqum/Project 4/Wifi Task/UJIndoorLoc/Models/Building")
B_gbm <- readRDS("mod_Bldg_gbm.rds")

setwd("~/Ubiqum/Project 4/Wifi Task/UJIndoorLoc/Models/Floor")
F_B0 <- readRDS("mod_Flr_B0.rds")
F_B1 <- readRDS("mod_Flr_B1.rds")
F_B2 <- readRDS("mod_Flr_B2.rds")

setwd("~/Ubiqum/Project 4/Wifi Task/UJIndoorLoc/Models/Latitude")
Lat_B0 <- readRDS("Mod_LatB0_Knn.rds")
Lat_B1 <- readRDS("Mod_LatB1_Knn.rds")
Lat_B2 <- readRDS("Mod_LatB2_Knn.rds")

setwd("~/Ubiqum/Project 4/Wifi Task/UJIndoorLoc/Models/Longitude")
Lon_B0 <- readRDS("Mod_LongB0_Knn.rds")
Lon_B1 <- readRDS("Mod_LongB1_Knn.rds")
Lon_B2 <- readRDS("Mod_LongB2_Knn.rds")

#load validation data
setwd("~/Ubiqum/Project 4/Wifi Task/UJIndoorLoc")
Val <- readRDS("ValClean.rds")

#predict building on validation data

#deploy GBm model on validation data
BldgPreds <- select(Val, WAP006:WAP517)


#make predictions using gbm model
BldgPred <- predict(B_gbm, BldgPreds)

#performace measurment
postResample(BldgPred, Val$BUILDINGID)

Accuracy     Kappa 
0.9990999 0.9985778  

#add predictions to validation data
Val$B_Pred <- BldgPred

#########################################################
#now predict floor by buildling

#Building 0
ValB0p <- filter(Val, B_Pred == 0)
ValB0p$FLOOR <- as.integer(ValB0p$FLOOR)
ValB0p$FLOOR <- as.factor(ValB0p$FLOOR)
ValB0pp <- select(ValB0p, WAP006:WAP517)

#make predictions
F_B0pred <- predict(F_B0, ValB0pp)
postResample(F_B0pred, ValB0p$FLOOR)
Accuracy     Kappa 
0.9719626 0.9603888 

#Add prediction to data set
ValB0p$F_Pred <- F_B0pred

#Building 1
ValB1p <- filter(Val, B_Pred == 1)
ValB1p$FLOOR <- as.integer(ValB1p$FLOOR)
ValB1p$FLOOR <- as.factor(ValB1p$FLOOR)
ValB1pp <- select(ValB1p, WAP006:WAP517)

#make predictions
F_B1pred <- predict(F_B1, ValB1pp)
postResample(F_B1pred, ValB1p$FLOOR)
Accuracy     Kappa 
0.7824675 0.6858865 

#add prediction to data set
ValB1p$F_Pred <- F_B1pred


#Building 2
ValB2p <- filter(Val, B_Pred == 2)

ValB2pp <- select(ValB2p, WAP006:WAP517)

#make predictions
F_B2pred <- predict(F_B2, ValB2pp)
postResample(F_B2pred, ValB2p$FLOOR)
Accuracy     Kappa 
0.8656716 0.8171550 

#add prediction to data set
ValB2p$F_Pred <- F_B2pred

#########################################################
#now predict latitude by buildling

#Building 0
#make predictions
Lat_B0pred <- predict(Lat_B0, ValB0pp)
postResample(Lat_B0pred, ValB0p$LATITUDE)
RMSE       Rsquared       MAE 
4.9238544 0.9762312 3.2256496 

#Add prediction to data set
ValB0p$Lat_Pred <- Lat_B0pred

#Building 1
#make predictions
Lat_B1pred <- predict(Lat_B1, ValB1pp)
postResample(Lat_B1pred, ValB1p$LATITUDE)
RMSE        Rsquared        MAE 
12.2607625  0.8822371  6.9902901 

#Add prediction to data set
ValB1p$Lat_Pred <- Lat_B1pred

#Building 2
#make predictions
Lat_B2pred <- predict(Lat_B2, ValB2pp)
postResample(Lat_B2pred, ValB2p$LATITUDE)
RMSE  Rsquared       MAE 
9.1947365 0.9019004 6.3374432  

#Add prediction to data set
ValB2p$Lat_Pred <- Lat_B2pred


#########################################################
#now predict longitude by buildling

#Building 0
#make predictions
ValB0ppp <- select(ValB0p, WAP006:WAP517, LATITUDE)
ValB1ppp <- select(ValB1p, WAP006:WAP517, LATITUDE)
ValB2ppp <- select(ValB2p, WAP006:WAP517, LATITUDE)

Lon_B0pred <- predict(Lon_B0, ValB0ppp)
postResample(Lon_B0pred, ValB0p$LONGITUDE)
RMSE  Rsquared       MAE 
5.7053786 0.9541593 3.4998480 

#Add prediction to data set
ValB0p$Lon_Pred <- Lon_B0pred

#Building 1
#make predictions
Lon_B1pred <- predict(Lon_B1, ValB1ppp)
postResample(Lon_B1pred, ValB1p$LONGITUDE)
RMSE   Rsquared        MAE 
10.7168540  0.9464445  6.4922576 

#Add prediction to data set
ValB1p$Lon_Pred <- Lon_B1pred

#Building 2
#make predictions
Lon_B2pred <- predict(Lon_B2, ValB2ppp)
postResample(Lon_B2pred, ValB2p$LONGITUDE)
RMSE   Rsquared        MAE 
13.1728780  0.8314732  7.6472452   

#Add prediction to data set
ValB2p$Lon_Pred <- Lon_B2pred

#save datasets
saveRDS(ValB0p, "ValB0p.rds")
saveRDS(ValB1p, "ValB1p.rds")
saveRDS(ValB2p, "ValB2p.rds")

ValB0f <- ValB0p %>%
  select(LONGITUDE:Lon_Pred) %>%
  mutate(B_correct = BUILDINGID == B_Pred) %>%
  mutate(F_correct = FLOOR == F_Pred) %>%
  mutate(Lat_error = abs(LATITUDE - Lat_Pred)) %>%
  mutate(Lon_error = abs(LONGITUDE - Lon_Pred))


ValB1f <- ValB1p %>%
  select(LONGITUDE:Lon_Pred) %>%
  mutate(B_correct = BUILDINGID == B_Pred) %>%
  mutate(F_correct = FLOOR == F_Pred) %>%
  mutate(Lat_error = abs(LATITUDE - Lat_Pred)) %>%
  mutate(Lon_error = abs(LONGITUDE - Lon_Pred))

ValB2f <- ValB2p %>%
  select(LONGITUDE:Lon_Pred) %>%
  mutate(B_correct = BUILDINGID == B_Pred) %>%
  mutate(F_correct = FLOOR == F_Pred) %>%
  mutate(Lat_error = abs(LATITUDE - Lat_Pred)) %>%
  mutate(Lon_error = abs(LONGITUDE - Lon_Pred))

write.csv(ValB0f, "ValB0p.csv")
write.csv(ValB1f, "ValB1p.csv")
write.csv(ValB2f, "ValB2p.csv")


B2details <- VALB2 %>%
  group_by(FLOOR) %>%
  summarise(sum(F_correct == TRUE), sum(F_correct == FALSE))

B1details <- VALB1 %>%
  group_by(FLOOR) %>%
  summarise(sum(F_correct == TRUE), sum(F_correct == FALSE))

B0details <- VALB0 %>%
  group_by(FLOOR) %>%
  summarise(sum(F_correct == TRUE), sum(F_correct == FALSE))
