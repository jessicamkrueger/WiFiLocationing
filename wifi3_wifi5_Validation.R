#try model on validation set

VAL <- read.csv("validationData.csv")
VAL[VAL==100] <- -101
VALtest <- select(VAL, -WAPs80, -WAPs)
VALtest <- filter(VALtest, BUILDINGID == 1 & FLOOR == 1)


#deploy model on wifi3 data

LatTest1 <- select(VALtest, WAP006:WAP517)

#make predictions lat using wifi3 model
latPred1 <- predict(kNN_Model9, LatTest1)

#performace measurment
postResample(latPred1, VALtest$LATITUDE)

RMSE   Rsquared        MAE 
11.0768943  0.9171034  6.6453003 
#not great

#deploy model on wifi5 data

LatTest1 <- select(VALtest, WAP006:WAP517)

#make predictions lat using wifi3 model
latPred2 <- predict(kNN_Model13, LatTest1)

#performace measurment
postResample(latPred2, VALtest$LATITUDE)
RMSE   Rsquared        MAE 
11.0768943  0.9171034  6.6453003 
#not great