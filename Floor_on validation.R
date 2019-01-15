#knn model for floor prediction on validation set

#load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(grid)
library(gridExtra)

#Load the necessary data
setwd("~/Ubiqum/Project 4/Wifi Task/UJIndoorLoc")

Val <- readRDS("ValClean.rds")
wifi6 <- readRDS("wifi6.rds")

#deploy model on validation data

FloorVal <- select(Val, WAP006:WAP517, BUILDINGID)

#make predictions lat using wifi3 model
FloorPredV <- predict(F_knn, FloorVal)

#performace measurment
postResample(FloorPredV, Val$FLOOR)
Accuracy     Kappa 
0.9045905    0.8667385 

#confusion matrix 
confusionMatrix(FloorPredV, Val$FLOOR)
Confusion Matrix and Statistics

             Reference
Prediction    0   1   2   3   4
          0 117   7   1   0   1
          1   8 405   6   0   0
          2   6  46 290   3   0
          3   1   4   9 169  14
          4   0   0   0   0  24

Overall Statistics

Accuracy : 0.9046          
95% CI : (0.8858, 0.9212)
No Information Rate : 0.4158          
P-Value [Acc > NIR] : < 2.2e-16       

Kappa : 0.8667          
Mcnemars Test P-Value : NA              

Statistics by Class:

                       Class: 0 Class: 1 Class: 2 Class: 3 Class: 4
Sensitivity            0.8864   0.8766   0.9477   0.9826   0.6154
Specificity            0.9908   0.9784   0.9317   0.9702   1.0000
Pos Pred Value         0.9286   0.9666   0.8406   0.8579   1.0000
Neg Pred Value         0.9848   0.9176   0.9791   0.9967   0.9862
Prevalence             0.1188   0.4158   0.2754   0.1548   0.0351
Detection Rate         0.1053   0.3645   0.2610   0.1521   0.0216
Detection Prevalence   0.1134   0.3771   0.3105   0.1773   0.0216
Balanced Accuracy      0.9386   0.9275   0.9397   0.9764   0.8077


#evaluate errors 
Val$FPrediction <- FloorPredV #add predictions to dataset

Val$Error <- Val$FPrediction == Val$FLOOR #add column to determine if prediciton was correct

FlErrors <- filter(Val, Error == FALSE) #create data set of just the errors

#plot the errors to find patterns
ggplot(FlErrors, aes(BUILDINGID))+
  geom_bar() #mostly bldg 1

ggplot(FlErrors, aes(PHONEID))+ 
  geom_bar() #not useful, all the same userID, but most errors come from phone 13 and 20

ggplot(FlErrors, aes(TIMESTAMP))+ 
  geom_histogram(aes(fill = BUILDINGID)) #mostly on one day but that's the day when most of the data was collected

ggplot(FlErrors, aes(RELATIVEPOSITION)) +
  geom_bar()

ggplot() +
  geom_point(aes(wifi6$LONGITUDE, wifi6$LATITUDE), color = "light gray") +
  geom_point(aes(FlErrors$LONGITUDE, FlErrors$LATITUDE), color = "dark blue") +
  
  labs(x = "Longitude", y = "Latitude", title = "Errors from k-NN Floor Prediction Model") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line =  element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(colour = "light gray"),
        axis.title.y = element_text(colour = "light gray"),
        plot.title = element_text(color= "dark gray"))

#make the data long with gathering
FlErrorsT <- gather(FlErrors, WAP, Value, WAP006:WAP517)

ggplot(FlErrorsT, aes(WAP, Value)) +
  geom_violin()
  

summary(FlErrors)
