#preditct building with wifi5 dataset

#load libraries
library(dplyr)
library(ggplot2)
library(caret)
library(grid)
library(gridExtra)

#Load the necessary data
setwd("~/Ubiqum/Project 4/Wifi Task/UJIndoorLoc")
wifi5 <- readRDS("wifi5.rds")


#remove all features other than WAPs and Latitude
B5 <- select(wifi5, WAP006:WAP517, BUILDINGID)


#trying running model on entire dataset to predict building
set.seed(234)

# define an 75%/25% train/test split of the dataset
inTraining2 <- createDataPartition(B5$BUILDINGID, p = .75, list = FALSE)
training2 <- B5[inTraining2,]
testing2 <- B5[-inTraining2,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 2, repeats = 1)

#train classification model
B_gbm <- train(BUILDINGID~., data = training2, method = "gbm", trControl=fitControl, verbose = FALSE)


#make predictions
predB_gbm <- predict(B_gbm, testing2)

#performace measurment
postResample(predB_gbm, testing2$BUILDINGID)
# output
Accuracy     Kappa 
0.9983062 0.9973835 


#check confusion matrix
confusionMatrix(predB_gbm,testing2$BUILDINGID)

Confusion Matrix and Statistics

              Reference
Prediction    0     1     2
          0   1312  0     0
          1   0     1297  8
          2   0     0     2106

Overall Statistics

Accuracy : 0.9983          
95% CI : (0.9967, 0.9993)
No Information Rate : 0.4476          
P-Value [Acc > NIR] : < 2.2e-16       

Kappa : 0.9974          
Mcnemars Test P-Value : NA              

Statistics by Class:

Class: 0 Class: 1 Class: 2
Sensitivity            1.0000   1.0000   0.9962
Specificity            1.0000   0.9977   1.0000
Pos Pred Value         1.0000   0.9939   1.0000
Neg Pred Value         1.0000   1.0000   0.9969
Prevalence             0.2778   0.2746   0.4476
Detection Rate         0.2778   0.2746   0.4459
Detection Prevalence   0.2778   0.2763   0.4459
Balanced Accuracy      1.0000   0.9988   0.9981


#review errors 
saveRDS(GBMerrors, "GBMbldgErrros.rds")
GMBerrors <- readRDS("GBMbldgErrros.rds")

GBMmean <- colMeans(GBMerrors[1:321])
sum(GBMmean == -101)
sum(GBMerrors[1:321] != -101) 
#the 8 observations don't have any signal detected on any WAP


#deploy model on validation data
#try model on validation set
setwd("~/Ubiqum/Project 4/Wifi Task/UJIndoorLoc")
VAL <- read.csv("validationData.csv")

VAL[VAL==100] <- -101
VALtest <- select(VAL, -WAPs80, -WAPs)
VALtest$BUILDINGID <- as.factor(VALtest$BUILDINGID)
VALtest$FLOOR <- as.factor(VALtest$FLOOR)
VALtest$SPACEID <- as.factor(VALtest$SPACEID)
VALtest$RELATIVEPOSITION <- as.factor(VALtest$RELATIVEPOSITION)
VALtest$USERID <- as.factor(VALtest$USERID)
VALtest$PHONEID <- as.factor(VALtest$PHONEID)

saveRDS(VALtest, "ValClean.rds")
ValData <- readRDS("ValClean.rds")

#deploy GBm model on validation data
gbmTest <- select(VALtest, WAP006:WAP517)


#make predictions using gbm model
gbmPred <- predict(B_gbm, gbmTest)

#performace measurment
postResample(gbmPred, VALtest$BUILDINGID)
Accuracy     Kappa 
0.9990999 0.9985778 

confusionMatrix(gbmPred, VALtest$BUILDINGID)

Confusion Matrix and Statistics

              Reference
Prediction    0     1     2
        0     535   0     0
        1     1     307   0
        2     0     0     268

Overall Statistics

Accuracy : 0.9991               
95% CI : (0.995, 1)           
No Information Rate : 0.4824               
P-Value [Acc > NIR] : < 0.00000000000000022

Kappa : 0.9986               
Mcnemars Test P-Value : NA                   

Statistics by Class:

                     Class: 0 Class: 1 Class: 2
Sensitivity            0.9981   1.0000   1.0000
Specificity            1.0000   0.9988   1.0000
Pos Pred Value         1.0000   0.9968   1.0000
Neg Pred Value         0.9983   1.0000   1.0000
Prevalence             0.4824   0.2763   0.2412
Detection Rate         0.4815   0.2763   0.2412
Detection Prevalence   0.4815   0.2772   0.2412
Balanced Accuracy      0.9991   0.9994   1.0000

#model only has ONE ERROR on validation data!! :)

#rerun gbm model with wifi6 data set
#Load the necessary data
setwd("~/Ubiqum/Project 4/Wifi Task/UJIndoorLoc")
wifi6 <- readRDS("wifi6.rds")


#remove all features other than WAPs and Latitude
B6 <- select(wifi6, WAP006:WAP517, BUILDINGID)


#trying running model on entire dataset to predict building
set.seed(234)

# define an 75%/25% train/test split of the dataset
inTraining6 <- createDataPartition(B6$BUILDINGID, p = .75, list = FALSE)
training6 <- B6[inTraining6,]
testing6 <- B6[-inTraining6,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 2, repeats = 1)

#train classification model
B_gbm6 <- train(BUILDINGID~., data = training6, method = "gbm", trControl=fitControl, verbose = FALSE)


#make predictions
predB_gbm6 <- predict(B_gbm6, testing6)

#performace measurment
postResample(predB_gbm6, testing6$BUILDINGID)
# output
Accuracy    Kappa 
1           1 


#check confusion matrix
confusionMatrix(predB_gbm6,testing6$BUILDINGID)

#test new gbm model on validation data

ValData <- readRDS("ValClean.rds")

#deploy GBm model on validation data
gbmTest6 <- select(ValData, WAP006:WAP517)


#make predictions using gbm model
gbmPred6 <- predict(B_gbm6, gbmTest6)

#performace measurment
postResample(gbmPred6, ValData$BUILDINGID)
Accuracy    Kappa 
0.9990999   0.9985778 

confusionMatrix(gbmPred6, ValData$BUILDINGID)
Confusion Matrix and Statistics

            Reference
Prediction   0     1     2
        0    535   0     0
        1    1     307   0
        2    0     0     268

Overall Statistics

Accuracy : 0.9991    
95% CI : (0.995, 1)
No Information Rate : 0.4824    
P-Value [Acc > NIR] : < 2.2e-16 

Kappa : 0.9986    
Mcnemars Test P-Value : NA        

Statistics by Class:

Class: 0 Class: 1 Class: 2
Sensitivity            0.9981   1.0000   1.0000
Specificity            1.0000   0.9988   1.0000
Pos Pred Value         1.0000   0.9968   1.0000
Neg Pred Value         0.9983   1.0000   1.0000
Prevalence             0.4824   0.2763   0.2412
Detection Rate         0.4815   0.2763   0.2412
Detection Prevalence   0.4815   0.2772   0.2412
Balanced Accuracy      0.9991   0.9994   1.0000


#only one error in validation data!