#Wifi Locationing Task

library(dplyr)
library(ggplot2)
library(caret)
library(gridExtra)
library(grid)
library(tidyr)

#Load the necessary data
setwd("~/Ubiqum/Project 4/Wifi Task/UJIndoorLoc")
wifi <- read.csv("trainingData.csv")

#understand data and values
summary(wifi)
str(wifi)

summary(wifi[,520:529]) #just shows the final columns

#convert data types to correct type
str(wifi[,520:529])
wifi$FLOOR <- as.factor(wifi$FLOOR)
wifi$BUILDINGID <- as.factor(wifi$BUILDINGID)
wifi$SPACEID <- as.factor(wifi$SPACEID)
wifi$RELATIVEPOSITION <- as.factor(wifi$RELATIVEPOSITION)
wifi$USERID <- as.factor(wifi$USERID)
wifi$PHONEID <- as.factor(wifi$PHONEID)


#are the spaceIDs distinct for each building?
wifiB1 <- filter(wifi, BUILDINGID == 1)
wifiB2 <- filter(wifi, BUILDINGID == 2)
wifiB0 <- filter(wifi, BUILDINGID == 0)

summary(wifiB2$SPACEID)
summary(wifiB1$SPACEID)
summary(wifiB0$SPACEID)


#below I take a few subsets of the data to understand what each attribute represents
#can the same space id be used on multiple floors?
wifiB1F1 <- wifiB1 %>%
  filter(FLOOR == 1) 

#can same spaceID be used in multiple buildings?
space215 <- wifi %>%
  filter(SPACEID == 215) %>%
  select(LONGITUDE:TIMESTAMP)

summary(space215)
#spaceID 215 is used for all buildings and all floors so it's not a unique identifier

#is spaceID unique within each building?
B2S215 <- wifi %>%
  filter(SPACEID == 215 & BUILDINGID == 2) %>%
  select(LONGITUDE:TIMESTAMP)

summary(B2S215)
#No, the same spaceID can be used on multiple floors within a building. That seems odd

#can we create a new varible to show building.floor.space?
wifiTEST <- wifi %>%
  select(LONGITUDE:TIMESTAMP) %>%
  mutate(LOCID = paste(BUILDINGID, FLOOR, SPACEID, sep = "."))


wifiTEST$LOCID <- as.factor(wifiTEST$LOCID)
str(wifiTEST$LOCID)

#are certain WAPs only associated with certain buildings?
#below I'm taking a few random samples of diff. WAPs to see what buildings they cover
WAP001 <- wifi %>%
  filter(WAP001 != 100) %>%
  select(WAP001, LONGITUDE:TIMESTAMP)

summary(WAP001) #only building 0

WAP245 <- wifi %>%
  filter(WAP245 != 100) %>%
  select(WAP245, LONGITUDE:TIMESTAMP)

summary(WAP245) #no data recorded for this WAP


WAP156 <- wifi %>%
  filter(WAP156 != 100) %>%
  select(WAP156, LONGITUDE:TIMESTAMP)

summary(WAP156) #building 0 and 1

ggplot(WAP156, aes(x=BUILDINGID, y=WAP156)) +
  geom_boxplot()

ggplot(WAP156, aes(x=BUILDINGID, y=WAP156)) +
  geom_violin()

WAP078<- wifi %>%
  filter(WAP078 != 100) %>%
  select(WAP078, LONGITUDE:TIMESTAMP)

summary(WAP078) #building 1 and 2

#WAP seems to be a good predictor of what building you are in

#How many WAPs and which ones are not collecting data and can be removed
WAPtest <- select(wifi, WAP240:WAP245)

wifi$WAP240 < 100

summary(wifi$WAP429)

hist(wifi$WAP111)


#remove all columns that have only 100s (no signal)
means <- colMeans(wifi[1:520]) #finds mean of each column
mean100 <- which(means == 100) #there are 55 WAPs with mean 100

WAPs <- names(mean100)

wifi2 <- select(wifi, -WAPs) #removes all WAPs with only 100s

#now change 100 values to -105
wifi2[wifi2 == 100] <- -105

wifi2$SPACEID[wifi2$SPACEID == -105] <- 100

#find columns that have low max WAP value
maxValue <- apply(wifi2, 2, function(x) max(x, na.rm = TRUE))
options(scipen=999)

#which max values are 80 orless (meaning very poor signal)
maxValue
max80 <- which(maxValue <=-80) #145 WAPs with a max value of -80 or less 
max80
WAPs80 <- names(max80)
WAPs80 <- WAPs80[1:144] #removes longitude from the list

wifi3 <- select(wifi2, -WAPs80) #removes all WAPs with max dbm of 80 or less
wifi3$BUILDINGID <- as.factor(wifi3$BUILDINGID)
wifi3$FLOOR <- as.factor(wifi3$FLOOR)
wifi3$SPACEID <- as.factor(wifi3$SPACEID)
wifi3$RELATIVEPOSITION <- as.factor(wifi3$RELATIVEPOSITION)
wifi3$USERID <- as.factor(wifi3$USERID)
wifi3$PHONEID <- as.factor(wifi3$PHONEID)

saveRDS(wifi3, "wifi3.rds")

maxCheck <- apply(wifi3, 2, function(x) max(x, na.rm = TRUE))
summary(maxCheck[1:320]) #checks max on only WAPs


#Remove columns that have max WAP of -68 or less
max67 <- which(maxValue <=-68) #145 WAPs with a max value of -80 or less 
max67
WAPs67 <- names(max67)
WAPs67 <- WAPs67[1:204] #removes longitude from the list

wifi4 <- select(wifi2, -WAPs67) #removes all WAPs with max dbm of 80 or less
saveRDS(wifi4, "wifi4.rds")

maxCheck <- apply(wifi4, 2, function(x) max(x, na.rm = TRUE))
summary(maxCheck[1:261]) #checks max on only WAPs


#locate values > 30 dBM in dataframe
maxValue3 <- apply(wifi3, 2, function(x) max(x, na.rm = TRUE))
options(scipen=999)
maxValue3

max30 <- which(maxValue > -30)
max30
summary(max30)

which(wifi3$WAP011 > -30)
wifi3[2325,5] #value of -13 in this cell
wifi3[2327,5] #value of -11 in this cell
