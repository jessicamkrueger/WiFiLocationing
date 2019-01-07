#Wifi Locationing Task

#load libraries
library(dplyr)
library(ggplot2)

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

summary(wifi$WAP001)
