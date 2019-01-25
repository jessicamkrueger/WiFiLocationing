#map the range of WAP signals for each spaceID

#load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(grid)
library(gridExtra)

#Load the necessary data
setwd("~/Ubiqum/Project 4/Wifi Task/UJIndoorLoc")
wifi6 <- readRDS("wifi6.rds")

summary(wifi6$SPACEID)
str(wifi6$SPACEID)

sp106 <- filter(wifi6, SPACEID == 106 & RELATIVEPOSITION == 1)
#88 records taken inside space 106

means106 <- colMeans(sp106[1:321])
means106a <- which(means106 != -101)
names106 <- names(means106a)

sp106.a <- select(sp106, names106, LONGITUDE:TIMESTAMP)
#there are different bldgs, floors in this sample

sp106.a <- mutate(sp106.a, BLDG_FLOOR = paste(paste("B", BUILDINGID, "F", FLOOR, sep = "."))) 
sp106.t <- gather(sp106.a, WAP, Value, WAP008:WAP517)

summary(sp106.a$BLDG_FLOOR)
summary(sp106.a$FLOOR)

#create B1F1 sample of space 106
B1F1.106 <- filter(sp106.a, BUILDINGID == 1 & FLOOR == 1)
#49 observations

boxplot(B1F1.106[1:68])
summary(B1F1.106$WAP008)
#want to remove outliers, but need to determine how to make that a fast
#process...cannot do it manually for all spaceIDs. Or maybe just review
#errors in bldg 1 and pritorize space ids by that?

ggplot(sp106.t, aes(WAP, Value)) +
  geom_boxplot() +
  facet_wrap(~BLDG_FLOOR)
#could create this chart by unique spaceID created for entire data set, but 
#that seems like it would be quite a lot of data to plot. 
#There are 933 unique reference points. Maybe I should just focus on B1 though?

