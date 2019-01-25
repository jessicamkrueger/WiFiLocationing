#load libraries
library(dplyr)
library(ggplot2)
library(caret)
library(grid)
library(gridExtra)

#Load the necessary data
setwd("~/Ubiqum/Project 4/Wifi Task/UJIndoorLoc")
wifi6 <- readRDS("wifi6.rds")


#remove all features other than WAPs, Building, and Floor
FLRB0 <- wifi6 %>%
  filter(BUILDINGID == 0) %>%
  select(WAP006:WAP517, FLOOR)

FLRB0$FLOOR <- as.integer(FLRB0$FLOOR)
FLRB0$FLOOR <- as.factor(FLRB0$FLOOR)

FLRB1 <- wifi6 %>%
  filter(BUILDINGID == 1) %>%
  select(WAP006:WAP517, FLOOR)

FLRB1$FLOOR <- as.integer(FLRB1$FLOOR)
FLRB1$FLOOR <- as.factor(FLRB1$FLOOR)


FLRB2 <- wifi6 %>%
  filter(BUILDINGID == 2) %>%
  select(WAP006:WAP517, FLOOR)

#find WAPS in each building that are not registering signals
B1WAPs <- colMeans(FLRB1[1:321])
B1WAPs <- which(B1WAPs == -101)
FLRB1.a <- select(FLRB1, -B1WAPs, -"WAP049")
#now remove WAPs with max value less than -80
B1max <- apply(FLRB1.a, 2, function(x) max(x, na.rm = TRUE))
options(scipen=999)

#which max values are 80 orless (meaning very poor signal)

B180 <- which(B1max <=-80) #145 WAPs with a max value of -80 or less 

B180s <- names(B180)
