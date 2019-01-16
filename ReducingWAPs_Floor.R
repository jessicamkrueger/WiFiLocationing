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
