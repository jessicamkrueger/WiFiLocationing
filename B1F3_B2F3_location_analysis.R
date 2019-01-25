#review WAP values in B1F3 (lots of errors) and B2F3

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

wifi6$SPACEID <- as.character(wifi6$SPACEID)
wifi6$SPACEID[wifi6$SPACEID == "-101"] <- 100
saveRDS(wifi6, "wifi6.rds")

B1F3 <- wifi6 %>%
  filter(BUILDINGID == 1 & FLOOR == 3) %>%
  mutate(LOC = paste(SPACEID, RELATIVEPOSITION, sep = "."))

B1F3means <- colMeans(B1F3[1:321])
summary(B1F3means)
remove <- which(B1F3means == -101)
B1F3means[204]

WAPremove <- names(remove)

B1F3a <- select(B1F3, -WAPremove)

B1F3T <- gather(B1F3a, WAP, RSSI, WAP011:WAP503)

str(B1F3T$LOC)
B1F3T$LOC <- as.factor(B1F3T$LOC)

ggplot(B1F3T, aes(WAP, RSSI)) +
  geom_boxplot() +
  facet_wrap(~LOC)
  
  
B2F3 <- wifi6 %>%
  filter(BUILDINGID == 2 & FLOOR == 3) %>%
  mutate(LOC = paste(SPACEID, RELATIVEPOSITION, sep = "."))


