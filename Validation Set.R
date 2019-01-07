#load libraries
library(dplyr)
library(ggplot2)
library(caret)

#Load the necessary data
setwd("~/Ubiqum/Project 4/Wifi Task/UJIndoorLoc")
valdata <- read.csv("validationData.csv")

summary(valdata[,520:529]) #just shows the final columns
