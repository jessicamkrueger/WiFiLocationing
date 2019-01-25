#review B1F2 and B2F4 for patterns in data/errors


#load libraries
library(dplyr)
library(ggplot2)
library(caret)
library(grid)
library(gridExtra)

#Load the necessary data
setwd("~/Ubiqum/Project 4/Wifi Task/UJIndoorLoc")
readRDS("ValB0.rds")
readRDS("ValB1.rds")
readRDS("ValB2.rds")

#create data set for each floor

ValB1F2 <- filter(ValB1, FLOOR == 2)
ValB2F4 <- filter(ValB2, FLOOR == 4)


#plot phone ID
ggplot(ValB1F2, aes(PHONEID)) +
  geom_bar(aes(fill = Error))

ggplot(ValB2F4, aes(PHONEID)) +
  geom_bar(aes(fill = Error))

ggplot(ValB2, aes(PHONEID)) +
  geom_bar(aes(fill = Error))

ggplot(ValB1, aes(PHONEID)) +
  geom_bar(aes(fill = Error))

ggplot(ValB0, aes(PHONEID)) +
  geom_bar(aes(fill = Error))

#phone IDs 13 and 20 are used most in the validation data
ggplot(Val, aes(PHONEID)) +
  geom_bar()

