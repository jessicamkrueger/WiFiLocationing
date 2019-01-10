#outlier exploration in wifi task

library(dplyr)
library(ggplot2)
library(caret)
library(gridExtra)
library(grid)
library(tidyr)

#Load the necessary data
setwd("~/Ubiqum/Project 4/Wifi Task/UJIndoorLoc")
wifi3 <- readRDS("wifi3.rds")

#find all WAP value that are greater than -30 and sort them by user, bldg, floor, 
#phone, timestamp

wifi3a <- select(wifi3, WAP006:WAP517)
sum(wifi3a > -30) #775 values in dataset greater than -30


wifi30sb <- filter(wifi3, WAP517 > -30)

wifi30s <-  filter_at(wifi3, vars(WAP006:WAP517), any_vars(.>-30))
wifi30sa <- select(wifi30s, WAP006:WAP517)
sum(wifi30sa > -30) #775 values returned, 482 rows

#plot the number of observations by different features (user, bldg, etc)

#Pattern by USERID?
ggplot(wifi30s, aes(USERID)) +
  geom_bar() +
  labs(title = "Distribution of Users Registering WAP Values Over -30") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), axis.line = element_line(colour = "gray")) 
#user 6 has majority of observations that are greater than -30

#Pattern by USERID?
ggplot(wifi30s, aes(USERID)) +
  geom_bar() +
  labs(title = "Distribution of Users Registering WAP Values Over -30") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "gray")) 
#user 6 has majority of observations that are greater than -30
#user6 is phone id 19 which is a Nexus 4, using android version 4.2.2
ggplot(wifi30s, aes(PHONEID)) +
  geom_bar() +
  labs(title = "Distribution of Phones Registering WAP Values Over -30") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "gray")) 
#shows same info as USERID chart -- users 6 and 14 (phones 19 and 7) have most >-30 values

#any pattern with building or floor?
ggplot(wifi30s, aes(BUILDINGID)) +
  geom_bar() +
  labs(title = "Distribution of Buildings Registering WAP Values Over -30") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "gray")) 
#mostly building 2

ggplot(wifi30s, aes(FLOOR)) +
  geom_bar() +
  labs(title = "Distribution of Floors Registering WAP Values Over -30") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "gray")) 
#mostly floors 3 and 4

#combine these charts to show more detail
ggplot(wifi30s, aes(BUILDINGID, FLOOR)) +
  geom_count() +
  labs(title = "Distribution of Building & Floor Registering WAP Values Over -30") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "gray")) 
#most >-30 values cominging from Building 2, floor 3 and 4

#combine these charts to show more detail
ggplot(wifi30s, aes(USERID)) +
  geom_bar(aes(fill=BUILDINGID)) +
  labs(title = "Distribution of User ID & Building Registering WAP Values Over -30") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line(colour = "light gray"),
        axis.text.x = element_text(colour = "light gray"),
        axis.text.y = element_text(colour = "light gray")) 
#most >-30 values cominging from Building 2, floor 3 and 4

#any timestamp patterns?
wifi30s$TIMESTAMP <- as.POSIXct(wifi30s$TIMESTAMP, origin="1970-01-01")

ggplot(wifi30s, aes(TIMESTAMP)) +
  geom_histogram(binwidth = ) +
  labs(title = "Distribution of Timestamps Registering WAP Values Over -30") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line(colour = "light gray"),
        axis.text.x = element_text(colour = "light gray"),
        axis.text.y = element_text(colour = "light gray")) 

#mostly on June 20

ggplot(wifi30s, aes(TIMESTAMP)) +
  geom_histogram(aes(fill = BUILDINGID)) +
  labs(title = "Distribution of Timestamps Registering WAP Values Over -30") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line(colour = "light gray"),
        axis.text.x = element_text(colour = "light gray"),
        axis.text.y = element_text(colour = "light gray")) 

#user 6 and user 14 are major concerns...we can remove these observations, all of their data,
#or all data from June 20 in building 2?

#how many values are there from building 2 on June 20?
wifi3$TIMESTAMP <- as.POSIXct(wifi3$TIMESTAMP, origin="1970-01-01")
June20 <- filter(wifi3, BUILDINGID == 2 & TIMESTAMP >= "2013-06-20")
summary(June20$TIMESTAMP)
summary(June20$BUILDINGID)
#June 20, BLDG 2 is ~9500 observations, ~50% data, so shouldn't just remove all June 20 data

#filter wifi by user 6 and compare the days, buildings, floors
user6 <- filter(wifi3, USERID == 6) #980 observations, all in Bldg 2
user14 <- filter(wifi3, USERID == 14) #1596 observations, 2/3 in Bldg 2, 1/3 in Bldg 1
#user 6 and user 14 make up 13% of data set
user1 <- filter(wifi3, USERID == 1) #2737
user3 <- filter(wifi3, USERID == 3) #192
user10 <- filter(wifi3, USERID == 10) #913
user15 <- filter(wifi3, USERID == 15) #498
user16 <- filter(wifi3, USERID == 16) #1032


#what buidlings do user 6 and 14 register in? (mostly 2, but some in 1)
ggplot(user6, aes(BUILDINGID)) +
  geom_bar()
ggplot(user14, aes(BUILDINGID)) +
  geom_bar()

#what percentage of user 6 and user 14's data is false?
user6BAD <- filter_at(user6, vars(WAP006:WAP517), any_vars(.>-30)) #430 bad observations
#430/980 = 44%

user14BAD <- filter_at(user14, vars(WAP006:WAP517), any_vars(.>-30)) #51 bad observations
#51/1596 = 3%
#so maybe remove ALL of user 6 data and just remove bad data from user 14 and all other users?

#look at user 6 data by day
ggplot(user6, aes(TIMESTAMP)) +
  geom_histogram() #all on June 20 it appears between (9:45-10:15 and 11:15-11:45)

ggplot(user6BAD, aes(TIMESTAMP)) +
  geom_histogram() #bad data also spread between same time periods as above

summary(user6$TIMESTAMP) #all user 6 data between 9:39 and 11:40am on June 20

#how many rows if we remove ALL of user 6 and all other bad data?
badData <- filter(wifi30s, USERID != 6)

#compare all users amount of bad data to their total amount of data
wifi30s$row <- c(1:492)

BadTotals <- wifi30s %>%
  group_by(USERID) %>%
  count(row) %>%
  summarize(total = sum(n))

BadTotals$dataTotal <- c(2737, 192, 980, 913, 1596, 498, 1032)

BadTotals <- mutate(BadTotals, Percent = total/dataTotal)
ggplot(BadTotals, aes(USERID, Percent)) +
  geom_col() +
  labs(title = "Percent of Observations with Values Over -30") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line(colour = "light gray"),
        axis.text.x = element_text(colour = "light gray"),
        axis.text.y = element_text(colour = "light gray")) 
#user 6 is by far the largest percent of bad data, close to 50%
#all other users have less than 4% of bad data


#final decision, remove all of user 6 and rest of the "bad" data
no6 <- filter(wifi3, USERID != 6)
wifi5 <- filter(no6, WAP061 <= -30 & WAP062 <= -30 & WAP065 <= -30 & 
                  WAP066 <= -30 & WAP105 <= -30 & WAP084 <= -30 & WAP085 <= -30 &
                  WAP080 <= -30 & WAP081 <= -30 & WAP121 <= -30 & WAP122 <= -30 &
                  WAP087 <= -30 & WAP262 <= -30 & WAP501 <= -30 & WAP495 <= -30 &
                  WAP516 <= -30 & WAP180 <= -30)

saveRDS(wifi5, "wifi5.rds") #outliers removed

wifi6 <- filter_at(wifi5, vars(WAP006:WAP517), any_vars(.==-22))
which(wifi6[1:321] == -22)
colnames(wifi6[156])


#find columns that have values > -30
maxValue6 <- apply(wifi6, 2, function(x) max(x, na.rm = TRUE))
options(scipen=999)

#which max values are 80 orless (meaning very poor signal)
maxValue6 <- as.integer(maxValue6)
max5 <- which(maxValue6 > -30) #145 WAPs with a max value of -80 or less 
max5
WAPs5 <- names(max5)
WAPs5
str(wifi5)
summary(wifi5)

str(wifi5)

ggplot(wifi5, aes(USERID)) +
  geom_bar()

wifi5Tidy <- gather(wifi5, WAP, Value, WAP006:WAP517)
ggplot(wifi5Tidy, aes(Value)) +
  geom_bar() +
  scale_x_continuous(limits = c(-30,0))
