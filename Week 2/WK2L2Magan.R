# Hank Magan
# 08.30.2021
# WK2L2Magan.R (Chicago weather data)

# clean up old stuff and set working directory
rm(list=ls())
setwd("C:/Users/hank/Documents/GitHub/IE3620/Week 2")
library(lubridate)

# import data set
chicago <- read.csv("chicago.csv")

# EDA of the data
class(chicago) # data.frame
names(chicago)
head(chicago)
summary(chicago)
str(chicago)
dim(chicago)

chicago$indx <- NULL
chicago$city <- NULL

names(chicago) <- c("Temp", "Dewpoint", "Date", "PM25", "PM10", "O3", "NO3")
chicago$Date <- mdy(chicago$Date)

PM25_mean <- mean(chicago$PM25, na.rm=TRUE)
PM10_mean <- mean(chicago$PM10, na.rm=TRUE)
chicago$PM25 <- ifelse(is.na(chicago$PM25), PM25_mean, chicago$PM25)
chicago$PM10 <- ifelse(is.na(chicago$PM10), PM10_mean, chicago$PM10)

chicago$PM25 <- chicago$PM25 / max(chicago$PM25)
chicago$PM10 <- chicago$PM10 / max(chicago$PM10)
