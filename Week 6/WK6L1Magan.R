# Hank Magan
# 09.26.2021
# WK6L1Magan.R (opiate data)

# clean up old stuff and set working directory
rm(list=ls())
setwd("~/GitHub/IE3620/Week 6")

# import libraries and dataset
library(ggplot2)
library(gridExtra)
opiates <- read.csv("opiates.csv", header=TRUE)

# check descriptive statistics and basic structure
str(opiates)
summary(opiates)
head(opiates)
tail(opiates)

# check for missing data
clean <- ifelse(complete.cases(opiates) == TRUE, 1, 0)
missing_col <- colnames(opiates)[apply(opiates, 2, anyNA)]
paste("There are ", dim(opiates)[1]-sum(clean), " rows with missing data accross the following columns:")
paste(missing_col)

# replace NAs with zeroes
opiates[is.na(opiates)] <- 0

# convert population data to population in millions
# i.e.  4430141 <- 4.430141
opiates$population <- opiates$population / 1000000

# boxplot of deaths vs. population of each division
boxplot <- ggplot(opiates, aes(x=population, y=deaths, group=factor(division_name))) + 
           geom_boxplot() + labs(title="Boxplot of deaths vs. population") + 
           xlab("Population (millions)") + ylab("Deaths") + theme(plot.title=
           element_text(hjust=0.5))

# scatterplot of population by deaths, colored by region
scatterplot <- ggplot(opiates, aes(x=population, y=deaths, color=region)) + 
               geom_jitter() + labs(title="Scatterplot of population by deaths and region") + 
               xlab("Population (millions)") + ylab("Deaths") + theme(plot.title=element_text(hjust=0.5))

# violin plot of deaths vs. population, grouped by state abbreviation
violin <- ggplot(opiates, aes(x=population, y=deaths, group=abbr)) + geom_violin() + 
          labs(title="Violin Plot of deaths vs. population") + xlab("Population (millions)") + 
          ylab("Deaths") + theme(plot.title=element_text(hjust=0.5)) + geom_boxplot()

# stripchart of deaths vs population (both adjusted), colored by FIPS and shaped by region
stripchart <- ggplot(opiates, aes(x=adjusted, y=adjusted_se, shape=region, color=fips)) + 
              geom_jitter() + labs(title="Stripchart of deaths vs. population") + 
              xlab("Population (millions)") + ylab("Deaths") + theme(plot.title=element_text(hjust=0.5))

# arrange all four plots onto one document (2x2)
grid.arrange(boxplot, scatterplot, violin, stripchart, ncol=2, nrow=2)
