# Hank Magan
# 08.19.2021
# WK1L1Magan.R (Cat Anatomy Script)
 
# clean up old stuff and set working directory
rm(list=ls())
setwd("C:/Users/hank/Documents/GitHub/IE3620/Week 1")

# use the MASS library and myfunctions.R code
library(MASS)
source("../myfunctions.R")

# Exploratory Data Analysis (EDA)
head(cats) # shows first 6 rows
dim(cats) # dimensions of data set
names(cats) # column names
summary(cats) # high level overview with descriptive statistics
str(cats) # structure of data set

# look at stats
hist(cats$Bwt) # creates a histogram; access columns using $
hist(cats$Hwt)

# transform data for a more normally distributed histogram
hist(log10(cats$Bwt))
hist(log10(cats$Hwt))
hist(rz.transform(cats$Bwt), main="Rank Z Transformation of Cat Body Weight")
hist(rz.transform(cats$Hwt), main="Rank Z Transformation of Cat Heart Weight")

# data cleanup
colnames(cats) <- c("Gender", "BodyWeight", "HeartWeight")
names(cats)
cats$Gender <- factor(cats$Gender, levels=c("F", "M"), labels=c("Female", "Males")) # not entirely necessary here
head(cats)
pairs(cats, upper.panel=panel.cor, diag.panel=panel.hist)

# simple plot
plot(cats$BodyWeight, cats$HeartWeight, main="Cat Heart Weight vs. Body Weight")

# create linear regression
fitline <- lm(cats$HeartWeight~cats$BodyWeight)
abline(fitline)