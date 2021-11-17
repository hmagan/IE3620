# Hank Magan
# 11.13.2021
# FinalMagan.R (final paper code)

# I recognize this is in no way complete, I had a rather busy week or two (sorry)
# The final paper will definitely include much more detail/techniques

# clean up old stuff and set working directory
rm(list=ls())
setwd("~/GitHub/IE3620/Final (12-15)")

# import libraries
library(dplyr)
library(tibble)
library(stringr)
library(ggplot2)

# import data
housingRaw <- read.csv("housing.csv")

# quick look at data
str(housingRaw)
glimpse(housingRaw)

# data is all one column and unlabeled: clean data
names(housingRaw) <- c("jargon") # rename singular column to something writable to then replace it...
housingRaw$jargon <- str_squish(housingRaw$jargon) # remove extra spaces in data
# i.e. "Hello    there      how are         you"
# becomes "Hello there how are you" (neat function!)

# set split column names (14 in total)
cols <- c("CRIM",    # per capita crime rate by town
          "ZN",      # proportion of residential land zoned for lots over 25,000 sq.ft
          "INDUS",   # proportion of non-retail business acres per town
          "CHAS",    # Charles River dummy variable (1 if tract bounds river; 0 otherwise)
          "NOX",     # nitric oxides concentration (parts per 10 million) [parts/10M]
          "RM",      # average number of rooms per dwelling
          "AGE",     # proportion of owner-occupied units built prior to 1940
          "DIS",     # weighted distances to five Boston employment centres
          "RAD",     # index of accessibility to radial highways
          "TAX",     # full-value property-tax rate per $10,000 [$/10k]
          "PTRATIO", # pupil-teacher ratio by town
          "B",       # B=1000(Bk - 0.63)^2 where Bk is the proportion of black people by town
          "LSTAT",   # % lower status of the population
          "MEDV"    # median value of owner-occupied homes in $1000's [k$]
          )

# separate raw data into new data set
housing <- separate(data=housingRaw, col=jargon, sep=" ", into=cols, remove=TRUE)

# convert chr columns into nums + logic col
housing[, 1:14] <- sapply(housing[, 1:14], as.numeric) # numeric conversion
housing$CHAS <- factor(housing$CHAS, levels=c(0, 1), labels=c(FALSE, TRUE)) # bool conversion

# check for NAs
clean <- ifelse(complete.cases(housing) == TRUE, 1, 0)
paste("There are ", dim(housing)[1]-sum(clean), " rows with missing data")

# take a look at new clean data
dim(housing)
glimpse(housing)
summary(housing)

# data viz
ggplot(data=housing, mapping=aes(x=RM, y=MEDV)) + geom_point() + geom_smooth(method='lm', formula= y~x)
ggplot(data=housing, mapping=aes(x=DIS, y=MEDV)) + geom_point() + geom_smooth(method='lm', formula= y~x)

# perform some things we learned, eg KNN, PCA, k-means clustering, etc
# add more visualizations
