# Hank Magan
# 08.30.2021
# WK2L1Magan.R (Titantic data)

# clean up old stuff and set working directory
rm(list=ls())
setwd("C:/Users/hank/Documents/GitHub/IE3620/Week 2")

# import data set
titanic <- read.csv("titanic.csv")

# EDA of the data
class(titanic) # data.frame
names(titanic)
head(titanic)
summary(titanic)
str(titanic)
dim(titanic)

# remove the id column
titanic$PassengerId <- NULL

# replace values with more descriptive variants
titanic$Survived <- factor(titanic$Survived, levels=c(0, 1), labels=c("No", "Yes"))
titanic$Pclass <- factor(titanic$Pclass, levels=c(1, 2, 3), labels=c("Upper", "Middle", "Lower"))
titanic$Embarked <- factor(titanic$Embarked, levels=c("C", "Q", "S"), labels=c("Cherbourg", "Queenstown", "Southampton"))

# replace missing ages with the mean age
mean_age <- mean(titanic$Age, na.rm=TRUE)
titanic$Age <- ifelse(is.na(titanic$Age), mean_age, titanic$Age)

# rename Sibsp and Parch columns
names(titanic)[6] <- "Siblings" # Sibsp
names(titanic)[7] <- "ParentsChildren" # Parch

# visualize remaining numerical data
par(mfrow=c(2, 2)) # observe 4 graphs at once
hist(titanic$Age, main="Age on the Titanic", xlab="Age")
hist(titanic$Siblings, main="Siblings of those on the Titanic", xlab="Siblings")
hist(titanic$ParentsChildren, main="Parents/Children of those on the Titanic", xlab="Parents/Children")
plot(titanic$Age, titanic$Fare, main="Age vs. Fare on the Titanic")
fitline <- lm(titanic$Age~titanic$Fare) # linear reg. line
abline(fitline)