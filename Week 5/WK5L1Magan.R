# Hank Magan
# 09.19.2021
# WK5L1Magan.R (cleaning cervical cancer data)

# clean up old stuff and set working directory
rm(list=ls())
setwd("~/GitHub/IE3620/Week 5")

# import libraries and dataset
library(dplyr)
source("../myfunctions.R")
cancer <- read.csv("cervicalCA.csv", header=TRUE, na.strings=c("?")) # make '?' show as NA

# rename columns using lowerCamelCase
names(cancer) <- c("age", "numPartners", "firstIntercourse", "numPregnancies", 
                   "isSmoker", "yearsSmoking", "packsPerYear", "onContra", 
                   "yearsOnContra", "onIUD", "yearsOnIUD", "hasSTDs", "numSTDs", 
                   "hasCondyloma", "hasCervCondyloma", "hasVagCondyloma", 
                   "hasVulvoPerinealCondy", "hasSyphilis", "hasPID", "hasGenHerpes", 
                   "hasMolluscum", "hasAIDS", "hasHIV", "hasHepB", "hasHPV", 
                   "numDiagnosis", "sinceFirstDiag", "sinceLastDiag", "hasCancerDiag", 
                   "hasCINDiag", "hasHPVDiag", "hasDiag", "hadHinselmann", "hadSchiller", 
                   "hadCytology", "hadBiopsy")

# check for missing data
clean <- ifelse(complete.cases(cancer) == TRUE, 1, 0)
paste("There are ", dim(cancer)[1]-sum(clean), " rows with missing data.")

# there is missing data; replace all NAs with zeroes
# working with medical data; we should not say a person has a disease
# when they don't
cancer[is.na(cancer)] <- 0

# change rows representing booleans using integers to actual booleans, or
# "logicals" as they are called in R
cancer[, c(5, 8, 10, 12, 14:25, 29:36)] <- lapply(cancer[, c(5, 8, 10, 12, 14:25, 29:36)], as.logical)

# write clean data to a new CSV
write.csv(cancer, "ccdataMod.csv", row.names=TRUE)
