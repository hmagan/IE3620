# Hank Magan
# 09.13.2021
# WK4L2Magan.R (melting point data)

# clean up old stuff and set working directory
rm(list=ls())
setwd("~/GitHub/IE3620/Week 4")

# import libraries and dataset
library(dplyr)
library(vtable)
mp <- read.csv("dirtyMPdata.csv", header=TRUE)

# check descriptive statistics and basic structure
str(mp)
summary(mp)
head(mp)
tail(mp)

# check for missing data
clean <- ifelse(complete.cases(mp) == TRUE, 1, 0)
paste("There are ", dim(mp)[1]-sum(clean), " rows with missing data.")

avg_energy <- mean(mp$energy, na.rm=TRUE)
mp$energy <- ifelse(is.na(mp$energy), avg_energy, mp$energy)

# rename just the first col to fix weird formatting
names(mp)[names(mp) == "ï..structure"] <- "structure"

mp_table <- mp %>% select(mp, molar.mass, heavy.atoms, logP, refractivity, dipole.moment)
names(mp_table) <- c("melting point/°C", "molecular weight/g/mol", "number of heavy atoms", 
                     "SlogP", "molar refractivity/cm^3", "dipole moment (AM1)/Debye")

sumtable(mp_table, 
         summ = c('min(x)', 'max(x)', 'mean(x)', 'sd(x)', 'pctile(x)[25]', 'pctile(x)[75]'), 
         summ.names = c('minimum', 'maximum', 'mean', 'standard deviation', 'Pctl. 25', 'Pctl. 75'))

# get rows with missing data
missing <- mp[rowSums(is.na(mp)) > 0, ]

