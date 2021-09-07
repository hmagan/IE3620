# Hank Magan
# 09.06.2021
# WK3L1Magan.R (esophogeal cancer data) data)

# clean up old stuff and set working directory
rm(list=ls())
setwd("C:/Users/hank/Documents/GitHub/IE3620/Week 3")

# import data set (included in datasets library)
library(datasets)
library(dplyr)

# explore dataset
summary(esoph)
head(esoph)
str(esoph)
dim(esoph)

# check for missing data
clean <- ifelse(complete.cases(esoph) == TRUE, 1, 0)
paste("There are ", dim(esoph)[1]-sum(clean), " rows with missing data.") # luckily, there is none

# rename columns to more descriptive names
names(esoph) <- c("AgeGroup", "Alcohol", "Tobacco", "Cases", "Controls")

# reclassify age, alocohol use, and tobacco use
esoph$AgeGroup <- factor(esoph$AgeGroup, 
                         levels=c("25-34", "35-44", "45-54", "55-64", "65-74", "75+"), 
                         labels=c("Millenials", "Xennials", "GenX", "Boomers", "Seniors", "Elderly"))
esoph$Alcohol <- factor(esoph$Alcohol, 
                        levels=c("0-39g/day", "40-79", "80-119", "120+"), 
                        labels=c("Minimal", "Moderate", "Significant", "Heavy"))
esoph$Tobacco <- factor(esoph$Tobacco, 
                        levels=c("0-9g/day", "10-19", "20-29", "30+"), 
                        labels=c("Minimal", "Moderate", "Significant", "Heavy"))

# create new datasets with specific data and variables
esoph_alc_age <- select(esoph, AgeGroup, Alcohol) # only show age & alcohol use
esoph_tob_heavy <- filter(esoph, Tobacco == "Heavy") # only show data of heavy tobacco users
esoph_alc_light_tob_mod <- filter(esoph, Alcohol == "Minimal", Tobacco == "Moderate") # only show data of light drinkers and moderate smokers

# add the "Difference" column
esoph <- mutate(esoph, Difference = abs(Controls - Cases))

# sort data by Difference in descending order, with AgeGroup as the secondary sort criterion
esoph <- arrange(esoph, -Difference, AgeGroup)

# summarize the mean Controls and Cases (rounded to two digits)
esoph_mean_controls_cases <- summarize(esoph, 
                                       AvgControls=mean(Controls) %>% round(digits=2), 
                                       AvgCases=mean(Cases) %>% round(digits=2))

# function to find data point furthest away from the mean
get_outlier <- function(column){
  mean <- mean(column)
  diff <- 0
  value <- NULL
  for(i in column){
    if(abs(i - mean) > diff){
      diff = abs(i - mean)
      value = i
    }
  }
  value
}

# find value furthest away from the mean in Controls
get_outlier(esoph$Controls)

# create new datasets grouped by certain variables
esoph_alc_group <- group_by(esoph, Alcohol)
esoph_mean_controls_cases_age_group <- group_by(esoph, AgeGroup) %>% summarize(AvgControls=mean(Controls) %>% round(digits=2), 
                                                                               AvgCases=mean(Cases) %>% round(digits=2)) 
# write the final dataset to a new CSV file
write.csv(esoph, "esophNew.csv", row.names=TRUE)
