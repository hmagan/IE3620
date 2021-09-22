# Hank Magan
# 09.07.2021 
# WK3L2Magan.R (heart data analyses)

# clean up old stuff and set working directory
rm(list=ls())
setwd("C:/Users/hank/Documents/GitHub/IE3620/Week 3")

# import data set and import dplyr
heart <- read.csv("dirtyheart.csv")
library(dplyr)

test <- heart

# replace NA values with averages
for(i in 1:ncol(test)){
  avg <- sapply(test[i], mean, na.rm=TRUE)
  col_name <- names(test)[i]
  # if it's a boolean (0 or 1), replace with whatever occurs more in the data
  # (i.e. if the mean is >=0.5, put a 1; else put a 0)
  if(col_name == "sex" | col_name == "fbs" | col_name == "restecg" | col_name == "exang" | col_name == "target"){
    avg <- ifelse(avg >= 0.5, 1, 0)
  }
  # replace and round off to integers
  test[i] <- test[i] %>% replace(is.na(.), avg) %>% round()
}
