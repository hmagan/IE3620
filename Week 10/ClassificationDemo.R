# Robert Gotwals
# November 4, 2020
# machinelearningdemo
# this code does diagnosisification using KNN
#
#set up and libraries
rm(list=ls())
options(warn=-1) # becareful warnings off
library(class)  # classification, knn
library(ggvis)   # another viz package
library(gmodels)  # model fitting package, CrossTable
library(tidyverse)
library(caret)
library(GGally)
library(gridExtra)
#

# Read in `data` data
data <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data"),  na.strings = "?", header = FALSE) 
# Print first lines
head(data)

# Add column names
#  Attribute                     Domain
# 1. Sample code number            id number
# 2. Clump Thickness               1 - 10
# 3. Uniformity of Cell Size       1 - 10
# 4. Uniformity of Cell Shape      1 - 10
# 5. Marginal Adhesion             1 - 10
# 6. Single Epithelial Cell Size   1 - 10
# 7. Bare Nuclei                   1 - 10
# 8. Bland Chromatin               1 - 10
# 9. Normal Nucleoli               1 - 10
# 10. Mitoses                       1 - 10
# 11. diagnosis:        (2 for benign, 4 for malignant) 
#
names(data) <- c("code", "thick", "size", "shape", "adhesion", "cell", "nuclei", "chromatin", "nucleoi", "mitoses","diagnosis")

# Check the result
head(data)
dim(data)
glimpse(data)
summary(data)
str(data)
#
data$nuclei <- ifelse(is.na(data$nuclei), mean(data$nuclei, na.rm = TRUE), data$nuclei)
summary(data)
#
#scatter plots
data %>% ggvis(x= ~thick, y=~size, fill = ~diagnosis) %>% layer_points()
data %>% ggvis(~nuclei, ~nucleoi, fill = ~diagnosis) %>% layer_points()
#
short1 <- data[, c(2:6,11)]
short2 <- data[, c(7:10,11)]
ggpairs(short1, aes(color = as.character(diagnosis)))
ggpairs(short2, aes(color = as.character(diagnosis)))
#
# Overall correlation
cor(as.numeric(data$nuclei), as.numeric(data$nucleoi))

# Division of `diagnosis`
table(data$diagnosis) 

# Percentual division of diagnosis
round(prop.table(table(data$diagnosis)) * 100, digits = 1)
# Refined summary overview
summary(data[c("thick", "size")])
#
set.seed(1234)
ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.7, 0.3))
ind
# Compose training set
data.training <- data[ind==1, 1:11]

# Inspect training set
head(data.training)

# Compose test set
data.test <- data[ind==2, 1:11]
dim(data.test)
dim(data.training)
dim(data)
#
# Compose `data` training labels
data.trainLabels <- data[ind==1,11]
dim(data.trainLabels)#
# Inspect result
print(data.trainLabels)
#
# Compose `data` test labels
data.testLabels <- data[ind==2, 11]
#
# Inspect result
print(data.testLabels)
#
data.training
data.test
data.trainLabels
# Build the model
data_pred <- knn(train = data.training, test = data.test, cl = data.trainLabels, k=3)

# Inspect `data_pred`
data_pred
# Put `data.testLabels` in a data frame
dataTestLabels <- data.frame(data.testLabels)
dataTestLabels
dim(dataTestLabels)
# Merge `data_pred` and `data.testLabels` 
merge <- data.frame(data.testLabels, data_pred)
dim(merge)

# Specify column names for `merge`
names <- colnames(data.test)
finaldata <- cbind(data.test, merge)
dim(finaldata)
names(finaldata) <- c(names, "Observed Diagnosis", "Predicted Diagnosis")

# Inspect `merge` 
head(finaldata)
write.csv(finaldata, "predictions.csv")
#
data.testLabels
#sink("crosstable.txt")
CrossTable(x = data.testLabels, y = data_pred, prop.chisq=FALSE)
#sink()
#####  END OF CODE
