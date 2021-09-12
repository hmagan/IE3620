# Hank Magan
# 09.12.2021
# WK4L1Magan.R (analyzing mouse data)

# clean up old stuff and set working directory
rm(list=ls())
setwd("~/GitHub/IE3620/Week 4")

# import libraries and dataset
library(dplyr)
mouse <- read.csv("mouseData.csv", header=TRUE)

# check for missing data
clean <- ifelse(complete.cases(mouse) == TRUE, 1, 0)
paste("There are ", dim(mouse)[1]-sum(clean), " rows with missing data.")

# since there is missing data, find out which columns contain missing data
missing_col <- colnames(mouse)[apply(mouse, 2, anyNA)]
paste("The following columns are missing data: ")
paste(missing_col)

# clean the missing data by replacing NAs with averages
avg_bp <- mean(mouse$Mean.BP, na.rm=TRUE)
mouse$Mean.BP <- ifelse(is.na(mouse$Mean.BP), avg_bp, mouse$Mean.BP)
avg_hr <- mean(mouse$Mean.HR, na.rm=TRUE)
mouse$Mean.HR <- ifelse(is.na(mouse$Mean.HR), avg_hr, mouse$Mean.HR)
avg_wt <- mean(mouse$Heart.Wt, na.rm=TRUE)
mouse$Heart.Wt <- ifelse(is.na(mouse$Heart.Wt), avg_wt, mouse$Heart.Wt)

# check descriptive statistics and basic structure
str(mouse)
summary(mouse)
head(mouse)
tail(mouse)

# give variables more descriptive names
names(mouse)[3:5] <- c("BloodPressure", "HeartRate", "HeartWeight")

# replace 1's and 0's with male and female for readability
mouse$sex <- factor(mouse$sex, levels=c(0, 1), labels=c("Female", "Male"))

# observe 3 plots as 1 graphic; 1 row, 3 col
par(mfrow=c(1, 3))

# view descriptive stats as boxplots
boxplot(mouse$BloodPressure, ylab="Systolic Blood Pressure", main="Box Plot of Mouse Blood Pressure")
boxplot(mouse$HeartRate, ylab="Heart Rate (BPM)", main="Box Plot of Mouse Heart Rate")
boxplot(mouse$HeartWeight, ylab="Hear Weight (kg)", main="Box Plot of Mouse Heart Weight")

# view numeric data as histograms
hist(mouse$BloodPressure, xlab="Systolic Blood Pressure", main="Histogram of Mouse Blood Pressure")
hist(mouse$HeartRate, xlab="Heart Rate (BPM)", main="Histogram of Mouse Heart Rate")
hist(mouse$HeartWeight, xlab="Hear Weight (kg)", main="Histogram of Mouse Heart Weight")

# change number of plots displayed; 1x2
par(mfrow=c(1, 2))

# separate data into male and female to observe heart weight differences
fem_mice <- mouse %>% filter(sex=="Female")
male_mice <- mouse %>% filter(sex=="Male")

# plot female HeartWeight~HeartRate
plot(fem_mice$HeartWeight~fem_mice$HeartRate, 
     xlim=c(550, 750), ylim=c(0.07, 0.15), 
     main="Female Heart Rate vs. Heart Weight", 
     xlab="Heart Rate (BPM)", ylab="Heart Weight (kg)")
fem_line <- lm(fem_mice$HeartWeight~fem_mice$HeartRate)
abline(fem_line)

# plot male HeartWeight~HeartRate
plot(male_mice$HeartWeight~male_mice$HeartRate, 
     xlim=c(550, 750), ylim=c(0.07, 0.15), 
     main="Male  Heart Rate vs. Heart Weight", 
     xlab="Heart Rate (BPM)", ylab="Heart Weight (kg)")
male_line <- lm(male_mice$HeartWeight~male_mice$HeartRate)
abline(male_line)

# males seem to have higher heart weight
# reset number of plots displayed to 1
par(mfrow=c(1, 1))

# find mean heart weight for males and females, respectively
fem <- fem_mice %>% summarize(HeartWeight = mean(HeartWeight)) %>% as.numeric
masc <- male_mice %>% summarize(HeartWeight = mean(HeartWeight)) %>% as.numeric

# create a simple barplot comparing male/female mean heart weights 
barplot(c(fem, masc), 
        xlab="Sex", ylab="Heart Weight (kg)", 
        ylim=c(0.0, 0.12), 
        names.arg=c("Female", "Male"), 
        main="Female vs. Male Mice Average Heart Weight")

# sex and heart weight seems to be correlated; run BIC test to test for causality
BIC(lm(mouse$HeartWeight~1)) # -1108.842
BIC(lm(mouse$HeartWeight~mouse$sex)) # -1193.981
# significant drop (-85.139); sex causes heart weight (to a degree)

# plot blood pressure vs. heart rate of whole dataset
plot(mouse$HeartRate~mouse$BloodPressure, 
     main="Mouse Heart Rate vs. Blood Pressure", 
     xlab="Systolic Blood Pressure", ylab="Heart Rate (BPM)")
fitline <- lm(mouse$HeartRate~mouse$BloodPressure)
abline(fitline)

# some correlation; check for causality (i.e. does BP cause heart rate?)
BIC(lm(mouse$HeartRate~1)) # 2217.143
BIC(lm(mouse$HeartRate~mouse$BloodPressure)) # 2204.246
# significant drop (-12.897); safe to say higher BP causes higher heart rate, 
# as observed on the plot

# change number of plots displayed; 1x2
par(mfrow=c(1, 2))

# plot heart weight vs BP; light negative correlation observed
plot(mouse$BloodPressure~mouse$HeartWeight, 
     main="Mouse Heart Weight vs. Blood Pressure", 
     xlab="Heart Weight (kg)", ylab="Systolic Blood Pressure")
fitline <- lm(mouse$BloodPressure~mouse$HeartWeight)
abline(fitline)

# plot heart rate vs BP (again); positive correlation observed as expected
plot(mouse$BloodPressure~mouse$HeartRate, 
     main="Mouse Heart Rate vs. Blood Pressure", 
     xlab="Heart Rate (BPM)", ylab="Systolic Blood Pressure")
fitline <- lm(mouse$BloodPressure~mouse$HeartRate)
abline(fitline)

# check for causality between the variables observed above using a multiple regression
BIC(lm(mouse$BloodPressure~1)) # 1603.311
BIC(lm(mouse$BloodPressure~mouse$HeartWeight)) # 1602.788; little causality
BIC(lm(mouse$BloodPressure~mouse$HeartRate)) # 1590.414; significant causality
BIC(lm(mouse$BloodPressure~mouse$HeartWeight+mouse$HeartRate)) # 1589.17; significant causality, slightly better
# results above indicate heart weight and heart rate work together a little to cause
# blood pressure, however only marginally; results are primarily driven by heart rate, 
# as observed on the plots

# reset number of plots back to 1
par(mfrow=c(1, 1))
