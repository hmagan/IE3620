# Hank Magan
# 11.13.2021
# FinalMagan.R (final paper code)

# clean up old stuff and set working directory
rm(list=ls())
setwd("~/GitHub/IE3620/Final (12-15)")

# import libraries
library(dplyr)
library(tibble)
library(stringr)
library(ggplot2)
library(cluster)
library(factoextra)
library(class)
library(gmodels)
library(reshape2)
library(gridExtra)

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
          "DIS",     # weighted distances to five Boston employment centers
          "RAD",     # index of accessibility to radial highways
          "TAX",     # full-value property-tax rate per $10,000 [$/10k]
          "PTRATIO", # pupil-teacher ratio by town
          "B",       # B=1000(Bk - 0.63)^2 where Bk is the proportion of black people by town
          "LSTAT",   # % lower status of the population
          "MEDV"     # median value of owner-occupied homes in $1000's [k$]
          )

# separate raw data into new data set
housing <- tidyr::separate(data=housingRaw, col=jargon, sep=" ", into=cols, remove=TRUE)
dim(housing)

# convert chr columns into nums + logic col
housing[, 1:14] <- sapply(housing[, 1:14], as.numeric) # numeric conversion
housing$CHAS <- factor(housing$CHAS, levels=c(0, 1), labels=c(FALSE, TRUE)) # bool conversion

# check for NAs
clean <- ifelse(complete.cases(housing) == TRUE, 1, 0)
paste("There are ", dim(housing)[1]-sum(clean), " rows with missing data")

summary(housing$MEDV)
# Min.  1st Qu.  Median    Mean  3rd Qu.    Max. 
# 5.00   17.00   21.20   22.53   25.00   50.00 

# remove rows (16 total) where MEDV == 50.0 (original data censored at 50.0)
housing <- housing[!(housing$MEDV == 50.0),]

summary(housing$MEDV)
# Min. 1st Qu.  Median    Mean  3rd Qu.    Max. 
# 5.00   16.70   20.90   21.63   24.70   48.80 

# take a look at new clean data
dim(housing)
glimpse(housing)
summary(housing)

# remove CHAS since unused and unhelpful in predicting MEDV
housing <- select(housing, -CHAS)

# calculate correlation matrix + plot the resulting heatmap
correlations <- cor(select(housing, CRIM, ZN, INDUS, NOX, RM, AGE, DIS, RAD, TAX, PTRATIO, B, LSTAT, MEDV))
melted <- melt(correlations)
melted$value <- round(melted$value, digits=2) # fits nums into boxes on heatmap

# actual plot; mainly styling with ggplot
heatmap <- ggplot(melted, aes(x=Var1, y=Var2, fill=value)) + geom_tile() + 
           scale_fill_gradient2(low="blue", high = "red", mid="white", midpoint = 0, 
           limit = c(-1, 1), space = "Lab", name="Correlation") + theme_minimal() + 
           theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 16, hjust = 1), 
                 axis.text.y = element_text(angle = 0, vjust = 1, size = 16, hjust = 1)) +
           coord_fixed()
heatmap + geom_text(aes(Var2, Var1, label = value), color="black", size=7.5) +
          theme(
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            panel.grid.major=element_blank(),
            panel.border=element_blank(),
            panel.background=element_blank(),
            axis.ticks=element_blank(), 
            legend.title=element_text(size=16), 
            legend.text=element_text(size=15)
          )

# grab most influential factors towards median price
top_factors <- melted[melted$Var1 == "MEDV" & abs(melted$value) >= 0.5 & melted$value != 1,] %>% 
               select(-Var1, -value)
top_factors <- c("INDUS", "NOX", "RM", "TAX", "PTRATIO", "LSTAT")
idx <- c(3, 4, 5, 9, 10, 12)

# generate correlational plots between significant factors
indus <- ggplot(housing, aes(x=INDUS, y=MEDV)) + 
         geom_point(color="#fa6e6e") + geom_smooth(method="lm", se=F) + theme_minimal() + theme(legend.position="none") + 
         labs(title="Non-retail Businesses vs. House Price") + xlab("Proportion of town's non-retail business acres") + 
         ylab("Median value of owner-occupied homes (in $1000's)") + scale_x_log10() + scale_y_log10() + theme(axis.text=element_text(size=20),
                                                                                                               axis.title=element_text(size=20), 
                                                                                                               plot.title=element_text(size=28, hjust=0.5))
nox <- ggplot(housing, aes(x=NOX, y=MEDV)) + 
       geom_point(color="#f96e9e") + geom_smooth(method="lm", se=F) + theme_minimal() + theme(legend.position="none") + 
       labs(title="Nitric Oxides in Air vs. House Price") + xlab("Nitric oxides concentration (parts per 10 million)") + 
       ylab("Median value of owner-occupied homes (in $1000's)") + theme(axis.text=element_text(size=20),
                                                                        axis.title=element_text(size=20), 
                                                                        plot.title=element_text(size=28, hjust=0.5))
rm <- ggplot(housing, aes(x=RM, y=MEDV)) + 
         geom_point(color="#e47ccb") + geom_smooth(method="lm", se=F) + theme_minimal() + theme(legend.position="none") + 
         labs(title="Number of Rooms vs. House Price") + xlab("Average number of rooms per dwelling") + 
         ylab("Median value of owner-occupied homes (in $1000's)") + theme(axis.text=element_text(size=20),
                                                                           axis.title=element_text(size=20), 
                                                                           plot.title=element_text(size=28, hjust=0.5))
tax <- ggplot(housing, aes(x=TAX, y=MEDV)) + 
         geom_point(color="#ba90ed") + geom_smooth(method="lm", se=F) + theme_minimal() + theme(legend.position="none") + 
         labs(title="Property Tax vs. House Price") + xlab("Full-value property-tax rate per $10,000") + 
         ylab("Median value of owner-occupied homes (in $1000's)") + theme(axis.text=element_text(size=20),
                                                                           axis.title=element_text(size=20), 
                                                                           plot.title=element_text(size=28, hjust=0.5))
ptratio <- ggplot(housing, aes(x=PTRATIO, y=MEDV)) + 
         geom_point(color="#7da3ff") + geom_smooth(method="lm", se=F) + theme_minimal() + theme(legend.position="none") + 
         labs(title="Pupil-teacher ratio vs. House Price") + xlab("Town's pupil-teacher ratio") + 
         ylab("Median value of owner-occupied homes (in $1000's)") + theme(axis.text=element_text(size=20),
                                                                           axis.title=element_text(size=20), 
                                                                           plot.title=element_text(size=28, hjust=0.5))
lstat <- ggplot(housing, aes(x=LSTAT, y=MEDV)) +
         geom_point(color="#12B2FF") + geom_smooth(method="lm", se=F) + theme_minimal() + theme(legend.position="none") + 
         labs(title="Lower-class Presence vs. House Price") + xlab("Percent lower status of the population") + 
         ylab("Median value of owner-occupied homes (in $1000's)") + theme(axis.text=element_text(size=20),
                                                                           axis.title=element_text(size=20), 
                                                                           plot.title=element_text(size=28, hjust=0.5))

indus
nox
rm
tax
ptratio
lstat

# compile into one graphic
grid.arrange(indus, nox, rm, tax, ptratio, lstat, ncol=3, nrow=2)

# add new col with different classifications for RANGE
housing <- housing %>% add_column(RANGE=NA)
housing[housing$MEDV >= 0.0,]$RANGE <- "<10k"
housing[housing$MEDV >= 10.0,]$RANGE <- "10k-20k"
housing[housing$MEDV >= 20.0,]$RANGE <- "20k-30k"
housing[housing$MEDV >= 30.0,]$RANGE <- "30k-40k"
housing[housing$MEDV >= 40.0,]$RANGE <- ">40k"

# set random seed
set.seed(7234002)

# clustering; kmeans
housing.km <- kmeans(housing[,top_factors], centers=2, nstart=25)
k2 <- clusplot(housing, housing.km$cluster, color=T, shade=T, labels=4, lines=0, 
               cex.axis=1.15, cex.lab=1.5, cex.main=2.5, main="Clusplot of Significant Price Factors: k = 2")

# classification; knn
housing.numerical <- housing %>% select(-RANGE)
ind <- sample(2, nrow(housing.numerical), replace=TRUE, prob=c(0.7, 0.3))

data.training <- housing.numerical[ind==1, idx]
data.test <- housing.numerical[ind==2, idx]
data.trainLabels <- housing[ind==1, 14]
data.testLabels <- housing[ind==2, 14]

# run knn algorithm
data.pred <- knn(train=data.training, test=data.test, cl=data.trainLabels, k=3)

merge <- data.frame(data.testLabels, data.pred)
names <- colnames(data.test)
finaldata <- cbind(data.test, merge)
names(finaldata)
names(finaldata) <- c(names, "Real Value", "Predicted Value")

table <- CrossTable(x=data.testLabels, y=data.pred, prop.chisq=FALSE)

# filter data for accuracy statistics
table$t
accuracy <- as.data.frame(table$t) %>% filter(x == y) %>% mutate(Acc=Freq) %>%
            select(-x, -Freq) %>% rename(RANGE=y)
idx <- rownames(data.test)
testing <- housing[idx,]

paste(accuracy$Acc[1], "/", nrow(testing[testing$RANGE == "<10k",]))
accuracy$Acc[1] <- accuracy$Acc[1] / nrow(testing[testing$RANGE == "<10k",]) * 100

paste(accuracy$Acc[3], "/", nrow(testing[testing$RANGE == "10k-20k",]))
accuracy$Acc[3] <- accuracy$Acc[3] /nrow(testing[testing$RANGE == "10k-20k",]) * 100

paste(accuracy$Acc[4], "/", nrow(testing[testing$RANGE == "20k-30k",]))
accuracy$Acc[4] <- accuracy$Acc[4] /nrow(testing[testing$RANGE == "20k-30k",]) * 100

paste(accuracy$Acc[5], "/", nrow(testing[testing$RANGE == "30k-40k",]))
accuracy$Acc[5] <- accuracy$Acc[5] /nrow(testing[testing$RANGE == "30k-40k",]) * 100

paste(accuracy$Acc[2], "/", nrow(testing[testing$RANGE == ">40k",]))
accuracy$Acc[2] <- accuracy$Acc[2] / nrow(testing[testing$RANGE == ">40k",]) * 100

# run BIC test to test for causality between significant figures
BIC(lm(housing$MEDV~1)) # 3417.103

# individual BIC
BIC(lm(housing$MEDV~housing$INDUS)) # 3204.794
BIC(lm(housing$MEDV~housing$NOX)) # 3266.027
BIC(lm(housing$MEDV~housing$RM)) # 3111.571
BIC(lm(housing$MEDV~housing$TAX)) # 3229.226
BIC(lm(housing$MEDV~housing$PTRATIO)) # 3269.837
BIC(lm(housing$MEDV~housing$LSTAT)) # 3269.837
# all drop, however they provide a much more
# significant value when combined together
BIC(lm(housing$MEDV~housing$INDUS + 
                    housing$NOX + 
                    housing$RM + 
                    housing$TAX + 
                    housing$PTRATIO + 
                    housing$LSTAT)) # 2810.167
# significant drop (-606.936); these factors cause change in price (to a degree)
