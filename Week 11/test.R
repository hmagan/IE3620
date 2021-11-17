source("../myfunctions.R")
days <- c(-36, -18, -9, -9, -7, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 4, 5, 5, 5, 5, 5, 5, 5, 6, 6, 8, 8, 8, 10, 10, 10, 10, 10, 10, 15, 15, 20, 20, 20, 20, 50, 69, 76, 150)
data = data.frame(days)

par(mfrow=c(1,2))

hist(data$days, main="garb data without rz", xlab="days")

data <- rz.transform(data$days)

hist(data, main="new and improved data", xlab="days")
