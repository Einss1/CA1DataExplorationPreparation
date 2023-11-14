library(ggplot2)
install.packages("gridExtra")
library(gridExtra)

cars <- read.csv(file = "cars.txt",stringsAsFactors = FALSE)
mydata <- cars
View(mydata)

ggplot(mydata, aes(x=mpg, y = weightlbs)) + geom_point()

normalizedMM <- function(x) {
  return ((x - min(x) )/ (max(x) - min(x)))
}

mpg_norm <- normalizedMM(mydata$mpg)
weightlbs_norm <- normalizedMM(mydata$weightlbs)

p1 <- ggplot(mydata, aes(x=mpg, y = weightlbs)) + geom_point() + labs(title = "Original data")

p2 <- ggplot(mydata, aes(x=mpg_norm, y = weightlbs_norm)) + geom_point()+ labs(title = "Minmax normalized data")


grid.arrange(p1, p2, ncol = 2, nrow = 1)


