# Task A Solution
givenData <- c(0.72, 0.92, 0.92, 1.43, 0.83, 0.48, 0.65, 0.78,
                  0.48, 0.96, 0.72, 0.48, 0.83, 0.49, 0.78, 0.96,
                  0.88, 1.03, 0.78, 1.12, 0.83, 0.78, 0.83, 1.06,
                  1.23, 0.18, 0.96, 1.18, 0.48, 0.55, 0.97, 1.21,
                  0.94, 0.38, 0.73, 0.65, 1.36, 0.47, 0.72, 0.77,
                  0.79, 1.26, 1.06, 0.90, 0.77, 0.35, 0.78, 0.77,
                  0.88, 1.20, 0.71, 0.95, 0.91, 0.64, 0.73, 1.09,
                  0.83, 0.78, 1.04, 1.33, 0.47, 0.16, 0.57, 0.65,
                  0.64, 0.65, 1.43, 0.63, 0.79, 1.00, 0.92, 0.45,
                  0.48, 0.79, 0.97, 0.57, 0.95, 1.12, 0.70, 1.05)

getClassIntervals <- seq(min(givenData), max(givenData), by = 0.20)
getFreq <- cut(givenData, breaks = getClassIntervals, include.lowest = TRUE)
getFreq  <- table(getFreq )

meanValue <- mean(givenData)
medianValue <- median(givenData)
quartiles <- quantile(givenData, probs = c(0.25, 0.5, 0.75))
modeValue <- names(sort(-table(givenData)))[1]
sdValue <- sd(givenData)
IQRValue <- IQR(givenData)

trimmedMean <- mean(givenData, trim = 0.05)
winsorizedMean <- mean(pmin(pmax(givenData, quantile(givenData, 0.05)), quantile(givenData, 0.95)))

printResults <- function (){
  cat("The Frequency Distribution is as follows:\n")
  cat(getFreq)
  cat("\nThe Mean is as follows:\n")
  cat(meanValue)
  cat("\nThe Median is as follows:\n")
  cat(medianValue)
  cat("\nThe Quartiles are as follows:\n")
  cat(quartiles)
  cat("\nThe Mode is as follows:\n")
  cat(modeValue)
  cat("\nThe Standard Deviation is as follows:\n")
  cat(sdValue)
  cat("\nThe Interquartile Range: is as follows: \n")
  cat(IQRValue)
  cat("\nThe Trimmed Mean with 5% trimming is :\n")
  cat(trimmedMean)
  cat("\nThe Winsorized Mean with 5% Winsorizing:\n")
  cat(winsorizedMean)
}

printResults()


# Task B Solution
newData <- c(9.3, 6.8, 9.8, 6.6, 4.3, 6.7, 6.4, 10.1, 8.9, 3.7,
             5.3, 6.5, 7.4, 8.3, 4.6, 7.9, 6.5, 5.1, 7.2, 8.7,
             7.9, 6.3, 2.7, 5.3, 8.8, 7.3, 9.0, 7.7, 8.4, 7.8,
             5.8, 6.4, 6.2, 5.8, 6.5, 6.0, 7.7, 5.0, 4.4, 4.7,
             5.4, 2.9, 4.0, 4.1, 4.1, 5.5, 3.1, 3.5, 5.4, 4.1,
             4.7, 6.2, 3.2, 2.7, 4.8, 2.6, 3.4, 6.2, 5.1, 4.0,
             5.0, 3.3, 2.4, 4.6, 2.8, 1.7, 0.9, 7.2, 9.9, 4.0,
             2.0, 2.0, 1.0, 3.2, 5.6, 3.4, 5.7, 7.0, 4.3, 3.4,
             3.0, 4.4, 2.0, 5.8, 1.5, 5.1, 5.0, 8.8, 4.0, 6.1,
             5.6, 5.4, 8.3, 8.8, 10.0, 4.8, 3.6, 2.5, 5.3, 2.2,
             4.1, 5.0)

classInts <- seq(floor(min(newData)), ceiling(max(newData)), by = 1)
freqTable <- cut(newData, breaks = classInts, include.lowest = TRUE)
freqTable <- table(freqTable)

generateMoments <- function(data, order) {
  n <- length(data)
  meanValue <- mean(data)
  centralMoments <- sum((data - meanValue)^order) / n
  noncentralMoments <- sum((data - meanValue)^order) / n
  return(list(central = centralMoments, noncentral = noncentralMoments))
}

firstMoment <- generateMoments(newData, 1)
secondMoment <- generateMoments(newData, 2)
thirdMoment <- generateMoments(newData, 3)
fourthMoment <- generateMoments(newData, 4)

skewness <- thirdMoment$central / (secondMoment$central^(3/2))
kurtosis <- fourthMoment$central / (secondMoment$central^2) - 3

printNewResults <- function() {
  cat("\nThe Frequency Distribution is as follows: \n")
  cat(freqTable)
  cat("\nThe First Moment (Mean) is as follows: \n")
  cat(firstMoment$noncentral)
  cat("\n The Second Moment (Variance) is as follows: \n")
  cat(secondMoment$central)
  cat("\n The Third Moment (Skewness) is as follows: \n")
  cat(skewness)
  cat("\nThe Fourth Moment (Kurtosis) is as follows: \n")
  cat(kurtosis)  
}

printNewResults()


# Task-C Solution
classIntsA <- seq(floor(min(givenData)), ceiling(max(givenData)) + 0.2, by = 0.20)
freqTableA <- cut(givenData, breaks = classIntsA, include.lowest = TRUE)
freqTableA <- table(freqTableA)
midsA <- classIntsA[-length(classIntsA)] + diff(classIntsA) / 2
densityA <- density(givenData)
upperLimsA <- classIntsA + 0.20
upperLimsA <- upperLimsA[-length(upperLimsA)]

classIntsB <- seq(floor(min(newData)), ceiling(max(newData)) + 1, by = 1)
freqTableB <- cut(newData, breaks = classIntsB, include.lowest = TRUE)
freqTableB <- table(freqTableB)
midsB <- classIntsB[-length(classIntsB)] + diff(classIntsB) / 2
densityB <- density(newData)
upperLimsB <- c(classIntsB[-1] + 1, max(classIntsB) + 1)
upperLimsB <- upperLimsB[-length(upperLimsB)]

par(mfrow=c(2, 2))

hist(givenData, breaks = classIntsA, main = "Frequency Histogram (Task A)", xlab = "Iron Solution Index", ylab = "Frequency", col="red")
plot(midsA, freqTableA, type = "l", main = "Frequency Polygon (Task A)", xlab = "Iron Solution Index", ylab = "Frequency" , col="blue")
plot(densityA, main = "Frequency Curve (Task A)", xlab = "Iron Solution Index", ylab = "Density", col="pink")
plot(upperLimsA, cumsum(freqTableA), type = "s", main = "Cumulative Frequency Curve (Task A)", xlab = "Iron Solution Index", ylab = "Cumulative Frequency", col="green")

hist(newData, breaks = classIntsB, main = "Frequency Histogram (Task B)", xlab = "Radioactive newData", ylab = "Frequency", col="lightblue")
plot(midsB, freqTableB, type = "l", main = "Frequency Polygon (Task B)", xlab = "Radioactive newData", ylab = "Frequency", col="red")
plot(densityB, main = "Frequency Curve (Task B)", xlab = "Radioactive newData", ylab = "Density", col="pink")
plot(upperLimsB, cumsum(freqTableB), type = "s", main = "Cumulative Frequency Curve (Task B)", xlab = "Radioactive newData", ylab = "Cumulative Frequency", col="green")

par(mfrow=c(1, 1))