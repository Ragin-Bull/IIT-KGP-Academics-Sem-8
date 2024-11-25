# Exercise-1:
# One way analysis of variance (ANOVA) is used to test the null hypothesis that three or more means are equal.
A <- c(40, 30, 50, 50, 30)
B <- c(60, 40, 55, 65)
C <- c(60, 50, 70, 65, 75, 40)

grandC <- c(A, B, C)
meanA <- mean(A)
meanB <- mean(B)
meanC <- mean(C)
meanGrandC <- mean(grandC)

findSumOfSquaresBetween <- function(A, B, C,  meanGrandC) {
  ans <- length(A) * (mean(A) - meanGrandC)^2 + length(B) * (mean(B) - meanGrandC)^2 + length(C) * (mean(C) - meanGrandC)^2
  return (ans)
}

findSumOfSquaresWithin <- function(A, B, C, meanA, meanB, meanC) {
  ans <- sum((A - meanA)^2) + sum((B - meanB)^2) + sum((C - meanC)^2)
  return (ans)
}

SSW <- findSumOfSquaresWithin(A, B, C, meanA, meanB, meanC)
SSB <- findSumOfSquaresBetween(A, B, C, meanGrandC)

dfW <- length(grandC) - 3
dfB <- 3 - 1

MSW <- SSW / dfW
MSB <- SSB / dfB

F <- MSB / MSW
F1 <- 2.81
F2 <- 3.89

if(F>F1){
  print("Reject the Hypothesis that all means are significantly similar at significance level 0.1")
}else{
  print("Accept the Hypothesis that all means are significantly similar at significance level 0.1")
}

if(F>F2){
  print("Reject the Hypothesis that all means are significantly similar at significance level 0.05")
}else{
  print("Accept the Hypothesis that all means are significantly similar at significance level 0.05")
}

# We need to calculate which group contributes the most to the difference
t1 <- 1.782
t2 <- 2.179

LSD <-t2* sqrt(MSW * (1/length(A) + 1/length(B) + 1/length(C)))

# Comparision between Brand A and Brand B
if(abs(meanA - meanB) > LSD){
  print("Reject the Hypothesis that Brand A and Brand B are significantly similar at significance level 0.05")
}else{
  print("Accept the Hypothesis that Brand A and Brand B are significantly similar at significance level 0.05")
}

# Comparision between Brand A and Brand C
if(abs(meanA - meanC) > LSD){
  print("Reject the Hypothesis that Brand A and Brand C are significantly similar at significance level 0.05")
}else{
  print("Accept the Hypothesis that Brand A and Brand C are significantly similar at significance level 0.05")
}

# Comparision between Brand B and Brand C
if(abs(meanB - meanC) > LSD){
  print("Reject the Hypothesis that Brand B and Brand C are significantly similar at significance level 0.05")
}else{
  print("Accept the Hypothesis that Brand B and Brand C are significantly similar at significance level 0.05")
}


# Exercise-2:
yield <- matrix(c(8, 10, 6, 8, 3, 4, 5, 4, 7, 8, 6, 7), nrow = 4, byrow = FALSE)
colnames(yield) <- c("A", "B", "C")
rownames(yield) <- c("alpha", "beta", "gamma", "delta")

rowMean <- apply(yield, 1, mean)
colMean <- apply(yield, 2, mean)

grandMean <- mean(yield)
SSTotal <- sum((yield - grandMean)^2)

SSRows <- sum((rowMean - grandMean)^2) * 3 # 3 is the number of columns
SSCols <- sum((colMean - grandMean)^2) * 4 # 4 is the number of rows

SSError <- SSTotal - SSRows - SSCols

dfRow <- 3  # No. of rows  - 1
dfCol <- 2  # No. of columns - 1
dfError <- (3 - 1) * (4 - 1)

MSRows <- SSRows / dfRow
MSCols <- SSCols / dfCol
MSError <- SSError / dfError

FRows <- MSRows / MSError
FCols <- MSCols / MSError

FCriticalCols <- 5.14
FCriticalRows <- 4.76

if (FRows > FCriticalRows) {
  print("Reject the Hypothesis that the average fertilizer effects  are significantly similar at significance level 0.05")
} else {
  print("Accept the Hypothesis that the average fertilizer effects are significantly similar at significance level 0.05")
}

if (FCols > FCriticalCols) {
  print("Reject the Hypothesis that the average yields are significantly similar at significance level 0.05")
} else {
  print("Accept the Hypothesis that the average yields  are significantly similar at significance level 0.05")
}

# Exercise-3:
clarity <- matrix(c(28.30, 42.38, 33.33, 40.42), nrow = 2, byrow = FALSE)
colnames(clarity) <- c("4", "6")
rownames(clarity) <- c("1.5", "2.5")

rowMean <- apply(clarity, 1, mean)
colMean <- apply(clarity, 2, mean)

grandMean <- mean(clarity)
SSTotal <- sum((clarity - grandMean)^2)

SSRows <- sum((rowMean - grandMean)^2) * 3 # 3 is the number of columns
SSCols <- sum((colMean - grandMean)^2) * 4 # 4 is the number of rows

SSInteraction <- SSTotal - SSRows - SSCols

dfRow <- nrow(clarity) - 1  # No. of rows  - 1
dfCol <- ncol(clarity) - 1  # No. of columns - 1
dfInteraction <- dfRow * dfCol
dfTotal <- nrow(clarity) * ncol(clarity) - 1

MSRows <- SSRows / dfRow
MSCols <- SSCols / dfCol
MSInteraction <- SSInteraction / dfInteraction

FRows <- MSRows / MSInteraction
FCols <- MSCols / MSInteraction

FCritical <- 7.71

if (FRows > FCritical) {
  print("Reject the Hypothesis that the means along rows  are significantly similar at significance level 0.05")
} else {
  print("Accept the Hypothesis that the means along rows are significantly similar at significance level 0.05")
}

if (FCols > FCritical) {
  print("Reject the Hypothesis that the means along columns are significantly similar at significance level 0.05")
} else {
  print("Accept the Hypothesis that the means along columns are significantly similar at significance level 0.05")
}

if(MSInteraction > 0 && FRows < FCritical && FCols < FCritical){
  print("Reject the Hypothesis that there is an interaction between rows and columns at significance level 0.05")
}else{
  print("Accept the Hypothesis that there is an interaction between rows and columns at significance level 0.05")
}

# Exercise-4:
data <- matrix(c(10, 14, 7, 8,
                 7, 18, 11, 8,
                 5, 10, 11, 9,
                 10, 10, 12, 14), nrow = 4, byrow = TRUE)

globalMean <- mean(data)
data <- data - globalMean
n <- 4

treatments <- c(-11, -4, 12, 3)
rowSums <- apply(data, 1, sum)
colSums <- apply(data, 2, sum)

SSRows <- sum(rowSums^2) / 4
SSCols <- sum(colSums^2) / 4
SSTreatments <- sum(treatments^2) / 4
SSTotal <- sum(data^2)
SSError <- SSTotal - SSRows - SSCols - SSTreatments

dfRows <- n - 1
dfCols <- n - 1
dfTreatments <- n - 1
dfTotal <- n^2 - 1
dfError <- dfTotal - dfRows - dfCols - dfTreatments

MSRows <- SSRows / dfRows
MSCols <- SSCols / dfCols
MSTreatments <- SSTreatments / dfTreatments
MSError <- SSError / dfError

FRows <- MSRows / MSError
FCols <- MSCols / MSError
FTreatments <- MSTreatments / MSError

FCritical <- 4.76

#Print everything obtained till now
cat("The data is: ", data, "\n")
cat("The sum of squares along rows is: ", SSRows, "\n")
cat("The sum of squares along columns is: ", SSCols, "\n")
cat("The sum of squares of treatments is: ", SSTreatments, "\n")
cat("The sum of squares of error is: ", SSError, "\n")
cat("The degrees of freedom along rows is: ", dfRows, "\n")
cat("The degrees of freedom along columns is: ", dfCols, "\n")
cat("The degrees of freedom of treatments is: ", dfTreatments, "\n")
cat("The degrees of freedom of error is: ", dfError, "\n")
cat("The mean square along rows is: ", MSRows, "\n")
cat("The mean square along columns is: ", MSCols, "\n")
cat("The mean square of treatments is: ", MSTreatments, "\n")
cat("The mean square of error is: ", MSError, "\n")
cat("The F value along rows is: ", FRows, "\n")
cat("The F value along columns is: ", FCols, "\n")
cat("The F value of treatments is: ", FTreatments, "\n")

if(FRows > FCritical){
  print("Reject the Hypothesis that the means along rows are significantly similar at significance level 0.05")
}else{
  print("Accept the Hypothesis that the means along rows are significantly similar at significance level 0.05")
}

if(FCols > FCritical){
  print("Reject the Hypothesis that the means along columns are significantly similar at significance level 0.05")
}else{
  print("Accept the Hypothesis that the means along columns are significantly similar at significance level 0.05")
}

if(FTreatments > FCritical){
  print("Reject the Hypothesis that the means of treatments are significantly similar at significance level 0.05")
}else{
  print("Accept the Hypothesis that the means of treatments are significantly similar at significance level 0.05")
}


# Exercise - 5:
data <- matrix(c(11, 10, 14, 8,
                 8, 12, 10, 12,
                 9, 11, 7, 15,
                 9, 8, 18, 6), nrow = 4, byrow = TRUE)

globalMean <- mean(data)
data <- data - globalMean
n <- 4

treatments <- c(-7, -11, 14, 4)
workplace <- c(3, 4, 2, -1)
rowSums <- apply(data, 1, sum)
colSums <- apply(data, 2, sum)

SSRows <- sum(rowSums^2) / 4
SSCols <- sum(colSums^2) / 4
SSTreatments <- sum(treatments^2) / 4
SSWorkplace <- sum(workplace^2) / 4
SSTotal <- sum(data^2)
SSError <- SSTotal - SSRows - SSCols - SSTreatments - SSWorkplace

dfRows <- n - 1
dfCols <- n - 1
dfTreatments <- n - 1
dfWorkplace <- n - 1
dfTotal <- n^2 - 1
dfError <- dfTotal - dfRows - dfCols - dfTreatments - dfWorkplace

MSRows <- SSRows / dfRows
MSCols <- SSCols / dfCols
MSTreatments <- SSTreatments / dfTreatments
MSWorkplace <- SSWorkplace / dfWorkplace
MSError <- SSError / dfError

FRows <- MSRows / MSError
FCols <- MSCols / MSError
FTreatments <- MSTreatments / MSError
FWorkplace <- MSWorkplace / MSError

FCritical <- 9.28

#Print everything obtained till now
cat("The data is: ", data, "\n")
cat("The sum of squares along rows is: ", SSRows, "\n")
cat("The sum of squares along columns is: ", SSCols, "\n")
cat("The sum of squares of treatments is: ", SSTreatments, "\n")
cat("The sum of squares of workplace is: ", SSWorkplace, "\n")
cat("The sum of squares of error is: ", SSError, "\n")
cat("The degrees of freedom along rows is: ", dfRows, "\n")
cat("The degrees of freedom along columns is: ", dfCols, "\n")
cat("The degrees of freedom of treatments is: ", dfTreatments, "\n")
cat("The degrees of freedom of workplace is: ", dfWorkplace, "\n")
cat("The degrees of freedom of error is: ", dfError, "\n")
cat("The mean square along rows is: ", MSRows, "\n")
cat("The mean square along columns is: ", MSCols, "\n")
cat("The mean square of treatments is: ", MSTreatments, "\n")
cat("The mean square of workplace is: ", MSWorkplace, "\n")
cat("The mean square of error is: ", MSError, "\n")
cat("The F value along rows is: ", FRows, "\n")
cat("The F value along columns is: ", FCols, "\n")
cat("The F value of treatments is: ", FTreatments, "\n")
cat("The F value of workplace is: ", FWorkplace, "\n")

if(FRows > FCritical){
  print("Reject the Hypothesis that the means along rows are significantly similar at significance level 0.05")
}else{
  print("Accept the Hypothesis that the means along rows are significantly similar at significance level 0.05")
}

if(FCols > FCritical){
  print("Reject the Hypothesis that the means along columns are significantly similar at significance level 0.05")
}else{
  print("Accept the Hypothesis that the means along columns are significantly similar at significance level 0.05")
}

if(FTreatments > FCritical){
  print("Reject the Hypothesis that the means of treatments are significantly similar at significance level 0.05")
}else{
  print("Accept the Hypothesis that the means of treatments are significantly similar at significance level 0.05")
}

if(FWorkplace > FCritical){
  print("Reject the Hypothesis that the means of workplace are significantly similar at significance level 0.05")
}else{
  print("Accept the Hypothesis that the means of workplace are significantly similar at significance level 0.05")
}