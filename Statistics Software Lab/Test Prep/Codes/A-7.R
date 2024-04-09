# Question - 1 Solution
find_Confidence_Interval_Given_sigma <- function(x, alpha, sigma) {
  n <- length(x)
  x_Bar <- mean(x)
  z <- qnorm(1 - alpha / 2)
  lower_Limit <- x_Bar - z * (sigma / sqrt(n))
  upper_Limit <- x_Bar + z * (sigma / sqrt(n))
  return(c(lower_Limit, upper_Limit))
}

find_Confidence_Interval_Sigma_Unknown <- function(x, alpha) {
  n <- length(x)
  x_Bar <- mean(x)
  s <- sd(x)
  t <- qt(1 - alpha / 2, df = n - 1)
  lower_Limit <- x_Bar - t * (s / sqrt(n))
  upper_Limit <- x_Bar + t * (s / sqrt(n))
  return(c(lower_Limit, upper_Limit))
}

# Given data is as follows:
x <- c(3.3, -0.3, -0.6, -0.9)
alpha <- 0.1
sigma <- 3

cat("Question 1\n")
cat("Given data is: ", x, "\n", "Mean is: ", mean(x), "\n", "Standard Deviation is: ", sd(x), "\n")
cat("90% Confidence Interval for the given data but for known sigma:", find_Confidence_Interval_Given_sigma(x, alpha, sigma), "\n")
cat("90% Confidence Interval for the given data but for unknown sigma:", find_Confidence_Interval_Sigma_Unknown(x, alpha), "\n")

# Question - 2 Solution
find_Confidence_Interval_With_Unknown_Data <- function(mean_Value, sigma, n, alpha){
  z <- qnorm(1 - alpha / 2)
  lower_Limit <- mean_Value - z * sigma / sqrt(n)
  upper_Limit <- mean_Value + z * sigma / sqrt(n)
  return(c(lower_Limit, upper_Limit))
}

mean_Value <- 11
std_Dev <- 3
alpha <- 0.05
n <- 150

cat("Question 2\n")
cat("Given unknown data with mean 11, standard deviation 3, n 150 and alpha 0.05\n")
cat("95% Confidence Interval for the given data:", find_Confidence_Interval_With_Unknown_Data(mean_Value, std_Dev, n, alpha), "\n")

# Question - 3 Solution
gas_volume <- c(290, 610, 790, 670, 770, 420, 600, 350, 800, 920, 
                410, 810, 620, 560, 550, 610, 510, 390, 480, 630, 
                470, 380, 550, 570, 730, 680, 530, 650, 1000, 720)

cat("Question 3\n")
cat("Given data is: ", gas_volume, "\n")
cat("Given mean is: ", mean(gas_volume), "\n", "Given standard deviation is: ", sd(gas_volume), "\n")
cat("99% Confidence Interval for the given data:", find_Confidence_Interval_Sigma_Unknown(gas_volume, 0.01), "\n")

# Question - 4 Solution
find_Confidence_Interval_For_Variance <- function(x, alpha) {
  n <- length(x)
  s_Squared <- var(x)
  lower_Limit <- (n - 1) * s_Squared / qchisq(1 - alpha / 2, df = n - 1)
  upper_Limit <- (n - 1) * s_Squared / qchisq(alpha / 2, df = n - 1)
  return(c(lower_Limit, upper_Limit))
}

x <- c(1.48, 1.26, 1.52, 1.56, 1.48, 1.46, 1.30, 1.28, 1.43, 1.43, 
       1.55, 1.57, 1.51, 1.53, 1.68, 1.37, 1.47, 1.61, 1.49, 1.43, 
       1.64, 1.51, 1.60, 1.65, 1.60, 1.64, 1.51, 1.51, 1.53, 1.74)

alpha <- 0.05
cat("Question 4\n")
cat("Given data is: ", x, "\n")
cat("The variance for the given data is: ", var(x), "\n")
cat("95% Confidence Interval for the given data:", find_Confidence_Interval_For_Variance(x, alpha), "\n")

#Question - 5 Solution
find_Confidence_Interval_For_Difference_Of_Means <- function(x1, x2, alpha) {
  n1 <- length(x1)
  n2 <- length(x2)
  x1_Bar <- mean(x1)
  x2_Bar <- mean(x2)
  s1 <- sd(x1)
  s2 <- sd(x2)
  degrees_Of_Freedom = (s1^2 / n1 + s2^2 / n2)^2 / ((s1^2 / n1)^2 / (n1 + 1) + (s2^2 / n2)^2 / (n2 + 1)) - 2
  t <- qt(1 - alpha / 2, df = degrees_Of_Freedom)
  lower_Limit <- (x1_Bar - x2_Bar) - t * sqrt((s1^2 / n1) + (s2^2 / n2))
  upper_Limit <- (x1_Bar - x2_Bar) + t * sqrt((s1^2 / n1) + (s2^2 / n2))
  return(c(lower_Limit, upper_Limit))
}

x1 <- c(1.79, 1.75, 1.67, 1.65, 1.87, 1.74, 1.94, 1.62, 2.06, 1.33, 1.96, 1.69, 1.70)
x2 <- c(2.39, 2.51, 2.86, 2.14, 2.56, 2.29, 2.49, 2.36, 2.58, 2.33, 2.62, 2.41, 1.94)
alpha <- 0.1

cat("Question 5\n")
cat("x1 = ", x1, "\n", "x2 = ", x2, "\n")
cat("Mean of x1 is: ", mean(x1), "\n", "Mean of x2 is: ", mean(x2), "\n")
cat("Standard deviation of x1 is: ", sd(x1), "\n", "Standard deviation of x2 is: ", sd(x2), "\n")
cat("The 95% Confidence Interval for the difference of means:", find_Confidence_Interval_For_Difference_Of_Means(x1, x2, alpha), "\n")

#Question - 6 Solution
x1 <- c(14, 12, 18, 16, 15)
x2 <- c(16, 15, 17, 16, 14)
alpha <- 0.05

cat("Question 6\n")
cat("x1 = ", x1, "\n", "x2 = ", x2, "\n")
cat("Mean of x1 is: ", mean(x1), "\n", "Mean of x2 is: ", mean(x2), "\n")
cat("Standard deviation of x1 is: ", sd(x1), "\n", "Standard deviation of x2 is: ", sd(x2), "\n")
cat("The Confidence Intervals for the difference of means:", find_Confidence_Interval_For_Difference_Of_Means(x1, x2, alpha), "\n")

#Question - 7 Solution
find_Confidence_Interval_For_Standard_Deviations_Data_Unknown <- function(alpha, s, n) {
  lower_Limit <- s * sqrt((n - 1) / qchisq(1 - alpha / 2, df = n - 1))
  upper_Limit <- s * sqrt((n - 1) / qchisq(alpha / 2, df = n - 1))
  return(c(lower_Limit, upper_Limit))
}
alpha <- 0.05
n <- 100
s <- 0.01

cat("Question 7\n")
cat("Given alpha is: ", alpha, "\n", "Given n is: ", n, "\n", "Given s is: ", s, "\n")
cat("The CI for Standard Deviations is: ", find_Confidence_Interval_For_Standard_Deviations_Data_Unknown(alpha, s, n), "\n")