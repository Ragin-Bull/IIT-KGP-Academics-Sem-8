# Exercise - 1
generate_hypothesis_texting_for_means <- function(x, given_mean, alpha){
  z <- (mean(x) - given_mean) / (sd(x)/sqrt(length(x)))
  critical_value <- qt(1 - alpha/2, length(x)-1)
  
  if(abs(z) > critical_value){
    print("Reject the Null Hypothesis")
  } else {
    print("Accept the Null Hypothesis")
  }
}

deviations <- c(11.28, -9.48, -10.42, 6.25, -8.51, 10.11, 1.95, -8.65, 6.47, -0.68)
generate_hypothesis_texting_for_means(deviations, 0, 0.05)
generate_hypothesis_texting_for_means(deviations, 0, 0.1)

# Exercise - 2
test_of_hypothesis_for_variance <- function(x, given_variance, alpha){
  chi_square <- (length(x) - 1) * var(x) / given_variance
  critical_value <- qchisq(1 - alpha, length(x)-1)
  
  if(chi_square > critical_value){
    print("Reject the Null Hypothesis")
  } else {
    print("Accept the Null Hypothesis")
  }
}

lengths <- c(7.12, 7.13, 7.01, 6.95, 6.89, 6.97, 6.99, 6.93, 7.05, 7.02)
test_of_hypothesis_for_variance(lengths, 0.005, 0.05)

# Exercise - 3


# Exercise - 4
test_of_hypothesis_for_means_less_than <- function(x, given_mean, alpha){
  t <- (mean(x) - given_mean) / (sd(x)/sqrt(length(x)))
  critical_value <- qt(1 - alpha, length(x)-1)
  
  if(t >= critical_value){
    print("Reject the Null Hypothesis that that the average time for the athelete to
complete the race is less than 10 seconds")
  } else {
    print("Accept the Null Hypothesis that that the average time for the athelete to
  complete the race is less than 10 seconds")
  }
}

times <- c(9.85, 9.93, 9.75, 9.77, 9.67, 9.87, 9.67, 9.94, 9.85, 9.75)
test_of_hypothesis_for_means_less_than(times, 10, 0.05)


# Exercise - 5
test_of_hypothesis_of_two_means_less_than <- function(alpha, mean1, variance1, mean2, variance2, n1, n2, alpha){
  # pooled_sd <- sqrt(((n1 - 1) * variance1 + (n2 - 1) * variance2) / (n1 + n2 - 2))
  # t_statistic <- (mean1 - mean2) / (pooled_sd * sqrt(1/n1 + 1/n2))
  t <- (mean1 - mean2) / (sqrt(variance1/n1 + variance2/n2))
  df <- n1 + n2 - 2
  critical_value <- qt(1-alpha, df)
  
  # Make decision
  if (t >= critical_value) {
    print("Reject the null hypothesis")
  } else {
    print("Accept the null hypothesis")
  }
}

test_of_hypothesis_of_two_means_less_than(0.05, 16, 1.4, 20, 2, 15, 19, 0.05)

# Exercise - 6
test_of_hypothesis_of_two_variances <- function(variance1, variance2, n1, n2, alpha){
  f <- variance1 / variance2
  critical_value <- qf(1-alpha/2, n1-1, n2-1)
  
  if(f > critical_value){
    print("Reject the Null Hypothesis")
  } else {
    print("Accept the Null Hypothesis")
  }
}


test_of_hypothesis_of_two_means <- function(mean1, mean2, variance1, variance2, n1, n2, alpha){
  t <- (mean1 - mean2) / (sqrt(variance1/n1 + variance2/n2))
  df <- n1 + n2 - 2
  critical_value <- qt(1-alpha/2, df)
  
  if(abs(t) > critical_value){
    print("Reject the Null Hypothesis")
  } else {
    print("Accept the Null Hypothesis")
  }
}

test_of_hypothesis_of_two_variances(3.89, 4.02, 8, 8, 0.05)
test_of_hypothesis_of_two_means(91.73, 93.75, 3.89, 4.02, 8, 8, 0.05)

# Exercise - 7
test_of_hypothesis_of_two_variances_given_data <- function(x, y, alpha){
  variance1 <- var(x)
  variance2 <- var(y)
  f <-  variance1 / variance2
  n1 <- length(x)
  n2 <- length(y)
  critical_value <- qf(1-alpha/2, n1-1, n2-1)
  
  if(f > critical_value){
    print("Reject the Null Hypothesis")
  } else {
    print("Accept the Null Hypothesis")
  }
}

test_of_hypothesis_of_two_means_given_data_less_than <- function(x, y, alpha){
  mean1 <- mean(x)
  mean2 <- mean(y)
  variance1 <- var(x)
  variance2 <- var(y)
  n1 <- length(x)
  n2 <- length(y)
  t <- (mean1 - mean2) / (sqrt(variance1/n1 + variance2/n2))
  df <- n1 + n2 - 2
  critical_value <- qt(1-alpha, df)
  
  # Make decision
  if (t >= critical_value) {
    print("Reject the null hypothesis")
  } else {
    print("Accept the null hypothesis")
  }
}

x<- c(150, 250, 240, 280, 290, 210, 220, 180)
y <- c(140, 230, 270, 190, 270, 200, 150, 200, 190, 170)

test_of_hypothesis_of_two_variances_given_data(x, y, 0.1)
test_of_hypothesis_of_two_means_given_data_less_than(x, y, 0.05)

# Exercise - 8

x <- c(12, 29, 16, 37, 28, 15)
y <- c(10, 28, 17, 35, 25, 16)

test_of_hypothesis_of_two_variances_given_data(x, y, 0.1)
test_of_hypothesis_of_two_means_given_data_less_than(x, y, 0.05)


# Exercise - 9