# Exercise - 1
# Assuming the value of variance for the data in unknown

get_value_of_t_given_mean_unknown_variance <- function(x, given_mean){
  t <- (mean(x) - given_mean) / (sd(x)/sqrt(length(x)))
  return(t)
}

alpha <- 0.05
deviations <- c(11.28, -9.48, -10.42, 6.25, -8.51, 10.11, 1.95, -8.65, 6.47, -0.68)
critical_value <- qt(1 - alpha/2, length(deviations)-1)
t <- get_value_of_t_given_mean_unknown_variance(deviations, 0)
print(t)
print(critical_value)

if(abs(t) > critical_value){
  cat("Reject the Null Hypothesis that mean is equal to 0 with alpha = 0.05\n")
} else {
  cat("Accept the Null Hypothesis that mean is equal to 0 with alpha = 0.05\n")
}

alpha <- 0.1
critical_value <- qt(1 - alpha/2, length(deviations)-1)

if(abs(t) > critical_value){
  cat("Reject the Null Hypothesis that mean is equal to 0 with alpha = 0.1\n")
} else {
  cat("Accept the Null Hypothesis that mean is equal to 0 with alpha = 0.1\n")
}


# Exercise - 2
get_value_of_x_given_variance <- function(x, given_variance){
  chi_square <- (length(x) - 1) * var(x) / given_variance
  return(chi_square)
}

lengths <- c(7.12, 7.13, 7.01, 6.95, 6.89, 6.97, 6.99, 6.93, 7.05, 7.02)
alpha <- 0.05
chi_square <- get_value_of_x_given_variance(lengths, 0.005)
critical_value <- qchisq(alpha, length(lengths)-1)
print(critical_value)
print(chi_square)

if(chi_square > critical_value){
  cat("Reject the Null Hypothesis that variance is less than equal to 0.005 with alpha = 0.05\n")
} else {
  cat("Accept the Null Hypothesis that variance is less than equal to 0.005 with alpha = 0.05\n")
}


# Exercise - 3
get_value_of_z_given_mean_known_variance <- function(x, given_mean, stdev){
  z <- (mean(x) - given_mean) / (stdev/sqrt(length(x)))
  return(z)
}

strengths <- c(578, 572, 570, 568, 572, 570, 570, 572, 596, 584)
z <- get_value_of_z_given_mean_known_variance(strengths, 570, 8.75)
alpha <- 0.01
critical_value <- qnorm(alpha)
print(critical_value)
print(z)

if(z > critical_value){
  cat("Reject the Null Hypothesis that mean = 570 with alpha = 0.1\n")
} else {
  cat("Accept the Null Hypothesis that mean = 570 with alpha = 0.1\n")
}


# Exercise - 4
alpha <- 0.05
times <- c(9.85, 9.93, 9.75, 9.77, 9.67, 9.87, 9.67, 9.94, 9.85, 9.75)
t <- get_value_of_t_given_mean_unknown_variance(times, 10)
critical_value <- qt(1-alpha, length(times)-1)
print(critical_value)
print(t)

if(t >= critical_value){
  print("Reject the Null Hypothesis that that the average time for the athelete to complete the race is less than 10 seconds")
} else {
  print("Accept the Null Hypothesis that that the average time for the athelete to complete the race is less than 10 seconds")
}


# Exercise - 5
test_of_hypothesis_of_two_means_less_than <- function(mean1, variance1, mean2, variance2, n1, n2){
  pooled_variance <- ((n1-1)*variance1 + (n2-1)*variance2) / (n1 + n2 - 2)
  t <- (mean1 - mean2) / sqrt(pooled_variance * (1/n1 + 1/n2))
  return(t)
}

t <- test_of_hypothesis_of_two_means_less_than(16, 1.4, 20, 2, 15, 19)
alpha <- 0.05
n1 <- 15
n2 <- 19
df <- n1 + n2 - 2
critical_value <- qt(1-alpha, df)
print(critical_value)
print(t)

if (t >= critical_value) {
  print("Reject the null hypothesis")
} else {
  print("Accept the null hypothesis")
}

# Exercise - 6
test_of_hypothesis_of_two_variances <- function(variance1, variance2, n1, n2, alpha){
  f <- variance1 / variance2
  critical_value1 <- qf(1-alpha/2, n1-1, n2-1)
  critical_value2 <- qf(alpha/2, n1-1, n2-1)
  print("Testing of variances")
  print(f)
  print(critical_value1)
  print(critical_value2)
  
  if(f>critical_value1 ||  f<critical_value2){
    cat("Reject the Null Hypothesis\n")
  } else {
    cat("Accept the Null Hypothesis\n")
  }
}


test_of_hypothesis_of_two_means <- function(mean1, mean2, variance1, variance2, n1, n2, alpha){
  pooled_variance <- ((n1-1)*variance1 + (n2-1)*variance2) / (n1 + n2 - 2)
  t <- (mean1 - mean2) / sqrt(pooled_variance * (1/n1 + 1/n2))
  df <- n1+n2-2
  critical_value <- qt(alpha/2, df)
  print("Testing of means")
  print(t)
  print(critical_value)
  
  if(abs(t) > abs(critical_value)){
    cat("Reject the Null Hypothesis")
  } else {
    cat("Accept the Null Hypothesis")
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
  critical_value1 <- qf(1-alpha/2, n1-1, n2-1)
  critical_value2 <- qf(alpha/2, n1-1, n2-1)
  print("Testing of variances")
  print(f)
  print(critical_value1)
  print(critical_value2)
  
  if(f>critical_value1 ||  f<critical_value2){
    cat("Reject the Null Hypothesis\n")
  } else {
    cat("Accept the Null Hypothesis\n")
  }
}

test_of_hypothesis_of_two_means_given_data_less_than <- function(x, y, alpha){
  mean1 <- mean(x)
  mean2 <- mean(y)
  variance1 <- var(x)
  variance2 <- var(y)
  n1 <- length(x)
  n2 <- length(y)
  pooled_variance <- ((n1-1)*variance1 + (n2-1)*variance2) / (n1 + n2 - 2)
  t <- (mean1 - mean2) / sqrt(pooled_variance * (1/n1 + 1/n2))
  df <- n1+n2-2
  critical_value <- qt(1-alpha, df)
  print("Testing of means")
  print(t)
  print(critical_value)
  
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
is_age_opinion_related <- function(data, alpha){
  O <- data
  E <- data
  chi_sq_value <- 0
  for(i in 1 : nrow(data)){
    for(j in 1 : ncol(data)){
      E[i,j] <- (sum(data[i,]) * sum(data[,j])) / sum(data)
      chi_sq_value  <- chi_sq_value  + ((O[i,j] - E[i,j])^2) / E[i,j]
    }
  }
  critical_value <- qchisq(alpha,(nrow(data)-1)*(ncol(data)-1))
  if(chi_sq_value  > critical_value){
    print("Reject the Null Hypothesis")
  } else {
    print("Accept the Null Hypothesis")
  }
}
is_age_opinion_related(matrix(c(400,600,100,400,500,500),nrow=2,ncol=3),0.05)