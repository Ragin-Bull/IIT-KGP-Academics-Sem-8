## Estimation of the value of pi:
BIG_NUM <- 1000
generate_rand_value <- function (num, lo, hi) {
  set.seed(67)
  U<-runif(num, lo, hi)
  return(U)
}

X <-generate_rand_value(BIG_NUM, -1, 1)
Y <-generate_rand_value(BIG_NUM, -1, 1)

count<-0

for(x in X){
  for(y in Y){
    if(x^2 + y^2<=1){
      count <- count + 1
    }
  }
}
pi_val <- 4*(count/(BIG_NUM*BIG_NUM))

# Approximately the value P(X^2 + Y^2 <=1) = pi/4, so we need to multiply the value by 4 inroder to estimate the value of pi
sprintf("The estimated value of pi is: %f", pi_val)


## Solution to assignment problems:
# a) Usage of Probability Integral Transform to generate a random sample from exponential distribution
sigma <- 67
theta <- 0

# Now we shall calculate the value of the given distribution
get_rand_exp_dist <- function (num){
  x <- runif(num)
  h <- theta + (-1/sigma)*log(1-x)
  return(h)
}

# Now we visualise the exponential distribution:
X <- get_rand_exp_dist(1000)
hist(X, main = "Histogram of Exponential Distribution", xlab = "Value", ylab = "Frequency", col = "lightblue", border = "black")

# First we generate the intervals and k = 8 (let's say)
lambda <- 1000/sum(X)
k<-152
intervals <- numeric(0)
prob<-1/k
E <- rep(1000*prob, k)

for(i in 0:(k-1)){
  intervals<-append(intervals, (-1/lambda)*log(1-i*prob))  
}

# Now we shall proceed to obtain the observed frequencies
get_observed_freq <- function (X, ints) {
  freqs <-numeric(0)
  for(i in 2 : length(ints)){
    freqs<-append(freqs, sum(X >= ints[i-1] & X< ints[i]))
  }
  freqs<-append(freqs, length(X)-sum(freqs))
  return(freqs)
}

O <- get_observed_freq(X, intervals)
W <- sum(((O-E)^2)/E)
criticial_value <- qchisq(0.95, k-1)
if(W > criticial_value){
  print("The given distribution doesnt follow Exponential Distribution")
} else {
  print("The given distribution follows Exponential Distribution")
}


# b)  Usage of Probability Integral Transform to generate a random sample from Cauchy Distribution
sig <- 0.67

get_cauchy_dist <- function(num) {
  x <- runif(num)
  h <- sig*tan(pi*(x-0.5))
  return(h)
}

X <- get_cauchy_dist(1000)
hist(X, main = "Histogram of Cauchy Distribution", xlab = "Value", ylab = "Frequency", col = "brown", border = "black")

k<-164
intervals <- numeric(0)
prob<-1/k
E <- rep(1000*prob, k)

for(i in 0:(k-1)){
  intervals<-append(intervals, sig*tan(pi*((i*prob)-0.5)))  
}

O <- get_observed_freq(X, intervals)
W <- sum(((O-E)^2)/E)
criticial_value <- qchisq(0.95, k-1)

if(W > criticial_value){
  print("The given distribution doesnt follow Cauchy Distribution")
} else {
  print("The given distribution follows Cauchy Distribution")
}

# c)  Usage of Probability Integral Transform to generate a random sample from Double Exponential Distribution
mu <- 0
beta <- 0.67
get_dde_dist <- function(num) {
  x <- runif(num, 0, 1)
  h <- mu - beta * sign(x - 0.5) * log(1 - 2 * abs(x - 0.5))
  return(h)
}

X <- get_dde_dist(1000)
hist(X, main = "Histogram of Double Exponential Distribution", xlab = "Value", ylab = "Frequency", col = "red", border = "black")

k<-157
intervals <- numeric(0)
prob<-1/k
E <- rep(1000*prob, k)

for(i in 0:(k-1)){
  intervals<-append(intervals, - beta * sign(prob*i - 0.5) * log(1 - 2 * abs(prob*i - 0.5)))  
}

O <- get_observed_freq(X, intervals)
W <- sum(((O-E)^2)/E)
criticial_value <- qchisq(0.95, k-1)

if(W > criticial_value){
  print("The given distribution doesnt follow Double Exponential Distribution")
} else {
  print("The given distribution follows Double Exponential Distribution")
}

# d)  Usage of Probability Integral Transform to generate a random sample from Right Trapezoidal Distribution
generate_rtz_dist <- function (num, lo, loin, lomax, hi) {
  x <- runif(num)
  h <- numeric(0)
  val <- (lomax - lo) / (hi - lo)
  
  for (i in 1:num) {
    if (x[i]<val) {
      h <- append(h, lo + sqrt(x[i] * (loin - lo) * (lomax - lo)))
    } else {
      h <- append(h, lomax + (x[i] - (lomax - lo) / (hi- lo)) * (hi - lomax))
    }
  }
  return(h)
}

lo = 1
loin=1
lomax=1
hi=3

X <- generate_rtz_dist(1000, lo,loin,lomax,hi)
hist(X, main = "Histogram of Right Trapezoidal Distribution", xlab = "Value", col = "red", border = "black")

k<-157
intervals <- numeric(0)
prob<-1/k
E <- rep(1000*prob, k)
val <- (lomax - lo) / (hi - lo)

for(i in 0:(k-1)){
  value <- i*prob
  if (value<val) {
    intervals <- append(intervals, lo + sqrt(value * (loin - lo) * (lomax - lo)))
  } else {
    intervals <- append(intervals, lomax + (value - (lomax - lo) / (hi- lo)) * (hi - lomax))
  }
}

O <- get_observed_freq(X, intervals)
W <- sum(((O-E)^2)/E)
criticial_value <- qchisq(0.95, k-1)

if(W > criticial_value){
  print("The given distribution doesnt follow Trapezoidal Distribution")
} else {
  print("The given distribution follows Trapezoidal Distribution")
}
# # e)  Usage of Probability Integral Transform to generate a random sample from Cauchy Distribution
generate_std_normie <- function(num) {
  x1 <- runif(num, 0, 1)
  x2 <- runif(num, 0, 1)
  
  z1 <- sqrt(-2 * log(x1)) * cos(2 * pi * x2)
  z2 <- sqrt(-2 * log(x1)) * sin(2 * pi * x2)
  
  return(cbind(z1, z2))
}

generate_normal <- function(num, mean, stdev) {
  z <- generate_std_normie(num)
  x <- mean + stdev*z
  return(x)
}

mean <- 0
stdev <- 1
num<-1000

X <- generate_normal(1000, mean=0, stdev=1)
hist(X, main = "Histogram of Normal Distribution", xlab = "Value", col = "green", border = "black")

brs <- seq(-4, 4, by = 1)
O <- table(cut(X, breaks = brs, include.lowest = TRUE))

E <- diff(pnorm(brs))
W <- sum((O - E * length(X))^2 / (E * length(X)))
k <- length(brs)
critical_value <- qchisq(0.95, k-1)

if (W > critical_value) {
  print("The sample doesnt follow a standard normal distribution")
} else {
  print("The sample follows a standard normal distribution.")
}

# k<-8
# intervals <- numeric(0)
# prob<-1/k
# E <- rep(100*prob, k)
# E
# for(i in 1:(k-1)){
#   intervals<-append(intervals, i*prob)
# }
# 
# intervals <- sqrt(-2 * log(intervals)) * cos(2 * pi * intervals)
# 
# O <- get_observed_freq(X, intervals)
# W <- sum(((O-E)^2)/E)
# criticial_value <- qchisq(0.95, 7)
# 
# if(W > criticial_value){
#   print("The given distribution doesnt follow Normal Distribution")
# } else {
#   print("The given distribution follows Normal Distribution")
# }

#  f)  Usage of Probability Integral Transform to generate a random sample from Cauchy Distribution
# generate_gamma_dist <- function(num, s, r) {
#   x <- runif(num)
#   
#   generate_gamma_inv <- function(x, s, r) {
#     gamma_inc <- function(s, a) {
#       integrate(function(q) q^(a-1) * exp(-q), lower = 0, upper = s)$value
#     }
#     
#     uniroot(function(x) gamma_inc(x, s) / gamma_inc(r, s) - x, interval = c(0, 100))$root
#   }
# 
#   h <- sapply(x, function(x_i) generate_gamma_inv (x_i, s, r))
#   return(h)
# }
# 
# s <- 2
# r <- 1
# num <- 100
# 
# X <- generate_gamma_dist(n, s, r)
# 
# hist(X, main = "Histogram of Normal Distribution", xlab = "Value", col = "green", border = "black")

num <- 1000
sh <- 2
sc <- 1

generate_sample <- rgamma(num, shape = sh, scale = sc)
E <- pgamma(sort(generate_sample), shape = sh, scale = sc)
O <- pnorm(sort(generate_sample))

W <- sum((O - E)^2 / E)
k <- num
get_sharp_value <- 1 - pchisq(W, df = (k-1))

if (get_sharp_value >= 0.05) {
  print("This distribution doesnt follow Gamma Distribution")
} else {
  print("This distribution follows Gamma Distribution")
}