# a) Generation of Random Variables for Bernoulli Distribution
set.seed(20067)
BIG_NUM <- 1000
p =0.67

generate_bernoulli_rv <- function(p) {
  U <- runif(BIG_NUM)
  n <- length(U)
  X <- numeric(0)
  
  for (i in 1:n) {
    if (U[i] < p)
      X <- append(X, 1)
    else
      X <- append(X, 0)
  }
  return(X)
}

# Generating the random sample of Bernoulli Distribution
X <- generate_bernoulli_rv(p)
hist(X, main = "Histogram of Bernoulli Distribution", xlab = "Value", ylab = "Frequency", col = "lightgreen", border = "black")

# Applying the Chi Square Goodness of fit test for the generated sample
get_frequency_bernoulli <- function (g) {
  n <- length(g)
  freq<-c(0,0)
  
  for (i in 1:n){
    if(g[i]==1){
      freq[1] <- freq[1] + 1
    } else{
      freq[2] <- freq[2] + 1
    }
  }
  return (freq)
}

E <- c(BIG_NUM*(p), BIG_NUM*(1- p))
O <- get_frequency_bernoulli(X)

W <- sum(((O-E)^2)/E)
criticial_value <- qchisq(0.95, length(E)-1)

if(W > criticial_value){
  print("The given distribution doesnt follow Bernoulli Distribution")
} else {
  print("The given distribution follows Bernoulli Distribution")
}

# b) Generating random valiues following Discrete Uniform Distribution in the interval (i,j) 
i = 10
j = 700

get_discrete_uniform_dist <- function () {
  U <- runif(BIG_NUM, 0, 1)
  return (i + floor((j-i+1)*U))
}

X<-get_discrete_uniform_dist()
hist(X, main = "Histogram of Uniform Distribution", xlab = "Value", ylab = "Frequency", col = "pink", border = "blue")

for(y in unique(X)){
  O <- append(O, length(X[X == y])) 
  E <- append(E, 1000/(j-i))
}

W <- sum(((O-E)^2)/E)

criticial_value <- qchisq(0.95, length(E)-1)
if(W > criticial_value){
  print("The given distribution doesnt follow Discrete Uniform Distribution")
} else {
  print("The given distribution follows Discrete Uniform Distribution")
}

# c) Generation of Random Sample from a Binomial Distribution
p <- 0.67
generate_binomial_distribution <- function (p) {
  U <- runif(BIG_NUM)
  X <- rep(0, BIG_NUM)
  for (l in 1:BIG_NUM){
    H <- generate_bernoulli_rv(p)
    X <- X + H
  }
  return (X)
}

p=0.5
X<-generate_binomial_distribution(p)
hist(X, main = "Histogram of Binomial Distribution", xlab = "Value", ylab = "Frequency", col = "orange", border = "blue")

# Now we shall calculate the frequency of each element in X
O <- numeric(0)
E <- numeric(0)

# O and E calculations:
for(y in unique(X)){
  O <- append(O, length(X[X == y])) 
  E <- append(E, (1000 * (choose(1000,y)*(p^y)*(1-p)^(1000-y))))
}

# Caluclations of values
W <- sum(((O-E)^2)/E)
criticial_value <- qchisq(0.95, length(E)-1)

if(W > criticial_value){
  print("The given distribution doesnt follow Binomial Distribution")
} else {
  print("The given distribution follows Binomial Distribution")
}

# d) Generation of Random Sample from Geometric Distribution
p=0.3

generate_geometric_distribution <- function (p) {
  U <- runif(BIG_NUM)
  H <- floor(log(U)/log(1-p))
  return (H)
}

X <- generate_geometric_distribution(0.3)
hist(X, main = "Histogram of Geometric Distribution", xlab = "Value", ylab = "Frequency", col = "lightyellow", border = "blue")

O <- numeric(0)
E <- numeric(0)

# O and E calculations:

for(y in unique(X)){
  O <- append(O, length(X[X == y])) 
  E <- append(E, 1000*((1-p)^y)*p)
}

# Calculations of values
W <- sum(((O-E)^2)/E)
criticial_value <- qchisq(0.95, length(E)-1)

if(W > criticial_value){
  print("The given distribution doesnt follow Geometric Distribution")
} else {
  print("The given distribution follows Geometric Distribution")
}

# e) Generating random sample from a negative binomial distribution
r <- 11
p <- 0.67

generate_negative_binomial_distribution_rv <- function (r, p) {
  X <- rep(0, BIG_NUM)
  for (i in 1: r){
    H <- generate_geometric_distribution(p)
    X <- X + H
  }
  return(X)
}

X <- generate_negative_binomial_distribution_rv(r, p)
hist(X, main = "Histogram of Neagtive Binomial Distribution", xlab = "Value", ylab = "Frequency", col = "lightgreen", border = "blue")

# O and E calculations:
O <- numeric(0)
E <- numeric(0)
for(y in unique(X)){
  O <- append(O, length(X[X == y])) 
  E <- append(E, 1000*(choose(y+r-1, y)*((1-p)^y)*p^r))
}

# Calculations of values
W <- sum(((O-E)^2)/E)
criticial_value <- qchisq(0.95, length(E)-1)

if(W > criticial_value){
  print("The given distribution doesnt follow Negative Binomial Distribution")
} else {
  print("The given distribution follows Negative Binomial Distribution")
}

# f) Generation of random values under Poisson Distribution
n <- 1000
lambda <- 13

generate_each_entry_of_Poisson <- function (lambda) {
  a <- exp(-lambda)
  b <- 1
  i <- 0
  X <- numeric(0)
  
  while(1) {
    u <- runif(1, 0, 1)
    b <- b*u
    
    if(b < a){
      return(i)
    }
    i <- i+1
    }

  return (X)
}


####################### Application of Direct Inverse Transform Method #######################
generate_each_entry_of_dist <- function(pmf){
  i <- 0
  U <- runif(1,0,1)
  value <- 0
  
  while(1){
    value <- value + pmf(i)
    if(value>U){
      return(i)
    }
    i <- i+1;
  }
}
generate_sample <- function(n, pmf){
  X <- numeric(0)
  for(i in 1:n){
    X <- append(X, generate_each_entry_of_dist(pmf))
  }
  return(X)
}


perform_chi_sq_test <- function(X, pmf) {
  O <- numeric(0)
  E <- numeric(0)
  for(y in unique(X)){
    O <- append(O, length(X[X == y])) 
    E <- append(E, 1000*pmf(y))
  }
  
  # Calculations of values
  W <- sum(((O-E)^2)/E)
  criticial_value <- qchisq(0.95, length(E)-1)
  
  if(W > criticial_value){
    print("The given distribution doesnt follow Required Distribution")
  } else {
    print("The given distribution follows Required Distribution")
  }  
}

# a) Bernoulli Distribution
p<-0.67
pmf <- function (x){
  if(x==0){
    return(1-p)
  }else{
    return(p)
  }
}

# b) Discrete Uniform Distribution
i <- 10
j <- 30
pmf <- function(x){
  return(1/(j-i+1))
}
X <- generate_sample(1000, pmf)
perform_chi_sq_test(X, pmf)

# c) Binomial Distribution
pmf <- function(x){
  p <- 0.67
  if(x==0){
    return(0)
  }
  return(choose(1000,x)*p^x*(1-p)^(1000-x))
}

X <- generate_sample(1000, pmf)
perform_chi_sq_test(X, pmf)

# d) Geometric Distribution
pmf <- function(x){
  if(x==0){
    return(0)
  }
  p <- 0.67
  return((1-p)^(x-1)*p)
}

X <- generate_sample(1000, pmf)
perform_chi_sq_test(X, pmf)

# e) Negative BInomial Distribution
pmf <- function(x){
  r <- 49
  p <- 0.67
  if(x<r){
    return(0)
  }
  return(choose(x-1,r-1)*p^r*(1-p)^(x-r))
}

X <- generate_sample(1000, pmf)
perform_chi_sq_test(X, pmf)

# f) Poisson Distribution
pmf <- function(x){
  lambda <- 29
  return((lambda^x)*exp(-lambda)/factorial(x))
}

X <- generate_sample(1000, pmf)
perform_chi_sq_test(X, pmf)
