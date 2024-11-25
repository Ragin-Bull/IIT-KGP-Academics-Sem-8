# a) Generating random permutations
set.seed(67)

generate_random_permutation <- function (n, p) {
  k <- n  
  
  while(k>=1){
    u <- runif(1, 0, 1)
    I <- floor(k*u) + 1
    temp <- p[I]
    p[I] <- p[k]
    p[k] <- temp
    k <- k-1  
  }
  
  return(p)
}

n <- 10
p <- seq(1, n, 1)
cat("Initial Permutation is: \n")
cat(p)
q <- generate_random_permutation(n, p)
cat("\nFinal Permutation is: \n")
cat(q, "\n")
plot(q, col="red", xlab="Values", ylab="Indices")


# b) Generation of a stationery Poisson Process
generate_stationery_poisson <- function(T, S, lambda){
  t <- 0
  I <- 0
  
  while(t < T){
    u <- runif(1, 0, 1)
    t <- t - (1/lambda)*log(u)
    if (t>T)
      break
    I <- I + 1
    S[I] <- t
  }
  return(S)  
}

T <- 10
S <- c()
lambda <- 5

S <- generate_stationery_poisson(T, S, lambda)
cat("The resultant array is as follows: ", S)
plot(S, col="brown", xlab="Values", ylab="Indices", pch=16)

# c)  
# Algorithm - 1 : Generation of a non-stationery Poisson Process
intensity_function <- function (t, lambda) {
  return(lambda*exp(-t))
}

generate_non_stationery_poisson_algo_1 <- function(T, S, lambda_upper_bound, fn){
  t <- 0
  I <- 0
  
  while(t < T){
    u <- runif(1, 0, 1)
    t <- t - (1/lambda)*log(u)
    if (t>T)
      break
    if(u <= fn(t, lambda)/lambda_upper_bound){
      I <- I + 1
      S[I] <- t      
    }
  }
  return(S)  
}

T <- 1000
S <- c()
lambda <- 40

S <- generate_non_stationery_poisson_algo_1(T, S, lambda, intensity_function)
cat("The resultant array is as follows: ", S)
plot(S ,  col="blue", xlab="Values", ylab="Indices", pch=16)


# Algorithm - 2 : Generation of a non-stationery Poisson Process
intensity_function <- function (t, lambda) {
  return(lambda*exp(-t))
}

generate_non_stationary_poisson_algo2 <- function(fn, intervals, l, k){
  t <- 0
  J <- 1
  I <- 0
  S <- numeric(0)
  flag <- FALSE
  
  while(flag == FALSE){
    u1 <- runif(1)
    X <- (-(1/l[J])*log(u1))
    while(TRUE){
      if(t+X <= intervals[J]){
        t<- t+X
        u2 <- runif(1)
        if(u2 <= fn(t,l[J])/l[J]){
          I <- I+1
          S[I] <- t
        }
        break
      }
      if(J == k+1){
        flag <- TRUE
        break
      }
      X <- (X-intervals[J]+t)*l[J]/l[J+1]
      J <- J + 1
    }
  }
  return(S)
}

intervals <- seq(10,100,5)
k <- length(intervals)-1
l <- numeric(0)

for(i in 0:k+1){
  l[i] = runif(1,1,50)
}
getAns <- generate_non_stationary_poisson_algo2(intensity_function, intervals, l, k)
print(getAns)
plot(getAns,  col="orange", xlab="Values", ylab="Indices", pch=16)