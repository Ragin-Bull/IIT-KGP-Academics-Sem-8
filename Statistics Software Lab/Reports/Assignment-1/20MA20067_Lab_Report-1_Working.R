set.seed(200067)
generate_random_number <- function(n,fn){
  u <- runif(n,0,1)
  #random_sample <- theta - sigma*log(1-u)
  random_sample <- fn(u)
  return(random_sample)
}
chi_square_statistics <- function(random_sample,cdf_function,n){
  hi <- max(random_sample)
  lo <- min(random_sample)
  no_of_points <- length(random_sample)
  bucket_size <- (hi-lo)/n
  intervals <- c()
  E <- c()
  O <- c()
  for(i in 1:n){
    intervals[i] <- lo +(i-1)*bucket_size
  }
  intervals[n+1] <- hi
  for(i in 1:n){
    E[i] <- (cdf_function(intervals[i+1])-cdf_function(intervals[i]))*no_of_points
    O[i] <- length(random_sample[random_sample<=intervals[i+1]])-length(random_sample[random_sample<intervals[i]])
  }
  res <- sum((O-E)^2/E)
  critical_value <- qchisq(0.95,n)
  print("chi square statistics")
  print(res)
  print("critical value")
  print(critical_value)
  if(res>critical_value){
    print("Reject the null hypothesis")
  }else{
    print("Accepted the null hypothesis")
  }
}

# 1a
# generation of a random sample from an exponential distribution
# f(x) = (1/sigma) *exp(-(x-theta)/sigma), x>theta, sigma >0
theta <- 0
sigma <- 1.46
random_sample <- generate_random_number(1000,function(u){
  return(theta - sigma*log(1-u))
})
# test result
random_sample <- rexp(1000,1/sigma)
chi_square_statistics(random_sample,function(x){
  return(1-exp(-(x-theta)/sigma))
},10)

# 1b
# generation of a random sample from a Cauchy distribution
# f(x) = sigma/(pi*{sigma^2 + (x-u)^2})
u <- 0
sigma <- 10
random_sample <- generate_random_number(1000,function(u){
  return(u + sigma*tan(pi*(u-0.5)))
})
#test result
chi_square_statistics(random_sample,function(x){
  return((1/pi)*(atan((x-u)/sigma) + pi/2))
},10)

#1c
# generation of a random sample from a Double Exponential (Laplace) distribution
# f(x) = (1/2*sigma)*exp(-|x-u|/sigma)
u <- 0
sigma <- 10
random_sample <- generate_random_number(1000,function(u){
  o <- c()
  for(i in 1:length(u)){
    u1 <- runif(1,0,1)
    u2 <- runif(1,0,1)
    if(u1 <= 0.5){
      o[i] <- sigma*log(u2)
    }else{
      o[i] <- -sigma*log(u2)
    }
  }
  return(o)
})
#test result
chi_square_statistics(random_sample,function(x){
  if(x<u){
    return((1/2)*(exp((x-u)/sigma)))
  }else{
    return((1/2)*(2-exp(-(x-u)/sigma)))
  }
},10)
#1d
# generation of a Right Trapezoidal Distribution
# f(x) = {
#           a + 2(1-1)x, 0<=x<=1
#           0, elsewhere
#}
a <- 0.023
random_sample <- generate_random_number(1000,function(u){
  o <- c()
  for(i in 1:length(u)){
    if(u[i]<=a){
      o[i] <- runif(1,0,1)
    }else{
      o[i] <- max(runif(1,0,1),runif(1,0,1))
    }
  }
  return(o)
})
#test result
chi_square_statistics(random_sample,function(x){
  if(x<0){
    return(0)
  }else if(x>1){
    return(1)
  }
  return(a*x + (1-a)*x^2)
},10)
# Chi - Square Test using Simpsons 1/3 rd rule 
simpsons_rule_integral <- function(fn,a,b,n){
  
  
  # Calculate the step size
  h <- (b - a) / n
  
  # Calculate the integral using Simpson's 1/3 rule
  result <- fn(a) + fn(b)
  
  for (i in 1:n-1) {
    x_i <- a + i * h
    result <- result + 2 * fn(x_i) * (1 + i %% 2)
  }
  
  result <- result*h/3
  
  return(result)
}


chi_square_statistic_simpsons <- function(random_sample,pdf_function,n){
  
  hi <- max(random_sample)
  lo <- min(random_sample)
  no_of_points <- length(random_sample)
  bucket_size <- (hi-lo)/n
  intervals <- c()
  E <- c()
  O <- c()
  for(i in 1:n){
    intervals[i] <- lo +(i-1)*bucket_size
  }
  intervals[n+1] <- hi
  for(i in 1:n){
    cdf <- simpsons_rule_integral(pdf_function,intervals[i],intervals[i+1],1000)
    E[i] <- cdf*no_of_points
    O[i] <- length(random_sample[random_sample<=intervals[i+1]])-length(random_sample[random_sample<intervals[i]])
  }
  
  res <- sum((O-E)^2/E)
  critical_value <- qchisq(0.95,n-1)
  return(c(res,critical_value))
}

test_result <- function(random_sample,pdf_function,bucket_size){
  res <- chi_square_statistic_simpsons(random_sample,pdf_function,bucket_size)
  print("Chi Square Statistic")
  print(res[1])
  print("critical value")
  print(res[2])
  if(res[2]<=res[1]){
    print("Reject the null hypothesis")
  }else{
    print("Accepted the null hypothesis")
  }
}


#1e
# generation of standard normal distribution
# f(x) = (1/sqrt(2*pi))*exp(-x^2/2)
random_sample <- generate_random_number(1000,function(u){
  u2 <- runif(length(u),0,1)
  random_sample = ((-2*log(u))^(1/2))*cos(2*pi*u2)
  return(random_sample)
})

#test result
test_result(random_sample,function(x){
  return((1/sqrt(2*pi))*exp(-x^2/2))
},10)


#1f
# generation of Gamma Random Variables
#f(x) = (1/(gamma(alpha)*beta^alpha))*x^(alpha-1)*exp(-x/beta)
generate_random_gamma_lesser <- function(n,alpha){
  b <- 1+alpha/exp(1)
  res <- c()
  i <- 1
  while(i<=n){
    u1<-runif(1,0,1)
    P<-b*u1
    if(P<=1){
      Y<- P^(1/alpha)
      #print(Y)
      u2<-runif(1,0,1)
      if(u2<=exp(-Y)){
        res[i]<-Y
        i<-i+1
      }
    }else{
      Y<-(-log((b-P)/alpha))
      u2<-runif(1,0,1)
      #print(Y)
      if(u2<=Y^(alpha-1)){
        res[i]<-Y
        i<i+1
      }
    }
  }
  return(res)
}
generate_random_gamma_more <- function(n,alpha){
  a <- (2*alpha-1)^(-1/2)
  b <- alpha - log(4)
  q <- alpha + 1/a
  theta <- 4.5
  d <- 1 + log(theta)
  res <- c()
  i <- 1
  while(i<=n){
    u1 <- runif(1,0,1)
    u2 <- runif(1,0,1)
    v <- a*log(u1/(1-u1))
    Y <- alpha*exp(v)
    Z <- (u1^2)*u2
    W <- b + q*v - Y
    if(W + d - theta*Z >= 0){
      res[i] <- Y
      i <- i+1
    }else{
      if(W >= log(Z)){
        res[i] <- Y
        i <- i+1
      }
    }
  }
  return(res)
}

generate_random_gamma <- function(n,alpha){
  if(alpha<1){
    return(generate_random_gamma_lesser(n,alpha))
  }else{
    return(generate_random_gamma_more(n,alpha))
  }
}
alpha <- 12
random_sample <- generate_random_gamma(1000,alpha)
test_result(random_sample,function(x){
  return(1/(gamma(alpha))*x^(alpha-1)*exp(-x))
},10)