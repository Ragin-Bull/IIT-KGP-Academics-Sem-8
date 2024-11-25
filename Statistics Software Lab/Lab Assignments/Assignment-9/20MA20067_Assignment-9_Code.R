# Exercise - 1
X <- c(20, 20, 30, 30, 30, 40, 40, 50, 50, 60)
y <- c(16.3, 26.7, 39.2, 63.5, 51.3, 98.4, 65.7, 104.1, 155.6, 217.2)

# a) Scatter plot
plot(X, y, xlab = "X", ylab = "y", main = "Scatter plot")

# b) Fitting a linear regression model
U <- sqrt(y)
meanX <- sum(X) / length(X)
meanU <- sum(U) / length(U)

beta1 <- sum((X - meanX) * (U - meanU)) / sum((X - meanX)^2)
beta0 <- meanU - beta1 * meanX
UPredicted <- beta0 + beta1 * X
cat("Intercept (beta0):", beta0, "\n")
cat("Slope (beta1):", beta1, "\n")

plot(X, U, xlab = "Initial Speed (X)", ylab = "Square Root of Stopping Distance (U)", main = "Linear Regression")
abline(beta0, beta1, col = "red")

# c) Confidence Intervals for beta0, beta1 and sigma squared
n <- length(X)
df <- n - 2
RSS <- sum((U - UPredicted)^2)
sigma2 <- RSS / df
SESlope <- sqrt(sigma2 / sum((X - meanX)^2))
SEIntercept <- sqrt(sigma2 * (1 / n + meanX^2 / sum((X - meanX)^2)))

t <- qt(0.975, df)
CIForSlope <- c(beta1 - t * SESlope, beta1 + t* SESlope)
CIForIntercept <- c(beta0 - t * SEIntercept, beta0 + t * SEIntercept)

chiLower <- qchisq(0.025, df)
chiUpper <- qchisq(0.975, df)

CISigma2 <- c((df * sigma2) / chiUpper, (df * sigma2) / chiLower)

cat("95% Confidence Interval for Slope (beta1):", CIForSlope, "\n")
cat("95% Confidence Interval for Intercept (beta0):", CIForIntercept, "\n")
cat("95% Confidence Interval for Sigma^2:", CISigma2, "\n")

# d) Test of Significance for beta1 and beta0
# For Beta0:
alpha <- 0.05
SSE <- sum((U - UPredicted)^2)
StandardErrorBeta0 <- sqrt((SSE / df) * ((1 / n) + (meanX^2 / sum((X - meanX)^2))))
tObserved <- beta0 / StandardErrorBeta0
pValue <- 2 * pt(abs(tObserved), n-2, lower.tail = FALSE)

if (pValue < alpha) {
  cat("Reject null hypothesis: Beta0 is significant.\n")
} else {
  cat("Accept the null hypothesis: Beta0 is not significant.\n")
}

# For Beta1
alpha <- 0.05
df <- n-2
SSE <- sum((U - UPredicted)^2)
StandardErrorBeta1 <- sqrt((SSE / df) / sum((X - meanX)^2))
tObserved <- beta1 / StandardErrorBeta1
pValue <- 2 * pt(abs(tObserved), n-2, lower.tail = FALSE)

if (pValue < alpha) {
  cat("Reject null hypothesis: Beta1 is significant.\n")
} else {
  cat("Accept the null hypothesis: Beta1 is not significant.\n")
}

# e) Confidence Interval for mean of y given X
X0 <- 35
UPredictedForX0 <- beta0 + beta1 * X0
SEPred <- sqrt(sigma2 * ( 1 / n + (X0 - meanX)^2 / sum((X - meanX)^2)))
df <- length(X) - 2
t <- qt(0.975, df)

CIForU <- c(UPredictedForX0- t * SEPred, UPredictedForX0 + t * SEPred)
CIForY <- CIForU^2
cat("95% Confidence Interval for the expected stopping distance when the initial speed is 35:", CIForY, "\n")

# f) Prediction Interval for a new observation of y given X
X0 <- 35
UPredictedForX0 <- beta0 + beta1 * X0
SEPred <- sqrt(sigma2 * (1 + 1 / n + (X0 - meanX)^2 / sum((X - meanX)^2)))
df <- length(X) - 2
t <- qt(0.975, df)
PIForU <- c(UPredictedForX0- t * SEPred, UPredictedForX0 + t * SEPred)

PIForY <- PIForU^2
cat("95% Confidence Interval for the expected stopping distance when the initial speed is 35:", PIForY, "\n")

# g) Lack of fit analysis
n <- length(X)
m <- length(unique(X))
SSE <- sum((U - beta0 - beta1*X)^2)

SSPE <- 0
for(i in 1:m){
  SSPE <- SSPE + sum((sqrt(y[X==unique(X)[i]]) - mean(sqrt(y[X==unique(X)[i]])))^2)
}

SSLOF <- SSE - SSPE

fStat <- (SSLOF/(m-2))/(SSPE/(n-m))
fCritical <- qf(0.95,m-2,n-m)

if(fStat > fCritical){
  cat("The model caught lacking","\n")
}else{
  cat("The model doesnt not lack in fitting the data","\n")
}

# Exercise - 2
n <- 20
X <- c(1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 5, 5, 5, 5, 5, 10, 10, 10, 10, 10)
y <- c(1.1, 0.7, 1.8, 0.4, 1.4, 3.0, 4.5, 4.9, 4.4, 7.3, 7.3, 8.2, 6.2, 13.1, 12.0, 12.6, 13.2, 13.1, 18.7, 19.7)
plot(X, y, xlab = "X", ylab = "y", main = "Scatter plot")

# Fitting a linear regression model
meanX <- sum(X) / length(X)
meanY <- sum(y) / length(y)

beta1 <- sum((X - meanX) * (y - meanY)) / sum((X - meanX)^2)
beta0 <- meanY - beta1 * meanX
YPredicted <- beta0 + beta1 * X
cat("Intercept (beta0):", beta0, "\n")
cat("Slope (beta1):", beta1, "\n")

plot(X, y, xlab = "Known Concentration (X)", ylab = "Computed Concentration (Y)", main = "Linear Regression")
abline(beta0, beta1, col = "red")

# Lack of fit test
m <- length(unique(X))
SSE <- sum((y - beta0 - beta1*X)^2)

SSPE <- 0
for(i in 1:m){
  SSPE <- SSPE + sum(((y[X==unique(X)[i]]) - mean((y[X==unique(X)[i]])))^2)
}

SSLOF <- SSE - SSPE

fStat <- (SSLOF/(m-2))/(SSPE/(n-m))
fCritical <- qf(0.95,m-2,n-m)

if(fStat > fCritical){
  cat("The model caught lacking","\n")
}else{
  cat("The model doesnt not lack in fitting the data","\n")
}

# Correlation Coefficient
correlation <- (n * sum(X * y) - sum(X) * sum(y)) / sqrt((n * sum(X^2) - sum(X)^2) * (n * sum(y^2) - sum(y)^2))
cat("Correlation Coefficient:", correlation, "\n")

# Test of significance for beta1 and beta0
# For Beta0:
alpha <- 0.05
df <- n-2
SSE <- sum((y - YPredicted)^2)
StandardErrorBeta0 <- sqrt((SSE / df) * ((1 / n) + (meanX^2 / sum((X - meanX)^2))))
tObserved <- beta0 / StandardErrorBeta0
pValue <- 2 * pt(abs(tObserved), n-2, lower.tail = FALSE)

if (pValue < alpha) {
  cat("Reject null hypothesis: Beta0 is significant.\n")
} else {
  cat("Accept the null hypothesis: Beta0 is not significant.\n")
}

# For Beta1
alpha <- 0.05
SSE <- sum((y - YPredicted)^2)
StandardErrorBeta1 <- sqrt((SSE / df) / sum((X - meanX)^2))
tObserved <- beta1 / StandardErrorBeta1
pValue <- 2 * pt(abs(tObserved), n-2, lower.tail = FALSE)

if (pValue < alpha) {
  cat("Reject null hypothesis: Beta1 is significant.\n")
} else {
  cat("Accept the null hypothesis: Beta1 is not significant.\n")
}

# Confidence Intervals for all the parameters
sigma2 <- SSE / df
SESlope <- sqrt(sigma2 / sum((X - meanX)^2))
SEIntercept <- sqrt(sigma2 * (1 / n + meanX^2 / sum((X - meanX)^2)))

t <- qt(0.975, df)
CIForSlope <- c(beta1 - t * SESlope, beta1 + t* SESlope)
CIForIntercept <- c(beta0 - t * SEIntercept, beta0 + t * SEIntercept)

chiLower <- qchisq(0.025, df)
chiUpper <- qchisq(0.975, df)

CISigma2 <- c((df * sigma2) / chiUpper, (df * sigma2) / chiLower)

cat("95% Confidence Interval for Slope (beta1):", CIForSlope, "\n")
cat("95% Confidence Interval for Intercept (beta0):", CIForIntercept, "\n")
cat("95% Confidence Interval for Sigma^2:", CISigma2, "\n")

# Exercise - 3
X <- c(1.00, 1.05, 1.10, 1.15, 1.21, 1.32, 1.44, 1.53, 1.63, 1.79, 1.91, 2.03, 2.12, 2.26, 2.37, 2.51)
y <- c(3.71, 3.81, 3.86, 3.93, 3.96, 4.20, 4.34, 4.51, 4.73, 5.35, 5.74, 6.14, 6.51, 6.98, 7.44, 7.76)

# a) Fitting a second degree polynomial regression model
XSquared <- X^2
designMatrix <- cbind(rep(1, length(X)), X, XSquared)

productMatrix <- (t(designMatrix)) %*% designMatrix
productY <- (t(designMatrix)) %*% y
coefficients <- solve(productMatrix, productY)
yPredicted <- coefficients[1] + coefficients[2] * X + coefficients[3] * X^2
cat("Coefficients:\n")
cat("beta_0: ", coefficients[1], "\nbeta_1: ", coefficients[2], "\nbeta_2: ", coefficients[3], "\n")


# Plotting of the curve
plot(X, y, pch=16, col="blue", xlab="X", ylab="Y", main="Scatter plot with regression curve")
curve(coefficients[1] * 1 + coefficients[2] * x + coefficients[3] * x^2, add = TRUE, col = "red", lwd = 2)

residuals <- y - (designMatrix %*% coefficients)
df <- length(y) - ncol(designMatrix)
standardErr <- sqrt(diag(solve(productMatrix)) * sum(residuals^2)/df)

alpha <- 0.05
tObserved <- coefficients / standardErr 
t <- qt(1 - alpha/2, df)
lowerBound <- coefficients - t * standardErr
upperBound <- coefficients + t * standardErr


cat("\n95% Confidence Intervals:\n")
for (i in 1:length(coefficients)) {
  cat("beta_", i-1, ": [", lowerBound[i], ", ", upperBound[i], "]\n")
}


# c) Test for significance of coefficients
# Test for significance of each coefficient (using if-else)
for (i in 1:length(coefficients)) {
  if (abs(tObserved[i]) > t) {
    cat("beta_", i-1, "is Significant (p < 0.05)\n")
  } else {
    cat("beta_", i-1, "is Not Significant (p >= 0.05)\n")
  }
}


# d) Computation of R squared
SSRes <- sum((y - yPredicted)^2)

meanY <- mean(y)
SSTotal <- sum((y - meanY)^2)
R2 <- 1 - (SSRes / SSTotal)

cat("Coefficient of determination (R^2):", R2, "\n")

# Exercise - 4
X <- c(1, 2, 3, 4, 5, 6)
y <- c(1.60, 4.50, 13.80, 40.20, 125.00, 300.00)
plot(X, y, pch = 16, col = "blue", xlab = "x", ylab = "y", main = "Scatter plot with fitted curve")
n <- length(X)

Y <- log(y)
# We note that beta0 is log a and b is beta1
meanX <- sum(X) / length(X)
meanY <- sum(Y) / length(Y)

beta1 <- sum((X - meanX) * (Y - meanY)) / sum((X - meanX)^2)
beta0 <- meanY - beta1 * meanX
a <- exp(beta0)

yPredicted <- beta0 + beta1 * X

# Lack of Fitness Test
SSLF <- sum((Y - yPredicted)^2)
SSE <- sum((Y - meanY)^2)

df1 <- n - 2
df2 <- n - 3
F_statistic <- (SSLF / df1) / (SSE / df2)

alpha <- 0.05
critical_value <- qf(1 - alpha, df1, df2)

cat("F-statistic:", F_statistic, "\n")
cat("Critical value:", critical_value, "\n")

if (F_statistic > critical_value) {
  cat("Reject null hypothesis: Lack of fit is significant.\n")
} else {
  cat("Accept the null hypothesis: Lack of fit is not significant.\n")
}

# Plotting
curve(a * exp(beta1 * x), from = min(X), to = max(X), col = "red", add = TRUE)

# Significance Testing for Parameters
# For Beta0:
alpha <- 0.05
SSE <- sum((Y - yPredicted)^2)
StandardErrorBeta0 <- sqrt((SSE / df2) * ((1 / n) + (meanX^2 / sum((X - meanX)^2))))
tObserved <- beta0 / StandardErrorBeta0
pValue <- 2 * pt(abs(tObserved), n-2, lower.tail = FALSE)

if (pValue < alpha) {
  cat("Reject null hypothesis: Beta0 is significant.\n")
} else {
  cat("Accept the null hypothesis: Beta0 is not significant.\n")
}

# For Beta1
alpha <- 0.05
df <- n-2
SSE <- sum((Y - yPredicted)^2)
StandardErrorBeta1 <- sqrt((SSE / df) / sum((X - meanX)^2))
tObserved <- beta1 / StandardErrorBeta1
pValue <- 2 * pt(abs(tObserved), n-2, lower.tail = FALSE)

if (pValue < alpha) {
  cat("Reject null hypothesis: Beta1 is significant.\n")
} else {
  cat("Accept the null hypothesis: Beta1 is not significant.\n")
}


# Exercise - 5
X <- c(2, 3, 4, 5, 6)
y <- c(144.0, 172.80, 207.40, 248.50, 298.50)
plot(X, y, pch = 16, col = "blue", xlab = "X", ylab = "Y", main = "Scatter plot with fitted curve")

Y <- log(y)
n <- length(X)


# We note that beta0 is log a and b is exp(beta1)
meanX <- sum(X) / length(X)
meanY <- sum(Y) / length(Y)

beta1 <- sum((X - meanX) * (Y - meanY)) / sum((X - meanX)^2)
beta0 <- meanY - beta1 * meanX
a <- exp(beta0)
b <- exp(beta1)

yPredicted <- beta0 + beta1 * X

# Lack of Fitness Test
SSLF <- sum((Y - yPredicted)^2)
SSE <- sum((Y - meanY)^2)

df1 <- n - 2
df2 <- n - 3
F_statistic <- (SSLF / df1) / (SSE / df2)

alpha <- 0.05
critical_value <- qf(1 - alpha, df1, df2)

cat("F-statistic:", F_statistic, "\n")
cat("Critical value:", critical_value, "\n")

if (F_statistic > critical_value) {
  cat("Reject null hypothesis: Lack of fit is significant.\n")
} else {
  cat("Accept the null hypothesis: Lack of fit is not significant.\n")
}

# Significance Testing for Parameters
# For Beta0:
alpha <- 0.05
SSE <- sum((Y - yPredicted)^2)
StandardErrorBeta0 <- sqrt((SSE / df1) * ((1 / n) + (meanX^2 / sum((X - meanX)^2))))
tObserved <- beta0 / StandardErrorBeta0
pValue <- 2 * pt(abs(tObserved), n-2, lower.tail = FALSE)

if (pValue < alpha) {
  cat("Reject null hypothesis: Beta0 is significant.\n")
} else {
  cat("Accept the null hypothesis: Beta0 is not significant.\n")
}

# For Beta1
alpha <- 0.05
SSE <- sum((Y - yPredicted)^2)
StandardErrorBeta1 <- sqrt((SSE / df1) / sum((X - meanX)^2))
tObserved <- beta1 / StandardErrorBeta1
pValue <- 2 * pt(abs(tObserved), n-2, lower.tail = FALSE)

if (pValue < alpha) {
  cat("Reject null hypothesis: Beta1 is significant.\n")
} else {
  cat("Accept the null hypothesis: Beta1 is not significant.\n")
}

# Plotting
curve(a * exp(b * x), from = min(X), to = max(X), col = "red", add = TRUE)