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
residuals <- U - UPredicted
df <- n - 2
RSS <- sum(residuals^2)

SEBeta1 <- sqrt(RSS / ((n - 2) * sum((X - meanX)^2)))
SEBeta0 <- sqrt(RSS * ((1 / n) + (meanX^2 / sum((X - meanX)^2))))

tBeta1 <- beta1 / SEBeta1
tBeta0 <- beta0 / SEBeta0

t <- qt(0.975, df = df)
pBeta1 <- 2 * pt(abs(tBeta1), df)
significantBeta1 <- abs(tBeta1) > t
pBeta0 <- 2 * pt(abs(tBeta0), df)
significantBeta0 <- abs(tBeta0) > t

cat("Slope coefficient (beta1):\n")
cat("  t-value:", tBeta1, "\n")
cat("  p-value:", pBeta1, "\n")
cat("  Is coefficient significant?:", significantBeta1, "\n")

cat("\nIntercept coefficient (beta0):\n")
cat("  t-value:", tBeta0, "\n")
cat("  p-value:", pBeta0, "\n")
cat("  Is coefficient significant?:", significantBeta0, "\n")

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
dfTotal <- n - 1
dfReg <- 2

PRESS <- sum((U - mean(U))^2) - RSS
dfError <- dfTotal - dfReg
FStat <- (PRESS / dfError) / (RSS / dfReg)
FCritical <- qf(0.95, dfError, dfReg)

lackOfSignificant <- FStat > FCritical

cat("Error Sum of Squares (PRESS):", PRESS, "\n")
cat("F-statistic:", FStat, "\n")
cat("Critical value for F-distribution:",FCritical, "\n")
cat("Is lack of fit significant (True/False)?:",lackOfSignificant, "\n")

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

plot(X, y, xlab = "Known Concentration (Y)", ylab = "Computed Concentration (Y)", main = "Linear Regression")
abline(beta0, beta1, col = "red")

cat("No the scatter plot does not suggest a linear relationship between X and y. The relationship is non-linear.\n")

# Correlation Coefficient
correlation <- (n * sum(X * y) - sum(X) * sum(y)) / sqrt((n * sum(X^2) - sum(X)^2) * (n * sum(y^2) - sum(y)^2))
cat("Correlation Coefficient:", correlation, "\n")

# Test of significance for beta1 and beta0
residuals <- y - YPredicted
df <- n - 2
RSS <- sum(residuals^2)

SEBeta1 <- sqrt(RSS / ((n - 2) * sum((X - meanX)^2)))
SEBeta0 <- sqrt(RSS * ((1 / n) + (meanX^2 / sum((X - meanX)^2))))

tBeta1 <- beta1 / SEBeta1
tBeta0 <- beta0 / SEBeta0

t <- qt(0.975, df = df)
pBeta1 <- 2 * pt(abs(tBeta1), df)
significantBeta1 <- abs(tBeta1) > t
pBeta0 <- 2 * pt(abs(tBeta0), df)
significantBeta0 <- abs(tBeta0) > t

cat("Slope coefficient (beta1):\n")
cat("  t-value:", tBeta1, "\n")
cat("  p-value:", pBeta1, "\n")
cat("  Is coefficient significant?:", significantBeta1, "\n")

cat("\nIntercept coefficient (beta0):\n")
cat("  t-value:", tBeta0, "\n")
cat("  p-value:", pBeta0, "\n")
cat("  Is coefficient significant?:", significantBeta0, "\n")

# Confidence Intervals for all the parameters
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

# Exercise - 3
X <- c(1.00, 1.05, 1.10, 1.15, 1.21, 1.32, 1.44, 1.53, 1.63, 1.79, 1.91, 2.03, 2.12, 2.26, 2.37, 2.51)
y <- c(3.71, 3.81, 3.86, 3.93, 3.96, 4.20, 4.34, 4.51, 4.73, 5.35, 5.74, 6.14, 6.51, 6.98, 7.44, 7.76)

# a) FItting a second degree polynomial regression model

polynomialRegression <- function(X, Y, degree) {
  n <- length(X)
  
  XPowers <- matrix(rep(X, degree + 1), ncol = degree + 1)
  
  for (i in 1 : (degree + 1)) {
    XPowers[,i] <- XPowers[,i]^(i - 1)
  }
  
  A <- t(XPowers) %*% XPowers
  B <- t(XPowers) %*% Y
  coefficients <- solve(A, B)
  
  return(coefficients)
}


coefficients <- polynomialRegression(X, y, degree = 2)
print(coefficients)

# b) Confidence Intervals for the parameters

predictY <- function(X, coefficients) {
  return(coefficients[1] * X + coefficients[2] * X^2 + coefficients[3])
}

predictedY <- predictY(X, coefficients)
residuals <- y - predictedY

n <- length(X)
df <- n - length(coefficients)
sigmaHat <- sum(residuals^2) / df

XDesign <- cbind(X^2, X, rep(1, length(X)))
XtXInv <- solve(t(XDesign) %*% XDesign)
cov <- sigmaHat * XtXInv

t <- qt(0.975, df)

coefficientsSE <- sqrt(diag(cov))
lowerBound <- coefficients - t * coefficientsSE
upperBound <- coefficients + t * coefficientsSE

for (i in 1:length(coefficients)) {
  cat("Coefficient", i, "95% confidence interval:", lowerBound[i], " to ", upperBound[i], "\n")
}

sigma2Lower <- (df * sigmaHat) / qchisq(0.975, df)
sigma2Upper <- (df * sigmaHat) / qchisq(0.025, df)
cat("Sigma^2 95% confidence interval:", sigma2Lower, " to ", sigma2Upper, "\n")

# c) Test of significance for the parameters
tVals <- coefficients / sqrt(diag(cov))

for (i in 1:length(coefficients)) {
  pValue <- 2 * pt(abs(tVals[i]), df, lower.tail = FALSE)
  significant <- ifelse(abs(tVals[i]) > t, "Yes", "No")
  
  cat("Coefficient", i, ":\n")
  cat("   t-value:", tVals[i], "\n")
  cat("   p-value:", pValue, "\n")
  cat("   Significant at 0.05 level?", significant, "\n")
}

# d) Computation of R squared
SSRes <- sum((y - predictedY)^2)

meanY <- mean(y)
SSTotal <- sum((y - meanY)^2)

R2 <- 1 - (SSRes / SSTotal)

# Print R^2
cat("Coefficient of determination (R^2):", R2, "\n")

# Plotting of the curve
plot(X, y, pch=16, col="blue", xlab="X", ylab="Y", main="Scatter plot with regression curve")
curve(coefficients[1] * 1 + coefficients[2] * x + coefficients[3] * x^2, add = TRUE, col = "red", lwd = 2)

# Exercise - 4
# Given data
x <- c(1, 2, 3, 4, 5, 6)
y <- c(1.60, 4.50, 13.80, 40.20, 125.00, 300.00)

# Define the nonlinear model function
nonlinear_model <- function(x, a, b) {
  return(a * exp(b * x))
}

# Fit the nonlinear model to the data
fit <- nls(y ~ nonlinear_model(x, a, b), start = list(a = 1, b = 1))

# Get the coefficients of the fitted model
coefficients <- coef(fit)
a <- coefficients["a"]
b <- coefficients["b"]

# Draw scatter plot
plot(x, y, pch = 16, col = "blue", xlab = "x", ylab = "y", main = "Scatter plot with fitted curve")

# Add fitted curve to the plot
curve(a * exp(b * x), from = min(x), to = max(x), col = "red", add = TRUE)

# Test for significance of coefficients
summary(fit)

# Exercise - 5
x <- c(2, 3, 4, 5, 6)
y <- c(144.0, 172.80, 207.40, 248.50, 298.50)

# Define the model function
model <- function(x, a, b) {
  return(a * b^x)
}

# Define the error function
error <- function(parameters) {
  a <- parameters[1]
  b <- parameters[2]
  predicted <- model(x, a, b)
  residuals <- y - predicted
  return(sum(residuals^2))
}

# Initial guesses for parameters
initial_parameters <- c(100, 1.1)

# Minimize the error function using optim
fit <- optim(initial_parameters, error)

# Extract fitted parameters
a <- fit$par[1]
b <- fit$par[2]

# Calculate residuals
predicted_values <- model(x, a, b)
residuals <- y - predicted_values

# Degrees of freedom
df <- length(x) - length(initial_parameters)

# Residual sum of squares
RSS <- sum(residuals^2)

# Residual standard error
RSE <- sqrt(RSS / df)

# Define the design matrix with one column removed
H <- model.matrix(~x)
H <- H[, -4]

# Calculate standard errors
XtX_inv <- solve(t(H) %*% H)
SE <- sqrt(diag(RSS * XtX_inv / df))

# Calculate t-values
t_values <- fit$par / SE

# Calculate p-values
p_values <- 2 * pt(abs(t_values), df)

# Display results
results <- data.frame(coefficients = fit$par, standard_errors = SE, t_values = t_values, p_values = p_values)
print(results)
