# Exercise - 1
# Using Builtin packages
# Your data
y <- c(2.6, 2.4, 17.32, 15.6, 16.12, 5.36, 6.19, 10.17, 2.62, 2.98, 6.92, 7.06)
x1 <- c(31, 31, 31.5, 31.5, 31.5, 30.5, 31.5, 30.5, 31, 30.5, 31, 30.5)
x2 <- c(21, 21, 24, 24, 24, 22, 22, 23, 21.5, 21.5, 22.5, 22.5)

data <- data.frame(y, x1, x2)
model <- lm(y ~ poly(x1, 2) + poly(x2, 2), data = data)
summary(model)


# Without using builtin Packages
# a) Finding the coefficients of the model
y <- c(2.6, 2.4, 17.32, 15.6, 16.12, 5.36, 6.19, 10.17, 2.62, 2.98, 6.92, 7.06)
x1 <- c(31, 31, 31.5, 31.5, 31.5, 30.5, 31.5, 30.5, 31, 30.5, 31, 30.5)
x2 <- c(21, 21, 24, 24, 24, 22, 22, 23, 21.5, 21.5, 22.5, 22.5)
X <- cbind(rep(1, length(x1)), x1, x2, x1^2, x2^2, x1*x2)

coefficients <- solve(t(X) %*% X) %*% t(X) %*% y
print(coefficients)

# b) Finding 95% confidence intervals for the coefficients
residuals <- y - X %*% coefficients
RSS <- sum(residuals^2)
df <- length(y) - ncol(X) 
sigma <- sqrt(RSS / df)

se <- sqrt(diag(sigma^2 * solve(t(X) %*% X)))

t_value <- qt(0.975, df)
confidence_intervals <- cbind(coefficients - t_value * se, coefficients + t_value * se)
print(confidence_intervals)

# c) Testing the hypothesis that the coefficients are equal to zero
t_values <- coefficients / se
p_values <- 2 * pt(abs(t_values), df = df, lower.tail = FALSE)

result <- data.frame(coefficients, t_values, p_values)
colnames(result) <- c("Coefficient", "t-value", "p-value")
print(result)

# d) Calculating the R-squared
fitted_values <- X %*% coefficients
mean_y <- mean(y)
TSS <- sum((y - mean_y)^2)
RSS <- sum((y - fitted_values)^2)
R_squared <- 1 - (RSS / TSS)

print(R_squared)


# Exercise - 2
# Using Builtin packages
X1 <- c(152, 183, 171, 165, 158, 161, 149, 158, 170, 153, 164, 190, 185)
X2 <- c(50, 20, 20, 30, 30, 50, 60, 50, 40, 55, 40, 40, 20)
Y <- c(120, 141, 124, 126, 117, 129, 123, 125, 132, 123, 132, 155, 147)

data <- data.frame(X1, X2, Y)
model <- lm(Y ~ X1 + X2, data = data)
summary(model)


# Without using builtin Packages
# a) Finding the coefficients of the model
X1 <- c(152, 183, 171, 165, 158, 161, 149, 158, 170, 153, 164, 190, 185)
X2 <- c(50, 20, 20, 30, 30, 50, 60, 50, 40, 55, 40, 40, 20)
Y <- c(120, 141, 124, 126, 117, 129, 123, 125, 132, 123, 132, 155, 147)

X <- cbind(rep(1, length(X1)), X1, X2)
Y <- matrix(Y)
coefficients <- solve(t(X) %*% X) %*% t(X) %*% Y
print(coefficients)

# b) ANOVA Testing
residuals <- Y - X %*% coefficients

RSS <- sum(residuals^2)
n <- length(Y)
p <- ncol(X) - 1 
df_residual <- n - p - 1

Y_mean <- mean(Y)
TSS <- sum((Y - Y_mean)^2)
F_statistic <- ((TSS - RSS) / p) / (RSS / df_residual)
p_value <- pf(F_statistic, df1 = p, df2 = df_residual, lower.tail = FALSE)

print(paste("F-statistic:", F_statistic))
print(paste("p-value:", p_value))


# Different thing
residuals <- Y - X %*% coefficients
n <- length(Y)
p <- ncol(X) - 1
df_residual <- n - p - 1
SE <- sqrt(diag(solve(t(X) %*% X) * sum(residuals^2) / df_residual))
t_values <- coefficients / SE
p_values <- 2 * pt(abs(t_values), df = df_residual, lower.tail = FALSE)
result <- data.frame(coefficients, p_values)
colnames(result) <- c("Coefficient", "p-value")
print(result)

# c) Finding 95% confidence intervals for the coefficients
residuals <- Y - X %*% coefficients

n <- length(Y)
p <- ncol(X) - 1
df_residual <- n - p - 1
SE <- sqrt(diag(solve(t(X) %*% X) * sum(residuals^2) / df_residual))
t_value <- qt(0.975, df = df_residual)
confidence_intervals <- cbind(coefficients - t_value * SE, coefficients + t_value * SE)
result <- data.frame(coefficients, confidence_intervals)
colnames(result) <- c("Coefficient", "Lower Bound (95% CI)", "Upper Bound (95% CI)")
print(result)

# d) Calculating the R-squared
fitted_values <- X %*% coefficients
mean_Y <- mean(Y)
TSS <- sum((Y - mean_Y)^2)
RSS <- sum((Y - fitted_values)^2)
R_squared <- 1 - (RSS / TSS)
print(R_squared)


# Exercise - 3
# Using the builtin functions
conversion <- c(49.0, 50.2, 50.5, 48.5, 47.5, 44.5, 28.0, 31.5, 34.5, 35.0, 38.0, 38.5, 15.0, 17.0, 20.5, 29.5)
temperature <- c(1300, 1300, 1300, 1300, 1300, 1300, 1200, 1200, 1200, 1200, 1200, 1200, 1100, 1100, 1100, 1100)
ratio <- c(7.5, 9.0, 11.0, 13.5, 17.0, 23.0, 5.3, 7.5, 11.0, 13.5, 17.0, 23.0, 5.3, 7.5, 11.0, 17.0)
contact_time <- c(0.012, 0.012, 0.0115, 0.013, 0.0135, 0.012, 0.04, 0.038, 0.032, 0.026, 0.034, 0.041, 0.084, 0.098, 0.092, 0.086)

data <- data.frame(conversion, temperature, ratio, contact_time)
full_model <- lm(conversion ~ . + I(temperature^2) + I(ratio^2) + I(contact_time^2) + temperature:ratio + temperature:contact_time + ratio:contact_time, data = data)

summary(full_model)
correlation_matrix <- cor(data)
print(correlation_matrix)
vif <- diag(solve(cor(data)))
print(vif)

# Without using builtin Packages


# Exercise - 4
temperature <- c(53, 57, 58, 63, 66, 67, 67, 67, 68, 69, 70, 70, 70, 70, 70, 75, 75, 76, 76, 76, 76, 78, 79, 81)
failure <- c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

data <- data.frame(temperature, failure)
model <- glm(failure ~ temperature, data = data, family = binomial(link = "logit"))
summary(model)

# Without using builtin Packages
logistic <- function(x) {
  return(1 / (1 + exp(-x)))
}

negative_log_likelihood <- function(beta, X, Y) {
  eta <- X %*% beta
  p <- logistic(eta)
  return(-sum(Y * log(p) + (1 - Y) * log(1 - p)))
}

beta <- rep(0, 2)
X <- cbind(rep(1, length(temperature)), temperature)

alpha <- 0.001
max_iter <- 1000

for (i in 1:max_iter) {
  eta <- X %*% beta
  p <- logistic(eta)
  gradient <- t(X) %*% (p - failure)
  beta <- beta - alpha * gradient
  
  if (max(abs(gradient)) < 1e-6) {
    break
  }
}

print(beta)


eta <- X %*% beta
p <- logistic(eta)

XtX_inv <- solve(t(X) %*% X)
se <- sqrt(diag(XtX_inv))
z_values <- beta / se
p_values <- 2 * (1 - pnorm(abs(z_values)))
result <- data.frame(Coefficient = beta, Z_value = z_values, P_value = p_values)
print(result)