# Install this package:
install.packages('corrplot')

# Exercise - 1
# a) Finding the coefficients of the model
y <- c(2.6, 2.4, 17.32, 15.6, 16.12, 5.36, 6.19, 10.17, 2.62, 2.98, 6.92, 7.06)
x1 <- c(31, 31, 31.5, 31.5, 31.5, 30.5, 31.5, 30.5, 31, 30.5, 31, 30.5)
x2 <- c(21, 21, 24, 24, 24, 22, 22, 23, 21.5, 21.5, 22.5, 22.5)
X <- cbind(rep(1, length(x1)), x1, x2, x1^2, x2^2, x1*x2)

coefficients <- solve(t(X) %*% X) %*% t(X) %*% y
for (i in 1:length(coefficients)) {
  print(paste("The coefficient", i, "is", coefficients[i]))
}

# b) Finding 95% confidence intervals for the coefficients
residuals <- y - X %*% coefficients
RSS <- sum(residuals^2)
df <- length(y) - ncol(X) 
sigma <- sqrt(RSS / df)

se <- sqrt(diag(sigma^2 * solve(t(X) %*% X)))

t <- qt(0.975, df)
confidence_intervals <- cbind(coefficients - t * se, coefficients + t * se)
for (i in 1:length(coefficients)) {
  print(paste("The 95% confidence interval for the coefficient", i, "is", confidence_intervals[i, 1], "to", confidence_intervals[i, 2]))
}

# c) Testing the hypothesis that the coefficients are equal to zero
ts <- coefficients / se
pValues <- 2 * pt(abs(ts), df = df, lower.tail = FALSE)

for(i in 1:length(coefficients)) {
  if(pValues[i] < 0.05) {
    print(paste("The coefficient", i, "is significant"))
  } else {
    print(paste("The coefficient", i, "is not significant"))
  }
}

# d) Calculating the R-squared
yPredicted <- X %*% coefficients
mean_y <- mean(y)
TSS <- sum((y - mean_y)^2)
RSS <- sum((y - yPredicted)^2)
R_squared <- 1 - (RSS / TSS)

cat("The R-squared value is", R_squared, "\n")


# Exercise - 2
# a) Finding the coefficients of the model
X1 <- c(152, 183, 171, 165, 158, 161, 149, 158, 170, 153, 164, 190, 185)
X2 <- c(50, 20, 20, 30, 30, 50, 60, 50, 40, 55, 40, 40, 20)
Y <- c(120, 141, 124, 126, 117, 129, 123, 125, 132, 123, 132, 155, 147)

X <- cbind(rep(1, length(X1)), X1, X2)
Y <- matrix(Y)
coefficients <- solve(t(X) %*% X) %*% t(X) %*% Y
for(i in 1:length(coefficients)) {
  print(paste("The coefficient", i, "is", coefficients[i]))
}

# b) ANOVA Testing
residuals <- Y - X %*% coefficients
n <- length(Y)
p <- ncol(X) - 1
df_residual <- n - p - 1
SE <- sqrt(diag(solve(t(X) %*% X) * sum(residuals^2) / df_residual))
ts <- coefficients / SE
pValues <- 2 * pt(abs(ts), df = df_residual, lower.tail = FALSE)

for(i in 1:length(coefficients)) {
  if(pValues[i] < 0.05) {
    print(paste("The coefficient", i, "is significant"))
  } else {
    print(paste("The coefficient", i, "is not significant"))
  }
}

# c) Finding 95% confidence intervals for the coefficients
residuals <- Y - X %*% coefficients

n <- length(Y)
p <- ncol(X) - 1
df_residual <- n - p - 1
SE <- sqrt(diag(solve(t(X) %*% X) * sum(residuals^2) / df_residual))
t <- qt(0.975, df = df_residual)
confidence_intervals <- cbind(coefficients - t * SE, coefficients + t * SE)
result <- data.frame(coefficients, confidence_intervals)
colnames(result) <- c("Coefficient", "Lower Bound (95% CI)", "Upper Bound (95% CI)")
print(result)

# d) Calculating the R-squared
yPredicted <- X %*% coefficients
mean_Y <- mean(Y)
TSS <- sum((Y - mean_Y)^2)
RSS <- sum((Y - yPredicted)^2)
R_squared <- 1 - (RSS / TSS)
cat("The R-squared value is", R_squared, "\n")


# Exercise - 3
Conversion <- c(49.0, 50.2, 50.5, 48.5, 47.5, 44.5, 28.0, 31.5, 34.5, 35.0, 38.0, 38.5, 15.0, 17.0, 20.5, 29.5)
Reactor_Temperature <- c(1300, 1300, 1300, 1300, 1300, 1300, 1200, 1200, 1200, 1200, 1200, 1200, 1100, 1100, 1100, 1100)
Contact_Time <- c(0.0120, 0.0120, 0.0115, 0.0130, 0.0135, 0.0120, 0.0400, 0.0380, 0.0320, 0.0260, 0.0340, 0.0410, 0.0840, 0.0980, 0.0920, 0.0860)
Ratio_H2_to_n_Heptane <- c(7.5, 9.0, 11.0, 13.5, 17.0, 23.0, 5.3, 7.5, 11.0, 13.5, 17.0, 23.0, 5.3, 7.5, 11.0, 17.0)

Reactor_Temperature_sq <- Reactor_Temperature^2
Ratio_H2_to_n_Heptane_sq <- Ratio_H2_to_n_Heptane^2
Contact_Time_sq <- Contact_Time^2

X <- cbind(1, Reactor_Temperature, Reactor_Temperature_sq, Ratio_H2_to_n_Heptane, Ratio_H2_to_n_Heptane_sq, Contact_Time, Contact_Time_sq)

model <- lm(Conversion ~ ., data = as.data.frame(X))
summary(model)

data <- data.frame(
  Reactor_Temperature = Reactor_Temperature,
  Ratio_H2_to_n_Heptane = Ratio_H2_to_n_Heptane,
  Contact_Time = Contact_Time
)

correlation_matrix <- cor(data)
print(correlation_matrix)

library(corrplot)
corrplot(correlation_matrix, method = "circle")

newData <- data.frame(
  Conversion = Conversion,
  Reactor_Temperature = Reactor_Temperature,
  Contact_Time = Contact_Time
)

model <- lm(Conversion ~ Reactor_Temperature + Contact_Time, data = newData)
summary(model)

# Exercise - 4
temperature <- c(53, 57, 58, 63, 66, 67, 67, 67, 68, 69, 70, 70, 70, 70, 70, 75, 75, 76, 76, 76, 76, 78, 79, 81)
failure <- c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0)

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
pValues <- 2 * (1 - pnorm(abs(z_values)))

for (i in 1:length(beta)) {
  if (pValues[i] < 0.05) {
    print(paste("The coefficient", i, "is significant"))
  } else {
    print(paste("The coefficient", i, "is not significant"))
  }
}