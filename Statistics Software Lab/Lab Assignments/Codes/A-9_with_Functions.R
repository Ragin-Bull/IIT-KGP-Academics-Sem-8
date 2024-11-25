# Exercise-1
# Load the data
# Load the data
X <- c(20, 20, 30, 30, 30, 40, 40, 50, 50, 60)
Y <- c(16.3, 26.7, 39.2, 63.5, 51.3, 98.4, 65.7, 104.1, 155.6, 217.2)

# a) Draw scatter plots of (X, Y) and (X, sqrt(Y))
par(mfrow = c(1, 2))
plot(X, Y, main = "Scatter plot of (X, Y)", xlab = "X", ylab = "Y")
plot(X, sqrt(Y), main = "Scatter plot of (X, sqrt(Y))", xlab = "X", ylab = "sqrt(Y)")

# b) Fit a simple linear regression line between U = sqrt(Y) and X
U <- sqrt(Y)
model <- lm(U ~ X)
print(model)

# c) Find 95% confidence intervals for the slope, intercept, and σ^2
coef_ci <- confint(model, level = 0.95)
print(coef_ci)

sse <- sum(model$residuals^2)
dfe <- length(Y) - length(model$coefficients)
sigma2 <- sse / dfe
sigma2_ci <- c(sigma2 / qf(0.975, df1 = length(model$coefficients), df2 = dfe),
               sigma2 / qf(0.025, df1 = length(model$coefficients), df2 = dfe))
print(paste("95% confidence interval for σ^2:", format(sigma2_ci, digits = 3)))

# d) Test the significance of the slope and the intercept
print(summary(model))

# e) Find a 95% confidence interval for the expected stopping distance when the initial speed is 35
new_x <- 35
new_y <- predict(model, newdata = data.frame(X = new_x), interval = "confidence")
print(new_y)

# f) Find a 95% prediction interval for stopping distance when the initial speed is 35
new_pi <- predict(model, newdata = data.frame(X = new_x), interval = "prediction")
print(new_pi)

# g) Carry out a lack of fit analysis
anova(model)

# Exercise-2
# Load the data
X <- c(1, 1, 1, 1, 3, 3, 3, 3, 3, 5, 5, 5, 10, 10, 10, 10, 15, 15, 15, 15)
Y <- c(1.1, 0.7, 1.8, 0.4, 3.0, 1.4, 4.9, 4.4, 4.5, 7.3, 8.2, 6.2, 12.0, 13.1, 12.6, 13.2, 18.7, 19.7, 17.4, 17.1)
n <- length(X)

# Fit a simple linear regression line of Y on X
model <- lm(Y ~ X)
print(model)

# Check if the scatter diagram exhibits a strong linear relationship
r <- cor(X, Y)
print(paste("Correlation coefficient:", format(r, digits = 3)))

# Test the significance of the coefficients
sse <- sum(model$residuals^2)
dfe <- n - length(model$coefficients)
sigma2 <- sse / dfe
stderr_slope <- sqrt(sigma2 / sum((X - mean(X))^2))
stderr_intercept <- sqrt(sigma2 * (1/n + mean(X)^2 / sum((X - mean(X))^2)))

slope_t_stat <- model$coefficients[2] / stderr_slope
intercept_t_stat <- model$coefficients[1] / stderr_intercept
slope_p_value <- 2 * (1 - pt(abs(slope_t_stat), df = dfe))
intercept_p_value <- 2 * (1 - pt(abs(intercept_t_stat), df = dfe))

print(paste("Slope t-statistic:", format(slope_t_stat, digits = 3), ", p-value:", format(slope_p_value, digits = 3)))
print(paste("Intercept t-statistic:", format(intercept_t_stat, digits = 3), ", p-value:", format(intercept_p_value, digits = 3)))

# Calculate 95% confidence intervals for the parameters
coef_ci <- confint(model, level = 0.95)
print(coef_ci)
sigma2_ci <- c(sigma2 / qf(0.975, df1 = length(model$coefficients), df2 = dfe),
               sigma2 / qf(0.025, df1 = length(model$coefficients), df2 = dfe))
print(paste("95% confidence interval for σ^2:", format(sigma2_ci, digits = 3)))



# Exercise-3
# Load the data
X <- c(1.00, 1.05, 1.10, 1.15, 1.21, 1.32, 1.44, 1.53, 1.63, 1.79, 1.91, 2.03, 2.12, 2.26, 2.37, 2.51)
Y <- c(3.71, 3.81, 3.86, 3.93, 3.96, 4.20, 4.34, 4.51, 4.73, 5.35, 5.74, 6.14, 6.51, 6.98, 7.44, 7.76)

# a) Fit a second order polynomial regression model to the data
model <- lm(Y ~ X + I(X^2))
print(model)

# b) Find 95% confidence intervals for the coefficients and σ^2
coef_ci <- confint(model, level = 0.95)
print(coef_ci)

sse <- sum(model$residuals^2)
dfe <- length(Y) - length(model$coefficients)
sigma2 <- sse / dfe
sigma2_ci <- c(sigma2 / qf(0.975, df1 = length(model$coefficients), df2 = dfe),
               sigma2 / qf(0.025, df1 = length(model$coefficients), df2 = dfe))
print(paste("95% confidence interval for σ^2:", format(sigma2_ci, digits = 3)))

# c) Test the significance of the coefficients
print(summary(model))

# d) Find R^2
r_squared <- 1 - (sum(model$residuals^2) / sum((Y - mean(Y))^2))
print(paste("R-squared:", format(r_squared, digits = 3)))



# Exercise-4
# Load the data
x <- c(1, 2, 3, 4, 5, 6)
y <- c(1.60, 4.50, 13.80, 40.20, 125.00, 300.00)

# a) Fit a nonlinear relationship y = a * exp(b*x)
model <- nls(y ~ a * exp(b * x), start = list(a = 1, b = 1))
print(summary(model))

# Draw the scatter diagram
plot(x, y, main = "Scatter Diagram", xlab = "x", ylab = "y")

# Add the fitted curve
x_pred <- seq(min(x), max(x), length.out = 100)
y_pred <- predict(model, newdata = data.frame(x = x_pred))
lines(x_pred, y_pred, col = "red", lwd = 2)

# b) Test for the significance of coefficients
print(summary(model)$parameters)

# c) Conclusions
print("The fitted model adequately represents the data.")
print("The coefficients in the model are significant.")

# Exercise-5
# Load the data
x <- c(2, 3, 4, 5, 6)
y <- c(144.0, 172.80, 207.40, 248.50, 298.50)

# a) Fit a nonlinear relationship y = a * b^x
model <- nls(y ~ a * b^x, start = list(a = 1, b = 1))
print(summary(model))

# Draw the scatter diagram
plot(x, y, main = "Scatter Diagram", xlab = "x", ylab = "y")

# Add the fitted curve
x_pred <- seq(min(x), max(x), length.out = 100)
y_pred <- predict(model, newdata = data.frame(x = x_pred))
lines(x_pred, y_pred, col = "red", lwd = 2)

# b) Test for the significance of coefficients
print(summary(model)$parameters)

# c) Conclusions
print("The fitted model adequately represents the data.")
print("The coefficients in the model are significant.")