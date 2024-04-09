# Exercise-1
# Load the data
y <- c(2.6, 2.4, 17.32, 15.6, 16.12, 5.36, 6.19, 10.17, 2.62, 2.98, 6.92, 7.06)
x1 <- c(31, 31, 31.5, 31.5, 31.5, 30.5, 31.5, 30.5, 31, 30.5, 31, 30.5)
x2 <- c(21, 21, 24, 24, 24, 22, 22, 23, 21.5, 21.5, 22.5, 22.5)

# Fit the second-order polynomial model
model <- lm(y ~ x1 + x2 + I(x1^2) + I(x2^2) + I(x1*x2))

# (i) Print the model summary
print(model)

# (ii) Confidence intervals for the coefficients
library(stats)
confint(model)

# (iii) Test for significance of regression coefficients
anova(model)

# (iv) Coefficient of determination
summary(model)$r.squared

# Exercies-2
# Load the data
X1 <- c(152, 183, 171, 165, 158, 161, 149, 158, 170, 153, 164, 190, 185)
X2 <- c(50, 20, 20, 30, 30, 50, 60, 50, 40, 55, 40, 40, 20)
Y <- c(120, 141, 124, 126, 117, 129, 123, 125, 132, 123, 132, 155, 147)

# Fit the multiple regression model
model <- lm(Y ~ X1 + X2)

# (a) Print the model summary
print(model)

# (b) Test for significance of all parameters
anova(model)

# (c) Set up 95% confidence intervals for regression coefficients
library(stats)
confint(model)

# (d) Calculate the coefficient of determination
summary(model)$r.squared


# Exercise-3
# Load the data
data <- data.frame(
  Conversion = c(49.0, 50.2, 50.5, 48.5, 47.5, 44.5, 28.0, 31.5, 34.5, 35.0, 38.0, 38.5, 15.0, 17.0, 20.5, 29.5),
  Temp = c(1300, 1300, 1300, 1300, 1300, 1300, 1200, 1200, 1200, 1200, 1200, 1200, 1100, 1100, 1100, 1100),
  Ratio = c(7.5, 9.0, 11.0, 13.5, 17.0, 23.0, 5.3, 7.5, 11.0, 13.5, 17.0, 23.0, 5.3, 7.5, 11.0, 17.0),
  Time = c(0.0120, 0.0120, 0.0115, 0.0130, 0.0135, 0.0120, 0.0400, 0.0380, 0.0320, 0.0260, 0.0340, 0.0410, 0.0840, 0.0980, 0.0920, 0.0860)
)

# Fit the full quadratic model
model <- lm(Conversion ~ Temp + Ratio + Time + I(Temp^2) + I(Ratio^2) + I(Time^2) + I(Temp*Ratio) + I(Temp*Time) + I(Ratio*Time), data = data)

# Print the model summary
print(model)

# Examine the correlation matrix
cor_matrix <- cor(data)
print(cor_matrix)

# Detect multicollinearity
diag_vals <- diag(solve(cor_matrix))
print(diag_vals)


# Exercise-4
data <- data.frame(
  Failure = c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0),
  Temp = c(53, 57, 58, 63, 66, 67, 67, 67, 68, 69, 70, 70, 70, 70, 70, 75, 75, 76, 76, 76, 76, 78, 79, 81)
)

# Fit the logistic regression model
model <- glm(Failure ~ Temp, data = data, family = binomial)

# Print the model summary
print(model)

# Test for significance of the regression coefficient
library(stats)
summary(model)$coefficients