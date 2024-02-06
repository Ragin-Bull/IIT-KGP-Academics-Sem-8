# Function to generate random samples from a multivariate normal distribution
generate_multivariate_normal_samples <- function(mu, sigma, num_samples = 5000) {
  p <- length(mu)
  
  # Step I: Decompose Sigma = CC^T
  C <- chol(sigma)
  
  # Step II: Generate Z1, Z2, ..., Zp as IID N(0, 1) random variates
  Z <- matrix(rnorm(num_samples * p), ncol = p)
  
  # Step III: Calculate Xi for each i
  X <- matrix(0, nrow = num_samples, ncol = p)
  for (i in 1:p) {
    X[, i] <- mu[i] + sum(C[i, 1:i] * Z[, 1:i])
  }
  
  # Step IV: Return X
  return(X)
}

# Example 1
mu_1 <- c(1, 2, 3)
sigma_1 <- matrix(c(1, 0.5, 0.2,
                    0.5, 2, 0.7,
                    0.2, 0.7, 3), nrow = 3, byrow = TRUE)

samples_1 <- generate_multivariate_normal_samples(mu_1, sigma_1)
sample_mean_1 <- colMeans(samples_1)
sample_covariance_1 <- cov(samples_1)

# Calculate norm error for mean and covariance
mean_norm_error_1 <- norm(sample_mean_1 - mu_1)
covariance_norm_error_1 <- norm(sample_covariance_1 - sigma_1)

cat("Example 1:\n")
cat("Sample Mean:", sample_mean_1, "\n")
cat("Sample Covariance Matrix:\n")
print(sample_covariance_1)
cat("Mean Norm Error:", mean_norm_error_1, "\n")
cat("Covariance Matrix Norm Error:", covariance_norm_error_1, "\n")
cat("\n")

# Repeat the process for other examples...

# Function to calculate the Frobenius norm of a matrix
norm <- function(mat) {
  sqrt(sum(mat^2))
}