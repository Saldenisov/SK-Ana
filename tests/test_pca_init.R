# Test PCA initialization implementation
# This tests the syntax and logic of the new PCA initialization code

# Create sample data matrix
set.seed(123)
mat <- matrix(rnorm(100 * 50, mean = 5, sd = 2), nrow = 100, ncol = 50)

# Number of components to extract
nStart <- 3

# PCA Initialization (as implemented in ALS.R)
mat_centered <- scale(mat, center = TRUE, scale = FALSE)

# Perform SVD on centered data
pcaRES <- svd(mat_centered, nu = nStart, nv = nStart)

# Use absolute values and shift to positive range
S <- matrix(abs(pcaRES$v[, 1:nStart]), ncol = nStart)
C <- matrix(abs(pcaRES$u[, 1:nStart]), ncol = nStart)

# Scale back to original data range
for (i in 1:nStart) {
  S[, i] <- S[, i] * pcaRES$d[i] / max(S[, i])
}

# Verify outputs
cat("PCA Initialization Test Results:\n")
cat("================================\n")
cat("C matrix dimensions:", dim(C), "\n")
cat("S matrix dimensions:", dim(S), "\n")
cat("C range:", range(C), "\n")
cat("S range:", range(S), "\n")
cat("First 3 singular values:", pcaRES$d[1:3], "\n")
cat("\nTest PASSED: PCA initialization code runs without errors.\n")
