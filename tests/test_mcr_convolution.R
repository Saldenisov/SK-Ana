# Test script for MCR-ALS with Convolution Broadening
# This script generates synthetic data with known broadening and tests recovery

# Source the ALS functions
source("server_files/ALS.R")

# Load required packages
library(nnls)

# Generate synthetic data
set.seed(42)

# Parameters
n_samples <- 20      # Number of spectra (different samples)
n_wavelengths <- 100 # Spectral resolution
n_components <- 3    # Number of pure components

# Create wavelength axis
wavelengths <- seq(400, 700, length.out = n_wavelengths)

# Create pure spectral profiles (S matrix)
# Component 1: Gaussian peak at 500 nm
S_true <- matrix(0, nrow = n_wavelengths, ncol = n_components)
S_true[, 1] <- exp(-((wavelengths - 500)^2) / (2 * 20^2))

# Component 2: Gaussian peak at 570 nm
S_true[, 2] <- exp(-((wavelengths - 570)^2) / (2 * 15^2))

# Component 3: Gaussian peak at 640 nm
S_true[, 3] <- exp(-((wavelengths - 640)^2) / (2 * 25^2))

# Normalize spectra
for (i in 1:n_components) {
  S_true[, i] <- S_true[, i] / max(S_true[, i])
}

# Create concentration profiles (C matrix) - random but positive
C_true <- matrix(runif(n_samples * n_components), nrow = n_samples, ncol = n_components)
# Normalize concentrations to sum to 1
C_true <- C_true / rowSums(C_true)

# Create broadening parameters (G matrix)
# Each sample has different broadening - 2 parameters per component
G_true <- matrix(0, nrow = n_samples, ncol = n_components * 2)

# Generate varying broadening for each sample
# Use percentage-based broadening: 1% to 10% of spectral range
# Spectral range is n_wavelengths = 100 points
for (i in 1:n_samples) {
  # Broadening increases with sample number (simulating thickness effect)
  # Start at 1% (1 point), increase to ~8% (8 points)
  base_pct <- 0.01 + (i - 1) * 0.0035  # 1% to 7.65%
  base_broadening <- base_pct * n_wavelengths
  
  for (k in 1:n_components) {
    G_true[i, (k-1)*2 + 1] <- base_broadening + runif(1, -0.2, 0.2)
    G_true[i, (k-1)*2 + 2] <- base_broadening * 0.5 + runif(1, -0.1, 0.1)
  }
}

# Generate observed data with broadening
cat("Generating synthetic data with broadening...\n")
D_obs <- matrix(0, nrow = n_samples, ncol = n_wavelengths)

for (i in 1:n_samples) {
  for (k in 1:n_components) {
    # Apply broadening to component k for sample i
    sigma1 <- G_true[i, (k-1)*2 + 1]
    sigma2 <- G_true[i, (k-1)*2 + 2]
    
    S_broadened <- convolve_spectrum(convolve_spectrum(S_true[, k], sigma1), sigma2)
    D_obs[i, ] <- D_obs[i, ] + C_true[i, k] * S_broadened
  }
}

# Add some noise
noise_level <- 0.01
D_obs <- D_obs + matrix(rnorm(n_samples * n_wavelengths, sd = noise_level), 
                        nrow = n_samples, ncol = n_wavelengths)

# Initialize with SVD
cat("Initializing with SVD...\n")
svd_res <- svd(D_obs, nu = n_components, nv = n_components)
C_init <- matrix(abs(svd_res$u[, 1:n_components]), ncol = n_components)
S_init <- matrix(abs(svd_res$v[, 1:n_components]), ncol = n_components)

# Test 1: Standard MCR-ALS (without broadening)
cat("\n=== Test 1: Standard MCR-ALS (no broadening) ===\n")
result_standard <- myals(
  C = C_init,
  Psi = D_obs,
  S = S_init,
  xC = 1:n_samples,
  xS = wavelengths,
  maxiter = 100,
  thresh = 1e-5,
  nonnegS = TRUE,
  nonnegC = TRUE,
  normS = TRUE,
  uniS = FALSE,
  optS1st = TRUE,
  smooth = 0,
  SumS = FALSE,
  silent = FALSE,
  broadening = FALSE
)

cat("Standard ALS - Final LOF: ", result_standard$lof, "%\n")

# Test 2: MCR-ALS with broadening
cat("\n=== Test 2: MCR-ALS with Broadening ===\n")
result_broadening <- myals(
  C = C_init,
  Psi = D_obs,
  S = S_init,
  xC = 1:n_samples,
  xS = wavelengths,
  maxiter = 100,
  thresh = 1e-5,
  nonnegS = TRUE,
  nonnegC = TRUE,
  normS = TRUE,
  uniS = FALSE,
  optS1st = TRUE,
  smooth = 0,
  SumS = FALSE,
  silent = FALSE,
  broadening = TRUE,
  G = NULL,  # Will be initialized automatically
  broadening_max_pct = 10  # Max 10% broadening
)

cat("Broadening ALS - Final LOF: ", result_broadening$lof, "%\n")

# Compare results
cat("\n=== Comparison ===\n")
cat("LOF improvement: ", result_standard$lof - result_broadening$lof, "%\n")

# Compare recovered broadening parameters
if (!is.null(result_broadening$G)) {
  cat("\nRecovered broadening parameters (first 5 samples):\n")
  print(head(result_broadening$G, 5))
  cat("\nTrue broadening parameters (first 5 samples):\n")
  print(head(G_true, 5))
  
  # Compute RMSE for broadening parameters
  G_rmse <- sqrt(mean((result_broadening$G - G_true)^2))
  cat("\nBroadening parameter RMSE: ", G_rmse, "\n")
}

# Plot comparison (if plotting is available)
if (requireNamespace("graphics", quietly = TRUE)) {
  cat("\nGenerating plots...\n")
  
  # Plot 1: True vs recovered spectra
  par(mfrow = c(2, 2))
  
  # Pure spectra comparison
  plot(wavelengths, S_true[, 1], type = "l", col = "black", lwd = 2,
       main = "True vs Recovered Spectra (Component 1)",
       xlab = "Wavelength", ylab = "Intensity")
  lines(wavelengths, result_standard$S[, 1], col = "blue", lwd = 1.5)
  lines(wavelengths, result_broadening$S[, 1], col = "red", lwd = 1.5)
  legend("topright", 
         c("True", "Standard ALS", "With Broadening"),
         col = c("black", "blue", "red"), lwd = 2)
  
  # Broadening parameters
  if (!is.null(result_broadening$G)) {
    plot(1:n_samples, G_true[, 1], type = "p", col = "black", pch = 19,
         main = "Broadening Parameter 1 (Component 1)",
         xlab = "Sample", ylab = "Sigma", ylim = range(c(G_true[, 1], result_broadening$G[, 1])))
    points(1:n_samples, result_broadening$G[, 1], col = "red", pch = 17)
    legend("topleft", c("True", "Recovered"), col = c("black", "red"), pch = c(19, 17))
  }
  
  # Concentration comparison (first component)
  plot(1:n_samples, C_true[, 1], type = "p", col = "black", pch = 19,
       main = "Concentrations (Component 1)",
       xlab = "Sample", ylab = "Concentration")
  points(1:n_samples, result_standard$C[, 1], col = "blue", pch = 17)
  points(1:n_samples, result_broadening$C[, 1], col = "red", pch = 15)
  legend("topleft", 
         c("True", "Standard ALS", "With Broadening"),
         col = c("black", "blue", "red"), pch = c(19, 17, 15))
  
  # Residuals comparison
  resid_std <- sqrt(rowSums(result_standard$resid^2))
  resid_broad <- sqrt(rowSums(result_broadening$resid^2))
  plot(1:n_samples, resid_std, type = "l", col = "blue", lwd = 2,
       main = "Residuals per Sample",
       xlab = "Sample", ylab = "RMSE", ylim = range(c(resid_std, resid_broad)))
  lines(1:n_samples, resid_broad, col = "red", lwd = 2)
  legend("topright", 
         c("Standard ALS", "With Broadening"),
         col = c("blue", "red"), lwd = 2)
}

cat("\n=== Test completed ===\n")
cat("Summary:\n")
cat("- Standard ALS LOF: ", result_standard$lof, "%\n")
cat("- Broadening ALS LOF: ", result_broadening$lof, "%\n")
cat("- Improvement: ", result_standard$lof - result_broadening$lof, "%\n")

if (result_broadening$lof < result_standard$lof) {
  cat("\n✓ SUCCESS: Broadening constraint improved the fit!\n")
} else {
  cat("\n⚠ WARNING: Broadening constraint did not improve the fit.\n")
}
