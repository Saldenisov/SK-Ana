# MCR-ALS Broadening Optimization Scheme

## Overview

This document explains how the broadening optimization works with **1 parameter per component** using **linear grid search**.

## Data Structure

### G Matrix Dimensions
- **Rows**: Number of samples (spectra)
- **Columns**: Number of components
- **Example**: 20 samples × 3 components = 20×3 matrix

```
G matrix for 2 dimensions (components):
Sample 1: [σ₁  σ₂]
Sample 2: [σ₁  σ₂]
Sample 3: [σ₁  σ₂]
...
Sample N: [σ₁  σ₂]
```

### Kinetics Plot
When broadening is enabled, kinetics plot shows:
- **C parameters**: Concentrations for each component
- **G parameters**: Broadening for each component

**Example with 2 components:**
- 2 C traces: C_1, C_2
- 2 G traces: G_1, G_2
- **Total: 4 traces**

## Three-Step Alternating Optimization

```
┌─────────────────────────────────────────────┐
│         MCR-ALS with Broadening             │
│                                             │
│  D ≈ C · (S ⊗ G)ᵀ                          │
│                                             │
│  D: observed data (samples × wavelengths)   │
│  C: concentrations (samples × components)   │
│  S: pure profiles (wavelengths × components)│
│  G: broadening (samples × components)       │
│  ⊗: convolution operation                   │
└─────────────────────────────────────────────┘

Iteration loop:
┌──────────────┐
│ Step 1:      │  Fix S, G → Optimize C
│ Optimize C   │  For each sample: C[i] = argmin ||D[i] - C[i]·S_broad[i]ᵀ||²
└──────┬───────┘  where S_broad[i] = convolve(S, G[i])
       │
       ↓
┌──────────────┐
│ Step 2:      │  Fix C, G → Optimize S
│ Optimize S   │  Update pure profiles S accounting for broadening
└──────┬───────┘  (uses weighted least squares across all samples)
       │
       ↓
┌──────────────┐
│ Step 3:      │  Fix C, S → Optimize G
│ Optimize G   │  For each sample independently:
└──────┬───────┘    For each component: find σ minimizing error
       │
       ↓
   Converged?
    No  │  Yes
    ↓   │   ↓
    └───┘  Return C, S, G
```

## Step 3: Broadening Optimization (Linear Grid Search)

### Algorithm

```
For each sample i = 1 to N:
    For each component k = 1 to n_components:
        1. Create grid: σ_grid = [σ_min, ..., σ_max]  (20 points)
        2. For each test value σ_test in grid:
            a. Set G_temp[i, k] = σ_test
            b. Reconstruct: D_recon[i] = Σⱼ C[i,j] · convolve(S[j], G_temp[i,j])
            c. Compute error: E = ||D[i] - D_recon[i]||²
            d. Track best: if E < best_error:
                   best_σ = σ_test
                   best_error = E
        3. Update: G[i, k] = best_σ
```

### Why Linear Search?

1. **Single parameter per component**: Only need to search 1D space
2. **Simple**: No gradient computation needed
3. **Robust**: Always finds global minimum on grid
4. **Fast enough**: 20 grid points is quick for 1D search
5. **Interpretable**: Easy to understand and debug

### Example: 2 Components, 1 Sample

```
Component 1:
  Test σ = [0.1, 0.5, 1.0, 1.5, 2.0, ..., 10.0]
  Error = [5.2, 4.1, 2.8, 2.1, 2.3, ..., 8.9]
                        ↑
                    best (σ = 1.5)
  → G[i, 1] = 1.5

Component 2:
  Test σ = [0.1, 0.5, 1.0, 1.5, 2.0, ..., 10.0]
  Error = [3.8, 2.9, 2.2, 2.0, 1.8, ..., 7.1]
                                  ↑
                              best (σ = 2.0)
  → G[i, 2] = 2.0

Result: G[i] = [1.5, 2.0]
```

## Code Implementation

### Core Function: `optimize_broadening_single`

```R
optimize_broadening_single <- function(data_row, C_row, S, G_init, 
                                      sigma_max = NULL, n_grid = 20) {
  # Optimize ONE sample's broadening parameters
  
  n_components <- ncol(S)
  spectral_range <- nrow(S)
  
  # Set bounds
  sigma_min <- 0.001 * spectral_range  # 0.1%
  if (is.null(sigma_max)) {
    sigma_max <- 0.10 * spectral_range  # 10% (default)
  }
  
  # Create grid
  sigma_grid <- seq(sigma_min, sigma_max, length.out = n_grid)
  
  # Optimize each component
  G_opt <- G_init
  
  for (k in 1:n_components) {
    best_sigma <- G_init[k]
    best_error <- Inf
    
    # Linear search over grid
    for (sigma_test in sigma_grid) {
      # Test this sigma
      G_temp <- G_opt
      G_temp[k] <- sigma_test
      
      # Reconstruct spectrum
      recon <- rep(0, length(data_row))
      for (j in 1:n_components) {
        S_broad <- convolve_spectrum(S[, j], G_temp[j])
        recon <- recon + C_row[j] * S_broad
      }
      
      # Compute error
      error <- sum((data_row - recon)^2)
      
      # Update best if improved
      if (error < best_error) {
        best_error <- error
        best_sigma <- sigma_test
      }
    }
    
    # Update this component's sigma
    G_opt[k] <- best_sigma
  }
  
  return(G_opt)
}
```

### Convolution Function

```R
convolve_spectrum <- function(spectrum, sigma) {
  # Apply Gaussian convolution to a single spectrum
  
  if (sigma <= 0 || !is.finite(sigma)) {
    return(spectrum)
  }
  
  # Create Gaussian kernel
  kernel_size <- max(3, ceiling(3 * sigma))
  x <- (-kernel_size):kernel_size
  kernel <- exp(-x^2 / (2 * sigma^2))
  kernel <- kernel / sum(kernel)  # Normalize
  
  # Pad signal
  n <- length(spectrum)
  padded <- c(rep(spectrum[1], kernel_size), 
              spectrum, 
              rep(spectrum[n], kernel_size))
  
  # Convolve
  conv_result <- stats::convolve(padded, rev(kernel), type = "open")
  
  # Extract center
  start_idx <- kernel_size + 1
  end_idx <- start_idx + n - 1
  result <- conv_result[start_idx:end_idx]
  
  return(result)
}
```

## Visualization

### Kinetics Plot (2 dimensions example)
```
    C_1 ──────  (concentration component 1)
    C_2 ──────  (concentration component 2)
    G_1 ──────  (broadening component 1)
    G_2 ──────  (broadening component 2)
    
Total: 4 traces
```

### Spectra Plot
```
Solid lines:   S (unbroadened profiles from optimization)
Dashed lines:  S ⊗ G_mean (broadened with average G)

This shows:
- What the pure profiles look like (sharp)
- How they appear after broadening (smoothed)
```

## Complexity Analysis

### Time Complexity per Iteration

**Step 1 (Optimize C):**
- O(N · K · W) where:
  - N = samples
  - K = components  
  - W = wavelength points
- Uses NNLS solver

**Step 2 (Optimize S):**
- O(N · K · W)
- Least squares across all samples

**Step 3 (Optimize G):**
- O(N · K · n_grid · W) where:
  - N = samples
  - K = components
  - n_grid = 20 (grid size)
  - W = wavelength points
- Linear search per component per sample

**Total per iteration:** O(N · K · W · n_grid)

### Example Numbers
- 20 samples × 3 components × 100 wavelengths × 20 grid points
- = 1,200,000 operations per iteration
- Typically converges in 30-50 iterations
- Total: ~60M operations (very fast)

## Advantages of This Approach

1. ✅ **Simple**: One parameter per component
2. ✅ **Robust**: Linear search finds global minimum on grid
3. ✅ **Interpretable**: Easy to understand σ values
4. ✅ **Fast**: Grid search is efficient for 1D
5. ✅ **Stable**: No gradient issues or local minima problems
6. ✅ **Debuggable**: Can plot error vs. σ to visualize

## Output Structure

```R
result <- myals(..., broadening = TRUE)

result$C  # (N × K) concentrations
result$S  # (W × K) pure profiles (unbroadened)
result$G  # (N × K) broadening parameters
result$lof  # Lack of fit percentage
```

## Summary

**Key points:**
- **1 parameter per component** (not 2)
- **Linear grid search** (not L-BFGS-B)
- **20 grid points** from 0.1% to user-defined max (default 10%)
- **Independent optimization** per sample, per component
- **Kinetics plot** shows C and G together (2 dims → 4 traces)
- **Spectra plot** shows unbroadened (solid) and broadened (dashed) profiles

This approach is simpler, more robust, and easier to understand than the previous multi-parameter gradient-based method.
