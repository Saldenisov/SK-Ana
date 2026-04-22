# PCA Initialization for ALS

## Overview

PCA (Principal Component Analysis) initialization has been added as a new option for initializing the Alternating Least Squares (ALS) algorithm in SK-Ana.

## What is PCA Initialization?

PCA initialization performs decomposition on **centered data** (data with mean subtracted), unlike SVD which works on the raw data. This approach:

1. **Centers the data**: Subtracts the mean from each column
2. **Performs SVD** on the centered matrix
3. **Takes absolute values** of the resulting vectors
4. **Scales appropriately** to preserve component importance

## Mathematical Details

```r
# Traditional SVD initialization
D = U · Σ · V^T
C₀ = |U[:, 1:k]|
S₀ = |V[:, 1:k]|

# PCA initialization
D_centered = D - mean(D)
D_centered = U · Σ · V^T
C₀ = |U[:, 1:k]| · Σ[:k]
S₀ = |V[:, 1:k]|
```

## When to Use PCA Initialization

### ✅ Use PCA when:
- Data has **significant baseline offsets**
- You observe **baseline drift** across experiments
- You want to focus on **variance** rather than absolute values
- Standard SVD initialization gives poor convergence
- Spectra have large background signals

### ❌ Don't use PCA when:
- Data is already baseline-corrected
- Absolute intensity values are important
- Working with pure noise data
- SVD initialization works well

## How to Use

1. Go to **ALS** tab
2. Click **Options** sub-tab
3. Under **Initialization**, select **PCA**
4. Set other constraints as needed
5. Click **Run**

## Comparison with Other Methods

| Method | Centering | Best For |
|--------|-----------|----------|
| **SVD** | No | General purpose, default choice |
| **PCA** | Yes | Data with baseline offsets/drifts |
| **NMF** | No | Strictly non-negative data |
| **Sequential** | Varies | Uncertain dimensionality |
| **Restart** | N/A | Refining previous results |

## Implementation Details

**Files Modified:**
- `ui_files/ALSInputOptions.R` - Added PCA to UI radio buttons
- `server_files/ALS.R` - Added PCA initialization logic (lines 956-972)
- `docs/als.md` - Updated documentation
- `EXPLANATION.md` - Added technical details

**Code Location:**
```r
# In server_files/ALS.R, around line 956
} else if (initALS == "PCA") {
  # Initialize with PCA (centered data)
  mat_centered <- scale(mat, center = TRUE, scale = FALSE)
  pcaRES <- svd(mat_centered, nu = nStart, nv = nStart)
  S = matrix(abs(pcaRES$v[, 1:nStart]), ncol = nStart)
  C = matrix(abs(pcaRES$u[, 1:nStart]), ncol = nStart)
  for (i in 1:nStart) {
    S[, i] <- S[, i] * pcaRES$d[i] / max(S[, i])
  }
}
```

## Testing

A test script is available at `test_pca_init.R` to verify the implementation.

## References

- Jolliffe, I.T. (2002). *Principal Component Analysis*. Springer.
- Wold, S., Esbensen, K., & Geladi, P. (1987). Principal component analysis. *Chemometrics and Intelligent Laboratory Systems*, 2(1-3), 37-52.

## Notes

- PCA initialization is deterministic (always produces the same result for the same data)
- Computation time is similar to SVD initialization
- The centering operation is automatically handled by R's `scale()` function
- Components are scaled to preserve relative importance from singular values

---

**Version:** 1.0  
**Date:** 2025-11-19  
**Author:** SK-Ana Development Team
