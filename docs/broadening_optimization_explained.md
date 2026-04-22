# Broadening Optimization - Detailed Explanation

## Overview

This document explains how the broadening parameters (G) are optimized in the MCR-ALS with convolution implementation, and why starting from small values is important.

## Optimization Strategy

### Three-Step Alternating Optimization

The algorithm cycles through three optimization steps:

```
Iteration cycle:
1. Optimize C (fix S, G)
2. Optimize S (fix C, G)  
3. Optimize G (fix C, S)
```

This is an extension of the standard two-step ALS algorithm (which only alternates C and S).

### Step 3: G Optimization in Detail

For each sample `i`, we optimize its broadening parameters `G[i,]` independently:

```R
minimize: ||D[i,] - Σ_k C[i,k] · convolve(S[k], G[i,k])||²
```

Where:
- `D[i,]` = observed spectrum for sample i
- `C[i,k]` = concentration of component k in sample i
- `S[k]` = pure spectral profile of component k
- `G[i,k]` = broadening parameters for component k in sample i (2 sigma values)
- `convolve()` = Gaussian convolution operation

#### Optimization Method: L-BFGS-B

We use **L-BFGS-B** (Limited-memory Broyden–Fletcher–Goldfarb–Shanno with Box constraints) because:

1. **Gradient-based**: Efficiently finds local minima for smooth objective functions
2. **Box constraints**: Enforces bounds on sigma values: `[0.01, 10.0]`
3. **Memory efficient**: Doesn't require storing full Hessian matrix
4. **Robust**: Handles the non-linear convolution operation well

#### Per-Sample Optimization

Each sample's broadening is optimized **independently** because:
- Different samples may have different thickness
- Different samples may have different measurement conditions
- No assumption of smooth variation across samples (though this could be added as regularization)

### Convolution Details

Each component undergoes **two successive Gaussian convolutions**:

```R
S_broadened = convolve(convolve(S, σ₁), σ₂)
```

Why two convolutions?
- **Flexibility**: Can approximate various broadening functions
- **Asymmetry**: Two different sigmas allow for asymmetric broadening
- **Voigt-like profiles**: Successive convolutions can approximate more complex peak shapes

### Gaussian Kernel Construction

For a given sigma σ:

```R
kernel_size = ceil(3 * σ)
x = [-kernel_size : kernel_size]
kernel = exp(-x² / (2σ²))
kernel = kernel / sum(kernel)  # Normalize to sum = 1
```

**Edge handling**: Reflection padding to minimize artifacts
```R
padded = [S[1], ..., S[1], S[1:n], S[n], ..., S[n]]
         |<- padding ->|  original  |<- padding ->|
```

## Why Start from Small Values?

### Initialization: G = 1% of Spectral Range

Starting from **small broadening values (1% of spectral range)** is critical because:

#### 1. **Avoid Over-Smoothing**
- Large sigma values (e.g., 0.5) heavily smooth spectra
- Over-smoothing loses important spectral features
- Makes it difficult to distinguish between components

**Example:**
```
Original peak:    ___/\___
σ = 0.1:         __/  \__
σ = 0.5:         ___~~~___  (feature lost!)
```

#### 2. **Easier Optimization**
- Starting from low broadening → algorithm can increase if needed
- Starting from high broadening → harder to "undo" excessive smoothing
- Objective function landscape is more favorable from low values

#### 3. **Better Convergence**
- Small initial values → S profiles retain sharp features
- Sharp features → better discrimination between components
- Better discrimination → more stable concentration estimates

#### 4. **Physical Motivation**
- In many experiments, broadening is a **perturbation** not the dominant effect
- Starting from minimal perturbation is physically reasonable
- Algorithm adds broadening only where data demands it

#### 5. **Identifiability**
- Low broadening → problem closer to standard MCR-ALS (well-understood)
- High broadening → more degrees of freedom, harder to constrain
- Gradual increase from low values maintains identifiability

### Comparison: Initial Values

| Initial Value | Result | Issue |
|---------------|--------|-------|
| 0.1% of range | Too rigid | May converge slowly; very little initial smoothing |
| **1% of range** | ✓ **Optimal** | Minimal smoothing; allows algorithm to add as needed |
| 5% of range | Over-smoothed | Loses features; hard to recover sharp peaks |
| 10%+ of range | Heavily blurred | Poor component separation; unstable |

**Note**: For 100 wavelength points, 1% = σ of 1 point; for 1000 points, 1% = σ of 10 points.

### Optimization Bounds

**Percentage-based bounds** (adapts to data resolution):
- **Lower bound**: 0.1% of spectral range (prevents numerical issues with σ → 0)
- **Upper bound**: User-defined, default 10% of spectral range

**Examples:**
- 100 wavelength points: σ ranges from 0.1 to 10 points
- 1000 wavelength points: σ ranges from 1 to 100 points
- 2000 wavelength points: σ ranges from 2 to 200 points

**Advantages:**
- Automatically scales with data resolution
- User-friendly percentage interface
- Consistent physical interpretation across datasets

## Convergence Behavior

### Typical Optimization Trajectory

```
Iteration | σ values (for 100-point spectrum)
----------|------------------------------------
    0     | [1.0, 1.0, 1.0, 1.0, 1.0, 1.0]  (initialization at 1%)
    1     | [1.0, 1.0, 1.0, 1.0, 1.0, 1.0]  (C step)
    2     | [1.0, 1.0, 1.0, 1.0, 1.0, 1.0]  (S step)
    3     | [1.2, 0.9, 1.5, 1.1, 1.3, 1.0]  (G step - first optimization)
    4     | [1.2, 0.9, 1.5, 1.1, 1.3, 1.0]  (C step)
    5     | [1.2, 0.9, 1.5, 1.1, 1.3, 1.0]  (S step)
    6     | [1.8, 1.2, 2.3, 1.6, 2.0, 1.5]  (G step - refined)
    ...
   30     | [2.5, 1.8, 3.5, 2.4, 3.0, 2.2]  (converged at 2-3%)
```

**In percentage terms**: Starting at 1%, converging to 2-3.5% for this example.

Notice:
- G parameters **increase gradually** from initial 0.1
- Different parameters converge to different values (sample/component-specific)
- Convergence is smooth, not oscillatory

### When Algorithm Increases σ

The algorithm increases σ when:
1. **Observed spectrum is broader** than pure component profile
2. **Residual error decreases** by adding broadening
3. **Sample-specific effects** cause peak widening

### When σ Stays Small

σ remains near 0.1 when:
1. **No significant broadening** in that sample
2. **Pure profiles already match** observed widths
3. **Data doesn't support** additional broadening (noise vs. signal)

## Diagnostic: Checking Optimization Quality

### Good Optimization

**Indicators:**
- G parameters vary smoothly across samples (if physical trend exists)
- LOF decreases compared to standard ALS
- Recovered S profiles are sharper than with standard ALS
- G values are reasonable (typically 0.1 - 2.0 for most data)

### Poor Optimization  

**Warning signs:**
- G parameters hit bounds (all at 0.01 or all at 10.0)
- G parameters oscillate between iterations
- LOF doesn't improve or gets worse
- Negative concentrations appear

**Solutions:**
- Check if data actually has broadening variation
- Adjust convergence threshold
- Increase max iterations
- Add regularization on G (smoothness penalty)

## Mathematical Formulation

### Full Objective Function

For the complete system:

```
minimize: Σ_i ||D[i,] - Σ_k C[i,k] · (S[k] ⊗ G[i,k])||²

subject to:
  C[i,k] ≥ 0  (non-negativity)
  S[j,k] ≥ 0  (non-negativity)
  Σ_j S[j,k] = 1  (normalization)
  0.01 ≤ G[i,p] ≤ 10.0  (bounds)
```

Where:
- `i` indexes samples (rows of D)
- `j` indexes wavelengths (columns of D)
- `k` indexes components
- `p` indexes broadening parameters (2 per component)
- `⊗` denotes convolution

### Gradient of G Step

For a single sample `i`, the gradient of the objective with respect to `G[i,p]` is:

```
∂L/∂G[i,p] = -2 · Σ_j (D[i,j] - D̂[i,j]) · C[i,k] · ∂/∂G[i,p][S ⊗ G]_j

where:
  D̂[i,j] = Σ_k C[i,k] · [S[k] ⊗ G[i,k]]_j  (predicted spectrum)
  k = floor(p/2) + 1  (component index)
```

The convolution derivative `∂/∂σ[S ⊗ G]` is computed numerically via finite differences in the optimization.

## Advanced: Regularization Options

### Not Currently Implemented (Future Work)

#### 1. Smoothness Regularization
Penalize rapid changes in G across samples:
```
L_reg = λ · Σ_i (G[i,:] - G[i-1,:])²
```

#### 2. Sparsity Regularization  
Encourage G parameters to stay small (L1 penalty):
```
L_reg = λ · Σ_i,p |G[i,p]|
```

#### 3. Group Regularization
Encourage similar broadening across components within a sample:
```
L_reg = λ · Σ_i Var(G[i,k] for all k)
```

These could be added to improve stability in difficult cases.

## Summary

**Key Points:**
1. G optimization uses **L-BFGS-B** with **percentage-based box constraints**
   - Lower bound: 0.1% of spectral range
   - Upper bound: User-defined (default 10%)
2. Each sample optimized **independently** for its broadening
3. **Two successive convolutions** per component for flexibility
4. **Start from 1% of spectral range** to avoid over-smoothing and ensure stable convergence
5. Algorithm increases σ only where data demands it
6. Typical converged values: 1-5% for most spectroscopic applications
7. **Scale-aware**: Automatically adapts to data resolution

**Philosophy:**
> Start conservatively (low broadening), let the data guide optimization upward, rather than starting aggressive (high broadening) and trying to undo excess smoothing.

This approach ensures robust convergence and prevents loss of important spectral features.
