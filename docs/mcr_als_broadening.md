# MCR-ALS with Convolution Broadening

## Overview

This implementation extends the standard MCR-ALS (Multivariate Curve Resolution - Alternating Least Squares) algorithm to account for sample-specific peak broadening in spectroscopic data. This is particularly useful for WAXS (Wide-Angle X-ray Scattering) data where different sample thicknesses cause varying degrees of peak broadening.

## Theory

The standard MCR-ALS decomposes a data matrix as:
```
D = C·S^T
```

With broadening, the model becomes:
```
D ≈ C·(S⊗G)^T
```

Where:
- `D` is the observed data matrix (samples × wavelengths)
- `C` is the concentration matrix (samples × components)
- `S` is the pure spectral profiles matrix (wavelengths × components)
- `G` is the broadening parameters matrix (samples × 2*components)
- `⊗` denotes convolution operation

Each sample has unique broadening parameters, and each component gets **2 broadening weights** (sigma parameters for successive Gaussian convolutions).

## Implementation Details

### Broadening Parameterization

For a system with `n` components and `m` samples:
- **G matrix dimensions**: `m × (2n)`
- **Parameter layout**: `[σ₁₁, σ₁₂, σ₂₁, σ₂₂, ..., σₙ₁, σₙ₂]` for each sample
  - Component 1: `σ₁₁`, `σ₁₂` (two sequential convolutions)
  - Component 2: `σ₂₁`, `σ₂₂`
  - Component n: `σₙ₁`, `σₙ₂`

**Percentage-Based Parameterization:**
- Broadening is expressed as **percentage of spectral range**
- **Initialization**: 1% of spectral range
- **Upper bound**: User-defined (default 10%)
- **Example**: For 1000 wavelength points, 1% = σ of 10 points

### Algorithm Steps

The broadening-enabled MCR-ALS uses a **three-step alternating optimization**:

1. **Optimize C (concentrations)**: Fix S and G, solve for C given broadened profiles
   ```R
   For each sample i:
     S_broad = Convolve(S, G[i,])
     C[i,] = argmin ||D[i,] - C[i,]·S_broad^T||²
   ```

2. **Optimize S (profiles)**: Fix C and G, update pure profiles
   ```R
   Solve for S accounting for varying broadening across samples
   (uses weighted least squares approach)
   ```

3. **Optimize G (broadening)**: Fix C and S, optimize broadening parameters
   ```R
   For each sample i:
     G[i,] = argmin ||D[i,] - C[i,]·Convolve(S, G[i,])^T||²
   ```

### Convolution Implementation

Gaussian convolution is applied using:
```R
convolve_spectrum(spectrum, sigma)
```

Each component undergoes **two successive convolutions** to provide flexible broadening:
```R
S_broadened = convolve_spectrum(convolve_spectrum(S, σ₁), σ₂)
```

Bounds on sigma parameters: `[0.01, 10.0]` to ensure numerical stability.

## Usage

### UI Activation

1. Navigate to the **S constraints** panel in the ALS interface
2. Check the **"Broadening"** checkbox
3. (Optional) Set **"Max broadening (%)"** - default is 10%
   - This controls the upper bound for optimization
   - Algorithm starts at 1% and can increase up to this limit
4. Run ALS as normal

The broadening parameters will be automatically initialized to 1% and optimized during the ALS iterations.

### Programmatic Usage

```R
# Standard ALS (no broadening)
result <- myals(
  C = C_init, 
  Psi = data_matrix, 
  S = S_init,
  broadening = FALSE,
  ...
)

# ALS with broadening
result <- myals(
  C = C_init, 
  Psi = data_matrix, 
  S = S_init,
  broadening = TRUE,
  G = NULL,  # Auto-initialize to 1%
  broadening_max_pct = 10,  # Maximum 10% broadening
  ...
)

# Access broadening parameters
broadening_params <- result$G
```

### Output Structure

When broadening is enabled, the result includes:
```R
result$G  # Matrix of broadening parameters (samples × 2*components)
```

Column names: `G_1, G_2, ..., G_{2n}`

## Testing

A test script is provided: `test_mcr_convolution.R`

This script:
1. Generates synthetic data with known broadening
2. Compares standard MCR-ALS vs broadening-enabled MCR-ALS
3. Validates parameter recovery
4. Produces comparison plots

Run the test:
```R
source("test_mcr_convolution.R")
```

Expected outcome: Broadening-enabled ALS should achieve lower LOF (Lack of Fit) when data contains sample-specific broadening.

## Performance Considerations

- **Computational cost**: ~3x slower than standard ALS due to third optimization step
- **Convergence**: May require more iterations; adjust `maxiter` if needed
- **Initialization**: G parameters start at **1%** of spectral range
- **Bounds**: Minimum 0.1%, Maximum user-defined (default **10%**)
- **Optimization method**: L-BFGS-B with box constraints for G parameters
- **Scale-aware**: Automatically adapts to data resolution (percentage-based)

## Visualization

When broadening is enabled:
- **Kinetics plot** displays both C (concentration) and G (broadening) parameters
- Plot title changes to "Kinetics + Broadening" 
- Y-axis label: "C / G" to indicate mixed content
- G parameters appear as additional traces showing how broadening varies across samples
- Legend includes all parameter names (C_1, C_2, ..., G_1, G_2, ...)

## Mathematical Details

### Gaussian Kernel

For sigma σ, the Gaussian kernel is:
```
K(x) = exp(-x²/(2σ²)) / Σexp(-x²/(2σ²))
```

Kernel size: `6σ` (truncated at ±3σ)

### Edge Handling

Convolution uses reflection padding to minimize edge artifacts:
```R
padded = [S[1]...S[1], S[1:n], S[n]...S[n]]
```

### Convergence Criterion

Broadening converges when:
```
|ΔError| < threshold
```

Where error is the relative reconstruction error:
```
Error = ||D - C·(S⊗G)^T||² / ||D||²
```

## Troubleshooting

### Issue: Broadening parameters go to minimum (0.01)
**Cause**: Data may not actually have broadening variation  
**Solution**: Check if sample-specific broadening exists in data

### Issue: Slower convergence
**Cause**: Additional optimization step  
**Solution**: Increase `maxiter` parameter or adjust convergence threshold

### Issue: G parameters oscillate
**Cause**: Ill-conditioned problem or insufficient constraints  
**Solution**: Ensure sufficient spectral resolution and component separation

## References

Based on the plan document specifying MCR-ALS with Convolution for WAXS Broadening analysis.

## Implementation Files

- **UI**: `ui_files/ALSInputConstraintsSpectra.R` - Added "Broadening" checkbox
- **Core Algorithm**: `server_files/ALS.R` - Modified functions:
  - `convolve_spectrum()` - Gaussian convolution helper
  - `optimize_broadening_single()` - G optimization for single sample
  - `getC()` - Modified to accept G parameter
  - `getS()` - Modified to accept G parameter  
  - `myals()` - Three-step optimization loop
  - `als()` - Wrapper with broadening support
- **Test**: `test_mcr_convolution.R` - Validation script

## Future Enhancements

Potential improvements:
- Alternative broadening functions (Lorentzian, Voigt)
- 2D broadening for detector data
- Regularization on G parameters (smoothness across samples)
- Parallel optimization of G parameters
- Adaptive sigma bounds based on data resolution
