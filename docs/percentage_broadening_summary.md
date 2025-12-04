# Percentage-Based Broadening - Implementation Summary

## Overview

The broadening parameters in MCR-ALS are now **percentage-based**, making them more intuitive and automatically scaled to the data resolution.

## Key Changes

### 1. Initialization
- **Old**: Fixed σ = 0.1
- **New**: σ = 1% of spectral range
  - 100 wavelength points → σ = 1.0
  - 1000 wavelength points → σ = 10.0
  - 2000 wavelength points → σ = 20.0

### 2. Bounds
- **Lower bound**: 0.1% of spectral range (prevents σ → 0)
- **Upper bound**: User-defined percentage (default 10%)

### 3. User Interface

New UI control in S constraints panel:

```
☑ Broadening
  └─ Max broadening (%): [10] (numeric input, range 0.1-50%)
```

**Default**: 10%  
**Tooltip**: "Maximum broadening as percentage of spectral range. Start: 1%, Max: this value"

### 4. Benefits

✅ **Scale-aware**: Automatically adapts to different data resolutions  
✅ **Intuitive**: Percentage is easier to understand than absolute sigma values  
✅ **Consistent**: Same percentage means same relative broadening across datasets  
✅ **Flexible**: User can set appropriate maximum based on their data

## Implementation Details

### Function Signatures

**myals():**
```R
myals(..., broadening = FALSE, G = NULL, broadening_max_pct = 10)
```

**optimize_broadening_single():**
```R
optimize_broadening_single(data_row, C_row, S, G_init, sigma_max = NULL)
```

### Conversion Logic

```R
spectral_range <- length(xS)  # Number of wavelength points

# Initial sigma (1%)
sigma_init <- 0.01 * spectral_range

# Maximum sigma (user-defined %)
sigma_max <- (broadening_max_pct / 100) * spectral_range

# Minimum sigma (0.1%)
sigma_min <- 0.001 * spectral_range
```

### Bounds in Optimization

```R
lower_bounds <- rep(0.001 * spectral_range, n_params)  # 0.1%
upper_bounds <- rep(sigma_max, n_params)                # User-defined %
```

## Examples

### Example 1: Low-resolution data (100 points)
- Start: 1% = σ of 1.0
- Max (10%): σ of 10.0
- Typical convergence: 2-5 points (2-5%)

### Example 2: High-resolution data (2000 points)
- Start: 1% = σ of 20.0
- Max (10%): σ of 200.0
- Typical convergence: 40-100 points (2-5%)

### Example 3: User sets Max = 5%
- More conservative upper bound
- Prevents excessive broadening
- Useful when peaks should remain sharp

## Usage

### Via UI
1. Check "Broadening" checkbox
2. Set "Max broadening (%)" to desired value (default 10%)
3. Run ALS

### Programmatically
```R
result <- myals(
  C = C_init,
  Psi = data_matrix,
  S = S_init,
  xS = wavelengths,  # Important: needed for % conversion
  broadening = TRUE,
  broadening_max_pct = 15,  # Allow up to 15% broadening
  ...
)
```

## Console Output

When broadening is enabled, the algorithm prints:
```
Broadening: Start=1% (σ=10), Max=10% (σ=100)
```

This helps users verify the absolute sigma values being used.

## Files Modified

### Core Algorithm
- `server_files/ALS.R`:
  - Modified `myals()` to accept `broadening_max_pct`
  - Modified `als()` wrapper to pass percentage parameter
  - Modified `optimize_broadening_single()` to use dynamic bounds
  - Added percentage-to-sigma conversion logic

### UI
- `ui_files/ALSInputConstraintsSpectra.R`:
  - Added `broadeningMaxPct` numeric input (conditional on broadening enabled)

### Documentation
- `docs/mcr_als_broadening.md`: Updated with percentage approach
- `docs/broadening_optimization_explained.md`: Updated examples and bounds
- `docs/percentage_broadening_summary.md`: This document

### Tests
- `test_mcr_convolution.R`: Updated to use percentage-based values

## Migration from Old Implementation

**No breaking changes** - old code will still work:
- If `broadening_max_pct` not provided, defaults to 10%
- Existing calls to `myals()` will use default value

**Recommended updates:**
- Add explicit `broadening_max_pct` parameter for clarity
- Update documentation to reference percentages instead of absolute values

## Advantages Over Fixed Values

| Aspect | Fixed (old) | Percentage (new) |
|--------|-------------|------------------|
| **Initialization** | Always 0.1 | Scales with data |
| **User control** | Edit code | UI slider |
| **Different resolutions** | Need adjustment | Automatic |
| **Interpretation** | Unclear meaning | Clear (% of range) |
| **Consistency** | Varies by dataset | Consistent meaning |

## Troubleshooting

### Issue: Broadening seems too small/large
**Solution**: Adjust "Max broadening (%)" in UI
- Too aggressive: Reduce from 10% to 5%
- Too conservative: Increase from 10% to 15-20%

### Issue: Parameters hit upper bound
**Symptom**: G values all converge to exactly `broadening_max_pct`  
**Cause**: Data may need more broadening than allowed  
**Solution**: Increase `broadening_max_pct` to 15% or 20%

### Issue: Parameters stay at 1%
**Symptom**: G values don't change from initialization  
**Cause**: Data may not have significant broadening variation  
**Solution**: This is normal - broadening may not be needed for this data

## Future Enhancements

Potential improvements:
1. **Per-component max**: Different max % for each component
2. **Auto-detection**: Estimate reasonable max % from peak widths
3. **Regularization**: Penalize G values far from 1% (L1 sparsity)
4. **Visualization**: Show % values in kinetics plot legend

## Summary

The percentage-based approach provides:
- ✅ **Better usability**: Intuitive percentage interface
- ✅ **Automatic scaling**: Adapts to data resolution
- ✅ **User control**: Easy to adjust via UI
- ✅ **Consistency**: Same meaning across different datasets
- ✅ **Physical interpretation**: % of spectral range is meaningful

**Default behavior**: Start at 1%, optimize up to 10% maximum.
