# Per-Component Spectra Constraints

## Overview

SK-Ana now supports **per-component non-negativity constraints** for ALS spectra, allowing you to specify which spectra must be positive and which can have negative values.

## Why Per-Component Constraints?

### Use Cases

1. **Difference Spectra**
   - Excited state absorption minus ground state bleaching
   - Product formation minus reactant depletion
   - Example: S₁ (absorption) > 0, S₂ (bleaching) unconstrained

2. **Decay-Associated Spectra (DAS)**
   - Some components represent decay, others may represent rise
   - Negative amplitudes can indicate population transfer
   - Example: S₁, S₂ > 0, S₃ can be negative

3. **Mixed Systems**
   - Combination of absorption and emission
   - Positive and negative contributions to signal
   - Example: S₁, S₂, S₃ > 0, S₄ (correction term) unconstrained

4. **Baseline Corrections**
   - Most spectra physical (positive)
   - One component for baseline drift (can be negative)
   - Example: S₁ to Sₙ₋₁ > 0, Sₙ unconstrained

## How to Use

### Step 1: Enable Per-Component Mode

1. Go to **ALS** tab
2. Click **S const.** sub-tab  
3. Check **"Per-component constraints"** checkbox

### Step 2: Configure Individual Constraints

Once enabled, you'll see checkboxes for each spectrum:

```
S_1 > 0  ☑  
S_2 > 0  ☑  
S_3 > 0  ☐  (unchecked = can be negative)
S_4 > 0  ☐  
```

- **Checked** (☑) = Spectrum must be **non-negative** (S ≥ 0)
- **Unchecked** (☐) = Spectrum is **unconstrained** (can be positive or negative)

### Step 3: Run ALS

Click **Run** to execute ALS with the per-component constraints.

## Examples

### Example 1: All Positive Except Last

**Scenario:** 3-component system where first two are physical absorptions, third is a correction term.

**Configuration:**
```
S_1 > 0  ☑  
S_2 > 0  ☑  
S_3 > 0  ☐  (unconstrained)
```

**Result:** S₁ and S₂ will be constrained to positive values, S₃ can have negative regions.

### Example 2: First 3 Positive, Last 2 Negative

**Scenario:** 5-component DAS analysis with difference spectra.

**Configuration:**
```
S_1 > 0  ☑  
S_2 > 0  ☑  
S_3 > 0  ☑  
S_4 > 0  ☐  (difference spectrum)
S_5 > 0  ☐  (difference spectrum)
```

**Result:** S₁, S₂, S₃ positive; S₄ and S₅ can be negative.

### Example 3: All Unconstrained (DAS Mode)

**Scenario:** Pure DAS analysis where all components can have mixed signs.

**Configuration:**
```
S_1 > 0  ☐  
S_2 > 0  ☐  
S_3 > 0  ☐  
```

**Result:** All spectra can have both positive and negative regions.

## Technical Details

### Algorithm

The implementation uses a **two-stage least squares approach** for mixed constraints:

1. **Stage 1:** Solve for unconstrained components using QR decomposition
2. **Stage 2:** Solve for constrained components using NNLS on the residual

For wavelength λᵢ:
```
# Separate indices
idx_pos  = {j | nonnegS[j] = TRUE}
idx_free = {j | nonnegS[j] = FALSE}

# Solve unconstrained first
S[λᵢ, idx_free] = (Cᵀ_free C_free)⁻¹ Cᵀ_free D[λᵢ]

# Then solve constrained with residual
resid = D[λᵢ] - C_free · S[λᵢ, idx_free]
S[λᵢ, idx_pos] = NNLS(C_pos, resid)
```

### Performance

- **Same constraint for all:** Fast path (unchanged from original)
- **Mixed constraints:** ~10-30% slower depending on mix ratio
- **All unconstrained:** Equivalent to original unconstrained mode

### Optimization

When all components have the same constraint (all positive or all unconstrained), the code automatically uses the optimized fast path.

## Tips & Best Practices

### When to Use Per-Component

✅ **Use when:**
- You have physical reasoning for mixed signs
- Dealing with difference spectra
- DAS analysis requires negative components
- One component is a correction/baseline term

❌ **Avoid when:**
- All spectra should be physical (use global constraint)
- No clear justification for negative values
- Data is pure absorption without bleaching

### Starting Point

1. **Start with all positive** (global constraint)
2. Examine residuals and fit quality
3. If residuals show systematic patterns, try per-component
4. Enable negative for specific components based on physical interpretation

### Validation

After using per-component constraints:

1. **Check residuals:** Should be featureless noise
2. **Inspect negative regions:** Do they make physical sense?
3. **Compare LOF:** Should be similar or better than global constraint
4. **Physical interpretation:** Can you explain negative values?

## Compatibility

### Backward Compatible

✅ The feature is **fully backward compatible**:
- Default behavior unchanged (global constraint)
- Existing projects work without modification
- Per-component mode is opt-in

### Other Constraints

Per-component constraints work with:
- ✅ Unimodality
- ✅ Normalization (intensity or L1)
- ✅ Smoothing
- ✅ Fixed spectral shapes
- ✅ Correction spectra
- ⚠️  **Note:** Unimodality may conflict with negative regions

## Limitations

1. **Unimodality conflict:** Unimodal constraint assumes single peak; may not work well with negative regions

2. **Normalization:** Normalization uses absolute values when negative regions present

3. **Interpretation:** Negative values require careful physical interpretation

4. **Convergence:** Mixed constraints may require more iterations to converge

## File Locations

**Modified Files:**
- `ui_files/ALSInputConstraintsSpectra.R` - UI checkboxes
- `server_files/ALS.R` - getS function and server logic
- `docs/als.md` - User documentation
- `EXPLANATION.md` - Technical documentation

**Implementation:**
- Core algorithm: `getS()` function, lines 57-170
- UI generation: `output$perComponentSUI`, lines 718-742
- Constraint building: ALS run section, lines 1093-1103

## References

### Difference Spectra
- van Stokkum, I. H. M., et al. (2004). Global and target analysis of time-resolved spectra. *Biochim. Biophys. Acta*, **1657**, 82-104.

### DAS Analysis
- Henry, E. R., & Hofrichter, J. (1992). Singular value decomposition: Application to analysis of experimental data. *Methods Enzymol.*, **210**, 129-192.

---

**Version:** 1.0  
**Date:** 2025-11-19  
**Feature Status:** Production Ready
