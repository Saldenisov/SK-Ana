# Correction Spectra MCR-ALS Extension

## Overview

The **Correction Spectra** extension enables a specialized MCR-ALS model where fixed spectral shapes are refined by paired correction spectra. This is useful when:

- You have **known/reference spectral shapes** but they may not perfectly fit your data
- You want to quantify **spectral deviations** from these references  
- You need **explicit control** over spectral components (fixed + corrections + free)

## Model Structure

With **k = 2n + a** components:

| Spectra Set | Count | Role | Constraints |
|-------------|-------|------|-------------|
| **Fixed (S_fix)** | n | Known/reference shapes | Hard-constrained, normalized to 1 |
| **Corrections (S_corr)** | n | Deviations from fixed | Orthogonal to S_fix, zero-mean, small |
| **Free (S_free)** | a | Additional components | Standard ALS (nonnegative, etc.) |

**Concentrations:**
- C_fix: Standard kinetics for fixed spectra
- C_corr: **Proportional to C_fix** (same time profile, different amplitude)
- C_free: Independent kinetics

## Mathematical Model

The decomposition is:

```
X ≈ [C_fix  C_corr  C_free] × [S_fix^T]
                               [S_corr^T]
                               [S_free^T]
```

With constraints:
- **Pairwise coupling:** C_corr[:, i] = α_i × C_fix[:, i]
- **Spectral orthogonality:** S_corr[, i] ⟂ S_fix[, i]
- **Zero-mean:** mean(S_corr[, i]) = 0
- **Regularization:** min ||S_corr||² (Tikhonov)

## How to Use

### Step 1: Load Fixed Spectra

1. Go to the **ALS Constraints → Spectra** panel
2. Upload reference/fixed spectral shapes using **"Fix spectral shape(s)"**
3. Select which fixed spectra to include in the model (checkboxes)

### Step 2: Enable Correction Spectra

1. A new panel **"Correction Spectra"** appears after fixed spectra are loaded
2. Check **"Enable correction spectra"**
3. Options appear:
   - **Correction penalty (λ):** Controls regularization strength
     - λ = -4: weak penalty, larger corrections allowed
     - λ = -2: moderate (default)
     - λ = 0: strong penalty, very small corrections
   - **Zero-mean corrections:** Recommended enabled

### Step 3: Run ALS

1. Set total number of dimensions in **"# Spectra"** box
   - For example: 2 fixed + 2 corrections + 1 free = **5 dimensions**
2. Run ALS as usual
3. **Output:** 
   - S_1, S_2: Fixed spectra (unchanged)
   - S_3, S_4: Correction spectra (paired with S_1, S_2)
   - S_5: Free spectrum
   - Kinetics automatically structured accordingly

## Interpretation

### Example: 2 Fixed Spectra + Corrections

**Input:**
- 2 reference spectra files (species A and B)

**Output dimensions (k=5):**
- **S_1 / C_1:** Species A (pure reference kinetics)
- **S_2 / C_2:** Species B (pure reference kinetics)
- **S_3 / C_3:** Correction for A (explains deviations from pure A)
- **S_4 / C_4:** Correction for B (explains deviations from pure B)
- **S_5 / C_5:** Free component (unknown species/baseline)

**Physical meaning:**
- If correction S_3 is near zero → reference A is good
- If correction S_3 has structure → reference A has systematic errors (e.g., peak shift, shoulders)
- C_3 has **same temporal behavior as C_1** but different amplitude

## Parameters Explained

### Correction Penalty (λ)

Controls how aggressively corrections are shrunk toward zero:

```
penalty_factor = 1 / (1 + λ × ||S_corr||₂)
```

- **λ = 10^-4 ≈ 0.0001:** Weak; corrections can be as large as you want
- **λ = 10^-2 = 0.01:** Moderate; corrections stay small-ish
- **λ = 10^0 = 1:** Strong; corrections are heavily damped
- **λ = 10^-3 (default slider -3):** Good starting point

### Zero-Mean Corrections

If enabled:
```
S_corr,i ← S_corr,i - mean(S_corr,i)
```

**Why?** 
- Prevents corrections from becoming a baseline shift
- Forces them to explain structured deviations (peaks, troughs)
- Recommended: **keep enabled**

## Advanced Usage

### Interpretation of Correction Magnitude

```r
# In output spectra S_3 vs S_1 (correction vs fixed):
correction_ratio <- max(abs(S_corr)) / max(abs(S_fix))
```

- **< 1%:** Reference is nearly perfect
- **1–10%:** Minor systematic deviations (acceptable)
- **10–50%:** Significant deviations (model needs review)
- **> 50%:** Reference may be wrong; check data

### Comparing Solutions

**Standard ALS (k=3):**
```
X ≈ C_1 S_1^T + C_2 S_2^T + C_3 S_3^T
```
Result: 3 unknown spectra

**Coupled Correction ALS (k=3 with 1 fixed):**
```
X ≈ C_fix S_fix^T + C_corr S_corr^T + C_free S_free^T
```
Result: 1 known, 1 correction, 1 free

**Advantage:** Interpretability (fixed component is constrained to known shape).

## Troubleshooting

### "Correction Spectra" panel doesn't appear
- **Cause:** No fixed spectra loaded
- **Fix:** Go to "Fix spectral shape(s)" and upload a reference file

### Corrections are zero or very small
- **Cause:** Reference spectra fit well
- **Fix:** This is good! It means your references are accurate.
- Or: Increase λ (move slider to -4)

### Corrections are too large (> 50% of fixed)
- **Cause:** Reference spectra don't match your data
- **Fix:** 
  - Try different reference files
  - Enable "Soft constraint" for fixed spectra (smaller weight)
  - Reduce λ to allow larger corrections

### Convergence is slow
- **Cause:** Over-regularization or conflicting constraints
- **Fix:** Try λ = -2 or -1; run more iterations

## Output Files

When you save ALS results with corrections enabled:

- **Spectra file:** S_1 (fixed), S_2 (fixed), S_3 (correction A), S_4 (correction B), S_5 (free), ...
- **Kinetics file:** C_1, C_2 (independent), C_3, C_4 (proportional to C_1, C_2), C_5 (independent), ...

**Note:** C_3 ∝ C_1 and C_4 ∝ C_2 by design.

## References

- Tauler, R. (1995). Multivariate curve resolution applied to second order data. *Chemometrics Intell. Lab. Syst.*, 30, 133–146.
- Jaumot, J., Gargallo, R., Juan, A. D., & Tauler, R. (2005). A graphical user-friendly interface of MCR-ALS. *Chemometrics Intell. Lab. Syst.*, 76, 101–110.

---

## Code Files

- **`server_files/ALS_CorrectionSpectra.R`** – Core functions (getC_Coupled, getS_Coupled, myals_Coupled, als_Coupled)
- **`ui_files/ALSInputConstraintsCorrectionSpectra.R`** – UI panel
- **`server_files/ALS_CorrectionSpectra_Server.R`** – Integration logic with main ALS
