# Correction Spectra ALS Extension - Integration Guide

## Overview
This extension adds support for **coupled correction spectra** in MCR-ALS analysis without modifying the existing ALS code. It allows you to:

- Fix n spectral shapes (from reference files)
- Automatically create n paired correction spectra (small, orthogonal deviations)
- Add a free spectra components
- Result: k = 2n + a total dimensions

## Files Created

### 1. Core Algorithm (`server_files/ALS_CorrectionSpectra.R`)
Implements specialized ALS functions:
- `getC_Coupled()` – Solve for concentrations with pairwise coupling
- `getS_Coupled()` – Solve for spectra with orthogonality constraints
- `myals_Coupled()` – Main ALS iteration loop with coupling
- `als_Coupled()` – Wrapper for sequential dimension growth

**When used:** Automatically called when user enables "Correction Spectra" option

### 2. UI Panel (`ui_files/ALSInputConstraintsCorrectionSpectra.R`)
New UI controls that appear **only when fixed spectra are loaded**:
- Checkbox: "Enable correction spectra"
- Slider: "Correction penalty (λ)" (controls regularization)
- Checkbox: "Zero-mean corrections"
- Help text & interpretation guide

**Integration:** Sourced from `ALSInputConstraintsSpectra.R` (already updated)

### 3. Server Integration (`server_files/ALS_CorrectionSpectra_Server.R`)
Enhanced ALS run logic that:
- Detects if correction spectra are enabled
- Routes to `als_Coupled()` or standard `als()` based on user choice
- Passes new parameters (nFixed, lambdaCorr, useCorrectionSpectra)

**Integration:** Must be sourced **after** ALS.R in server.R

### 4. Documentation (`docs/correction_spectra.md`)
Complete user guide with:
- Mathematical model description
- Usage workflow (3 steps)
- Parameter interpretation
- Troubleshooting
- Output interpretation

## Installation Steps

### Step 1: Update server.R
Add this line **after** the existing `source("server_files/ALS.R")` line:

```r
# ALS Correction Spectra Extension
source("server_files/ALS_CorrectionSpectra_Server.R")
```

**Location in server.R:** Look for line ~14 where ALS.R is sourced, add immediately after.

### Step 2: Verify UI Integration
The UI panel is already included in `ui_files/ALSInputConstraintsSpectra.R`:

```r
# Correction spectra extension
source('ui_files/ALSInputConstraintsCorrectionSpectra.R')
```

**No changes needed** – it's already in the file.

### Step 3: Test Integration

1. Restart the Shiny app
2. Go to **ALS Constraints → Spectra**
3. Upload a fixed spectrum file ("Fix spectral shape(s)")
4. Verify new panel **"Correction Spectra"** appears
5. Check the "Enable correction spectra" checkbox
6. Set λ slider and run ALS

## File Locations

```
C:\dev\SK-Ana\
├── server_files/
│   ├── ALS.R                              (existing, unchanged)
│   ├── ALS_CorrectionSpectra.R            (NEW)
│   └── ALS_CorrectionSpectra_Server.R     (NEW)
├── ui_files/
│   ├── ALSInputConstraintsSpectra.R       (modified: source() added)
│   └── ALSInputConstraintsCorrectionSpectra.R  (NEW)
└── docs/
    ├── als.md                             (existing)
    └── correction_spectra.md              (NEW)
```

## Backward Compatibility

✅ **Fully backward compatible:**
- Standard ALS works exactly as before
- Correction spectra only enabled when:
  1. Fixed spectra are loaded, AND
  2. User checks "Enable correction spectra"
- If disabled → routes to original `als()` function
- **No existing code is modified** (only extended)

## Key Parameters

### In ALS_CorrectionSpectra_Server.R

When calling `als_Coupled()`:

| Parameter | Type | Default | Meaning |
|-----------|------|---------|---------|
| `nFixed` | int | 0 | Number of fixed spectra (= ncol(S0)) |
| `useCorrectionSpectra` | bool | FALSE | Enable coupled correction mode |
| `lambdaCorr` | float | 0 | Tikhonov λ = 10^(slider_value) |
| (all others) | - | - | Same as standard `als()` |

### In getC_Coupled(), getS_Coupled()

Both take `nFixed` parameter:
- Projects C_corr onto C_fix direction (keeps same kinetics)
- Orthogonalizes S_corr to S_fix (removes parallel component)

## How It Works: Workflow

```
User loads fixed spectra
         ↓
UI shows "Correction Spectra" panel
         ↓
User enables & sets λ
         ↓
User clicks "Run ALS"
         ↓
doALS_Enhanced observer fires
         ↓
Check: useCorrectionSpectra && nFixed > 0?
         ├─ YES → call als_Coupled()
         └─ NO → call als() (standard)
         ↓
Run in background (callr::r_bg)
         ↓
Results → resALS$results
         ↓
Display with automatic structure labels
```

## Example Output

**Setup:**
- 2 fixed spectra (S0)
- Correction spectra enabled
- nAls = 5 (total dimensions)

**Result:**
```
S_1 / C_1  → Fixed spectrum 1 (reference A)
S_2 / C_2  → Fixed spectrum 2 (reference B)  
S_3 / C_3  → Correction for S_1 (orthogonal to S_1, C_3 ∝ C_1)
S_4 / C_4  → Correction for S_2 (orthogonal to S_2, C_4 ∝ C_2)
S_5 / C_5  → Free spectrum (independent)
```

## Customization

### Changing Default λ
In `ALSInputConstraintsCorrectionSpectra.R`, line 43:
```r
value = -2,  # Change this to adjust default slider value
```

### Modifying Orthogonality Enforcement
In `getS_Coupled()`, lines 142-170: Adjust orthogonalization logic

### Changing Regularization Formula
In `getS_Coupled()`, lines 163-165: Modify penalty calculation

## Troubleshooting

### Correction Spectra panel doesn't appear
- **Check:** Are fixed spectra actually loaded?
- **Fix:** Upload spectrum file in "Fix spectral shape(s)"

### Functions not found error
- **Check:** Is `ALS_CorrectionSpectra_Server.R` sourced in server.R?
- **Fix:** Add `source()` line as described in Step 1

### Background process fails silently
- **Check:** stdout/stderr in console
- **File:** `C:\Users\denisov\AppData\Local\Temp\als_*.stdout`
- **Look for:** "Running ALS (dim = X, coupled)" message

## References & Theory

See `docs/correction_spectra.md` for:
- Mathematical formulation
- Physical interpretation
- Advanced usage
- Parameter selection guide

---

## Support

For questions or issues:
1. Check `docs/correction_spectra.md`
2. Review console output (browser developer tools → Console)
3. Check temp log file: `tempdir() + "/als_*.stdout"`
4. Examine result structure with `str(resALS$results)`
