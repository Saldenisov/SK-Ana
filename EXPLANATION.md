# SK-Ana Methods & Algorithms Explanation

## Overview

This document explains the analytical methods and algorithms implemented in SK-Ana for spectro-kinetic data analysis.

---

## 1. Singular Value Decomposition (SVD)

### Purpose
- Estimate the number of significant components above noise
- Denoise data by reconstruction from significant singular values
- Inform decomposition complexity

### Mathematical Model
Given data matrix **D** (n_delay × n_wavl):

```
D = U · Σ · V^T
```

Where:
- **U**: Left singular vectors (n_delay × k) - kinetics basis
- **Σ**: Diagonal matrix of singular values (k × k)
- **V**: Right singular vectors (n_wavl × k) - spectra basis
- **k**: Number of components

### Process
1. Compute SVD: `D ≈ U · Σ · V^T`
2. Analyze singular values spectrum
3. Identify noise baseline (from largest singular values)
4. Determine significant components (those above noise)
5. Reconstruct with k components for denoising

### Outputs
- **Singular values**: Scree plot showing component importance
- **Lack-of-fit**: Signal percentage not represented vs. components
- **Vectors**: Wavelength-wise and delay-wise singular vectors
- **Residuals**: Data - Model comparison

### Key Parameters
- **Dimension**: Number of singular values to use for reconstruction

---

## 2. Multi-Curve Resolution - Alternating Least Squares (MCR-ALS)

### Purpose
Factorize data matrix into physically meaningful spectra and kinetic profiles with constraints.

### Mathematical Model
```
D ≈ C · S^T
```

Where:
- **D**: Data matrix (n_delay × n_wavl)
- **C**: Concentration profiles/kinetics (n_delay × k)
- **S**: Pure component spectra (n_wavl × k)
- **k**: Number of species

### Algorithm (Alternating Optimization)

**Initialization:**
- Option 1: |SVD| (absolute values of SVD vectors)
- Option 2: NMF (Non-negative Matrix Factorization)
- Option 3: Sequential (build up dimensions iteratively)
- Option 4: Restart (from previous run)

**Iteration Loop:**
```
1. Fix S, optimize C:
   C = argmin ||D - C·S^T||² subject to constraints on C

2. Fix C, optimize S:
   S = argmin ||D - C·S^T||² subject to constraints on S

3. Check convergence:
   If ||D - C·S^T|| < threshold, STOP
   Else, repeat from step 1
```

### Constraints

**On Spectra (S):**
- **Non-negativity** (S ≥ 0): Physical constraint
- **Unimodality**: Each spectrum has single peak
- **Normalization**: 
  - max(S) = 1 (intensity)
  - sum(S) = 1 (area)
- **Smoothness**: Loess smoothing with span parameter
- **Fixed shapes**: Constrain to known reference spectra (hard or soft)

**On Kinetics (C):**
- **Non-negativity** (C ≥ 0): Concentration constraint
- **Closure**: Conservation of matter (sum of C = 1 at each time)
- **Presence matrix**: Specify species occurrence in tiled experiments

### Convergence
- **Max iterations**: Typically 50-500
- **Threshold**: log₁₀(convergence) typically -6 to -3
- **Metric**: Lack-of-fit vs SVD with same dimension

### Ambiguity Analysis

MCR-ALS solutions are not unique due to **rotational ambiguity**:

```
D = C·S^T = (C·T)·(T^(-1)·S)^T
```

For any invertible transformation matrix **T**, (C·T, S·T⁻¹) is also a valid solution.

**Exploration Process:**
1. Select 2-3 vectors to explore
2. Define positivity threshold (relative to noise)
3. Set exploration step size
4. Brute-force search transformation matrices
5. Return subset of valid (C', S') pairs satisfying constraints

**Outputs:**
- Range of feasible spectra and kinetics
- Confidence intervals/uncertainty bands
- Identifiability assessment

---

## 3. Correction Spectra Extension (MCR-ALS)

### Purpose
Refine fixed reference spectra with paired correction components when references don't perfectly fit data.

### Model Structure
```
D ≈ [C_fix  C_corr  C_free] × [S_fix^T  ]
                                [S_corr^T ]
                                [S_free^T ]
```

With **k = 2n + a** components:
- **n** fixed spectra (known reference shapes)
- **n** correction spectra (deviations from fixed)
- **a** free spectra (standard ALS)

### Constraints

**Pairwise Coupling (Kinetics):**
```
C_corr[:, i] = α_i × C_fix[:, i]
```
Correction kinetics proportional to fixed (same time profile).

**Spectral Orthogonality:**
```
S_corr[:, i] ⊥ S_fix[:, i]
```
Remove component parallel to fixed:
```
S_corr[:, i] ← S_corr[:, i] - proj(S_corr[:, i] onto S_fix[:, i])
```

**Zero-Mean:**
```
mean(S_corr[:, i]) = 0
```
Prevents baseline-like behavior.

**Tikhonov Regularization:**
```
penalty = λ · ||S_corr||₂²
```
Controls correction magnitude (λ = 10^(-4 to 0)).

### Interpretation
If correction spectra ≈ 0 → references are accurate  
If corrections have structure → systematic deviations from references

---

## 4. Hybrid Hard-Soft Modeling (HH-SM)

### Purpose
Incorporate mechanistic kinetic models (ODEs) into spectral decomposition for interpretable rate constants.

### Model
```
D ≈ C_model(k, t) · S^T
```

Where:
- **C_model(k, t)**: Kinetics from ODE integration with rate constants **k**
- **S**: Spectra (optimized by least-squares)
- **k**: Rate constants (optimized by non-linear algorithm)

### Reaction Scheme Definition

**Format:**
```
# Example: A -> B -> C
A -> B ; k1_init / uncertainty_factor
B -> C ; k2_init / uncertainty_factor

eps_A = max_extinction / F_eps
eps_B = max_extinction / F_eps
eps_C = max_extinction / F_eps

c0_A_1 = initial_conc / F_c0
c0_B_1 = initial_conc / F_c0
```

**Components:**
- Reactions: `Reactants -> Products ; k_value / unc_factor`
- Extinction coefficients: `eps_X = value / F_eps`
- Initial concentrations: `c0_X_exp = value / F_c0`

### Optimization Strategy

**Bayesian Framework:**
Maximize posterior probability:
```
p(k | D, model) ∝ p(D | k, model) · p(k)
```

**Two-Level Optimization:**
1. **Outer loop** (non-linear): Optimize rate constants **k**
2. **Inner loop** (linear): For each k, solve for S by least-squares

**Algorithms:**
- **Local search**: Around initial values within uncertainty box
- **Global search**: Multiple random starting points (genoud algorithm)

### Special Cases

**Decay-Associated Spectra (DAS):**
```
A -> 0 ; k1 / unc
B -> 0 ; k2 / unc
C -> 0 ; k3 / unc
```
Parallel decays with no reactions between species.

**Sequential Reactions:**
```
A -> B -> C ; k1, k2
```

**Complex Networks:**
```
A + B -> C ; k1
C -> D + E ; k2
```

### Identifiability Analysis

**Posterior Density:**
- Compare prior (blue) vs posterior (salmon) distributions
- Well-identified: Posterior narrower than prior
- Poorly-identified: Posterior at uncertainty box limits

**Correlation Analysis:**
- Pairwise scatterplots of parameter samples
- High correlation → difficult to identify independently

---

## 5. Data Preprocessing & Selection

### Selection Tools
- **OD Range**: Visualization only (no data exclusion)
- **Wavelength Range**: Select min/max wavelengths
- **Delay Range**: Select min/max time delays

### Masking

**Baseline Masks:**
- Define regions for baseline correction
- Average masked values to zero (delay-wise)
- Applied between consecutive masks in tiled data

**Wavelength Masks:**
- Exclude noisy wavelength regions
- Typical: Laser wavelengths, artifacts

**Delay Masks:**
- Exclude time regions
- Typical: Baseline areas, Cherenkov effects, signal rise

### Compression
- **Load-time**: Average by blocks during import
- **Post-process**: Average after tiling/assembly
- **Purpose**: Reduce noise, save processing time

---

## 6. Algorithm Details

### Non-Negative Least Squares (NNLS)

Used in ALS C-step and S-step:
```
min ||Y - X·β||²  subject to β ≥ 0
```

**R Implementation:** `nnls::nnls()`

### Unimodality Constraint

Applied after NNLS solve:
```
1. Find spectrum maximum position
2. Apply monotonic regression left of max (increasing)
3. Apply monotonic regression right of max (decreasing)
```

**R Implementation:** `Iso::pava()`

### Smoothness (Loess)

Applied to spectra after optimization:
```
S_smooth ← loess(S, span = smoothing_factor)
```

**R Implementation:** `stats::loess()`

### ODE Integration

For kinetic models:
```
dC/dt = f(C, k, t)
C(t=0) = C₀
```

**R Implementation:** `deSolve::ode()`

---

## 7. Quality Metrics

### Lack-of-Fit (LOF)
```
LOF = 100 × ||D - D_model||_F / ||D||_F
```
Where ||·||_F is Frobenius norm.

**Target:** LOF_ALS ≈ LOF_SVD (with same dimension)

### Mean Squared Error (MSE)
```
MSE = (1/n) · ||D - D_model||²
```

### Residuals Analysis
- **Visual**: Residuals map (should be featureless noise)
- **SVD of residuals**: Vectors should show no structure
- **Histogram**: Should be Gaussian
- **Q-Q plot**: Assess normality

---

## 8. Workflow Summary

```
1. Load Data (Project tab)
   ↓
2. Define Masks & Selection (Data Selection tab)
   ↓
3. SVD Analysis (determine k)
   ↓
4. Branch:
   ├─ MCR-ALS (ALS tab)
   │  ├─ Set constraints
   │  ├─ Run ALS
   │  └─ Explore ambiguity
   │
   └─ Kinetic Modeling (Kinet tab)
      ├─ Define reaction scheme
      ├─ Run optimization
      └─ Check identifiability
   ↓
5. Export Results (Downloads tab)
```

---

## 9. Best Practices

### SVD
- Always inspect singular values before ALS
- Glitch removal helps identify true components
- Use filtered matrix for ALS if noise is high

### MCR-ALS
- Start with |SVD| initialization
- Enable non-negativity unless DAS analysis
- Use sequential initialization for difficult datasets
- Check residuals SVD for structured error
- Explore ambiguity for 2-3 component systems

### Kinetic Modeling
- Start with simple schemes (DAS)
- Fix well-known rate constants (set unc_factor = 1)
- Use global optimization with restart
- Check identifiability (posterior vs prior)
- Ensure LOF_kinet ≈ LOF_SVD

### Correction Spectra
- Use when references are approximate
- Start with λ = -2 (moderate penalty)
- Keep zero-mean enabled
- Interpret corrections as systematic errors
- If corrections large (>50%), reconsider references

---

## 10. References

### Methods
- Ruckebusch, C., Sliwa, M., Pernot, P., de Juan, A., & Tauler, R. (2012). Comprehensive data analysis of femtosecond transient absorption spectra: A review. *J. Photochem. Photobiol. C*, **13**, 1–27. DOI: 10.1016/j.jphotochemrev.2011.10.002

### MCR-ALS
- Tauler, R. (1995). Multivariate curve resolution applied to second order data. *Chemometrics Intell. Lab. Syst.*, **30**, 133–146.
- Jaumot, J., Gargallo, R., Juan, A. D., & Tauler, R. (2005). A graphical user-friendly interface of MCR-ALS. *Chemometrics Intell. Lab. Syst.*, **76**, 101–110.

### Algorithms
- Lawson, C. L. & Hanson, R. J. (1974). *Solving Least Squares Problems*. Prentice-Hall. (NNLS algorithm)

---

**Version:** 3.4.7b  
**Last Updated:** 2025-11-18  
**Status:** Complete
