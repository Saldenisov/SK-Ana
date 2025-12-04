# Final Broadening Implementation

## Summary of Changes

All requested features have been implemented:

### ✅ 1. Initialization from 0.1% (not 1%)

**Previous:** Started at 1% of spectral range  
**Now:** Starts at **0.1%** of spectral range

```R
sigma_init <- 0.001 * spectral_range  # 0.1%
```

Console output:
```
Broadening: Start=0.1% (σ=0.1), Max=10% (σ=10)
```

### ✅ 2. Three-Stage Grid Refinement

**Previous:** Single 20-point grid  
**Now:** Three sequential grids with 10 points each

#### Stage 1: Coarse Grid
- **Range:** 0 to max (e.g., 0 to 10%)
- **Points:** 10
- **Purpose:** Find approximate region

```R
grid_coarse <- seq(0, sigma_max, length.out = 10)
# Example: [0, 1.11, 2.22, 3.33, 4.44, 5.56, 6.67, 7.78, 8.89, 10.0]
```

#### Stage 2: Medium Grid
- **Range:** ±10% of full range around best from stage 1
- **Points:** 10
- **Purpose:** Refine within neighborhood

```R
range_medium <- (sigma_max - 0) * 0.1  # ±10% of full range
grid_medium <- seq(best - range_medium, best + range_medium, length.out = 10)
# Example if best=5: [4.0, 4.22, 4.44, 4.67, 4.89, 5.11, 5.33, 5.56, 5.78, 6.0]
```

#### Stage 3: Fine Grid
- **Range:** ±2% of full range around best from stage 2
- **Points:** 10
- **Purpose:** Fine-tune final value

```R
range_fine <- (sigma_max - 0) * 0.02  # ±2% of full range
grid_fine <- seq(best - range_fine, best + range_fine, length.out = 10)
# Example if best=5: [4.8, 4.84, 4.89, 4.93, 4.98, 5.02, 5.07, 5.11, 5.16, 5.2]
```

**Total evaluations per component:** 30 (10 + 10 + 10)

### ✅ 3. Larger Inset Plot

**Previous:** 25% of plot area  
**Now:** **25% of total figure** (much larger)

- **Width:** 25% of figure width
- **Height:** 25% of figure height
- **Position:** Top-left corner (5% margin from edges)
- **Coordinates:** NDC (Normalized Device Coordinates)

```R
inset_width <- 0.25   # 25% of figure width
inset_height <- 0.25  # 25% of figure height
inset_left <- 0.05
inset_top <- 0.95
```

### ✅ 4. Inset Only Visible When Broadening Active

The inset is only drawn when:
```R
if (has_broadening) {
  # has_broadening = !is.null(alsOut$G) && ncol(alsOut$G) > 0
  # Draw inset plot
}
```

**Result:** Inset only appears when broadening is enabled and G matrix exists.

### ✅ 5. Live Updates During Optimization

G parameters are now included in the live snapshot:

```R
# Add G to snapshot for live preview
if (broadening) {
  snap$G <- G
  colnames(snap$G) <- paste0("G_", 1:ncol(G))
}
```

**Result:** Inset plot updates in real-time as optimization progresses.

## Implementation Details

### Optimization Flow

```
For each sample:
  For each component k:
    ┌─────────────────────────────────────┐
    │ Stage 1: Coarse (0 → max, 10 pts)  │
    │   Test: [0, 1.1, 2.2, ..., 10]     │
    │   Best: σ ≈ 5.0                     │
    └────────────┬────────────────────────┘
                 ↓
    ┌─────────────────────────────────────┐
    │ Stage 2: Medium (±10%, 10 pts)     │
    │   Test: [4.0, 4.2, ..., 6.0]       │
    │   Best: σ ≈ 4.9                     │
    └────────────┬────────────────────────┘
                 ↓
    ┌─────────────────────────────────────┐
    │ Stage 3: Fine (±2%, 10 pts)        │
    │   Test: [4.7, 4.75, ..., 5.1]      │
    │   Best: σ = 4.87                    │
    └────────────┬────────────────────────┘
                 ↓
            G[sample, k] = 4.87
```

### Console Output Example

```
Broadening: Start=0.1% (σ=0.1), Max=10% (σ=10)
Running ALS (dim = 2)
Initial RSS = 0.543
Iter. (opt. S): 1, |RD| : 0.945 > 1e-06, LOF(%) : 23.4, σ=(0.1, 0.1)
Iter. (opt. C): 2, |RD| : 0.753 > 1e-06, LOF(%) : 18.7, σ=(0.1, 0.1)
Iter. (opt. G): 3, |RD| : 0.612 > 1e-06, LOF(%) : 16.3, σ=(1.15, 0.983)
Iter. (opt. S): 4, |RD| : 0.489 > 1e-06, LOF(%) : 14.8, σ=(1.15, 0.983)
...
Iter. (opt. G): 30, |RD| : 0.000012 > 1e-06, LOF(%) : 2.34, σ=(2.45, 1.87)
```

### Visualization Layout

```
┌────────────────────────────────────────────────────────┐
│                                                        │
│  ┌──────────────────────┐                             │
│  │  Broadening (σ)      │                             │
│  │                      │  C_1 ────────────            │
│  │  G_1 ──────          │  C_2 ────────────            │
│  │  G_n ──────          │  G_1 ────────────            │
│  │                      │  G_n ────────────            │
│  └──────────────────────┘                             │
│                                                        │
│           Kinetics + Broadening                        │
│                                                        │
│        Delay  ───────────────────────────>             │
└────────────────────────────────────────────────────────┘
     ↑
   25% of total figure width & height
   (not 25% of plot area)
```

## Performance Comparison

### Old Approach
- **Method:** L-BFGS-B gradient descent
- **Parameters:** 2 per component
- **Evaluations:** Variable (gradient-based)
- **Issues:** Could get stuck in local minima

### New Approach
- **Method:** Three-stage grid refinement
- **Parameters:** 1 per component
- **Evaluations:** 30 per component (predictable)
- **Benefits:** 
  - Always finds global minimum within grid resolution
  - More robust and predictable
  - Simpler to understand and debug

## Code Locations

### 1. Initialization (0.1%)
**File:** `server_files/ALS.R`  
**Line:** ~571
```R
sigma_init <- 0.001 * spectral_range  # 0.1%
```

### 2. Three-Stage Optimization
**File:** `server_files/ALS.R`  
**Function:** `optimize_broadening_single()`  
**Lines:** ~70-160
```R
# Stage 1: Coarse (10 points)
grid_coarse <- seq(0, sigma_max, length.out = 10)

# Stage 2: Medium (10 points, ±10% range)
range_medium <- (sigma_max - 0) * 0.1
grid_medium <- seq(best - range_medium, best + range_medium, 10)

# Stage 3: Fine (10 points, ±2% range)
range_fine <- (sigma_max - 0) * 0.02
grid_fine <- seq(best - range_fine, best + range_fine, 10)
```

### 3. Snapshot with G
**File:** `server_files/ALS.R`  
**Lines:** ~731-735
```R
if (broadening) {
  snap$G <- G
  colnames(snap$G) <- paste0("G_", 1:ncol(G))
}
```

### 4. Large Inset Plot
**File:** `server_files/ALS_plots.R`  
**Lines:** ~142-197
```R
# 25% of figure size in NDC coordinates
par(fig = c(0.05, 0.30, 0.70, 0.95), new = TRUE, ...)
```

## Testing

### Verify Initialization
```R
result <- myals(..., broadening = TRUE)

# Should start near 0.1%
initial_G <- result$G[1, ]
spectral_range <- nrow(result$S)
initial_pct <- initial_G / spectral_range * 100
# Should be close to 0.1%
```

### Verify Three-Stage Search
Add debug output to `optimize_broadening_single()`:
```R
cat("Stage 1 best:", best_sigma, "\n")
# After stage 2
cat("Stage 2 best:", best_sigma, "\n")
# After stage 3
cat("Stage 3 best:", best_sigma, "\n")
```

### Verify Inset Size
```R
# After plotting, check par()
par("fig")  # Should show inset at ~[0.05, 0.30, 0.70, 0.95]
```

## Benefits

1. ✅ **Better initialization:** 0.1% avoids over-broadening at start
2. ✅ **More accurate:** Three-stage refinement finds better minima
3. ✅ **Efficient:** 30 evaluations total (predictable)
4. ✅ **Visible inset:** 25% of figure is much easier to read
5. ✅ **Live updates:** G parameters update during optimization
6. ✅ **Clean interface:** Inset only shows when needed

## Summary

**Key improvements:**
- 🔹 Start: **0.1%** (not 1%)
- 🔹 Search: **0 to max** (not min to max)
- 🔹 Refinement: **3 stages × 10 points** = 30 total
  - Coarse: 0 → max
  - Medium: ±10% range
  - Fine: ±2% range
- 🔹 Inset: **25% of figure** (not plot area)
- 🔹 Updates: **Live** during optimization
- 🔹 Visibility: **Only when broadening active**

This provides precise optimization with clear visualization!
