# Broadening Output Format

## Console Output

When broadening is enabled, the iteration output now includes sigma (σ) values.

### Format

```
Iter. (opt. [C|S|G]): N, |RD| : X.XX > threshold, LOF(%) : XX.X, σ=(σ1, σ2, ..., σn)
```

### Examples

**2 components (dimensions):**
```
Iter. (opt. C): 1, |RD| : 0.945 > 1e-06, LOF(%) : 23.4, σ=(1.01, 0.982)
Iter. (opt. S): 2, |RD| : 0.753 > 1e-06, LOF(%) : 18.7, σ=(1.01, 0.982)
Iter. (opt. G): 3, |RD| : 0.612 > 1e-06, LOF(%) : 16.3, σ=(1.15, 1.08)
Iter. (opt. C): 4, |RD| : 0.489 > 1e-06, LOF(%) : 14.8, σ=(1.15, 1.08)
...
Iter. (opt. G): 30, |RD| : 0.000012 > 1e-06, LOF(%) : 2.34, σ=(2.45, 1.87)
```

**3 components:**
```
Iter. (opt. C): 1, |RD| : 0.945 > 1e-06, LOF(%) : 23.4, σ=(1.01, 0.982, 1.03)
Iter. (opt. S): 2, |RD| : 0.753 > 1e-06, LOF(%) : 18.7, σ=(1.01, 0.982, 1.03)
Iter. (opt. G): 3, |RD| : 0.612 > 1e-06, LOF(%) : 16.3, σ=(1.15, 1.08, 1.21)
...
```

### Interpretation

- **σ values**: Mean broadening across all samples (in absolute units)
- **Values shown**: Average of G matrix columns
- **Updates**: Changes when step = "G" (broadening optimization step)

### Step Types

- **C**: Optimizing concentrations
- **S**: Optimizing spectral profiles
- **G**: Optimizing broadening parameters (only when broadening enabled)

## Visualization

### Kinetics Plot with Inset

When broadening is enabled, the kinetics plot includes:

1. **Main plot**: C and G parameters together
   - Solid lines with points: C_1, C_2, G_1, G_2
   - All parameters on same scale

2. **Inset plot** (top-left corner):
   - Shows **only** G parameters
   - For 2+ components: Plots G_1 and G_n (first and last)
   - Size: ~25% of main plot
   - Y-axis: σ (sigma)
   - Legend: "G_1", "G_n"

#### Example Layout (2 dimensions)

```
┌────────────────────────────────────────────┐
│ ┌──────────────┐                           │
│ │ Broadening(σ)│                           │
│ │  G_1 ──────  │   C_1 ───────────         │
│ │  G_2 ──────  │   C_2 ───────────         │
│ └──────────────┘   G_1 ───────────         │
│                    G_2 ───────────         │
│                                            │
│              Kinetics + Broadening         │
│                                            │
│           Delay  ───────────────>          │
└────────────────────────────────────────────┘
```

### Spectra Plot

Shows both unbroadened and broadened profiles:

```
┌────────────────────────────────────────────┐
│                                            │
│   ──────  S_1 (unbroadened, solid)        │
│   ──────  S_2 (unbroadened, solid)        │
│   ┈┈┈┈┈┈  S_1 broadened (dashed)          │
│   ┈┈┈┈┈┈  S_2 broadened (dashed)          │
│                                            │
│  Spectra (solid=unbroadened,              │
│          dashed=broadened) / LOF: 2.3%    │
│                                            │
│        Wavelength  ───────────────>        │
└────────────────────────────────────────────┘
```

## Code Examples

### Reading Console Output

The console output is HTML formatted with `<br/>` tags:

```R
# Example output string:
"Iter. (opt. C): 1, |RD| : 0.945 > 1e-06, LOF(%) : 23.4, σ=(1.01, 0.982)<br/>"
```

### Accessing G Values Programmatically

```R
result <- myals(..., broadening = TRUE)

# G matrix: samples × components
result$G
#         G_1   G_2
# [1,]   1.15  1.08
# [2,]   1.23  1.14
# [3,]   1.89  1.45
# ...

# Mean G values (as shown in console)
colMeans(result$G)
# G_1   G_2 
# 1.42  1.22

# Convert to percentages
spectral_range <- nrow(result$S)
colMeans(result$G) / spectral_range * 100
# G_1   G_2 
# 1.42% 1.22%
```

## Implementation Details

### Console Output Generation

```R
# In myals() iteration loop:
if (!silent && (iter %% update_interval == 1)) {
  lof_iter <- 100 * sqrt(rss)
  
  # Determine step type
  if (broadening) {
    step_type <- if (iter %% 3 == (b %% 3)) "S" 
                 else if (iter %% 3 == ((b + 1) %% 3)) "C" 
                 else "G"
  } else {
    step_type <- ifelse(iter %% 2 == b, "S", "C")
  }
  
  msg <- paste0(
    "Iter. (opt. ", step_type, "): ", iter,
    ", |RD| : ", signif(abs(RD), 3), " > ", thresh,
    ", LOF(%) : ", signif(lof_iter, 3)
  )
  
  # Add G values
  if (broadening) {
    G_mean <- colMeans(G)
    g_str <- paste0("(", paste(signif(G_mean, 3), collapse=", "), ")")
    msg <- paste0(msg, ", σ=", g_str)
  }
  
  cat(msg, "<br/>")
}
```

### Inset Plot Creation

```R
# In plotAlsVec() for type = "Kin":
if (has_broadening) {
  # Get G parameters only
  n_comp <- ncol(alsOut$C)
  G_only <- alsOut$G[, 1:n_comp, drop=FALSE]
  
  # Define inset position (top-left, 25% size)
  usr_orig <- par("usr")
  x_range <- usr_orig[2] - usr_orig[1]
  y_range <- usr_orig[4] - usr_orig[3]
  
  # Create subplot
  par(fig = c(...), new=TRUE, mar=c(2,2,1,1))
  
  # Plot G_1 and G_n
  matplot(x, G_only[, c(1, ncol(G_only))],
          type = "l", lwd = 1.5,
          ylab = expression(sigma),
          main = expression(paste("Broadening (", sigma, ")")))
  
  legend("topright", 
         legend = c("G_1", paste0("G_", ncol(G_only))))
}
```

## Benefits

### Enhanced Output

1. **Immediate feedback**: See σ values during optimization
2. **Track convergence**: Watch how σ changes across iterations
3. **Verify behavior**: Ensure σ values are reasonable (1-10% range)
4. **Debug issues**: Identify if σ values hit bounds

### Inset Plot Advantages

1. **Focused view**: G parameters isolated from C
2. **Scale appropriate**: Y-axis scaled to σ range (not 0-1 like C)
3. **Key components**: Shows first and last (often most informative)
4. **Space efficient**: Doesn't require separate plot

### Dual Spectra View

1. **Compare profiles**: See effect of broadening
2. **Validate results**: Check if broadening is realistic
3. **Physical insight**: Understand what broadening does
4. **Quality check**: Ensure unbroadened profiles are sharp

## Troubleshooting

### Issue: σ values not shown in console
**Cause**: Broadening not enabled or silent mode  
**Solution**: Set `broadening = TRUE` and `silent = FALSE`

### Issue: Inset plot not appearing
**Cause**: G matrix not in result or plotting area too small  
**Solution**: Ensure broadening is enabled and plot window is large enough

### Issue: σ values seem too large/small
**Cause**: Different spectral resolution than expected  
**Solution**: Check `spectral_range` and adjust `broadening_max_pct`

### Issue: G_1 and G_n look identical in inset
**Cause**: Components may have similar broadening  
**Solution**: This is normal if samples are homogeneous

## Summary

**New features:**
1. ✅ Console output includes σ=(σ1, σ2, ...) after each iteration
2. ✅ Kinetics plot has inset showing G_1 and G_n evolution
3. ✅ Spectra plot overlays broadened profiles (dashed) on unbroadened (solid)
4. ✅ Step type shows "G" when optimizing broadening

**Output format:**
```
Iter. (opt. C): 1, |RD| : 0.945 > 1e-06, LOF(%) : 23.4, σ=(1.01, 0.982)
```

**Visualization:**
- Main kinetics: C + G together
- Inset: G parameters only (G_1, G_n)
- Spectra: Solid (sharp) + Dashed (broadened)

This provides complete visibility into broadening optimization!
