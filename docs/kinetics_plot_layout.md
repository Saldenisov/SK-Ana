# Kinetics Plot Layout

## Final Design

### Main Plot: C Only (Concentrations)
- **Shows:** Concentration profiles for all components
- **Does NOT show:** G parameters
- **Y-axis:** C (concentrations, 0-1 range)
- **Title:** "Kinetics (G in inset)" when broadening is active

### Inset Plot: G Only (Broadening)
- **Shows:** G_1 and G_n (first and last component broadening)
- **Location:** Top-left corner
- **Size:** 35% of figure width × 35% of figure height
- **Y-axis:** σ (sigma, broadening values)
- **Only visible:** When broadening is enabled

## Example Layout (2 Dimensions)

```
┌──────────────────────────────────────────────────────┐
│                                                      │
│  ┌─────────────────────┐                            │
│  │ Broadening (σ)      │                            │
│  │                     │                            │
│  │  G_1 ─────────      │   C_1 ─────────────        │
│  │  G_2 ─────────      │   C_2 ─────────────        │
│  │                     │                            │
│  │                     │                            │
│  └─────────────────────┘                            │
│                                                      │
│            Kinetics (G in inset)                     │
│                                                      │
│         Delay  ────────────────────────>             │
└──────────────────────────────────────────────────────┘
       ↑
    35% of figure (width & height)
```

## Rationale

### Why Separate C and G?

1. **Different scales:**
   - C: 0-1 (normalized concentrations)
   - G: 0-10 (absolute sigma values in points)
   - Plotting together would make one or both hard to read

2. **Different interpretations:**
   - C: Sample composition (what's in the mixture)
   - G: Measurement artifact (broadening due to thickness, etc.)

3. **Clarity:**
   - Main plot focuses on chemistry (concentrations)
   - Inset shows instrumental effect (broadening)

4. **Visual hierarchy:**
   - C is primary result → main plot
   - G is secondary (correction factor) → inset

## Previous vs. New Layout

### Previous (Incorrect)
```
Main plot shows: C_1, C_2, G_1, G_2
Problem: 4 traces with different scales mixed together
```

### New (Correct)
```
Main plot: C_1, C_2 only
Inset: G_1, G_2 only
Benefits: Clear separation, appropriate scales
```

## For Different Numbers of Components

### 1 Component
**Main plot:** C_1  
**Inset:** G_1 only (single line)

### 2 Components
**Main plot:** C_1, C_2  
**Inset:** G_1, G_2 (both shown)

### 3+ Components
**Main plot:** C_1, C_2, C_3, ...  
**Inset:** G_1, G_n (first and last only, to avoid clutter)

## Inset Size Details

### Dimensions
- **Width:** 35% of figure
- **Height:** 35% of figure
- **Margin from edges:** 3%

### Position (NDC coordinates)
```R
inset_left <- 0.03
inset_right <- 0.38  # 0.03 + 0.35
inset_bottom <- 0.62 # 1 - 0.03 - 0.35
inset_top <- 0.97    # 1 - 0.03
```

### Visual Properties
- **Line width:** 3 (thicker for visibility)
- **Font sizes:**
  - Title: 1.1× normal
  - Axis labels: 1.1× normal
  - Tick labels: 1.0× normal
  - Legend: 1.0× normal
- **Box:** 2pt border

## Code Summary

```R
# Main plot - ALWAYS shows only C
matplot(x, alsOut$C,
        ylab = "C",
        main = if(has_broadening) "Kinetics (G in inset)" else "Kinetics")

# Inset plot - ONLY when broadening is active
if (has_broadening) {
  par(fig = c(0.03, 0.38, 0.62, 0.97), new = TRUE)
  
  # Plot G_1 and G_n
  matplot(x, alsOut$G[, c(1, ncol(alsOut$G))],
          ylab = expression(sigma),
          main = expression(paste("Broadening (", sigma, ")")),
          lwd = 3)
}
```

## Benefits of Final Design

1. ✅ **Clear visual separation** between C and G
2. ✅ **Appropriate scales** for each parameter type
3. ✅ **No clutter** in main plot (only concentrations)
4. ✅ **Large inset** (35%) easy to read
5. ✅ **Shows trends** in G across samples
6. ✅ **Only appears when needed** (broadening active)
7. ✅ **Informative title** indicates where G is located

## User Experience

**Without broadening:**
- Main plot: Concentrations only
- No inset (clean, simple)

**With broadening:**
- Main plot: Concentrations (chemistry)
- Inset: Broadening parameters (instrumental correction)
- Title indicates G is in inset
- Clear, professional visualization

This provides optimal clarity for both cases!
