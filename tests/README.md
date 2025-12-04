# SK-Ana Tests

Test files and debugging scripts for SK-Ana.

## Test Files

| File | Description |
|------|-------------|
| `test_als_simple.R` | Simple ALS algorithm tests |
| `test_mcr_convolution.R` | MCR convolution tests |
| `test_pca_init.R` | PCA initialization tests |
| `debug_als.R` | ALS debugging script |

## Running Tests

To run a test file:

```r
source("tests/test_als_simple.R")
```

Or from RStudio:
1. Open the test file
2. Click "Source" or press Cmd/Ctrl + Shift + S

## Test Coverage

These tests cover:
- ALS algorithm functionality
- MCR convolution operations
- PCA initialization
- Debugging scenarios

---

**Note**: These are development/debugging scripts, not formal unit tests.
