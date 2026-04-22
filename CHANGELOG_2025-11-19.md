# SK-Ana Release Notes - November 19, 2025

## Version: latest
**Docker Image:** `saldenisov/skana:latest`  
**Release Date:** 2025-11-19  
**Image Digest:** sha256:7d5c1b7e4ecf7cd5a26595dfb057bf22be8e5572ac4107e7a8a5175f1b3cc973

---

## 🎉 New Features

### PCA Initialization for ALS
Added **Principal Component Analysis (PCA)** as a new initialization method for the Alternating Least Squares (ALS) algorithm.

**What it does:**
- Centers data before decomposition (subtracts mean)
- Focuses on variance rather than absolute values
- Particularly useful for data with baseline offsets or drifts

**How to use:**
1. Navigate to **ALS** → **Options** tab
2. Under **Initialization**, select **PCA**
3. Configure other constraints as needed
4. Click **Run**

**When to use PCA:**
- ✅ Data has significant baseline offsets
- ✅ Baseline drift across experiments
- ✅ Large background signals in spectra
- ✅ SVD initialization shows poor convergence

---

## 📝 Files Modified

### Core Application
- **`ui_files/ALSInputOptions.R`** - Added PCA radio button option
- **`server_files/ALS.R`** - Implemented PCA initialization logic

### Documentation
- **`docs/als.md`** - Added PCA description and usage
- **`EXPLANATION.md`** - Added mathematical details and best practices
- **`PCA_INITIALIZATION.md`** - Comprehensive PCA guide (new file)
- **`DOCKER_DEPLOY.md`** - Docker deployment instructions (new file)

### Testing
- **`test_pca_init.R`** - Test script for PCA implementation (new file)

---

## 🔧 Technical Details

### PCA Algorithm
```r
# Center the data
D_centered = D - mean(D)

# Perform SVD on centered data
D_centered = U · Σ · V^T

# Initialize with absolute values and scaling
C₀ = |U[:, 1:k]| · Σ[:k]
S₀ = |V[:, 1:k]|
```

### Implementation
- Location: `server_files/ALS.R`, lines 956-972
- Method: Uses R's `scale()` function for centering
- Deterministic: Always produces same result for same input
- Performance: Similar computation time to SVD

---

## 📊 Initialization Methods Comparison

| Method | Centering | Non-negative | Best Use Case |
|--------|-----------|--------------|---------------|
| SVD | ❌ | After | General purpose, default |
| **PCA** | ✅ | After | Baseline offsets/drifts |
| NMF | ❌ | ✅ | Strictly non-negative data |
| Sequential | Varies | After | Uncertain dimensionality |
| Restart | N/A | N/A | Refining previous results |

---

## 🐳 Docker Image Details

### Pull Command
```bash
docker pull saldenisov/skana:latest
```

### Run Command
```bash
docker run -d -p 3840:3840 --name skana -e PORT=3840 saldenisov/skana:latest
```

### Access
```
http://localhost:3840
```

### Image Specifications
- **Base:** rocker/shiny:4.4.1
- **R Version:** 4.4.1
- **Size:** ~1.68 GB
- **Platform:** linux/amd64

### Included Packages (Key)
- shiny, shinythemes, shinycssloaders, shinyBS
- DT, nnls, Iso, NMFN
- deSolve, Rsolnp, rgenoud
- fields, mvtnorm, msm, xtable
- callr, processx
- RColorBrewer, viridisLite, changepoint

---

## 🚀 Quick Start

### For New Users
```bash
# Pull and run
docker pull saldenisov/skana:latest
docker run -d -p 3840:3840 --name skana -e PORT=3840 saldenisov/skana:latest

# Open browser to http://localhost:3840
```

### For Existing Users
```bash
# Stop old container
docker stop skana
docker rm skana

# Pull latest
docker pull saldenisov/skana:latest

# Run new version
docker run -d -p 3840:3840 --name skana -e PORT=3840 saldenisov/skana:latest
```

---

## 📚 Documentation

### New Documentation
- **PCA_INITIALIZATION.md** - Complete PCA guide
- **DOCKER_DEPLOY.md** - Docker deployment guide
- **CHANGELOG_2025-11-19.md** - This file

### Updated Documentation
- **docs/als.md** - Added PCA method description
- **EXPLANATION.md** - Enhanced initialization section with PCA details

---

## 🧪 Testing

A test script is available to verify PCA implementation:

```bash
# Inside container or local R
Rscript test_pca_init.R
```

Expected output:
```
PCA Initialization Test Results:
================================
C matrix dimensions: 100 3 
S matrix dimensions: 50 3 
C range: 0 0.xxx 
S range: 0 xxx 
First 3 singular values: xxx xxx xxx 

Test PASSED: PCA initialization code runs without errors.
```

---

## 🔄 Backwards Compatibility

✅ **Fully backwards compatible** - Existing projects and workflows are unaffected  
✅ Default initialization remains **SVD**  
✅ All previous features remain functional  
✅ No breaking changes  

---

## 📖 References

### PCA Method
- Jolliffe, I.T. (2002). *Principal Component Analysis*. Springer.
- Wold, S., Esbensen, K., & Geladi, P. (1987). Principal component analysis. *Chemometrics and Intelligent Laboratory Systems*, 2(1-3), 37-52.

### MCR-ALS Background
- Ruckebusch, C., et al. (2012). Comprehensive data analysis of femtosecond transient absorption spectra. *J. Photochem. Photobiol. C*, **13**, 1–27.

---

## 🐛 Known Issues

None reported for this release.

---

## 💡 Tips & Best Practices

### When to Try PCA
1. SVD initialization converges slowly
2. Residuals show baseline patterns
3. Data collected across different experiments with offset baselines
4. Spectra have significant background signals

### Workflow Suggestion
1. Start with **SVD** (default)
2. If convergence is poor, try **PCA**
3. For strictly positive data, try **NMF**
4. For difficult datasets, use **Sequential**

---

## 🙏 Acknowledgments

This release builds upon the robust MCR-ALS framework developed by the SK-Ana team and incorporates best practices from the chemometrics community.

---

## 📞 Support

- **GitHub Repository:** https://github.com/Saldenisov/SK-Ana
- **Docker Hub:** https://hub.docker.com/r/saldenisov/skana
- **Issues:** https://github.com/Saldenisov/SK-Ana/issues

---

**Built with:** R 4.4.1, Shiny, Docker  
**Maintained by:** SK-Ana Development Team  
**License:** CeCILL V2.1
