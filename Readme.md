
[![DOI](https://zenodo.org/badge/87315085.svg)](https://zenodo.org/badge/latestdoi/87315085)
[![Docker Build](https://github.com/Saldenisov/SK-Ana/actions/workflows/docker-build-push.yml/badge.svg)](https://github.com/Saldenisov/SK-Ana/actions/workflows/docker-build-push.yml)
[![Docker Pulls](https://img.shields.io/docker/pulls/saldenisov/skana)](https://hub.docker.com/r/saldenisov/skana)

# **SK-Ana**: **S**pectro**K**inetic **Ana**lysis

`Shiny` interface for the analysis of spectrokinetic matrices:

* Singular Value Decomposition (SVD)

* Multi Curve Resolution by Alternated Least Squares (MCR-ALS)

* Hybrid hard-soft modeling (DAS, homogeneous chemistry...)


<!--The code can be tested here: https://upsa.shinyapps.io/SK-Ana/-->

## Running SK-Ana

### Run locally without Docker (R 4.2+)

1. Install R 4.2+ from CRAN (tested with R 4.2.3 and 4.4.1).
2. (Optional) Install RStudio.
3. Open a terminal in the project root (this folder).
4. Start the app in one of the following ways:
   
   **Method A: Using R console/terminal**
   ```r
   setwd("C:/path/to/SK-Ana")  # Adjust path as needed
   shiny::runApp(".")
   ```
   
   **Method B: Using the helper script**
   - Double-click on `scripts/run_app_3840.R` in Windows Explorer/Finder, or
   - In R console: `source("scripts/run_app_3840.R")` 
   - This will launch the app on http://localhost:3840
   
   **Method C: Using RStudio**
   - Open `server.R` or `ui.R` in RStudio
   - Click "Run App" button

5. The app will open automatically in your browser, or go to:
   - http://localhost:3838 (default shiny port) or
   - http://localhost:3840 (if using `run_app_3840.R`)

On first launch, required packages will be installed automatically if missing (e.g. `outliers`, `nnls`, `Iso`, `viridis`, `httpuv`, `changepoint`, `shiny`, `shinyBS`, `DT`, `Rsolnp`, `fields`, `NMFN`, `tools`, `shinycssloaders`, `rgenoud`, `mvtnorm`, `deSolve`, `msm`, `xtable`). Depending on your OS, you may need to install some of them manually using `install.packages(...)` in R.

## User's manual

__New__: online [here](https://saldenisov.github.io/SK-Ana/)

---

## What is SK-Ana?

SK-Ana (Spectro-Kinetic Analysis) is an R + Shiny application for analyzing time-resolved spectroscopic datasets (e.g., pulse radiolysis, transient absorption, pump–probe). Such experiments produce 2D matrices where one axis is time (kinetics) and the other is wavelength (spectra). The goal is to recover the spectra of transient species and their kinetic profiles, and optionally fit a mechanistic reaction model.

### Data model
A measured dataset D(t, λ) is modeled as a sum of species spectra and time-dependent concentrations plus noise:

D(t_i, λ_j) ≈ Σ_k C_k(t_i) · S_k(λ_j) + ε(t_i, λ_j)

where C_k(t) are kinetic profiles (concentrations vs time), S_k(λ) are species spectra, and ε is noise.

---

## Core capabilities

### 1) Singular Value Decomposition (SVD)
- Estimates the number of significant components above noise.
- Denoises by reconstructing the dataset from significant singular values.

### 2) Non-Negative Matrix Factorization (NNMF)
- Factorizes D ≈ C · Sᵀ with non-negativity on spectra and concentrations.
- Optional constraints (smoothness, sparsity, unimodality) can be applied.
- Subject to rotational/scaling ambiguity (multiple valid solutions may exist).

### 3) MCR-ALS (Multi-Curve Resolution – Alternating Least Squares)
- Alternates between solving for C and S under chosen constraints:
  - positivity (spectra, concentrations)
  - mass/stoichiometry conservation
  - known spectral shapes or fixed regions
  - time masks or kinetic restrictions
- Provides uncertainty/interval estimates for spectra/kinetics.

### 4) Hybrid Hard–Soft Modeling (HH-SM)
- Couples empirical data fitting with mechanistic kinetic models (ODEs).
- Optimizes non-linear rate constants (k) and linear spectral coefficients jointly.
- Supports Decay-Associated Spectra (DAS) and full target-mechanism fitting.

### 5) Global analysis across conditions
- Fits multiple datasets simultaneously (e.g., different concentrations, solvents, windows) with shared spectra and condition-dependent kinetics.
- Enables cross-validation and robust parameter estimation.

### 6) Ambiguity and error analysis
- Explores solution-space ambiguity (T-transform invariances for D = C · Sᵀ).
- Quantifies confidence bounds on spectra/kinetics and assesses identifiability.

---

## Typical workflow
1. Import a spectro-kinetic matrix (time × wavelength).
2. Run SVD to estimate component count and denoise (optional).
3. Initialize and run MCR-ALS with constraints to extract C and S.
4. If applicable, define a kinetic scheme and switch to HH-SM to fit rate constants and refine spectra globally.
5. Inspect residuals, confidence intervals, and ambiguity diagnostics.
6. Export spectra, kinetics, fitted parameters, and reconstructed datasets.

---

## Typical use cases
- Pulse radiolysis studies (solvated electrons, radical chemistry).
- Femtosecond–microsecond pump–probe transient absorption.
- Photochemical intermediates and reaction-mechanism elucidation.
- Polymerization and redox kinetics; comparison with TD‑DFT/ab‑initio spectra.

---

## Technical notes
- Interactive GUI built with R/Shiny; runs locally or in Docker.
- Compatible with R 4.4.x (tested with R 4.4.1)
- Removed dependency on `inlmisc`; includes local `GetColors` implementation
- Includes helper scripts in `scripts/` directory for easy local deployment
- Recommended Docker deployment (see container section below):
  - `docker run -d -p 3840:3840 --name skana saldenisov/skana:latest`
  - Access via http://localhost:3840
- Integrates visualization, matrix factorization, and model fitting in one app.

## Project Structure

```
SK-Ana/
├── README.md                    # This file
├── app.R                        # Shiny app entry point
├── ui.R, server.R, global.R     # Main application files
├── error_handler.R              # Error handling
├── Dockerfile, Dockerfile.arm64 # Docker build files
├── ui_files/                    # UI components
├── server_files/                # Server logic
├── data/                        # Example datasets
├── scripts/                     # Utility scripts (run_app_3840.R, etc.)
├── tests/                       # Test files
├── docs/                        # Documentation
│   ├── deployment/              # Docker, CI/CD docs
│   └── development/             # Technical docs
├── .github/workflows/           # CI/CD automation
└── renv/                        # R package management
```

---

## Related software
- MCR‑ALS (MATLAB GUI): SVD, MCR‑ALS, hybrid, ambiguity analysis.
- GloTarAn (R + Java): Global/target analysis (SVD, DAS).
- SK‑Ana (R + Shiny): SVD, MCR‑ALS, hybrid modeling, ambiguity exploration.

---

## In summary
SK‑Ana provides a unified workflow to deconvolve, model, and interpret time‑resolved spectro‑kinetic data using both data‑driven and mechanistic approaches—bridging experiment, analysis, and simulation for robust mechanistic insight.

<!--
## Local install 

* Download the latest release [here](https://github.com/ppernot/SK-Ana/releases/latest) 
  and decompress the archive in a dedicated directory. If you want the most recent code
  version (with more bugs risks) download it [here](https://github.com/ppernot/SK-Ana/archive/master.zip)

* You will also need

    + [R](https://cran.rstudio.com/) (Mandatory)
    
    + [RStudio](https://www.rstudio.com/products/rstudio/download/#download) (Optional)

### How to run SK-Ana (for non-docker-based installs)

* Using `R`: go to the installation directory and run the command `shiny::runApp()` in a `R` console
    
* Using `RStudio`

    1. go to the installation directory and double-click on `server.R` or `ui.R`

    2. in `RStudio` click on `Run App`

For other options and more detailed information see [here](https://shiny.rstudio.com/articles/running.html).

In both cases, a web interface should open in your favorite browser. At the first launch,
the code will install a set of packages, if they are not installed already: 
`outliers`, `nnls`, `Iso`, `viridis`, `httpuv`, `changepoint`, `shiny`, `shinyBS`, `DT`, 
`Rsolnp`, `fields`, `NMFN`, `tools`, `shinycssloaders`, `rgenoud`, `mvtnorm`, `deSolve`, 
`msm`, and `xtable`. 
Depending on your OS, you might have to install them manually.
-->

## Docker container

For cross-platform compatibility, the preferred installation method is through a Docker container. SK-Ana provides **multi-platform Docker images** that automatically work on **Windows, Linux, Intel Mac, and Apple Silicon Mac**.

🔄 **Automated Builds**: Docker images are automatically built and published via GitHub Actions when new code is pushed. Both amd64 and arm64 architectures are built simultaneously and combined into a single multi-platform image.

### Prerequisites

0. Install [Docker Desktop](https://www.docker.com/products/docker-desktop)
   - **Windows**: Docker Desktop for Windows
   - **Mac**: Docker Desktop for Mac (supports both Intel and Apple Silicon)
   - **Linux**: Docker Engine or Docker Desktop

### Option 1: Pull Pre-built Image (Recommended)

The [saldenisov/skana](https://hub.docker.com/r/saldenisov/skana) Docker image includes all latest fixes and R 4.4.1 compatibility.

#### All Platforms (Recommended - Multi-Architecture Image):

The `latest` tag is a multi-platform image that automatically selects the correct architecture:

```bash
docker run -d -p 3840:3840 --name skana saldenisov/skana:latest
```

**Automatically works on:**
- ✅ Windows (amd64)
- ✅ Linux (amd64)
- ✅ Mac Intel (amd64)
- ✅ Mac Apple Silicon (arm64) - **No platform warnings!**

Access at: **http://localhost:3840**

#### Platform-Specific Images (Optional):

If you want to explicitly specify the architecture:

**For Windows/Linux/Intel Mac (amd64):**
```bash
docker run -d -p 3840:3840 --name skana saldenisov/skana:latest-amd64
```

**For Mac Apple Silicon (arm64):**
```bash
docker run -d -p 3840:3840 --name skana saldenisov/skana:latest-arm64
```

### Container Management

**Stop container:**
```bash
docker stop skana
```

**Restart container:**
```bash
docker restart skana
```

**Remove container:**
```bash
docker stop skana
docker rm skana
```

**Update to latest version:**
```bash
docker pull saldenisov/skana:latest
docker stop skana
docker rm skana
docker run -d -p 3840:3840 --name skana saldenisov/skana:latest
```

### Option 2: Build from Source

For **Mac Apple Silicon users** or users who want to customize the image:

#### Mac Apple Silicon (arm64) - Native Build:

1. Clone or download this repository
2. Navigate to the SK-Ana directory
3. Build the arm64-optimized image:

```bash
cd /path/to/SK-Ana
docker build --platform linux/arm64 -f Dockerfile.arm64 -t skana:arm64 .
```

4. Run the native arm64 container:

```bash
docker run -d -p 3840:3840 -e PORT=3840 --name skana skana:arm64
```

**Benefits of native arm64 build:**
- No emulation overhead
- Better performance
- No platform mismatch warnings

#### Windows/Linux/Intel Mac - Standard Build:

```bash
cd /path/to/SK-Ana
docker build -t skana:latest .
docker run -d -p 3840:3840 -e PORT=3840 --name skana skana:latest
```

### Available Images

| Image Tag | Architectures | Base Image | Use Case |
|-----------|---------------|------------|----------|
| `latest` | amd64, arm64 | Multi-platform | **Recommended** - Auto-selects architecture |
| `latest-amd64` | amd64 only | rocker/shiny:4.4.1 | Windows, Linux, Intel Mac |
| `latest-arm64` | arm64 only | r-base:4.4.1 | Mac Apple Silicon (M1/M2/M3) |

**Note:** The `latest` tag automatically selects the correct architecture for your system. No need to specify platform explicitly!

### Available Dockerfiles (For Building from Source)

| Dockerfile | Platform | Base Image | Use Case |
|------------|----------|------------|----------|
| `Dockerfile` | amd64 (x86_64) | rocker/shiny:4.4.1 | Windows, Linux, Intel Mac |
| `Dockerfile.arm64` | arm64 | r-base:4.4.1 | Mac Apple Silicon (M1/M2/M3) |

### Option 3: Original Docker Image

The original [ppernot1/skana](https://hub.docker.com/repository/docker/ppernot1/skana) Docker container (amd64 only):

```bash
docker run -d -p 3840:3840 --name skana-original ppernot1/skana
```

Access at: **http://localhost:3840**

**Note**: The updated `saldenisov/skana` image includes bug fixes and is recommended for new deployments.

### Troubleshooting

**Platform warning on Mac:**
If you see:
```
WARNING: The requested image's platform (linux/amd64) does not match the detected host platform (linux/arm64/v8)
```

You have two options:
1. Continue using emulation (works fine, slightly slower)
2. Build native arm64 image using `Dockerfile.arm64` (see "Build from Source" section)

**Port already in use:**
```bash
# Use a different port
docker run -d -p 3841:3840 --name skana saldenisov/skana:latest
# Access at http://localhost:3841
```

**Container won't start:**
```bash
# Check logs
docker logs skana

# Restart Docker Desktop and try again
```

For detailed Docker documentation, see:
- **[docs/deployment/DOCKER_PLATFORM_GUIDE.md](docs/deployment/DOCKER_PLATFORM_GUIDE.md)** - Platform-specific quick reference (recommended starting point)
- **[DOCKER.md](DOCKER.md)** - Complete Docker deployment guide
- **[README_DOCKER.md](README_DOCKER.md)** - Cross-platform Docker instructions
- **[docs/deployment/](docs/deployment/)** - All deployment documentation


## How to cite SK-Ana

If you use SK-Ana in one of your publications, do not forget to cite it and include the version you used for reproducibility:

> Pernot, P. (2018) SK-Ana: Analysis of Spectro-Kinetic Data (Version X.X).    
> https://doi.org/10.5281/zenodo.1064370

## References

### Methods

* C. Ruckebusch, M. Sliwa, P. Pernot, A. d. Juan, R. Tauler (2012) 
"Comprehensive data analysis of femtosecond transient absorption spectra: 
A review", _J. Photochem. Photobiol. C_ __13__:1–27. [DOI](http://dx.doi.org/10.1016/j.jphotochemrev.2011.10.002)

### Tutorial (in french)

* _Apport de la simulation à la détermination de mécanismes réactionnels en 
chimie sous rayonnement_. Action Nationale de Formation CNRS "Chimie sous
Rayonnement et Radiochimie" (Oléron, 2017/09) 
[PDF](https://universite-paris-saclay.hal.science/hal-04618186)


### Works using SK-Ana (please let me know of any missing reference)

* D.H. Cruz Neto, J. Soto, N. Maity, Ch. Lefumeux, T. Nguyen, P. Pernot, K. Steenkeste, D. Peláez, M.-H. Ha-Thi and Th. Pino (2023) _J. Phys. Chem. Letters_ __14__:4789–4795.
(https://doi.org/10.1021/acs.jpclett.3c00594)

* M. Knezevic, V.-D. Quach, I. Lampre, M. Erard, P. Pernot, D. Berardan, Ch. Colbeau-Justin and M.N. Ghazzal (2023) _J. Mater. Chem. A_ __11__:6226-6236. (https://doi.org/10.1039/D2TA09920A)

* K. Iwamatsu, R. Gakhar, Ph. Halstenberg, B. Layne, S.M. Pimblott and J.F. Wishart (2022) _Phys. Chem. Chem. Phys._ __24__:25088-25098. (https://doi.org/10.1039/D2CP01194H)

* C. Shang and P.E. Reiller (2021) _Dalton Trans._  __50__:17165-17180. (https://doi.org/10.1039/D1DT03204F)

* M. Puget, V. Shcherbakov, S. Denisov, P. Moreau, J.-P. Dognon, M. Mostafavi and S. Le Caër (2021) _Chem. Eur. J._  __27__:8185. (https://doi.org/10.1002/chem.202100562) 

* R. Kaczmarek, S. Ward, D. Debnath, T. Jacobs, A. D. Stark, D. Korczyński, A. Kumar, M. D. Sevilla, S. A. Denisov, V. Shcherbakov, P. Pernot, M. Mostafavi, R. Dembinski and A. Adhikary (2020) _Chemistry – A European Journal_ __26__:9495–9505. (http://dx.doi.org/10.1002/chem.202000247)

* S. Al Gharib, J.-L. Marignier, A.K. El Omar, A. Naja, S. Le Caer, M. Mostafavi and J. Belloni (2019) _J. Phys. Chem. C_  __123__:22624-22633. (http://dx.doi.org/10.1021/acs.jpcc.9b06090)

* F. Wang, P. Pernot, J.-L. Marignier, P. Archirel and M. Mostafavi (2019) _J. Phys. Chem. B_ __123_, 2019, pp. __:6599-6608. (http://dx.doi.org/10.1021/acs.jpcb.9b05560)

* F. Wang, G.P. Horne, P. Pernot, P. Archirel and M. Mostafavi (2018) _J. Phys. Chem. B_  __122_, 2018, pp. __:7134-7142. (http://dx.doi.org/10.1021/acs.jpcb.8b03715)

* J. Ma, A. Kumar, Y. Muroya, S. Yamashita, T. Sakurai, S.A. Denisov, M.D. Sevilla, A. Adhikary, S. Seki and M. Mostafavi (2019) _Nat. Commun._ __10__:102. (https://doi.org/10.1038/s41467-018-08005-z)

* J. Ma, S. A. Denisov, J. Marignier, P. Pernot, A. Adhikary, S. Seki and M. Mostafavi (2018) _J. Phys. Chem. Lett._  __9__:5105-5109. (http://dx.doi.org/10.1021/acs.jpclett.8b02170)

* J. Ma, J. Marignier, P. Pernot, C. Houée-Levin, A. Kumar, M. D. Sevilla, A. Adhikary and M. Mostafavi (2018) _Phys. Chem. Chem. Phys._  __20__:14927-14937. (http://dx.doi.org/10.1039/C8CP00352A)

* J. Ma, P. Archirel, P. Pernot, U. Schmidhammer, S. L. Caër and M. Mostafavi (2016) _J. Phys. Chem. B_ __120__:773–784. (http://dx.doi.org/10.1021/acs.jpcb.5b11315)

* J. Ma, P. Archirel, U. Schmidhammer, J. Teuler, P. Pernot and M. Mostafavi (2013) _J. Phys. Chem. A_ __117__:14048–14055. (http://dx.doi.org/10.1021/jp410598y)








