
[![DOI](https://zenodo.org/badge/87315085.svg)](https://zenodo.org/badge/latestdoi/87315085)


# **SK-Ana**: **S**pectro**K**inetic **Ana**lysis

`Shiny` interface for the analysis of spectrokinetic matrices:

* Singular Value Decomposition (SVD)

* Multi Curve Resolution by Alternated Least Squares (MCR-ALS)

* Hybrid hard-soft modeling (DAS, homogeneous chemistry...)


<!--The code can be tested here: https://upsa.shinyapps.io/SK-Ana/-->

## Dev44 branch (R 4.4)

This branch (`dev44`) updates SK-Ana for R 4.4 compatibility and diverges from the original `master` as follows:

- Uses R 4.4.x (tested with 4.4.1)
- Removed dependency on `inlmisc`; added a local `GetColors` implementation
- Added `run_app_3840.R` helper script to launch the app on port 3840
- Minor package/dependency adjustments and configuration cleanups

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
   - Double-click on `run_app_3840.R` in Windows Explorer, or
   - In R console: `source("run_app_3840.R")` 
   - This will launch the app on http://localhost:3840
   
   **Method C: Using RStudio**
   - Open `server.R` or `ui.R` in RStudio
   - Click "Run App" button

5. The app will open automatically in your browser, or go to:
   - http://localhost:3838 (default shiny port) or
   - http://localhost:3840 (if using `run_app_3840.R`)

On first launch, required packages will be installed automatically if missing (e.g. `outliers`, `nnls`, `Iso`, `viridis`, `httpuv`, `changepoint`, `shiny`, `shinyBS`, `DT`, `Rsolnp`, `fields`, `NMFN`, `tools`, `shinycssloaders`, `rgenoud`, `mvtnorm`, `deSolve`, `msm`, `xtable`). Depending on your OS, you may need to install some of them manually using `install.packages(...)` in R.

## User's manual

__New__: online [here](https://ppernot.github.io/SK-Ana/index.html)

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
- Recommended Docker one‑liner (see container section below):
  - docker run -d -p 3840:3840 --name skana saldenisov/skana
  - Access via http://localhost:3840 (or http://127.0.0.1:3840)
- Integrates visualization, matrix factorization, and model fitting in one app.

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

For cross-plateform compatibility issues, the preferred installation
method is through a docker container.

### Option 1: Updated Docker Image (Recommended)

The [saldenisov/skana](https://hub.docker.com/r/saldenisov/skana) Docker image includes all latest fixes and R 4.4.1 compatibility.

0. Install [Docker](https://www.docker.com/products/docker-desktop)

1. Run the container:
```bash
docker run -d -p 3840:3840 --name skana saldenisov/skana
```

2. Access SK-Ana in your browser:
   - **Windows/Mac/Linux**: http://localhost:3840 or http://127.0.0.1:3840
   - The application will be available at the above addresses once the container starts

3. When finished:
```bash
docker stop skana
docker rm skana
```

4. For further sessions (reuse existing container):
```bash
docker restart skana
```

5. To get the latest version:
```bash
docker pull saldenisov/skana
```

### Option 2: Original Docker Image

The original [ppernot1/skana](https://hub.docker.com/repository/docker/ppernot1/skana) Docker container:

1. Run the container:
```bash
docker run -d -p 3840:3840 --name skana-original ppernot1/skana
```

2. Access at http://localhost:3840

**Note**: The updated `saldenisov/skana` image includes bug fixes and is recommended for new deployments.


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










