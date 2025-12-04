# SK-Ana Codebase Overview

## 🎯 What is SK-Ana?

**SK-Ana** (SpectroKinetic Analysis) is a Shiny-based R web application for analyzing time-resolved spectroscopic data. It helps researchers deconvolve and model 2D spectro-kinetic matrices (time × wavelength) to extract:
- Spectra of transient chemical species
- Kinetic profiles (concentration vs time)
- Rate constants and reaction mechanisms

### Core Use Cases
- Pulse radiolysis (solvated electrons, radical chemistry)
- Femtosecond–microsecond pump–probe transient absorption
- Photochemical intermediates
- Polymerization and redox kinetics

---

## 📂 Project Structure

```
SK-Ana/
├── app.R                          # Entry point (Railway deployment)
├── ui.R                           # Main UI router
├── server.R                       # Main server router & initialization
├── global.R                       # Global constants, libraries, colors
├── error_handler.R                # Error handling utilities
├── _dependencies.R                # Package management
│
├── ui_files/                      # UI components (one file per tab)
│   ├── project.R                  # Project management (new/open/save)
│   ├── dataSelection.R            # Data import and masking
│   ├── SVD.R                      # Singular Value Decomposition tab
│   ├── ALS.R                      # ALS (Alternating Least Squares) main
│   │   ├── ALSInputConstraintsSpectra.R
│   │   ├── ALSInputConstraintsCorrectionSpectra.R
│   │   ├── ALSInputConstraintsKinet.R
│   │   ├── ALSInputOptions.R
│   │   ├── ALSInputRun.R
│   │   ├── ALSOutputVectors.R
│   │   ├── ALSOutputAmbiguity.R
│   │   └── ALSOutputDiagnostics.R
│   ├── kinet.R                    # Kinetics modeling tab
│   │   ├── kinetInputModel.R
│   │   ├── kinetInputRun.R
│   │   ├── kinetOutputVectors.R
│   │   ├── kinetOutputBestParams.R
│   │   ├── kinetOutputIdentifiability.R
│   │   ├── kinetOutputDiagnostics.R
│   │   ├── kinetOutputTrace.R
│   │   └── kinetSmoothingOptions.R
│   ├── downloads.R                # Export/download tab
│   ├── about.R                    # About page
│   └── debug_tab.R                # Debug console
│
├── server_files/                  # Server logic (processed in sequence)
│   ├── helpers.R                  # Shared utilities
│   ├── process_utils.R            # Data processing utilities
│   ├── getData.R                  # File import & matrix parsing
│   ├── sliders.R                  # Dynamic slider generation
│   ├── project.R                  # Project management logic
│   ├── selectAreaAndMasks.R       # Region selection & masking
│   ├── SVD.R                      # SVD computation & visualization
│   ├── ALS_plots.R                # ALS result plotting
│   ├── ALS.R                      # MCR-ALS algorithm core
│   ├── ALS_CorrectionSpectra_Server.R  # Correction spectra extension
│   ├── debug_console.R            # Debug console & logging
│   ├── kinetHypercubeTransfo.R    # Data transformation for kinetics
│   ├── kinetParsers.R             # Parse kinetic models (ODEs)
│   ├── kinetSpectrokineticModel.R # Hybrid hard-soft model
│   ├── kinetBayesian.R            # Bayesian parameter estimation
│   ├── kinetInterface.R           # Kinetics UI integration
│   ├── kinetics_smoothing.R       # Savitzky-Golay filtering
│   └── report.R                   # Report generation
│
├── data/                          # Example data
│   ├── data.csv, data_ABC_IRF.csv  # Sample datasets
│   ├── spectrum_*.csv             # Reference spectra
│   ├── scheme_ABC.txt             # Reaction scheme
│   └── genMat.R                   # Generate synthetic data
│
├── docs/                          # Documentation
├── tests/                         # Test files
├── outputDir/                     # Output storage (created at runtime)
├── renv/                          # Dependency management
└── docker/                        # Docker configuration
```

---

## 🔄 Application Workflow

### 1. **Entry Point** (`app.R`)
```r
source("global.R")          # Load globals and libraries
ui <- source("ui.R")$value  # Load UI definition
server <- source("server.R")$value  # Load server logic
shinyApp(ui, server)        # Create & run Shiny app
```

### 2. **Global Initialization** (`global.R`)
Sets up:
- **Version info**: `Version = "3.4.7b"`, `DateVersion = "2023-11-03"`
- **Libraries**: shiny, DT, nnls, deSolve, mvtnorm, etc.
- **Global constants**: 
  - `sideWidth = 4`, `mainWidth = 8` (layout proportions)
  - `plotHeight = 500px` (chart dimensions)
  - `debounceDelay = 750ms` (slider responsiveness)
- **Color palettes**: 
  - `imgColors` (davos scheme for heatmaps)
  - `cutColors` (jet scheme for cuts)
  - `resColors` (BuRd scheme for residuals)
  - `lineColors` (default viridis)
- **Helper functions**: `string2Expr()`, `GetColors()`, `col2tr()` (color transparency)

### 3. **UI Router** (`ui.R`)
Creates a tabbed interface with tabs:
1. **Project** → New/Open/Save projects
2. **Data Selection** → Import and mask data
3. **SVD** → Singular Value Decomposition
4. **ALS** → MCR-ALS decomposition
5. **Kinet** → Kinetics modeling & rate constant fitting
6. **Downloads** → Export results
7. **About** → About page
8. **Debug Console** → Real-time logs

Each tab's UI is loaded from `ui_files/` subfolder using a helper function:
```r
source_ui <- function(...) {
  source(file.path("ui_files", ...), local = TRUE)$value
}
```

### 4. **Server Initialization** (`server.R`)
```r
function(input, output, session) {
  # Create outputDir if needed
  # Initialize Inputs reactive list (empty state)
  # Source all server_files in order (critical!)
}
```

**Initialization sequence of `Inputs` (reactive state):**
```r
Inputs$gotData        ← FALSE      # Has user loaded data?
Inputs$process        ← FALSE      # Processing in progress?
Inputs$finish         ← FALSE      # Processing complete?
Inputs$validData      ← TRUE       # Data passes validation?
Inputs$fileOrig       ← NULL       # Original file path
Inputs$matOrig        ← NULL       # Original matrix
Inputs$wavlOrig       ← NULL       # Original wavelengths
Inputs$delayOrig      ← NULL       # Original delays/times
Inputs$mat            ← NULL       # Processed matrix
Inputs$wavl           ← NULL       # Wavelengths used
Inputs$delay          ← NULL       # Delays/times used
Inputs$baselineMask   ← NA         # Mask for baseline correction
Inputs$delayMask      ← NA         # Mask for delay regions
Inputs$wavlMask       ← NA         # Mask for wavelength regions
```

---

## 📋 Server Files (Execution Order)

These files are sourced sequentially in `server.R`. **Order matters** because later files depend on earlier ones:

### Phase 1: Data Loading & Utilities
1. **helpers.R** - Shared functions (`showMSE()`, `getExternalSpectra()`)
2. **process_utils.R** - Data transformation utilities
3. **getData.R** - File import, CSV parsing, matrix construction

### Phase 2: UI & Interactive Controls
4. **sliders.R** - Dynamic slider generation for masks
5. **project.R** - Project file I/O (save/load `.Rda` files)
6. **selectAreaAndMasks.R** - Region selection, masking logic

### Phase 3: SVD Analysis
7. **SVD.R** - Singular Value Decomposition computation & plots

### Phase 4: MCR-ALS Decomposition
8. **ALS_plots.R** - Plot results (spectra, kinetics, residuals)
9. **ALS.R** - Main MCR-ALS algorithm
10. **ALS_CorrectionSpectra_Server.R** - Extension for correction spectra

### Phase 5: Debug & Diagnostics
11. **debug_console.R** - Real-time logging system

### Phase 6: Kinetics Modeling
12. **kinetHypercubeTransfo.R** - Data reshaping for ODE integration
13. **kinetParsers.R** - Parse reaction schemes (e.g., "A -> B -> C")
14. **kinetSpectrokineticModel.R** - Hybrid hard-soft model (DAS)
15. **kinetBayesian.R** - Bayesian parameter estimation

### Phase 7: Kinetics UI & Smoothing
16. **kinetInterface.R** - Kinetics tab UI integration
17. **kinetics_smoothing.R** - Savitzky-Golay filter

### Phase 8: Reports
18. **report.R** - Generate downloadable reports

---

## 🎨 UI Component Breakdown

### Tab 1: **Project** (`ui_files/project.R`)
**Purpose:** Create, load, or save projects

**Left Panel (Sidebar):**
- Tab A: **New Project**
  - Project name input
  - Predefined file formats (CSV, ELYSE, Fluo, Streak, Other)
  - Conditional controls (if "Other" selected):
    - Header checkbox
    - Separator radio buttons (`,`, `;`, `\t`, space)
    - Decimal separator (`.` or `,`)
    - Data structure (`wxd` = wavelength×delay, `dxw` = delay×wavelength)
  - Load-time compression factors (Delay, Wavl)
  - Transform delay options (No, Index, Log10)
  - File upload
  - Post-process compression factors

- Tab B: **Open**
  - Load existing `.Rda` project file

- Tab C: **Save**
  - Download current project

**Right Panel (Main):**
- Raw data table preview
- Project metadata
- Vignette plot (small preview)

### Tab 2: **Data Selection** (`ui_files/dataSelection.R`)
**Purpose:** Subset data and define regions to exclude

**Left Panel:**
- Tab A: **Selection**
  - Sliders for OD, Wavelength, and Delay ranges
  
- Tab B: **Baseline** (regions to exclude)
  - Number of baseline masks
  - Auto-detect button
  - Dynamic mask UI
  
- Tab C: **Wavl Mask** (wavelength exclusion)
  - Similar structure to Baseline
  
- Tab D: **Delay Mask** (time exclusion)
  - Similar structure

**Right Panel:**
- Heatmap of raw data (click + drag to select regions)
- Transects at reference wavelength
- Cuts at reference delay
- Sliders to define reference points

### Tab 3: **SVD** (`ui_files/SVD.R`)
**Purpose:** Singular Value Decomposition for denoising

**Left Panel:**
- SVD dimension input (how many components to keep)
- Glitch removal options

**Right Panel (Tabs):**
- **Singular Values** → Scree plot
- **Vectors** → Singular vectors (kinetics and spectra)
- **Data vs. Model** → Comparison plot
- **Residuals** → Residual map
- **Contributions** → Component contributions
- **Statistics** → Summary table

### Tab 4: **ALS** (`ui_files/ALS.R`)
**Purpose:** MCR-ALS decomposition with constraints

**Left Panel (Tabs):**
- **Constraints** (different subtabs for spectra, kinetics, correction)
- **Options** (iteration settings, error metrics)
- **Run** (execute ALS algorithm)

**Right Panel (Tabs):**
- **Vectors** → Extracted spectra and kinetics
- **Ambiguity** → T-transform exploration
- **Diagnostics** → Fit quality metrics

### Tab 5: **Kinet** (`ui_files/kinet.R`)
**Purpose:** Hybrid hard-soft modeling with ODEs

**Left Panel (Tabs):**
- **Model** → Define reaction scheme (e.g., "A --k1--> B --k2--> C")
- **Run** → Optimization parameters

**Right Panel (Tabs):**
- **Best Params** → Optimized rate constants
- **Vectors** → Final spectra and kinetics
- **Diagnostics** → Fit quality

---

## 🔑 Key Data Structures

### Reactive State (`Inputs`)
Central reactive list holding all analysis state:
```r
Inputs <- reactiveValues(
  # Input data
  gotData = FALSE,
  matOrig = NULL,      # n_delay × n_wavl matrix
  wavlOrig = NULL,     # wavelengths
  delayOrig = NULL,    # time delays
  
  # Processed data (after selection)
  mat = NULL,          # Subset matrix
  wavl = NULL,         # Subset wavelengths
  delay = NULL,        # Subset delays
  
  # Masks (logical vectors)
  baselineMask = NA,   # TRUE = exclude this row
  delayMask = NA,      # TRUE = exclude this row
  wavlMask = NA,       # TRUE = exclude this column
  
  # Results
  SVD_D = NULL,        # SVD singular values
  SVD_U = NULL,        # SVD left vectors (kinetics basis)
  SVD_V = NULL,        # SVD right vectors (spectra basis)
  
  ALS_C = NULL,        # ALS kinetic profiles
  ALS_S = NULL,        # ALS spectra
  ALS_residuals = NULL,
  
  Kinet_pars = NULL,   # Optimized rate constants
  Kinet_C = NULL,      # Model-fitted kinetics
  Kinet_S = NULL       # Model-fitted spectra
)
```

### File Formats Supported
**Input:** CSV, TXT (configurable delimiter/decimal)

**Output:**
- `.Rda` projects (full state)
- `.csv` spectra/kinetics
- `.Rmd` reports (with plots)

---

## ⚙️ Core Algorithms

### 1. SVD (Singular Value Decomposition)
**File:** `server_files/SVD.R`
- Computes: D ≈ U · Σ · Vᵀ
- Determines significant components above noise
- Used for denoising via truncation

### 2. MCR-ALS (Multi-Curve Resolution)
**Files:** `server_files/ALS.R`, `ALS_plots.R`
- Factorizes: D ≈ C · Sᵀ
- Constraints: non-negativity, unimodality, smoothness, mass balance
- Alternates: solve for C (kinetics) → solve for S (spectra)
- Handles rotational ambiguity via T-transforms

### 3. Hybrid Hard-Soft Modeling
**Files:** `server_files/kinet*.R`
- Couples ODE integration with spectral fitting
- Solves: minimize ||D - C_model(k) · Sᵀ||
- Optimizes rate constants (k) + spectral coefficients
- Uses DAS (Decay-Associated Spectra) as special case

### 4. Ambiguity Analysis
- Explores T-transform rotations in solution space
- Quantifies confidence intervals on C and S
- Identifies identifiable vs. non-identifiable parameters

---

## 🎯 Typical User Workflow

1. **Project Tab**
   - Click "New Project"
   - Select data format
   - Upload CSV file → matrix loaded into `Inputs$matOrig`

2. **Data Selection Tab**
   - Adjust OD/wavelength/delay ranges
   - Draw masks for noise regions (e.g., baseline)
   - Click "Save Selections" → creates `Inputs$mat` (subset)

3. **SVD Tab**
   - Set dimension (number of components)
   - Inspect singular values scree plot
   - If satisfied, click "Denoise" → `Inputs$mat` smoothed

4. **ALS Tab**
   - Define constraints (positivity, smoothness, etc.)
   - Click "Run ALS" → extracts `Inputs$ALS_C` and `Inputs$ALS_S`
   - Inspect spectra/kinetics plots

5. **Kinet Tab** (optional, if kinetic model available)
   - Define reaction scheme (e.g., "A -> B -> C")
   - Click "Fit Model" → optimize rate constants
   - Generates `Inputs$Kinet_pars`

6. **Downloads Tab**
   - Export results as CSV or PDF report

---

## 📝 File-by-File Breakdown

### `app.R`
- **Purpose:** Entry point for Shiny app
- **Does:** Sources global, loads UI/server, launches shinyApp()

### `ui.R`
- **Purpose:** Main UI router
- **Does:** Creates navbar with 7 tabs, each loading a component
- **Key:** Uses `source_ui()` helper for modular loading

### `server.R`
- **Purpose:** Main server router
- **Does:** Initializes reactive state, sources 18 server files in order
- **Critical:** Order of sourcing matters (dependencies between files)

### `global.R`
- **Purpose:** Global configuration
- **Does:** 
  - Sets locale (C = dot decimal)
  - Loads 28+ packages
  - Defines color palettes
  - Sets UI dimension constants
  - Wraps critical functions with `safely()` for error handling

### `error_handler.R`
- **Purpose:** Centralized error handling
- **Does:** Wraps functions to catch errors gracefully

### Server Files

#### Data Loading
**`getData.R`** - File import engine
- Reads CSV/TXT files
- Handles compression factors (aggregate rows/columns)
- Validates data structure (matrix, numeric)
- Stores in `Inputs$matOrig`, `Inputs$wavlOrig`, `Inputs$delayOrig`

**`process_utils.R`** - Data transformation
- Normalization, scaling
- Log transforms
- Data validation checks

#### Interactive Controls
**`sliders.R`** - Dynamic UI generation
- Creates mask sliders from number input
- Updates when user changes "Nb of masks" input

**`selectAreaAndMasks.R`** - Selection logic
- Handles brush/click events on plots
- Converts plot coordinates to data indices
- Updates `Inputs$baselineMask`, `Inputs$delayMask`, `Inputs$wavlMask`

#### Analysis Engines
**`SVD.R`** - Singular Value Decomposition
- Computes U, Σ, V via `svd()` function
- Stores in `Inputs$SVD_U/V/D`
- Generates scree plot output

**`ALS.R`** - MCR-ALS algorithm
- Core optimization loop
- Solves non-negative least squares for C and S
- Applies constraints each iteration
- Returns `Inputs$ALS_C`, `Inputs$ALS_S`

**`ALS_plots.R`** - Visualization
- Spectra/kinetics line plots
- Residual heatmaps
- Ambiguity explorer plots

#### Kinetics Modeling
**`kinetSpectrokineticModel.R`** - ODE integration
- Solves differential equations (reaction mechanism)
- Returns kinetic profiles C_model(t, k)

**`kinetBayesian.R`** - Parameter optimization
- Global optimization (Bayesian methods)
- Fits rate constants to data

#### Utilities
**`helpers.R`** - Shared functions
- `showMSE()` - Condition for showing MSE
- `getExternalSpectra()` - Load reference spectra from files

**`debug_console.R`** - Logging system
- Captures all warnings/errors
- Real-time display in Debug Console tab

---

## 🔗 Data Flow Diagram

```
[CSV File Upload]
       ↓
    getData.R
       ↓
   Inputs$matOrig ← Raw data matrix
       ↓
selectAreaAndMasks.R (apply masks)
       ↓
   Inputs$mat ← Masked subset
       ↓
   ┌────┬────┬────┐
   ↓    ↓    ↓    ↓
 SVD  ALS  Kinet Report
   ↓    ↓    ↓    ↓
   └────┬────┬────┘
        ↓
   [Visualization Plots]
        ↓
   [Export Results]
```

---

## 🛠️ Extension Points

### Adding a New Tab
1. Create `ui_files/myFeature.R` (UI definition)
2. Create `server_files/myFeature.R` (server logic)
3. Add to `server.R` sourcing list
4. Add `tabPanel()` in `ui.R`

### Adding a New Constraint to ALS
- Modify `server_files/ALS.R` constraint application
- Add UI controls in `ui_files/ALSInputConstraints*.R`

### Adding a New Export Format
- Modify `server_files/report.R` or create new file
- Add download handler in `ui_files/downloads.R`

---

## 🧪 Testing

**Test location:** `tests/` directory

Common test scenarios:
- Load sample CSV → check matrix dimensions
- Apply SVD → verify U, Σ, V shapes
- Run ALS → verify convergence
- Export results → check file integrity

---

## 📦 Dependencies

**Core packages:**
- `shiny` - Web framework
- `nnls` - Non-negative least squares
- `deSolve` - ODE integration
- `mvtnorm` - Multivariate statistics
- `fields` - Spatial visualization
- `DT` - Interactive tables
- Full list in `global.R` (28 packages)

---

## 🚀 Deployment

**Local:** `Rscript run_app_3840.R` or `shiny::runApp()`

**Docker:** `docker run -p 3840:3840 saldenisov/skana:latest`

**Railway:** Detects PORT env var, runs on configured host:port

---

## 📊 Key Outputs

**Files generated:**
- `Spectra_ALS.csv` - Extracted spectra
- `Kinetics_ALS.csv` - Extracted kinetics
- `report.html` / `.Rmd` - Analysis report
- `project.Rda` - Full project state (for later reload)

---

*This is version 3.4.7b (2023-11-03) of SK-Ana*

