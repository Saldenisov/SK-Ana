# SK-Ana Python (Streamlit)

Independent Python package and Streamlit interface alongside the original R/Shiny application. The transferred `spectrokinetic_transformers.py` remains in place as the numerical source for MCR-ALS and sklearn transformers. `skana/` adds matrix input, SVD, kinetic ODE modeling, MAP fitting, ambiguity search, exports, and reports. The application never starts or calls R. It uses the pure Python `rdata` package to import numerical matrices from R `.Rda` projects; an R runtime is needed only for optional cross-language tests. All demonstrations and tests use synthetic data.

## Install and run

Python 3.10 or newer is required. Run commands from `E:\SK-Ana\python` on Windows PowerShell:

```powershell
py -3.11 -m venv .venv
.\.venv\Scripts\python.exe -m pip install --upgrade pip
.\.venv\Scripts\python.exe -m pip install -e ".[dev]"
.\.venv\Scripts\python.exe -m streamlit run skana\app.py
```

Open `http://localhost:8501`. Select **Use synthetic A → B demo data** on the Project tab to explore the analyses without uploading data. For the core and application without test/notebook tools, use `pip install -e .` or `pip install -r requirements.txt`. `pyproject.toml` specifies NumPy, pandas, SciPy, scikit-learn, Matplotlib, Streamlit, and `rdata`; the `dev` extra adds pytest, nbformat, nbclient, and ipykernel.

On a Linux Everest session with Python 3.10+ already available, from the repository root:

```bash
cd python
python3 -m venv .venv
.venv/bin/python -m pip install -e '.[dev]'
.venv/bin/python -m streamlit run skana/app.py --server.address 127.0.0.1 --server.port 8501
```

For remote use, forward port 8501 through SSH according to Everest's access policy. These commands do not assume a particular scheduler, Python module name, or network exposure. R remains independent: launch it from the repository root using `run_app.bat` on Windows or `./run_app.sh` on Linux; its default app port is 3840. Neither launcher needs the other language installed.

## Inputs and workflow

Upload `.csv`, `.tsv`, `.txt`, or `.dat` corner-cell matrices. First row contains wavelength and first column delay (`wxd`); choose `dxw` when the first row contains delay and first column wavelength. The matrix always becomes delay × wavelength internally. Presets: Auto, ELYSE/TSV, CSV, Fluo, Streak, or custom separator/decimal. Auto detects delimiters but defaults to `wxd`; confirm orientation in the UI. Optional text header, nonfinite signal values replaced with zero, sorted axes, block compression, and delay transformation follow the R data path where stated in [inventory](R_PORT_INVENTORY.md). Axes must be finite. Multiple inputs can be averaged, delay-tiled, or wavelength-tiled when the corresponding coordinate grids agree exactly.

Data selection offers delay/wavelength ranges, baseline intervals, exclusion masks, automatic signal masks, an SVD-vector glitch selector, and delay/wavelength transects. Automatic masks use an RMS threshold and differ from R's variance changepoint algorithm; inspect retained row/column counts before fitting. Download a selected matrix in the same corner-cell CSV layout. Save/reopen a Python project as a pickle-free `.npz` and selection settings as `.json`. Import an R `.Rda` project to migrate its matrix and axes. A separate R `Selections.Rda` file transfers saved ranges and masks when original matrix dimensions match. Other R widget settings and fitted results are not imported.

SVD shows singular values, rank-by-rank lack of fit and residual standard deviation, vectors, model, residuals, and statistics CSV. MCR-ALS shows component profiles, model, residual heatmap and distribution, residual singular values, contributions, and a ZIP of kinetics, spectra, model, residual, broadening (when enabled), and metrics. Controls expose rank, initialization (SVD/PCA/NMF/sequential/restart), optional SVD filtering, nonnegativity, closure, normalization, unimodality, smoothing, fixed spectra, component presence, correction spectra, and Gaussian broadening. The two/three-component ambiguity explorer searches rotated nonfixed factors and exports a selected solution or all accepted solutions as NPZ. The three-component search uses a bounded six-parameter grid. Fixed spectra upload must be CSV with `wavelength,S_1,...` column headers and a wavelength range covering selected data. Savitzky–Golay smoothing changes displayed kinetics only. Export can bundle the original project matrix, selection settings, selected matrix, SVD statistics and available fits in one workspace ZIP.

The **Kinetics** tab parses R-style reactions (`A + B -> C ; k / F`), initial concentrations (`c0_A_1 = value / F`), and extinction coefficients (`eps_A = value / F`). A factor `F > 1` makes a parameter variable with a truncated standard-normal prior in contracted coordinates `z = log(value/nominal)/log(F)`, bounded to `[-3,3]`. Each experiment is integrated from physical time zero with SciPy LSODA (`rtol=1e-6`, `atol=1e-8`). Spectra are solved by least squares or NNLS, optionally using fixed shapes; amplitudes follow the R extinction normalization. Unweighted MAP minimizes `N/2·log(SSE) + Σz²/2`; weighted MAP uses `SSE/(2σ²) + Σz²/2`. SciPy differential evolution and L-BFGS-B replace R's `rgenoud` and `Rsolnp`; this changes search and convergence behavior. Results include concentrations, spectra, model, residual, fitted parameters, approximate local covariance, plots, and ZIP export. The HTML report is generated entirely in Python and embeds plots for offline viewing.

Example scheme:

```text
A -> B ; 0.7 / 1.5
c0_A_1 = 1
eps_A = 1
eps_B = 0.8
```

Optional kinetic spectral smoothing uses a Python local quadratic fit approximating R's `loess`; that branch has not been cross-validated.

The R log sliders use powers of ten. The Python API takes linear `thresh`, `w_close_c`, and `w_hard_s0`; Streamlit converts its log controls. The UI defaults match the R controls (`optS1st=FALSE`, `maxiter=1000`, `alsThresh=10^-6`, `nonnegC/S=TRUE`, `normS=TRUE`). Normalization is applied to fixed spectra before optimization as in R. `ALSResult` contains C (delay × component), S (wavelength × component), model, residual `data-model`, normalized RSS, LOF percent, iteration count, and convergence status.

API example:

```python
from skana import read_matrix, svd, als

with open("synthetic_matrix.csv", "rb") as source:
    data = read_matrix(source, preset="CSV")
decomposition = svd(data, rank=2)
result = als(data, n_components=2, maxiter=300, thresh=1e-7)
print(decomposition.statistics, result.lof)
```

The executable synthetic example is [skana_synthetic_demo.ipynb](notebooks/skana_synthetic_demo.ipynb). It creates all observations in memory, round-trips a CSV matrix, runs SVD and MCR-ALS, reconstructs the signal, and plots profiles, residual maps, histogram, and residual singular values. A second synthetic A → B matrix demonstrates kinetic fitting and residuals.

## Verification and known differences

From `E:\SK-Ana\python`:

```powershell
.\.venv\Scripts\python.exe -m pytest tests -q
.\.venv\Scripts\python.exe -c 'import nbformat; from nbclient import NotebookClient; p="notebooks/skana_synthetic_demo.ipynb"; n=nbformat.read(p, as_version=4); NotebookClient(n, timeout=120, kernel_name="python3").execute(cwd="."); print("Notebook OK")'
```

The notebook may also be run interactively. The parity tests invoke `Rscript` from the repository's `.R_skana` runtime when present, otherwise a system `Rscript`; they skip explicitly when neither exists. They use generated, nonsecret matrices and the actual R `getC`, `getS`, `myals`, kinetic ODE/model, and Bayesian objective functions. A synthetic `.Rda` fixture tests Python import. No patient files are read.

`tests/test_streamlit_flow.py` runs the Streamlit widget path with built-in synthetic A → B data: import, narrow the delay range from 40 to 39 rows, change SVD rank, run MCR-ALS, check its reconstruction and residuals, and fit kinetics on the same selection. This checks the main interface workflow, not every optional control or uploaded file format. The same path was also exercised by clicking the running local site. For the exact two-component demo, ALS can reach numerical residual precision before its strict relative-RSS stopping condition is met; `converged=false` at the iteration limit does not imply a large reconstruction error, so inspect LOF and RSS together.

`tests/test_pipeline.py` checks a separate noisy 20×30 CSV round trip, an excluded delay, SVD, ALS convergence, and residual identity. The uploaded-file path was also exercised in the browser: the selected matrix was 19×30, and default two-component ALS converged in 164 iterations with LOF 0.0472% (200-iteration limit, threshold 1e-6). These deterministic synthetic checks do not establish accuracy for every experimental matrix or advanced constraint combination.

On the 18×24 synthetic two-component benchmark, R 4.3.3 and Python agreed to: SVD values `rtol=1e-10, atol=1e-12`; constrained/unconstrained C, closure C, normalized/mixed S `rtol=1e-6, atol=1e-8`; ALS model `rtol=1e-4, atol=1e-5`, LOF difference <0.01 percentage point, RSS difference <1e-5. Observed maximum model difference was `7.62e-7`, LOF difference `1.1e-5` percentage point, RSS difference `3.74e-13`, and iteration counts Python/R were `150/151`. R's loop can take one step beyond `maxiter`; Python stops at `maxiter`.

**Broadening differs:** R's `stats::convolve` extraction in `convolve_spectrum` shifts the output by five wavelength samples when sigma is 1.5 samples in the tested 24-point spectrum. The Python core performs centered, edge-padded convolution. The measured maximum absolute R/Python difference was `0.334`. Broadening-enabled ALS is therefore available as a Python analysis mode but is **not R-equivalent**. NMF initialization uses scikit-learn rather than R `NMFN`; smoothing uses a moving average rather than R `loess`; unimodality uses isotonic segments rather than `Iso::ufit`. These modes, fixed-spectrum and correction workflows have not all been cross-validated, so full numerical equivalence is not claimed.

The Python two-component ambiguity search follows R's transformation and positivity conditions, with a 100-step cap per direction to bound runtime. The three-component mode evaluates a finite grid around the identity; it does not exhaust R's unbounded search. R `rotAmb2`/`rotAmb3` could not be numerically compared in the local R 4.3.3 runtime: `if (class(Ri) != "try-error")` fails because an invertible matrix has two class strings (`matrix`, `array`). Python tests check reconstruction and positivity; these modes are not claimed R-validated.

On a synthetic A → B data set, the R/Python kinetic ODE and reconstructed model differ by at most `4.51e-7` and `4.71e-7`, respectively (`rtol=2e-5`, `atol=2e-6`). On identical deterministic noisy data, fitted rate constants were `0.7001585` (R) and `0.7001586` (Python); the regression gate is absolute difference < `0.01`. These checks do not validate arbitrary reaction schemes or posterior uncertainty. Python's finite-difference covariance and displayed 95% intervals are local approximations, clipped to prior bounds. The literal product `0` is treated as a sink rather than an R species; negative physical times are rejected. See [R feature inventory and coverage](R_PORT_INVENTORY.md) for remaining gaps and exact coverage.
