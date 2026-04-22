# SK-Ana Scripts

Utility scripts for the isolated `R_skana` runtime and app launch.

## Scripts

| File | Description | Usage |
|------|-------------|-------|
| `setup_r_skana.sh` | Create or update the isolated local `R_skana` runtime on macOS/Linux | `./scripts/setup_r_skana.sh` |
| `setup_r_skana.bat` | Create or update the isolated local `R_skana` runtime on Windows | `scripts\\setup_r_skana.bat` |
| `r_skana.sh` | Open `R` or run a command inside the isolated `R_skana` runtime | `./scripts/r_skana.sh` |
| `r_skana.bat` | Windows wrapper for commands inside `R_skana` | `scripts\\r_skana.bat` |
| `run_app_3840.sh` | Launch SK-Ana on port 3840 from `R_skana` | `./scripts/run_app_3840.sh` |
| `run_app_3840.bat` | Launch SK-Ana on port 3840 from `R_skana` on Windows | `scripts\\run_app_3840.bat` |
| `run_app_3840.R` | R-side app launcher used from the isolated runtime | Internal use |
| `relock_r_skana.R` | Refresh `renv.lock` from the isolated runtime | `./scripts/r_skana.sh Rscript scripts/relock_r_skana.R` |
| `_dependencies.R` | SK-Ana package list and installer helpers | Internal use |
| `r_skana.environment.yml` | Base runtime definition for the isolated R environment | Internal use |

## Recommended flow

1. Get SK-Ana in one of these ways:

`git clone`:

```bash
git clone https://github.com/Saldenisov/SK-Ana.git
cd SK-Ana
```

ZIP download:
Download the ZIP from GitHub, unpack it, and open a terminal in the extracted `SK-Ana` folder.

2. Run the launcher:

### macOS / Linux

```bash
./run_app.sh
```

### Windows

```bat
run_app.bat
```

What the launcher does automatically:

- if the folder is already a git checkout, it updates it from `origin/master`
- if the folder came from a ZIP without `.git`, it initializes git in that extracted folder and then updates it from `origin/master`
- if Git is missing, it tries to install or bootstrap Git first
- it creates or refreshes the isolated `R_skana` runtime
- it launches the app

If you want to prepare the runtime in advance without starting Shiny, run `setup_r_skana.sh` / `setup_r_skana.bat` directly.

For locked-down Windows machines without administrator rights, `setup_r_skana.bat` can also use a pre-approved local executable:

```bat
set SK_ANA_MICROMAMBA_EXE=Z:\approved-tools\micromamba.exe
scripts\setup_r_skana.bat
```

You can also place that file at `scripts\vendor\windows\micromamba.exe` and the script will pick it up automatically.

## Use `R_skana` directly

### Open an R console in the isolated runtime

macOS / Linux:

```bash
./R_skana.sh
```

Windows:

```bat
R_skana.bat
```

### Run one-off commands in `R_skana`

macOS / Linux:

```bash
./R_skana.sh Rscript tests/testthat.R
```

Windows:

```bat
R_skana.bat Rscript tests\testthat.R
```

### Host/port override for app launch

```bash
SK_ANA_HOST=127.0.0.1 PORT=3842 ./run_app.sh
```

```bat
set SK_ANA_HOST=127.0.0.1
set PORT=3842
run_app.bat
```

## Dependencies

`setup_r_skana.*` installs:

- local `micromamba`
- an isolated environment named `R_skana`
- pinned base `R 4.3.3`
- repo-local `renv` library, cache, and sandbox under `.R_skana/`
- core SK-Ana packages from `conda-forge` where available
- remaining SK-Ana packages inside the isolated runtime without touching another R installation

The app then runs only inside this isolated runtime, so it does not need to reuse or reconfigure another R installation on the machine.

## Refresh the lockfile

macOS / Linux:

```bash
./R_skana.sh Rscript scripts/relock_r_skana.R
```

Windows:

```bat
R_skana.bat Rscript scripts\relock_r_skana.R
```

---

**Tip**: For Docker deployment, these scripts are not needed. Use:

```bash
docker run -d -p 3840:3840 --name skana saldenisov/skana:latest
```
