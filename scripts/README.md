# SK-Ana Scripts

Utility scripts for development and deployment.

## Scripts

| File | Description | Usage |
|------|-------------|-------|
| `run_app_3840.R` | Launch SK-Ana on port 3840 | `Rscript scripts/run_app_3840.R` or double-click |
| `_dependencies.R` | Dependency management | Internal use |

## Running SK-Ana

### Using run_app_3840.R

**From terminal:**
```bash
Rscript scripts/run_app_3840.R
```

**From R console:**
```r
source("scripts/run_app_3840.R")
```

**From File Explorer (Windows) or Finder (Mac):**
- Double-click `scripts/run_app_3840.R`
- App will launch on http://localhost:3840

### Direct Shiny Launch

Alternatively, from project root:
```r
shiny::runApp(".", port = 3840)
```

---

## Dependencies

The `_dependencies.R` script manages package dependencies. It's used internally by the application.

---

**Tip**: For Docker deployment, these scripts are not needed. Use:
```bash
docker run -d -p 3840:3840 --name skana saldenisov/skana:latest
```
