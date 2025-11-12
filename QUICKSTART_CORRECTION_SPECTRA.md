# Quick Start: Correction Spectra ALS

## Installation (30 seconds)

✅ **Already done!** The following files have been created and integrated:

1. `server_files/ALS_CorrectionSpectra.R` – Core algorithm
2. `ui_files/ALSInputConstraintsCorrectionSpectra.R` – UI panel  
3. `server_files/ALS_CorrectionSpectra_Server.R` – Server integration
4. `server.R` – Updated to source the new server file
5. `docs/correction_spectra.md` – Full documentation

## First Use (3 steps)

### 1️⃣ Load Reference Spectra
- Go to **ALS Constraints → Spectra**
- Click **"Fix spectral shape(s)"**
- Upload your reference spectrum file (.csv, .dat, .txt)
- Check the spectrum name to enable it

### 2️⃣ Enable Correction Spectra
A new panel appears: **"Correction Spectra (paired with fixed spectra)"**

- ✅ Check **"Enable correction spectra"**
- Adjust **"Correction penalty (λ)": -2 (default is good)**
- Keep **"Zero-mean corrections"** enabled (recommended)

### 3️⃣ Run ALS
- Set **"# Spectra"** = 2 fixed + 2 corrections + 1 free = **5** (for example)
- Click **"Run ALS"** as usual
- Results automatically split into:
  - S_1, S_2 = fixed (reference shapes)
  - S_3, S_4 = corrections (paired deviations)
  - S_5 = free (new component)

## What You Get

**Spectra (S):**
- Fixed spectra stay at reference values
- Corrections show deviations from references
- Corrections are small + zero-mean + orthogonal to fixed

**Kinetics (C):**
- Fixed: normal kinetics
- Corrections: **same time profile as paired fixed** (just scaled)
- Free: independent kinetics

## Example Interpretation

If your 2 reference spectra are "Species A" and "Species B":

```
Component    Spectrum         Kinetics              Meaning
───────────────────────────────────────────────────────────
S_1 / C_1    Species A ref    Normal decay          Pure A reference
S_2 / C_2    Species B ref    Normal decay          Pure B reference  
S_3 / C_3    A correction     ∝ C_1 but different   Where ref A fails
S_4 / C_4    B correction     ∝ C_2 but different   Where ref B fails
S_5 / C_5    Free spectrum    Independent           Unknown species
```

If S_3 and S_4 are near zero → references are perfect ✨
If S_3 and S_4 have structure → references have systematic errors

## Parameter Guide

### Correction penalty (λ)
Slider from -4 to 0, corresponds to λ = 10^(value)

| Slider | λ | Effect |
|--------|---|--------|
| -4 | 10⁻⁴ | Corrections can be large (weak penalty) |
| -2 | 10⁻² | Moderate (good default) |
| 0 | 10⁰ | Corrections are very small (strong penalty) |

**Recommendation:** Start with -2, adjust if needed

### Zero-mean corrections
- **ON (default):** Prevents corrections from acting as baseline shift
- **OFF:** Allows any structure (rarely needed)

## Troubleshooting

| Problem | Solution |
|---------|----------|
| "Correction Spectra" panel missing | Load a fixed spectrum file first |
| Corrections are almost zero | Good! References fit well. Or try λ = -4 to allow larger |
| Corrections too large (> 50%) | References don't match data. Try different files or λ = -1 |
| Program crashes | Check Console (F12) for errors. Verify spectrum file format |

## Full Documentation

See `docs/correction_spectra.md` for:
- Mathematical model
- Advanced usage
- All parameters explained
- Output file interpretation
- Detailed troubleshooting

## Files Modified

- `server.R` – Added: `"ALS_CorrectionSpectra_Server.R"` to file list
- `ui_files/ALSInputConstraintsSpectra.R` – Added: `source()` call to new UI

## Files Created

- `server_files/ALS_CorrectionSpectra.R` (494 lines)
- `ui_files/ALSInputConstraintsCorrectionSpectra.R` (97 lines)
- `server_files/ALS_CorrectionSpectra_Server.R` (276 lines)
- `docs/correction_spectra.md` (full docs)
- `CORRECTION_SPECTRA_INTEGRATION.md` (integration guide)
- This file

---

**Ready to try it? Restart the app and go to ALS Constraints → Spectra!** 🎯
