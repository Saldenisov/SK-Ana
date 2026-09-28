"""Direct R/Python checks on identical synthetic inputs; skip if R unavailable."""

from pathlib import Path
import shutil
import subprocess

import numpy as np
import pandas as pd
import pytest

from skana import MatrixData, als, svd
from spectrokinetic_transformers import convolve_spectrum, solve_C, solve_S


ROOT = Path(__file__).resolve().parents[2]
MICROMAMBA = ROOT / ".R_skana" / "tools" / "micromamba" / "micromamba.exe"
R_ENV = ROOT / ".R_skana" / "micromamba"


def _r_command():
    if MICROMAMBA.exists() and (R_ENV / "envs" / "R_skana").exists():
        return [str(MICROMAMBA), "run", "-r", str(R_ENV), "-n", "R_skana", "Rscript"]
    executable = shutil.which("Rscript")
    return [executable] if executable else None


def test_r_python_numerical_parity(tmp_path):
    command = _r_command()
    if command is None:
        pytest.skip("Rscript unavailable; install R runtime to run cross-language parity test")
    delay = np.linspace(0, 8, 18)
    wavelength = np.linspace(400, 700, 24)
    c_true = np.column_stack((np.exp(-delay / 2.4), 1 - np.exp(-delay / 2.4)))
    s_true = np.column_stack((np.exp(-((wavelength - 470) / 33) ** 2),
                              np.exp(-((wavelength - 620) / 43) ** 2)))
    matrix = c_true @ s_true.T
    data = MatrixData(matrix, delay, wavelength)
    u, _, vt = np.linalg.svd(matrix, full_matrices=False)
    c0, s0 = np.abs(u[:, :2]), np.abs(vt[:2].T)
    inputs = (("matrix.csv", matrix), ("c0.csv", c0), ("s0.csv", s0))
    for filename, values in inputs:
        np.savetxt(tmp_path / filename, values, delimiter=",", fmt="%.17g")
    subprocess.run(command + [str(Path(__file__).with_name("r_reference.R")),
                              str(ROOT), *(str(tmp_path / name) for name, _ in inputs),
                              str(tmp_path)], check=True, capture_output=True, text=True, timeout=120)

    def r_matrix(name):
        return pd.read_csv(tmp_path / name).to_numpy(dtype=float)

    np.testing.assert_allclose(svd(data, 2).singular_values, r_matrix("singular.csv").ravel()[:10],
                               rtol=1e-10, atol=1e-12)
    np.testing.assert_allclose(solve_C(s0, matrix, c0), r_matrix("get_c.csv"),
                               rtol=1e-6, atol=1e-8)
    np.testing.assert_allclose(solve_C(s0, matrix, c0, nonneg_c=False),
                               r_matrix("get_c_free.csv"), rtol=1e-6, atol=1e-8)
    np.testing.assert_allclose(solve_C(s0, matrix, c0, close_c=True, w_close_c=10),
                               r_matrix("get_c_closure.csv"), rtol=1e-6, atol=1e-8)
    np.testing.assert_allclose(solve_S(c0, matrix, s0, wavelength), r_matrix("get_s.csv"),
                               rtol=1e-6, atol=1e-8)
    np.testing.assert_allclose(solve_S(c0, matrix, s0, wavelength,
                                      nonneg_s=[True, False], norm_s=False),
                               r_matrix("get_s_mixed.csv"), rtol=1e-6, atol=1e-8)
    # R's stats::convolve extraction begins five samples before the centered
    # convolution for sigma=1.5. Preserve and report this known discrepancy.
    py_conv = convolve_spectrum(s0[:, 0], 1.5)
    r_conv = r_matrix("convolution.csv").ravel()
    assert np.max(np.abs(py_conv - r_conv)) > 0.3
    np.testing.assert_allclose(r_conv[5:], py_conv[:-5], rtol=1e-10, atol=1e-12)
    python_fit = als(data, n_components=2, maxiter=150, thresh=1e-7,
                     opt_s_first=False)
    r_model = r_matrix("als_model.csv")
    r_lof, r_rss, r_iter = r_matrix("als_metrics.csv").ravel()
    np.testing.assert_allclose(python_fit.model, r_model, rtol=1e-4, atol=1e-5)
    assert abs(python_fit.lof - r_lof) < 0.01
    assert abs(python_fit.rss - r_rss) < 1e-5
    assert abs(python_fit.iter - r_iter) <= 2
    print(f"R/Python: max model abs diff={np.max(np.abs(python_fit.model-r_model)):.3g}; "
          f"LOF diff={abs(python_fit.lof-r_lof):.3g} pp; "
          f"RSS diff={abs(python_fit.rss-r_rss):.3g}; "
          f"iterations={python_fit.iter}/{int(r_iter)}")
