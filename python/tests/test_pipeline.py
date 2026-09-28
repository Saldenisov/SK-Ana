"""Synthetic CSV-to-analysis integration without a Streamlit session."""

import numpy as np

from skana import MatrixData, als, export_matrix, read_matrix, select, svd


def test_noisy_csv_selection_svd_and_als_pipeline():
    delay = np.linspace(0, 5, 20)
    wavelength = np.linspace(400, 700, 30)
    concentrations = np.column_stack((np.exp(-delay), 1 - np.exp(-delay)))
    spectra = np.column_stack((np.exp(-((wavelength - 480) / 40) ** 2),
                               np.exp(-((wavelength - 610) / 45) ** 2)))
    matrix = concentrations @ spectra.T
    matrix += np.random.default_rng(8).normal(0, 2e-4, matrix.shape)

    content = export_matrix(MatrixData(matrix, delay, wavelength), "dxw")
    imported = read_matrix(content, preset="CSV", orientation="dxw")
    np.testing.assert_allclose(imported.matrix, matrix, atol=1e-14)

    selected = select(imported, excluded_delays=[(0.0, 0.0)])
    assert selected.matrix.shape == (19, 30)
    assert selected.delay[0] > 0

    decomposition = svd(selected, 2)
    assert decomposition.model.shape == selected.matrix.shape
    assert decomposition.statistics.iloc[1].lof_pct < 0.1

    result = als(selected, n_components=2, maxiter=300, thresh=1e-6)
    assert result.converged
    assert result.lof < 0.1
    np.testing.assert_allclose(result.model + result.resid, selected.matrix, atol=1e-12)
