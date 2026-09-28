import numpy as np

from skana import MatrixData, als, als_contributions, svd


def synthetic():
    d = np.linspace(0, 6, 20)
    w = np.linspace(400, 700, 30)
    c = np.column_stack((np.exp(-d / 1.8), 1 - np.exp(-d / 1.8)))
    s = np.column_stack((np.exp(-((w - 470) / 28) ** 2),
                         np.exp(-((w - 620) / 38) ** 2)))
    return MatrixData(c @ s.T, d, w)


def test_svd_reconstruction_and_statistics():
    data = synthetic()
    result = svd(data, 2)
    np.testing.assert_allclose(result.model, data.matrix, atol=1e-12)
    assert result.statistics.iloc[1].lof_pct < 1e-10
    assert np.isclose(result.statistics.iloc[0].residual_sd,
                      np.std(data.matrix - np.outer(result.u[:, 0], result.vt[0]) *
                             result.singular_values[0], ddof=1))


def test_als_reconstruction():
    data = synthetic()
    result = als(data, n_components=2, maxiter=300, thresh=1e-8)
    assert result.lof < 1.0
    np.testing.assert_allclose(result.resid, data.matrix - result.model, atol=1e-12)
    assert result.c.min() >= -1e-10
    assert result.s.min() >= -1e-10
    np.testing.assert_allclose(np.sum(als_contributions(result), axis=0), result.model)


def test_fixed_spectrum_normalized_as_r():
    data = synthetic()
    fixed = np.exp(-((data.wavelength - 470) / 28) ** 2) * 2
    result = als(data, n_components=2, fixed_spectra=fixed, maxiter=40)
    np.testing.assert_allclose(result.s[:, 0], fixed / np.max(np.abs(fixed)), atol=1e-12)


def test_broadened_contributions_sum_to_model():
    data = synthetic()
    result = als(data, n_components=2, maxiter=6, broadening=[True, False])
    np.testing.assert_allclose(np.sum(als_contributions(result), axis=0), result.model,
                               atol=1e-12)
