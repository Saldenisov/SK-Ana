"""Synthetic checks for the standalone kinetic model."""

from pathlib import Path
import subprocess
from io import BytesIO
from zipfile import ZipFile

import numpy as np
import pandas as pd
import pytest

from skana import (MatrixData, concentrations, fit_kinetics, format_scheme,
                   parse_scheme, solve_spectra, stoichiometry)
from test_r_parity import ROOT, _r_command
from skana.app import _zip_kinetics


SCHEME = "A -> B ; 0.7 / 1.5\nc0_A_1 = 1\neps_A = 1\neps_B = 0.8\n"


def synthetic():
    time = np.linspace(0.04, 5, 35)
    wavelength = np.linspace(400, 680, 50)
    scheme = parse_scheme(SCHEME)
    blank = MatrixData(np.zeros((time.size, wavelength.size)), time, wavelength)
    c = concentrations(blank, scheme)
    s = np.column_stack((np.exp(-((wavelength - 470) / 35) ** 2),
                         np.exp(-((wavelength - 560) / 40) ** 2)))
    s /= s.max(axis=0)
    s[:, 1] *= 0.8
    return MatrixData(c @ s.T, time, wavelength), scheme, c


def test_scheme_and_ode():
    data, scheme, c = synthetic()
    assert scheme.species == ("A", "B")
    left, net = stoichiometry(scheme)
    np.testing.assert_array_equal(left, [[1, 0]])
    np.testing.assert_array_equal(net, [[-1, 1]])
    np.testing.assert_allclose(c[:, 0], np.exp(-0.7 * data.time), atol=3e-6)
    np.testing.assert_allclose(c.sum(axis=1), 1, atol=1e-8)
    again = parse_scheme(format_scheme(scheme))
    np.testing.assert_allclose(again.initial, scheme.initial)
    np.testing.assert_allclose(again.extinction, scheme.extinction)
    np.testing.assert_allclose([r.rate for r in again.reactions], [0.7])


def test_physical_time_preserved_across_experiments():
    time = np.tile(np.array([0., 1., 2., 3.]), 2)
    scheme = parse_scheme(SCHEME + "c0_A_2 = 0.5\n", n_experiments=2)
    data = MatrixData(np.zeros((8, 3)), np.arange(8), np.arange(3),
                      delay_id=np.repeat([1, 2], 4), time=time)
    c = concentrations(data, scheme)
    np.testing.assert_allclose(c[:4, 0], np.exp(-0.7 * time[:4]), atol=3e-6)
    np.testing.assert_allclose(c[4:, 0], 0.5 * np.exp(-0.7 * time[4:]), atol=3e-6)


def test_sequential_and_bimolecular_reaction_networks():
    time = np.linspace(0, 3, 20)
    blank = MatrixData(np.zeros((20, 3)), time, np.arange(3))
    sequence = parse_scheme("A -> B ; 1\nB -> C ; 0.5\nc0_A_1 = 1\neps_A = 1\n")
    c = concentrations(blank, sequence)
    np.testing.assert_allclose(c[:, 0], np.exp(-time), atol=3e-6)
    np.testing.assert_allclose(c[:, 1], 2 * (np.exp(-0.5 * time) - np.exp(-time)), atol=3e-6)
    np.testing.assert_allclose(c.sum(axis=1), 1, atol=1e-8)
    bimolecular = parse_scheme("A + B -> C ; 0.8\nc0_A_1 = 1\nc0_B_1 = 1\neps_C = 1\n")
    c = concentrations(blank, bimolecular)
    np.testing.assert_allclose(c[:, 0], 1 / (1 + 0.8 * time), atol=3e-6)
    np.testing.assert_allclose(c[:, 1], c[:, 0], atol=1e-8)
    np.testing.assert_allclose(c[:, 2], 1 - c[:, 0], atol=1e-8)


def test_local_fit_recovers_rate_and_reconstructs():
    data, scheme, _ = synthetic()
    result = fit_kinetics(data, scheme)
    assert result.success
    assert abs(result.parameters["k_1"] - 0.7) < 2e-4
    assert result.lof < 0.01
    assert result.covariance is None or result.covariance.shape == (1, 1)
    np.testing.assert_allclose(result.model + result.residual, data.matrix, atol=1e-12)
    with ZipFile(BytesIO(_zip_kinetics(data, result, format_scheme(scheme)))) as archive:
        assert {"scheme.txt", "concentrations_all.csv", "spectra.csv",
                "model.csv", "residual.csv", "parameters.json",
                "local_uncertainty.csv"}.issubset(archive.namelist())


def test_kinetic_spectral_smoothing_is_finite_and_normalized():
    data, scheme, c = synthetic()
    active, spectra = solve_spectra(data, c, scheme, scheme.extinction,
                                    nonnegative=True, smooth=0.3)
    assert active.shape == c.shape
    assert np.isfinite(spectra).all()
    assert np.min(spectra) >= 0
    np.testing.assert_allclose(spectra.max(axis=0), [1, 0.8], atol=1e-12)


def test_weighted_global_and_local_fit():
    data, scheme, _ = synthetic()
    noise = 0.002 * np.sin(np.arange(data.matrix.size).reshape(data.matrix.shape))
    noisy = MatrixData(data.matrix + noise, data.time, data.wavelength)
    result = fit_kinetics(noisy, scheme, weighted=True, sigma=0.002,
                          global_generations=1, population_factor=5)
    assert result.success
    assert abs(result.parameters["k_1"] - 0.7) < 0.02


def test_r_python_kinetics_on_same_input(tmp_path):
    command = _r_command()
    if command is None:
        pytest.skip("Rscript unavailable")
    data, scheme, c = synthetic()
    np.savetxt(tmp_path / "time.csv", data.time, delimiter=",", fmt="%.17g")
    np.savetxt(tmp_path / "matrix.csv", data.matrix, delimiter=",", fmt="%.17g")
    subprocess.run(command + [str(Path(__file__).with_name("r_kinetic_reference.R")),
                              str(ROOT), str(tmp_path / "time.csv"),
                              str(tmp_path / "matrix.csv"), str(tmp_path)],
                   check=True, capture_output=True, text=True, timeout=120)
    r_c = pd.read_csv(tmp_path / "kinetic_concentrations.csv").to_numpy(float)
    r_model = pd.read_csv(tmp_path / "kinetic_model.csv").to_numpy(float)
    np.testing.assert_allclose(c, r_c, rtol=2e-5, atol=2e-6)
    fixed = scheme.with_values(rate_factors=np.array([1.]))
    result = fit_kinetics(data, fixed)
    np.testing.assert_allclose(result.model, r_model, rtol=2e-5, atol=2e-6)
    print(f"Kinetics R/Python max concentration difference {np.max(np.abs(c-r_c)):.3g}; "
          f"model difference {np.max(np.abs(result.model-r_model)):.3g}")


def test_r_python_kinetic_optimization_on_noisy_input(tmp_path):
    command = _r_command()
    if command is None:
        pytest.skip("Rscript unavailable")
    data, scheme, _ = synthetic()
    noise = 0.003 * np.sin(np.arange(data.matrix.size).reshape(data.matrix.shape) * 0.37)
    noisy = MatrixData(data.matrix + noise, data.time, data.wavelength)
    np.savetxt(tmp_path / "time.csv", noisy.time, delimiter=",", fmt="%.17g")
    np.savetxt(tmp_path / "matrix.csv", noisy.matrix, delimiter=",", fmt="%.17g")
    subprocess.run(command + [str(Path(__file__).with_name("r_kinetic_reference.R")),
                              str(ROOT), str(tmp_path / "time.csv"),
                              str(tmp_path / "matrix.csv"), str(tmp_path), "fit"],
                   check=True, capture_output=True, text=True, timeout=120)
    _, r_rate, convergence = pd.read_csv(tmp_path / "kinetic_fit.csv").to_numpy(float).ravel()
    assert convergence == 0
    result = fit_kinetics(noisy, scheme)
    assert result.success
    assert abs(result.parameters["k_1"] - r_rate) < 0.01
    print(f"Noisy kinetic rate R/Python {r_rate:.7g}/{result.parameters['k_1']:.7g}")
