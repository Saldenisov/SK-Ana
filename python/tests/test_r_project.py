"""Pure Python import of SK-Ana R project serialization."""

from pathlib import Path
import subprocess

import numpy as np
import pytest

from skana import read_r_project, read_r_selection
from test_r_parity import _r_command


def test_import_r_project_without_r_runtime_at_read_time(tmp_path):
    command = _r_command()
    if command is None:
        pytest.skip("Rscript unavailable to generate synthetic fixture")
    path = tmp_path / "synthetic.Rda"
    subprocess.run(command + [str(Path(__file__).with_name("r_project_fixture.R")), str(path)],
                   check=True, capture_output=True, text=True, timeout=120)
    data = read_r_project(path.read_bytes())
    assert data.name == "synthetic_project"
    assert data.matrix.shape == (3, 4)
    np.testing.assert_array_equal(data.delay, [10, 11, 12])
    np.testing.assert_array_equal(data.time, [0, 1, 2])
    np.testing.assert_array_equal(data.wavelength, [400, 500, 600, 700])
    np.testing.assert_array_equal(data.matrix, np.arange(1, 13).reshape(3, 4, order="F"))


def test_non_project_rejected():
    with pytest.raises(ValueError, match="Cannot read R project"):
        read_r_project(b"not an R file")


def test_import_r_selections(tmp_path):
    command = _r_command()
    if command is None:
        pytest.skip("Rscript unavailable to generate synthetic fixture")
    path = tmp_path / "selections.Rda"
    subprocess.run(command + [str(Path(__file__).with_name("r_selection_fixture.R")), str(path)],
                   check=True, capture_output=True, text=True, timeout=120)
    config = read_r_selection(path.read_bytes())
    assert config["matrix_dimensions"] == (8, 6)
    assert config["delay_range"] == (2, 7)
    assert config["wavelength_range"] == (430, 650)
    assert config["baseline_ranges"] == [(2, 3)]
    assert config["excluded_delays"] == [(4, 5)]
    assert config["excluded_wavelengths"] == [(490, 510)]
