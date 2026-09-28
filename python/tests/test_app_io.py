from io import BytesIO
from zipfile import ZipFile

import numpy as np

from skana import MatrixData, als, rotate_two
from skana.app import (_fixed_spectra, _project_bytes, _read_project,
                       _cut_indices, _workspace_zip, _zip_als, _zip_rotation)


class Upload:
    def __init__(self, content):
        self.content = content

    def getvalue(self):
        return self.content


def data():
    d = np.linspace(0, 3, 8)
    w = np.linspace(400, 700, 12)
    matrix = np.outer(np.exp(-d), np.exp(-((w - 480) / 35) ** 2)) + np.outer(
        1 - np.exp(-d), np.exp(-((w - 600) / 45) ** 2))
    return MatrixData(matrix, d, w, "synthetic", "dxw")


def test_project_roundtrip_without_pickle():
    original = data()
    restored = _read_project(Upload(_project_bytes(original)))
    np.testing.assert_allclose(restored.matrix, original.matrix)
    np.testing.assert_allclose(restored.delay, original.delay)
    np.testing.assert_allclose(restored.time, original.time)
    assert restored.orientation == "dxw"


def test_fixed_spectra_interpolation():
    original = data()
    source = b"wavelength,S_1\n400,0\n550,1\n700,0\n"
    interpolated = _fixed_spectra(Upload(source), original.wavelength)
    assert interpolated.shape == (12, 1)
    assert interpolated.min() >= 0


def test_dense_cut_averages_nearby_points():
    axis = np.linspace(0, 10, 201)
    indices = _cut_indices(axis, 5.0)
    assert indices.size > 1
    assert 100 in indices
    assert _cut_indices(axis, 0).tolist() == [0]


def test_als_zip_contents():
    original = data()
    result = als(original, n_components=2, maxiter=40)
    with ZipFile(BytesIO(_zip_als(original, result))) as archive:
        assert set(archive.namelist()) == {
            "kinetics.csv", "spectra.csv", "model.csv", "residual.csv", "summary.json"
        }
        assert len(archive.read("model.csv")) > 100
    rotations = rotate_two(result.c, result.s, step=0.1)
    assert rotations
    with ZipFile(BytesIO(_zip_rotation(original, rotations[0]))) as archive:
        assert set(archive.namelist()) == {"kinetics.csv", "spectra.csv", "rotation.json"}


def test_workspace_zip_contains_reopenable_project_and_selection():
    original = data()
    config = {"matrix_dimensions": list(original.matrix.shape), "delay_range": [0, 3]}
    with ZipFile(BytesIO(_workspace_zip(original, original, config))) as archive:
        assert set(archive.namelist()) == {
            "project.npz", "selected_matrix.csv", "selections.json", "svd_statistics.csv"
        }
        restored = _read_project(Upload(archive.read("project.npz")))
        np.testing.assert_allclose(restored.matrix, original.matrix)
