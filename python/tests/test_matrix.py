import numpy as np
import pytest

from skana import (MatrixData, baseline_correct, combine, compress, export_matrix,
                   read_matrix, select, suggest_baseline_intervals,
                   suggest_signal_mask)


def sample():
    return MatrixData(np.arange(12, dtype=float).reshape(3, 4),
                      np.array([0.0, 1.0, 2.0]), np.array([400., 500., 600., 700.]))


@pytest.mark.parametrize("orientation", ["wxd", "dxw"])
def test_matrix_roundtrip(orientation):
    original = sample()
    loaded = read_matrix(export_matrix(original, orientation), preset="CSV", orientation=orientation)
    np.testing.assert_allclose(loaded.matrix, original.matrix)
    np.testing.assert_allclose(loaded.delay, original.delay)
    np.testing.assert_allclose(loaded.wavelength, original.wavelength)


def test_compression_and_selection():
    data = compress(sample(), 2, 3)
    np.testing.assert_allclose(data.matrix, [[3., 5.], [9., 11.]])
    np.testing.assert_allclose(data.delay, [0.5, 2.])
    np.testing.assert_allclose(data.wavelength, [500., 700.])
    selected = select(sample(), delay_range=(1., 2.), wavelength_range=(500., 700.))
    assert selected.matrix.shape == (2, 3)


def test_automatic_signal_masks_keep_active_region():
    matrix = np.zeros((10, 8))
    matrix[2:8, 1:7] = 1
    data = MatrixData(matrix, np.arange(10), np.arange(8))
    delay_keep = suggest_signal_mask(data, "delay", 0.1)
    wavelength_keep = suggest_signal_mask(data, "wavelength", 0.1)
    assert not delay_keep[0] and not delay_keep[-1]
    assert wavelength_keep[1:7].all()
    selected = select(data, delay_keep=delay_keep, wavelength_keep=wavelength_keep)
    assert selected.matrix.sum() == matrix.sum()
    assert suggest_baseline_intervals(data, 0.1) == []  # one-row quiet lead after padding
    later = matrix.copy()
    later[:4] = 0
    later[4:8, 1:7] = 1
    assert suggest_baseline_intervals(MatrixData(later, np.arange(10), np.arange(8)), 0.1) == [(0., 2.)]


def test_combine_grid_validation():
    data = sample()
    mean = combine([data, data], "average")
    np.testing.assert_allclose(mean.matrix, data.matrix)
    tiled = combine([data, data], "tile_delay")
    assert tiled.matrix.shape == (6, 4)
    assert tiled.delay_id.tolist() == [1, 1, 1, 2, 2, 2]
    np.testing.assert_allclose(tiled.time, [0, 1, 2, 0, 1, 2])
    with pytest.raises(ValueError, match="identical"):
        combine([data, MatrixData(data.matrix, data.delay + 1, data.wavelength)], "average")


def test_csv_decimal_comma_and_header():
    content = "label;1;2\n0;400;500\n1;1,5;2,5\n2;3,5;4,5\n"
    data = read_matrix(content, preset="Other", sep=";", decimal=",")
    np.testing.assert_allclose(data.matrix, [[1.5, 2.5], [3.5, 4.5]])


def test_auto_format_and_blank_corner():
    content = ",400,500\n0,1,2\n1,3,4\n"
    np.testing.assert_allclose(read_matrix(content, preset="Auto").matrix,
                               [[1, 2], [3, 4]])
    content = "0;400;500\n0;1,2;2,3\n1;3,4;4,5\n"
    np.testing.assert_allclose(read_matrix(content, preset="Auto").matrix,
                               [[1.2, 2.3], [3.4, 4.5]])


def test_elyse_tsv_orientation():
    original = sample()
    csv = export_matrix(original, "dxw").decode("utf-8").replace(",", "\t")
    loaded = read_matrix(csv, preset="ELYSE (TSV)")
    np.testing.assert_allclose(loaded.matrix, original.matrix)


def test_invalid_axis_rejected():
    with pytest.raises(ValueError, match="Coordinate"):
        read_matrix("0,400,500\nx,1,2\n2,3,4\n", preset="CSV")


def test_baseline_per_wavelength_and_segment():
    data = MatrixData(np.array([[2., 4.], [4., 6.], [7., 9.], [9., 11.]]),
                      np.arange(4.), np.array([400., 500.]))
    corrected = baseline_correct(data, [(0., 1.), (2., 3.)])
    np.testing.assert_allclose(corrected.matrix, [[-1., -1.], [1., 1.], [-1., -1.], [1., 1.]])
