import numpy as np

from skana import rotate_three, rotate_two


def test_rotations_preserve_model_and_positivity():
    c = np.array([[1., 0.], [0.7, 0.3], [0.3, 0.7], [0., 1.]])
    s = np.array([[1., 0.], [0.7, 0.3], [0.2, 0.8], [0., 1.]])
    solutions = rotate_two(c, s, step=0.1, max_steps=20)
    assert solutions
    for solution in solutions:
        np.testing.assert_allclose(solution.c @ solution.s.T, c @ s.T, atol=1e-12)
        assert solution.c.min() >= -0.01 * solution.c.max() - 1e-12
        assert solution.s.min() >= -0.01 * solution.s.max() - 1e-12


def test_three_component_rotations_preserve_model():
    c = np.array([[1., 0., 0.], [0.6, 0.3, 0.1], [0.2, 0.6, 0.2],
                  [0.1, 0.3, 0.6], [0., 0., 1.]])
    s = np.array([[1., 0., 0.], [0.5, 0.4, 0.1], [0.1, 0.7, 0.2],
                  [0.1, 0.3, 0.6], [0., 0., 1.]])
    solutions = rotate_three(c, s, step=0.05, radius=1)
    assert solutions
    for solution in solutions:
        np.testing.assert_allclose(solution.c @ solution.s.T, c @ s.T, atol=1e-12)
        assert solution.c.min() >= -0.01 * solution.c.max() - 1e-12
        assert solution.s.min() >= -0.01 * solution.s.max() - 1e-12
