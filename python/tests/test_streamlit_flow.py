"""Exercise the synthetic demo through Streamlit's widget execution path."""

from pathlib import Path

import numpy as np
from streamlit.testing.v1 import AppTest


APP = Path(__file__).resolve().parents[1] / "skana" / "app.py"


def _widget(elements, label):
    return next(element for element in elements if element.label == label)


def _no_exceptions(app):
    assert len(app.exception) == 0, [error.value for error in app.exception]
    assert len(app.error) == 0, [error.value for error in app.error]


def test_demo_selection_svd_als_and_kinetics():
    app = AppTest.from_file(str(APP), default_timeout=120).run()
    _widget(app.checkbox, "Use synthetic A → B demo data").check().run()
    _no_exceptions(app)
    assert any("40 delays × 65 wavelengths" in item.value for item in app.success)

    _widget(app.slider, "Delay range").set_value((0.01, 5.0)).run()
    _no_exceptions(app)
    assert any("Selected: 39 × 65" in item.value for item in app.markdown)

    _widget(app.number_input, "Model rank").set_value(3).run()
    _no_exceptions(app)
    assert _widget(app.number_input, "Model rank").value == 3
    assert app.dataframe[0].value.iloc[0]["singular_value"] > 0

    _widget(app.number_input, "Maximum iterations").set_value(20).run()
    _widget(app.button, "Run MCR-ALS").click().run()
    _no_exceptions(app)
    als_result = app.session_state["als_result"]
    als_selection = app.session_state["als_selection"]
    assert als_selection.matrix.shape == (39, 65)
    assert als_result.model.shape == (39, 65)
    assert np.isfinite(als_result.lof)
    np.testing.assert_allclose(als_result.model + als_result.resid,
                               als_selection.matrix, atol=1e-10)

    _widget(app.button, "Run kinetic fit").click().run()
    _no_exceptions(app)
    kinetic_result = app.session_state["kinetic_result"]
    assert kinetic_result.model.shape == (39, 65)
    assert np.isfinite(kinetic_result.lof)
