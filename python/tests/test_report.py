import numpy as np

from skana import MatrixData, build_report, concentrations, fit_kinetics, parse_scheme, svd


def test_self_contained_html_report():
    data = MatrixData(np.arange(12).reshape(3, 4), np.arange(3),
                      np.arange(4), "synthetic <sample>")
    html = build_report(data, svd_result=svd(data, 2)).decode("utf-8")
    assert "synthetic &lt;sample&gt;" in html
    assert "data:image/png;base64," in html
    assert "Singular value decomposition" in html
    assert "lof_pct" in html


def test_report_includes_kinetic_scheme_and_integrated_profiles():
    time = np.linspace(0, 2, 8)
    wavelength = np.linspace(400, 600, 10)
    scheme_text = "A -> B ; 0.5\nc0_A_1 = 1\neps_A = 1\neps_B = 1\n"
    scheme = parse_scheme(scheme_text)
    blank = MatrixData(np.zeros((8, 10)), time, wavelength)
    c = concentrations(blank, scheme)
    s = np.column_stack((np.linspace(1, 0, 10), np.linspace(0, 1, 10)))
    data = MatrixData(c @ s.T, time, wavelength)
    result = fit_kinetics(data, scheme)
    html = build_report(data, kinetic_result=result,
                        scheme_text=scheme_text).decode("utf-8")
    assert "Chemical scheme" in html
    assert "Integrated kinetics" in html
    assert "A -&gt; B" in html
