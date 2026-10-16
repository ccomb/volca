"""A reduced model evaluated here gives the scores of the whole system solved again.

The system: root R uses P and Q; P (moved by y) uses Q, uses T only away from
y = 2, uses some of itself, and its reference amount moves; Q uses P back (a
loop). R is moved by z. The model is built from the full solve at the defaults
as the engine builds it, then compared with the full solve at other values.
"""

from __future__ import annotations

import pytest

np = pytest.importorskip("numpy")

from volca import ParameterName, ReducedModel  # noqa: E402
from volca import _reduced  # noqa: E402
from volca._reduced import _Evaluator  # noqa: E402

R, P, Q, T = range(4)
K = 0.4
# Characterisation factors of the two emissions, by method.
H = np.array([[1.0, 0.5], [0.0, 2.0]])
DEFAULTS = (2.0, 0.5)


def _system(y: float, z: float) -> tuple:
    """The technosphere per unit of each process, and its emissions per unit."""
    reference = np.array([1.0, (y + 1) - 0.1 * y, 1.0, 1.0])
    tech = np.zeros((4, 4))
    for supplier, consumer, amount in [(P, R, 2 * z), (Q, R, 0.5 * 0.3), (Q, P, 2 * y * K), (T, P, y - 2), (P, Q, 0.2)]:
        tech[supplier, consumer] = amount
    bio = np.zeros((2, 4))
    for flow, process, amount in [(0, R, 0.1), (0, P, y), (1, P, 0.5), (1, Q, 1.0), (0, T, 3.0)]:
        bio[flow, process] = amount
    return tech / reference, bio / reference


def _full(y: float, z: float) -> np.ndarray:
    tech, bio = _system(y, z)
    x = np.linalg.solve(np.eye(4) - tech, np.eye(4)[R])
    return H @ bio @ x


def _model() -> ReducedModel:
    tech, bio = _system(*DEFAULTS)
    g = np.linalg.inv(np.eye(4) - tech)
    moving, suppliers = [R, P], [P, Q, T]
    line = lambda stored, formula=None: {"stored": stored, "formula": formula}  # noqa: E731
    processes = [
        {
            "processId": "root-act_root-prod",
            "name": "R",
            "scaling": g[R, R],
            "values": {},
            "parameters": {"z": 1},
            "bindings": [],
            "reference": {"outputs": [line(1.0)], "selfLoops": [], "inputs": []},
            "technosphere": [
                {"supplier": 0, "factor": 1.0, "line": line(1.0, ["*", 2, "z"])},
                {"supplier": 1, "factor": 0.5, "line": line(0.3)},
            ],
            "biosphere": [{"factors": [1.0, 0.0], "line": line(0.1)}],
        },
        {
            "processId": "p",
            "name": "P",
            "scaling": g[P, R],
            "values": {"k": K},
            "parameters": {"y": 0},
            "bindings": [{"name": "out", "formula": ["+", "y", 1]}],
            "reference": {"outputs": [line(3.0, "out")], "selfLoops": [line(0.2, ["*", 0.1, "y"])], "inputs": []},
            "technosphere": [
                {"supplier": 1, "factor": 2.0, "line": line(0.8, ["*", "y", "k"])},
                {"supplier": 2, "factor": 1.0, "line": line(0.0, ["-", "y", 2])},
            ],
            "biosphere": [
                {"factors": [1.0, 0.0], "line": line(2.0, "y")},
                {"factors": [0.5, 2.0], "line": line(0.5)},
            ],
        },
    ]
    return ReducedModel.from_json(
        {
            "methods": [{"id": "m1", "name": "Climate", "unit": "kg"}, {"id": "m2", "name": "Water", "unit": "m3"}],
            "excludeLongTerm": False,
            "baseline": list(_full(*DEFAULTS)),
            "parameters": [
                {"process": "p", "parameter": {"name": "y", "value": DEFAULTS[0]}},
                {"parameter": {"name": "z", "value": DEFAULTS[1]}},
            ],
            "processes": processes,
            "suppliers": [
                {"processId": str(i), "database": "db", "scores": list(H @ bio @ g[:, i]), "scalings": [g[p, i] for p in moving]}
                for i in suppliers
            ],
            "scoringSets": [
                {
                    "name": "Single score",
                    "unit": "Pt",
                    "variables": {"cc": 0, "wu": 1},
                    "computed": [{"name": "tot", "formula": ["+", "cc", "wu"]}],
                    "normalization": {"cc": 2.0},
                    "weighting": {"cc": 0.5, "wu": 1.0, "tot": 0.1},
                    "scores": {"single": ["+", "cc", "wu"], "total": "tot"},
                    "displayMultiplier": 1000.0,
                }
            ],
        },
        "root-act",
    )


@pytest.fixture()
def sets() -> np.ndarray:
    rng = np.random.default_rng(7)
    return np.column_stack([rng.uniform(1.5, 3.0, 50), rng.uniform(0.2, 1.0, 50)])


def test_scores_equal_the_full_solve(sets):
    got = _model().evaluate(sets)
    expected = np.array([_full(y, z) for y, z in sets])
    assert np.allclose(got.scores, expected, rtol=1e-10, atol=0)
    single = got.scoring_sets["Single score"]
    assert np.allclose(single["single"], (expected[:, 0] / 2 * 0.5 + expected[:, 1]) * 1000, rtol=1e-12)
    assert np.allclose(single["total"], expected.sum(axis=1) * 0.1 * 1000, rtol=1e-12)


def test_defaults_give_the_baseline():
    model = _model()
    assert np.array_equal(model.evaluate([DEFAULTS]).scores[0], np.array(model.baseline))


def test_sets_taken_a_few_at_a_time_give_the_same_scores(sets, monkeypatch):
    whole = _model().evaluate(sets).scores
    monkeypatch.setattr(_reduced, "_CHUNK_BYTES", 8 * 2 * (3 + 2) * 7)
    assert np.allclose(_model().evaluate(sets).scores, whole, rtol=1e-13, atol=0)


def _with_emission(formula: object) -> ReducedModel:
    """The model with P's first emission given this formula."""
    model = _model()
    model.processes[1]["biosphere"][0]["line"]["formula"] = formula
    return model


def test_a_set_without_a_finite_amount_is_refused_by_position():
    model = _with_emission(["ln", "y"])
    with pytest.raises(ValueError, match=r"set 1: an emission of 'P' has no finite amount"):
        model.evaluate([DEFAULTS, (0.0, 0.5)])


def test_a_set_that_turns_the_root_over_is_refused_by_position():
    model = _model()
    model.processes[0]["reference"]["outputs"][0]["formula"] = ["-", ["*", "z", 4], 1]
    with pytest.raises(ValueError, match=r"set 0: the reference amount of 'R' changes sign"):
        model.evaluate([(2.0, 0.1)])


def test_an_unknown_function_is_named():
    model = _with_emission(["gamma", "y"])
    with pytest.raises(ValueError, match="no function 'gamma' of 1 arguments"):
        model.evaluate([DEFAULTS])


def test_a_set_must_hold_one_value_per_parameter():
    with pytest.raises(ValueError, match="2 values"):
        _model().evaluate([[1.0]])


@pytest.mark.parametrize(
    "tree, value",
    [
        (["-", 3], -3),
        (["-", 7, 2], 5),
        (["not", 0], 1),
        (["abs", -2], 2),
        (["sqr", 3], 9),
        (["sqrt", 9], 3),
        (["ln", 1], 0),
        (["log10", 1000], 3),
        (["log", 8, 2], 3),
        (["trunc", -2.75], -2),
        (["frac", -2.75], -0.75),
        (["round", 2.5], 3),
        (["round", -2.5], -2),
        (["roundeven", 2.5], 2),
        (["ceil", 1.2], 2),
        (["floor", -1.2], -2),
        (["cotan", 1], 1 / np.tan(1)),
        (["ipower", 2, 3.7], 8),
        (["^", 2, 0.5], 2**0.5),
        (["div", 7.6, 2], 4),
        (["div", -7, 2], -3),
        (["mod", -7, 3], -1),
        (["xor", 1, 0], 1),
        (["xor", 2, 3], 0),
        (["=", 2, 2], 1),
        (["<>", 2, 2], 0),
        (["<=", 2, 3], 1),
        (["min", 3, 1, 2], 1),
        (["max", 3, 1, 2], 3),
        (["sum", 1, 2, 3], 6),
        (["average", 1, 2, 6], 3),
        (["and", 1, 2, 0], 0),
        (["or", 0, 0, 5], 1),
        (["if", ["<", 1, 2], 10, 20], 10),
        (["if", 0, ["/", 1, 0], 20], 20),
    ],
)
def test_each_function_gives_its_value(tree, value):
    with np.errstate(all="ignore"):
        assert _Evaluator(np)(tree, {}) == pytest.approx(value)


def test_div_by_what_rounds_to_zero_gives_no_number():
    assert np.isnan(_Evaluator(np)(["div", 3, 0.4], {}))


def test_reduced_model_dispatch(mocked_client, make_response):
    client, session = mocked_client
    client._checked = True
    client._server_wire = 69
    session.post.return_value = make_response(
        {"methods": [], "excludeLongTerm": True, "baseline": [], "parameters": [], "processes": [], "suppliers": [], "scoringSets": []}
    )
    model = client.reduced_model(
        "root-act", [ParameterName("yield"), ParameterName("mix_de", process="p")], collection="methods", exclude_long_term=True
    )
    assert session.post.call_args[0][0] == "http://test.local/api/v1/db/testdb/activity/root-act/reduced-model/methods"
    assert dict(session.post.call_args[1]["params"])["exclude-long-term"] == "true"
    assert session.post.call_args[1]["json"] == {"parameters": [{"name": "yield"}, {"name": "mix_de", "process": "p"}]}
    assert model.process_id == "root-act" and model.exclude_long_term


def test_reduced_model_needs_revision_69(mocked_client):
    from volca import VoLCAError

    client, session = mocked_client
    client._checked = True
    client._server_wire = 68
    with pytest.raises(VoLCAError, match="wire revision >= 69"):
        client.reduced_model("root-act", [ParameterName("yield")], collection="methods")
    session.post.assert_not_called()
