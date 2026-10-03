"""Offline request-shaping tests for copying and changing a method collection.

They mock ``Client._session`` and assert on the wire shape (URL, JSON body,
query parameters) and on the wire gate, so they never touch a live engine.
"""

from __future__ import annotations

import pytest

from volca.client import VoLCAError
from volca.types import FactorMatch, LCIABatchResult, NewMethodFactor, ScoringRow
from tests.conftest import _make_response

BASE = "http://test.local/api/v1/method-collections"


def _engine(session, wire: int = 42) -> None:
    """Answer get_version with ``wire``, and every POST with an empty change."""
    session.get.return_value = _make_response(
        {"version": "0.16.0", "gitHash": "abc", "gitTag": "v0.16.0", "buildTarget": "x", "wireVersion": wire}
    )
    session.post.return_value = _make_response({"line": 1, "touched": 1, "before": 1.0, "after": 27.0})


def _posted(session) -> tuple[str, dict | None, dict | None]:
    args, kwargs = session.post.call_args
    return args[0], kwargs.get("json"), kwargs.get("params")


def test_copy(mocked_client):
    client, session = mocked_client
    _engine(session)
    session.post.return_value = _make_response({"name": "my-copy", "source": "plain-indicators"})
    assert client.copy_method_collection("plain-indicators", "my copy")["name"] == "my-copy"
    url, body, _ = _posted(session)
    assert url == f"{BASE}/plain-indicators/copy/my%20copy"
    assert body is None


def test_set_sends_only_what_it_names(mocked_client):
    client, session = mocked_client
    _engine(session)
    assert client.set_method_factor("copy", "m", "f", 27.0) == {"line": 1, "touched": 1, "before": 1.0, "after": 27.0}
    url, body, _ = _posted(session)
    assert url == f"{BASE}/copy/factors"
    assert body == {"op": "set", "methodId": "m", "flowId": "f", "newValue": 27.0}


def test_set_with_place_and_value(mocked_client):
    client, session = mocked_client
    _engine(session)
    client.set_method_factor("copy", "m", "f", 2.0, location="FR", value=1.0)
    _, body, _ = _posted(session)
    assert body == {"op": "set", "methodId": "m", "flowId": "f", "newValue": 2.0, "location": "FR", "value": 1.0}


def test_remove(mocked_client):
    client, session = mocked_client
    _engine(session)
    client.remove_method_factor("copy", "m", "f")
    _, body, _ = _posted(session)
    assert body == {"op": "remove", "methodId": "m", "flowId": "f"}


def test_add(mocked_client):
    client, session = mocked_client
    _engine(session)
    factor = NewMethodFactor(flow_id="f", name="Ammonia", direction="Output", value=3.0, unit="kg", compartment=("air", "", ""))
    client.add_method_factor("copy", "m", factor)
    _, body, _ = _posted(session)
    assert body == {
        "op": "add",
        "methodId": "m",
        "factor": {
            "flowId": "f",
            "name": "Ammonia",
            "direction": "Output",
            "value": 3.0,
            "unit": "kg",
            "compartment": {"medium": "air", "subcompartment": "", "qualifier": ""},
        },
    }


def test_scale_and_set_all(mocked_client):
    client, session = mocked_client
    _engine(session)
    client.scale_method_factors("copy", FactorMatch(flow_name_prefix="Methane"), 2.0)
    _, body, _ = _posted(session)
    assert body == {"op": "scale", "match": {"flowNamePrefix": "Methane"}, "scale": 2.0}
    client.set_method_factors("copy", FactorMatch(category="Methane"), 0.0)
    _, body, _ = _posted(session)
    assert body == {"op": "set-all", "match": {"category": "Methane"}, "newValue": 0.0}


def test_an_empty_selector_is_refused_before_sending():
    with pytest.raises(ValueError, match="at least one"):
        FactorMatch()


def test_undo(mocked_client):
    client, session = mocked_client
    _engine(session)
    client.undo_method_edit("copy", line=3)
    url, _, params = _posted(session)
    assert url == f"{BASE}/copy/undo"
    assert params == {"line": 3}


def test_history_and_flows(mocked_client):
    client, session = mocked_client
    _engine(session)
    client.method_history("copy")
    assert session.get.call_args[0][0] == f"{BASE}/copy/history"
    client.search_method_flows("copy", "meth", limit=5)
    assert session.get.call_args[0][0] == f"{BASE}/copy/flows"
    assert session.get.call_args[1]["params"] == {"q": "meth", "limit": 5}


def test_an_older_engine_is_refused(mocked_client):
    client, session = mocked_client
    _engine(session, wire=41)
    with pytest.raises(VoLCAError, match="wire revision >= 42"):
        client.set_method_factor("copy", "m", "f", 27.0)
    session.post.assert_not_called()


@pytest.mark.parametrize(
    ("call", "expected"),
    [
        (lambda c: c.add_method_category("copy", "Water use", "m3"), {"op": "add", "name": "Water use", "unit": "m3"}),
        (
            lambda c: c.add_method_category("copy", "Water use", "m3", impact_category="Water", methodology="AWARE"),
            {"op": "add", "name": "Water use", "unit": "m3", "impactCategory": "Water", "methodology": "AWARE"},
        ),
        (lambda c: c.rename_method_category("copy", "m", "x"), {"op": "rename", "methodId": "m", "name": "x"}),
        (lambda c: c.set_method_category_unit("copy", "m", "x"), {"op": "set-unit", "methodId": "m", "unit": "x"}),
        (lambda c: c.remove_method_category("copy", "m"), {"op": "remove", "methodId": "m"}),
    ],
)
def test_category_changes(mocked_client, call, expected):
    client, session = mocked_client
    _engine(session, wire=43)
    call(client)
    url, body, _ = _posted(session)
    assert url == f"{BASE}/copy/categories"
    assert body == expected


def test_category_changes_need_wire_43(mocked_client):
    client, session = mocked_client
    _engine(session, wire=42)
    with pytest.raises(VoLCAError, match="wire revision >= 43"):
        client.add_method_category("copy", "Water use", "m3")


_GAS = ScoringRow("Gas", {"m": 1.0}, normalization=2.0, weight=0.5)
_GAS_WIRE = {"label": "Gas", "terms": [{"methodId": "m", "coefficient": 1.0}], "normalization": 2.0, "weight": 0.5}


@pytest.mark.parametrize(
    ("call", "expected"),
    [
        (lambda c: c.create_scoring_set("copy", "Mine", [_GAS]), {"op": "create", "set": "Mine", "rows": [_GAS_WIRE]}),
        (lambda c: c.create_scoring_set("copy", "Mine", [], unit="mPt"), {"op": "create", "set": "Mine", "unit": "mPt", "rows": []}),
        (lambda c: c.remove_scoring_set("copy", "Mine"), {"op": "remove", "set": "Mine"}),
        (lambda c: c.rename_scoring_set("copy", "Mine", "Ours"), {"op": "rename", "set": "Mine", "name": "Ours"}),
        (lambda c: c.set_scoring_set_unit("copy", "Mine", "mPt"), {"op": "set-unit", "set": "Mine", "unit": "mPt"}),
        (lambda c: c.set_scoring_set_multiplier("copy", "Mine", 1000.0), {"op": "set-multiplier", "set": "Mine", "multiplier": 1000.0}),
        (lambda c: c.set_scoring_set_multiplier("copy", "Mine", None), {"op": "set-multiplier", "set": "Mine"}),
        (lambda c: c.add_scoring_row("copy", "Mine", _GAS), {"op": "add-row", "set": "Mine", "row": _GAS_WIRE}),
        (lambda c: c.change_scoring_row("copy", "Mine", "gas", _GAS), {"op": "change-row", "set": "Mine", "variable": "gas", "row": _GAS_WIRE}),
        (lambda c: c.remove_scoring_row("copy", "Mine", "gas"), {"op": "remove-row", "set": "Mine", "variable": "gas"}),
        (lambda c: c.set_scoring_formula("copy", "Mine", "gas", "2 * methane"), {"op": "set-formula", "set": "Mine", "variable": "gas", "formula": "2 * methane"}),
        (lambda c: c.set_score("copy", "Mine", "Twice", "2 * gas"), {"op": "set-score", "set": "Mine", "score": "Twice", "formula": "2 * gas"}),
        (lambda c: c.remove_score("copy", "Mine", "Twice"), {"op": "remove-score", "set": "Mine", "score": "Twice"}),
    ],
)
def test_scoring_set_changes(mocked_client, call, expected):
    client, session = mocked_client
    _engine(session, wire=45)
    call(client)
    url, body, _ = _posted(session)
    assert url == f"{BASE}/copy/scoring-sets"
    assert body == expected


def test_scoring_set_changes_need_wire_45(mocked_client):
    client, session = mocked_client
    _engine(session, wire=44)
    with pytest.raises(VoLCAError, match="wire revision >= 45"):
        client.add_scoring_row("copy", "Mine", _GAS)
    session.post.assert_not_called()


def test_a_row_groups_a_category():
    with pytest.raises(ValueError, match="groups no category"):
        ScoringRow("Empty", {})


def test_a_score_reads_the_value_of_every_row():
    result = LCIABatchResult.from_json({"results": [], "scoringRows": {"Mine": {"gas": {"category": "Gas", "value": 3.0}}}})
    assert result.scoring_rows["Mine"]["gas"].value == 3.0
