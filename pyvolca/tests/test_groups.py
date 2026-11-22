"""Offline tests for a score split between groups of processes.

The four group routes go through the OpenAPI dispatcher (``_call``), so they
rely on the fixture_spec entries for them. The tests assert the dispatched
URL and body, the typed parsing, a group read back from the engine sent
unchanged, and the wire gate.
"""

from __future__ import annotations

import pytest

from volca import VoLCAError
from volca.types import Attribution, ClassificationFilter, Group, GroupCandidates, GroupContributions, MatchMode

_METHOD_ID = "00000000-0000-0000-0000-000000000001"
_FUNCTIONAL_UNIT = {"amount": 1, "unit": "kg", "product": "wheat"}
_TRANSPORT = {"name": "Transport", "filter": {"classifications": [{"system": "CPC", "value": "65", "mode": "starts-with"}]}}


def _client(mocked_client, wire: int):
    client, session = mocked_client
    client._checked = True
    client._server_wire = wire
    return client, session


def test_a_group_reads_back_from_the_engine_as_it_is_sent():
    wire = {
        "name": "Not road",
        "filter": {
            "product": "lorry",
            "classifications": [
                {"system": "CPC", "value": "65", "mode": "starts-with"},
                {"system": "CPC", "value": "6511", "mode": "not-starts-with"},
            ],
        },
    }
    group = Group.from_json(wire)
    assert group.classifications == (ClassificationFilter("CPC", "65", MatchMode.STARTS_WITH),)
    assert group.left_out == (ClassificationFilter("CPC", "6511", MatchMode.STARTS_WITH),)
    assert group.to_wire() == wire


def test_group_contributions_sends_the_groups_and_reads_every_line(mocked_client, make_response):
    client, session = _client(mocked_client, 119)
    session.post.return_value = make_response(
        {
            "attribution": "own",
            "methods": [{"id": _METHOD_ID, "name": "Climate change", "unit": "kg CO2 eq"}],
            "total": {_METHOD_ID: 1.0},
            "groups": [{"name": "Transport", "scores": {_METHOD_ID: 0.25}, "activitiesCounted": 3}],
            "rest": {_METHOD_ID: 0.7},
            "withheldDatabases": [{"database": "dep", "scores": {_METHOD_ID: 0.05}}],
            "functionalUnit": _FUNCTIONAL_UNIT,
        }
    )
    result = client.group_contributions("proc_a", [Group.from_json(_TRANSPORT)], attribution="own", collection="methods", exclude_long_term=True)
    assert session.post.call_args[0][0] == "http://test.local/api/v1/db/testdb/activity/proc_a/groups/methods"
    assert dict(session.post.call_args[1]["params"])["exclude-long-term"] == "true"
    assert session.post.call_args[1]["json"] == {"groups": [_TRANSPORT], "attribution": "own"}
    assert isinstance(result, GroupContributions) and result.attribution is Attribution.OWN
    parts = [g.scores[_METHOD_ID] for g in result.groups] + [result.rest[_METHOD_ID]] + [w.scores[_METHOD_ID] for w in result.withheld_databases]
    assert sum(parts) == pytest.approx(result.total[_METHOD_ID])


def test_group_members_names_the_group_and_the_limit(mocked_client, make_response):
    client, session = _client(mocked_client, 119)
    session.post.return_value = make_response(
        {"method": "Climate change", "unit": "kg CO2 eq", "totalScore": 1.0, "functionalUnit": _FUNCTIONAL_UNIT, "activities": []}
    )
    client.group_members("proc_a", _METHOD_ID, [Group.from_json(_TRANSPORT)], group="Transport", limit=5, collection="methods")
    assert session.post.call_args[0][0] == f"http://test.local/api/v1/db/testdb/activity/proc_a/group-members/methods/{_METHOD_ID}"
    assert session.post.call_args[1]["json"] == {"groups": [_TRANSPORT], "group": "Transport", "limit": 5}


def test_group_candidates_and_sets_hand_back_groups_ready_to_send(mocked_client, make_response):
    client, session = _client(mocked_client, 120)
    session.post.return_value = make_response(
        {
            "method": "Climate change",
            "unit": "kg CO2 eq",
            "total": 1.0,
            "candidates": [{"group": _TRANSPORT, "activitiesCounted": 4, "score": 0.3, "examples": ["transport, freight, lorry"]}],
        }
    )
    session.get.return_value = make_response([{"name": "CPC", "system": "CPC", "groups": [_TRANSPORT]}])
    candidates = client.group_candidates("proc_a", _METHOD_ID, [], "dep::lorry", collection="methods")
    assert session.post.call_args[0][0] == f"http://test.local/api/v1/db/testdb/activity/proc_a/group-candidates/methods/{_METHOD_ID}"
    assert session.post.call_args[1]["json"] == {"groups": [], "candidate": "dep::lorry"}
    assert isinstance(candidates, GroupCandidates)
    [ready] = client.group_sets()
    assert session.get.call_args[0][0] == "http://test.local/api/v1/db/testdb/group-sets"
    assert ready.groups == [candidates.candidates[0].group]


def test_an_engine_before_the_groups_is_never_asked(mocked_client):
    client, session = _client(mocked_client, 119)
    with pytest.raises(VoLCAError, match="wire revision >= 120"):
        client.group_sets()
    client._server_wire = 118
    with pytest.raises(VoLCAError, match="wire revision >= 119"):
        client.group_contributions("proc_a", [], collection="methods")
    session.post.assert_not_called()
    session.get.assert_not_called()
