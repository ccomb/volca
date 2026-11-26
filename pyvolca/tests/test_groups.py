"""Offline tests for a score split between groups of processes.

The four group routes go through the OpenAPI dispatcher (``_call``), so they
rely on the fixture_spec entries for them. The tests assert the dispatched
URL and body, the typed parsing, a group read back from the engine sent
unchanged, and the wire gate.
"""

from __future__ import annotations

import pytest

from volca import VoLCAError
from volca.types import Attribution, ClassificationFilter, ContributingFlows, FlowCriteria, Group, GroupCandidates, GroupContributions, MatchMode, SubstanceMatch

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


def test_a_group_reads_a_mode_as_the_engine_does():
    group = Group.from_json({"name": "Fuel", "filter": {"classifications": [{"system": "CPC", "value": "33", "mode": "Equals"}, {"system": "CPC", "value": "333", "mode": "Not-Exact"}]}})
    assert group.classifications == (ClassificationFilter("CPC", "33", MatchMode.EXACT),)
    assert group.left_out == (ClassificationFilter("CPC", "333", MatchMode.EXACT),)


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


# A group keeping a substance as the engine hands it back in a candidate: every absent field written null.
_N2O_ECHO = {
    "name": "N2O",
    "filter": {"name": None, "product": None, "location": None, "classifications": None, "flow": {"name": "N2O", "match": "substance", "medium": None}},
}


def test_a_group_keeping_a_substance_reads_back_from_the_engine():
    group = Group.from_json(_N2O_ECHO)
    assert group == Group("N2O", flow=FlowCriteria("N2O"))
    assert group.to_wire() == {"name": "N2O", "filter": {"flow": {"name": "N2O", "match": "substance"}}}
    fields = {"name": "N2O of the fields", "filter": {"name": "at farm", "flow": {"name": "nitrous", "match": "contains", "medium": "air"}}}
    assert Group.from_json(fields).flow == FlowCriteria("nitrous", SubstanceMatch.CONTAINS, "air")
    assert Group.from_json(fields).to_wire() == fields
    assert Group("Water", flow=FlowCriteria(medium="water")).to_wire() == {"name": "Water", "filter": {"flow": {"medium": "water"}}}


@pytest.mark.parametrize(
    ("args", "message"),
    [
        ({}, "needs a substance or a medium"),
        ({"substance": "  "}, "cannot be blank"),
        ({"medium": ""}, "cannot be blank"),
        ({"medium": "air", "match": "contains"}, "give the substance"),
        ({"substance": "N2O", "match": "equals"}, "match must be one of"),
    ],
)
def test_a_flow_criterion_the_engine_would_refuse_is_refused_before_sending(args, message):
    with pytest.raises(ValueError, match=message):
        FlowCriteria(**args)


def test_group_flows_names_the_group_and_reads_every_line(mocked_client, make_response):
    client, session = _client(mocked_client, 122)
    session.post.return_value = make_response(
        {
            "method": "Climate change",
            "unit": "kg CO2 eq",
            "totalScore": 0.4,
            "functionalUnit": _FUNCTIONAL_UNIT,
            "topFlows": [{"flowName": "Dinitrogen monoxide", "contribution": 0.3, "sharePct": 75.0, "flowId": "f-n2o", "category": "air"}],
            "withheldDatabases": [{"database": "dep", "contribution": 0.1, "sharePct": 25.0, "reason": "withheld"}],
            "cutoffInputs": [],
            "withheldCutoffs": [],
            "basis": None,
        }
    )
    n2o = Group.from_json(_N2O_ECHO)
    flows = client.group_flows("proc_a", _METHOD_ID, [n2o], group="N2O", limit=5, collection="methods")
    assert session.post.call_args[0][0] == f"http://test.local/api/v1/db/testdb/activity/proc_a/group-flows/methods/{_METHOD_ID}"
    assert session.post.call_args[1]["json"] == {"groups": [n2o.to_wire()], "group": "N2O", "limit": 5}
    assert isinstance(flows, ContributingFlows)
    assert flows.total_score == pytest.approx(flows.top_flows[0].contribution + flows.withheld_databases[0].contribution)


def test_group_candidates_from_a_flow(mocked_client, make_response):
    client, session = _client(mocked_client, 122)
    session.post.return_value = make_response(
        {"method": "Climate change", "unit": "kg CO2 eq", "total": 1.0, "candidates": [{"group": _N2O_ECHO, "activitiesCounted": 2, "score": 0.3, "examples": []}]}
    )
    offered = client.group_candidates("proc_a", _METHOD_ID, [], candidate_flow="f-n2o", collection="methods")
    assert session.post.call_args[1]["json"] == {"groups": [], "candidateFlow": "f-n2o"}
    assert offered.candidates[0].group == Group("N2O", flow=FlowCriteria("N2O"))


def test_group_candidates_takes_a_process_or_a_flow_exactly(mocked_client):
    client, session = _client(mocked_client, 122)
    with pytest.raises(ValueError, match="exactly one"):
        client.group_candidates("proc_a", _METHOD_ID, [], collection="methods")
    with pytest.raises(ValueError, match="exactly one"):
        client.group_candidates("proc_a", _METHOD_ID, [], "dep::lorry", candidate_flow="f-n2o", collection="methods")
    session.post.assert_not_called()


def test_an_engine_before_the_groups_of_substances_is_never_asked_of_them(mocked_client, make_response):
    client, session = _client(mocked_client, 121)
    n2o = Group("N2O", flow=FlowCriteria("N2O"))
    with pytest.raises(VoLCAError, match="with a group keeping a substance needs engine wire revision >= 122"):
        client.group_contributions("proc_a", [n2o], collection="methods")
    with pytest.raises(VoLCAError, match="wire revision >= 122"):
        client.group_flows("proc_a", _METHOD_ID, [], collection="methods")
    with pytest.raises(VoLCAError, match="wire revision >= 122"):
        client.group_candidates("proc_a", _METHOD_ID, [], candidate_flow="f-n2o", collection="methods")
    session.post.assert_not_called()
    session.post.return_value = make_response(
        {"method": "Climate change", "unit": "kg CO2 eq", "totalScore": 1.0, "functionalUnit": _FUNCTIONAL_UNIT, "activities": []}
    )
    client.group_members("proc_a", _METHOD_ID, [Group.from_json(_TRANSPORT)], collection="methods")
    session.post.assert_called_once()


def test_an_engine_before_the_groups_is_never_asked(mocked_client):
    client, session = _client(mocked_client, 119)
    with pytest.raises(VoLCAError, match="wire revision >= 120"):
        client.group_sets()
    client._server_wire = 118
    with pytest.raises(VoLCAError, match="wire revision >= 119"):
        client.group_contributions("proc_a", [], collection="methods")
    session.post.assert_not_called()
    session.get.assert_not_called()
