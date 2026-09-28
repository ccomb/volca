"""Offline tests for flow/method detail, mapping status, and stats lookups."""

from __future__ import annotations

import pytest

from volca.client import VoLCAError
from volca.types import CollectionCoverage, FlowDetail, MappingStatus, MethodDetail, MethodFactor

# A method id travels the URL as a UUID; the client resolves anything else
# against the engine's loaded methods first.
_M1 = "00000000-0000-0000-0000-000000000001"


def _resp(session_attr, json_body: dict) -> None:
    from tests.conftest import _make_response

    session_attr.return_value = _make_response(json_body)


class TestFlowDetail:
    def test_keeps_flow_union_raw(self, mocked_client):
        client, session = mocked_client
        _resp(session.get, {"flow": {"kind": "biosphere", "name": "CO2"}, "unitName": "kg", "usageCount": 12})
        out = client.get_flow("flow-uuid")
        assert session.get.call_args[0][0] == "http://test.local/api/v1/db/testdb/flow/flow-uuid"
        assert isinstance(out, FlowDetail)
        assert out.flow == {"kind": "biosphere", "name": "CO2"}
        assert out.unit_name == "kg" and out.usage_count == 12

    def test_flow_activities_parses_rows(self, mocked_client):
        client, session = mocked_client
        _resp(session.get, [
            {"processId": "p_r", "activityName": "Wheat", "location": "FR", "productName": "wheat", "productAmount": 1.0, "productUnit": "kg"},
        ])
        out = client.get_flow_activities("flow-uuid")
        assert session.get.call_args[0][0] == "http://test.local/api/v1/db/testdb/flow/flow-uuid/activities"
        assert out[0].activity_name == "Wheat"


class TestMethodDetail:
    def test_method_detail_optional_fields(self, mocked_client):
        client, session = mocked_client
        _resp(session.get, {"id": "m1", "name": "Climate change", "unit": "kg CO2 eq", "category": "Climate", "factorCount": 200})
        out = client.get_method(_M1)
        assert session.get.call_args[0][0] == f"http://test.local/api/v1/method/{_M1}"
        assert isinstance(out, MethodDetail)
        assert out.factor_count == 200
        assert out.description is None and out.methodology is None

    def test_method_factors_parsed(self, mocked_client):
        client, session = mocked_client
        _resp(session.get, [{"flowRef": "f1", "flowName": "CO2", "direction": "Output", "value": 1.0}])
        out = client.get_method_factors(_M1)
        assert session.get.call_args[0][0] == f"http://test.local/api/v1/method/{_M1}/factors"
        assert isinstance(out[0], MethodFactor)
        assert out[0].flow_ref == "f1" and out[0].value == 1.0


class TestMappingStatus:
    def test_acronym_fields_parse(self, mocked_client):
        client, session = mocked_client
        _resp(session.get, {
            "methodId": "m1",
            "methodName": "Climate change",
            "totalFactors": 100,
            "mappedByUUID": 40,
            "mappedByCAS": 20,
            "mappedByName": 10,
            "mappedBySynonym": 5,
            "unmapped": 25,
            "coverage": 75.0,
            "dbBiosphereCount": 300,
            "uniqueDbFlowsMatched": 75,
            "unmappedFlows": [{"flowRef": "f9", "flowName": "Unobtanium", "direction": "Input"}],
        })
        out = client.get_mapping_status(_M1)
        assert session.get.call_args[0][0] == f"http://test.local/api/v1/db/testdb/method/{_M1}/mapping"
        assert isinstance(out, MappingStatus)
        assert out.mapped_by_uuid == 40
        assert out.mapped_by_cas == 20
        assert out.db_biosphere_count == 300
        assert out.unique_db_flows_matched == 75
        assert out.unmapped_flows[0].flow_name == "Unobtanium"


class TestCollectionCoverage:
    def test_collection_name_is_url_encoded(self, mocked_client):
        client, session = mocked_client
        _resp(session.get, {
            "collection": "EF 3.1",
            "dbName": "testdb",
            "totalFlows": 300,
            "characterizedFlows": 210,
        })
        out = client.get_collection_coverage("EF 3.1")
        assert session.get.call_args[0][0] == (
            "http://test.local/api/v1/db/testdb/method-collection/EF%203.1/coverage"
        )
        assert isinstance(out, CollectionCoverage)
        assert out.characterized_flows == 210
        assert out.total_flows == 300


class TestStats:
    def test_stats_returns_raw_dict(self, mocked_client):
        client, session = mocked_client
        _resp(session.get, {"memory_used_bytes": 12345, "databases_loaded": 2})
        out = client.get_stats()
        assert session.get.call_args[0][0] == "http://test.local/api/v1/stats"
        assert out["memory_used_bytes"] == 12345


class TestMethodCollection:
    """A method UUID several collections carry is chosen with ``collection``."""

    @staticmethod
    def _at_wire(client, wire: int) -> None:
        client._checked = True
        client._server_wire = wire

    def test_collection_travels_as_a_query_parameter(self, mocked_client):
        client, session = mocked_client
        self._at_wire(client, 29)
        _resp(session.get, [])
        client.get_method_factors(_M1, collection="EF 3.1")
        assert session.get.call_args[0][0] == f"http://test.local/api/v1/method/{_M1}/factors"
        assert session.get.call_args[1]["params"] == {"collection": "EF 3.1"}

    def test_no_collection_sends_none(self, mocked_client):
        client, session = mocked_client
        _resp(session.get, [])
        client.get_method_factors(_M1)
        assert session.get.call_args[1]["params"] == {"collection": None}

    def test_an_engine_that_would_ignore_it_is_refused_before_any_request(self, mocked_client):
        client, session = mocked_client
        self._at_wire(client, 28)
        with pytest.raises(VoLCAError, match="collection"):
            client.get_method("Climate change", collection="EF 3.1")
        session.get.assert_not_called()
