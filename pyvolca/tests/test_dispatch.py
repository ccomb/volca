"""Offline tests for the runtime dispatcher in :mod:`volca.client`.

These tests mock out ``requests.Session`` and verify that each wrapper
builds the correct URL, HTTP method, and query parameters from a
hand-crafted OpenAPI fixture. No real engine required.
"""

from __future__ import annotations

import pytest

from volca.client import Client, VoLCAError, _candidate_wire_names, _Operation, _parse_spec, _resolve_wire_name


# ---------------------------------------------------------------------------
# Pure helpers — no mocking required
# ---------------------------------------------------------------------------


class TestNameTranslation:
    def test_snake_case_already_matches_self(self):
        assert _candidate_wire_names("name") == ["name"]

    def test_snake_to_camel(self):
        cands = _candidate_wire_names("process_id")
        assert "processId" in cands

    def test_snake_to_kebab(self):
        cands = _candidate_wire_names("min_quantity")
        assert "min-quantity" in cands

    def test_three_candidates_in_priority_order(self):
        cands = _candidate_wire_names("db_name")
        # snake first, then camel, then kebab
        assert cands == ["db_name", "dbName", "db-name"]

    def test_single_word_collapses_to_one_candidate(self):
        assert _candidate_wire_names("limit") == ["limit"]


class TestResolveWireName:
    def test_matches_path_capture_via_camelcase(self, fixture_spec):
        ops = _parse_spec(fixture_spec)
        op = ops["get_activity"]
        assert _resolve_wire_name("process_id", op) == "processId"

    def test_matches_query_via_kebab(self, fixture_spec):
        ops = _parse_spec(fixture_spec)
        op = ops["get_supply_chain"]
        assert _resolve_wire_name("min_quantity", op) == "min-quantity"

    def test_aggregate_uses_snake_case_query_params(self, fixture_spec):
        """Aggregate's query params are snake_case in the spec, so the
        snake kwarg matches directly without camel or kebab translation."""
        ops = _parse_spec(fixture_spec)
        op = ops["aggregate"]
        assert _resolve_wire_name("is_input", op) == "is_input"
        assert _resolve_wire_name("max_depth", op) == "max_depth"

    def test_unknown_kwarg_returns_none(self, fixture_spec):
        ops = _parse_spec(fixture_spec)
        op = ops["get_activity"]
        assert _resolve_wire_name("does_not_exist", op) is None


# ---------------------------------------------------------------------------
# Dispatcher integration — uses mocked session
# ---------------------------------------------------------------------------


class TestDispatcher:
    def test_list_databases_has_no_path_params(self, mocked_client, make_response):
        client, session = mocked_client
        session.get.return_value = make_response({
            "databases": [{
                "name": "test",
                "displayName": "Test DB",
                "status": "loaded",
                "path": "data/test",
                "loadAtStartup": True,
                "isUploaded": False,
                "activityCount": 42,
                "description": None,
                "format": "EcoSpold 2",
                "dependsOn": ["ecoinvent-3.9"],
            }],
        })
        result = client.list_databases()
        assert len(result) == 1
        assert result[0].name == "test"
        assert result[0].display_name == "Test DB"
        assert result[0].depends_on == ["ecoinvent-3.9"]
        assert result[0].activity_count == 42
        session.get.assert_called_once()
        called_url = session.get.call_args[0][0]
        assert called_url == "http://test.local/api/v1/db"

    def test_get_activity_substitutes_path_captures(self, mocked_client, make_response):
        client, session = mocked_client
        session.get.return_value = make_response({
            "activity": {"exchanges": [], "name": "test"},
        })
        try:
            client.get_activity("abc_def")
        except Exception:
            pass  # ActivityDetail.from_json may fail on minimal payload
        called_url = session.get.call_args[0][0]
        assert called_url == "http://test.local/api/v1/db/testdb/activity/abc_def"

    def test_search_activities_sends_query_params(self, mocked_client, make_response):
        client, session = mocked_client
        session.get.return_value = make_response({"srResults": []})
        client.search_activities(name="wheat", geo="FR", limit=5)
        called = session.get.call_args
        assert called[0][0] == "http://test.local/api/v1/db/testdb/activities"
        params = dict(called[1]["params"])
        # The wrapper forwards exact=False and offset=0 as wrapper defaults.
        assert params["name"] == "wheat"
        assert params["geo"] == "FR"
        assert params["limit"] == "5"
        assert params["offset"] == "0"
        assert params["exact"] == "false"

    def test_search_activities_drops_none_kwargs(self, mocked_client, make_response):
        client, session = mocked_client
        session.get.return_value = make_response({"srResults": []})
        client.search_activities(name="wheat")  # geo, limit, etc. left at None
        params = dict(session.get.call_args[1]["params"])
        assert "name" in params
        assert "geo" not in params
        assert "limit" not in params

    def test_kebab_case_query_param_translation(self, mocked_client, make_response):
        """``min_quantity`` Python → ``min-quantity`` wire."""
        client, session = mocked_client
        session.get.return_value = make_response({
            "root": {"processId": "x", "name": "y", "location": "FR", "product": "p", "productAmount": 1.0, "productUnit": "kg"},
            "supplyChain": [],
            "edges": [],
            "totalActivities": 0,
            "filteredActivities": 0,
        })
        try:
            client.get_supply_chain("abc_def", min_quantity=0.5, max_depth=3)
        except Exception:
            pass  # SupplyChain.from_json may fail on partial fixture
        params = dict(session.get.call_args[1]["params"])
        assert params.get("min-quantity") == "0.5"
        assert params.get("max-depth") == "3"

    def test_supply_chain_forwards_preset(self, mocked_client, make_response):
        """preset= must reach the supply-chain endpoint as a query param."""
        client, session = mocked_client
        session.get.return_value = make_response({
            "root": {"processId": "x", "name": "y", "location": "FR", "product": "p", "productAmount": 1.0, "productUnit": "kg"},
            "supplyChain": [],
            "edges": [],
            "totalActivities": 0,
            "filteredActivities": 0,
        })
        try:
            client.get_supply_chain("abc_def", preset="raw-ingredients")
        except Exception:
            pass
        params = dict(session.get.call_args[1]["params"])
        assert params.get("preset") == "raw-ingredients"

    def test_aggregate_forwards_preset(self, mocked_client, make_response):
        """preset= must reach the aggregate endpoint as a query param."""
        client, session = mocked_client
        session.get.return_value = make_response({})
        client.call(
            "aggregate",
            process_id="abc",
            scope="supply_chain",
            preset="raw-ingredients",
        )
        params = dict(session.get.call_args[1]["params"])
        assert params.get("preset") == "raw-ingredients"

    def test_get_impacts_uses_all_four_path_captures(self, mocked_client, make_response):
        client, session = mocked_client
        session.get.return_value = make_response({"score": 1.0})
        client.get_impacts("abc_def", method_id="method-uuid", collection="EF3.1")
        called_url = session.get.call_args[0][0]
        assert called_url == "http://test.local/api/v1/db/testdb/activity/abc_def/impacts/EF3.1/method-uuid"

    def test_substitutions_upgrade_to_post(self, mocked_client, make_response):
        client, session = mocked_client
        session.post.return_value = make_response({"score": 2.0})
        client.get_impacts(
            "abc_def",
            method_id="method-uuid",
            substitutions=[{"from": "a", "to": "b", "consumer": "c"}],
        )
        session.post.assert_called_once()
        session.get.assert_not_called()
        body = session.post.call_args[1]["json"]
        assert body == {
            "srSubstitutions": [{"subFrom": "a", "subTo": "b", "subConsumer": "c"}]
        }

    def test_unknown_operation_raises(self, mocked_client):
        client, _ = mocked_client
        with pytest.raises(VoLCAError, match="Unknown operationId"):
            client.call("does_not_exist")

    def test_unknown_kwarg_raises(self, mocked_client, make_response):
        client, session = mocked_client
        session.get.return_value = make_response({"dlrDatabases": []})
        with pytest.raises(VoLCAError, match="unknown kwargs"):
            client.call("list_databases", nonsense_param="foo")

    def test_auto_injects_db_name_from_instance(self, mocked_client, make_response):
        """Client(db='testdb') should inject db_name without the caller passing it."""
        client, session = mocked_client
        session.get.return_value = make_response({"srResults": []})
        client.search_activities(name="x")
        called_url = session.get.call_args[0][0]
        assert "/db/testdb/" in called_url

    def test_db_name_override(self, mocked_client, make_response):
        client, session = mocked_client
        session.get.return_value = make_response({"srResults": []})
        client.call("search_activities", db_name="other", name="x")
        called_url = session.get.call_args[0][0]
        assert "/db/other/" in called_url

    def test_missing_db_raises_clearly(self, fixture_spec, make_response):
        """Client(db='') with an operation that needs dbName should raise."""
        client = Client(base_url="http://test.local", db="")
        client._operations = _parse_spec(fixture_spec)
        with pytest.raises(VoLCAError, match="requires a database"):
            client.call("search_activities", name="x")

    def test_bool_query_param_formats_as_true_false(self, mocked_client, make_response):
        client, session = mocked_client
        session.get.return_value = make_response({})
        client.call(
            "aggregate",
            process_id="abc",
            scope="direct",
            is_input=True,
            max_depth=2,
        )
        params = dict(session.get.call_args[1]["params"])
        assert params.get("is_input") == "true"
        assert params.get("max_depth") == "2"
