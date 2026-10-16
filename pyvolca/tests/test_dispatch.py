"""Offline tests for the runtime dispatcher in :mod:`volca.client`.

These tests mock out ``requests.Session`` and verify that each wrapper
builds the correct URL, HTTP method, and query parameters from a
hand-crafted OpenAPI fixture. No real engine required.
"""

from __future__ import annotations

import pytest

from volca.client import Client, VoLCAError, _candidate_wire_names, _Operation, _parse_spec, _resolve_wire_name
from volca.types import Method, ParameterValue

_WATER_USE_ID = "00000000-0000-0000-0000-000000000002"


def _method(name: str, collection: str, id: str = _WATER_USE_ID) -> Method:
    """One loaded method, as :meth:`Client.list_methods` would return it."""
    return Method(
        id=id, name=name, category=name, unit="m3", factor_count=1, collection=collection
    )


def _minimal_lcia_result() -> dict:
    """Minimal wire payload accepted by :meth:`LCIAResult.from_json`."""
    return {
        "methodId": "00000000-0000-0000-0000-000000000001",
        "methodName": "Climate change",
        "category": "Climate change",
        "damageCategory": "Climate change",
        "score": 1.23,
        "unit": "kg CO2 eq",
        "mappedFlows": 0,
        "functionalUnit": "1.0 kg",
        "topContributors": [],
    }


# ---------------------------------------------------------------------------
# Pure helpers: no mocking required
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
# Dispatcher integration: uses mocked session
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

    def test_search_activities_sends_query_params(self, mocked_client, make_response, empty_envelope):
        client, session = mocked_client
        session.get.return_value = make_response(empty_envelope())
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

    def test_search_activities_drops_none_kwargs(self, mocked_client, make_response, empty_envelope):
        client, session = mocked_client
        session.get.return_value = make_response(empty_envelope())
        client.search_activities(name="wheat")  # geo, limit, etc. left at None
        params = dict(session.get.call_args[1]["params"])
        assert "name" in params
        assert "geo" not in params
        assert "limit" not in params

    def test_search_activities_classification_match_sends_mode(self, mocked_client, make_response, empty_envelope):
        """classification_match reaches the wire as ``classification-mode``."""
        client, session = mocked_client
        session.get.return_value = make_response(empty_envelope())
        client.search_activities(classification="Category", classification_value="Food", classification_match="exact")
        params = dict(session.get.call_args[1]["params"])
        assert params["classification-value"] == "Food"
        assert params["classification-mode"] == "exact"

    def test_search_activities_omits_mode_by_default(self, mocked_client, make_response, empty_envelope):
        """No classification_match → no ``classification-mode`` query param (server default: contains)."""
        client, session = mocked_client
        session.get.return_value = make_response(empty_envelope())
        client.search_activities(classification="Category", classification_value="Food")
        params = dict(session.get.call_args[1]["params"])
        assert "classification-mode" not in params

    def test_search_activities_rejects_bad_match_mode(self, mocked_client):
        """A typo'd match mode fails client-side before any HTTP call."""
        client, _ = mocked_client
        with pytest.raises(ValueError):
            client.search_activities(classification="Category", classification_value="Food", classification_match="exct")

    def test_search_activities_forwards_sort_order(self, mocked_client, make_response, empty_envelope):
        """sort= and order= must reach the wire as query params (same path as get_consumers etc.)."""
        client, session = mocked_client
        session.get.return_value = make_response(empty_envelope())
        client.search_activities(name="wheat", sort="location", order="desc")
        params = dict(session.get.call_args[1]["params"])
        assert params["sort"] == "location"
        assert params["order"] == "desc"

    def test_search_activities_omits_sort_order_by_default(self, mocked_client, make_response, empty_envelope):
        """Unset sort/order must not appear in the query string (server defaults apply)."""
        client, session = mocked_client
        session.get.return_value = make_response(empty_envelope())
        client.search_activities(name="wheat")
        params = dict(session.get.call_args[1]["params"])
        assert "sort" not in params
        assert "order" not in params

    def test_search_activities_page_kwargs_compute_offset(self, mocked_client, make_response, empty_envelope):
        """page=3 + page_size=20 must translate to offset=40, limit=20."""
        client, session = mocked_client
        session.get.return_value = make_response(empty_envelope())
        client.search_activities(name="wheat", page=3, page_size=20)
        params = dict(session.get.call_args[1]["params"])
        assert params["limit"] == "20"
        assert params["offset"] == "40"

    def test_search_activities_returns_iterable_search_results(self, mocked_client, make_response):
        """Iteration auto-fetches subsequent pages."""
        from volca import SearchResults

        client, session = mocked_client
        page1 = {
            "results": [
                {"processId": "a", "activityName": "A", "location": "FR", "productName": "p", "productAmount": 1.0, "productUnit": "kg"},
                {"processId": "b", "activityName": "B", "location": "FR", "productName": "p", "productAmount": 1.0, "productUnit": "kg"},
            ],
            "total": 3,
            "offset": 0,
            "limit": 2,
            "hasMore": True,
            "searchTimeMs": 0.1,
        }
        page2 = {
            "results": [
                {"processId": "c", "activityName": "C", "location": "FR", "productName": "p", "productAmount": 1.0, "productUnit": "kg"},
            ],
            "total": 3,
            "offset": 2,
            "limit": 2,
            "hasMore": False,
            "searchTimeMs": 0.1,
        }
        session.get.side_effect = [make_response(page1), make_response(page2)]
        results = client.search_activities(page_size=2)
        assert isinstance(results, SearchResults)
        assert len(results) == 3
        assert results.page_size == 2
        assert results.has_more is True
        names = [a.activity_name for a in results]
        assert names == ["A", "B", "C"]
        # Page 2 was fetched lazily during iteration.
        assert session.get.call_count == 2

    def test_search_activities_mixing_page_and_offset_raises(self, mocked_client):
        """Mixing page-style and wire-style pagination must raise."""
        client, _ = mocked_client
        with pytest.raises(VoLCAError, match="Mix of page-style"):
            client.search_activities(page=2, offset=10)

    def test_search_activities_page_without_page_size_raises(self, mocked_client):
        """page=N alone is ambiguous: we don't fabricate a page size to derive offset."""
        client, _ = mocked_client
        with pytest.raises(VoLCAError, match="page=N requires an explicit page_size"):
            client.search_activities(page=2)

    def test_kebab_case_query_param_translation(self, mocked_client, make_response):
        """``min_quantity`` Python → ``min-quantity`` wire."""
        client, session = mocked_client
        session.get.return_value = make_response({
            "root": {"processId": "x", "activityName": "y", "location": "FR", "productName": "p", "productAmount": 1.0, "productUnit": "kg"},
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
            "root": {"processId": "x", "activityName": "y", "location": "FR", "productName": "p", "productAmount": 1.0, "productUnit": "kg"},
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
        session.get.return_value = make_response(_minimal_lcia_result())
        result = client.get_impacts("abc_def", method_id=_WATER_USE_ID, collection="EF3.1")
        called_url = session.get.call_args[0][0]
        assert called_url == f"http://test.local/api/v1/db/testdb/activity/abc_def/impacts/EF3.1/{_WATER_USE_ID}"
        assert result.score == 1.23

    def test_substitutions_upgrade_to_post(self, mocked_client, make_response):
        client, session = mocked_client
        session.post.return_value = make_response(_minimal_lcia_result())
        client.get_impacts(
            "abc_def",
            method_id=_WATER_USE_ID,
            collection="EF3.1",
            substitutions=[{"from": "a", "to": "b", "consumer": "c"}],
        )
        session.post.assert_called_once()
        session.get.assert_not_called()
        body = session.post.call_args[1]["json"]
        assert body == {
            "substitutions": [{"from": "a", "to": "b", "consumer": "c"}]
        }

    def test_parameters_upgrade_to_post(self, mocked_client, make_response):
        client, session = mocked_client
        client._checked = True
        client._server_wire = 66
        session.post.return_value = make_response(_minimal_lcia_result())
        client.get_impacts(
            "abc_def",
            method_id=_WATER_USE_ID,
            collection="EF3.1",
            parameters=[ParameterValue("yield", 2.0), ParameterValue("mix_de", 1, process="abc_def")],
        )
        session.get.assert_not_called()
        assert session.post.call_args[1]["json"] == {
            "substitutions": [],
            "parameters": [
                {"name": "yield", "value": 2.0},
                {"name": "mix_de", "value": 1, "process": "abc_def"},
            ],
        }

    def test_parameters_never_reach_an_engine_that_would_ignore_them(self, mocked_client):
        client, session = mocked_client
        client._checked = True
        client._server_wire = 65
        with pytest.raises(VoLCAError, match="wire revision >= 66"):
            client.get_impacts_batch("abc_def", collection="EF3.1", parameters=[ParameterValue("yield", 2.0)])
        session.post.assert_not_called()

    def test_get_impacts_batch_parses_scoring_indicators(self, mocked_client, make_response):
        """Batch endpoint has no operationId; ensure direct-HTTP call hits the
        right URL and the typed result exposes the new per-indicator map.
        """
        client, session = mocked_client
        session.get.return_value = make_response({
            "results": [_minimal_lcia_result()],
            "singleScore": 1.5,
            "singleScoreUnit": "Pt",
            "normWeightSetName": "PEF",
            "availableNWsets": ["PEF", "ECS"],
            "scoringResults": {"PEF": {"score": 1.5}},
            "scoringUnits": {"PEF": "mPt"},
            "scoringIndicators": {
                "ECS": {"climate": {"category": "Climate change", "value": 1.42e-6}},
            },
        })
        result = client.get_impacts_batch("abc_def", collection="EF3.1")
        called_url = session.get.call_args[0][0]
        assert called_url == "http://test.local/api/v1/db/testdb/activity/abc_def/impacts/EF3.1"
        assert result.single_score == 1.5
        assert result.available_nw_sets == ["PEF", "ECS"]
        assert result.scoring_indicators["ECS"]["climate"].category == "Climate change"
        assert result.scoring_indicators["ECS"]["climate"].value == 1.42e-6

    def test_get_impacts_batch_substitutions_post(self, mocked_client, make_response):
        client, session = mocked_client
        session.post.return_value = make_response({
            "results": [],
            "availableNWsets": [],
            "scoringResults": {},
            "scoringUnits": {},
            "scoringIndicators": {},
        })
        client.get_impacts_batch(
            "abc_def",
            collection="EF3.1",
            substitutions=[{"from": "a", "to": "b", "consumer": "c"}],
        )
        session.post.assert_called_once()
        session.get.assert_not_called()
        body = session.post.call_args[1]["json"]
        assert body == {
            "substitutions": [{"from": "a", "to": "b", "consumer": "c"}]
        }

    def test_get_impacts_batch_passes_exclude_long_term(self, mocked_client, make_response):
        """The engine route takes exclude-long-term; a caller comparing against a
        source that zeroes long-term factors needs to be able to say so here too,
        not only through score_activities.
        """
        client, session = mocked_client
        empty = {
            "results": [],
            "availableNWsets": [],
            "scoringResults": {},
            "scoringUnits": {},
            "scoringIndicators": {},
        }
        session.get.return_value = make_response(empty)
        client.get_impacts_batch("abc_def", collection="EF3.1", exclude_long_term=True)
        assert session.get.call_args[1]["params"] == {"exclude-long-term": "true"}

        # Left unsaid, the engine keeps its own default: no query parameter at all.
        session.get.return_value = make_response(empty)
        client.get_impacts_batch("abc_def", collection="EF3.1")
        assert session.get.call_args[1]["params"] == {}

        session.post.return_value = make_response(empty)
        client.get_impacts_batch(
            "abc_def",
            collection="EF3.1",
            substitutions=[{"from": "a", "to": "b", "consumer": "c"}],
            exclude_long_term=True,
        )
        assert session.post.call_args[1]["params"] == {"exclude-long-term": "true"}

    def test_get_impacts_batch_requires_db(self, fixture_spec, make_response):
        client = Client(base_url="http://test.local", db="")
        client._operations = _parse_spec(fixture_spec)
        with pytest.raises(VoLCAError, match="requires a database"):
            client.get_impacts_batch("abc_def")

    def test_unknown_operation_raises(self, mocked_client):
        client, _ = mocked_client
        with pytest.raises(VoLCAError, match="Unknown operationId"):
            client.call("does_not_exist")

    def test_body_on_get_operation_raises(self, mocked_client):
        client, session = mocked_client
        with pytest.raises(VoLCAError, match="takes no JSON body"):
            client.call("list_databases", body={"x": 1})
        session.get.assert_not_called()

    def test_unknown_kwarg_raises(self, mocked_client, make_response):
        client, session = mocked_client
        session.get.return_value = make_response({"dlrDatabases": []})
        with pytest.raises(VoLCAError, match="unknown kwargs"):
            client.call("list_databases", nonsense_param="foo")

    def test_auto_injects_db_name_from_instance(self, mocked_client, make_response, empty_envelope):
        """Client(db='testdb') should inject db_name without the caller passing it."""
        client, session = mocked_client
        session.get.return_value = make_response(empty_envelope())
        client.search_activities(name="x")
        called_url = session.get.call_args[0][0]
        assert "/db/testdb/" in called_url

    def test_db_name_override(self, mocked_client, make_response, empty_envelope):
        client, session = mocked_client
        session.get.return_value = make_response(empty_envelope())
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


class TestMethodResolution:
    """A caller knows the method; which collection carries it is the engine's business."""

    def test_name_resolves_to_its_collection_and_id(self, mocked_client, make_response):
        client, session = mocked_client
        client._methods_cache[:] = [_method("Water use", "EF3.1")]
        session.get.return_value = make_response(_minimal_lcia_result())
        client.get_impacts("abc_def", method_id="water USE")
        assert session.get.call_args[0][0].endswith(f"/impacts/EF3.1/{_WATER_USE_ID}")

    def test_unknown_method_never_reaches_the_engine(self, mocked_client, make_response):
        client, session = mocked_client
        client._methods_cache[:] = [_method("Water use", "EF3.1")]
        session.get.return_value = make_response([])
        with pytest.raises(VoLCAError, match="No loaded method"):
            client.get_impacts("abc_def", method_id="EF3.1-water-use")

    def test_same_name_in_two_collections_refuses_to_guess(self, mocked_client):
        client, _ = mocked_client
        client._methods_cache[:] = [
            _method("Water use", "EF3.1"),
            _method("Water use", "EF3.0", id="00000000-0000-0000-0000-000000000003"),
        ]
        with pytest.raises(VoLCAError, match="matches several loaded methods"):
            client.get_impacts("abc_def", method_id="Water use")

    def test_a_uuid_reaches_a_collection_less_url_without_a_lookup(self, mocked_client, make_response):
        """These routes search every collection themselves; asking first would
        only add a round-trip and turn their answer into a client refusal."""
        client, session = mocked_client
        session.get.return_value = make_response({"id": _WATER_USE_ID, "name": "Water use", "unit": "m3", "category": "Water use", "factorCount": 1})
        client.get_method(_WATER_USE_ID)
        assert session.get.call_count == 1
        assert session.get.call_args[0][0].endswith(f"/api/v1/method/{_WATER_USE_ID}")

    def test_an_upper_case_uuid_resolves_like_the_engine_reads_it(self, mocked_client, make_response):
        client, session = mocked_client
        client._methods_cache[:] = [_method("Water use", "EF3.1")]
        session.get.return_value = make_response(_minimal_lcia_result())
        client.get_impacts("abc_def", method_id=_WATER_USE_ID.upper())
        assert session.get.call_args[0][0].endswith(f"/impacts/EF3.1/{_WATER_USE_ID}")

    def test_a_uuid_the_engine_cannot_read_is_not_forwarded(self, mocked_client, make_response):
        """Python reads urn: and brace forms the engine's parser refuses."""
        client, session = mocked_client
        client._methods_cache[:] = [_method("Water use", "EF3.1")]
        session.get.return_value = make_response([])
        with pytest.raises(VoLCAError, match="No loaded method"):
            client.get_impacts("abc_def", method_id=f"urn:uuid:{_WATER_USE_ID}", collection="EF3.1")
        assert not [c for c in session.get.call_args_list if "/impacts/" in c[0][0]]

    def test_a_method_id_that_is_not_text_says_so(self, mocked_client):
        client, _ = mocked_client
        with pytest.raises(VoLCAError, match="must be a method UUID or a method name"):
            client.get_impacts("abc_def", method_id=None)

    def test_a_cold_cache_is_read_once_before_refusing(self, mocked_client, make_response):
        client, session = mocked_client
        session.get.return_value = make_response([])
        with pytest.raises(VoLCAError, match="No loaded method"):
            client.get_impacts("abc_def", method_id="Water use")
        assert session.get.call_count == 1

    def test_an_empty_answer_is_not_kept(self, mocked_client, make_response):
        """An engine still loading must not silence the client for good."""
        client, session = mocked_client
        session.get.side_effect = [
            make_response([]),
            make_response([{"id": _WATER_USE_ID, "name": "Water use", "category": "Water use", "unit": "m3", "factorCount": 1, "collection": "EF3.1"}]),
            make_response({"results": []}),
        ]
        with pytest.raises(VoLCAError, match="No loaded collection carries a method"):
            client.get_impacts_batch("abc_def")
        client.get_impacts_batch("abc_def")
        assert session.get.call_args[0][0].endswith("/impacts/EF3.1")

    def test_a_clone_shares_the_lookup_and_its_invalidation(self, mocked_client, make_response):
        client, session = mocked_client
        client._methods_cache[:] = [_method("Water use", "EF3.1")]
        other = client.use("otherdb")
        _resp = make_response({"success": True, "message": "loaded"})
        session.post.return_value = _resp
        other.load_method_collection("EF3.0")
        assert client._methods_cache == []

    def test_a_url_without_a_collection_still_takes_a_name(self, mocked_client, make_response):
        client, session = mocked_client
        client._methods_cache[:] = [_method("Water use", "EF3.1")]
        session.get.return_value = make_response({"id": _WATER_USE_ID, "name": "Water use", "unit": "m3", "category": "Water use", "factorCount": 1})
        client.get_method("Water use")
        assert session.get.call_args[0][0].endswith(f"/api/v1/method/{_WATER_USE_ID}")

    def test_batch_uses_the_only_loaded_collection(self, mocked_client, make_response):
        client, session = mocked_client
        client._methods_cache[:] = [_method("Water use", "EF3.1")]
        session.get.return_value = make_response({"results": [_minimal_lcia_result()]})
        client.get_impacts_batch("abc_def")
        assert session.get.call_args[0][0].endswith("/impacts/EF3.1")

    def test_batch_refuses_to_choose_between_collections(self, mocked_client):
        client, _ = mocked_client
        client._methods_cache[:] = [
            _method("Water use", "EF3.1"),
            _method("Land use", "plain-indicators", id="00000000-0000-0000-0000-000000000004"),
        ]
        with pytest.raises(VoLCAError, match="Several method collections"):
            client.get_impacts_batch("abc_def")

    def test_a_miss_refetches_before_giving_up(self, mocked_client, make_response):
        """A collection loaded since the client last looked must still be seen."""
        client, session = mocked_client
        client._methods_cache.clear()
        session.get.side_effect = [
            make_response([
                {
                    "id": _WATER_USE_ID,
                    "name": "Water use",
                    "category": "Water use",
                    "unit": "m3",
                    "factorCount": 1,
                    "collection": "EF3.1",
                }
            ]),
            make_response(_minimal_lcia_result()),
        ]
        client.get_impacts("abc_def", method_id="Water use")
        assert session.get.call_args[0][0].endswith(f"/impacts/EF3.1/{_WATER_USE_ID}")
