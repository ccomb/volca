"""Shared pytest fixtures for pyvolca tests.

The offline tests mock ``requests.Session`` so no real engine is required.
The drift test reads a committed OpenAPI spec to check that every
hand-written wrapper's operationId exists in the engine's current surface.
"""

from __future__ import annotations

import json
import subprocess
from pathlib import Path
from typing import Any, Callable
from unittest.mock import MagicMock

import pytest

from volca.client import Client


# ---------------------------------------------------------------------------
# A minimal OpenAPI fixture that covers the operations the tests exercise.
# Kept in sync with the real engine by the drift test (see test_drift.py).
# ---------------------------------------------------------------------------


@pytest.fixture(scope="session")
def fixture_spec() -> dict[str, Any]:
    """A hand-crafted OpenAPI 3 spec with the shape the dispatcher expects."""
    return {
        "openapi": "3.0.0",
        "paths": {
            "/api/v1/db": {
                "get": {
                    "operationId": "list_databases",
                    "parameters": [],
                },
            },
            "/api/v1/db/{dbName}/activity/{processId}": {
                "get": {
                    "operationId": "get_activity",
                    "parameters": [
                        {"name": "dbName", "in": "path", "required": True, "schema": {"type": "string"}},
                        {"name": "processId", "in": "path", "required": True, "schema": {"type": "string"}},
                    ],
                },
            },
            "/api/v1/db/{dbName}/activities": {
                "get": {
                    "operationId": "search_activities",
                    "parameters": [
                        {"name": "dbName", "in": "path", "required": True, "schema": {"type": "string"}},
                        {"name": "name", "in": "query", "required": False, "schema": {"type": "string"}},
                        {"name": "geo", "in": "query", "required": False, "schema": {"type": "string"}},
                        {"name": "product", "in": "query", "required": False, "schema": {"type": "string"}},
                        {"name": "exact", "in": "query", "required": False, "schema": {"type": "boolean"}},
                        {"name": "preset", "in": "query", "required": False, "schema": {"type": "string"}},
                        {"name": "classification", "in": "query", "required": False, "schema": {"type": "string"}},
                        {"name": "classification-value", "in": "query", "required": False, "schema": {"type": "string"}},
                        {"name": "classification-mode", "in": "query", "required": False, "schema": {"type": "string"}},
                        {"name": "limit", "in": "query", "required": False, "schema": {"type": "integer"}},
                        {"name": "offset", "in": "query", "required": False, "schema": {"type": "integer"}},
                        {"name": "sort", "in": "query", "required": False, "schema": {"type": "string"}},
                        {"name": "order", "in": "query", "required": False, "schema": {"type": "string"}},
                    ],
                },
            },
            "/api/v1/db/{dbName}/activity/{processId}/supply-chain": {
                "get": {
                    "operationId": "get_supply_chain",
                    "parameters": [
                        {"name": "dbName", "in": "path", "required": True, "schema": {"type": "string"}},
                        {"name": "processId", "in": "path", "required": True, "schema": {"type": "string"}},
                        {"name": "name", "in": "query", "required": False, "schema": {"type": "string"}},
                        {"name": "min-quantity", "in": "query", "required": False, "schema": {"type": "number"}},
                        {"name": "max-depth", "in": "query", "required": False, "schema": {"type": "integer"}},
                        {"name": "preset", "in": "query", "required": False, "schema": {"type": "string"}},
                    ],
                },
            },
            "/api/v1/db/{dbName}/activity/{processId}/impacts/{collection}/{methodId}": {
                "get": {
                    "operationId": "get_impacts",
                    "parameters": [
                        {"name": "dbName", "in": "path", "required": True, "schema": {"type": "string"}},
                        {"name": "processId", "in": "path", "required": True, "schema": {"type": "string"}},
                        {"name": "collection", "in": "path", "required": True, "schema": {"type": "string"}},
                        {"name": "methodId", "in": "path", "required": True, "schema": {"type": "string"}},
                        {"name": "top-flows", "in": "query", "required": False, "schema": {"type": "integer"}},
                    ],
                },
            },
            "/api/v1/db/{dbName}/activity/{processId}/aggregate": {
                "get": {
                    "operationId": "aggregate",
                    "parameters": [
                        {"name": "dbName", "in": "path", "required": True, "schema": {"type": "string"}},
                        {"name": "processId", "in": "path", "required": True, "schema": {"type": "string"}},
                        {"name": "scope", "in": "query", "required": False, "schema": {"type": "string"}},
                        {"name": "aggregate", "in": "query", "required": False, "schema": {"type": "string"}},
                        {"name": "group_by", "in": "query", "required": False, "schema": {"type": "string"}},
                        {"name": "is_input", "in": "query", "required": False, "schema": {"type": "boolean"}},
                        {"name": "max_depth", "in": "query", "required": False, "schema": {"type": "integer"}},
                        {"name": "filter_name", "in": "query", "required": False, "schema": {"type": "string"}},
                        {"name": "preset", "in": "query", "required": False, "schema": {"type": "string"}},
                    ],
                },
            },
            "/api/v1/methods": {
                "get": {"operationId": "list_methods", "parameters": []},
            },
            "/api/v1/db/{dbName}/classifications": {
                "get": {
                    "operationId": "list_classifications",
                    "parameters": [
                        {"name": "dbName", "in": "path", "required": True, "schema": {"type": "string"}},
                    ],
                },
            },
            "/api/v1/presets": {
                "get": {"operationId": "list_presets", "parameters": []},
            },
            "/api/v1/db/{dbName}/activity/{processId}/sensitivity/{collection}/{methodId}": {
                "post": {
                    "operationId": "compute_sensitivity",
                    "parameters": [
                        {"name": "dbName", "in": "path", "required": True, "schema": {"type": "string"}},
                        {"name": "processId", "in": "path", "required": True, "schema": {"type": "string"}},
                        {"name": "collection", "in": "path", "required": True, "schema": {"type": "string"}},
                        {"name": "methodId", "in": "path", "required": True, "schema": {"type": "string"}},
                    ],
                },
            },
            "/api/v1/db/{dbName}/impacts/{collection}": {
                "post": {
                    "operationId": "score_activities",
                    "parameters": [
                        {"name": "dbName", "in": "path", "required": True, "schema": {"type": "string"}},
                        {"name": "collection", "in": "path", "required": True, "schema": {"type": "string"}},
                        {"name": "top-flows", "in": "query", "required": False, "schema": {"type": "integer"}},
                        {"name": "exclude-long-term", "in": "query", "required": False, "schema": {"type": "boolean"}},
                    ],
                },
            },
        },
    }


@pytest.fixture()
def mocked_client(fixture_spec) -> tuple[Client, MagicMock]:
    """A Client whose session is mocked out, preloaded with ``fixture_spec``.

    Returns ``(client, session_mock)``: tests assert on session calls
    to verify the dispatcher built the correct URL / method / params.
    """
    client = Client(base_url="http://test.local", db="testdb", password="secret")
    mock_session = MagicMock()
    client._session = mock_session
    # Preload the operation table to skip the real spec fetch.
    from volca.client import _parse_spec
    client._operations = _parse_spec(fixture_spec)
    return client, mock_session


def _make_response(json_body: Any, status: int = 200) -> MagicMock:
    """Build a mock ``requests.Response`` that the client's _json accepts."""
    r = MagicMock()
    r.status_code = status
    r.reason = "OK" if status < 400 else "ERROR"
    r.content = json.dumps(json_body).encode()
    r.text = json.dumps(json_body)
    r.json.return_value = json_body
    r.headers = {}
    r.history = []
    r.raise_for_status = MagicMock()
    return r


@pytest.fixture()
def make_response() -> Callable[..., MagicMock]:
    """Factory for synthetic response objects in offline tests."""
    return _make_response


def _empty_envelope(limit: int = 20) -> dict:
    """Wire-complete empty SearchResults envelope.

    Pyvolca 0.5.0 rejects envelopes missing the pagination keys when a fetch
    callback is wired (strict mode). Tests that only care about request
    params, not the response, use this to satisfy the contract.
    """
    return {"results": [], "total": 0, "offset": 0, "limit": limit, "hasMore": False, "searchTimeMs": 0.0}


@pytest.fixture()
def empty_envelope() -> Callable[..., dict]:
    """Factory for a wire-complete empty SearchResults envelope."""
    return _empty_envelope


# ---------------------------------------------------------------------------
# Live spec for the drift test.
# ---------------------------------------------------------------------------


@pytest.fixture()
def readme_namespace() -> dict[str, Any]:
    """A namespace pre-populated for README example tests.

    Provides ``c``: a mocked Client whose every public method returns a
    realistic typed value built from the dataclasses in :mod:`volca.types`.
    The README executes against this without touching the network.

    Why a hand-rolled fake instead of the existing ``mocked_client`` /
    ``make_response`` fixtures: those mock at the HTTP layer, which forces
    every readme example to round-trip through ``_call``'s URL assembly.
    The point of the README test is to catch breakage of the *typed
    surface* (renamed methods, removed dataclass fields), not to retest
    the dispatcher; that is covered by ``test_dispatch.py`` and
    ``test_drift.py``.
    """
    from unittest.mock import MagicMock

    import volca
    from volca import (
        Activity,
        ActivityContribution,
        ActivityDetail,
        ActivityDiff,
        AggregateGroup,
        AggregateResult,
        AggregateScope,
        BioDirection,
        BiosphereExchange,
        CharacterizationFactor,
        CharacterizationResult,
        ClassificationSystem,
        Client,
        Compartment,
        ConsumerResult,
        ConsumersResponse,
        ContributingActivities,
        ContributingFlows,
        DatabaseStatus,
        Flow,
        FlowContribution,
        InventoryFlow,
        InventoryResult,
        InventoryStatistics,
        LCIABatchResult,
        LCIAResult,
        Method,
        SearchResults,
        ServerVersion,
        SupplyChain,
        SupplyChainEntry,
        TechRole,
        TechnosphereExchange,
        VoLCAError,
    )

    activity_a = Activity(
        process_id="aaaa1111-aaaa-bbbb-cccc-111122223333_dddd2222-eeee-ffff-aaaa-444455556666",
        activity_name="Wheat flour, type 55, at plant",
        location="FR",
        product_name="wheat flour",
        product_amount=1.0,
        product_unit="kg",
    )
    activity_b = Activity(
        process_id="bbbb2222-aaaa-bbbb-cccc-111122223333_eeee3333-eeee-ffff-aaaa-444455556666",
        activity_name="Wheat flour, type 65, at plant",
        location="FR",
        product_name="wheat flour",
        product_amount=1.0,
        product_unit="kg",
    )
    activity_detail = ActivityDetail(
        process_id=activity_a.process_id,
        activity_name=activity_a.activity_name,
        location=activity_a.location,
        unit="kg",
        description=["Bread-making wheat flour, soft variety, T55."],
        classifications={"ISIC rev.4 ecoinvent": "1061: Manufacture of grain mill products"},
        product_name="wheat flour",
        product_amount=1.0,
        product_unit="kg",
        all_products=[activity_a],
        exchanges=[
            TechnosphereExchange(
                flow_name="soft wheat grain, conventional",
                amount=1.31,
                unit="kg",
                role=TechRole.INPUT,
                target_activity_name="Soft wheat grain production, FR",
                target_location="FR",
                target_process_id="cccc3333-aaaa-bbbb-cccc-111122223333_aaaa4444-eeee-ffff-aaaa-444455556666",
            ),
            BiosphereExchange(
                flow_name="Carbon dioxide, fossil",
                compartment=Compartment(name="air"),
                amount=0.41,
                unit="kg",
                direction=BioDirection.EMISSION,
                flow_id="349b29d1-3e58-4c66-98b9-9d1a1b1f1234",
            ),
        ],
    )
    supply_chain = SupplyChain(
        root=activity_a,
        total_activities=42,
        filtered_activities=3,
        entries=[
            SupplyChainEntry(
                process_id="cccc3333-aaaa-bbbb-cccc-111122223333_aaaa4444-eeee-ffff-aaaa-444455556666",
                database_name="agribalyse-3-2",
                activity_name="Soft wheat grain, at farm",
                location="FR",
                quantity=1.31,
                unit="kg",
                scaling_factor=1.31,
                depth=1,
                upstream_count=1,
            ),
        ],
    )
    consumer_b = ConsumerResult(
        process_id=activity_b.process_id,
        activity_name="Sandwich bread, sliced, at plant",
        location="FR",
        product_name="bread",
        product_amount=1.0,
        product_unit="kg",
        depth=1,
    )
    consumers = ConsumersResponse(
        consumers=SearchResults(
            results=[consumer_b],
            total=1,
            offset=0,
            limit=10,
            has_more=False,
            search_time_ms=0.5,
        ),
    )
    lcia_result = LCIAResult(
        method_id="EF3.1-climate-change",
        method_name="EF v3.1 - Climate change",
        category="climate change",
        damage_category="climate",
        score=0.823,
        unit="kg CO2 eq",
        mapped_flows=12,
        functional_unit="kg",
        normalized_score=1.02e-4,
        weighted_score=2.1e-3,
        top_contributors=[
            FlowContribution(
                flow_name="Carbon dioxide, fossil",
                contribution=0.41,
                share_pct=49.8,
                flow_id="ef-co2-fossil",
                category="air/urban air",
            ),
        ],
    )
    lcia_batch = LCIABatchResult(
        results=[lcia_result],
        single_score=2.1e-3,
        single_score_unit="Pt",
    )
    aggregate_result = AggregateResult(
        scope=AggregateScope.BIOSPHERE,
        filtered_total=1.42,
        filtered_unit="kg",
        filtered_count=18,
        groups=[
            AggregateGroup(key="Carbon dioxide, fossil", quantity=0.41, count=4, unit="kg"),
            AggregateGroup(key="Methane, fossil", quantity=0.011, count=2, unit="kg"),
        ],
    )

    from volca import DatabaseInfo

    c = MagicMock(spec=Client)
    c.search_activities.return_value = SearchResults(
        results=[activity_a, activity_b],
        total=2,
        offset=0,
        limit=20,
        has_more=False,
        search_time_ms=0.3,
    )
    c.search_flows.return_value = SearchResults(
        results=[
            Flow(
                id="ef-co2-fossil",
                name="Carbon dioxide, fossil",
                category="air",
                unit_name="kg",
            ),
        ],
        total=1,
        offset=0,
        limit=50,
        has_more=False,
        search_time_ms=0.1,
    )
    c.get_activity.return_value = activity_detail
    c.get_supply_chain.return_value = supply_chain
    c.get_consumers.return_value = consumers
    c.get_inventory.return_value = InventoryResult(
        root=activity_a,
        total_flows=18,
        emission_flows=10,
        resource_flows=8,
        flows=[
            InventoryFlow(
                flow_id="ef-co2-fossil",
                flow_name="Carbon dioxide, fossil",
                quantity=0.41,
                unit_name="kg",
                is_emission=True,
                category="air/urban air",
            ),
        ],
        statistics=InventoryStatistics(
            total_quantity=1.42,
            emission_quantity=0.42,
            resource_quantity=1.0,
            top_categories=[("air/urban air", 5), ("water/river", 3)],
        ),
    )
    c.get_contributing_flows.return_value = ContributingFlows(
        method="EF v3.1 - Climate change",
        unit="kg CO2 eq",
        total_score=0.823,
        top_flows=[
            FlowContribution(
                flow_name="Carbon dioxide, fossil",
                contribution=0.41,
                share_pct=49.8,
                flow_id="ef-co2-fossil",
                category="air/urban air",
            ),
        ],
    )
    c.get_contributing_activities.return_value = ContributingActivities(
        method="EF v3.1 - Climate change",
        unit="kg CO2 eq",
        total_score=0.823,
        activities=[
            ActivityContribution(
                process_id="cccc3333-aaaa-bbbb-cccc-111122223333_aaaa4444-eeee-ffff-aaaa-444455556666",
                activity_name="Soft wheat grain, at farm",
                product_name="soft wheat grain",
                location="FR",
                contribution=0.31,
                share_pct=38.2,
            ),
        ],
    )
    c.get_characterization.return_value = CharacterizationResult(
        method="EF v3.1 - Climate change",
        unit="kg CO2 eq",
        matches=1,
        shown=1,
        factors=[
            CharacterizationFactor(
                method_flow_name="Carbon dioxide, fossil",
                cf_value=1.0,
                cf_unit="kg CO2 eq / kg",
                direction="Output",
                db_flow_name="Carbon dioxide, fossil",
                flow_id="ef-co2-fossil",
                flow_unit="kg",
                category="air",
                match_strategy="uuid",
            ),
        ],
    )
    c.get_version.return_value = ServerVersion(
        version="0.5.0", git_hash="abc1234", git_tag=None, build_target="x86_64-linux",
    )

    def _impacts(process_id, *args, **kwargs):
        if process_id == "nonexistent-pid":
            raise VoLCAError("not found", status_code=404, body="activity not found")
        return lcia_result

    c.get_impacts.side_effect = _impacts
    c.get_impacts_batch.return_value = lcia_batch
    c.aggregate.return_value = aggregate_result
    c.list_databases.return_value = [
        DatabaseInfo(
            name="agribalyse-3.2",
            display_name="Agribalyse 3.2",
            status=DatabaseStatus.LOADED,
            path="/data/agribalyse-3.2",
            activity_count=2517,
        ),
    ]
    c.list_methods.return_value = [
        Method(
            id="EF3.1-climate-change",
            name="Climate change",
            category="climate change",
            unit="kg CO2 eq",
            factor_count=420,
            collection="ef-31",
        ),
    ]
    c.list_classifications.return_value = [
        ClassificationSystem(
            name="ISIC rev.4 ecoinvent",
            values=["1061", "1071", "0111"],
            activity_count=2517,
        ),
    ]
    c.list_presets.return_value = []
    c.load_database.return_value = {"status": "loaded"}
    c.unload_database.return_value = {"status": "unloaded"}
    c.use.return_value = c
    c.refresh_stubs.return_value = None

    return {
        # Pull every public name into the exec namespace so examples can
        # reference any type without an explicit import.
        **{name: getattr(volca, name) for name in volca.__all__},
        "c": c,
    }


@pytest.fixture(scope="session")
def live_spec() -> dict[str, Any] | None:
    """The OpenAPI spec dumped from the currently-built engine binary.

    Returns None when no binary is there at all, so a checkout that has
    never built the engine skips the drift tests rather than failing. A
    binary that is present and will not answer is a failure, not an
    absence: the whole point of these tests is that nobody notices when
    they stop running.
    """
    # Walk up from pyvolca/tests/ to find the cabal dist-newstyle dir.
    here = Path(__file__).resolve()
    # .../volca-public/pyvolca/tests/conftest.py
    #                   ^~~~~~~~~ 2 levels up = pyvolca dir
    volca_public = here.parent.parent.parent
    binary = _newest_engine_binary(volca_public / "dist-newstyle")
    if binary is None:
        return None
    try:
        out = subprocess.check_output([str(binary), "dump-openapi"], timeout=30)
    except (subprocess.CalledProcessError, subprocess.TimeoutExpired) as exc:
        raise RuntimeError(f"{binary} will not dump its OpenAPI spec: {exc}") from exc
    return json.loads(out)


def _newest_engine_binary(dist_newstyle: Path) -> Path | None:
    """The most recently built engine executable, whatever level it was built at.

    cabal names the directory above `build/` after the optimization level:
    `opt/` at -O2, `noopt/` at -O0, and nothing at all at -O1. Matching only
    one of the three is how these tests came to skip themselves silently for
    months: CI downloads the engine artefact into `dist-newstyle/`, the level
    moved, and the glob stopped matching a binary that was sitting right
    there. `**` matches all three and the next one too.
    """
    pattern = "build/*/ghc-*/volca-*/x/volca/**/build/volca/volca"
    found = [p for p in dist_newstyle.rglob(pattern) if p.is_file()]
    return max(found, key=lambda p: p.stat().st_mtime) if found else None
