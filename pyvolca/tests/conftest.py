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
                        {"name": "limit", "in": "query", "required": False, "schema": {"type": "integer"}},
                        {"name": "offset", "in": "query", "required": False, "schema": {"type": "integer"}},
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
                        {"name": "is_input", "in": "query", "required": False, "schema": {"type": "boolean"}},
                        {"name": "max_depth", "in": "query", "required": False, "schema": {"type": "integer"}},
                        {"name": "filter_name", "in": "query", "required": False, "schema": {"type": "string"}},
                        {"name": "preset", "in": "query", "required": False, "schema": {"type": "string"}},
                    ],
                },
            },
        },
    }


@pytest.fixture()
def mocked_client(fixture_spec) -> tuple[Client, MagicMock]:
    """A Client whose session is mocked out, preloaded with ``fixture_spec``.

    Returns ``(client, session_mock)`` — tests assert on session calls
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
    r.history = []
    r.raise_for_status = MagicMock()
    return r


@pytest.fixture()
def make_response() -> Callable[..., MagicMock]:
    """Factory for synthetic response objects in offline tests."""
    return _make_response


# ---------------------------------------------------------------------------
# Live spec for the drift test.
# ---------------------------------------------------------------------------


@pytest.fixture()
def readme_namespace() -> dict[str, Any]:
    """A namespace pre-populated for README example tests.

    Provides ``c`` — a mocked Client whose every public method returns a
    realistic typed value built from the dataclasses in :mod:`volca.types`.
    The README executes against this without touching the network.

    Why a hand-rolled fake instead of the existing ``mocked_client`` /
    ``make_response`` fixtures: those mock at the HTTP layer, which forces
    every readme example to round-trip through ``_call``'s URL assembly.
    The point of the README test is to catch breakage of the *typed
    surface* (renamed methods, removed dataclass fields), not to retest
    the dispatcher — that is covered by ``test_dispatch.py`` and
    ``test_drift.py``.
    """
    from unittest.mock import MagicMock

    import volca
    from volca import (
        Activity,
        ActivityDetail,
        ActivityDiff,
        AggregateGroup,
        AggregateResult,
        BiosphereExchange,
        Client,
        ConsumerResult,
        ConsumersResponse,
        FlowContribution,
        LCIABatchResult,
        LCIAResult,
        SupplyChain,
        SupplyChainEntry,
        TechnosphereExchange,
        VoLCAError,
    )

    activity_a = Activity(
        process_id="aaaa1111-aaaa-bbbb-cccc-111122223333_dddd2222-eeee-ffff-aaaa-444455556666",
        name="Wheat flour, type 55, at plant",
        location="FR",
        product="wheat flour",
        product_amount=1.0,
        product_unit="kg",
    )
    activity_b = Activity(
        process_id="bbbb2222-aaaa-bbbb-cccc-111122223333_eeee3333-eeee-ffff-aaaa-444455556666",
        name="Wheat flour, type 65, at plant",
        location="FR",
        product="wheat flour",
        product_amount=1.0,
        product_unit="kg",
    )
    activity_detail = ActivityDetail(
        process_id=activity_a.process_id,
        name=activity_a.name,
        location=activity_a.location,
        unit="kg",
        description=["Bread-making wheat flour, soft variety, T55."],
        classifications={"ISIC rev.4 ecoinvent": "1061: Manufacture of grain mill products"},
        reference_product="wheat flour",
        reference_product_amount=1.0,
        reference_product_unit="kg",
        all_products=[activity_a],
        exchanges=[
            TechnosphereExchange(
                flow_name="soft wheat grain, conventional",
                flow_category="agricultural",
                amount=1.31,
                unit="kg",
                is_input=True,
                is_reference=False,
                target_activity="Soft wheat grain production, FR",
                target_location="FR",
                target_process_id="cccc3333-aaaa-bbbb-cccc-111122223333_aaaa4444-eeee-ffff-aaaa-444455556666",
            ),
            BiosphereExchange(
                flow_name="Carbon dioxide, fossil",
                flow_category="air",
                amount=0.41,
                unit="kg",
                is_input=False,
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
                name="Soft wheat grain, at farm",
                location="FR",
                quantity=1.31,
                unit="kg",
                scaling_factor=1.31,
            ),
        ],
    )
    consumers = ConsumersResponse(
        consumers=[
            ConsumerResult(
                process_id=activity_b.process_id,
                name="Sandwich bread, sliced, at plant",
                location="FR",
                product="bread",
                product_amount=1.0,
                product_unit="kg",
                depth=1,
            ),
        ],
        total=1,
        offset=0,
        limit=10,
        has_more=False,
        search_time_ms=0.5,
    )
    lcia_result = LCIAResult(
        method_id="EF3.1-climate-change",
        method_name="EF v3.1 — Climate change",
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
        scope="biosphere",
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
    c.search_activities.return_value = [activity_a, activity_b]
    c.search_flows.return_value = [
        {"flow_id": "ef-co2-fossil", "name": "Carbon dioxide, fossil", "unit": "kg", "category": "air"},
    ]
    c.get_activity.return_value = activity_detail
    c.get_supply_chain.return_value = supply_chain
    c.get_consumers.return_value = consumers
    c.get_inventory.return_value = {
        "flows": [
            {"flow_id": "ef-co2-fossil", "name": "Carbon dioxide, fossil", "amount": 0.41, "unit": "kg"},
        ],
        "total_count": 18,
    }
    c.get_contributing_flows.return_value = {
        "contributors": [
            {"flow_id": "ef-co2-fossil", "name": "Carbon dioxide, fossil", "share_pct": 49.8},
        ],
    }
    c.get_contributing_activities.return_value = {
        "contributors": [
            {"process_id": "cccc3333-aaaa-bbbb-cccc-111122223333_aaaa4444-eeee-ffff-aaaa-444455556666",
             "name": "Soft wheat grain, at farm", "share_pct": 38.2},
        ],
    }
    c.get_characterization.return_value = {
        "method_id": "EF3.1-climate-change",
        "factors": [{"flow_id": "ef-co2-fossil", "cf": 1.0, "unit": "kg CO2 eq / kg"}],
    }

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
            status="loaded",
            path="/data/agribalyse-3.2",
            activity_count=2517,
        ),
    ]
    c.list_methods.return_value = [
        {"methodId": "EF3.1-climate-change", "name": "Climate change", "unit": "kg CO2 eq"},
    ]
    c.list_classifications.return_value = [
        {"system": "ISIC rev.4 ecoinvent", "values": ["1061", "1071", "0111"]},
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

    Returns None if the binary isn't built — callers should skip their
    drift test rather than fail CI hard in that case.
    """
    # Walk up from pyvolca/tests/ to find the cabal dist-newstyle dir.
    here = Path(__file__).resolve()
    # .../volca-public/pyvolca/tests/conftest.py
    #                   ^~~~~~~~~ 2 levels up = pyvolca dir
    volca_public = here.parent.parent.parent
    candidates = list(
        (volca_public / "dist-newstyle").rglob("build/*/ghc-*/volca-*/x/volca/opt/build/volca/volca")
    )
    if not candidates:
        return None
    binary = sorted(candidates)[-1]
    try:
        out = subprocess.check_output([str(binary), "dump-openapi"], timeout=30)
    except (subprocess.CalledProcessError, subprocess.TimeoutExpired):
        return None
    return json.loads(out)
