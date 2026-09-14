"""Drift test: every hand-written wrapper's operationId exists in the live spec.

This test fails when the engine renames or removes an operation that a
pyvolca wrapper still references, which would produce a 404 at runtime
otherwise. Skipped when the engine binary isn't available (e.g. on a
Python-only dev machine).
"""

from __future__ import annotations

import pytest

from volca.client import Client, _parse_spec


# Hand-written wrappers in Client that route through _call. Each listed
# operationId must exist in the live engine's OpenAPI spec.
#
# This list is the pyvolca side of the "Resources SOT" contract. Adding a
# wrapper means adding its operationId here; the drift test catches typos
# and engine-side renames.
WRAPPER_OPERATIONS = [
    "list_databases",
    "load_database",
    "unload_database",
    "list_presets",
    "search_activities",
    "search_flows",
    "count_search_matches",
    "list_classifications",
    "get_activity",
    "get_supply_chain",
    "aggregate",
    "get_consumers",
    "get_path_to",
    "compare_activities",
    "compare_databases",
    "get_inventory",
    "get_impacts",
    "list_methods",
    "get_flow_mapping",
    "get_characterization",
    "get_contributing_flows",
    "get_contributing_activities",
    "compute_sensitivity",
    "score_activities",
]


def test_every_wrapper_operation_exists_in_live_spec(live_spec):
    """Every wrapper's operationId must be present in the engine's current OpenAPI spec."""
    if live_spec is None:
        pytest.skip(
            "No built engine binary found (run `cabal build` first). "
            "The drift test runs in CI where the engine is always built."
        )
    live_ops = _parse_spec(live_spec)
    missing = [op for op in WRAPPER_OPERATIONS if op not in live_ops]
    assert not missing, (
        f"pyvolca wrappers reference operationIds that don't exist in the "
        f"live engine spec: {missing}. Either update the wrapper list in "
        f"test_drift.py and remove the stale wrapper from client.py, or "
        f"restore the operationId in API/Resources.hs if it was accidentally "
        f"dropped."
    )


def test_all_resources_have_operationid(live_spec):
    """Sanity check: the engine's enrichment layer actually stamps operationIds.

    Regression guard against the enrichment code in API/OpenApi.hs silently
    losing its way, e.g. if a Resource constructor is added without updating
    apiPath, or if the lens plumbing breaks.
    """
    if live_spec is None:
        pytest.skip("No built engine binary found.")
    live_ops = _parse_spec(live_spec)
    # 22 of the 25 Resource constructors carry an apiPath, so each stamps an
    # operationId; the 3 MCP-only ones (list_geographies / compare_impacts /
    # list_scoring_sets) correctly have none. A looser bound would silently
    # tolerate a path-template mismatch dropping an operationId.
    assert len(live_ops) >= 22, (
        f"Only {len(live_ops)} operations have operationIds in the live spec. "
        f"Expected ≥22. Check API/OpenApi.hs enrichWithResources."
    )
