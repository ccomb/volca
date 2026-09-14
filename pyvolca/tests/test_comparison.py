"""Reading the engine's comparison of two activities, and of two databases.

Pure parsing: the payloads are shaped as wire revision 25 writes them, so no
engine binary is needed.
"""

from __future__ import annotations

from types import SimpleNamespace

import pytest

from volca import ActivityComparison, DatabaseComparison, compare_activities


def summary(name: str) -> dict:
    return {
        "processId": "a_p",
        "activityName": name,
        "location": "FR",
        "productName": "wheat grain",
        "productAmount": 1.0,
        "productUnit": "kg",
        "allocationPercent": None,
    }


COMPARISON = {
    "base": summary("wheat production"),
    "other": summary("wheat production, adapted"),
    "summary": [
        {"tag": "ActivityNameChanged", "before": "wheat production", "after": "wheat production, adapted"}
    ],
    "exchanges": [
        {
            "flowId": "f1",
            "flowName": "Carbon dioxide, fossil",
            "compartment": {"name": "air", "sub": None},
            "role": {"tag": "BioLine", "direction": "Emission"},
            "change": {
                "tag": "LineChanged",
                "match": "SameFlowName",
                "before": {"amount": 1.0, "unit": "kg"},
                "after": {"amount": 2.0, "unit": "kg"},
            },
        },
        {
            "flowId": "f2",
            "flowName": "electricity, medium voltage",
            "compartment": None,
            "role": {"tag": "TechLine", "role": "Input"},
            "change": {"tag": "LineRemoved", "before": {"amount": 0.3, "unit": "kWh"}},
        },
    ],
    "uncompared": [
        {
            "flowName": "sludge",
            "compartment": None,
            "role": {"tag": "WasteLine", "side": "WasteOutput"},
            "reason": {"tag": "MixedUnits", "baseUnits": ["g", "kg"], "otherUnits": ["kg"]},
        }
    ],
}


def test_an_activity_comparison_reads_every_part():
    c = ActivityComparison.from_json(COMPARISON)

    assert c.other.activity_name == "wheat production, adapted"
    assert [(s.field, s.before, s.after) for s in c.summary] == [
        ("activity_name", "wheat production", "wheat production, adapted")
    ]
    co2, electricity = c.exchanges
    assert (co2.kind, co2.role, co2.change, co2.matched_on) == ("biosphere", "Emission", "changed", "SameFlowName")
    assert co2.compartment is not None and co2.compartment.name == "air"
    assert co2.before is not None and co2.after is not None
    assert (co2.before.amount, co2.after.amount) == (1.0, 2.0)
    assert (electricity.kind, electricity.change, electricity.after, electricity.matched_on) == (
        "technosphere",
        "removed",
        None,
        None,
    )
    (sludge,) = c.uncompared
    assert (sludge.kind, sludge.role, sludge.reason) == ("waste", "WasteOutput", "mixed_units")
    assert (sludge.base_units, sludge.other_units) == (["g", "kg"], ["kg"])
    assert not c.identical


def test_a_database_comparison_keeps_its_counts_beside_truncated_lists():
    d = DatabaseComparison.from_json(
        {
            "addedCount": 2,
            "removedCount": 0,
            "changedCount": 1,
            "ambiguousCount": 1,
            "unchangedCount": 40,
            "added": [summary("oat production")],
            "removed": [],
            "changed": [{"match": "SameProduct", "comparison": COMPARISON}],
            "ambiguous": [
                {"match": "SameNames", "base": [summary("rye production"), summary("rye production")], "other": [summary("rye production")]}
            ],
        }
    )

    assert (d.added_count, len(d.added)) == (2, 1)
    assert d.changed[0].matched_on == "SameProduct"
    assert d.changed[0].comparison.exchanges[0].flow_name == "Carbon dioxide, fossil"
    assert (d.ambiguous[0].matched_on, len(d.ambiguous[0].base), len(d.ambiguous[0].other)) == ("SameNames", 2, 1)


def test_the_client_side_helper_says_what_replaced_it():
    client = SimpleNamespace(aggregate=lambda *args, **kwargs: SimpleNamespace(groups=[]))
    with pytest.warns(DeprecationWarning, match="Client.compare_activities"):
        compare_activities(client, "a_p", "b_p")  # type: ignore[arg-type]
