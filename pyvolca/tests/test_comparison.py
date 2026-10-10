"""Reading the engine's comparison of two activities, and of two databases.

Pure parsing: the payloads are shaped as wire revision 25 writes them, so no
engine binary is needed.
"""

from __future__ import annotations

from datetime import date
from types import SimpleNamespace

import pytest

from volca import ActivityComparison, DatabaseComparison, DatasetDates, compare_activities


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
        {"tag": "ActivityNameChanged", "before": "wheat production", "after": "wheat production, adapted"},
        {"tag": "DescriptionChanged", "before": [], "after": ["From the 2024 survey."]},
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
            "flowId": "f3",
            "flowName": "barley grain",
            "compartment": None,
            "role": {"tag": "TechLine", "role": "Input"},
            "change": {
                "tag": "SupplierChanged",
                "before": {"activityName": "barley production", "location": "FR"},
                "after": {"activityName": "barley production, organic", "location": "FR"},
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
        ("activity_name", "wheat production", "wheat production, adapted"),
        ("description", [], ["From the 2024 survey."]),
    ]
    co2, barley, electricity = c.exchanges
    assert (barley.change, barley.before, barley.after) == ("supplier", None, None)
    assert barley.supplier_before is not None and barley.supplier_after is not None
    assert (barley.supplier_before.activity_name, barley.supplier_after.activity_name) == (
        "barley production",
        "barley production, organic",
    )
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


def test_a_dates_change_reads_both_sides_as_dates():
    c = ActivityComparison.from_json(
        {
            **COMPARISON,
            "summary": [
                {
                    "tag": "DatesChanged",
                    "before": {"created": None, "lastRevised": None, "stated": "2019-12-04"},
                    "after": {"created": None, "lastRevised": None, "stated": "2023-05-02"},
                }
            ],
        }
    )
    (change,) = c.summary
    assert change.field == "dates"
    assert change.before == DatasetDates(stated=date(2019, 12, 4))
    assert change.after == DatasetDates(stated=date(2023, 5, 2))


def test_a_database_comparison_keeps_its_counts_beside_truncated_lists():
    d = DatabaseComparison.from_json(
        {
            "addedCount": 2,
            "removedCount": 0,
            "changedCount": 1,
            "redatedCount": 3,
            "ambiguousCount": 1,
            "unchangedCount": 40,
            "added": [summary("oat production")],
            "removed": [],
            "changed": [{"match": "SameProduct", "comparison": COMPARISON}],
            "redated": [{"match": "SameProcessId", "comparison": COMPARISON}],
            "ambiguous": [
                {"match": "SameNames", "base": [summary("rye production"), summary("rye production")], "other": [summary("rye production")]}
            ],
        }
    )

    assert (d.added_count, len(d.added)) == (2, 1)
    assert d.changed[0].matched_on == "SameProduct"
    assert (d.redated_count, d.redated[0].matched_on) == (3, "SameProcessId")
    assert d.changed[0].comparison.exchanges[0].flow_name == "Carbon dioxide, fossil"
    assert (d.ambiguous[0].matched_on, len(d.ambiguous[0].base), len(d.ambiguous[0].other)) == ("SameNames", 2, 1)


def test_the_client_side_helper_says_what_replaced_it():
    client = SimpleNamespace(aggregate=lambda *args, **kwargs: SimpleNamespace(groups=[]))
    with pytest.warns(DeprecationWarning, match="Client.compare_activities"):
        compare_activities(client, "a_p", "b_p")  # type: ignore[arg-type]


def test_a_database_comparison_from_an_engine_before_redated_pairs_reads_none():
    d = DatabaseComparison.from_json(
        {
            "addedCount": 0,
            "removedCount": 0,
            "changedCount": 0,
            "ambiguousCount": 0,
            "unchangedCount": 3,
            "added": [],
            "removed": [],
            "changed": [],
            "ambiguous": [],
        }
    )

    assert (d.redated_count, d.redated) == (0, [])
