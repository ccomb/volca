"""``decompose`` puts what it reads from /aggregate and what it reads off the
activity on one basis.

The engine counts every aggregate for one unit of the process's reference
product, dividing as its matrix does (by what the process makes net of what it
consumes of its own product), while the exchanges of ``get_activity`` are as
the dataset declares them. A decomposition that took a coproduct as written
would set it against energies and masses counted on another basis, so it reads
the coproduct's amount from /aggregate too, and only its role from the
activity.
"""

from __future__ import annotations

from unittest.mock import MagicMock

import pytest

from volca import (
    ActivityDetail,
    AggregateGroup,
    AggregateResult,
    AggregateScope,
    FunctionalUnit,
    TechnosphereExchange,
    TechRole,
)
from volca.agribalyse import decompose


def _cheese_written_for_ten() -> ActivityDetail:
    """A process using electricity directly, written for 10 kg of cheese, whose
    whey coproduct is written at 4 kg."""

    def line(name: str, amount: float, unit: str, role: TechRole) -> TechnosphereExchange:
        return TechnosphereExchange(
            flow_name=name, amount=amount, unit=unit, role=role,
            target_activity_name=None, target_location=None, target_process_id=None,
        )

    return ActivityDetail(
        process_id="cheese_cheese", activity_name="Cheese", location="FR", unit="kg",
        description=[], classifications={}, product_name="cheese",
        product_amount=10.0, product_unit="kg", all_products=[],
        exchanges=[
            line("cheese", 10.0, "kg", TechRole.REFERENCE_PRODUCT),
            line("Electricity, medium voltage", 5.0, "kWh", TechRole.INPUT),
            line("whey", 4.0, "kg", TechRole.COPRODUCT),
        ],
    )


def _per_unit(groups: list[AggregateGroup]) -> AggregateResult:
    """What the engine answers: for one kg of cheese."""
    return AggregateResult(
        scope=AggregateScope.DIRECT,
        functional_unit=FunctionalUnit(1.0, "kg", "cheese"),
        filtered_total_per_unit=0.5,
        filtered_unit="kg",
        filtered_count=len(groups),
        groups=groups,
    )


def _engine(**kw: object) -> AggregateResult:
    if kw.get("is_input") is False:
        # The direct technosphere outputs other than the reference, in kg. The
        # whey reads 0.4 / 0.9 here, not the 0.4 that 4 kg / 10 kg would give:
        # the engine divides by the net output, which only it knows.
        assert kw == {
            "scope": "direct",
            "is_input": False,
            "filter_is_reference": False,
            "filter_exchange_type": "technosphere",
            "filter_unit": "kg",
            "group_by": "name",
        }
        return _per_unit([AggregateGroup(key="whey", quantity_per_unit=0.4 / 0.9, count=1, unit="kg")])
    return _per_unit([AggregateGroup(key="milk", quantity_per_unit=1.0, count=1, unit="kg")])


def test_coproducts_are_the_engine_per_unit_amounts() -> None:
    client = MagicMock()
    client.get_activity.return_value = _cheese_written_for_ten()
    client.aggregate.side_effect = lambda _pid, **kw: _engine(**kw)

    result = decompose(client, "cheese_cheese")

    assert result.co_products == [("whey", pytest.approx(0.4 / 0.9), "kg")]
    assert result.electricity_mj == 0.5
    assert result.raw_material_kg == 1.0
