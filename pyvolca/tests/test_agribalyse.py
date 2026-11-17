"""``decompose`` puts what it reads from /aggregate and what it reads off the
activity on one basis.

The engine counts every aggregate for one unit of the process's reference
product, while the exchanges of ``get_activity`` are as the dataset declares
them: a process written for 10 kg lists its coproduct at the amount that comes
out with 10 kg. A decomposition that took the second as written would set a
coproduct against energies and masses a tenth of what it lists.
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


def _cheese_declared_at(amount: float | None) -> ActivityDetail:
    """A process using electricity directly whose reference product is declared
    at ``amount`` kg and whose whey coproduct is written at 4 kg."""

    def line(name: str, amount: float, unit: str, role: TechRole) -> TechnosphereExchange:
        return TechnosphereExchange(
            flow_name=name, amount=amount, unit=unit, role=role,
            target_activity_name=None, target_location=None, target_process_id=None,
        )

    return ActivityDetail(
        process_id="cheese_cheese", activity_name="Cheese", location="FR", unit="kg",
        description=[], classifications={}, product_name="cheese",
        product_amount=amount, product_unit="kg", all_products=[],
        exchanges=[
            line("cheese", amount or 0.0, "kg", TechRole.REFERENCE_PRODUCT),
            line("Electricity, medium voltage", 5.0, "kWh", TechRole.INPUT),
            line("whey", 4.0, "kg", TechRole.COPRODUCT),
        ],
    )


def _per_unit_aggregate() -> AggregateResult:
    """What the engine answers for any scope: for one kg of cheese."""
    return AggregateResult(
        scope=AggregateScope.DIRECT,
        functional_unit=FunctionalUnit(1.0, "kg", "cheese"),
        filtered_total_per_unit=0.5,
        filtered_unit="kg",
        filtered_count=1,
        groups=[AggregateGroup(key="milk", quantity_per_unit=1.0, count=1, unit="kg")],
    )


@pytest.mark.parametrize(
    ("declared", "whey_per_unit"),
    [
        (10.0, 0.4),
        (-10.0, 0.4),  # a magnitude: the sign is the direction, not the amount
        (0.0, 4.0),  # nothing to divide by, so the line stays as written
        (None, 4.0),
    ],
)
def test_coproducts_are_per_unit_like_the_totals(declared, whey_per_unit) -> None:
    client = MagicMock()
    client.get_activity.return_value = _cheese_declared_at(declared)
    client.aggregate.return_value = _per_unit_aggregate()

    result = decompose(client, "cheese_cheese")

    assert result.co_products == [("whey", pytest.approx(whey_per_unit), "kg")]
    assert result.electricity_mj == 0.5
    assert result.raw_material_kg == 1.0
