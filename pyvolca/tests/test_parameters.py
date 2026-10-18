"""An activity's parameters and its exchanges' formulas, as the engine writes them."""

from __future__ import annotations

from volca.types import (
    ActivityDetail,
    ExchangeFormula,
    ExchangeSelector,
    Parameter,
    TechnosphereExchange,
    Uncertainty,
    parse_exchange,
)


def _activity(**extra: object) -> dict:
    pfa: dict = {
        "processId": "a_b",
        "activityName": "butter",
        "location": "FR",
        "unit": "kg",
        "exchanges": [],
    }
    pfa.update(extra)
    return {"activity": pfa}


def test_reads_an_input_parameter_with_its_uncertainty_and_a_calculated_one() -> None:
    detail = ActivityDetail.from_json(
        _activity(
            parameters=[
                {
                    "name": "Qm",
                    "value": 20.53,
                    "formula": None,
                    "unit": None,
                    "description": "Quantity of milk collected per year",
                    "uncertainty": {"distribution": "lognormal", "geometricMean": 20.53, "geometricSd": 1.07},
                },
                {"name": "share", "value": None, "formula": "Qm / 2", "unit": "kg", "description": "", "uncertainty": None},
            ],
            databaseParameters=[
                {
                    "name": "yield",
                    "value": 4,
                    "formula": None,
                    "unit": None,
                    "description": "",
                    "uncertainty": {"distribution": "triangular", "minimum": 1, "mode": 4, "maximum": 6},
                }
            ],
        )
    )
    assert detail.parameters == [
        Parameter(
            name="Qm",
            value=20.53,
            description="Quantity of milk collected per year",
            uncertainty=Uncertainty(distribution="lognormal", geometric_mean=20.53, geometric_sd=1.07),
        ),
        Parameter(name="share", value=None, formula="Qm / 2", unit="kg"),
    ]
    assert detail.database_parameters == [
        Parameter(name="yield", value=4, uncertainty=Uncertainty(distribution="triangular", minimum=1, mode=4, maximum=6))
    ]


def test_an_older_engine_gives_no_parameters() -> None:
    detail = ActivityDetail.from_json(_activity())
    assert detail.parameters == [] and detail.database_parameters == []


def test_reads_an_exchange_formula_and_its_absence() -> None:
    def line(formula: dict | None) -> TechnosphereExchange:
        exchange = parse_exchange(
            {
                "exchange": {"tag": "TechnosphereExchange", "amount": 4.0, "role": "Input", "formula": formula},
                "flowName": "fuel",
                "unitName": "kg",
            }
        )
        assert isinstance(exchange, TechnosphereExchange)
        return exchange

    assert line({"name": "fuel_use", "text": "Qm * 2"}).formula == ExchangeFormula(text="Qm * 2", name="fuel_use")
    assert line(None).formula is None


def test_reads_the_position_that_names_a_line_in_its_contributions() -> None:
    def line(**extra: object) -> int | None:
        return parse_exchange(
            {
                "exchange": {"tag": "BiosphereExchange", "amount": 0.05, "direction": "Emission", "flowId": "f"},
                "flowName": "carbon dioxide",
                "unitName": "kg",
                **extra,
            }
        ).position

    assert line(position=3) == 3
    assert line() is None


def test_reads_the_selector_an_edit_reaches_a_line_by() -> None:
    def selector(**extra: object) -> ExchangeSelector | None:
        exchange = parse_exchange(
            {
                "exchange": {"tag": "TechnosphereExchange", "amount": 0.2, "role": "Input"},
                "flowName": "electricity",
                "unitName": "kWh",
                **extra,
            }
        )
        assert isinstance(exchange, TechnosphereExchange)
        return exchange.selector

    assert selector(selector={"kind": "input", "flow": "f", "location": "FR"}) == ExchangeSelector.input_of_flow("f", "FR")
    assert selector() is None
