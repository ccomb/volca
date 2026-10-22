"""Offline tests for the comparison helpers: units to one basis, ratios, bands."""

from __future__ import annotations

import pytest

from volca import UnitDefinition
from volca.compare_plot import (
    DifferentMeasures,
    OppositeSigns,
    Point,
    UnknownUnit,
    ZeroScore,
    band,
    band_shares,
    fold,
    per_left_unit,
    ratio,
    scatter_grid,
    widest,
)

UNITS = {
    "kg": UnitDefinition("mass", 1.0),
    "g": UnitDefinition("mass", 0.001),
    "m3": UnitDefinition("volume", 1.0),
    "l": UnitDefinition("volume", 0.001),
}


class TestPerLeftUnit:
    def test_same_unit_needs_no_scaling(self):
        assert per_left_unit("m3", "m3", UNITS) == 1.0

    def test_a_score_per_g_is_a_thousand_times_smaller_than_per_kg(self):
        assert per_left_unit("kg", "g", UNITS) == pytest.approx(1000.0)
        assert per_left_unit("l", "m3", UNITS) == pytest.approx(0.001)

    def test_two_measures_need_a_factor(self):
        assert per_left_unit("m3", "kg", UNITS) == DifferentMeasures("m3", "kg")
        assert per_left_unit("m3", "kg", UNITS, factor=1000.0) == 1000.0

    def test_a_factor_is_ignored_between_units_of_one_measure(self):
        assert per_left_unit("kg", "g", UNITS, factor=7.0) == pytest.approx(1000.0)

    def test_an_unknown_unit_is_named(self):
        assert per_left_unit("kg", "tkm", UNITS) == UnknownUnit("tkm")


class TestRatio:
    def test_right_over_left(self):
        assert ratio(2.0, 3.0) == 1.5

    def test_two_credits_compare(self):
        assert ratio(-2.0, -1.0) == 0.5

    def test_zero_and_opposite_signs_say_why_not(self):
        assert ratio(0.0, 1.0) == ZeroScore()
        assert ratio(1.0, -1.0) == OppositeSigns()


class TestBands:
    def test_a_band_reads_the_same_both_ways(self):
        assert band(1.05) == band(1 / 1.05) == "±10 %"
        assert band(2.0) == band(0.5) == "×2"
        assert band(11.0) == "beyond"

    def test_shares_list_every_band(self):
        shares = band_shares([1.0, 1.4, 1.4, 50.0])
        assert shares == {"±10 %": 0.25, "×1.5": 0.5, "×2": 0.0, "×10": 0.0, "beyond": 0.25}
        assert set(band_shares([])) == set(shares)

    def test_widest_reads_gaps_either_way(self):
        points = [Point("a", 1, 1.1), Point("b", 10, 1), Point("c", 1, 5)]
        assert [p.label for p in widest(points, 2)] == ["b", "c"]

    def test_fold(self):
        assert fold(3.0) == "×3"
        assert fold(0.25) == "÷4"


def test_scatter_grid_writes_a_picture(tmp_path):
    pytest.importorskip("matplotlib")
    path = tmp_path / "clouds.png"
    points = {"Climate": [Point("a", 1, 2), Point("b", 3, 3)], "Water": []}
    scatter_grid(points, path, colour={"a": "step 1", "b": "step 2"}, labels=("before", "after"))
    assert path.stat().st_size > 0


def test_scatter_grid_refuses_more_groups_than_it_can_tell_apart(tmp_path):
    pytest.importorskip("matplotlib")
    points = {"Climate": [Point(f"a{i}", 1, 2) for i in range(21)]}
    with pytest.raises(ValueError, match="21 groups"):
        scatter_grid(points, tmp_path / "clouds.png", colour={f"a{i}": f"step {i}" for i in range(21)})
