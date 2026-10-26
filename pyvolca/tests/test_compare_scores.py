"""Offline tests for holding one database's scores under two method implementations."""

from __future__ import annotations

import pytest

from volca import Method
from volca.compare_plot import Point
from volca.compare_scores import pair_categories, join


def method(name: str, category: str = "unknown", unit: str = "kg CO2 eq", collection: str = "c") -> Method:
    return Method(id=name, name=name, category=category, unit=unit, factor_count=1, collection=collection)


class TestPairCategories:
    def test_names_pair_case_and_spacing_aside(self):
        p = pair_categories([method("Climate change")], [method("climate  CHANGE")])
        assert [(x.left.name, x.right.name, x.matched_on) for x in p.pairs] == [("Climate change", "climate  CHANGE", "same name")]

    def test_words_pair_what_punctuation_alone_parts(self):
        p = pair_categories([method("Human toxicity, cancer_inorganics")], [method("Human toxicity cancer (inorganics)")])
        assert [x.matched_on for x in p.pairs] == ["same words"]

    def test_impact_category_pairs_what_names_leave(self):
        p = pair_categories([method("Acidification", "acidification")], [method("AP", "Acidification")])
        assert [x.matched_on for x in p.pairs] == ["same impact category"]

    def test_unknown_impact_category_pairs_nothing(self):
        p = pair_categories([method("A")], [method("B")])
        assert p.pairs == [] and [m.name for m in p.unpaired_left] == ["A"]

    def test_a_key_several_categories_hold_is_ambiguous(self):
        left = [method("Climate change, fossil", "climate change"), method("Climate change, biogenic", "climate change")]
        p = pair_categories(left, [method("GWP", "Climate change")])
        assert p.pairs == []
        assert [(a.matched_on, len(a.left), len(a.right)) for a in p.ambiguous] == [("same impact category", 2, 1)]

    def test_forced_pairs_come_first_and_leave_the_cascade(self):
        p = pair_categories([method("Land use"), method("Water")], [method("Land occupation"), method("Water")], {"Land use": "Land occupation"})
        assert {(x.left.name, x.matched_on) for x in p.pairs} == {("Land use", "forced"), ("Water", "same name")}

    def test_a_forced_name_must_hold_one_category(self):
        with pytest.raises(ValueError, match="right collection"):
            pair_categories([method("A")], [method("B")], {"A": "C"})

    def test_units_compare_without_punctuation_and_differing_ones_stand_apart(self):
        p = pair_categories(
            [method("Climate change", unit="kg CO2 Equivalents"), method("Water use", unit="m3 world eq")],
            [method("Climate change", unit="kg CO2-Eq"), method("Water use", unit="m3")],
        )
        assert [x.left.name for x in p.pairs] == ["Climate change"]
        assert [d.pair.left.name for d in p.different_units] == ["Water use"]

    def test_a_forced_pair_is_kept_whatever_its_units(self):
        p = pair_categories([method("Fossils", unit="MJ")], [method("Fossil use", unit="MJ (net calorific)")], {"Fossils": "Fossil use"})
        assert [x.matched_on for x in p.pairs] == ["forced"] and p.different_units == []


class TestJoin:
    def test_scores_pair_by_process_and_read_under_category_names(self):
        pair = pair_categories([method("Climate change")], [method("GWP 100", "climate change")], {"Climate change": "GWP 100"}).pairs
        left = [{"process_id": "p1", "activity_name": "wheat", "Climate change": 2.0}, {"process_id": "p2", "activity_name": "milk", "Climate change": 0.0}]
        right = [{"process_id": "p2", "activity_name": "milk", "GWP 100": 1.0}, {"process_id": "p1", "activity_name": "wheat", "GWP 100": 3.0}]
        [cat] = join(left, right, pair)
        assert cat.points == [Point("wheat", 2.0, 3.0)]
        assert cat.left_out == {"ZeroScore": 1}

    def test_an_activity_missing_on_the_right_is_counted(self):
        pair = pair_categories([method("X")], [method("X")]).pairs
        [cat] = join([{"process_id": "p1", "activity_name": "a", "X": 1.0}], [], pair)
        assert cat.points == [] and cat.left_out == {"NotScoredOnTheRight": 1}

    def test_a_burden_against_a_credit_is_not_drawn(self):
        pair = pair_categories([method("X")], [method("X")]).pairs
        [cat] = join([{"process_id": "p", "activity_name": "a", "X": 1.0}], [{"process_id": "p", "activity_name": "a", "X": -1.0}], pair)
        assert cat.left_out == {"OppositeSigns": 1}
