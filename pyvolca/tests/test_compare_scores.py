"""Offline tests for holding one database's scores under two method implementations."""

from __future__ import annotations

import pytest

from volca import Method, PairedActivities, UnitDefinition
from volca.compare_plot import Point
from volca.compare_scores import CategoryPair, join, pair_categories, scale_to_left


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
        assert [d.left.name for d in p.different_units] == ["Water use"]

    def test_a_forced_pair_is_kept_whatever_its_units(self):
        p = pair_categories([method("Fossils", unit="MJ")], [method("Fossil use", unit="MJ (net calorific)")], {"Fossils": "Fossil use"})
        assert [x.matched_on for x in p.pairs] == ["forced"] and p.different_units == []


    def test_a_category_forced_twice_is_refused(self):
        with pytest.raises(ValueError, match="two forced pairs"):
            pair_categories([method("A"), method("B")], [method("C")], {"A": "C", "B": "C"})

    def test_pairs_keep_the_left_collection_order(self):
        names = [f"c{i}" for i in range(20)]
        p = pair_categories([method(n) for n in names], [method(n) for n in reversed(names)])
        assert [x.left.name for x in p.pairs] == names


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

    def test_an_activity_missing_on_the_left_is_counted(self):
        pair = pair_categories([method("X")], [method("X")]).pairs
        [cat] = join([], [{"process_id": "p1", "activity_name": "a", "X": 1.0}], pair)
        assert cat.points == [] and cat.left_out == {"NotScoredOnTheLeft": 1}

    def test_a_burden_against_a_credit_is_not_drawn(self):
        pair = pair_categories([method("X")], [method("X")]).pairs
        [cat] = join([{"process_id": "p", "activity_name": "a", "X": 1.0}], [{"process_id": "p", "activity_name": "a", "X": -1.0}], pair)
        assert cat.left_out == {"OppositeSigns": 1}


def paired(left_id: str, right_id: str) -> PairedActivities:
    def summary(pid: str) -> dict:
        return {"processId": pid, "activityName": pid, "location": "CH", "productName": pid, "productAmount": 1.0, "productUnit": "kg"}

    return PairedActivities.from_json({"match": "SameNames", "base": summary(left_id), "other": summary(right_id)})


def row(pid: str, unit: str | None, score: float, amount: float = 1.0) -> dict:
    return {"process_id": pid, "activity_name": pid, "product_amount": amount, "product_unit": unit, "Climate": score}


UNITS = {"kg": UnitDefinition("mass", 1.0), "g": UnitDefinition("mass", 0.001), "m3": UnitDefinition("volume", 1.0)}


class TestScaleToLeft:
    def test_a_right_score_per_g_is_brought_to_one_kg_under_the_left_id(self):
        left, right, left_out = scale_to_left([paired("a", "x")], [row("a", "kg", 2.0)], [row("x", "g", 0.002)], UNITS)
        assert (right[0]["process_id"], right[0]["Climate"], right[0]["product_unit"]) == ("a", 2.0, "g")
        assert [p.right for p in join(left, right, [CategoryPair(method("Climate"), method("Climate"), "same name")])[0].points] == [2.0]
        assert not left_out

    def test_a_waste_written_with_minus_one_scores_as_one_unit(self):
        left, right, _ = scale_to_left([paired("a", "x")], [row("a", "kg", 3.0, amount=-1.0)], [row("x", "kg", 3.0)], UNITS)
        assert [p.right for p in join(left, right, [CategoryPair(method("Climate"), method("Climate"), "same name")])[0].points] == [3.0]

    def test_references_of_different_measures_are_counted_once_not_drawn(self):
        left, right, left_out = scale_to_left([paired("a", "x")], [row("a", "kg", 1.0)], [row("x", "m3", 1.0)], UNITS)
        assert (left, right, left_out) == ([], [], {"DifferentMeasures": 1})

    def test_unscored_unknown_unit_and_missing_reference_each_say_why(self):
        pairs = [paired("a", "x"), paired("b", "y"), paired("c", "z"), paired("d", "w")]
        lefts = [row("b", "kg", 1.0), row("c", "kg", 1.0), row("d", None, 1.0)]
        rights = [row("x", "kg", 1.0), row("z", "furlong", 1.0), row("w", "kg", 1.0)]
        _, _, left_out = scale_to_left(pairs, lefts, rights, UNITS)
        assert left_out == {"NotScoredOnTheLeft": 1, "NotScoredOnTheRight": 1, "UnknownUnit": 1, "NoReferenceProduct": 1}
