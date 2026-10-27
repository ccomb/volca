"""Hold one database's scores under two implementations of a method.

Two implementations of one method (EF 3.1 as its authors publish it and as
a software adapts it, say) name their categories each their own way. Comparing
them takes three steps, each a function here: :func:`pair_categories` pairs
the categories of the two collections, :func:`join` puts each activity's two
scores side by side for every pair, and :meth:`volca.Client.compare_methods`
does both from two calls to :meth:`volca.Client.score_table`. The points draw
with :func:`volca.compare_plot.scatter_grid`.

Two databases scored under one collection (one dataset in two formats, say)
pair by activity instead: :func:`scale_to_left` brings each pair's right
scores to the left reference, then :func:`join` holds them per method, and
:meth:`volca.Client.compare_database_scores` does it all.
"""

from __future__ import annotations

import re
from collections import Counter
from collections.abc import Callable, Mapping, Sequence
from dataclasses import dataclass, field
from typing import Literal

from .compare_plot import DifferentMeasures, Point, UnknownUnit, per_left_unit, ratio
from .types import DatabaseComparison, Method, PairedActivities, UnitDefinition

CategoryMatch = Literal["forced", "same name", "same words", "same impact category"]


@dataclass(frozen=True)
class CategoryPair:
    """A category of each collection, and the rung that paired them."""

    left: Method
    right: Method
    matched_on: CategoryMatch


@dataclass(frozen=True)
class AmbiguousCategories:
    """Categories that answer to one key on a rung, too many for a pair."""

    matched_on: CategoryMatch
    left: list[Method]
    right: list[Method]


@dataclass(frozen=True)
class CategoryPairing:
    """How the categories of two collections paired, and what was left."""

    pairs: list[CategoryPair]
    different_units: list[CategoryPair]
    ambiguous: list[AmbiguousCategories]
    unpaired_left: list[Method]
    unpaired_right: list[Method]


@dataclass(frozen=True)
class CategoryPoints:
    """One category pair's points, and how many activities could not be drawn and why."""

    pair: CategoryPair
    points: list[Point]
    left_out: Counter[str] = field(default_factory=Counter)


@dataclass(frozen=True)
class MethodComparison:
    """One database scored under two collections, as :meth:`volca.Client.compare_methods` returns it."""

    pairing: CategoryPairing
    categories: list[CategoryPoints]


@dataclass(frozen=True)
class DatabaseScoreComparison:
    """Two databases scored under one collection, as :meth:`volca.Client.compare_database_scores` returns it.

    ``activities`` is how the activities paired, with what did not; ``left_out``
    counts the pairs set aside before any score was read, by reason. Each of
    ``categories`` pairs a method with itself.
    """

    activities: DatabaseComparison
    left_out: Counter[str]
    categories: list[CategoryPoints]


def category_key(name: str) -> str:
    """A category name without case and with its spacing collapsed, as the engine pairs them."""
    return " ".join(name.split()).casefold()


def words_key(name: str) -> str:
    """A name's words alone: ``Human toxicity, cancer_inorganics`` is ``Human toxicity cancer (inorganics)``."""
    return " ".join(re.findall(r"[^\W_]+", name.casefold()))


def unit_key(unit: str) -> str:
    """A result unit's words alone, ``equivalents`` read as ``eq``: ``kg CO2 Equivalents`` is ``kg CO2-eq``."""
    return " ".join("eq" if w in ("equivalent", "equivalents") else w for w in words_key(unit).split())


def pair_categories(
    left: Sequence[Method],
    right: Sequence[Method],
    forced: Mapping[str, str] | None = None,
) -> CategoryPairing:
    """Pair the categories of two collections the way the engine does.

    ``forced`` pairs come first, each written left name to right name. The rest
    pair on their name, case and spacing aside, then on its words alone,
    punctuation aside too, then on their impact category (``"unknown"`` is
    none). The engine's own pairing stops short of the words rung: two
    implementations of one method punctuate their names each their own way. On each rung a key held by exactly one category on
    each side makes a pair; a key held by more is ambiguous and leaves the
    cascade. A pair the cascade made whose result units differ, punctuation
    aside, is set apart in ``different_units``: its two scores may not measure
    the same thing. A forced pair is the caller's word, units included
    (``MJ`` against ``MJ (net calorific)``, say).
    Raises ValueError for a forced name no category or several categories
    hold, and for a category named in two forced pairs.
    """
    forced = dict(forced or {})
    pairs: list[CategoryPair] = []
    ambiguous: list[AmbiguousCategories] = []

    def named(name: str, methods: Sequence[Method], side: str) -> Method:
        found = [m for m in methods if category_key(m.name) == category_key(name)]
        if len(found) != 1:
            raise ValueError(f"{len(found)} categories of the {side} collection are named {name!r}; a forced pair needs exactly one")
        return found[0]

    for l, r in forced.items():
        pairs.append(CategoryPair(named(l, left, "left"), named(r, right, "right"), "forced"))
    taken = {id(p.left) for p in pairs} | {id(p.right) for p in pairs}
    if len(taken) != 2 * len(pairs):
        raise ValueError("a category is named in two forced pairs; each pairs once")
    rest_left = [m for m in left if id(m) not in taken]
    rest_right = [m for m in right if id(m) not in taken]

    rungs: list[tuple[CategoryMatch, Callable[[Method], str | None]]] = [
        ("same name", lambda m: category_key(m.name)),
        ("same words", lambda m: words_key(m.name)),
        ("same impact category", lambda m: None if m.category == "unknown" else category_key(m.category)),
    ]
    for matched_on, key in rungs:
        by_left: dict[str, list[Method]] = {}
        by_right: dict[str, list[Method]] = {}
        for m in rest_left:
            if (k := key(m)) is not None:
                by_left.setdefault(k, []).append(m)
        for m in rest_right:
            if (k := key(m)) is not None:
                by_right.setdefault(k, []).append(m)
        done: set[int] = set()
        for k, ls in by_left.items():
            if (rs := by_right.get(k)) is None:
                continue
            if len(ls) == 1 and len(rs) == 1:
                pairs.append(CategoryPair(ls[0], rs[0], matched_on))
            else:
                ambiguous.append(AmbiguousCategories(matched_on, ls, rs))
            done |= {id(m) for m in ls + rs}
        rest_left = [m for m in rest_left if id(m) not in done]
        rest_right = [m for m in rest_right if id(m) not in done]

    same = [p for p in pairs if p.matched_on == "forced" or unit_key(p.left.unit) == unit_key(p.right.unit)]
    return CategoryPairing(
        pairs=same,
        different_units=[p for p in pairs if p not in same],
        ambiguous=ambiguous,
        unpaired_left=rest_left,
        unpaired_right=rest_right,
    )


def join(
    left_rows: Sequence[Mapping],
    right_rows: Sequence[Mapping],
    pairs: Sequence[CategoryPair],
) -> list[CategoryPoints]:
    """Each activity's two scores, one list of points per category pair.

    The rows are :meth:`volca.Client.score_table` rows of one database under
    two collections, or rows of two databases :func:`scale_to_left` already
    put on one basis, so an activity is the same on both sides and its scores
    are per the same reference: they pair by ``process_id`` and need no
    scaling. A score is read under its category's name, never its impact
    category, which several categories share. An activity scored on one side
    only, a zero, or a burden against a credit is counted in ``left_out``
    rather than drawn. Two credits make a point: their ratio holds, though a
    log scale draws neither.
    """
    right_by_id = {r["process_id"]: r for r in right_rows}
    left_only = len({l["process_id"] for l in left_rows} - right_by_id.keys())
    right_only = len(right_by_id.keys() - {l["process_id"] for l in left_rows})
    joined = []
    for pair in pairs:
        points: list[Point] = []
        left_out: Counter[str] = +Counter(NotScoredOnTheRight=left_only, NotScoredOnTheLeft=right_only)
        for l in left_rows:
            if (r := right_by_id.get(l["process_id"])) is None:
                continue
            a, b = l[pair.left.name], r[pair.right.name]
            judged = ratio(a, b)
            if isinstance(judged, float):
                points.append(Point(l["activity_name"], a, b))
            else:
                left_out[type(judged).__name__] += 1
        joined.append(CategoryPoints(pair, points, left_out))
    return joined


ROW_IDENTITY = ("process_id", "activity_name", "product_amount", "product_unit")
"""The keys of a :meth:`volca.Client.score_table` row that are not scores."""


def scale_to_left(
    pairs: Sequence[PairedActivities],
    left_rows: Sequence[Mapping],
    right_rows: Sequence[Mapping],
    units: Mapping[str, UnitDefinition],
) -> tuple[list[Mapping], list[Mapping], Counter[str]]:
    """Each pair's two rows on one basis, ready for :func:`join`.

    A score is per one unit of its activity's reference, whatever the amount
    and sign the reference is written with, so the right row's scores are
    multiplied by :func:`volca.compare_plot.per_left_unit` and the row takes
    the left process id. A pair is set aside, and counted under its reason,
    when either side was not scored, has no reference product, or when the
    two references measure different things or use a unit the table lacks:
    the reason holds for every method, so it is counted once per pair.
    """
    left_by_id = {r["process_id"]: r for r in left_rows}
    right_by_id = {r["process_id"]: r for r in right_rows}
    left: list[Mapping] = []
    right: list[Mapping] = []
    left_out: Counter[str] = Counter()
    for pair in pairs:
        l, r = left_by_id.get(pair.base.process_id), right_by_id.get(pair.other.process_id)
        if l is None or r is None:
            left_out["NotScoredOnTheLeft" if l is None else "NotScoredOnTheRight"] += 1
            continue
        if l["product_unit"] is None or r["product_unit"] is None:
            left_out["NoReferenceProduct"] += 1
            continue
        scale = per_left_unit(l["product_unit"], r["product_unit"], units)
        if isinstance(scale, (UnknownUnit, DifferentMeasures)):
            left_out[type(scale).__name__] += 1
            continue
        left.append(l)
        right.append(
            {**r, "process_id": l["process_id"]} | {k: v * scale for k, v in r.items() if k not in ROW_IDENTITY}
        )
    return left, right, left_out
