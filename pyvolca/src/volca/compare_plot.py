"""Put two scores on one basis, sort their gaps into bands, and draw them.

A score is per one unit of its activity's reference product. Comparing an
activity scored per kg with one scored per g, or per m3, needs both scores
turned to one unit first: :func:`per_left_unit` gives the scale, or says why
there is none.
:func:`ratio` then compares the two, :func:`band` and :func:`band_shares` sort
the ratios, :func:`widest` picks the largest gaps, and :func:`scatter_grid`
draws them (it alone needs matplotlib: ``pip install pyvolca[plot]``).
"""

from __future__ import annotations

from collections.abc import Mapping, Sequence
from dataclasses import dataclass
from math import log
from pathlib import Path

from .types import UnitDefinition


@dataclass(frozen=True)
class UnknownUnit:
    """The unit table holds no such unit."""

    unit: str


@dataclass(frozen=True)
class DifferentMeasures:
    """The two units measure different things (kg and m3) and no factor was given."""

    left: str
    right: str


@dataclass(frozen=True)
class ZeroScore:
    """One side scores zero: no ratio says anything about it."""


@dataclass(frozen=True)
class OppositeSigns:
    """One side is a burden, the other a credit."""


@dataclass(frozen=True)
class Point:
    """One activity scored on both sides, the right already per one left unit."""

    label: str
    left: float
    right: float


def per_left_unit(
    left_unit: str,
    right_unit: str,
    units: Mapping[str, UnitDefinition],
    factor: float | None = None,
) -> float | UnknownUnit | DifferentMeasures:
    """What a right score is multiplied by to be per one left unit.

    A score is per one unit of what its activity does, whatever the sign its
    reference is written with: a waste market written with -1 m3 and a
    treatment written with +1 m3 both score the treatment of one m3. Only the
    units differ: a right score per g is multiplied by 1000 to be per kg.
    ``factor`` is how many right units one left unit holds when the two
    measure different things (a density, for kg against m3), and is used only
    then.
    """
    for unit in (left_unit, right_unit):
        if unit not in units:
            return UnknownUnit(unit)
    l, r = units[left_unit], units[right_unit]
    if l.dimension == r.dimension:
        return l.factor / r.factor
    if factor is None:
        return DifferentMeasures(left_unit, right_unit)
    return factor


def ratio(left: float, right: float) -> float | ZeroScore | OppositeSigns:
    """Right over left, for two scores already on one basis; two credits compare."""
    if left == 0 or right == 0:
        return ZeroScore()
    if (left > 0) != (right > 0):
        return OppositeSigns()
    return right / left


BANDS: list[tuple[float, str]] = [(1.1, "±10 %"), (1.5, "×1.5"), (2, "×2"), (10, "×10")]
"""Each band's widest fold and its label; a ratio past the last is ``"beyond"``."""


def band(r: float) -> str:
    """The narrowest band a positive ratio falls in, read the same both ways (2 and 0.5 are ×2)."""
    fold = max(r, 1 / r)
    return next((label for edge, label in BANDS if fold <= edge), "beyond")


def band_shares(ratios: Sequence[float]) -> dict[str, float]:
    """The share of ratios in each band, every band listed, empty ones at 0."""
    counts = {label: 0 for _, label in BANDS} | {"beyond": 0}
    for r in ratios:
        counts[band(r)] += 1
    return {label: n / len(ratios) if ratios else 0.0 for label, n in counts.items()}


def widest(points: Sequence[Point], n: int) -> list[Point]:
    """The ``n`` points whose two sides differ most, either way."""
    return sorted(points, key=lambda p: abs(log(p.right / p.left)), reverse=True)[:n]


def fold(r: float) -> str:
    """A ratio as a reader says it: ``×3`` above one, ``÷3`` below."""
    return f"×{r:.3g}" if r >= 1 else f"÷{1 / r:.3g}"


def scatter_grid(
    points_by_axis: Mapping[str, Sequence[Point]],
    path: str | Path,
    *,
    colour: Mapping[str, str] | None = None,
    labels: tuple[str, str] = ("left", "right"),
    named: int = 3,
) -> None:
    """One log-log scatter per axis (a category, say), written to ``path``.

    Each plot has its diagonal, a ±10 % and a ×2 band around it, and its
    ``named`` widest gaps labelled. ``colour`` maps a point's label to a group
    (a production step, say), one colour per group with a legend. Points must
    be positive: a log scale draws neither zero nor a credit.
    """
    from matplotlib import colormaps
    from matplotlib.figure import Figure
    from matplotlib.lines import Line2D

    columns = 5
    rows = -(-len(points_by_axis) // columns)
    groups = sorted(set(colour.values())) if colour else []
    palette = dict(zip(groups, colormaps["tab10"].colors))
    fig = Figure(figsize=(5 * columns, 5 * rows))
    axes = fig.subplots(rows, columns, squeeze=False)
    for ax in axes.flat[len(points_by_axis) :]:
        ax.set_visible(False)
    for ax, (axis, points) in zip(axes.flat, points_by_axis.items()):
        ax.set_title(axis, fontsize=9)
        if not points:
            continue
        xs, ys = [p.left for p in points], [p.right for p in points]
        colours = [palette[colour[p.label]] for p in points] if colour else None
        ax.scatter(xs, ys, c=colours, s=8, alpha=0.7)
        for rank, p in enumerate(widest(points, named)):
            ax.annotate(f"{p.label[:40]} {fold(p.right / p.left)}", (p.left, p.right), xytext=(4, 3 + 10 * rank), textcoords="offset points", fontsize=6)
        lo, hi = min(*xs, *ys), max(*xs, *ys)
        ax.fill_between([lo, hi], [lo / 2, hi / 2], [lo * 2, hi * 2], color="grey", alpha=0.15, lw=0)
        ax.fill_between([lo, hi], [lo * 0.9, hi * 0.9], [lo * 1.1, hi * 1.1], color="grey", alpha=0.3, lw=0)
        ax.plot([lo, hi], [lo, hi], color="black", lw=0.5)
        ax.set(xscale="log", yscale="log")
    axes[0, 0].set(xlabel=labels[0], ylabel=labels[1])
    if palette:
        handles = [Line2D([], [], marker="o", ls="", color=c, label=g) for g, c in palette.items()]
        fig.legend(handles=handles, loc="upper center", ncol=len(palette))
        fig.subplots_adjust(top=0.95)
    fig.savefig(path, bbox_inches="tight")
