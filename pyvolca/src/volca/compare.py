"""Diff two activities side-by-side.

Composed from two ``/aggregate`` calls and a client-side merge. Groups by
``flow_id`` (UUID) by default, not by flow name, because flow names vary
slightly across variants / locales / versions while flow IDs are stable.

Replaced by :meth:`Client.compare_activities`, which compares the exchanges in
the engine, role, unit and tolerance included, and across two databases.
"""

from __future__ import annotations

import warnings
from dataclasses import dataclass, field
from typing import TYPE_CHECKING

from ._compat import RENAMED_REMOVED_IN
from .types import AggregateGroup

if TYPE_CHECKING:
    from .client import Client


@dataclass
class ActivityDiffRow:
    """One matched or unmatched flow in an activity comparison."""

    key: str                 # flow_id (or the group_by key used)
    left: float | None       # quantity in the left activity; None if only on right
    right: float | None      # quantity in the right activity; None if only on left
    unit: str | None

    @property
    def delta(self) -> float:
        """right - left (0 if one side is missing)."""
        return (self.right or 0.0) - (self.left or 0.0)


@dataclass
class ActivityDiff:
    """Result of ``compare_activities``."""

    scope: str
    group_by: str
    matched: list[ActivityDiffRow] = field(default_factory=list)
    left_only: list[ActivityDiffRow] = field(default_factory=list)
    right_only: list[ActivityDiffRow] = field(default_factory=list)


def compare_activities(
    client: "Client",
    pid_left: str,
    pid_right: str,
    *,
    scope: str = "direct",
    group_by: str = "flow_id",
    is_input: bool | None = True,
    **aggregate_kwargs: object,
) -> ActivityDiff:
    """Diff two activities by flow_id (default) at the requested scope.

    Returns three lists:
    - ``matched``: flows present in both activities (with left, right, delta).
    - ``left_only``: flows present only in the left activity.
    - ``right_only``: flows present only in the right activity.

    Default ``is_input=True`` restricts the comparison to inputs, which is the
    common case for "what does this variant consume differently?". Pass
    ``is_input=None`` to include outputs as well.
    """
    warnings.warn(
        "volca.compare_activities() is replaced by Client.compare_activities(), "
        "which compares the exchanges in the engine, role, unit and tolerance "
        "included, and across two databases. volca.compare_activities() keeps "
        f"working until pyvolca {RENAMED_REMOVED_IN} removes it.",
        DeprecationWarning,
        stacklevel=2,
    )
    left = client.aggregate(
        pid_left,
        scope=scope,
        group_by=group_by,
        is_input=is_input,
        **aggregate_kwargs,  # type: ignore[arg-type]
    )
    right = client.aggregate(
        pid_right,
        scope=scope,
        group_by=group_by,
        is_input=is_input,
        **aggregate_kwargs,  # type: ignore[arg-type]
    )

    left_map: dict[str, AggregateGroup] = {g.key: g for g in left.groups}
    right_map: dict[str, AggregateGroup] = {g.key: g for g in right.groups}

    matched: list[ActivityDiffRow] = []
    left_only: list[ActivityDiffRow] = []
    right_only: list[ActivityDiffRow] = []

    all_keys = set(left_map) | set(right_map)
    for key in sorted(all_keys):
        lg = left_map.get(key)
        rg = right_map.get(key)
        unit = (lg.unit if lg else None) or (rg.unit if rg else None)
        if lg is not None and rg is not None:
            matched.append(ActivityDiffRow(key=key, left=lg.quantity, right=rg.quantity, unit=unit))
        elif lg is not None:
            left_only.append(ActivityDiffRow(key=key, left=lg.quantity, right=None, unit=unit))
        else:
            assert rg is not None
            right_only.append(ActivityDiffRow(key=key, left=None, right=rg.quantity, unit=unit))

    return ActivityDiff(
        scope=scope,
        group_by=group_by,
        matched=matched,
        left_only=left_only,
        right_only=right_only,
    )
