"""Compatibility policy: the single source of truth, both ways round.

Two promises live here. Towards the engine, pyvolca speaks a range of revisions
of the JSON wire format; the engine advertises its own revision as
``wireVersion`` on ``/api/v1/version``, and this module owns the comparison: the
oldest wire this client accepts, the newest it understands, the engine hint
shown when the check fails, and the check itself.

Towards the code that imports pyvolca, a published name is not free to move. A
name that changes under a working script costs its author an afternoon of edits
with nothing to point at, so a rename ships the new name and keeps the old one
working as an alias that says what replaced it. :func:`warn_renamed` writes that
sentence, and :data:`RENAMED_REMOVED_IN` says when the old name goes away.

It is deliberately import-cheap (only the stdlib at runtime) so the release
preflight can read :data:`MIN_ENGINE_HINT` without pulling in ``requests`` or
the client, and so importing it can never cycle back through the client.
"""

from __future__ import annotations

import os
import warnings
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from .types import ServerVersion

REQUIRED_WIRE = 2
"""The oldest JSON wire-format revision this pyvolca accepts. Everything in
the client works against it except the revision-gated capabilities (see
``Client._require_wire``), which check the engine's advertised revision
before sending anything."""

KNOWN_WIRE = 23
"""The newest wire revision this pyvolca understands (revision 23 added the
``supplier_claim`` an exchange carries, how its source designated its supplier
before linking answered, and the supplier ambiguities a database setup report
carries; revision 22 added the ``native_id`` an activity carries, the
identifier its source file gave the dataset block it was read from; revision 21
added the ``block`` an activity summary carries and the ``block_products``
beside it; revision 20 added the ``derive_database`` route and the
``allocation`` and ``source`` a database status carries; revision 19 lets a flow
search name several kinds at once, comma-separated, so the biosphere-and-waste
bucket the search counts report is listable in one call; revision 18 added
``properties`` on a technosphere exchange, the dry and wet mass its source
states of that line; revision 17 added
``count_search_matches``, how many processes, products and flows one query
matches; revision 16 added ``producer_count`` on a flow search result, the
number of activities that make it, and the ``role`` parameter asking a
flow's activities for one side of it only; revision 15 added
``mass_allocation_percent`` on a product of a multi-output block, the share it would
carry if the allocation key were its mass; revision 14 added the
``AvoidedProduct`` exchange role, the ``unallocated`` quality check and the
``share`` and ``classification`` fields of a technosphere exchange; revision
13 added the compartment an unmapped factor of the mapping report carries;
revision 12 added the node type a tree export reports where a link names a
row no loaded database holds; revision 11 added the
data bundle's version on the version route; revision 10 added the role
a waste line reports; revision 9 added the kind
a flow search reports and filters on; revision 8 added the two
quality reports as downloadable CSV; revision 7 added editing an activity's
exchanges; revision 6 added explain_cf and the match_kind field on flow
contributions; revision 5 added writing activities; revision 4 added the
quality-report routes). An engine advertising more is newer than this client
and may answer shapes it cannot decode."""

RENAMED_REMOVED_IN = "1.0"
"""The release that drops the names :func:`warn_renamed` warns about. A retired
name lives at least until then, and never disappears in the release that
introduces its replacement."""


def warn_renamed(old: str, new: str) -> None:
    """Warn that ``old`` is now ``new``, and say when ``old`` stops working.

    ``stacklevel=3`` reports the caller's own line rather than a line inside
    pyvolca, which is what makes the warning visible from a script at all: the
    interpreter shows a :class:`DeprecationWarning` only when it is raised from
    ``__main__``.
    """
    warnings.warn(
        f"{old}() is now {new}(), which takes the same arguments. "
        f"{old}() keeps working until pyvolca {RENAMED_REMOVED_IN} removes it.",
        DeprecationWarning,
        stacklevel=3,
    )


MIN_ENGINE_HINT = "0.9.1"
"""First engine release that advertises :data:`REQUIRED_WIRE`. Used only for the
error message and the release preflight; it is not, by itself, a runtime gate
(the engine's ``wireVersion`` is)."""


def check(sv: ServerVersion) -> None:
    """Raise :class:`VoLCAError` if the engine's wire is too old; warn if newer.

    The opt-out is read here, not in the caller, so every entry point,
    the client's first operation and :meth:`volca.server.Server.start` alike,
    honours it. It is deliberately noisy: silencing the check is a foot-gun.
    """
    if os.environ.get("VOLCA_SKIP_COMPAT_CHECK"):
        warnings.warn(
            "VOLCA_SKIP_COMPAT_CHECK set, skipping the engine "
            "wire-compatibility check."
        )
        return

    w = sv.wire_version
    if w is None or w < REQUIRED_WIRE:
        from .client import VoLCAError  # deferred: keep this module client-free

        spoken = "pre-1" if w is None else str(w)
        raise VoLCAError(
            f"This engine (v{sv.version}) speaks an older wire than pyvolca "
            f"needs (wire {spoken} < {REQUIRED_WIRE}). Upgrade the engine to "
            f">= v{MIN_ENGINE_HINT}, or `pip install 'pyvolca<0.7.2'`."
        )
    if w > KNOWN_WIRE:
        warnings.warn(
            f"Engine v{sv.version} speaks wire {w}; this pyvolca knows up to "
            f"wire {KNOWN_WIRE}. Some responses may not decode; upgrade "
            "pyvolca."
        )
