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

KNOWN_WIRE = 104
"""The newest wire revision this pyvolca understands (revision 104 added the
public name ``by`` of the account that made each change of a database's or a
collection's history; revision 103 added
``available_method_collections()`` and ``install_method_collection()``, the
method collections an engine offers to download and the call that installs
and loads one; revision 102 added the
``basis`` of a score to its contributing flows and activities and to the
baseline of a sensitivity sweep; revision 101 added the
``flows_from`` of ``upload_method_collection()``, the EcoSpold 2 database whose
master data gives a workbook of factors its units and CAS numbers; revision 100 added the
``database`` each line of a database's history was made on, its own or the
parent a child is divided from; revision 99 added the
``basis`` of a score, what it rests on: the engine, the databases read and
how each differs from its release, the method collection, and what the
request substituted and set; revision 98 added the
``parameters`` of ``system_model()``, each method parameter a model sets with
the processes of the parent it moves; revision 97 added the
``probable_misses`` of a score, the flows it read no factor for although its
method writes their substance elsewhere in their medium; revision 96 added the
delivery-sheet row of one process of a child, read by the web interface and not
by this client; revision 95 added the
``usage`` of a licence and of the terms to accept, the counting of computations
the terms ask for their publisher; revision 94 added
``set_description()``, which describes an uploaded database or clears its
description; revision 93 added the rows of a reference table, read by the web
interface and not by this client; revision 92 added
``set_display_name()``, which renames an uploaded database; revision 91 added the
``substitutions`` of ``system_model()``, each coproduct a model credits against
a process of the parent rather than divides; revision 90 added
``DatabaseInfo.added``, the moment the engine made a database; revision 89 added the
``set_wet_masses`` of ``edit_exchanges()``, which states the wet mass of a
product output, its density when it is in litres or cubic metres; revision 88
added the ``"SameCutNames"`` rung of a database comparison; revision 87 added
the ``set_dry_masses`` of ``edit_exchanges()``, which states the dry mass of a
product output; revision 86 added the ``unchanged`` pairs of a database
comparison and ``DatabaseComparison.pairs()``; revision 85 made
``derive_database()`` make a parent or a child divided by a system model, and
added ``system_model()``; revision 84 added
the ``set_prices`` of ``edit_exchanges()``, which prices a product output;
revision 83 added
the ``"none"`` allocation key ``derive_database()`` accepts, which keeps a
block of several products whole; revision 82 added
the ``price`` among the ``properties`` of a technosphere exchange, and the
``"price"`` allocation key ``derive_database()`` accepts; revision 81 added
the download of a unit table, which ``download_units()`` and
``unit_table()`` read; revision 80 added
the reference amount and unit of each batch score, which
``ScoredActivity.product_amount`` and ``product_unit`` read, and of each
catalogue entry; revision 79 added
the reason a write gives in ``note=``, the history ``database_history()``
reads, and ``DatabaseInfo.modification``; revision 78 added
the terms of use a database asks its readers to accept, which
``terms_to_accept()`` lists and ``accept_terms()`` accepts; revision 77 added
the scenarios a database ships, which ``Scenario.shipped`` marks; revision 76 added
the ``selector`` of each exchange, and the selectors naming an input or a
waste output by its flow and location; revision 75 added
the scenarios of a database, which ``scenarios()``, ``save_scenario()``
and ``drop_scenario()`` list, save and drop; revision 74 added
the database's own parameters, which ``database_parameters()`` reads and
``edit_database_parameters()`` edits; revision 73 added
the formulas and parameters ``edit_exchanges()`` sets and removes; revision 72 added
the formula language, which ``formula_language()`` reads, and the formula
check, which ``check_formula()`` asks for; revision 71 added
the ``NAME_ALONE`` supplier claim; revision 70 added
what each line of an activity adds to its scores, which
``get_impacts_batch(exchange_contributions=True)`` asks for, and the
``position`` of each exchange; revision 69 added
the reduced-model route, which ``reduced_model()`` reads; revision 68 added
the ``parameter_sets`` of a batch score and the ``parameter_set`` of each
row; revision 67 added
the ``flow`` and ``limit`` of an inventory request and the
``matched_flows`` it counts; revision 66 added
the ``parameters`` a scoring, inventory or supply chain request sets the
database at; revision 65 added
the ``parameters`` and ``database_parameters`` of an activity, with the
uncertainty of an input parameter; revision 64 added the ``formula`` an
exchange carries; revision 63 added the ``waterBalance`` check of the quality
report, which this client hands over as it reads it; revision 62 added
the relink mapping a mapping page writes, read as it is: rows keyed by the
supplier process an input names, suppliers pinned by identity, a conversion
between units of different measures, and the ``alias_conflict`` and
``alias_declined`` reasons of a gap report; revision 61 added
the ``openlca`` export format, a database written as an openLCA package;
revision 60 added
``apply_changes``, which makes an activity say each change a comparison
wrote that it does not say yet; revision 59 added
the ``openlca-package`` database format, a format name pyvolca passes
through as it reads it; revision 58 added
the ``cutoff_inputs`` of a result, the unsupplied inputs its calculation
counted as zero; revision 57 added
``changes_present``, whether an activity already holds each change a
comparison wrote, and the description and supplier changes a comparison
reports; revision 56 added
the ``name``, ``location`` and ``description`` ``edit_exchanges`` restates;
revision 55 added
``get_flow_factors``, every factor the loaded collections give one flow, and
the ``method_id`` of an ``explain_cf`` answer; revision 54 added
the ``heldAs`` of a ``usage`` line; revision 53 added
the usage log, read with ``usage`` and forgotten with ``forget_usage``;
revision 52 added
``default_method_collection``, the collection offered first; revision 51 added
a failed load, copy or upload answered with an HTTP error, which this client
raises on either way; revision 50 added
uploading a package as one, the ``requiredReleases`` of a setup and
``accept_substitution``; revision 49 added
the ``package`` of ``export_database``; revision 48 added
the ``release`` of a database, which ``set_release`` declares, and the
``systemModels`` of its setup; revision 47 added
the edges a supply chain draws between databases, and its
``withheld_inputs``; revision 46 added
the ``withheld_databases`` a dependency's licence groups its part in;
revision 45 added
reading a collection's scoring sets as rows and changing them, and the
``scoring_rows`` of an activity's score; revision 44 added
what a database's licence withholds from an activity and a score;
revision 43 added
adding, renaming, changing the unit of and removing a copy's categories;
revision 42 added
copying a method collection, changing the factors of a copy, undoing a change
and reading its history, and the flows a collection characterizes; revision 41 added
the ``licence`` of a database's setup and of its entry in the list, which ``set_licence`` changes, and ``licences``; revision 40 added
the ``files`` a literature entry ships, read by ``document_file``; revision 39 added
``process`` on the activity search, the rows of the processes named, in the
order named; revision 38 added the
``catalogue`` of a database, every process page by page with a fingerprint of
the list; revision 37 added the
single score of a SimaPro method's normalization-weighting set in
``scoring_results``, and the ``units`` of a scoring set's variables; revision 36 added the
``documentation`` of a database's setup, the export its file is and the
system descriptions and literature references it holds; revision 35 added the
``dates`` an activity carries and the ``dates`` entry an activity comparison
reports when two datasets state different ones; revision 34 added the
``reading`` of each method factor, its medium and where its location was read;
revision 33 added
``profile_method_collection``, what each impact category of a loaded method
collection holds; revision 32 added the
``category`` a method collection comparison can be narrowed to; revision 31 added
``compare_method_collections``, the comparison of two loaded method
collections, factor by factor; revision 30 added the
``max_batch_activities`` and ``max_top_flows`` the hosting route reports;
revision 29 added the
``collection`` query parameter the method routes take, and the 409 they answer
for a method UUID several loaded collections carry; revision 28 added the
contributing flows and activities of a single score, under
``score/{set}/{score}``, answered in the shapes an indicator's are; revision 27 added the
``regional`` match kind a contributing flow can carry, naming a factor the
method states per location, where the field used to be absent and its absence
meant no factor reached the flow at all; revision 26 added the
``supplier_activity`` a gap entry and a missing supplier carry, the activity
their inputs named, and the ``locations`` a missing supplier carries, the
locations those inputs stated; revision 25 added
``compare_activities`` and ``compare_databases``, the comparison of two
activities and of two loaded databases; revision 24 added the
``reasons`` a supplier-gap entry carries, every reason its product was refused
a supplier under, where ``reason`` and ``detail`` name only the first of them,
and made the setup report's missing-supplier list name a product once per
reason; revision 23 added the
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
