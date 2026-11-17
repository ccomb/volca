"""Helpers specific to Agribalyse processes.

Every helper here is database-specific (text matching on flow names, regex
on pfaDescription, pattern detection from input shapes). The volca core
engine knows nothing about any of this. To add Ginko/WFLDB support, create
sibling modules; do not factor.

The split:
- Pure helpers (no Client) operate on already-loaded data: ``parse_allocation``,
  ``detect_pattern``, ``classify_exchange``.
- Client-using helpers go through the generic ``/aggregate`` primitive:
  ``decompose``, ``get_direct_energy``.

Call ``decompose(client, pid)`` to reverse-engineer a transformation process
in one shot.
"""

from __future__ import annotations

import re
from dataclasses import dataclass, field
from typing import TYPE_CHECKING

from .types import (
    ActivityDetail,
    ClassificationFilter,
    Exchange,
    MatchMode,
    SupplyChainEntry,
    TechnosphereExchange,
)

if TYPE_CHECKING:
    from .client import Client


# ---------------------------------------------------------------------------
# Data
# ---------------------------------------------------------------------------


@dataclass
class Allocation:
    """Allocation factors parsed from an activity's description text."""

    method: str | None
    factors: dict[str, float]


@dataclass
class Decomposition:
    """Reverse-engineered transformation parameters for one Agribalyse process.

    Every amount is for one unit of the process's reference product, whatever
    amount the dataset declares: what ``/aggregate`` counts, and the coproducts
    divided the same way.
    """

    pattern: str                               # "wrapper_wfldb" | "direct" | "layered"
    raw_material_name: str | None
    raw_material_kg: float
    electricity_mj: float
    heat_mj: float
    tap_water_kg: float
    wastewater_m3: float
    biowaste_kg: float
    co_products: list[tuple[str, float, str]] = field(default_factory=list)
    allocation: Allocation | None = None
    # True when a layered process is shimmed onto a [Dummy] operation stub
    # (Agribalyse placeholder with no exchanges); energies are forced to 0.
    dummy_op: bool = False
    operation_name: str | None = None          # transformation step name, for debug
    # True for fully-allocated by-product activities (residues, bran, …)
    # whose upstream inputs are all zero because 100% of the mass/impact is
    # attributed to a sibling main product elsewhere in Agribalyse.
    is_byproduct: bool = False


# ---------------------------------------------------------------------------
# Pure helpers
# ---------------------------------------------------------------------------


# Known allocation phrasings in Agribalyse pfaDescription:
#   "Allocation: butter 33%, skimmed milk 63%, buttermilk 4%"
#   "Allocation method: dry matter"
#   "butter: 33%, skimmed milk: 63%"
# Parser is deliberately conservative: unknown phrasings return None rather
# than guessing.
_ALLOC_KEYWORD_RE = re.compile(r"allocat", re.IGNORECASE)
_ALLOC_METHOD_RE = re.compile(
    r"allocation\s*method[:\s]+([^.;,\n]+)", re.IGNORECASE
)
_FACTOR_RE = re.compile(
    r"([A-Za-z][A-Za-z0-9 _\-]+?)\s*[:=]?\s*([0-9]+(?:\.[0-9]+)?)\s*%"
)

# Grammatical words that the greedy factor regex would otherwise pick up as
# co-product names from fragments like "… and 25%", "less than 10%",
# "allocation is 100%". Kept in lowercase to match the normalised key.
_FACTOR_STOPWORDS = {
    "and", "or", "of", "is", "are", "the", "a", "an", "with", "without",
    "less", "less than", "more", "more than", "allocation", "approximately",
    "about", "around", "by", "to", "from", "in", "at", "than",
}


def parse_allocation(description: list[str]) -> Allocation | None:
    """Parse the allocation block from a process's description paragraphs.

    The authoritative per-product shares live on the structured
    ``ActivityDetail.all_products`` (``allocation_percent``); this text parse
    is kept for the human-readable allocation *method* (dry matter, economic,
    …), which the wire doesn't carry, and as a fallback for older databases
    without structured shares. Returns ``None`` when no allocation block is
    found (genuine mono-product processes) or when the text can't be parsed.
    """
    text = " ".join(description or [])
    if not _ALLOC_KEYWORD_RE.search(text):
        return None

    # Method (optional)
    method: str | None = None
    m = _ALLOC_METHOD_RE.search(text)
    if m:
        method = m.group(1).strip().lower()

    # Extract "name NN%" pairs. Normalise whitespace in names, and drop the
    # tail word if it's a stopword ("... solid waste treatment is 50%" →
    # key "solid waste treatment", not "… is").
    factors: dict[str, float] = {}
    for name, pct in _FACTOR_RE.findall(text):
        clean = " ".join(name.split()).lower()
        while clean and clean.rsplit(" ", 1)[-1] in _FACTOR_STOPWORDS:
            clean = clean.rsplit(" ", 1)[0] if " " in clean else ""
        if len(clean) < 3 or clean in _FACTOR_STOPWORDS:
            continue
        factors[clean] = round(float(pct) / 100.0, 4)

    # A "100%" single factor is essentially no allocation, so skip.
    if not factors or (len(factors) == 1 and list(factors.values())[0] >= 0.99):
        return None
    return Allocation(method=method, factors=factors)


def detect_pattern(activity: ActivityDetail) -> str:
    """Return one of ``"wrapper_wfldb"``, ``"direct"``, ``"layered"``.

    Heuristic:
      - Exactly one technosphere input whose target_activity contains "WFLDB"
        → wrapper_wfldb (the real work happens inside the WFLDB sub-process).
      - Otherwise, if level-0 has direct energy/water exchanges (electricity,
        heat, tap water) → direct.
      - Otherwise → layered (the real work is inside a consumption mix
        sub-process and must be walked via get_supply_chain).
    """
    tech_inputs = activity.technosphere_inputs

    wfldb_inputs = [
        e for e in tech_inputs
        if e.target_activity_name and "WFLDB" in e.target_activity_name
    ]
    if len(wfldb_inputs) == 1:
        return "wrapper_wfldb"

    has_direct_energy = any(
        classify_exchange(e) in ("electricity", "heat", "water")
        for e in activity.inputs
    )
    if has_direct_energy:
        return "direct"
    return "layered"


def classify_exchange(exchange: Exchange) -> str:
    """Classify an exchange by its functional role in an Agribalyse process.

    Returns one of: electricity, heat, water, wastewater, biowaste, transport,
    cleaning, infrastructure, raw_material, other.

    Used as a building block for ad-hoc inspection. ``decompose`` does not
    call this: it issues targeted ``/aggregate`` queries instead.
    """
    name = (exchange.flow_name or "").lower()
    unit = (exchange.unit or "").lower()

    if "electricity" in name and unit == "kwh":
        return "electricity"
    if "heat" in name and unit == "mj":
        return "heat"
    if "wastewater" in name or "waste water" in name:
        return "wastewater"
    if "tap water" in name or "water, deionised" in name or name.startswith("water, "):
        return "water"
    if "biowaste" in name or "bio-waste" in name:
        return "biowaste"
    if "transport" in name or unit in ("tkm", "kgkm"):
        return "transport"
    if "cleaning" in name or "detergent" in name:
        return "cleaning"
    if "factory" in name or "plant construction" in name or unit == "unit":
        return "infrastructure"
    if (
        isinstance(exchange, TechnosphereExchange)
        and exchange.is_input
        and unit == "kg"
    ):
        return "raw_material"
    return "other"


# ---------------------------------------------------------------------------
# Helpers that call the client (via /aggregate)
# ---------------------------------------------------------------------------


def get_direct_energy(client: "Client", process_id: str) -> tuple[float, float]:
    """Return ``(electricity_kWh, heat_MJ)`` summed over level-0 inputs, for one
    unit of the process's reference product (the basis ``/aggregate`` counts in,
    whatever amount the process declares).

    Two /aggregate calls, nothing more.
    """
    elec = client.aggregate(
        process_id,
        scope="direct",
        is_input=True,
        filter_name="Electricity",
        filter_unit="kWh",
    ).filtered_total_per_unit
    heat = client.aggregate(
        process_id,
        scope="direct",
        is_input=True,
        filter_name="Heat",
        filter_unit="MJ",
    ).filtered_total_per_unit
    return elec, heat


_UNIT_TO_KG = {"kg": 1.0, "ton": 1000.0, "g": 0.001}


def _per_unit_divisor(activity: ActivityDetail) -> float:
    """What the engine divides an activity's exchanges by to count them for one
    unit of its reference product: the magnitude of the amount the dataset
    declares, one when it declares none or zero.

    ``/aggregate`` answers per unit in every scope, while the exchanges of
    ``get_activity`` are as declared, so a figure read off the exchanges needs
    this divisor to sit beside one read from ``/aggregate``.
    """
    return abs(activity.product_amount or 0.0) or 1.0


def _kg_equiv(entry: SupplyChainEntry) -> float:
    """Normalise a supply-chain entry's scaling factor to kg-equivalent.

    Agribalyse mixes reference units across activities (crop farms are modelled
    in tons, food processes in kg). Comparing raw scaling factors across units
    would always favour kg-based entries. Returns 0 for non-mass units so they
    drop out of max() selections.
    """
    return entry.scaling_factor * _UNIT_TO_KG.get(entry.unit, 0.0)


def _find_layered_operations(
    client: "Client", process_id: str
) -> tuple[list[SupplyChainEntry], bool]:
    """Locate the transformation operations of a layered Agribalyse process.

    Returns (operations, dummy_op). Looks only at the wrapper's direct
    technosphere inputs (max_depth=1) and filters server-side by
    ``Category type = processing``. Wrappers like "apple pomace" chain
    multiple parallel sub-operations (washing, extracting, pasteurizing…),
    so we return the full list and let the caller sum energies across them.
    Excludes ``[Dummy]`` placeholders from the returned list but signals
    their presence via ``dummy_op=True``.
    """
    ops = client.get_supply_chain(
        process_id,
        max_depth=1,
        classification_filters=[
            ClassificationFilter("Category type", "processing", MatchMode.EXACT),
        ],
    ).entries
    real = [e for e in ops if not e.activity_name.startswith("[Dummy]")]
    dummy_op = not real and any(e.activity_name.startswith("[Dummy]") for e in ops)
    return real, dummy_op


def _find_layered_raw(
    client: "Client", process_id: str
) -> SupplyChainEntry | None:
    """Pick the dominant farm-gate raw material of a layered process.

    Uses the server-side ``raw`` preset (Agricultural\\...\\Transformation OR-set),
    so consumption-mix intermediaries are skipped automatically. Falls back to
    None if the chain has no raw material (rare; typically happens for pure
    processing stubs).
    """
    raws = client.get_supply_chain(process_id, preset="raw").entries
    return max(raws, key=_kg_equiv, default=None) if raws else None


def decompose(client: "Client", process_id: str) -> Decomposition:
    """Reverse-engineer transformation parameters for an Agribalyse process.

    Orchestrator over ~10 ``/aggregate`` calls plus one ``get_activity`` for
    the description, pattern detection and co-products enumeration.

    For wrapper_wfldb processes, descends once into the WFLDB sub-process and
    re-runs the aggregate queries there. For layered processes, walks the
    supply chain 2 hops deep.
    """
    act = client.get_activity(process_id)
    pattern = detect_pattern(act)

    # Pattern A: descend into the WFLDB sub-process; aggregates run there and
    # the description (with allocation factors) lives there too. The WFLDB
    # core may itself be flat (direct energy inputs) or layered (only
    # sub-processes), so we re-detect once after descending.
    target_pid = process_id
    target_description = act.description
    alloc_act = act  # detail that owns the allocation shares (all_products)
    target_pattern = pattern
    dummy_op = False
    operation_name: str | None = None
    if pattern == "wrapper_wfldb":
        wfldb_e = next(
            (
                e for e in act.technosphere_inputs
                if e.target_activity_name and "WFLDB" in e.target_activity_name
            ),
            None,
        )
        if wfldb_e and wfldb_e.target_process_id:
            target_pid = wfldb_e.target_process_id
            target_act = client.get_activity(target_pid)
            target_description = target_act.description
            alloc_act = target_act
            target_pattern = detect_pattern(target_act)

    # Pattern C: for layered processes, point the aggregate queries at the
    # real transformation operations rather than the wrapper. Walking the
    # wrapper's own supply chain would pull in upstream farm-gate energy.
    # Wrappers may chain several parallel processing sub-steps (washing,
    # pasteurizing, concentrating…), so we sum across all of them weighted
    # by each sub-step's scaling factor.
    raw_name: str | None = None
    raw_kg: float = 0.0
    operations: list[SupplyChainEntry] = []
    is_byproduct = False
    if pattern == "layered":
        operations, dummy_op = _find_layered_operations(client, process_id)
        raw_entry = _find_layered_raw(client, process_id)

        # Fully-allocated by-products (e.g. "Alaska pollock, residues") have
        # every technosphere input at amount=0 because 100% of the upstream
        # mass/impact is attributed to the sibling main product. The preset
        # walk stops on zero-amount edges, so raw_entry is None. Recover the
        # raw from a non-[Dummy] parent via one extra preset walk.
        if raw_entry is None and all(
            e.amount == 0.0 for e in act.technosphere_inputs
        ) and act.technosphere_inputs:
            is_byproduct = True
            parent = next(
                (
                    e for e in act.technosphere_inputs
                    if e.target_process_id
                    and not (e.target_activity_name or "").startswith("[Dummy]")
                ),
                None,
            )
            if parent and parent.target_process_id:
                raw_entry = _find_layered_raw(client, parent.target_process_id)

        if raw_entry is not None:
            raw_name = raw_entry.activity_name
            raw_kg = _kg_equiv(raw_entry)
        if operations:
            operation_name = "; ".join(o.activity_name for o in operations)

    if target_pattern == "layered":
        sc_scope, sc_depth = "supply_chain", 2
    else:
        sc_scope, sc_depth = "direct", None

    def agg_total_single(pid: str, **kw: object) -> float:
        return client.aggregate(
            pid,
            scope=sc_scope,
            max_depth=sc_depth,
            is_input=True,
            **kw,  # type: ignore[arg-type]
        ).filtered_total_per_unit

    def agg_total(**kw: object) -> float:
        if pattern == "layered" and operations:
            # Sum each operation's per-unit contribution, weighted by its
            # scaling factor in the wrapper's chain.
            return sum(
                op.scaling_factor
                * client.aggregate(
                    op.process_id,
                    scope="supply_chain",
                    max_depth=2,
                    is_input=True,
                    **kw,  # type: ignore[arg-type]
                ).filtered_total_per_unit
                for op in operations
            )
        return agg_total_single(target_pid, **kw)

    if pattern != "layered":
        raw = client.aggregate(
            target_pid,
            scope="direct",
            is_input=True,
            filter_unit="kg",
            filter_name_not=["Tap water", "Wastewater", "Biowaste"],
            group_by="name",
            aggregate="sum_quantity",
        )
        raw_name = raw.groups[0].key if raw.groups else None
        raw_kg = raw.groups[0].quantity_per_unit if raw.groups else 0.0

    # Technosphere outputs only: the /aggregate primitive lumps biosphere
    # emissions (NOx, water, heat waste…) into "direct" exchanges, so walk
    # the typed exchange list and discriminate by class instead.
    per_unit = _per_unit_divisor(act)
    grouped: dict[tuple[str, str], float] = {}
    for e in act.exchanges:
        if (
            isinstance(e, TechnosphereExchange)
            and not e.is_input
            and not e.is_reference
        ):
            key = (e.flow_name, e.unit)
            grouped[key] = grouped.get(key, 0.0) + e.amount / per_unit
    co_products = [(name, qty, unit) for (name, unit), qty in grouped.items()]

    # Agribalyse uses allocation-based modelling, so co-products rarely
    # appear as technosphere outputs; the engine records each co-product's
    # allocation share on all_products instead. Prefer that authoritative
    # split (allocation_percent); the description text is rounded and can
    # drift from the applied factor, so it's only a fallback for older
    # databases without structured shares. `allocation` is still parsed for
    # its human-readable method label (dry matter, economic, …).
    allocation = parse_allocation(target_description)
    if not co_products:
        co_products = [
            (p.product_name, p.allocation_percent / 100.0, "kg/kg")
            for p in alloc_act.all_products
            if p.allocation_percent is not None
            and p.process_id != alloc_act.process_id
        ]
    if not co_products and allocation is not None and allocation.factors:
        act_name_l = act.activity_name.lower()
        main_key = max(
            allocation.factors,
            key=lambda k: sum(1 for w in k.split() if w in act_name_l),
            default=None,
        )
        co_products = [
            (name, fraction, "kg/kg")
            for name, fraction in allocation.factors.items()
            if name != main_key
        ]

    if dummy_op:
        elec_mj = heat_mj = tap_water_kg = wastewater_m3 = biowaste_kg = 0.0
    else:
        elec_mj = agg_total(filter_name="Electricity", filter_unit="MJ")
        heat_mj = agg_total(filter_name="Heat", filter_unit="MJ")
        tap_water_kg = agg_total(filter_name="Tap water", filter_unit="kg")
        wastewater_m3 = agg_total(filter_name="Wastewater", filter_unit="m3")
        biowaste_kg = agg_total(filter_name="Biowaste", filter_unit="kg")

    return Decomposition(
        pattern=pattern,
        raw_material_name=raw_name,
        raw_material_kg=raw_kg,
        electricity_mj=elec_mj,
        heat_mj=heat_mj,
        tap_water_kg=tap_water_kg,
        wastewater_m3=wastewater_m3,
        biowaste_kg=biowaste_kg,
        co_products=co_products,
        allocation=allocation,
        dummy_op=dummy_op,
        operation_name=operation_name,
        is_byproduct=is_byproduct,
    )


def is_allocated(activity: ActivityDetail) -> bool:
    """True iff the activity splits its burden across several co-products.

    Prefers the structured allocation shares on ``all_products``
    (:attr:`ActivityDetail.is_allocated`), falling back to an allocation block
    in the description text for older Agribalyse databases that carry no
    structured shares, the same precedence :func:`decompose` uses to
    enumerate co-products.
    """
    return activity.is_allocated or parse_allocation(activity.description) is not None
