"""Helpers specific to Agribalyse processes.

Every helper here is database-specific (text matching on flow names, regex
on pfaDescription, pattern detection from input shapes). The volca core
engine knows nothing about any of this. To add Ginko/WFLDB support, create
sibling modules — do not factor.

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
    """Reverse-engineered transformation parameters for one Agribalyse process."""

    pattern: str                               # "wrapper_wfldb" | "direct" | "layered"
    raw_material_name: str | None
    raw_material_kg: float
    electricity_kwh: float
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


def parse_allocation(description: list[str]) -> Allocation | None:
    """Parse allocation factors from a process's pfaDescription paragraphs.

    Returns ``None`` when no allocation block is found (genuine mono-product
    processes) or when the text cannot be parsed.
    """
    text = " ".join(description or [])
    if not _ALLOC_KEYWORD_RE.search(text):
        return None

    # Method (optional)
    method: str | None = None
    m = _ALLOC_METHOD_RE.search(text)
    if m:
        method = m.group(1).strip().lower()

    # Extract "name NN%" pairs. Normalise whitespace in names.
    factors: dict[str, float] = {}
    for name, pct in _FACTOR_RE.findall(text):
        clean = " ".join(name.split()).lower()
        # Skip false positives like "100 %" on its own by requiring a name
        # at least 3 chars.
        if len(clean) < 3:
            continue
        factors[clean] = round(float(pct) / 100.0, 4)

    # A "100%" single factor is essentially no allocation — skip.
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
        if e.target_activity and "WFLDB" in e.target_activity
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
    call this — it issues targeted ``/aggregate`` queries instead.
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
    """Return ``(electricity_kWh, heat_MJ)`` summed over level-0 inputs.

    Two /aggregate calls, nothing more.
    """
    elec = client.aggregate(
        process_id,
        scope="direct",
        is_input=True,
        filter_name="Electricity",
        filter_unit="kWh",
    ).filtered_total
    heat = client.aggregate(
        process_id,
        scope="direct",
        is_input=True,
        filter_name="Heat",
        filter_unit="MJ",
    ).filtered_total
    return elec, heat


_UNIT_TO_KG = {"kg": 1.0, "ton": 1000.0, "g": 0.001}


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
            ClassificationFilter("Category type", "processing", "exact"),
        ],
    ).entries
    real = [e for e in ops if not e.name.startswith("[Dummy]")]
    dummy_op = not real and any(e.name.startswith("[Dummy]") for e in ops)
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
    target_pattern = pattern
    dummy_op = False
    operation_name: str | None = None
    if pattern == "wrapper_wfldb":
        wfldb_e = next(
            (
                e for e in act.technosphere_inputs
                if e.target_activity and "WFLDB" in e.target_activity
            ),
            None,
        )
        if wfldb_e and wfldb_e.target_process_id:
            target_pid = wfldb_e.target_process_id
            target_act = client.get_activity(target_pid)
            target_description = target_act.description
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
    if pattern == "layered":
        operations, dummy_op = _find_layered_operations(client, process_id)
        raw_entry = _find_layered_raw(client, process_id)
        if raw_entry is not None:
            raw_name = raw_entry.name
            raw_kg = _kg_equiv(raw_entry)
        if operations:
            operation_name = "; ".join(o.name for o in operations)

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
        ).filtered_total

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
                ).filtered_total
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
        raw_kg = raw.groups[0].quantity if raw.groups else 0.0

    # Technosphere outputs only — the /aggregate primitive lumps biosphere
    # emissions (NOx, water, heat waste…) into "direct" exchanges, so walk
    # the typed exchange list and discriminate by class instead.
    grouped: dict[tuple[str, str], float] = {}
    for e in act.exchanges:
        if (
            isinstance(e, TechnosphereExchange)
            and not e.is_input
            and not e.is_reference
        ):
            key = (e.flow_name, e.unit)
            grouped[key] = grouped.get(key, 0.0) + e.amount
    co_products = [(name, qty, unit) for (name, unit), qty in grouped.items()]

    if dummy_op:
        elec_kwh = heat_mj = tap_water_kg = wastewater_m3 = biowaste_kg = 0.0
    else:
        elec_kwh = agg_total(filter_name="Electricity", filter_unit="kWh")
        heat_mj = agg_total(filter_name="Heat", filter_unit="MJ")
        tap_water_kg = agg_total(filter_name="Tap water", filter_unit="kg")
        wastewater_m3 = agg_total(filter_name="Wastewater", filter_unit="m3")
        biowaste_kg = agg_total(filter_name="Biowaste", filter_unit="kg")

    return Decomposition(
        pattern=pattern,
        raw_material_name=raw_name,
        raw_material_kg=raw_kg,
        electricity_kwh=elec_kwh,
        heat_mj=heat_mj,
        tap_water_kg=tap_water_kg,
        wastewater_m3=wastewater_m3,
        biowaste_kg=biowaste_kg,
        co_products=co_products,
        allocation=parse_allocation(target_description),
        dummy_op=dummy_op,
        operation_name=operation_name,
    )


def is_allocated(activity: ActivityDetail) -> bool:
    """True iff an activity's description contains a parseable allocation block."""
    return parse_allocation(activity.description) is not None
