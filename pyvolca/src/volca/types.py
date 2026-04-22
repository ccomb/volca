"""Data types for VoLCA API responses."""

import dataclasses
import re
from dataclasses import dataclass, field
from typing import Any, ClassVar, Union


_CAMEL_BOUNDARY = re.compile(r"(?<!^)(?=[A-Z])")


def _to_snake(s: str) -> str:
    """camelCase → snake_case. Idempotent on already-snake strings."""
    return _CAMEL_BOUNDARY.sub("_", s).lower()


@dataclass
class FromJson:
    """Mixin: build the dataclass from a JSON dict by snake-casing keys.

    Picks only the keys that match declared fields. Subclasses with nested
    dataclass fields, recursive parsing, or envelope unwrapping should
    override `from_json`.
    """

    @classmethod
    def from_json(cls, d: dict) -> Any:
        names = {f.name for f in dataclasses.fields(cls)}
        return cls(**{k: v for k, v in ((_to_snake(k), v) for k, v in d.items()) if k in names})


@dataclass
class DatabaseInfo(FromJson):
    """One entry of :meth:`Client.list_databases`.

    ``depends_on`` names the databases this one links against for cross-DB
    flow resolution — mirrors the ``dependsOn`` list surfaced by the relink
    endpoint. Derived from the engine's declared topology, not runtime state.
    """

    name: str
    display_name: str
    status: str  # "unloaded" | "partially_linked" | "loaded"
    path: str
    load_at_startup: bool = False
    is_uploaded: bool = False
    activity_count: int = 0
    description: str | None = None
    format: str | None = None
    depends_on: list[str] = field(default_factory=list)


@dataclass
class ScoringIndicator(FromJson):
    """One per-variable entry inside ``LCIABatchResult.scoringIndicators``.

    ``value`` is pre-multiplied by the scoring set's ``displayMultiplier``
    (configured in the scoring TOML) and expressed in the set's display unit.
    ``category`` names the impact category the variable was resolved from.
    """

    category: str
    value: float


@dataclass
class ClassificationFilter:
    """Filter a supply-chain/consumers query by a classification (system, value, mode).

    Matches one classification system entry (e.g. ("Category", "Agricultural\\\\Food",
    "exact")). Mode is "exact" (case-insensitive equality) or "contains" (substring).
    Multiple filters are AND-combined by the server.
    """

    system: str
    value: str
    mode: str = "contains"


@dataclass
class Activity(FromJson):
    process_id: str
    name: str
    location: str
    product: str
    product_amount: float
    product_unit: str


@dataclass
class ConsumerResult(FromJson):
    """Activity that consumes a given supplier, with BFS depth."""
    process_id: str
    name: str
    location: str
    product: str
    product_amount: float
    product_unit: str
    depth: int  # hops from the queried supplier (1 = direct consumer)
    classifications: dict[str, str] = field(default_factory=dict)  # ISIC / CPC / Category, mirrors SupplyChainEntry


@dataclass
class SupplyChainEntry(FromJson):
    process_id: str
    name: str
    location: str
    quantity: float
    unit: str
    scaling_factor: float
    classifications: dict[str, str] = field(default_factory=dict)


@dataclass
class PathStep:
    """One step in the supply chain path returned by get_path_to.

    Note: the /path endpoint emits snake_case JSON directly (built via
    aeson's `object [...]` rather than generic ToJSON), so it bypasses
    the engine's stripLowerPrefix transform.
    """
    process_id: str
    name: str
    location: str
    unit: str
    cumulative_quantity: float
    scaling_factor: float
    local_step_ratio: float | None = None  # absent on root step

    @classmethod
    def from_json(cls, d: dict) -> "PathStep":
        return cls(
            process_id=d["process_id"],
            name=d["name"],
            location=d["location"],
            unit=d["unit"],
            cumulative_quantity=d["cumulative_quantity"],
            scaling_factor=d["scaling_factor"],
            local_step_ratio=d.get("local_step_ratio"),
        )


@dataclass
class PathResult:
    """Shortest upstream path from a root process to a matching activity."""
    path: list[PathStep]
    path_length: int
    total_ratio: float

    @classmethod
    def from_json(cls, d: dict) -> "PathResult":
        return cls(
            path=[PathStep.from_json(s) for s in d["path"]],
            path_length=d["path_length"],
            total_ratio=d["total_ratio"],
        )


@dataclass
class SupplyChainEdge:
    """`from`/`to` are Python keywords, so they're stored under from_id/to_id."""
    from_id: str
    to_id: str
    amount: float

    @classmethod
    def from_json(cls, d: dict) -> "SupplyChainEdge":
        return cls(from_id=d["edgeFrom"], to_id=d["edgeTo"], amount=d["edgeAmount"])


@dataclass
class SupplyChain:
    root: Activity
    total_activities: int
    filtered_activities: int
    entries: list[SupplyChainEntry] = field(default_factory=list)
    edges: list[SupplyChainEdge] = field(default_factory=list)

    @classmethod
    def from_json(cls, d: dict) -> "SupplyChain":
        return cls(
            root=Activity.from_json(d["root"]),
            total_activities=d["totalActivities"],
            filtered_activities=d["filteredActivities"],
            entries=[SupplyChainEntry.from_json(e) for e in d["supplyChain"]],
            edges=[SupplyChainEdge.from_json(e) for e in d.get("edges", [])],
        )


@dataclass
class ConsumersResponse:
    """Reverse supply chain (/consumers) — paginated consumer list plus
    optional edge set. Mirrors :class:`SupplyChain` so callers have a
    uniform {entries, edges} shape in both traversal directions.
    ``edges`` is populated only when ``include_edges=True``.
    """
    consumers: list[ConsumerResult]
    total: int
    offset: int
    limit: int
    has_more: bool
    search_time_ms: float
    edges: list[SupplyChainEdge] = field(default_factory=list)

    @classmethod
    def from_json(cls, d: dict) -> "ConsumersResponse":
        results = d["results"]
        return cls(
            consumers=[ConsumerResult.from_json(c) for c in results["results"]],
            total=results["total"],
            offset=results["offset"],
            limit=results["limit"],
            has_more=results["hasMore"],
            search_time_ms=results.get("searchTimeMs", 0.0),
            edges=[SupplyChainEdge.from_json(e) for e in d.get("edges", [])],
        )


# ---------------------------------------------------------------------------
# Exchanges (typed)
# ---------------------------------------------------------------------------

@dataclass
class TechnosphereExchange:
    """An exchange with another activity (input or output of an intermediate product).

    Built from an `ExchangeWithUnit` envelope: outer fields like flowName/unitName
    live next to an inner `exchange` object (the discriminated `Exchange` sum).
    """

    flow_name: str
    flow_category: str
    amount: float
    unit: str
    is_input: bool
    is_reference: bool
    target_activity: str | None
    target_location: str | None
    target_process_id: str | None

    is_biosphere: bool = False  # discriminator for callers using duck typing

    @classmethod
    def from_json(cls, ewu: dict) -> "TechnosphereExchange":
        inner = ewu["exchange"]
        return cls(
            flow_name=ewu["flowName"],
            flow_category=ewu["flowCategory"],
            amount=inner["amount"],
            unit=ewu["unitName"],
            is_input=inner["isInput"],
            is_reference=inner["isReference"],
            target_activity=ewu.get("targetActivity"),
            target_location=ewu.get("targetLocation"),
            target_process_id=ewu.get("targetProcessId"),
        )


@dataclass
class BiosphereExchange:
    """An exchange with the environment (resource extraction or emission)."""

    flow_name: str
    flow_category: str
    amount: float
    unit: str
    is_input: bool  # True = resource extraction, False = emission

    is_biosphere: bool = True  # discriminator for callers using duck typing

    @classmethod
    def from_json(cls, ewu: dict) -> "BiosphereExchange":
        inner = ewu["exchange"]
        return cls(
            flow_name=ewu["flowName"],
            flow_category=ewu["flowCategory"],
            amount=inner["amount"],
            unit=ewu["unitName"],
            is_input=inner["isInput"],
        )


Exchange = Union[TechnosphereExchange, BiosphereExchange]


def parse_exchange(ewu: dict) -> Exchange:
    """Parse an `ExchangeWithUnit` JSON dict (as returned by GET /activity).

    The inner `exchange` object is tagged with a `"tag"` discriminator
    (``"TechnosphereExchange"`` or ``"BiosphereExchange"``) and carries all
    variant-specific fields flat at the same level.
    """
    tag = ewu["exchange"].get("tag")
    if tag == "TechnosphereExchange":
        return TechnosphereExchange.from_json(ewu)
    if tag == "BiosphereExchange":
        return BiosphereExchange.from_json(ewu)
    raise ValueError(f"Unknown exchange variant tag: {tag!r}")


def parse_exchange_detail(ed: dict) -> Exchange:
    """Parse an `ExchangeDetail` JSON dict (returned by GET /activity/{pid}/inputs|outputs).

    The richer ExchangeDetail shape carries full Flow / Unit / ActivitySummary
    objects. This parser projects it onto the lean Exchange representation; if a
    caller needs the richer shape it should hit the REST endpoint directly.
    """
    inner = ed["exchange"]
    flow = ed.get("flow", {})
    flow_name = flow.get("name", "")
    flow_category = flow.get("category", "")
    unit = ed.get("exchangeUnitName", "")
    tag = inner.get("tag")
    if tag == "TechnosphereExchange":
        target = ed.get("targetActivity") or {}
        return TechnosphereExchange(
            flow_name=flow_name,
            flow_category=flow_category or "technosphere",
            amount=inner["amount"],
            unit=unit,
            is_input=inner["isInput"],
            is_reference=inner["isReference"],
            target_activity=target.get("name"),
            target_location=target.get("location"),
            target_process_id=target.get("processId"),
        )
    if tag == "BiosphereExchange":
        return BiosphereExchange(
            flow_name=flow_name,
            flow_category=flow_category,
            amount=inner["amount"],
            unit=unit,
            is_input=inner["isInput"],
        )
    raise ValueError(f"Unknown exchange variant tag: {tag!r}")


# ---------------------------------------------------------------------------
# Typed activity detail
# ---------------------------------------------------------------------------

@dataclass
class ActivityDetail:
    """Typed wrapper around the JSON returned by GET /activity/{pid}.

    Use the .inputs / .outputs / .technosphere_inputs convenience properties
    instead of walking the raw exchanges list.
    """

    process_id: str
    name: str
    location: str
    unit: str
    description: list[str]
    classifications: dict[str, str]
    reference_product: str | None
    reference_product_amount: float | None
    reference_product_unit: str | None
    all_products: list[Activity]
    exchanges: list[Exchange]

    @classmethod
    def from_json(cls, d: dict) -> "ActivityDetail":
        # The /activity endpoint returns ActivityInfo: `activity` is the ActivityForAPI.
        pfa = d["activity"]
        return cls(
            process_id=pfa["processId"],
            name=pfa["name"],
            location=pfa["location"],
            unit=pfa["unit"],
            description=pfa.get("description", []),
            classifications=pfa.get("classifications", {}),
            reference_product=pfa.get("referenceProduct"),
            reference_product_amount=pfa.get("referenceProductAmount"),
            reference_product_unit=pfa.get("referenceProductUnit"),
            all_products=[Activity.from_json(a) for a in pfa.get("allProducts", [])],
            exchanges=[parse_exchange(e) for e in pfa.get("exchanges", [])],
        )

    @property
    def inputs(self) -> list[Exchange]:
        return [e for e in self.exchanges if e.is_input]

    @property
    def outputs(self) -> list[Exchange]:
        return [e for e in self.exchanges if not e.is_input]

    @property
    def technosphere_inputs(self) -> list[TechnosphereExchange]:
        return [
            e for e in self.exchanges
            if isinstance(e, TechnosphereExchange) and e.is_input
        ]

    @property
    def is_allocated(self) -> bool:
        """True iff description contains a parseable allocation block.

        Implemented in volca/agribalyse.py to keep Agribalyse-specific text
        parsing out of the generic types module.
        """
        from .agribalyse import parse_allocation
        return parse_allocation(self.description) is not None


# ---------------------------------------------------------------------------
# Aggregation (for the /aggregate primitive)
# ---------------------------------------------------------------------------

@dataclass
class AggregateGroup(FromJson):
    """One bucket inside an AggregateResult."""
    key: str
    quantity: float
    count: int
    unit: str | None = None
    share: float | None = None


@dataclass
class AggregateResult:
    """Result of a Client.aggregate() call.

    ``filtered_total`` is the sum across all items matching the filters (the
    top-level number). ``groups`` is the per-bucket breakdown when ``group_by``
    was set; empty otherwise.
    """
    scope: str
    filtered_total: float
    filtered_unit: str | None
    filtered_count: int
    groups: list[AggregateGroup] = field(default_factory=list)

    @classmethod
    def from_json(cls, d: dict) -> "AggregateResult":
        return cls(
            scope=d["scope"],
            filtered_total=d["filteredTotal"],
            filtered_unit=d.get("filteredUnit"),
            filtered_count=d["filteredCount"],
            groups=[AggregateGroup.from_json(g) for g in d.get("groups", [])],
        )
