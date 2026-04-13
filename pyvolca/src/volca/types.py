"""Data types for VoLCA API responses."""

from dataclasses import dataclass, field
from typing import Union


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
class Activity:
    process_id: str
    name: str
    location: str
    product: str
    product_amount: float
    product_unit: str

    @classmethod
    def from_json(cls, d: dict) -> "Activity":
        return cls(
            process_id=d["prsId"],
            name=d["prsName"],
            location=d["prsLocation"],
            product=d["prsProduct"],
            product_amount=d["prsProductAmount"],
            product_unit=d["prsProductUnit"],
        )


@dataclass
class ConsumerResult:
    """Activity that consumes a given supplier, with BFS depth."""
    process_id: str
    name: str
    location: str
    product: str
    product_amount: float
    product_unit: str
    depth: int  # hops from the queried supplier (1 = direct consumer)

    @classmethod
    def from_json(cls, d: dict) -> "ConsumerResult":
        return cls(
            process_id=d["crId"],
            name=d["crName"],
            location=d["crLocation"],
            product=d["crProduct"],
            product_amount=d["crProductAmount"],
            product_unit=d["crProductUnit"],
            depth=d["crDepth"],
        )


@dataclass
class SupplyChainEntry:
    process_id: str
    name: str
    location: str
    quantity: float
    unit: str
    scaling_factor: float
    classifications: dict[str, str] = field(default_factory=dict)

    @classmethod
    def from_json(cls, d: dict) -> "SupplyChainEntry":
        return cls(
            process_id=d["sceProcessId"],
            name=d["sceName"],
            location=d["sceLocation"],
            quantity=d["sceQuantity"],
            unit=d["sceUnit"],
            scaling_factor=d["sceScalingFactor"],
            classifications=d.get("sceClassifications", {}),
        )


@dataclass
class PathStep:
    """One step in the supply chain path returned by get_path_to."""
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
    from_id: str
    to_id: str
    amount: float

    @classmethod
    def from_json(cls, d: dict) -> "SupplyChainEdge":
        return cls(
            from_id=d["sceEdgeFrom"],
            to_id=d["sceEdgeTo"],
            amount=d["sceEdgeAmount"],
        )


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
            root=Activity.from_json(d["scrRoot"]),
            total_activities=d["scrTotalActivities"],
            filtered_activities=d["scrFilteredActivities"],
            entries=[SupplyChainEntry.from_json(e) for e in d["scrSupplyChain"]],
            edges=[SupplyChainEdge.from_json(e) for e in d.get("scrEdges", [])],
        )


# ---------------------------------------------------------------------------
# Exchanges (typed)
# ---------------------------------------------------------------------------

@dataclass
class TechnosphereExchange:
    """An exchange with another activity (input or output of an intermediate product)."""

    flow_name: str            # ewuFlowName
    flow_category: str        # ewuFlowCategory ("technosphere")
    amount: float             # techAmount
    unit: str                 # ewuUnitName
    is_input: bool            # techIsInput
    is_reference: bool        # techIsReference (True for the activity's reference product)
    target_activity: str | None      # ewuTargetActivity — supplier name
    target_location: str | None      # ewuTargetLocation
    target_process_id: str | None    # ewuTargetProcessId — used to descend into the supplier

    is_biosphere: bool = False  # discriminator for callers using duck typing

    @classmethod
    def from_json(cls, ewu: dict) -> "TechnosphereExchange":
        inner = ewu["ewuExchange"]
        return cls(
            flow_name=ewu["ewuFlowName"],
            flow_category=ewu["ewuFlowCategory"],
            amount=inner["techAmount"],
            unit=ewu["ewuUnitName"],
            is_input=inner["techIsInput"],
            is_reference=inner["techIsReference"],
            target_activity=ewu.get("ewuTargetActivity"),
            target_location=ewu.get("ewuTargetLocation"),
            target_process_id=ewu.get("ewuTargetProcessId"),
        )


@dataclass
class BiosphereExchange:
    """An exchange with the environment (resource extraction or emission)."""

    flow_name: str            # ewuFlowName
    flow_category: str        # ewuFlowCategory — compartment (e.g. "air", "water", "soil")
    amount: float             # bioAmount
    unit: str                 # ewuUnitName
    is_input: bool            # bioIsInput — True = resource extraction, False = emission

    is_biosphere: bool = True  # discriminator for callers using duck typing

    @classmethod
    def from_json(cls, ewu: dict) -> "BiosphereExchange":
        inner = ewu["ewuExchange"]
        return cls(
            flow_name=ewu["ewuFlowName"],
            flow_category=ewu["ewuFlowCategory"],
            amount=inner["bioAmount"],
            unit=ewu["ewuUnitName"],
            is_input=inner["bioIsInput"],
        )


Exchange = Union[TechnosphereExchange, BiosphereExchange]


def parse_exchange(ewu: dict) -> Exchange:
    """Parse an `ExchangeWithUnit` JSON dict (as returned by GET /activity).

    The inner `ewuExchange` object is tagged with a `"tag"` discriminator
    (``"TechnosphereExchange"`` or ``"BiosphereExchange"``) and carries all
    variant-specific fields flat at the same level.
    """
    tag = ewu["ewuExchange"].get("tag")
    if tag == "TechnosphereExchange":
        return TechnosphereExchange.from_json(ewu)
    if tag == "BiosphereExchange":
        return BiosphereExchange.from_json(ewu)
    raise ValueError(f"Unknown exchange variant tag: {tag!r}")


def parse_exchange_detail(ed: dict) -> Exchange:
    """Parse an `ExchangeDetail` JSON dict (as returned by GET /activity/{pid}/inputs|outputs).

    The richer ExchangeDetail shape carries full Flow / Unit / ActivitySummary
    objects. This parser projects it onto the lean Exchange representation; if a
    caller needs the richer shape it should hit the REST endpoint directly.
    """
    inner = ed["edExchange"]
    flow = ed.get("edFlow", {})
    flow_name = flow.get("flowName", "")
    flow_category = flow.get("flowCategory", "")
    unit = ed.get("edExchangeUnitName", "")
    if "TechnosphereExchange" in inner:
        tx = inner["TechnosphereExchange"]
        target = ed.get("edTargetActivity") or {}
        return TechnosphereExchange(
            flow_name=flow_name,
            flow_category=flow_category or "technosphere",
            amount=tx["techAmount"],
            unit=unit,
            is_input=tx["techIsInput"],
            is_reference=tx["techIsReference"],
            target_activity=target.get("prsName"),
            target_location=target.get("prsLocation"),
            target_process_id=target.get("prsId"),
        )
    if "BiosphereExchange" in inner:
        bx = inner["BiosphereExchange"]
        return BiosphereExchange(
            flow_name=flow_name,
            flow_category=flow_category,
            amount=bx["bioAmount"],
            unit=unit,
            is_input=bx["bioIsInput"],
        )
    raise ValueError(f"Unknown exchange variant: {list(inner.keys())}")


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
    description: list[str]                  # pfaDescription paragraphs
    classifications: dict[str, str]         # pfaClassifications
    reference_product: str | None
    reference_product_amount: float | None
    reference_product_unit: str | None
    all_products: list[Activity]            # pfaAllProducts
    exchanges: list[Exchange]               # parsed via parse_exchange

    @classmethod
    def from_json(cls, d: dict) -> "ActivityDetail":
        # The /activity endpoint returns ActivityInfo: piActivity is the ActivityForAPI.
        pfa = d["piActivity"]
        return cls(
            process_id=pfa["pfaProcessId"],
            name=pfa["pfaName"],
            location=pfa["pfaLocation"],
            unit=pfa["pfaUnit"],
            description=pfa.get("pfaDescription", []),
            classifications=pfa.get("pfaClassifications", {}),
            reference_product=pfa.get("pfaReferenceProduct"),
            reference_product_amount=pfa.get("pfaReferenceProductAmount"),
            reference_product_unit=pfa.get("pfaReferenceProductUnit"),
            all_products=[Activity.from_json(a) for a in pfa.get("pfaAllProducts", [])],
            exchanges=[parse_exchange(e) for e in pfa.get("pfaExchanges", [])],
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
        """True iff pfaDescription contains a parseable allocation block.

        Implemented in volca/agribalyse.py to keep Agribalyse-specific text
        parsing out of the generic types module.
        """
        from .agribalyse import parse_allocation
        return parse_allocation(self.description) is not None


# ---------------------------------------------------------------------------
# Aggregation (for the /aggregate primitive)
# ---------------------------------------------------------------------------

@dataclass
class AggregateGroup:
    """One bucket inside an AggregateResult."""
    key: str
    quantity: float
    unit: str | None
    share: float | None
    count: int

    @classmethod
    def from_json(cls, d: dict) -> "AggregateGroup":
        return cls(
            key=d["aggGroupKey"],
            quantity=d["aggGroupQuantity"],
            unit=d.get("aggGroupUnit"),
            share=d.get("aggGroupShare"),
            count=d["aggGroupCount"],
        )


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
            scope=d["aggScope"],
            filtered_total=d["aggFilteredTotal"],
            filtered_unit=d.get("aggFilteredUnit"),
            filtered_count=d["aggFilteredCount"],
            groups=[AggregateGroup.from_json(g) for g in d.get("aggGroups", [])],
        )
