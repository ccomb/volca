"""Data types for VoLCA API responses."""

from dataclasses import dataclass, field


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


@dataclass
class SubstitutionResult:
    from_id: str
    to_id: str
    coefficient: float

    @classmethod
    def from_json(cls, d: dict) -> "SubstitutionResult":
        return cls(from_id=d["sbrFrom"], to_id=d["sbrTo"], coefficient=d["sbrCoefficient"])


@dataclass
class Variant:
    original_process_id: str
    substitutions: list[SubstitutionResult]
    supply_chain: list[SupplyChainEntry]
    total_activities: int

    @classmethod
    def from_json(cls, d: dict) -> "Variant":
        return cls(
            original_process_id=d["varOriginalProcessId"],
            substitutions=[SubstitutionResult.from_json(s) for s in d["varSubstitutions"]],
            supply_chain=[SupplyChainEntry.from_json(e) for e in d["varSupplyChain"]],
            total_activities=d["varTotalActivities"],
        )
