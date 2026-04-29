"""VoLCA Python client — Life Cycle Assessment engine.

See https://volca.run/docs/guides/pyvolca/ for the full guide.
"""

from .client import Client, VoLCAError
from .compare import ActivityDiff, ActivityDiffRow, compare_activities
from .server import Server
from .types import (
    Activity,
    ActivityDetail,
    AggregateGroup,
    AggregateResult,
    BiosphereExchange,
    ClassificationFilter,
    ConsumerResult,
    ConsumersResponse,
    DatabaseInfo,
    Exchange,
    FlowContribution,
    LCIABatchResult,
    LCIAResult,
    PathResult,
    PathStep,
    ScoringIndicator,
    SupplyChain,
    SupplyChainEdge,
    SupplyChainEntry,
    TechnosphereExchange,
)

__all__ = [
    "Activity",
    "ActivityDetail",
    "ActivityDiff",
    "ActivityDiffRow",
    "AggregateGroup",
    "AggregateResult",
    "BiosphereExchange",
    "Client",
    "ClassificationFilter",
    "ConsumerResult",
    "ConsumersResponse",
    "DatabaseInfo",
    "Exchange",
    "FlowContribution",
    "LCIABatchResult",
    "LCIAResult",
    "PathResult",
    "PathStep",
    "ScoringIndicator",
    "Server",
    "SupplyChain",
    "SupplyChainEdge",
    "SupplyChainEntry",
    "TechnosphereExchange",
    "VoLCAError",
    "compare_activities",
]
