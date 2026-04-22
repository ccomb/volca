"""VoLCA Python client — Life Cycle Assessment engine."""

from .client import Client, VoLCAError
from .compare import ActivityDiff, ActivityDiffRow, compare_activities
from .server import Server
from .types import (
    ActivityDetail,
    AggregateGroup,
    AggregateResult,
    BiosphereExchange,
    ClassificationFilter,
    DatabaseInfo,
    Exchange,
    ScoringIndicator,
    TechnosphereExchange,
)

__all__ = [
    "ActivityDetail",
    "ActivityDiff",
    "ActivityDiffRow",
    "AggregateGroup",
    "AggregateResult",
    "BiosphereExchange",
    "Client",
    "ClassificationFilter",
    "DatabaseInfo",
    "Exchange",
    "ScoringIndicator",
    "Server",
    "TechnosphereExchange",
    "VoLCAError",
    "compare_activities",
]
