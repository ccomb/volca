"""Data types for VoLCA API responses."""

import dataclasses
import re
from dataclasses import dataclass, field
from enum import Enum
from typing import Any, Callable, ClassVar, Generic, Iterator, Literal, TypeVar, Union

from ._compat import warn_renamed


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


class _StrEnum(str, Enum):
    """Base for str-backed enums whose f-string form is the wire value.

    Python 3.11 added :class:`enum.StrEnum`, but pyvolca still supports
    3.10, so we mimic its behavior with explicit overrides. Subclasses
    inherit equality with raw strings (from ``str``), JSON serialise as
    their value (also from ``str``), and ``str(X.A)`` / ``f"{X.A}"``
    return ``"a"`` instead of ``"X.A"``.
    """

    __str__ = str.__str__  # type: ignore[assignment]
    __format__ = str.__format__  # type: ignore[assignment]


T = TypeVar("T")


@dataclass
class SearchResults(Generic[T]):
    """Paginated wire envelope, mirrors Haskell ``SearchResults a``.

    Carries one page of results plus pagination metadata. Iterating walks
    every page lazily, fetching subsequent pages on demand via the
    ``_fetch`` callback. ``len()`` returns ``total``: the server-reported
    count across *all* pages, not just the items currently held.

    Wire fields (``results``, ``total``, ``offset``, ``limit``, ``has_more``,
    ``search_time_ms``) mirror the server type exactly. Page-style helpers
    (``page_size``, ``page(n)``) are client conveniences computed from them.

    Pages fetched during iteration are cached on the instance, so re-iterating
    replays the cache without hitting the server. Wrap in ``list(...)`` to
    materialise eagerly if you prefer.
    """

    results: list[T]
    total: int
    offset: int
    limit: int
    has_more: bool
    search_time_ms: float

    # Page fetcher: (offset, limit) -> raw JSON dict in the SearchResults
    # wire shape. ``limit`` may be None to let the server apply its own
    # default. None on detached envelopes (single-page in-memory results).
    _fetch: Callable[[int, int | None], dict] | None = field(
        default=None, repr=False, compare=False
    )
    _parse: Callable[[dict], T] | None = field(default=None, repr=False, compare=False)
    # Items fetched lazily during iteration past ``results``. Cached so that
    # a second iteration replays without re-hitting the server.
    _fetched: list[T] = field(default_factory=list, repr=False, compare=False)
    _exhausted: bool = field(default=False, repr=False, compare=False)

    @property
    def page_size(self) -> int:
        """Server-applied limit (page size for further fetches)."""
        return self.limit

    def __len__(self) -> int:
        return self.total

    def __getitem__(self, i: "int | slice") -> "T | list[T]":
        """Index or slice the *current* page only.

        Use ``list(sr)`` first when you need indexing/slicing across all
        pages; ``__getitem__`` deliberately stays local to avoid hidden
        round trips.
        """
        return self.results[i]

    def __iter__(self) -> Iterator[T]:
        """Yield items across all pages, fetching subsequent pages on demand.

        Yields the initial page, then any already-cached follow-up pages,
        then continues fetching until ``has_more`` is False. Subsequent
        iterations replay from the cache.

        Raises :class:`RuntimeError` if the server returns ``hasMore=True``
        with no items: that means the pagination contract is broken and
        silently stopping would let callers consume an incomplete result
        set without ever learning the engine misbehaved.
        """
        yield from self.results
        yield from self._fetched
        if self._exhausted or not self.has_more or self._fetch is None or self._parse is None:
            self._exhausted = True
            return
        offset = self.offset + len(self.results) + len(self._fetched)
        limit = self.limit
        while True:
            raw = self._fetch(offset, limit)
            items = [self._parse(x) for x in raw.get("results", [])]
            if not items:
                raise RuntimeError(
                    f"Server returned hasMore=True with no items at offset={offset}, "
                    f"limit={limit}. Pagination contract broken, refusing to truncate "
                    "silently. Report this to the engine team."
                )
            self._fetched.extend(items)
            yield from items
            if not raw.get("hasMore", False):
                self._exhausted = True
                return
            offset = raw.get("offset", offset) + len(items)

    def page(self, n: int, *, page_size: int | None = None) -> "SearchResults[T]":
        """Fetch a specific page (1-based). Returns a fresh SearchResults.

        ``page_size`` overrides the current ``limit`` for the fetched page;
        the returned envelope's ``limit`` reflects what the server actually
        applied.
        """
        if n < 1:
            raise ValueError(f"page must be >= 1, got {n}")
        if self._fetch is None or self._parse is None:
            raise RuntimeError(
                "SearchResults has no fetcher attached, cannot fetch additional pages. "
                "This SearchResults was likely constructed in-memory (e.g. a test fixture)."
            )
        ps = page_size if page_size is not None else self.limit
        offset = (n - 1) * ps
        raw = self._fetch(offset, ps)
        return SearchResults.from_raw(raw, parse=self._parse, fetch=self._fetch)

    @classmethod
    def from_raw(
        cls,
        raw: dict,
        *,
        parse: Callable[[dict], T],
        fetch: Callable[[int, int | None], dict] | None = None,
    ) -> "SearchResults[T]":
        """Build from the wire envelope.

        Wire keys: ``results``, ``total``, ``offset``, ``limit``, ``hasMore``,
        ``searchTimeMs``. ``fetch`` is the callback used by iteration and
        ``page(n)`` to retrieve further pages. Omit only when the envelope
        is a single-page snapshot (``hasMore=False``); otherwise iteration
        would silently truncate and the constructor raises.

        When ``fetch`` is provided (the production path), every wire key is
        required: missing fields would let pyvolca silently default to a
        truncated total or page size, which is the exact silent-undercount
        bug 0.5.0 set out to eliminate. When ``fetch`` is None (test
        fixtures), missing fields fall back to permissive defaults so
        callers can build small envelopes by hand.
        """
        items = [parse(x) for x in raw.get("results", [])]
        if fetch is not None:
            missing = {"total", "offset", "limit", "hasMore"} - set(raw)
            if missing:
                raise ValueError(
                    f"SearchResults wire envelope is missing required keys: {sorted(missing)}. "
                    "The engine's response shape changed or this isn't a SearchResults envelope."
                )
            return cls(
                results=items,
                total=raw["total"],
                offset=raw["offset"],
                limit=raw["limit"],
                has_more=raw["hasMore"],
                search_time_ms=raw.get("searchTimeMs", 0.0),
                _fetch=fetch,
                _parse=parse,
            )
        # Detached fixture: lenient defaults, but still refuse hasMore=True
        # (would silently truncate during iteration).
        has_more = raw.get("hasMore", False)
        if has_more:
            raise ValueError(
                "SearchResults envelope reports hasMore=True but no fetch callback "
                "was provided. Iteration would silently truncate. Pass fetch=, or "
                "set hasMore=False on test fixtures."
            )
        return cls(
            results=items,
            total=raw.get("total", len(items)),
            offset=raw.get("offset", 0),
            limit=raw.get("limit", len(items)),
            has_more=False,
            search_time_ms=raw.get("searchTimeMs", 0.0),
            _fetch=None,
            _parse=parse,
        )


class DatabaseStatus(_StrEnum):
    """Lifecycle state of a database in the engine.

    ``UNLOADED``: declared in the engine config but not yet loaded.
    ``PARTIALLY_LINKED``: loaded, but some cross-DB flow references could
    not be resolved against currently-loaded dependencies.
    ``LOADED``: loaded and fully linked.

    Inherits from :class:`str`, so ``dataclasses.asdict(db)["status"]``
    serialises as the bare wire string.
    """

    UNLOADED = "unloaded"
    PARTIALLY_LINKED = "partially_linked"
    LOADED = "loaded"


@dataclass
class DatabaseInfo(FromJson):
    """One entry of :meth:`Client.list_databases`.

    ``depends_on`` names the databases this one links against for cross-DB
    flow resolution, mirroring the ``dependsOn`` list surfaced by the relink
    endpoint. Derived from the engine's declared topology, not runtime state.

    ``allocation`` is the key its multi-output blocks were divided by:
    ``"declared"`` for the shares the source itself states, otherwise the name
    of the property recomputed them (``"dry mass"``, ``"wet mass"``). Without
    it a column of shares cannot say which of the two it is showing.
    ``source`` names the database this one was derived from, and is ``None``
    for one read straight from its files. Both are ``None`` against an engine
    older than wire revision 20.
    """

    name: str
    display_name: str
    status: DatabaseStatus
    path: str
    load_at_startup: bool = False
    is_uploaded: bool = False
    activity_count: int = 0
    description: str | None = None
    format: str | None = None
    depends_on: list[str] = field(default_factory=list)
    allocation: str | None = None
    source: str | None = None

    @classmethod
    def from_json(cls, d: dict) -> "DatabaseInfo":
        names = {f.name for f in dataclasses.fields(cls)}
        kwargs = {k: v for k, v in ((_to_snake(k), v) for k, v in d.items()) if k in names}
        kwargs["status"] = DatabaseStatus(kwargs["status"])
        return cls(**kwargs)


@dataclass
class ScoringIndicator(FromJson):
    """One per-variable entry inside ``LCIABatchResult.scoring_indicators``.

    ``value`` is pre-multiplied by the scoring set's ``displayMultiplier``
    (configured in the scoring TOML) and expressed in the set's display unit.
    ``category`` is the indicator's display name: the scoring set's
    ``labels`` entry when one is configured (typically for computed
    variables), otherwise the impact category the variable was resolved
    from, or as a last resort the raw variable key.
    """

    category: str
    value: float


@dataclass
class FlowContribution(FromJson):
    """Top contributing elementary flow for an impact category.

    Emitted inside ``LCIAResult.top_contributors``.
    """

    flow_name: str
    contribution: float  # in the impact unit
    share_pct: float  # 0..100
    flow_id: str
    category: str  # e.g. "air/urban air"
    cf_value: float = 0.0  # raw characterization factor
    compartment: str | None = None
    match_kind: str | None = None  # how the factor was found; None when no factor in the method reaches this flow


@dataclass
class LCIAResult:
    """LCIA score for one impact category on one activity.

    Returned directly by :meth:`Client.get_impacts`, and nested inside
    :class:`LCIABatchResult.results` (one entry per impact category).

    ``functional_unit`` is the basis ``score`` is reported against, and it is
    always one unit of the reference product (``"1.00 kg of cream"``) however
    much of it the source says the process produces.
    """

    method_id: str
    method_name: str
    category: str
    damage_category: str
    score: float
    unit: str
    mapped_flows: int
    functional_unit: str
    normalized_score: float | None = None
    weighted_score: float | None = None  # in Pt
    top_contributors: list[FlowContribution] = field(default_factory=list)

    @classmethod
    def from_json(cls, d: dict) -> "LCIAResult":
        return cls(
            method_id=d["methodId"],
            method_name=d["methodName"],
            category=d["category"],
            damage_category=d["damageCategory"],
            score=d["score"],
            unit=d["unit"],
            mapped_flows=d["mappedFlows"],
            functional_unit=d["functionalUnit"],
            normalized_score=d.get("normalizedScore"),
            weighted_score=d.get("weightedScore"),
            top_contributors=[FlowContribution.from_json(c) for c in d.get("topContributors", [])],
        )


@dataclass
class LCIABatchResult:
    """Batch LCIA: every impact category in a method collection, for one activity.

    Returned by :meth:`Client.get_impacts_batch`. Carries the per-method
    impact results plus any formula-based scoring sets configured in the
    engine TOML (PEF, ECS, or any named set).

    ``scoring_indicators`` gives the per-variable normalized-weighted
    breakdown of each scoring set, already multiplied by the set's
    ``displayMultiplier`` and expressed in its display unit (see
    :class:`ScoringIndicator`). Lets callers render per-indicator charts
    alongside the aggregate ``scoring_results``.
    """

    results: list[LCIAResult]
    single_score: float | None = None  # sum of weighted scores, in Pt
    single_score_unit: str | None = None
    norm_weight_set_name: str | None = None
    available_nw_sets: list[str] = field(default_factory=list)
    scoring_results: dict[str, dict[str, float]] = field(default_factory=dict)
    scoring_units: dict[str, str] = field(default_factory=dict)
    scoring_indicators: dict[str, dict[str, ScoringIndicator]] = field(default_factory=dict)

    @classmethod
    def from_json(cls, d: dict) -> "LCIABatchResult":
        raw_indicators = d.get("scoringIndicators", {})
        return cls(
            results=[LCIAResult.from_json(r) for r in d.get("results", [])],
            single_score=d.get("singleScore"),
            single_score_unit=d.get("singleScoreUnit"),
            norm_weight_set_name=d.get("normWeightSetName"),
            available_nw_sets=d.get("availableNWsets", []),
            scoring_results=d.get("scoringResults", {}),
            scoring_units=d.get("scoringUnits", {}),
            scoring_indicators={
                set_name: {var: ScoringIndicator.from_json(si) for var, si in per_set.items()}
                for set_name, per_set in raw_indicators.items()
            },
        )


@dataclass
class PerturbedResult:
    """One perturbation outcome from :meth:`Client.compute_sensitivity`.

    The engine flattens an ``Either`` on the wire: a success carries
    ``impact`` and ``delta_impact`` (with ``error`` None), a failure carries
    ``error`` (with the other two None). ``perturbation`` echoes the request
    entry (including its ``label`` if one was supplied), so results correlate
    without an out-of-band index.
    """

    perturbation: dict
    impact: "LCIAResult | None"
    delta_impact: "float | None"
    error: "str | None"

    @classmethod
    def from_json(cls, d: dict) -> "PerturbedResult":
        raw_impact = d.get("impact")
        return cls(
            perturbation=d["perturbation"],
            impact=LCIAResult.from_json(raw_impact) if raw_impact is not None else None,
            delta_impact=d.get("deltaImpact"),
            error=d.get("error"),
        )


@dataclass
class SensitivityResult:
    """Sensitivity analysis: baseline impact plus one entry per perturbation.

    Returned by :meth:`Client.compute_sensitivity`. ``perturbed`` preserves
    the order of the requested perturbations.
    """

    baseline: LCIAResult
    perturbed: list[PerturbedResult]

    @classmethod
    def from_json(cls, d: dict) -> "SensitivityResult":
        return cls(
            baseline=LCIAResult.from_json(d["baseline"]),
            perturbed=[PerturbedResult.from_json(p) for p in d["perturbed"]],
        )


@dataclass
class ScoredActivity:
    """One process's batch impacts inside a :class:`BatchScores`.

    ``impacts`` is the same :class:`LCIABatchResult` that
    :meth:`Client.get_impacts_batch` returns for a single process.
    """

    process_id: str
    activity_name: str
    impacts: LCIABatchResult


@dataclass
class BatchScores:
    """Result of :meth:`Client.score_activities` scoring many processes at once.

    ``results`` carries one :class:`ScoredActivity` per process the engine
    computed; ``not_found`` and ``invalid`` list the process ids it could not
    resolve, and ``unscorable`` those that resolve but name an activity the
    engine refuses to score (its quality report's ``unallocated`` check says
    why; wire revision 14, empty from an older engine). A non-empty list is a
    partial result to inspect, not a failure.
    """

    results: list[ScoredActivity]
    not_found: list[str]
    invalid: list[str]
    unscorable: list[str]

    @classmethod
    def from_json(cls, d: dict) -> "BatchScores":
        return cls(
            results=[
                ScoredActivity(
                    process_id=r["processId"],
                    activity_name=r["activityName"],
                    impacts=LCIABatchResult.from_json(r["impacts"]),
                )
                for r in d["results"]
            ],
            not_found=d["notFound"],
            invalid=d["invalid"],
            unscorable=d.get("unscorable", []),
        )


class MatchMode(_StrEnum):
    """How a :class:`ClassificationFilter` value is compared against the entry.

    ``EXACT``: case-insensitive equality. ``CONTAINS``: case-insensitive
    substring. Inherits from :class:`str` so ``json.dumps(MatchMode.EXACT)``
    and ``dataclasses.asdict(filter)["mode"]`` both serialise as the bare
    string ``"exact"`` / ``"contains"``.
    """

    EXACT = "exact"
    CONTAINS = "contains"


MatchModeLike = Union[MatchMode, Literal["exact", "contains"]]
"""Internal alias: a :class:`MatchMode` member or its literal string form.

Pyright autocompletes both shapes and rejects typos (``"exct"``, ``"Exact"``)
statically; the constructor normalises to :class:`MatchMode` at runtime."""


@dataclass(init=False, frozen=True)
class ClassificationFilter:
    """Filter a supply-chain/consumers query by a classification (system, value, mode).

    Matches one classification system entry, e.g.
    ``ClassificationFilter("Category", "Agricultural\\\\Food", "exact")`` or
    ``ClassificationFilter("Category", "Agricultural\\\\Food", MatchMode.EXACT)``.
    Multiple filters are AND-combined by the server.
    """

    system: str
    value: str
    mode: MatchMode = MatchMode.CONTAINS

    def __init__(
        self,
        system: str,
        value: str,
        mode: MatchModeLike = MatchMode.CONTAINS,
    ):
        if isinstance(mode, MatchMode):
            resolved = mode
        else:
            try:
                resolved = MatchMode(mode)
            except ValueError:
                valid = ", ".join(repr(m.value) for m in MatchMode)
                raise ValueError(
                    f"mode must be one of {valid} (or a MatchMode member); got {mode!r}"
                ) from None
        object.__setattr__(self, "system", system)
        object.__setattr__(self, "value", value)
        object.__setattr__(self, "mode", resolved)


@dataclass
class Activity(FromJson):
    """One activity in a database: the row returned by /activities search.

    ``process_id`` is the engine's canonical address (``activityUUID_productUUID``)
    and is what you pass to every detail endpoint (:meth:`Client.get_activity`,
    :meth:`Client.get_supply_chain`, :meth:`Client.get_impacts`, …).
    ``activity_name`` is the activity name (e.g. ``"wheat flour, at plant"``);
    ``product_name`` is the reference output product (e.g. ``"wheat flour"``);
    ``product_amount`` and ``product_unit`` are what the source says this
    process produces (typically ``1.0`` of ``"kg"`` / ``"mj"`` / etc.; a
    database imported from SimaPro or Brightway Excel states it in the
    canonical unit of its dimension, so a 1 kWh reference product reads
    ``3.6`` of ``"mj"``, and a coproduct its block states at 0.0686462 kg
    reads that). It is not the basis a score or an inventory is reported
    against: those are always per **one** ``product_unit``, whatever the
    amount here.
    ``location`` is the
    geography code (``"FR"``, ``"GLO"``, ``"RoW"``…). A process has no name of
    its own; compose a label from ``activity_name`` + ``product_name``.

    ``allocation_percent`` is this product's share (0..100) of the parent
    activity's exchanges in a multi-output (allocated) process, e.g. a
    cheese activity that also yields whey, cream and permeate gives each
    product its own share, summing to ~100. It is ``None`` for single-output
    processes. ``allocation_formula`` carries the raw symbolic formula when
    the source expressed the share as an expression rather than a number,
    else ``None``.

    ``mass_allocation_percent`` is the share the same product would carry if the key
    were its mass rather than what the source declared, so the two read
    side by side: the cheese above is declared 51.4 % of its block where its
    mass is 11.7 %, and a kilo of it therefore carries 4.4 times more under
    the declared key. It is ``None`` outside an activity's own product list,
    when the block's products are not all stated in a mass, and against an
    engine older than wire revision 15.

    ``block`` names the source block the process came out of, as a value to
    compare and never to read: the coproducts of one block carry the same one
    and nothing else does, which is what lets a listing gather them.
    ``block_products`` is how many products that block holds, so a page
    showing fewer rows than that knows it is showing part of a block and not
    the whole of it. Both are ``None`` against an engine older than wire
    revision 21.
    """

    process_id: str
    activity_name: str
    location: str
    product_name: str
    product_amount: float
    product_unit: str
    allocation_percent: float | None = None
    allocation_formula: str | None = None
    mass_allocation_percent: float | None = None
    block: str | None = None
    block_products: int | None = None


@dataclass
class Flow(FromJson):
    """One flow as returned by /flows.

    Mirrors the server's :code:`FlowSearchResult`. ``kind`` says which of the
    three a flow is: ``"technosphere"`` for a product one activity makes and
    another consumes, ``"biosphere"`` for a substance exchanged with nature,
    ``"waste"`` for a waste. It is ``None`` against an engine older than wire
    revision 9, which did not report it.

    ``category`` is the medium alone ("air", "water", "soil", "natural resource") and
    ``compartment`` the sub-compartment ("agricultural"), which is often all
    that tells two same-named flows apart. Only a biosphere flow has either:
    that is where "taken from nature" and "released to nature" are told apart.
    ``synonyms`` maps language code → list of synonym strings (empty when the
    database carries no synonym index).

    ``producer_count`` is how many activities make this flow, which is the
    number of ways a database offers to produce it. It is ``None`` on a flow
    no activity can produce (biosphere, waste) and against an engine older
    than wire revision 16; a zero would be the different statement that
    nothing makes it.
    """

    id: str
    name: str
    category: str
    unit_name: str
    kind: str | None = None
    compartment: str | None = None
    synonyms: dict[str, list[str]] = field(default_factory=dict)
    producer_count: int | None = None


@dataclass
class SearchCounts(FromJson):
    """How many processes, products and flows one query matches.

    The three are disjoint and together cover the database, so they partition
    what a query found. Use them to tell which of the three a term is really
    about before listing any of them: a term matching 2 processes and 300
    flows is a substance name, not a product.

    Needs engine wire revision 17.
    """

    processes: int
    products: int
    flows: int


@dataclass
class ConsumerResult(FromJson):
    """Activity that consumes a given supplier, with BFS depth."""
    process_id: str
    activity_name: str
    location: str
    product_name: str
    product_amount: float
    product_unit: str
    depth: int  # hops from the queried supplier (1 = direct consumer)
    classifications: dict[str, str] = field(default_factory=dict)  # ISIC / CPC / Category, mirrors SupplyChainEntry


@dataclass
class SupplyChainEntry(FromJson):
    """One activity in a :class:`SupplyChain.entries` list.

    ``quantity`` is the cumulative amount of this activity's reference
    product consumed per functional unit of the root activity, in ``unit``.
    ``unit`` is the producing activity's reference-product unit, which for a
    database imported from SimaPro or Brightway Excel is the canonical unit of
    its dimension (``"kg"``, ``"mj"``, ``"m3"``), not the unit written on the
    exchange that consumes it. An input stated as ``0.22 kWh`` therefore shows
    up here as its equivalent in ``mj``: 1 kWh is 3.6 MJ.
    ``scaling_factor`` is the multiplier the solver applied to this
    activity to produce ``quantity``, i.e. ``quantity = ref_output * scaling_factor``.
    ``classifications`` mirrors the producing activity's classifications
    (ISIC, CPC, Category, …) so callers can filter by taxonomy without a
    second :meth:`Client.get_activity` round trip.
    ``depth`` is the BFS shortest-path distance from the queried root
    (0 = the root itself), ``upstream_count`` the number of direct
    consumers of this activity inside the chain, and ``database_name``
    the database the entry lives in (they differ across linked databases).
    """

    process_id: str
    database_name: str
    activity_name: str
    location: str
    quantity: float
    unit: str
    scaling_factor: float
    depth: int  # hops from the queried root (0 = root), BFS shortest path
    upstream_count: int
    classifications: dict[str, str] = field(default_factory=dict)


@dataclass
class PathStep:
    """One step in the supply chain path returned by get_path_to.

    Note: the /path endpoint is hand-built (aeson `object [...]`) but now
    emits camelCase keys (``processId``, ``activityName``,
    ``cumulativeQuantity``, …) like the rest of the API.
    """
    process_id: str
    activity_name: str
    location: str
    unit: str
    cumulative_quantity: float
    scaling_factor: float
    local_step_ratio: float | None = None  # absent on root step

    @classmethod
    def from_json(cls, d: dict) -> "PathStep":
        return cls(
            process_id=d["processId"],
            activity_name=d["activityName"],
            location=d["location"],
            unit=d["unit"],
            cumulative_quantity=d["cumulativeQuantity"],
            scaling_factor=d["scalingFactor"],
            local_step_ratio=d.get("localStepRatio"),
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
    """A consumer→supplier link in the supply chain.

    ``from``/``to`` are Python keywords, so the process ids are stored under
    ``from_id``/``to_id``. ``from_db``/``to_db`` carry each endpoint's database
    name, which is required to route edges across databases (the same process
    id can exist in more than one loaded DB).
    """
    from_id: str
    from_db: str
    to_id: str
    to_db: str
    amount: float

    @classmethod
    def from_json(cls, d: dict) -> "SupplyChainEdge":
        return cls(
            from_id=d["edgeFrom"],
            from_db=d["edgeFromDb"],
            to_id=d["edgeTo"],
            to_db=d["edgeToDb"],
            amount=d["edgeAmount"],
        )


@dataclass
class SupplyChain:
    """Flat supply chain of an activity.

    ``total_activities`` is the unfiltered upstream count; ``filtered_activities``
    is what remains after the server applies ``classification_filters`` /
    ``min_quantity`` / ``preset``. ``entries`` is the slice the server actually
    returned; it may be shorter than ``filtered_activities`` when ``limit``
    truncates. Use :attr:`has_more` to detect that case rather than comparing
    lengths by hand.
    """

    root: Activity
    total_activities: int
    filtered_activities: int
    entries: list[SupplyChainEntry] = field(default_factory=list)
    edges: list[SupplyChainEdge] = field(default_factory=list)

    @property
    def has_more(self) -> bool:
        """True when the server truncated ``entries`` below ``filtered_activities``.

        Surfacing this lets callers detect silent truncation: if you passed
        ``limit=100`` and ``filtered_activities`` is 500, downstream LCA work
        would be wrong without flagging the gap.
        """
        return len(self.entries) < self.filtered_activities

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
    """Reverse supply chain (/consumers): paginated consumer list plus
    optional edge set. Mirrors :class:`SupplyChain` so callers have a
    uniform {entries, edges} shape in both traversal directions.

    ``consumers`` is a :class:`SearchResults[ConsumerResult]`: iterate it
    to walk every consumer across all pages. ``edges`` is populated only
    when ``include_edges=True``.
    """
    consumers: "SearchResults[ConsumerResult]"
    edges: list[SupplyChainEdge] = field(default_factory=list)

    @classmethod
    def from_json(
        cls,
        d: dict,
        *,
        fetch: Callable[[int, int | None], dict] | None = None,
    ) -> "ConsumersResponse":
        """Parse the /consumers wire envelope.

        ``fetch`` is a page fetcher returning the inner ``results`` envelope
        for ``(offset, limit)``, used by ``SearchResults`` for lazy
        iteration. The client wires this so users get pagination for free;
        callers building ConsumersResponse manually (e.g. tests) can omit
        it and the resulting SearchResults is "detached" (one page only).
        """
        inner_fetch: Callable[[int, int | None], dict] | None
        if fetch is None:
            inner_fetch = None
        else:
            page_fetch = fetch  # narrowed to non-None for the closure below

            def _fetch_results(o: int, l: int | None) -> dict:
                return page_fetch(o, l)["results"]

            inner_fetch = _fetch_results
        return cls(
            consumers=SearchResults.from_raw(
                d["results"], parse=ConsumerResult.from_json, fetch=inner_fetch,
            ),
            edges=[SupplyChainEdge.from_json(e) for e in d.get("edges", [])],
        )


# ---------------------------------------------------------------------------
# Exchanges (typed)
# ---------------------------------------------------------------------------

def _exchange_comment(ewu: dict | None, inner: dict) -> str | None:
    """Pick the per-exchange free-text comment.

    The wire exposes it in two places: ``exComment`` flat on the
    ``ExchangeWithUnit`` envelope, and ``comment`` inside the inner
    ``Exchange`` (mirrored from the source format's free-text field).
    Prefer the flat one when present; fall back to inner.
    """
    if ewu is not None:
        flat = ewu.get("exComment")
        if flat is not None:
            return flat
    return inner.get("comment")


@dataclass(frozen=True)
class Compartment:
    """Biosphere compartment (medium + optional subcompartment).

    Frozen so it's hashable and immutable, so callers can use it as a dict key
    when grouping flows by compartment, and accidental mutation is rejected.
    """

    name: str
    sub: str | None = None

    @classmethod
    def from_json(cls, c: dict | None) -> "Compartment | None":
        if c is None:
            return None
        return cls(name=c.get("name", ""), sub=c.get("sub"))


class TechRole(_StrEnum):
    """Role a technosphere exchange plays within its host activity.

    ``REFERENCE_PRODUCT``: the activity's reference output product.
    ``COPRODUCT``: a product output the source left unallocated; an activity
    still carrying one is refused a score (see the ``unallocated`` check of
    the quality report).
    ``AVOIDED_PRODUCT``: a substitution, the product the activity displaces;
    a credit on that product's producer (wire revision 14).
    ``REFERENCE_INPUT``: the reference input (in waste-treatment activities).
    ``INPUT``: any other technosphere input.
    """

    REFERENCE_PRODUCT = "ReferenceProduct"
    COPRODUCT = "Coproduct"
    AVOIDED_PRODUCT = "AvoidedProduct"
    REFERENCE_INPUT = "ReferenceInput"
    INPUT = "Input"


def _role_is_input(role: TechRole) -> bool:
    return role in (TechRole.INPUT, TechRole.REFERENCE_INPUT)


def _role_is_reference(role: TechRole) -> bool:
    return role in (TechRole.REFERENCE_PRODUCT, TechRole.REFERENCE_INPUT)


class ClaimKind(_StrEnum):
    """How a source file designates the supplier of an exchange.

    ``PRODUCT``: the product row itself, its flow and where it has one its
    location - SimaPro, ILCD, and a Brightway row naming no activity.
    ``ID``: the supplier activity's own identifier, as EcoSpold 2 writes it.
    ``NAME``: the supplier activity by name, from a Brightway ``name`` column.
    ``DATASET_NUMBER``: the number the supplier dataset is published under,
    as EcoSpold 1 writes it.
    """

    PRODUCT = "ClaimByProduct"
    ID = "ClaimById"
    NAME = "ClaimByName"
    DATASET_NUMBER = "ClaimByDatasetNumber"


@dataclass(frozen=True)
class SupplierClaim:
    """What the source said about an exchange's supplier, before linking.

    Every format designates a supplier differently, and linking overwrites
    what it resolved to; this is the claim itself, which linking never
    rewrites, so a reader can tell what the file asked for from what the
    engine found. ``value`` is the identifier, name or dataset number the
    claim carries, and is ``None`` for :attr:`ClaimKind.PRODUCT`, which names
    the supplier by the product row and carries nothing else.

    Wire revision 23; ``None`` on a technosphere exchange from an older engine.
    """

    kind: ClaimKind
    value: str | int | None = None

    @classmethod
    def from_json(cls, d: dict | None) -> "SupplierClaim | None":
        if not d or "tag" not in d:
            return None
        return cls(kind=ClaimKind(d["tag"]), value=d.get("contents"))


@dataclass
class TechnosphereExchange:
    """An exchange with another activity. Carries no compartment: the
    producing activity's classifications describe the product taxonomy.

    ``supplier_claim`` is how the source designated the supplier, which is not
    the same question as which activity it resolved to: ``target_process_id``
    is the answer, this is what was asked (wire revision 23).
    """

    flow_name: str
    amount: float
    unit: str
    role: TechRole
    target_activity_name: str | None
    target_location: str | None
    target_process_id: str | None
    comment: str | None = None
    supplier_claim: SupplierClaim | None = None

    is_biosphere: bool = False  # discriminator for callers using duck typing
    is_waste: bool = False

    @property
    def is_input(self) -> bool:
        """True for technosphere inputs (``role`` is ``INPUT`` or ``REFERENCE_INPUT``).

        Lets callers split exchanges into inputs vs. outputs without
        knowing the four-role taxonomy.
        """
        return _role_is_input(self.role)

    @property
    def is_reference(self) -> bool:
        """True for reference roles (``REFERENCE_PRODUCT`` / ``REFERENCE_INPUT``).

        The reference exchange is the one that defines the activity's
        functional unit, the basis the LCA result is normalised to.
        """
        return _role_is_reference(self.role)

    @classmethod
    def from_json(cls, ewu: dict) -> "TechnosphereExchange":
        inner = ewu["exchange"]
        return cls(
            flow_name=ewu["flowName"],
            amount=inner["amount"],
            unit=ewu["unitName"],
            role=TechRole(inner["role"]),
            target_activity_name=ewu.get("targetActivityName"),
            target_location=ewu.get("targetLocation"),
            target_process_id=ewu.get("targetProcessId"),
            comment=_exchange_comment(ewu, inner),
            supplier_claim=SupplierClaim.from_json(inner.get("supplierClaim")),
        )


class BioDirection(_StrEnum):
    """Direction of a biosphere exchange.

    ``RESOURCE``: extraction from the environment (input).
    ``EMISSION``: release to the environment (output).

    Lookup is case-insensitive (``BioDirection("emission")`` works): the
    engine reads the wire value that way, so the client should not be
    stricter than the server it speaks for.
    """

    RESOURCE = "Resource"
    EMISSION = "Emission"

    @classmethod
    def _missing_(cls, value: object) -> "BioDirection | None":
        if isinstance(value, str):
            for member in cls:
                if member.value.lower() == value.lower():
                    return member
        return None


def _direction_is_input(direction: BioDirection) -> bool:
    return direction is BioDirection.RESOURCE


@dataclass
class BiosphereExchange:
    """An exchange with the environment (resource extraction or emission).

    ``flow_id`` is what a line writes back when its words cannot address the
    flow on their own: a name several flows answer to, or one whose source
    recorded no compartment. Everything else restates as
    :meth:`BioExchange.from_name` takes it.
    """

    flow_name: str
    compartment: Compartment | None
    amount: float
    unit: str
    direction: BioDirection  # "Resource" = extraction, "Emission" = release
    flow_id: str
    comment: str | None = None

    is_biosphere: bool = True  # discriminator for callers using duck typing
    is_waste: bool = False

    @property
    def is_input(self) -> bool:
        """True for resource extractions (``direction`` is ``RESOURCE``).

        Biosphere inputs are resource extractions; outputs are emissions
        to the environment.
        """
        return _direction_is_input(self.direction)

    @property
    def is_reference(self) -> bool:
        """Always False: biosphere exchanges cannot be reference flows.

        The reference flow defines the functional unit and is always a
        technosphere product (see :class:`TechnosphereExchange.is_reference`).
        """
        return False

    @classmethod
    def from_json(cls, ewu: dict) -> "BiosphereExchange":
        inner = ewu["exchange"]
        return cls(
            flow_name=ewu["flowName"],
            compartment=Compartment.from_json(ewu.get("compartment")),
            amount=inner["amount"],
            unit=ewu["unitName"],
            direction=BioDirection(inner["direction"]),
            flow_id=inner["flowId"],
            comment=_exchange_comment(ewu, inner),
        )


class WasteRole(_StrEnum):
    """What a waste line does within its activity.

    ``TREATS_WASTE``: an input, so this activity is the one treating it.
    ``SENT_TO_TREATMENT``: an output whose treatment was found.
    ``FINAL_WASTE_FLOW``: an output naming no treatment, so nothing treats it.
    ``TREATMENT_NOT_LOADED``: an output naming a treatment no loaded database
    ships, so its burden is missing rather than accounted for.

    The last two both arrive with no target, which is why the role is stated
    rather than worked out from the target fields.
    """

    TREATS_WASTE = "TreatsWaste"
    SENT_TO_TREATMENT = "SentToTreatment"
    FINAL_WASTE_FLOW = "FinalWasteFlow"
    TREATMENT_NOT_LOADED = "TreatmentNotLoaded"


@dataclass
class WasteExchange:
    """An exchange of a waste flow with a treatment activity.

    Shares the technosphere matrix with product flows but tracked as its own
    kind so callers can tell a "waste sent to landfill" output apart from a
    product input. A waste output no treatment is found for contributes zero
    impact, the same cut-off semantics as an orphan technosphere input;
    ``role`` says whether that is because nothing treats it or because the
    treatment it names was not loaded.
    """

    flow_name: str
    amount: float
    unit: str
    is_input: bool  # True = consumed by treatment process; False = generated (typical case)
    target_activity_name: str | None
    target_location: str | None
    target_process_id: str | None
    comment: str | None = None
    role: WasteRole | None = None  # None from an engine older than wire 10
    supplier_claim: SupplierClaim | None = None  # how the source named the treatment (wire 23)

    is_biosphere: bool = False
    is_waste: bool = True

    @property
    def is_reference(self) -> bool:
        """Always False: waste flows never define an activity's functional unit.

        Treatment activities have a ``ReferenceInput`` instead, exposed
        via :class:`TechnosphereExchange`.
        """
        return False

    @classmethod
    def from_json(cls, ewu: dict) -> "WasteExchange":
        inner = ewu["exchange"]
        raw_role = ewu.get("wasteRole")
        return cls(
            flow_name=ewu["flowName"],
            amount=inner["amount"],
            unit=ewu["unitName"],
            is_input=inner["isInput"],
            target_activity_name=ewu.get("targetActivityName"),
            target_location=ewu.get("targetLocation"),
            target_process_id=ewu.get("targetProcessId"),
            comment=_exchange_comment(ewu, inner),
            role=WasteRole(raw_role) if raw_role else None,
            supplier_claim=SupplierClaim.from_json(inner.get("supplierClaim")),
        )


Exchange = Union[TechnosphereExchange, BiosphereExchange, WasteExchange]


def parse_exchange(ewu: dict) -> Exchange:
    """Parse an `ExchangeWithUnit` JSON dict (as returned by GET /activity).

    The inner `exchange` object is tagged with a `"tag"` discriminator
    (``"TechnosphereExchange"``, ``"BiosphereExchange"`` or
    ``"WasteExchange"``) and carries all variant-specific fields flat at the
    same level.
    """
    tag = ewu["exchange"].get("tag")
    if tag == "TechnosphereExchange":
        return TechnosphereExchange.from_json(ewu)
    if tag == "BiosphereExchange":
        return BiosphereExchange.from_json(ewu)
    if tag == "WasteExchange":
        return WasteExchange.from_json(ewu)
    raise ValueError(f"Unknown exchange variant tag: {tag!r}")


def parse_exchange_detail(ed: dict) -> Exchange:
    """Parse an ``ExchangeDetail`` JSON dict (returned by GET /activity/{pid}/inputs|outputs).

    The flow is a tagged sum: ``{"kind": "technosphere"|"biosphere"|"waste",
    "flow": <flow>}``. The flow's ``kind`` lines up with the exchange variant
    tag.
    """
    inner = ed["exchange"]
    flow_outer = ed.get("flow") or {}
    flow_kind = flow_outer.get("kind")
    flow_payload = flow_outer.get("flow") or {}
    unit = ed.get("exchangeUnitName", "")
    comment = _exchange_comment(ed, inner)
    tag = inner.get("tag")
    if tag == "TechnosphereExchange":
        if flow_kind not in (None, "technosphere"):
            raise ValueError(
                f"TechnosphereExchange carried flow kind {flow_kind!r}"
            )
        target = ed.get("targetActivity") or {}
        return TechnosphereExchange(
            flow_name=flow_payload.get("name", ""),
            amount=inner["amount"],
            unit=unit,
            role=TechRole(inner["role"]),
            target_activity_name=target.get("activityName"),
            target_location=target.get("location"),
            target_process_id=target.get("processId"),
            comment=comment,
            supplier_claim=SupplierClaim.from_json(inner.get("supplierClaim")),
        )
    if tag == "BiosphereExchange":
        if flow_kind not in (None, "biosphere"):
            raise ValueError(
                f"BiosphereExchange carried flow kind {flow_kind!r}"
            )
        return BiosphereExchange(
            flow_name=flow_payload.get("name", ""),
            compartment=Compartment.from_json(flow_payload.get("compartment")),
            amount=inner["amount"],
            unit=unit,
            direction=BioDirection(inner["direction"]),
            flow_id=inner["flowId"],
            comment=comment,
        )
    if tag == "WasteExchange":
        if flow_kind not in (None, "waste"):
            raise ValueError(
                f"WasteExchange carried flow kind {flow_kind!r}"
            )
        target = ed.get("targetActivity") or {}
        return WasteExchange(
            flow_name=flow_payload.get("name", ""),
            amount=inner["amount"],
            unit=unit,
            is_input=inner["isInput"],
            target_activity_name=target.get("activityName"),
            target_location=target.get("location"),
            target_process_id=target.get("processId"),
            comment=comment,
            supplier_claim=SupplierClaim.from_json(inner.get("supplierClaim")),
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

    ``native_id`` is the identifier the source file gave the dataset block this
    process was read from - SimaPro's ``Process identifier``, the number an
    EcoSpold 1 dataset is published under - which is what a reader copies to
    find the dataset back in the file it came from. It is ``None`` where the
    format has none, where the format names a dataset by the UUID
    ``process_id`` already spells, and against an engine older than wire
    revision 22.
    """

    process_id: str
    activity_name: str
    location: str
    unit: str
    description: list[str]
    classifications: dict[str, str]
    product_name: str | None
    product_amount: float | None
    product_unit: str | None
    all_products: list[Activity]
    exchanges: list[Exchange]
    native_id: str | None = None

    @classmethod
    def from_json(cls, d: dict) -> "ActivityDetail":
        # The /activity endpoint returns ActivityInfo: `activity` is the ActivityForAPI.
        pfa = d["activity"]
        return cls(
            process_id=pfa["processId"],
            activity_name=pfa["activityName"],
            location=pfa["location"],
            unit=pfa["unit"],
            description=pfa.get("description", []),
            classifications=pfa.get("classifications", {}),
            product_name=pfa.get("productName"),
            product_amount=pfa.get("productAmount"),
            product_unit=pfa.get("productUnit"),
            all_products=[Activity.from_json(a) for a in pfa.get("allProducts", [])],
            exchanges=[parse_exchange(e) for e in pfa.get("exchanges", [])],
            native_id=pfa.get("nativeId"),
        )

    @property
    def inputs(self) -> list[Exchange]:
        """Every input exchange: technosphere inputs and biosphere resources.

        Equivalent to filtering :attr:`exchanges` by ``e.is_input``. Mixed
        kinds: callers needing only one variant should use
        :attr:`technosphere_inputs` or filter manually.
        """
        return [e for e in self.exchanges if e.is_input]

    @property
    def outputs(self) -> list[Exchange]:
        """Every output exchange: products and biosphere emissions.

        Includes the reference product, coproducts (in allocated
        activities), and all biosphere emissions.
        """
        return [e for e in self.exchanges if not e.is_input]

    @property
    def technosphere_inputs(self) -> list[TechnosphereExchange]:
        """Only the technosphere inputs (ingredients from other activities).

        Excludes biosphere inputs (resource extractions) and waste
        outputs. The common case when answering "what does this activity
        consume from upstream?".
        """
        return [
            e for e in self.exchanges
            if isinstance(e, TechnosphereExchange) and e.is_input
        ]

    @property
    def allocation_percent(self) -> float | None:
        """This process's own allocation share (0..100), or ``None``.

        A multi-output process splits the parent activity's burden across its
        co-products; every :attr:`all_products` entry carries its share. This
        returns the share of *this* process (the entry whose ``process_id``
        matches), and ``None`` for single-output processes.
        """
        return next(
            (p.allocation_percent for p in self.all_products
             if p.process_id == self.process_id),
            None,
        )

    @property
    def is_allocated(self) -> bool:
        """True iff the activity splits its burden across several co-products.

        Reads the structured ``allocation_percent`` the engine sets on each
        :attr:`all_products` entry (authoritative), not the description text.
        """
        return sum(
            1 for p in self.all_products if p.allocation_percent is not None
        ) > 1


# ---------------------------------------------------------------------------
# Aggregation (for the /aggregate primitive)
# ---------------------------------------------------------------------------


class AggregateScope(_StrEnum):
    """What the ``/aggregate`` primitive groups over.

    ``DIRECT``: direct exchanges of the activity. ``SUPPLY_CHAIN``: the
    upstream activities reachable via cumulative flow. ``BIOSPHERE``: only
    biosphere flows in the supply chain. ``CONSUMPTION``: every scaled
    technosphere edge (who consumes what, in scaled units); the scope that
    answers "total X consumed upstream" without double counting, via
    ``filter_consumer_not``.
    """

    DIRECT = "direct"
    SUPPLY_CHAIN = "supply_chain"
    BIOSPHERE = "biosphere"
    CONSUMPTION = "consumption"


class AggregateOp(_StrEnum):
    """How values are reduced within a bucket.

    ``SUM_QUANTITY``: sum of quantities (default). ``COUNT``: number of
    matching entries. ``SHARE``: each bucket's percentage of the filtered
    total (0..100).
    """

    SUM_QUANTITY = "sum_quantity"
    COUNT = "count"
    SHARE = "share"


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
    scope: AggregateScope
    filtered_total: float
    filtered_unit: str | None
    filtered_count: int
    groups: list[AggregateGroup] = field(default_factory=list)

    @classmethod
    def from_json(cls, d: dict) -> "AggregateResult":
        return cls(
            scope=AggregateScope(d["scope"]),
            filtered_total=d["filteredTotal"],
            filtered_unit=d.get("filteredUnit"),
            filtered_count=d["filteredCount"],
            groups=[AggregateGroup.from_json(g) for g in d.get("groups", [])],
        )


# ---------------------------------------------------------------------------
# Substitution (input for /supply-chain, /inventory, /impacts)
# ---------------------------------------------------------------------------

@dataclass(frozen=True)
class Substitution:
    """Replace one supplier with another in the upstream supply chain.

    All fields are process_ids. ``consumer`` identifies which downstream
    consumer's input to rewrite, scoping the swap to one edge: the same
    upstream supplier can be replaced by different alternatives in different
    parts of the tree. Omit it (leave ``None``) to apply the swap globally,
    replacing the supplier on every consumer at once.

    Frozen so callers can put it in a set / dict key and re-use the same
    substitution across multiple calls without aliasing risk.
    """

    from_pid: str  # the supplier being replaced (wire: ``from``)
    to_pid: str  # the replacement supplier (wire: ``to``)
    consumer: str | None = None  # the consumer to scope to; None = global swap

    def to_wire(self) -> dict:
        """Serialise to the wire shape consumed by SubstitutionRequest."""
        d = {"from": self.from_pid, "to": self.to_pid}
        if self.consumer is not None:
            d["consumer"] = self.consumer
        return d


# ---------------------------------------------------------------------------
# Method catalog & flow mapping
# ---------------------------------------------------------------------------

@dataclass
class Method(FromJson):
    """One LCIA method, returned by :meth:`Client.list_methods`.

    Pass ``id`` (or ``name``, which the client resolves against the loaded
    methods) wherever a ``method_id`` is asked for. ``collection`` is the
    parent method collection (e.g. ``"ef-31"``); the client reads it off the
    resolved method, so it is worth passing to :meth:`Client.get_impacts` /
    :meth:`Client.get_impacts_batch` only to pin one of several loaded.
    """

    id: str
    name: str
    category: str
    unit: str
    factor_count: int
    collection: str


@dataclass
class ClassificationSystem(FromJson):
    """One classification system declared by a database.

    ``values`` are the distinct entries in this system; ``activity_count`` is
    how many activities carry at least one classification under this system
    (helps callers pick a worthwhile filter dimension).
    """

    name: str
    values: list[str] = field(default_factory=list)
    activity_count: int = 0


@dataclass(frozen=True)
class PresetFilter:
    """One filter triple inside a :class:`Preset`."""

    system: str
    value: str
    mode: MatchMode = MatchMode.CONTAINS

    @classmethod
    def from_json(cls, d: dict) -> "PresetFilter":
        return cls(system=d["system"], value=d["value"], mode=MatchMode(d["mode"]))


@dataclass
class Preset:
    """A named classification preset declared in the engine config.

    Apply by passing ``preset=preset.name`` to filtering endpoints (the engine
    expands it server-side into the ``filters`` triples).
    """

    name: str
    label: str
    description: str | None
    filters: list[PresetFilter] = field(default_factory=list)

    @classmethod
    def from_json(cls, d: dict) -> "Preset":
        return cls(
            name=d["name"],
            label=d["label"],
            description=d.get("description"),
            filters=[PresetFilter.from_json(f) for f in d.get("filters", [])],
        )


@dataclass
class ServerVersion:
    """Server build metadata returned by :meth:`Client.get_version`.

    ``git_tag`` is None for untagged dev builds. ``build_target`` names the
    platform triple the binary was compiled for (e.g. ``"x86_64-linux"``).
    ``wire_version`` is the engine's advertised JSON wire-format revision, or
    None for engines that predate it (everything up to v0.7.x).
    ``data_version`` names the reference-data bundle the engine reads; None
    when it reads none, or for engines that predate the field. Two engines of
    one version giving two scores for one calculation differ here.
    """

    version: str
    git_hash: str
    git_tag: str | None
    build_target: str
    wire_version: int | None = None
    data_version: str | None = None

    @classmethod
    def from_json(cls, d: dict) -> "ServerVersion":
        return cls(
            version=d["version"],
            git_hash=d["gitHash"],
            git_tag=d.get("gitTag") or None,
            build_target=d["buildTarget"],
            # Plain .get (not "... or None"): wire 0 is a distinct value, not
            # "absent". Absent (old engine) → None; present → the int verbatim.
            wire_version=d.get("wireVersion"),
            data_version=d.get("dataVersion"),
        )


@dataclass
class FlowMappingEntry(FromJson):
    """One DB biosphere flow and the CF (if any) assigned to it.

    ``cf_value`` is ``None`` when this DB flow has no characterization factor
    in the method: that flow contributes 0 to the score for the method.
    ``match_strategy`` records how the mapping was resolved (``"uuid"``,
    ``"cas"``, ``"name"``, ``"synonym"``, ``"proxy"``).
    """

    flow_id: str
    flow_name: str
    flow_category: str
    cf_value: float | None = None
    cf_flow_name: str | None = None
    match_strategy: str | None = None


@dataclass
class FlowMapping:
    """CF-coverage report for one method against the current database.

    ``matched_flows / total_flows`` is the coverage ratio: how many of the
    database's biosphere flows have a CF in this method. Mirrors the engine
    response of :meth:`Client.get_flow_mapping`.
    """

    method_name: str
    method_unit: str
    total_flows: int
    matched_flows: int
    flows: list[FlowMappingEntry] = field(default_factory=list)

    @property
    def coverage_pct(self) -> float:
        """Matched fraction expressed as 0..100. Returns 0 when total is 0."""
        return 100.0 * self.matched_flows / self.total_flows if self.total_flows else 0.0

    @classmethod
    def from_json(cls, d: dict) -> "FlowMapping":
        return cls(
            method_name=d["methodName"],
            method_unit=d["methodUnit"],
            total_flows=d["totalFlows"],
            matched_flows=d["matchedFlows"],
            flows=[FlowMappingEntry.from_json(f) for f in d.get("flows", [])],
        )


# ---------------------------------------------------------------------------
# Characterization, contributing flows / activities
# ---------------------------------------------------------------------------

@dataclass
class CharacterizationFactor(FromJson):
    """One characterization factor matched against a database biosphere flow.

    Returned in the ``factors`` list of :class:`CharacterizationResult`.
    ``match_strategy`` records how the CF was matched to the DB flow
    (``"uuid"``, ``"cas"``, ``"name"``, ``"synonym"``, ``"proxy"``).
    """

    method_flow_name: str
    cf_value: float
    cf_unit: str
    direction: str  # "Input" | "Output"
    db_flow_name: str
    flow_id: str
    flow_unit: str
    category: str
    match_strategy: str
    compartment: str | None = None


@dataclass
class ExplainedStep:
    """One rung of the factor-matching cascade, and what it made of the flow."""

    rung: str
    result: str  # "hit" | "miss" | "not_applicable" | "vetoed" | "ambiguous"
    veto: str | None = None

    @classmethod
    def from_json(cls, d: dict) -> "ExplainedStep":
        return cls(rung=d["rung"], result=d["result"], veto=d.get("veto"))


@dataclass
class ExplainedFlow:
    """The flow an explanation is about, as the cascade sees it."""

    id: str
    name: str
    unit: str
    category: str
    compartment: str | None = None
    cas: str | None = None

    @classmethod
    def from_json(cls, d: dict) -> "ExplainedFlow":
        return cls(
            id=d["id"],
            name=d["name"],
            unit=d["unit"],
            category=d["category"],
            compartment=d.get("compartment"),
            cas=d.get("cas"),
        )


@dataclass
class ExplainedMatch:
    """The factor that was served, and where it came from."""

    rung: str
    cf_value: float
    cf_unit: str
    method_flow_name: str
    match_strategy: str
    method_cas: str | None = None
    unit_conversion: str | None = None
    refusal: str | None = None

    @classmethod
    def from_json(cls, d: dict) -> "ExplainedMatch":
        return cls(
            rung=d["rung"],
            cf_value=d["cfValue"],
            cf_unit=d["cfUnit"],
            method_flow_name=d["methodFlowName"],
            match_strategy=d["matchStrategy"],
            method_cas=d.get("methodCas"),
            unit_conversion=d.get("unitConversion"),
            refusal=d.get("refusal"),
        )


@dataclass
class ExplainCFResult:
    """Result of :meth:`Client.explain_cf`.

    ``explanation`` is written by the engine: show it as it is rather than
    rewording the codes. The structured fields are for comparing, filtering or
    linking. ``outcome`` is ``"characterized"``, ``"conversion_refused"`` (a
    factor was found but the flow's unit cannot be converted to its basis, so
    the flow scores nothing) or ``"no_factor"``.
    """

    method: str
    method_unit: str
    flow: ExplainedFlow
    outcome: str
    explanation: list[str] = field(default_factory=list)
    match: ExplainedMatch | None = None
    steps_tried: list[ExplainedStep] = field(default_factory=list)
    regional_factor_count: int = 0

    @classmethod
    def from_json(cls, d: dict) -> "ExplainCFResult":
        raw_match = d.get("match")
        return cls(
            method=d["method"],
            method_unit=d["methodUnit"],
            flow=ExplainedFlow.from_json(d["flow"]),
            outcome=d["outcome"],
            explanation=list(d.get("explanation", [])),
            match=ExplainedMatch.from_json(raw_match) if raw_match else None,
            steps_tried=[ExplainedStep.from_json(s) for s in d.get("stepsTried", [])],
            regional_factor_count=d.get("regionalFactorCount", 0),
        )


@dataclass
class CharacterizationResult:
    """Result of :meth:`Client.get_characterization`.

    The engine truncates ``factors`` to ``shown`` rows (server-side ``limit``).
    ``matches`` is the unfiltered total: use :attr:`has_more` to detect when
    the slice is incomplete.
    """

    method: str
    unit: str
    matches: int  # total CFs matching the filter (before truncation)
    shown: int  # rows actually returned in `factors`
    factors: list[CharacterizationFactor] = field(default_factory=list)

    @property
    def has_more(self) -> bool:
        """True when the server truncated below ``matches``."""
        return self.shown < self.matches

    @classmethod
    def from_json(cls, d: dict) -> "CharacterizationResult":
        return cls(
            method=d["method"],
            unit=d["unit"],
            matches=d["matches"],
            shown=d["shown"],
            factors=[CharacterizationFactor.from_json(f) for f in d.get("factors", [])],
        )


@dataclass
class ActivityContribution(FromJson):
    """One upstream activity's contribution to an LCIA score.

    Returned in :class:`ContributingActivities.activities`. ``share_pct`` is
    the percentage of the total impact this activity contributes (0..100).
    """

    process_id: str
    activity_name: str
    product_name: str
    location: str
    contribution: float
    share_pct: float


@dataclass
class ContributingFlows:
    """Top elementary flows driving an LCIA score.

    Note: the engine does not report a total: ``top_flows`` is whatever the
    server returned under ``limit``, but pyvolca cannot tell whether more
    flows were truncated. If you need exhaustive coverage, pass a generous
    ``limit`` and inspect ``share_pct`` totals.
    """

    method: str
    unit: str
    total_score: float
    top_flows: list[FlowContribution] = field(default_factory=list)

    @classmethod
    def from_json(cls, d: dict) -> "ContributingFlows":
        return cls(
            method=d["method"],
            unit=d["unit"],
            total_score=d["totalScore"],
            top_flows=[FlowContribution.from_json(f) for f in d.get("topFlows", [])],
        )


@dataclass
class ContributingActivities:
    """Top upstream activities driving an LCIA score.

    Same engine-side limitation as :class:`ContributingFlows`: the server
    reports no total, so pyvolca cannot derive ``has_more``. Pass a generous
    ``limit`` and inspect ``share_pct`` if exhaustive coverage matters.
    """

    method: str
    unit: str
    total_score: float
    activities: list[ActivityContribution] = field(default_factory=list)

    @classmethod
    def from_json(cls, d: dict) -> "ContributingActivities":
        return cls(
            method=d["method"],
            unit=d["unit"],
            total_score=d["totalScore"],
            activities=[ActivityContribution.from_json(a) for a in d.get("activities", [])],
        )


# ---------------------------------------------------------------------------
# Inventory
# ---------------------------------------------------------------------------

@dataclass
class InventoryFlow:
    """One row of an inventory: a biosphere flow scaled to the functional unit.

    ``is_emission`` distinguishes outputs (releases) from inputs (resource
    extraction). ``flow_id`` is the database UUID; ``compartment`` is the
    medium label (e.g. ``"air/urban air"``) when the source dataset declared
    one. ``category`` is the engine-normalised category used for grouping.
    """

    flow_id: str
    flow_name: str
    quantity: float
    unit_name: str
    is_emission: bool
    category: str
    compartment: str | None = None

    @classmethod
    def from_json(cls, d: dict) -> "InventoryFlow":
        flow = d.get("flow") or {}
        # The engine's BiosphereFlow.compartment is structured ({name, sub})
        # under stripLowerPrefix; flatten to a display string for ergonomic use.
        compartment_obj = flow.get("compartment")
        if isinstance(compartment_obj, dict):
            name = compartment_obj.get("name") or ""
            sub = compartment_obj.get("sub")
            compartment = f"{name}/{sub}" if (name and sub) else (name or None)
        else:
            compartment = None
        return cls(
            flow_id=flow.get("id", ""),
            flow_name=flow.get("name", ""),
            quantity=d["quantity"],
            unit_name=d["unitName"],
            is_emission=d["isEmission"],
            category=d.get("category", ""),
            compartment=compartment,
        )


@dataclass
class InventoryStatistics:
    """Roll-up totals of an inventory result.

    ``emission_quantity`` and ``resource_quantity`` are sums by direction;
    ``total_quantity`` is the sum of absolute values. ``top_categories``
    lists ``(category_name, flow_count)`` pairs ordered by frequency.
    """

    total_quantity: float
    emission_quantity: float
    resource_quantity: float
    top_categories: list[tuple[str, int]] = field(default_factory=list)

    @classmethod
    def from_json(cls, d: dict) -> "InventoryStatistics":
        # Wire shape is [[name, count], ...]; convert to tuples for hashability.
        cats = [(c[0], c[1]) for c in d.get("topCategories", [])]
        return cls(
            total_quantity=d["totalQuantity"],
            emission_quantity=d["emissionQuantity"],
            resource_quantity=d["resourceQuantity"],
            top_categories=cats,
        )


@dataclass
class InventoryResult:
    """Life-cycle inventory of an activity: cumulative biosphere flows.

    Returned by :meth:`Client.get_inventory`. The engine does not paginate:
    ``flows`` is the full inventory (filtered by ``flow=`` substring when
    requested). ``statistics`` carries the per-direction roll-ups and the
    most-populated categories.

    ``root`` is the activity the inventory was computed for. ``total_flows``,
    ``emission_flows``, ``resource_flows`` mirror the engine's metadata block.
    """

    root: Activity
    total_flows: int
    emission_flows: int
    resource_flows: int
    flows: list[InventoryFlow]
    statistics: InventoryStatistics

    @classmethod
    def from_json(cls, d: dict) -> "InventoryResult":
        meta = d["metadata"]
        return cls(
            root=Activity.from_json(meta["rootActivity"]),
            total_flows=meta["totalFlows"],
            emission_flows=meta["emissionFlows"],
            resource_flows=meta["resourceFlows"],
            flows=[InventoryFlow.from_json(f) for f in d.get("flows", [])],
            statistics=InventoryStatistics.from_json(d["statistics"]),
        )


@dataclass
class FlowDetail:
    """Detail of one flow, returned by :meth:`Client.get_flow`.

    ``flow`` is the raw flow record: a tagged union (technosphere product,
    biosphere flow, waste flow, or unresolved) whose shape depends on its
    kind, kept as a dict rather than forced into one dataclass.
    ``usage_count`` is how many exchanges reference it.
    """

    flow: dict
    unit_name: str
    usage_count: int

    @classmethod
    def from_json(cls, d: dict) -> "FlowDetail":
        return cls(
            flow=d["flow"],
            unit_name=d["unitName"],
            usage_count=d["usageCount"],
        )


@dataclass
class MethodDetail(FromJson):
    """Detail of one LCIA method, returned by :meth:`Client.get_method`.

    ``factor_count`` is the number of characterization factors; ``methodology``
    and ``description`` are free-text metadata the source may or may not carry.
    """

    id: str
    name: str
    unit: str
    category: str
    factor_count: int
    description: "str | None" = None
    methodology: "str | None" = None


@dataclass
class MethodFactor(FromJson):
    """One characterization factor of a method (:meth:`Client.get_method_factors`).

    ``direction`` is the flow direction the factor applies to; ``value`` is the
    factor in the method's unit per the flow's unit. A method routinely holds
    several factors sharing one ``flow_name`` (the same substance emitted to
    air vs. water, or one regionalized factor per ``location``), so
    ``compartment``, ``location`` and ``unit`` are what tell them apart.
    Each is ``None`` when the source method does not carry that axis, or
    when the engine predates these fields.
    """

    flow_ref: str
    flow_name: str
    direction: str
    value: float
    unit: "str | None" = None
    compartment: "str | None" = None
    location: "str | None" = None


@dataclass
class UnmappedFlow(FromJson):
    """A method factor with no matching database flow (in :class:`MappingStatus`)."""

    flow_ref: str
    flow_name: str
    direction: str
    #: The compartment the method states, rendered as the factors route renders
    #: it ("air", "water/groundwater/long-term"); None when the row states none,
    #: or when the engine predates wire revision 13.
    compartment: str | None = None


@dataclass
class MappingStatus:
    """How a method's factors map onto a database's biosphere flows.

    Returned by :meth:`Client.get_mapping_status`. The ``mapped_by_*`` counts
    break the match cascade down by stage (UUID, then CAS, then name, then
    synonym); ``coverage`` is the matched percentage (0–100), and
    ``unmapped_flows`` lists the factors still without a database flow.

    Parsed by hand rather than via the snake-case mixin because the acronym
    runs (``mappedByUUID``, ``mappedByCAS``, ``dbBiosphereCount``) do not
    survive the generic camelCase→snake_case conversion.
    """

    method_id: str
    method_name: str
    total_factors: int
    mapped_by_uuid: int
    mapped_by_cas: int
    mapped_by_name: int
    mapped_by_synonym: int
    unmapped: int
    coverage: float
    db_biosphere_count: int
    unique_db_flows_matched: int
    unmapped_flows: list[UnmappedFlow]

    @classmethod
    def from_json(cls, d: dict) -> "MappingStatus":
        return cls(
            method_id=d["methodId"],
            method_name=d["methodName"],
            total_factors=d["totalFactors"],
            mapped_by_uuid=d["mappedByUUID"],
            mapped_by_cas=d["mappedByCAS"],
            mapped_by_name=d["mappedByName"],
            mapped_by_synonym=d["mappedBySynonym"],
            unmapped=d["unmapped"],
            coverage=d["coverage"],
            db_biosphere_count=d["dbBiosphereCount"],
            unique_db_flows_matched=d["uniqueDbFlowsMatched"],
            unmapped_flows=[UnmappedFlow.from_json(u) for u in d["unmappedFlows"]],
        )


@dataclass
class CollectionCoverage(FromJson):
    """How much of one database a whole method collection characterizes.

    Returned by :meth:`Client.get_collection_coverage`.
    ``characterized_flows`` counts distinct emission and resource flows that
    at least one of the collection's methods resolves a factor for, with the
    same lookup scoring uses, a figure no sum over the per-method
    :class:`MappingStatus` values can recover, since the methods overlap.
    """

    collection: str
    db_name: str
    total_flows: int
    characterized_flows: int


# ---------------------------------------------------------------------------
# Authoring: what you send when you write an activity
# ---------------------------------------------------------------------------
#
# These are the only input types in this module; everything above describes a
# response. They mirror what the engine accepts and validate what it can check
# locally, so an obviously malformed line fails before a round trip rather than
# after one.


@dataclass(frozen=True)
class TechInput:
    """One product an activity consumes, named by the process that supplies it.

    ``provider`` is a ``process_id`` (``activityUUID_productUUID``, or a bare
    activity UUID when that activity has a single product), the same address
    every read endpoint hands out. The flow follows from the supplier, so it is
    never stated separately. ``unit`` defaults to the supplier's own reference
    unit; another one is fine as long as it converts.
    """

    provider: str
    amount: float
    unit: str | None = None
    comment: str | None = None

    def to_wire(self) -> dict:
        return _drop_none(
            {
                "provider": self.provider,
                "amount": self.amount,
                "unit": self.unit,
                "comment": self.comment,
            }
        )


@dataclass(frozen=True)
class BioExchange:
    """One resource taken from the environment, or one emission released into it.

    Name the flow one way or the other, never both: ``flow`` is the identifier
    of a flow the database already has, which :meth:`Client.search_flows` and
    :attr:`BiosphereExchange.flow_id` both return, while ``name`` with
    ``compartment`` and ``unit`` names one in words. Use the two constructors
    rather than the fields, :meth:`from_id` and :meth:`from_name`, which is why
    passing both or neither raises here instead of at the server.

    A biosphere amount is never converted, so an exchange states its amount in
    the flow's own unit.
    """

    direction: BioDirection
    amount: float
    flow: str | None = None
    name: str | None = None
    compartment: str | None = None
    sub_compartment: str | None = None
    unit: str | None = None
    comment: str | None = None

    def __post_init__(self) -> None:
        if (self.flow is None) == (self.name is None):
            raise ValueError(
                "a biosphere exchange names its flow by identifier (flow=...) "
                "or in words (name=..., compartment=..., unit=...), "
                "not both and not neither"
            )
        if self.name is not None and self.compartment is None:
            raise ValueError(
                f"biosphere flow {self.name!r} needs a compartment "
                "(air, water, soil, natural resource)"
            )
        if self.name is not None and self.unit is None:
            raise ValueError(
                f"biosphere flow {self.name!r} needs a unit "
                "(it is half of the flow's identity, so it cannot be defaulted)"
            )

    @classmethod
    def from_id(
        cls,
        flow: str,
        direction: BioDirection | str,
        amount: float,
        *,
        unit: str | None = None,
        comment: str | None = None,
    ) -> "BioExchange":
        """An exchange on a flow the database already declares, by its identifier.

        :meth:`Client.search_flows` is where identifiers come from.
        """
        return cls(
            direction=BioDirection(direction),
            amount=amount,
            flow=flow,
            unit=unit,
            comment=comment,
        )

    @classmethod
    def from_name(
        cls,
        name: str,
        compartment: str,
        direction: BioDirection | str,
        amount: float,
        unit: str,
        *,
        sub_compartment: str | None = None,
        comment: str | None = None,
    ) -> "BioExchange":
        """An exchange on the flow this name, compartment and unit address.

        That is the flow the database already declares under them when it has
        one, which is what keeps an exchange written from an inventory pointing
        at the curated flow rather than at a twin of it. ``sub_compartment`` is
        part of the address, not a decoration: a flow recorded in
        ``water/river`` is reached with ``sub_compartment="river"`` and not by
        ``water`` alone.

        A name nothing answers to brings a flow into the database, and since no
        characterization factor matches a brand-new flow by identity, the
        engine returns a warning alongside the write rather than refusing it.
        A name several flows answer to in the unit stated is refused, listing
        their identifiers, so the exchange can name the one it means; two flows
        of one name in two units are told apart by the unit itself.
        """
        return cls(
            direction=BioDirection(direction),
            amount=amount,
            name=name,
            compartment=compartment,
            sub_compartment=sub_compartment,
            unit=unit,
            comment=comment,
        )

    @classmethod
    def existing(
        cls,
        flow: str,
        direction: BioDirection | str,
        amount: float,
        *,
        unit: str | None = None,
        comment: str | None = None,
    ) -> "BioExchange":
        """Retired name of :meth:`from_id`. Still works, and says so."""
        warn_renamed("BioExchange.existing", "BioExchange.from_id")
        return cls.from_id(flow, direction, amount, unit=unit, comment=comment)

    @classmethod
    def introducing(
        cls,
        name: str,
        compartment: str,
        direction: BioDirection | str,
        amount: float,
        unit: str,
        *,
        sub_compartment: str | None = None,
        comment: str | None = None,
    ) -> "BioExchange":
        """Retired name of :meth:`from_name`. Still works, and says so.

        It was named for what it did when a name reached nothing: bring the
        flow into the database. It now reaches the flow the database already
        declares under that name, and only introduces one when nothing does.
        """
        warn_renamed("BioExchange.introducing", "BioExchange.from_name")
        return cls.from_name(
            name,
            compartment,
            direction,
            amount,
            unit,
            sub_compartment=sub_compartment,
            comment=comment,
        )

    def to_wire(self) -> dict:
        return _drop_none(
            {
                "flow": self.flow,
                "name": self.name,
                "compartment": self.compartment,
                "subCompartment": self.sub_compartment,
                "direction": self.direction.value,
                "amount": self.amount,
                "unit": self.unit,
                "comment": self.comment,
            }
        )


@dataclass(frozen=True)
class WasteOutput:
    """One residue an activity hands to a treatment process.

    ``provider`` names that treatment process, exactly as a :class:`TechInput`
    names its producer.
    """

    provider: str
    amount: float
    unit: str | None = None
    comment: str | None = None

    def to_wire(self) -> dict:
        return _drop_none(
            {
                "provider": self.provider,
                "amount": self.amount,
                "unit": self.unit,
                "comment": self.comment,
            }
        )


@dataclass(frozen=True)
class ActivityInput:
    """An activity as you write it: the body of :meth:`Client.create_activities`.

    The inventory is three lists rather than one, so a field that means
    something on a supplier link cannot be sent on an emission.

    You do not choose the ``process_id``. The engine mints it from the name,
    the location, and the product name and unit, which is what makes writing
    the same activity twice a correction of one row rather than two. One
    reference product per activity: coproducts and allocation are not supported
    yet, and this type does not pretend they are.
    """

    name: str
    location: str
    product_name: str
    product_amount: float
    product_unit: str
    description: list[str] = field(default_factory=list)
    inputs: list[TechInput] = field(default_factory=list)
    biosphere: list[BioExchange] = field(default_factory=list)
    waste_outputs: list[WasteOutput] = field(default_factory=list)

    def to_wire(self) -> dict:
        return {
            "name": self.name,
            "location": self.location,
            "description": list(self.description),
            "productName": self.product_name,
            "productAmount": self.product_amount,
            "productUnit": self.product_unit,
            "inputs": [i.to_wire() for i in self.inputs],
            "biosphere": [b.to_wire() for b in self.biosphere],
            "wasteOutputs": [w.to_wire() for w in self.waste_outputs],
        }


@dataclass(frozen=True)
class ExchangeSelector:
    """Which lines of an inventory an edit is about.

    ``kind`` is ``"input"``, ``"waste"`` or ``"biosphere"``. The first two name
    their provider by process id; the third names its flow by identity. There
    is no kind for the reference product or a coproduct: changing those changes
    what the activity *is*, which is not what an inventory edit does.

    A selector may name several lines, and then it applies to all of them;
    :meth:`Client.edit_exchanges` reports how many. Naming none is refused by
    the engine rather than passed off as done.
    """

    kind: str
    provider: str | None = None
    flow: str | None = None

    def __post_init__(self) -> None:
        if self.kind not in ("input", "waste", "biosphere"):
            raise ValueError(
                f"unknown selector kind {self.kind!r} (expected input, waste or biosphere)"
            )
        if self.kind == "biosphere":
            if self.flow is None or self.provider is not None:
                raise ValueError(
                    "a biosphere selector names its flow (flow=...), and nothing else"
                )
        elif self.provider is None or self.flow is not None:
            raise ValueError(
                f"a selector of kind {self.kind} names its provider (provider=...), and nothing else"
            )

    @classmethod
    def input_from(cls, provider: str) -> "ExchangeSelector":
        """A technosphere input, by the process id of what supplies it."""
        return cls(kind="input", provider=provider)

    @classmethod
    def waste_to(cls, provider: str) -> "ExchangeSelector":
        """A waste output, by the process id of the treatment it goes to."""
        return cls(kind="waste", provider=provider)

    @classmethod
    def biosphere_flow(cls, flow: str) -> "ExchangeSelector":
        """A biosphere exchange, by flow id."""
        return cls(kind="biosphere", flow=flow)

    def to_wire(self) -> dict:
        return _drop_none({"kind": self.kind, "provider": self.provider, "flow": self.flow})


@dataclass(frozen=True)
class SetAmount:
    """The lines to restate, and what to restate them to."""

    select: ExchangeSelector
    amount: float

    def to_wire(self) -> dict:
        return {"select": self.select.to_wire(), "amount": self.amount}


def _drop_none(d: dict) -> dict:
    """Omit absent optional fields rather than sending explicit nulls."""
    return {k: v for k, v in d.items() if v is not None}
