"""HTTP client for the VoLCA HTTP API, dispatched by OpenAPI operationId.

Design
------
The client fetches the engine's OpenAPI spec once (from
``GET /api/v1/openapi.json``) and builds a dispatch table keyed on
``operationId``. Each public method is a thin typed wrapper that calls
``self._call("operation_id", **python_kwargs)`` and lets the dispatcher
handle path substitution, query-string assembly, and JSON decoding.

This removes ~250 lines of hand-written query-param plumbing and
decouples pyvolca's PyPI release cadence from engine endpoint changes:
when the engine renames a query parameter, pyvolca picks it up on the
next spec fetch with no code change. See ``README.md`` for the
user-facing view.

Kwarg name translation
----------------------
Python wrappers use snake_case kwargs (``process_id``, ``min_quantity``).
The OpenAPI spec carries the original Servant names (``processId``,
``min-quantity``). ``_call`` canonicalizes by trying each kwarg name
against a list of candidate wire names:

  1. The name as-is (``name``, ``limit``)
  2. snake_case → camelCase (``process_id`` → ``processId``)
  3. snake_case → kebab-case (``min_quantity`` → ``min-quantity``)

and picks the first that matches a spec parameter. Unknown kwargs raise
``VoLCAError`` so typos are caught at call time.

Substitutions
-------------
If a wrapper is called with ``substitutions=[{...}]``, ``_call`` upgrades
the operation from GET to POST and sends the substitution body. Works
transparently for ``get_inventory``, ``get_supply_chain``, and
``get_impacts``: all the endpoints that have POST-with-substitutions
variants in the Servant API.
"""

from __future__ import annotations

import urllib.parse
import uuid
import warnings
from concurrent.futures import ThreadPoolExecutor
from enum import Enum
from pathlib import Path
from typing import Any, Iterable, Literal, Sequence, get_args

import requests

from .types import (
    Activity,
    ActivityDetail,
    ActivityInput,
    AggregateOp,
    AggregateResult,
    AggregateScope,
    BatchScores,
    BioExchange,
    CharacterizationResult,
    ClassificationFilter,
    ClassificationSystem,
    CollectionCoverage,
    ConsumerResult,
    ConsumersResponse,
    ContributingActivities,
    ContributingFlows,
    DatabaseInfo,
    DatabaseStatus,
    Exchange,
    ExchangeSelector,
    ExplainCFResult,
    Flow,
    FlowDetail,
    FlowMapping,
    InventoryResult,
    LCIABatchResult,
    LCIAResult,
    MappingStatus,
    MatchMode,
    MatchModeLike,
    Method,
    MethodDetail,
    MethodFactor,
    PathResult,
    Preset,
    SearchCounts,
    SearchResults,
    SensitivityResult,
    ServerVersion,
    SetAmount,
    Substitution,
    SupplyChain,
    TechInput,
    WasteOutput,
    parse_exchange_detail,
)
from . import _compat


class VoLCAError(Exception):
    """Error from the VoLCA API."""

    def __init__(self, message: str, status_code: int | None = None, body: str = ""):
        self.status_code = status_code
        self.body = body
        super().__init__(message)


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------


def _snake_to_camel(s: str) -> str:
    """``process_id`` → ``processId``."""
    head, *tail = s.split("_")
    return head + "".join(p.capitalize() for p in tail)


def _snake_to_kebab(s: str) -> str:
    """``min_quantity`` → ``min-quantity``."""
    return s.replace("_", "-")


def _is_uuid(s: str) -> bool:
    """Whether ``s`` is a UUID written the way the engine's URLs accept it.

    Canonical dashed form only: Python also reads ``{...}``, ``urn:uuid:...``
    and the undashed 32 characters, which the engine's parser refuses, and
    letting those through would send the caller a 400 from the far end
    instead of a message naming what is wrong.
    """
    try:
        return str(uuid.UUID(s)) == s.casefold()
    except (ValueError, AttributeError, TypeError):
        return False


def _candidate_wire_names(py_name: str) -> list[str]:
    """Wire names to try for a Python kwarg, in priority order.

    The same Python name may map to different wire forms depending on
    whether the spec places it in ``path`` (Servant uses camelCase) or in
    ``query`` (Servant sometimes uses snake_case, sometimes kebab-case,
    sometimes camelCase: the aggregate endpoint is snake_case, for
    example). Return every plausible form; the caller matches against
    the spec's parameter list.
    """
    seen = []
    for cand in (py_name, _snake_to_camel(py_name), _snake_to_kebab(py_name)):
        if cand not in seen:
            seen.append(cand)
    return seen


_EXPORT_FORMATS = frozenset(
    {"simapro", "ecospold1", "ecospold2", "ilcd", "brightway"}
)
"""Target keywords accepted by ``POST /api/v1/db/{dbName}/export``.

Mirrors the engine's ``parseExportFormat`` (case-folded). Validated
client-side so a typo fails before the round-trip with the same message
shape the engine would have returned."""


_METHOD_EXPORT_FORMATS = frozenset({"simapro", "csv", "openlca", "ilcd"})
"""Target keywords accepted by ``POST /api/v1/method-collections/{name}/export``.

Mirrors the engine's ``parseMethodExportFormat``: a space of its own,
smaller than the database export formats."""


RefDataKind = Literal["flow-synonyms", "compartment-mappings", "units"]
"""Reference-data families sharing the same ``/api/v1/{kind}`` URL scheme.

Each family exposes list / load / unload / delete / upload at the same
paths, so the client's reference-data methods take the ``kind`` as an
argument instead of cloning five methods per family. The Literal lets
pyright reject a typo at check time; ``_ref_kind`` still validates at
runtime for untyped callers."""

_REF_DATA_KINDS = frozenset(get_args(RefDataKind))


def _ref_kind(kind: str) -> str:
    """Validate a reference-data ``kind``, or raise before any request."""
    if kind not in _REF_DATA_KINDS:
        raise VoLCAError(
            f"unknown reference data kind: {kind!r} "
            f"(expected {'|'.join(sorted(_REF_DATA_KINDS))})"
        )
    return kind


_FORMATTED_SCALARS = (str, int, float)


def _format_query_value(value: Any) -> Any:
    """Convert a Python value to a query-string-ready form.

    Booleans become ``"true"``/``"false"``. Lists remain lists (requests
    encodes repeated keys for list values). :class:`enum.Enum` members are
    serialised via their ``.value`` so StrEnums round-trip cleanly.
    Everything else is stringified.
    """
    if value is None:
        return None
    if isinstance(value, bool):
        return "true" if value else "false"
    if isinstance(value, Enum):
        return _format_query_value(value.value)
    if isinstance(value, _FORMATTED_SCALARS):
        return str(value)
    if isinstance(value, list):
        return [_format_query_value(v) for v in value]
    return str(value)


def _coerce_class_filter(c: dict | tuple) -> dict:
    """Normalize one classification filter to the wire ``DeleteClassFilter``.

    Accepts a ``{"system", "value", "exact"?}`` dict or a
    ``(system, value, exact?)`` tuple. ``exact`` defaults to False. Raises
    VoLCAError on a malformed entry rather than silently dropping fields.
    """
    if isinstance(c, dict):
        missing = {"system", "value"} - set(c)
        if missing:
            raise VoLCAError(
                f"Classification filter missing keys: {sorted(missing)}. "
                "Expected {'system', 'value', 'exact'?}."
            )
        return {
            "system": c["system"],
            "value": c["value"],
            "exact": bool(c.get("exact", False)),
        }
    if isinstance(c, (tuple, list)):
        if len(c) not in (2, 3):
            raise VoLCAError(
                f"Classification filter tuple must be (system, value[, exact]); "
                f"got {len(c)} items."
            )
        system, value = c[0], c[1]
        exact = bool(c[2]) if len(c) == 3 else False
        return {"system": system, "value": value, "exact": exact}
    raise VoLCAError(
        f"Classification filter must be a dict or tuple, got {type(c).__name__}."
    )


def _resolve_page_args(
    page: int | None,
    page_size: int | None,
    limit: int | None,
    offset: int | None,
) -> tuple[int | None, int]:
    """Reconcile the page/page_size convenience kwargs with wire-level limit/offset.

    Returns ``(wire_limit, wire_offset)``. ``wire_limit`` may be None, which
    leaves it off the request entirely so the engine applies its own
    default page size (matching what the web UI gets).

    Raises VoLCAError if the kwargs combination cannot map to a single
    unambiguous ``(offset, limit)``. The page-style and wire-style families
    cannot mix, and ``page=N`` without ``page_size`` is rejected: we refuse
    to fabricate a page size, since assuming one would silently misalign
    the offset whenever the engine's default differs.
    """
    page_style = page is not None or page_size is not None
    wire_style = offset is not None  # bare `limit=N` is just a cap, not pagination
    if page_style and wire_style:
        raise VoLCAError(
            "Mix of page-style (page=, page_size=) and wire-style (offset=) "
            "pagination kwargs. Use one or the other."
        )
    if page is not None and page_size is None:
        raise VoLCAError(
            "page=N requires an explicit page_size=M: offset cannot be derived "
            "from page alone without committing to a page size."
        )
    if page is not None and page < 1:
        raise VoLCAError(f"page must be >= 1, got {page}")
    if page_size is not None and page_size < 1:
        raise VoLCAError(f"page_size must be >= 1, got {page_size}")
    if page_style:
        # page_size alone (no page=) means "page 1 with this size".
        return page_size, ((page or 1) - 1) * (page_size or 0)
    return limit, offset or 0


SubstitutionLike = Substitution | dict
"""A :class:`Substitution` or a legacy ``{"from", "to", "consumer"}`` dict.

``consumer`` is optional: omit it for a global swap. The dict form is
accepted for backwards-compat one-liner ergonomics; the typed form is
preferred (catches typos at construction time)."""


def _substitution_body(substitutions: list[SubstitutionLike]) -> dict:
    """Build the request body for substitution endpoints.

    Accepts :class:`Substitution` instances or the legacy dict form with
    ``from`` / ``to`` keys and an optional ``consumer`` key (omit it for a
    global swap).
    """

    def coerce(s: SubstitutionLike) -> dict:
        if isinstance(s, Substitution):
            return s.to_wire()
        # dict form: validate the required keys here so typos like
        # ``"comsumer"`` fail with a clear error rather than at the engine.
        missing = {"from", "to"} - set(s)
        if missing:
            raise VoLCAError(
                f"Substitution dict missing keys: {sorted(missing)}. "
                "Use a Substitution(from_pid=, to_pid=, consumer=) instead."
            )
        body = {"from": s["from"], "to": s["to"]}
        if "consumer" in s:
            body["consumer"] = s["consumer"]
        return body

    return {"substitutions": [coerce(s) for s in substitutions]}


# ---------------------------------------------------------------------------
# Spec loader
# ---------------------------------------------------------------------------


class _Operation:
    """Dispatch entry for a single operationId.

    Immutable after the spec is parsed. Built by ``_parse_spec``.
    """

    __slots__ = ("operation_id", "method", "path_template", "path_params", "query_params")

    def __init__(
        self,
        operation_id: str,
        method: str,
        path_template: str,
        path_params: list[str],
        query_params: list[str],
    ):
        self.operation_id = operation_id
        self.method = method
        self.path_template = path_template
        self.path_params = path_params
        self.query_params = query_params

    @property
    def wire_names(self) -> set[str]:
        return set(self.path_params) | set(self.query_params)


def _parse_spec(spec: dict) -> dict[str, _Operation]:
    """Walk an OpenAPI spec and index operations by ``operationId``.

    Only operations that carry an explicit ``operationId`` are indexed;
    infrastructure endpoints like ``/auth`` and ``/version`` (which have
    no matching entry in ``API.Resources``) are skipped.
    """
    ops: dict[str, _Operation] = {}
    for path, item in spec.get("paths", {}).items():
        for method, op in item.items():
            if not isinstance(op, dict):
                continue
            operation_id = op.get("operationId")
            if not operation_id:
                continue
            path_params: list[str] = []
            query_params: list[str] = []
            for param in op.get("parameters", []):
                loc = param.get("in")
                name = param.get("name")
                if loc == "path":
                    path_params.append(name)
                elif loc == "query":
                    query_params.append(name)
            ops[operation_id] = _Operation(
                operation_id=operation_id,
                method=method.upper(),
                path_template=path,
                path_params=path_params,
                query_params=query_params,
            )
    return ops


# ---------------------------------------------------------------------------
# Public client
# ---------------------------------------------------------------------------


class Client:
    """HTTP client for the VoLCA HTTP API.

    Usage::

        c = Client(db="agribalyse-3.2", password="1234")
        plants = c.search_activities(name="at plant")
        chain = c.get_supply_chain(plants[0].process_id, name="at farm")

    Substitutions can be passed to ``get_supply_chain``, ``get_inventory``,
    and ``get_impacts`` to compute results with a different upstream
    supplier, fast::

        subs = [{"from": old_pid, "to": new_pid, "consumer": consumer_pid}]
        result = c.get_impacts(pid, method_id=mid, substitutions=subs)
    """

    def __init__(
        self,
        base_url: str = "http://localhost:8080",
        db: str = "",
        password: str = "",
    ):
        """Build a client targeting one VoLCA server and one default database.

        Args:
            base_url: Server URL (no trailing slash needed).
            db: Default database name. Used by every operation that takes a
                ``dbName`` path capture; pass ``db_name=`` per call to override.
            password: Bearer token sent in the Authorization header on every
                request. The Server class reads this from ``server.password``
                in the engine TOML.
        """
        self.base_url = base_url.rstrip("/")
        self.db = db
        self._session = requests.Session()
        self._session.headers["Accept"] = "application/json"
        if password:
            self._session.headers["Authorization"] = f"Bearer {password}"
        # Lazily fetched on first _call() invocation.
        self._operations: dict[str, _Operation] | None = None
        # One-shot wire-compatibility gate (see _ensure_compatible). The
        # engine's advertised wire revision is cached alongside so per-feature
        # gates (_require_wire) don't refetch it.
        self._checked = False
        self._server_wire: int | None = None
        # Loaded methods, fetched on the first call that has to resolve a
        # method or a collection (see _resolve_method), emptied whenever a
        # collection is loaded or unloaded. Emptied and refilled in place,
        # never rebound: methods belong to the engine, not to a database, so
        # the clones use() hands out share this one list and one invalidation.
        self._methods_cache: list[Method] = []

    # -- Spec / dispatch plumbing --

    def _load_operations(self) -> dict[str, _Operation]:
        """Fetch the OpenAPI spec and build the dispatch table (cached)."""
        if self._operations is None:
            self._ensure_compatible()
            spec = self._json(self._session.get(f"{self.base_url}/api/v1/openapi.json"))
            self._operations = _parse_spec(spec)
        return self._operations

    def _ensure_compatible(self) -> None:
        """One-shot wire-compatibility gate, run before the first real call.

        Placed inside the spec-fetch branch so it fires once per client, right
        before we first depend on the engine's wire. A client handed a
        preloaded operation table (the offline fixtures) never reaches it
        through dispatch; only a wire-gated capability (:meth:`_require_wire`)
        forces the check, since it must ask the live engine.
        ``get_version`` is a direct GET, so this does not recurse.
        """
        if self._checked:
            return
        sv = self.get_version()
        _compat.check(sv)
        self._server_wire = sv.wire_version
        self._checked = True

    def _require_wire(self, minimum: int, feature: str, engine_hint: str) -> None:
        """Refuse ``feature`` unless the engine's wire revision is >= ``minimum``.

        For capabilities where an older engine would not merely lack the
        feature but silently misread the request (it drops the unknown wire
        key), the request must never be sent at all. ``engine_hint`` is the
        first engine release advertising ``minimum``, shown in the error.
        """
        self._ensure_compatible()
        wire = self._server_wire
        if wire is None or wire < minimum:
            spoken = "pre-1" if wire is None else str(wire)
            raise VoLCAError(
                f"{feature} needs engine wire revision >= {minimum}; this "
                f"engine speaks wire {spoken}. Upgrade the engine to "
                f">= v{engine_hint}."
            )

    def refresh_stubs(self) -> None:
        """Fetch the OpenAPI spec from the server and refresh the dispatch table.

        Also regenerates the `.pyi` type stubs in the installed pyvolca
        package directory so IDE autocomplete reflects the current engine.
        Useful when the engine is upgraded without reinstalling pyvolca.

        This is the explicit "the engine was upgraded" path, the likeliest
        place to meet a wire *change*, so it forgets the cached wire and
        re-runs the gate against the live engine before fetching a spec
        pyvolca can't decode. Without the reset, a client that first met an
        older engine would keep refusing wire-gated capabilities after an
        in-place upgrade.
        """
        self._checked = False
        self._server_wire = None
        self._ensure_compatible()
        spec = self._json(self._session.get(f"{self.base_url}/api/v1/openapi.json"))
        self._operations = _parse_spec(spec)
        from . import _stub_gen
        _stub_gen.write_stubs_for_spec(spec)

    # -- Method resolution --
    #
    # Every LCIA URL carries a collection *and* a method: the engine keys its
    # CF caches by collection, since one UUID can live in several. Callers
    # rarely know or care which collection carries "Water use", so these two
    # helpers answer it from the engine instead of making the caller guess.

    def _loaded_methods(self, refresh: bool = False) -> list[Method]:
        """Every method the engine has loaded, cached on the client.

        Empty means "not looked yet", so an engine that answered nothing once
        is asked again rather than refusing every later call for the life of
        the client.
        """
        # ponytail: no lock, so a thread fan-out warming a cold cache repeats
        # the request rather than waiting; add one if the extra GETs ever show.
        if refresh or not self._methods_cache:
            self._methods_cache[:] = self.list_methods()
        return self._methods_cache

    def _resolve_method(
        self, method_id: str, collection: str | None
    ) -> tuple[str, str]:
        """Map a method UUID *or name* to the ``(collection, uuid)`` a URL needs.

        A UUID with an explicit collection is passed straight through: no
        lookup, no round-trip. Anything else is matched against the engine's
        loaded methods, which is what lets ``get_impacts(pid, "Water use")``
        work without the caller knowing the collection. An unknown or
        ambiguous name is an error naming the candidates, never a guess.
        """
        if not isinstance(method_id, str):
            raise VoLCAError(
                "method_id must be a method UUID or a method name, got "
                f"{type(method_id).__name__}."
            )
        if collection is not None and _is_uuid(method_id):
            return collection, method_id

        needle = method_id.strip().casefold()

        def match(methods: list[Method]) -> list[Method]:
            return [
                m
                for m in methods
                # The engine writes UUIDs in lower case and reads them in
                # either, so an id is compared the way a name is.
                if (m.id.casefold() == needle or m.name.casefold() == needle)
                and (collection is None or m.collection == collection)
            ]

        warm = bool(self._methods_cache)
        hits = match(self._loaded_methods())
        if not hits and warm:
            # A collection may have been loaded since we last looked.
            hits = match(self._loaded_methods(refresh=True))
        if not hits:
            where = "" if collection is None else f" in collection {collection!r}"
            raise VoLCAError(
                f"No loaded method named or identified by {method_id!r}{where}. "
                "See list_methods()."
            )
        if len({(m.collection, m.id) for m in hits}) > 1:
            candidates = ", ".join(sorted(f"{m.collection}/{m.id}" for m in hits))
            raise VoLCAError(
                f"{method_id!r} matches several loaded methods ({candidates}). "
                "Pass collection= to choose one."
            )
        return hits[0].collection, hits[0].id

    def _method_uuid(self, method_id: str) -> str:
        """The UUID of a method given by UUID or name, for URLs with no collection.

        A UUID goes straight to the URL: these routes look a method up across
        every loaded collection themselves, so resolving here would only add a
        round-trip and turn the engine's own answer into a client-side refusal.
        """
        if _is_uuid(method_id):
            return method_id
        return self._resolve_method(method_id, None)[1]

    def _resolve_collection(self, collection: str | None) -> str:
        """The collection a whole-collection call runs against.

        With a single collection loaded there is nothing to choose; with
        several, the choice is the caller's, and refusing names them rather
        than picking one.
        """
        if collection is not None:
            return collection
        names = sorted({m.collection for m in self._loaded_methods()})
        if len(names) == 1:
            return names[0]
        if not names:
            raise VoLCAError(
                "No loaded collection carries a method. See "
                "list_method_collections() and load_method_collection()."
            )
        raise VoLCAError(
            f"Several method collections are loaded ({', '.join(names)}); "
            "pass collection= to choose one."
        )

    def _call(
        self,
        operation_id: str,
        *,
        substitutions: list[SubstitutionLike] | None = None,
        body: dict | None = None,
        **kwargs: Any,
    ) -> Any:
        """Dispatch an OpenAPI operation by ``operationId``.

        Path captures and query parameters come from the spec. Python
        kwarg names are canonicalized against the spec's parameter list
        (see :func:`_candidate_wire_names`). ``db_name`` defaults to the
        instance's ``self.db`` when the operation expects ``dbName`` and
        it wasn't explicitly passed.

        ``body`` is the JSON request body for operations the spec declares
        as POST (e.g. ``compute_sensitivity``, ``score_activities``). If
        ``substitutions`` is given instead, the operation is upgraded from
        GET to POST with a ``SubstitutionRequest`` body; the two are
        mutually exclusive and ``substitutions`` wins.
        """
        ops = self._load_operations()
        op = ops.get(operation_id)
        if op is None:
            raise VoLCAError(
                f"Unknown operationId {operation_id!r} (not in OpenAPI spec). "
                f"Is the engine outdated, or does this operation only exist in MCP?"
            )

        # Auto-inject db_name from instance state if the op needs it and
        # the caller didn't pass it explicitly.
        if "dbName" in op.path_params and "db_name" not in kwargs and "dbName" not in kwargs:
            if not self.db:
                raise VoLCAError(
                    f"Operation {operation_id!r} requires a database but "
                    f"Client(db=...) is empty. Pass db_name= explicitly or "
                    f"construct the client with db=...."
                )
            kwargs["db_name"] = self.db

        # Drop None-valued kwargs: they shouldn't become query string entries.
        kwargs = {k: v for k, v in kwargs.items() if v is not None}

        # Split kwargs into path captures vs. query params, canonicalizing names.
        path_values: dict[str, Any] = {}
        query_values: list[tuple[str, Any]] = []
        unknown: list[str] = []

        for py_name, value in kwargs.items():
            wire_name = _resolve_wire_name(py_name, op)
            if wire_name is None:
                unknown.append(py_name)
                continue
            if wire_name in op.path_params:
                path_values[wire_name] = value
            else:
                # Query param: list values emit repeated keys.
                formatted = _format_query_value(value)
                if isinstance(formatted, list):
                    for item in formatted:
                        query_values.append((wire_name, item))
                else:
                    query_values.append((wire_name, formatted))

        if unknown:
            raise VoLCAError(
                f"Operation {operation_id!r} got unknown kwargs: {sorted(unknown)}. "
                f"Accepted parameters: path={sorted(op.path_params)}, "
                f"query={sorted(op.query_params)}."
            )

        # Verify all required path captures were supplied. Anything missing
        # is a bug in the calling wrapper, but surface it clearly.
        missing_path = [p for p in op.path_params if p not in path_values]
        if missing_path:
            raise VoLCAError(
                f"Operation {operation_id!r} missing required path captures: "
                f"{missing_path}"
            )

        # Substitute path captures. The spec uses `{name}` placeholders.
        url_path = op.path_template
        for name, value in path_values.items():
            url_path = url_path.replace("{" + name + "}", str(value))
        url = self.base_url + url_path

        # Upgrade to POST when substitutions are supplied; otherwise an
        # explicit body (spec-declared POST operations) is sent as-is.
        method = op.method
        if substitutions:
            method = "POST"
            body = _substitution_body(substitutions)
        elif body is not None and method not in ("POST", "PUT"):
            raise VoLCAError(
                f"operation {operation_id!r} is {method} and takes no JSON body"
            )

        # Send.
        if method == "GET":
            r = self._session.get(url, params=query_values)
        elif method == "POST":
            r = self._session.post(url, params=query_values, json=body or {})
        elif method == "DELETE":
            r = self._session.delete(url, params=query_values)
        elif method == "PUT":
            r = self._session.put(url, params=query_values, json=body or {})
        else:
            raise VoLCAError(f"Unsupported HTTP method {method!r} for {operation_id!r}")

        return self._json(r)

    # -- Response parsing --

    @staticmethod
    def _json(r: requests.Response):
        """Parse JSON response, raising a clear error on failure."""
        try:
            r.raise_for_status()
        except requests.HTTPError:
            body = r.text[:200]
            raise VoLCAError(
                f"{r.status_code} {r.reason} for {r.request.method} {r.url}: {body}",
                status_code=r.status_code,
                body=r.text,
            ) from None
        if not r.content:
            raise VoLCAError(
                f"Empty response for {r.request.method} {r.url} (status {r.status_code})",
                status_code=r.status_code,
                body="",
            )
        try:
            return r.json()
        except requests.exceptions.JSONDecodeError:
            hint = ""
            if "<!DOCTYPE" in r.text[:50] or "<html" in r.text[:50]:
                if r.history:
                    hint = (
                        f" (redirected from {r.history[0].url}; "
                        "auth headers are dropped on redirect, try using https://)"
                    )
                else:
                    hint = " (got HTML: is the URL correct?)"
            raise VoLCAError(
                f"Non-JSON response for {r.request.method} {r.url} "
                f"(status {r.status_code}){hint}",
                status_code=r.status_code,
                body=r.text,
            ) from None

    # -- Session state --

    def use(self, db_name: str) -> "Client":
        """Return a new client targeting a different database.

        Shares the underlying HTTP session, dispatch table, and any other
        Client-level state with the original; only ``db`` is overridden.
        New fields added to :meth:`Client.__init__` propagate automatically
        (no manual mirror to keep in sync).
        """
        c = object.__new__(Client)
        c.__dict__ = self.__dict__.copy()
        c.db = db_name
        return c

    def get_version(self) -> ServerVersion:
        """Return server build metadata: version, git hash/tag, build target.

        Uses a direct HTTP call: ``/api/v1/version`` has no operationId
        since it predates the Resources ADT.
        """
        return ServerVersion.from_json(
            self._json(self._session.get(f"{self.base_url}/api/v1/version"))
        )

    # ------------------------------------------------------------------
    # Typed wrappers
    # ------------------------------------------------------------------
    #
    # Each wrapper is a 2–3 line thunk around self._call. The dispatcher
    # handles URL assembly and query encoding. Wrappers exist only to
    # provide:
    #
    #   - Python-idiomatic kwargs (snake_case) with type hints
    #   - Dataclass return types
    #   - IDE autocomplete for the common operations
    #
    # Operations without a hand-written wrapper are still reachable via
    # ``client.call(operation_id, **kw)``, see the ``call`` method below.

    def call(self, operation_id: str, **kwargs: Any) -> Any:
        """Escape hatch: call any OpenAPI operation by operationId.

        Returns the raw JSON (no dataclass wrapping). Use this for
        operations that don't have an ergonomic wrapper yet, or for new
        endpoints added after the installed pyvolca was released.
        """
        return self._call(operation_id, **kwargs)

    # -- Database management --

    def list_databases(self) -> list[DatabaseInfo]:
        """List every database declared in the engine config.

        The typed entries carry ``depends_on``, so callers can derive
        cross-DB dependency sets from declared topology rather than
        hardcoding allowlists.
        """
        raw = self._call("list_databases")["databases"]
        return [DatabaseInfo.from_json(d) for d in raw]

    def load_database(self, db_name: str) -> dict:
        """Load a database into memory so it answers queries.

        Declared dependencies are loaded first; has no effect if the
        database is already loaded.
        """
        return self._call("load_database", db_name=db_name)

    def unload_database(self, db_name: str) -> dict:
        """Unload a database from memory to free RAM. The disk copy is kept.

        Refused if another loaded database still depends on it.
        """
        return self._call("unload_database", db_name=db_name)

    def derive_database(
        self,
        new_name: str,
        allocation: str,
        db_name: str | None = None,
    ) -> dict:
        """Read a database's sources again under another allocation key.

        Divides every multi-output block by a physical property of the
        products instead of by the shares the source declares: ``"wet mass"``
        weighs each product as it is, ``"dry mass"`` weighs its dry matter,
        ``"declared"`` keeps the source's own shares. The source is untouched
        and both stay loadable side by side, so two allocations of one study
        can be compared.

        This is a full load, not a copy: seconds to minutes on a large
        database. Refused when the key divides no block of the source, or when
        it is the key that source already reads under: either would leave that
        source under a second name.
        """
        self._require_wire(20, "derive_database", engine_hint="0.12.1")
        return self._call(
            "derive_database",
            db_name=self._db(db_name),
            new_name=new_name,
            allocation=allocation,
        )

    # -- Database write operations --
    #
    # These mutate a loaded database (copy / delete / relink / export /
    # dependency wiring). The engine does NOT register them as Resources, so
    # they carry no operationId and are unreachable through the OpenAPI
    # dispatcher. Each method therefore builds its URL directly, exactly like
    # load_database / unload_database above.
    #
    # The engine reports failures in-band as ``{"success": false, ...}`` (HTTP
    # 200) for some of these handlers, so _require_success surfaces those as
    # VoLCAError rather than letting a failed call look like a success.

    def _db(self, db_name: str | None) -> str:
        """Resolve the target database, falling back to ``self.db``.

        Raises VoLCAError when neither an explicit ``db_name`` nor a
        client-level default is available, and never silently targets ``""``.
        """
        name = db_name or self.db
        if not name:
            raise VoLCAError(
                "No database specified and Client(db=...) is empty. "
                "Pass db_name= or construct the client with db=...."
            )
        return name

    @staticmethod
    def _require_success(payload: dict, action: str) -> dict:
        """Raise VoLCAError if the engine reported an in-band failure.

        Handlers that return ``{"success": false, "message": ...}`` with HTTP
        200 would otherwise look like a success. Surface the engine's own
        message instead of silently returning the failure envelope.
        """
        if payload.get("success") is False:
            raise VoLCAError(
                f"{action} failed: {payload.get('message', 'no message')}"
            )
        return payload

    def _upload(
        self,
        url_path: str,
        source: str | Path | bytes,
        name: str,
        description: str | None,
        action: str,
    ) -> dict:
        """POST an octet-stream upload; metadata travels in query params.

        ``source`` is a filesystem path (streamed from disk by ``requests``,
        never fully read into memory) or raw ``bytes``. Shared by every
        upload endpoint (databases, method collections, and each reference
        data family), since they all take the same query-param + streamed
        body shape.

        The engine reports rejections in-band (HTTP 200 with
        ``success=false``: missing name, plan cap reached, file too large,
        extraction failure), so failures surface through _require_success
        rather than an HTTP error.
        """
        params: dict = {"name": name}
        if description:
            params["description"] = description
        headers = {"Content-Type": "application/octet-stream"}
        url = f"{self.base_url}{url_path}"
        if isinstance(source, bytes):
            payload = self._json(
                self._session.post(url, params=params, data=source, headers=headers)
            )
        else:
            with open(source, "rb") as fh:
                payload = self._json(
                    self._session.post(url, params=params, data=fh, headers=headers)
                )
        return self._require_success(payload, action)

    def copy_database(self, new_name: str, db_name: str | None = None) -> dict:
        """Copy a loaded database in memory under a new name.

        ``new_name`` is a path segment; the source defaults to ``self.db``.
        Returns the engine's ``ActivateResponse`` dict
        (``{"success", "message", "database"?}``). Raises VoLCAError if the
        engine reports ``success=false``.
        """
        src = self._db(db_name)
        payload = self._json(
            self._session.post(f"{self.base_url}/api/v1/db/{src}/copy/{new_name}")
        )
        return self._require_success(payload, "copy_database")

    def delete_activities(
        self,
        *,
        name: str = "",
        location: str = "",
        product: str = "",
        classifications: list[dict | tuple] | None = None,
        exact: bool = False,
        keep: list[str] | None = None,
        extra: list[str] | None = None,
        ids: list[str] | None = None,
        db_name: str | None = None,
    ) -> dict:
        """Delete activities selected by filter, or exactly the ``ids`` list.

        Builds a ``DeleteSelectionRequest``: the filter fields select the whole
        matching set, ``keep`` spares matched process ids, and ``extra`` adds
        ones the filter missed. ``classifications`` is a list of
        ``{"system", "value", "exact"}`` dicts or ``(system, value, exact)``
        tuples.

        ``ids`` names the selection verbatim instead of filtering; the filter
        arguments (and ``exact``) must then stay unset: the two modes are
        exclusive, mirroring the engine. Needs an engine speaking wire
        revision 3 (>= v0.9.3): an older one would silently drop the unknown
        ``ids`` key and read the request as an empty filter ("everything"),
        so pyvolca refuses to send it rather than let the engine guess.

        Returns the ``DeleteSelectionResponse`` dict
        (``{"success", "message", "deleted"}``); raises VoLCAError on
        ``success=false``.
        """
        if ids is not None and (name or location or product or classifications or exact):
            raise VoLCAError(
                "delete_activities: ids cannot be combined with the filter "
                "arguments (name/location/product/classifications/exact)"
            )
        # Omit blank filters entirely: sending "name":"" makes the engine treat
        # an empty string as a real (unsatisfiable) name filter, so a
        # product-only delete would match nothing and still report success.
        body: dict = {
            "classifications": [
                _coerce_class_filter(c) for c in (classifications or [])
            ],
            "exact": exact,
            "keep": keep or [],
            "extra": extra or [],
        }
        for key, value in (("name", name), ("location", location), ("product", product)):
            if value:
                body[key] = value
        if ids is not None:
            self._require_wire(3, "delete_activities(ids=...)", engine_hint="0.9.3")
            body["ids"] = list(ids)
        target = self._db(db_name)
        payload = self._json(
            self._session.post(
                f"{self.base_url}/api/v1/db/{target}/delete", json=body
            )
        )
        return self._require_success(payload, "delete_activities")

    def create_activities(
        self,
        activities: "list[ActivityInput] | ActivityInput",
        db_name: str | None = None,
    ) -> dict:
        """Write new activities into a database that can hold them.

        Each activity's ``process_id`` is minted by the engine from its name,
        its location, and its product name and unit (you do not choose it),
        and comes back in ``written``. Writing the same activity twice is therefore
        a conflict, not a second row; use :meth:`replace_activity` to correct
        one that is already there.

        Only a database of your own accepts writes: one you uploaded, or a
        copy. A database the engine reads from its configuration is background
        data the whole installation shares, and is refused.

        A batch is judged as a whole. If anything is wrong the engine reports
        every complaint at once and writes nothing, so a ten-line inventory is
        fixed in one round trip.

        Returns ``{"written": [process_id], "transient": bool, "warnings": [...]}``.
        ``transient`` is true when the edit lives in memory only; ``warnings``
        carries what the engine wants you to know but would not refuse over
        (a brand-new biosphere flow no method characterizes yet, for one).

        Needs an engine speaking wire revision 5 (the routes do not exist
        before it, and an absent route is a 404 that reads exactly like a
        misspelled database name).
        """
        batch = [activities] if isinstance(activities, ActivityInput) else list(activities)
        self._require_wire(5, "create_activities", engine_hint="0.9.5")
        target = self._db(db_name)
        return self._json(
            self._session.post(
                f"{self.base_url}/api/v1/db/{target}/activities",
                json={"activities": [a.to_wire() for a in batch]},
            )
        )

    def replace_activity(
        self,
        process_id: str,
        activity: "ActivityInput",
        db_name: str | None = None,
    ) -> dict:
        """Rewrite one activity the database already holds, keeping its identity.

        ``process_id`` must be the identity ``activity`` mints to; that is,
        the name, the location, and the product name and unit must be the ones
        the row already has, spelling and case included. Change any of those and you are describing a different
        activity, which the engine refuses rather than writing to a second row;
        create that one and delete the old one instead.

        Returns the same shape as :meth:`create_activities`.
        """
        self._require_wire(5, "replace_activity", engine_hint="0.9.5")
        target = self._db(db_name)
        return self._json(
            self._session.put(
                f"{self.base_url}/api/v1/db/{target}/activity/{process_id}",
                json=activity.to_wire(),
            )
        )

    def edit_exchanges(
        self,
        process_id: str,
        *,
        remove: Sequence[ExchangeSelector] = (),
        set_amounts: Sequence[SetAmount] = (),
        add_inputs: Sequence[TechInput] = (),
        add_biosphere: Sequence[BioExchange] = (),
        add_waste_outputs: Sequence[WasteOutput] = (),
        db_name: str | None = None,
    ) -> dict:
        """Change what one activity consumes and emits, keeping the activity.

        This reaches what :meth:`replace_activity` cannot: an activity that came
        in from a database file. Its identity was minted by whichever parser
        read it, so no description addresses it, and a description could not
        carry back its classification, synonyms, parameters, pedigree or
        coproducts anyway. Here you name only the lines that change, and
        everything else stays as it was.

        Only the inventory side is addressable. The reference product and any
        coproduct carry the activity's identity and its allocation, so no
        selector reaches them.

        A selector that names nothing is refused rather than treated as done.
        One that names several lines applies to all of them, and the counts come
        back per selector, in the order you stated them::

            {"removed": [2], "amountsSet": [], "added": 1,
             "transient": False, "warnings": [...]}

        Only a database of your own accepts edits: copy a configured one first.

        Needs an engine speaking wire revision 7.
        """
        self._require_wire(7, "edit_exchanges", engine_hint="0.9.5")
        target = self._db(db_name)
        return self._json(
            self._session.post(
                f"{self.base_url}/api/v1/db/{target}/activity/{process_id}/exchanges",
                json={
                    "remove": [s.to_wire() for s in remove],
                    "setAmounts": [s.to_wire() for s in set_amounts],
                    "addInputs": [i.to_wire() for i in add_inputs],
                    "addBiosphere": [b.to_wire() for b in add_biosphere],
                    "addWasteOutputs": [w.to_wire() for w in add_waste_outputs],
                },
            )
        )

    def relink(
        self, dep_db: str, mapping_csv: str, db_name: str | None = None
    ) -> dict:
        """Re-link a database against a dependency using a name→name alias CSV.

        ``mapping_csv`` is the CSV *text* (header row + source/target columns),
        sent inline so the engine needs no filesystem access. Returns the
        ``RelinkResponse`` dict (``{"dbName", "unresolvedBefore",
        "unresolvedAfter", "crossDBLinks", "dependsOn"}``).
        """
        target = self._db(db_name)
        body = {"depDb": dep_db, "mappingCsv": mapping_csv}
        return self._json(
            self._session.post(
                f"{self.base_url}/api/v1/db/{target}/relink", json=body
            )
        )

    def relink_from_file(
        self, dep_db: str, mapping_path: str, db_name: str | None = None
    ) -> dict:
        """Read a mapping CSV file and call :meth:`relink` with its text."""
        csv_text = Path(mapping_path).read_text(encoding="utf-8")
        return self.relink(dep_db, csv_text, db_name=db_name)

    def export_database(self, fmt: str, db_name: str | None = None) -> bytes:
        """Export a loaded database, returning the serialized bytes.

        ``fmt`` is one of ``simapro|ecospold1|ecospold2|ilcd|brightway``,
        validated client-side; an unknown value raises VoLCAError before any
        request. Single-file formats carry their bytes directly; EcoSpold 2 /
        ILCD multi-file trees come back zipped.

        The engine streams the payload as raw bytes. Best-effort approximation
        warnings arrive in the ``X-Volca-Export-Warnings`` response header
        (percent-encoded, newline-joined) and are surfaced through
        :mod:`warnings`. Raises VoLCAError on an HTTP error.
        """
        fmt_norm = fmt.strip().lower()
        if fmt_norm not in _EXPORT_FORMATS:
            raise VoLCAError(
                f"unknown export format: {fmt!r} "
                f"(expected {'|'.join(sorted(_EXPORT_FORMATS))})"
            )
        target = self._db(db_name)
        resp = self._session.post(
            f"{self.base_url}/api/v1/db/{target}/export",
            json={"format": fmt_norm},
            headers={"Accept": "application/octet-stream"},
        )
        if resp.status_code >= 400:
            raise VoLCAError(
                f"export_database failed (HTTP {resp.status_code}): "
                f"{resp.text[:500]}"
            )
        header = resp.headers.get("X-Volca-Export-Warnings", "")
        for line in urllib.parse.unquote(header).split("\n"):
            if line:
                warnings.warn(line, stacklevel=2)
        return resp.content

    def export_to_file(
        self, fmt: str, out_path: str, db_name: str | None = None
    ) -> None:
        """Export a database (see :meth:`export_database`) and write it to a file."""
        Path(out_path).write_bytes(self.export_database(fmt, db_name=db_name))

    def add_dependency(self, dep_name: str, db_name: str | None = None) -> dict:
        """Declare ``dep_name`` as a dependency of the target database.

        Returns the engine's ``DatabaseSetupInfo`` dict describing the updated
        dependency topology.
        """
        target = self._db(db_name)
        return self._json(
            self._session.post(
                f"{self.base_url}/api/v1/db/{target}/add-dependency/{dep_name}"
            )
        )

    def remove_dependency(self, dep_name: str, db_name: str | None = None) -> dict:
        """Remove ``dep_name`` from the target database's dependencies.

        Returns the updated ``DatabaseSetupInfo`` dict.
        """
        target = self._db(db_name)
        return self._json(
            self._session.post(
                f"{self.base_url}/api/v1/db/{target}/remove-dependency/{dep_name}"
            )
        )

    # -- Upload & staged-database lifecycle --
    #
    # An uploaded database lands *staged*: extracted and format-detected, but
    # not loaded. The path from archive to a usable database is
    #   upload_database → get_setup (see what deps are missing) →
    #   add_dependency (wire each) → finalize_database (build matrices, load).
    # set_data_path picks the data file when the archive holds several.

    def upload_database(
        self,
        source: str | Path | bytes,
        name: str,
        *,
        description: str | None = None,
    ) -> dict:
        """Upload a database archive; stage it under a generated slug.

        ``source`` is a path to a ZIP / CSV / XLSX archive (or its raw
        ``bytes``); ``name`` is the display name. The engine auto-detects the
        format (EcoSpold 1/2, SimaPro CSV, ILCD, OpenLCA JSON-LD, Brightway
        Excel) and stages the database without loading it.

        Returns the ``UploadResponse`` dict
        (``{"success", "message", "slug", "format"}``); ``slug`` is the name
        every later call targets. Then inspect :meth:`get_setup`, wire missing
        dependencies with :meth:`add_dependency`, and call
        :meth:`finalize_database` to build matrices and load it.

        Raises VoLCAError on any rejection (uploads disabled on the plan, size
        cap exceeded, unreadable archive); the engine reports these in-band
        with HTTP 200 and ``success=false``.
        """
        return self._upload(
            "/api/v1/db/upload", source, name, description, "upload_database"
        )

    def ensure_database(self, source: str | Path | bytes, name: str | None = None) -> str:
        """Idempotently make the archive at ``source`` a loaded database.

        The one-call form of the upload lifecycle: match by display name
        (default: the file's stem), upload only when absent, finalize the
        staged copy, load if unloaded. Returns the slug every later call
        targets: run it at the top of a script and it converges on the same
        loaded database every time instead of re-uploading. A match that is
        already loaded, even partially linked, is left untouched.

        A staged copy that is not ready to finalize raises VoLCAError naming
        the blocker (missing suppliers, no activities parsed); fix it with
        :meth:`add_dependency` or :meth:`set_data_path`, then
        :meth:`finalize_database`. The gate also holds on re-runs: an upload
        left staged by an earlier failed run goes through the same readiness
        check instead of being loaded half-linked.
        """
        if name is None:
            if isinstance(source, bytes):
                raise VoLCAError(
                    "ensure_database: name= is required when source is bytes"
                )
            name = Path(source).stem
        for db in self.list_databases():
            if name in (db.display_name, db.name):
                if db.status == DatabaseStatus.UNLOADED:
                    if db.is_uploaded:
                        self._finalize_when_ready(db.name, name)
                    else:
                        self.load_database(db.name)
                return db.name
        slug = self.upload_database(source, name=name)["slug"]
        self._finalize_when_ready(slug, name)
        return slug

    def _finalize_when_ready(self, slug: str, name: str) -> None:
        """Readiness gate of :meth:`ensure_database`: finalize or refuse.

        Finalizing builds the matrices and loads the database, so a staged
        copy that ``get_setup`` reports not ready raises with the concrete
        blocker instead: a half-linked database silently undercounts and
        the consumer can't tell.
        """
        setup = self.get_setup(slug)
        if setup.get("isReady", False):
            self.finalize_database(slug)
            return
        missing = setup.get("missingSuppliers") or []
        if missing:
            blocker = (
                f"missing suppliers {missing!r}; wire them with "
                "add_dependency, then finalize_database"
            )
        elif not setup.get("activityCount"):
            blocker = (
                "no activities parsed; pick the data file with "
                "set_data_path (see availablePaths in get_setup), "
                "then finalize_database"
            )
        else:
            blocker = (
                f"{setup.get('unresolvedLinks')} unresolved links; "
                "inspect get_setup"
            )
        raise VoLCAError(
            f"ensure_database: {name!r} (slug {slug!r}) is not ready to "
            f"finalize: {blocker}."
        )

    def get_setup(self, db_name: str | None = None) -> dict:
        """Setup status of a staged or loaded database (``DatabaseSetupInfo``).

        Key fields: ``isReady`` (can it be finalized/loaded), ``missingSuppliers``
        and ``unresolvedLinks`` (unmet cross-database links), ``dependencies``
        (declared deps), ``dataPath`` / ``availablePaths`` (the selected data
        file and the alternatives, see :meth:`set_data_path`), ``completeness``,
        and ``supplierAmbiguities``: the inputs several activities of one
        dependency answered equally well, each naming the product asked for,
        the activity linked to and how many tied, so the supplier meant can be
        named rather than guessed (wire revision 23).
        """
        target = self._db(db_name)
        return self._json(self._session.get(f"{self.base_url}/api/v1/db/{target}/setup"))

    def set_data_path(self, path: str, db_name: str | None = None) -> dict:
        """Choose which data file a staged multi-file archive should use.

        ``path`` must be one of the ``availablePaths`` reported by
        :meth:`get_setup`, relative to the upload directory. Returns the
        updated ``DatabaseSetupInfo`` dict.
        """
        target = self._db(db_name)
        return self._json(
            self._session.post(
                f"{self.base_url}/api/v1/db/{target}/set-data-path", json={"path": path}
            )
        )

    def finalize_database(self, db_name: str | None = None) -> dict:
        """Build matrices for a staged database and load it (``ActivateResponse``).

        Call after dependencies resolve (:meth:`get_setup` reports
        ``isReady``). Raises VoLCAError if the engine reports ``success=false``
        (e.g. unresolved suppliers).
        """
        target = self._db(db_name)
        payload = self._json(
            self._session.post(f"{self.base_url}/api/v1/db/{target}/finalize")
        )
        return self._require_success(payload, "finalize_database")

    def delete_database(self, db_name: str | None = None) -> dict:
        """Delete a database entirely: unload it and remove its uploaded files.

        Returns the ``ActivateResponse`` dict; raises VoLCAError on
        ``success=false``.
        """
        target = self._db(db_name)
        payload = self._json(self._session.delete(f"{self.base_url}/api/v1/db/{target}"))
        return self._require_success(payload, "delete_database")

    def list_presets(self) -> list[Preset]:
        """List classification presets configured in this instance.

        Each :class:`Preset` carries its ``filters`` (list of
        :class:`PresetFilter` triples). Apply by passing ``preset=p.name``
        to filtering endpoints.
        """
        return [Preset.from_json(p) for p in self._call("list_presets")]

    # -- Search --

    def search_activities(
        self,
        name: str | None = None,
        *,
        geo: str | None = None,
        product: str | None = None,
        preset: str | None = None,
        classification: str | None = None,
        classification_value: str | None = None,
        classification_match: MatchModeLike | None = None,
        page: int | None = None,
        page_size: int | None = None,
        limit: int | None = None,
        offset: int | None = None,
        sort: str | None = None,
        order: str | None = None,
        exact: bool = False,
    ) -> SearchResults[Activity]:
        """Search activities in the current database.

        All filters are AND-combined and case-insensitive. ``name`` and
        ``product`` match by substring unless ``exact=True``.

        Returns a paginated :class:`SearchResults`: iterate it to walk
        every match across all pages (subsequent pages fetched on demand),
        or use ``.page(n)`` for explicit page access. ``len(results)`` is
        the server-reported total across all pages.

        Args:
            name: Substring (or exact match) on activity name.
            geo: Geography code (``"FR"``, ``"GLO"``, ``"RoW"``…).
            product: Substring on the reference product name.
            preset: Apply a named classification preset configured in the engine.
            classification: System name (``"ISIC rev.4 ecoinvent"``).
            classification_value: Substring within that system's value.
            classification_match: How ``classification_value`` is compared:
                :class:`MatchMode.CONTAINS` (default, substring) or
                :class:`MatchMode.EXACT` (case-insensitive equality). Ignored
                when ``classification`` is unset.
            page: 1-based page number. Must be paired with ``page_size``:
                offset cannot be derived from page alone.
            page_size: Items per page (becomes the wire-level ``limit``).
                Alone (no ``page``) means "page 1 with this size".
            limit: Wire-level cap on returned items. Prefer ``page_size``.
            offset: Wire-level starting index. Prefer ``page`` + ``page_size``.
            sort: Sort key: ``"name"`` or ``"location"``. When set, results
                are ordered lexicographically instead of by relevance.
            order: ``"desc"`` to reverse; ascending otherwise.
            exact: When True, ``name`` and ``product`` are matched exactly.

        Returns:
            :class:`SearchResults[Activity]`, iterable across all pages.
        """
        wire_limit, wire_offset = _resolve_page_args(page, page_size, limit, offset)
        common: dict[str, Any] = dict(
            name=name,
            geo=geo,
            product=product,
            preset=preset,
            classification=classification,
            classification_value=classification_value,
            classification_mode=(
                MatchMode(classification_match).value
                if classification_match is not None
                else None
            ),
            sort=sort,
            order=order,
            exact=exact,
        )

        def fetch(o: int, l: int | None) -> dict:
            return self._call("search_activities", **common, limit=l, offset=o)

        raw = fetch(wire_offset, wire_limit)
        return SearchResults.from_raw(raw, parse=Activity.from_json, fetch=fetch)

    def resolve_activities(
        self,
        names: Iterable[str],
        *,
        by: Literal["name", "product"] = "name",
        geo: str | None = None,
        exact: bool = True,
        limit: int = 5,
        workers: int = 8,
    ) -> dict[str, list[Activity]]:
        """Resolve a batch of names to their matching activities, concurrently.

        One :meth:`search_activities` call per unique name, fanned out over
        ``workers`` threads on the client's HTTP session. This replaces the
        two patterns scripts keep hand-rolling: downloading the whole
        database to build a name→process_id dict, and per-name thread pools.

        The result maps every input name to its matches: the mapping is
        total, so misses are visible, never silently dropped:

        * ``[]``: no match; the name does not resolve.
        * one :class:`Activity`: unambiguous; ``matches[0].process_id``.
        * several: ambiguous (same name across geographies or products);
          disambiguate with ``geo=`` or inspect the candidates.

        With ``exact=False`` matches are relevance-ranked (best first), so
        ``matches[0]`` is the engine's best fuzzy guess.

        Args:
            names: Names to resolve. Duplicates are searched once.
            by: Match against activity ``"name"`` or reference ``"product"``.
            geo: Restrict every search to one geography code.
            exact: Exact (default) or substring/ranked matching.
            limit: Maximum candidates returned per name.
            workers: Concurrent searches.

        Returns:
            ``{name: matches}`` for every input name, in input order.
        """
        if by not in ("name", "product"):
            raise VoLCAError(f"by= must be 'name' or 'product', got {by!r}")
        unique = list(dict.fromkeys(names))
        if not unique:
            return {}

        def search(n: str) -> list[Activity]:
            kwargs: dict[str, Any] = {by: n}
            matches = self.search_activities(
                **kwargs, geo=geo, exact=exact, limit=limit
            ).results
            if exact:
                # Engines released before the product filter honored exact=
                # return substring matches; re-check equality (same casefold
                # rule as the engine) so a near-miss never resolves silently.
                want = n.casefold()
                field = "activity_name" if by == "name" else "product_name"
                matches = [a for a in matches if getattr(a, field).casefold() == want]
            return matches

        with ThreadPoolExecutor(max_workers=min(workers, len(unique))) as pool:
            return dict(zip(unique, pool.map(search, unique)))

    def search_flows(
        self,
        query: str | None = None,
        *,
        kind: str | None = None,
        page: int | None = None,
        page_size: int | None = None,
        limit: int | None = None,
        offset: int | None = None,
        sort: str | None = None,
        order: str | None = None,
    ) -> SearchResults[Flow]:
        """Search flows in the current database.

        Three kinds of flow answer, and :attr:`Flow.kind` says which each one
        is: a technosphere product one activity makes and another consumes, a
        biosphere substance exchanged with nature, or a waste.

        Returns a paginated :class:`SearchResults[Flow]`: iterate to walk
        every match across all pages, or use ``.page(n)`` for explicit
        access. See :meth:`search_activities` for the pagination contract.

        Args:
            query: Words matched case-insensitively against flow names and
                synonyms. Every word must appear, in any order, and a word
                matches inside a longer one (``chlor`` finds
                ``Trichloroethane``). Punctuation separates words, so
                ``water fossil`` and ``water, fossil`` search alike. With no
                ``sort`` asked for, names carrying the query as typed come
                first. An empty query returns nothing.
            kind: Keep only these kinds: ``"technosphere"``, ``"biosphere"``
                or ``"waste"``. Omit for all three. Name several separated by
                commas, as in ``"biosphere,waste"``, which is what is
                exchanged with nature or discarded and the bucket
                :meth:`count_search_matches` reports as ``flows``. One kind
                needs engine wire revision 9: an older engine would drop the
                filter and answer with every kind. Several need revision 19,
                where an older engine reads the whole value as one name and
                refuses it. Either way the request is refused here first, with
                a message naming the revision, rather than sent.
            page / page_size: Web-style pagination; convert to wire-level
                ``offset`` / ``limit``.
            limit / offset: Wire-level escape hatch.
            sort: Sort key: ``"name"`` (default), ``"category"``, or ``"unit"``.
            order: ``"desc"`` to reverse; ascending otherwise.
        """
        if kind is not None:
            self._require_wire(9, "search_flows(kind=...)", engine_hint="0.9.6")
            if "," in kind:
                self._require_wire(
                    19, "search_flows(kind=...) naming several kinds", engine_hint="0.12.1"
                )
        wire_limit, wire_offset = _resolve_page_args(page, page_size, limit, offset)

        def fetch(o: int, l: int | None) -> dict:
            return self._call(
                "search_flows", q=query, kind=kind, sort=sort, order=order, limit=l, offset=o
            )

        raw = fetch(wire_offset, wire_limit)
        return SearchResults.from_raw(raw, parse=Flow.from_json, fetch=fetch)

    def count_search_matches(self, query: str) -> SearchCounts:
        """How many processes, products and flows a query matches.

        One call rather than three searches, for a search box that labels its
        tabs with counts. The three are disjoint and together cover the
        database.

        Args:
            query: The search term. Required: an empty box has nothing to
                count, and the engine refuses a blank one rather than
                answering three zeros, which would read as "this database has
                nothing".
        """
        self._require_wire(17, "count_search_matches", engine_hint="0.12.1")
        return SearchCounts.from_json(self._call("count_search_matches", q=query))

    def list_classifications(self) -> list[ClassificationSystem]:
        """List classification systems and their values for the current database.

        ``ClassificationSystem.activity_count`` tells how widely each system
        is populated, useful for picking a filter dimension with enough
        signal.
        """
        return [ClassificationSystem.from_json(c) for c in self._call("list_classifications")]

    # -- Activity details --

    def get_activity(self, process_id: str) -> ActivityDetail:
        """Fetch an activity's full detail.

        Returns a typed ActivityDetail. Use ``act.inputs`` / ``act.outputs`` /
        ``act.technosphere_inputs`` to filter exchanges instead of walking
        ``act.exchanges`` directly.
        """
        return ActivityDetail.from_json(self._call("get_activity", process_id=process_id))

    def get_inputs(self, process_id: str) -> list[Exchange]:
        """Return the input exchanges of an activity (richer metadata than ``get_activity``).

        Uses a direct HTTP call because ``/inputs`` has no operationId
        (it's a non-Resources auxiliary endpoint).
        """
        raw = self._json(
            self._session.get(
                f"{self.base_url}/api/v1/db/{self.db}/activity/{process_id}/inputs"
            )
        )
        return [parse_exchange_detail(e) for e in raw]

    def get_outputs(self, process_id: str) -> list[Exchange]:
        """Return the output exchanges of an activity. See :meth:`get_inputs` for notes."""
        raw = self._json(
            self._session.get(
                f"{self.base_url}/api/v1/db/{self.db}/activity/{process_id}/outputs"
            )
        )
        return [parse_exchange_detail(e) for e in raw]

    # -- Supply chain --

    def get_supply_chain(
        self,
        process_id: str,
        *,
        name: str | None = None,
        location: str | None = None,
        limit: int | None = None,
        min_quantity: float | None = None,
        max_depth: int | None = None,
        preset: str | None = None,
        classification_filters: list[ClassificationFilter] | None = None,
        sort: str | None = None,
        order: str | None = None,
        substitutions: list[SubstitutionLike] | None = None,
        include_edges: bool | None = None,
    ) -> SupplyChain:
        """Get the flat supply chain of an activity.

        Returns a :class:`SupplyChain`. Check ``result.has_more`` to detect
        when ``limit`` truncated ``entries`` below ``filtered_activities``:
        further downstream analysis on a truncated chain would be wrong
        without flagging the gap.

        Each entry carries its own ``unit``, and entries do not share one: it
        is the producing activity's reference-product unit, which is not
        always the unit written on the exchange that consumes it. Read it off
        the entry rather than off the activity you started from.

        Args:
            max_depth: Max hops from root. 1 = direct inputs only.
            classification_filters: Restrict entries to those matching any
                of the given ClassificationFilter triples. Multiple filters
                are AND-combined by the server.
            sort: Sort key: ``"name"``, ``"location"``, ``"unit"``,
                ``"depth"``, ``"consumers"``, or ``"amount"``. Default
                orders by descending absolute quantity.
            order: ``"desc"`` to reverse; ascending otherwise.
            substitutions: When provided, the call is upgraded to POST and
                the scaling vector is recomputed with the substituted
                suppliers. Accepts :class:`Substitution` (preferred) or the
                legacy ``{"from", "to", "consumer"}`` dict form; ``consumer``
                is optional: omit it for a global swap.
        """
        classifications = [f.system for f in classification_filters or []]
        classification_values = [f.value for f in classification_filters or []]
        classification_modes = [f.mode.value for f in classification_filters or []]
        raw = self._call(
            "get_supply_chain",
            process_id=process_id,
            name=name,
            location=location,
            limit=limit,
            min_quantity=min_quantity,
            max_depth=max_depth,
            preset=preset,
            classification=classifications or None,
            classification_value=classification_values or None,
            classification_mode=classification_modes or None,
            sort=sort,
            order=order,
            include_edges=include_edges,
            substitutions=substitutions,
        )
        return SupplyChain.from_json(raw)

    # -- Aggregate primitive --

    def aggregate(
        self,
        process_id: str,
        scope: AggregateScope | str,
        *,
        is_input: bool | None = None,
        max_depth: int | None = None,
        filter_name: str | None = None,
        filter_name_not: list[str] | str | None = None,
        filter_unit: str | None = None,
        preset: str | None = None,
        filter_classification: list[ClassificationFilter] | None = None,
        filter_target_name: str | None = None,
        filter_consumer: str | None = None,
        filter_consumer_not: list[str] | str | None = None,
        filter_is_reference: bool | None = None,
        group_by: str | None = None,
        aggregate: AggregateOp | str | None = None,
    ) -> AggregateResult:
        """SQL-group-by aggregation over direct exchanges, supply chain, or biosphere flows.

        Args:
            scope: :class:`AggregateScope` member (``DIRECT`` / ``SUPPLY_CHAIN``
                / ``BIOSPHERE`` / ``CONSUMPTION``) or the equivalent wire
                string. Strings are accepted for one-liner ergonomics but
                bypass static checking. ``CONSUMPTION`` rows are scaled
                technosphere edges: use it for "total X consumed upstream"
                questions. Net electricity without grid double counting::

                    aggregate(pid, "consumption", filter_name="electricity",
                              filter_consumer_not=["electricity"])

                Grass eaten by cattle across the whole chain::

                    aggregate(pid, "consumption", filter_name="grass",
                              filter_consumer="cattle")
            filter_consumer: substring match on the consuming activity's name
                (``CONSUMPTION`` scope only).
            filter_consumer_not: exclude edges whose consumer name contains
                any of these substrings (list or comma-separated string).
                Items always split on commas on the wire, so a name that
                itself contains a comma ("electricity production, hard
                coal") becomes two independent substrings; use a
                comma-free fragment of the name instead.
            group_by: omit for a single-bucket result (just the totals).
                Supported keys: ``"name"``, ``"flow_id"``, ``"name_prefix"``,
                ``"unit"``, ``"location"``, ``"target_name"``,
                ``"consumer_name"`` (``CONSUMPTION`` scope),
                ``"classification.<system>"``.
            aggregate: :class:`AggregateOp` member or wire string
                (``"sum_quantity"`` by default, ``"count"``, or ``"share"``).
        """
        # filter_classification goes over the wire as "System=Value[:exact]" strings.
        if filter_classification:
            filter_strings = [
                f"{f.system}={f.value}" + (":exact" if f.mode is MatchMode.EXACT else "")
                for f in filter_classification
            ]
        else:
            filter_strings = None
        # filter_name_not: accept list or comma-string, send as comma-string.
        if isinstance(filter_name_not, list):
            filter_name_not_csv: str | None = ",".join(filter_name_not)
        else:
            filter_name_not_csv = filter_name_not
        if isinstance(filter_consumer_not, list):
            filter_consumer_not_csv: str | None = ",".join(filter_consumer_not)
        else:
            filter_consumer_not_csv = filter_consumer_not
        raw = self._call(
            "aggregate",
            process_id=process_id,
            scope=scope,
            is_input=is_input,
            max_depth=max_depth,
            filter_name=filter_name,
            filter_name_not=filter_name_not_csv,
            filter_unit=filter_unit,
            preset=preset,
            filter_classification=filter_strings,
            filter_target_name=filter_target_name,
            filter_consumer=filter_consumer,
            filter_consumer_not=filter_consumer_not_csv,
            filter_is_reference=filter_is_reference,
            group_by=group_by,
            aggregate=aggregate,
        )
        return AggregateResult.from_json(raw)

    # -- Consumers (reverse supply chain) --

    def get_consumers(
        self,
        process_id: str,
        *,
        name: str | None = None,
        location: str | None = None,
        product: str | None = None,
        preset: str | None = None,
        classification_filters: list[ClassificationFilter] | None = None,
        page: int | None = None,
        page_size: int | None = None,
        limit: int | None = None,
        offset: int | None = None,
        max_depth: int | None = None,
        sort: str | None = None,
        order: str | None = None,
        include_edges: bool = False,
    ) -> ConsumersResponse:
        """Find all activities that transitively consume this supplier.

        Args:
            max_depth: Max hops from supplier. 1 = direct consumers only.
            classification_filters: ClassificationFilter entries restricting
                the results. Multiple filters are AND-combined by the server.
                Mode is :class:`MatchMode.EXACT` or :class:`MatchMode.CONTAINS`.
            sort: Sort key: ``"name"``, ``"location"``, ``"product"``,
                ``"amount"``, or ``"unit"``. Default orders by depth.
            order: ``"desc"`` to reverse; ascending otherwise.
            include_edges: When True, the response carries every technosphere
                edge whose endpoints are both reachable from the supplier.
                Callers can walk these to reconstruct supplier→consumer paths
                without a second ``get_path_to`` round-trip.

        Returns a :class:`ConsumersResponse` whose ``consumers`` attribute is
        a :class:`SearchResults[ConsumerResult]` (iterate it to walk every
        consumer across all pages) and whose ``edges`` attribute carries
        the traversal subgraph (empty by default).
        """
        classifications = [f.system for f in classification_filters or []]
        classification_values = [f.value for f in classification_filters or []]
        classification_modes = [f.mode.value for f in classification_filters or []]
        wire_limit, wire_offset = _resolve_page_args(page, page_size, limit, offset)
        common: dict[str, Any] = dict(
            process_id=process_id,
            name=name,
            location=location,
            product=product,
            preset=preset,
            classification=classifications or None,
            classification_value=classification_values or None,
            classification_mode=classification_modes or None,
            max_depth=max_depth,
            sort=sort,
            order=order,
            include_edges=include_edges,
        )

        def fetch(o: int, l: int | None) -> dict:
            return self._call("get_consumers", **common, limit=l, offset=o)

        raw = fetch(wire_offset, wire_limit)
        return ConsumersResponse.from_json(raw, fetch=fetch)

    def get_path_to(self, process_id: str, target: str) -> PathResult:
        """Find the shortest upstream path from process to first activity whose name matches target.

        Returns a PathResult whose path is ordered root → target. Each step
        includes cumulative_quantity, scaling_factor, and (except the root)
        local_step_ratio.
        """
        return PathResult.from_json(
            self._call("get_path_to", process_id=process_id, target=target)
        )

    # -- Tree (SPA-only endpoint, no operationId, direct HTTP) --

    def get_tree(self, process_id: str) -> dict:
        """Fetch the recursive activity tree used by the analysis SPA.

        ``/tree`` has no operationId in the OpenAPI spec; it's kept for the
        SPA's lazy-expanding graph widget and intentionally not exposed as
        a Resource. Included here as a direct HTTP call for scripts that
        need the same shape.
        """
        return self._json(
            self._session.get(
                f"{self.base_url}/api/v1/db/{self.db}/activity/{process_id}/tree"
            )
        )

    # -- Inventory & impacts --

    def get_inventory(
        self,
        process_id: str,
        *,
        flow: str | None = None,
        limit: int | None = None,
        substitutions: list[SubstitutionLike] | None = None,
    ) -> InventoryResult:
        """Compute the life-cycle inventory (cumulative biosphere flows) for an activity.

        Returns an :class:`InventoryResult` with the per-elementary-flow
        totals scaled to one functional unit of the activity's reference
        product. Use :meth:`get_impacts` to apply a characterization method
        to the inventory; use :meth:`aggregate` with ``scope="biosphere"``
        for grouped views.

        Args:
            flow: Substring filter on flow name.
            limit: Cap on returned flow rows. (Server returns full inventory
                otherwise; the engine doesn't paginate this endpoint.)
            substitutions: Upstream supplier swaps; see :meth:`get_supply_chain`.
        """
        return InventoryResult.from_json(
            self._call(
                "get_inventory",
                process_id=process_id,
                flow=flow,
                limit=limit,
                substitutions=substitutions,
            )
        )

    def get_impacts(
        self,
        process_id: str,
        method_id: str,
        *,
        collection: str | None = None,
        top_flows: int | None = None,
        substitutions: list[SubstitutionLike] | None = None,
    ) -> LCIAResult:
        """Compute the LCIA score for a single impact category on an activity.

        Use :meth:`get_impacts_batch` to retrieve every category in a method
        collection at once (and any configured scoring sets).

        Args:
            method_id: A method UUID, or the method's name ("Water use");
                a name is resolved against the engine's loaded methods.
            collection: Method collection name. Left out, it is read off the
                resolved method, so the caller needs to know only the method.
            top_flows: Max top contributing flows to return (default 5).
        """
        collection, method_id = self._resolve_method(method_id, collection)
        return LCIAResult.from_json(
            self._call(
                "get_impacts",
                process_id=process_id,
                collection=collection,
                method_id=method_id,
                top_flows=top_flows,
                substitutions=substitutions,
            )
        )

    def get_impacts_batch(
        self,
        process_id: str,
        *,
        collection: str | None = None,
        substitutions: list[SubstitutionLike] | None = None,
        exclude_long_term: bool | None = None,
    ) -> LCIABatchResult:
        """Compute LCIA for every impact category in a collection, in one call.

        The response carries the per-method :class:`LCIAResult` list plus any
        formula-based scoring sets declared in the engine config (PEF, ECS…).
        ``scoring_indicators`` gives the per-variable breakdown of each
        scoring set, pre-multiplied by the set's ``displayMultiplier``.
        ``exclude_long_term`` drops long-term emissions before scoring, the
        same switch :meth:`score_activities` carries.

        Left without a ``collection``, the call runs against the only loaded
        one, and refuses when several are loaded rather than picking one.

        Uses a direct HTTP call: the batch endpoint has no operationId in the
        OpenAPI spec (the dispatcher primary is the single-method variant), so
        this wrapper bypasses ``_call`` and builds the URL itself.
        """
        if not self.db:
            raise VoLCAError(
                "get_impacts_batch requires a database; construct Client(db=...)."
            )
        collection = self._resolve_collection(collection)
        url = (
            f"{self.base_url}/api/v1/db/{self.db}/activity/{process_id}"
            f"/impacts/{collection}"
        )
        params = (
            {}
            if exclude_long_term is None
            else {"exclude-long-term": _format_query_value(exclude_long_term)}
        )
        if substitutions:
            r = self._session.post(
                url, json=_substitution_body(substitutions), params=params
            )
        else:
            r = self._session.get(url, params=params)
        return LCIABatchResult.from_json(self._json(r))

    def compute_sensitivity(
        self,
        process_id: str,
        method_id: str,
        perturbations: list[dict],
        *,
        collection: str | None = None,
    ) -> SensitivityResult:
        """How much one impact score moves when technosphere links are perturbed.

        Each perturbation is a dict
        ``{"consumer": pid, "supplier": pid, "delta": -0.05, "label"?: str}``:
        ``delta`` is *relative* (the coefficient becomes ``a * (1 + delta)``,
        so ``-1.0`` removes the link). Returns the ``baseline`` :class:`LCIAResult`
        plus one :class:`PerturbedResult` per perturbation, each carrying
        either the perturbed impact and its delta, or an ``error`` string when
        that perturbation could not be resolved. ``method_id`` takes a method
        name as well as a UUID, and ``collection`` is read off the resolved
        method unless you pin it.
        """
        collection, method_id = self._resolve_method(method_id, collection)
        return SensitivityResult.from_json(
            self._call(
                "compute_sensitivity",
                process_id=process_id,
                collection=collection,
                method_id=method_id,
                body={"perturbations": perturbations},
            )
        )

    def score_activities(
        self,
        process_ids: list[str],
        *,
        collection: str | None = None,
        top_flows: int | None = None,
        exclude_long_term: bool | None = None,
    ) -> BatchScores:
        """Score many processes in one call (every category of a collection each).

        Returns a :class:`BatchScores`: ``results`` holds one
        :class:`ScoredActivity` per process the engine could compute, while
        ``not_found`` / ``invalid`` list the ids it could not resolve; inspect
        them, a partial result is not an error. ``top_flows`` caps the top
        contributors per category; ``exclude_long_term`` drops long-term
        emissions from the totals. Left without a ``collection``, the call runs
        against the only loaded one, and refuses when several are loaded rather
        than picking one.
        """
        collection = self._resolve_collection(collection)
        return BatchScores.from_json(
            self._call(
                "score_activities",
                collection=collection,
                top_flows=top_flows,
                exclude_long_term=exclude_long_term,
                body={"processIds": process_ids},
            )
        )

    # -- Methods --

    def list_methods(self) -> list[Method]:
        """List every LCIA method available in the engine.

        Each :class:`Method` carries ``id``, ``name``, ``category``, ``unit``,
        ``factor_count``, and the parent ``collection``. Every ``method_id``
        argument takes either, so this list is for browsing, not for looking
        up an id before a call.
        """
        return [Method.from_json(m) for m in self._call("list_methods")]

    def get_flow_mapping(self, method_id: str) -> FlowMapping:
        """Get the characterization-factor-to-database-flow mapping coverage.

        :class:`FlowMapping.coverage_pct` summarises how many of the DB's
        biosphere flows the method has a CF for; ``flows`` is the per-flow
        breakdown including unmatched rows (``cf_value=None``).
        """
        return FlowMapping.from_json(
            self._call("get_flow_mapping", method_id=self._method_uuid(method_id))
        )

    def get_characterization(
        self,
        method_id: str,
        *,
        flow: str | None = None,
        limit: int | None = None,
    ) -> CharacterizationResult:
        """Look up characterization factors for a method matched to database flows.

        Returns a :class:`CharacterizationResult` carrying ``matches`` (total
        rows the filter selected) and ``shown`` (rows actually returned under
        ``limit``). Check ``result.has_more`` to detect truncation.
        """
        return CharacterizationResult.from_json(
            self._call(
                "get_characterization",
                method_id=self._method_uuid(method_id),
                flow=flow,
                limit=limit,
            )
        )

    def explain_cf(self, method_id: str, flow_id: str) -> ExplainCFResult:
        """Explain why one flow scores with the characterization factor it does.

        ``result.explanation`` is a list of sentences written by the engine:
        show them as they are. The structured fields say the same thing in a
        form you can compare or filter on, and ``result.steps_tried`` lists the
        rungs the cascade walked before the one that answered.
        """
        return ExplainCFResult.from_json(
            self._call(
                "explain_cf", method_id=self._method_uuid(method_id), flow_id=flow_id
            )
        )

    def get_contributing_flows(
        self,
        process_id: str,
        method_id: str,
        *,
        collection: str | None = None,
        limit: int | None = None,
    ) -> ContributingFlows:
        """Which elementary flows drive a given impact category.

        Returns a :class:`ContributingFlows`. Caveat: the engine does not
        report the total flow count, so pyvolca cannot derive ``has_more``
        from the response. Pass a generous ``limit`` if you need exhaustive
        coverage and inspect ``share_pct`` totals. ``method_id`` takes a method
        name as well as a UUID, and ``collection`` is read off the resolved
        method unless you pin it.
        """
        collection, method_id = self._resolve_method(method_id, collection)
        return ContributingFlows.from_json(
            self._call(
                "get_contributing_flows",
                process_id=process_id,
                collection=collection,
                method_id=method_id,
                limit=limit,
            )
        )

    def get_contributing_activities(
        self,
        process_id: str,
        method_id: str,
        *,
        collection: str | None = None,
        limit: int | None = None,
    ) -> ContributingActivities:
        """Which upstream activities drive a given impact category.

        Same engine-side limitation as :meth:`get_contributing_flows`: no
        total exposed, so ``has_more`` cannot be derived. Inspect
        ``share_pct`` totals to gauge coverage. ``method_id`` takes a method
        name as well as a UUID, and ``collection`` is read off the resolved
        method unless you pin it.
        """
        collection, method_id = self._resolve_method(method_id, collection)
        return ContributingActivities.from_json(self._call(
            "get_contributing_activities",
            process_id=process_id,
            collection=collection,
            method_id=method_id,
            limit=limit,
        ))

    # -- Method collections --
    #
    # A method collection is an uploaded ILCD method file staged and loaded
    # independently of any database. Same list/load/unload/delete/upload shape
    # as reference data below, but its own endpoint family.

    def list_method_collections(self) -> list[dict]:
        """List every method collection the engine knows (loaded or staged).

        Each entry carries ``name``, ``displayName``, ``status``,
        ``methodCount`` and ``format``.
        """
        payload = self._json(
            self._session.get(f"{self.base_url}/api/v1/method-collections")
        )
        return payload["methods"]

    def load_method_collection(self, name: str) -> dict:
        """Load a staged method collection so its methods become available."""
        payload = self._json(
            self._session.post(f"{self.base_url}/api/v1/method-collections/{name}/load")
        )
        self._methods_cache.clear()
        return self._require_success(payload, "load_method_collection")

    def unload_method_collection(self, name: str) -> dict:
        """Unload a method collection from memory (the staged file is kept)."""
        payload = self._json(
            self._session.post(
                f"{self.base_url}/api/v1/method-collections/{name}/unload"
            )
        )
        self._methods_cache.clear()
        return self._require_success(payload, "unload_method_collection")

    def delete_method_collection(self, name: str) -> dict:
        """Delete a method collection: unload it and remove its staged file."""
        payload = self._json(
            self._session.delete(f"{self.base_url}/api/v1/method-collections/{name}")
        )
        self._methods_cache.clear()
        return self._require_success(payload, "delete_method_collection")

    def upload_method_collection(
        self,
        source: str | Path | bytes,
        name: str,
        *,
        description: str | None = None,
    ) -> dict:
        """Upload an ILCD method file as a staged method collection.

        ``source`` is a path to the method archive (or its raw ``bytes``).
        Same streamed-body + query-param shape as :meth:`upload_database`;
        returns the ``UploadResponse`` dict and raises VoLCAError on rejection.
        """
        return self._upload(
            "/api/v1/method-collections/upload",
            source,
            name,
            description,
            "upload_method_collection",
        )

    def export_method_collection(self, name: str, fmt: str = "simapro") -> bytes:
        """Export a loaded method collection, returning the serialized bytes.

        ``fmt`` names the target format: ``simapro`` (SimaPro method CSV),
        ``csv`` (columnar CSV, one column per impact category, the
        spreadsheet view), ``openlca`` (a zip of openLCA JSON-LD impact
        categories), or ``ilcd`` (a zip of an ILCD LCIA-method package,
        one method dataset per impact category plus its flow datasets).
        Projection warnings (anything the format cannot
        carry faithfully) arrive in the ``X-Volca-Export-Warnings`` response
        header and are surfaced through :mod:`warnings`. Raises VoLCAError
        on an HTTP error, including a collection that is not loaded.
        """
        fmt_norm = fmt.strip().lower()
        if fmt_norm not in _METHOD_EXPORT_FORMATS:
            raise VoLCAError(
                f"unknown method export format: {fmt!r} "
                f"(expected {'|'.join(sorted(_METHOD_EXPORT_FORMATS))})"
            )
        resp = self._session.post(
            f"{self.base_url}/api/v1/method-collections/{name}/export",
            json={"format": fmt_norm},
            headers={"Accept": "application/octet-stream"},
        )
        if resp.status_code >= 400:
            raise VoLCAError(
                f"export_method_collection failed (HTTP {resp.status_code}): "
                f"{resp.text[:500]}"
            )
        header = resp.headers.get("X-Volca-Export-Warnings", "")
        for line in urllib.parse.unquote(header).split("\n"):
            if line:
                warnings.warn(line, stacklevel=2)
        return resp.content

    # -- Reference data (flow synonyms, compartment mappings, units) --
    #
    # Three families, one URL scheme (/api/v1/{kind}/...), so these methods
    # take the family as a ``kind`` argument. ``kind`` is one of
    # "flow-synonyms", "compartment-mappings", "units", validated up front.

    def list_reference_data(self, kind: RefDataKind) -> list[dict]:
        """List reference-data sets of one ``kind`` (loaded, staged, or built-in).

        Each entry carries ``name``, ``displayName``, ``status``, ``isAuto``
        (a built-in bundled set) and ``entryCount``.
        """
        payload = self._json(
            self._session.get(f"{self.base_url}/api/v1/{_ref_kind(kind)}")
        )
        return payload["items"]

    def load_reference_data(self, kind: RefDataKind, name: str) -> dict:
        """Load a staged reference-data set of ``kind`` into memory."""
        payload = self._json(
            self._session.post(f"{self.base_url}/api/v1/{_ref_kind(kind)}/{name}/load")
        )
        return self._require_success(payload, "load_reference_data")

    def unload_reference_data(self, kind: RefDataKind, name: str) -> dict:
        """Unload a reference-data set of ``kind`` from memory."""
        payload = self._json(
            self._session.post(
                f"{self.base_url}/api/v1/{_ref_kind(kind)}/{name}/unload"
            )
        )
        return self._require_success(payload, "unload_reference_data")

    def delete_reference_data(self, kind: RefDataKind, name: str) -> dict:
        """Delete a reference-data set of ``kind`` and remove its staged file."""
        payload = self._json(
            self._session.delete(f"{self.base_url}/api/v1/{_ref_kind(kind)}/{name}")
        )
        return self._require_success(payload, "delete_reference_data")

    def upload_reference_data(
        self,
        kind: RefDataKind,
        source: str | Path | bytes,
        name: str,
        *,
        description: str | None = None,
    ) -> dict:
        """Upload a reference-data CSV of ``kind`` as a staged set.

        ``source`` is a path to the CSV (or its raw ``bytes``). Same
        streamed-body + query-param shape as :meth:`upload_database`.
        """
        return self._upload(
            f"/api/v1/{_ref_kind(kind)}/upload",
            source,
            name,
            description,
            "upload_reference_data",
        )

    def get_synonym_groups(self, name: str) -> list[list[str]]:
        """Return the synonym groups of a flow-synonyms set (lists of aliases)."""
        payload = self._json(
            self._session.get(f"{self.base_url}/api/v1/flow-synonyms/{name}/groups")
        )
        return payload["groups"]

    def download_flow_synonyms(self, name: str) -> bytes:
        """Download a flow-synonyms set as its raw CSV bytes.

        Raises VoLCAError on an HTTP error (e.g. the set does not exist).
        """
        resp = self._session.get(
            f"{self.base_url}/api/v1/flow-synonyms/{name}/download"
        )
        if resp.status_code >= 400:
            raise VoLCAError(
                f"download_flow_synonyms failed (HTTP {resp.status_code}): "
                f"{resp.text[:500]}",
                status_code=resp.status_code,
                body=resp.text,
            )
        return resp.content

    # -- Flow, method & instance detail --

    def get_flow(self, flow_id: str, db_name: str | None = None) -> FlowDetail:
        """Detail of one flow: its record, unit, and how many exchanges use it."""
        target = self._db(db_name)
        return FlowDetail.from_json(
            self._json(self._session.get(f"{self.base_url}/api/v1/db/{target}/flow/{flow_id}"))
        )

    def get_flow_activities(
        self, flow_id: str, db_name: str | None = None, *, role: str | None = None
    ) -> list[Activity]:
        """Activities on one side of a flow, or both.

        Args:
            flow_id: The flow to ask about.
            db_name: Database to ask; the current one by default.
            role: ``"producer"`` for the activities that make the flow,
                ``"consumer"`` for those that use it, ``"any"`` or omitted for
                both. Needs engine wire revision 16; an older engine would
                ignore the parameter and answer with both sides, so the
                request is refused rather than sent.

        Note that both sides together are narrower than asking for both: an
        avoided product is an exchange on the flow that neither makes it for
        sale nor consumes it, and only the unfiltered call lists it.
        """
        if role is not None:
            self._require_wire(16, "get_flow_activities(role=...)", engine_hint="0.12.1")
        target = self._db(db_name)
        raw = self._json(
            self._session.get(
                f"{self.base_url}/api/v1/db/{target}/flow/{flow_id}/activities",
                params=None if role is None else {"role": role},
            )
        )
        return [Activity.from_json(a) for a in raw]

    def get_method(self, method_id: str) -> MethodDetail:
        """Detail of one LCIA method: unit, category, methodology, factor count."""
        return MethodDetail.from_json(
            self._json(
                self._session.get(
                    f"{self.base_url}/api/v1/method/{self._method_uuid(method_id)}"
                )
            )
        )

    def get_method_factors(self, method_id: str) -> list[MethodFactor]:
        """The characterization factors of a method (flow, direction, value)."""
        raw = self._json(
            self._session.get(
                f"{self.base_url}/api/v1/method/{self._method_uuid(method_id)}/factors"
            )
        )
        return [MethodFactor.from_json(f) for f in raw]

    def get_mapping_status(
        self, method_id: str, db_name: str | None = None
    ) -> MappingStatus:
        """How well a method's factors map onto a database's biosphere flows.

        Reports the cascade breakdown (matched by UUID / CAS / name / synonym),
        the ``coverage`` fraction, and the ``unmapped_flows`` still without a CF.
        """
        target = self._db(db_name)
        return MappingStatus.from_json(
            self._json(
                self._session.get(
                    f"{self.base_url}/api/v1/db/{target}"
                    f"/method/{self._method_uuid(method_id)}/mapping"
                )
            )
        )

    def get_collection_coverage(
        self, collection: str, db_name: str | None = None
    ) -> CollectionCoverage:
        """How much of a database a whole method collection characterizes.

        Counts the distinct emission and resource flows at least one of the
        collection's methods resolves a factor for, with the same lookup
        scoring uses. Distinct across methods: their factors overlap, so the
        per-method figures from :meth:`get_mapping_status` do not add up to
        this number.
        """
        target = self._db(db_name)
        return CollectionCoverage.from_json(
            self._json(
                self._session.get(
                    f"{self.base_url}/api/v1/db/{target}/method-collection/{urllib.parse.quote(collection, safe='')}/coverage"
                )
            )
        )

    def get_stats(self) -> dict:
        """Return the engine's runtime statistics (memory use, loaded sizes).

        Keys are already snake_case on the wire, so this returns the raw dict.
        """
        return self._json(self._session.get(f"{self.base_url}/api/v1/stats"))


def _resolve_wire_name(py_name: str, op: _Operation) -> str | None:
    """Match a Python kwarg name to a spec parameter name, or return None."""
    spec_params = op.wire_names
    for candidate in _candidate_wire_names(py_name):
        if candidate in spec_params:
            return candidate
    return None
