"""HTTP client for the VoLCA REST API, dispatched by OpenAPI operationId.

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
next spec fetch with no code change. See ``docs/guides/pyvolca.md`` for
the user-facing view.

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
``get_impacts`` — all the endpoints that have POST-with-substitutions
variants in the Servant API.
"""

from __future__ import annotations

from typing import Any

import requests

from .types import (
    Activity,
    ActivityDetail,
    AggregateResult,
    ClassificationFilter,
    ConsumerResult,
    ConsumersResponse,
    DatabaseInfo,
    Exchange,
    PathResult,
    SupplyChain,
    parse_exchange_detail,
)


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


def _candidate_wire_names(py_name: str) -> list[str]:
    """Wire names to try for a Python kwarg, in priority order.

    The same Python name may map to different wire forms depending on
    whether the spec places it in ``path`` (Servant uses camelCase) or in
    ``query`` (Servant sometimes uses snake_case, sometimes kebab-case,
    sometimes camelCase — the aggregate endpoint is snake_case, for
    example). Return every plausible form; the caller matches against
    the spec's parameter list.
    """
    seen = []
    for cand in (py_name, _snake_to_camel(py_name), _snake_to_kebab(py_name)):
        if cand not in seen:
            seen.append(cand)
    return seen


_FORMATTED_SCALARS = (str, int, float)


def _format_query_value(value: Any) -> Any:
    """Convert a Python value to a query-string-ready form.

    Booleans become ``"true"``/``"false"``. Lists remain lists (requests
    encodes repeated keys for list values). Everything else is stringified.
    """
    if value is None:
        return None
    if isinstance(value, bool):
        return "true" if value else "false"
    if isinstance(value, _FORMATTED_SCALARS):
        return str(value)
    if isinstance(value, list):
        return [_format_query_value(v) for v in value]
    return str(value)


def _substitution_body(substitutions: list[dict]) -> dict:
    """Build the request body for substitution endpoints.

    Accepts Python-style dicts with ``from``/``to``/``consumer`` keys and
    rewrites them into the Servant ``SubstitutionRequest`` shape.
    """
    return {
        "srSubstitutions": [
            {"subFrom": s["from"], "subTo": s["to"], "subConsumer": s["consumer"]}
            for s in substitutions
        ]
    }


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
    """HTTP client for the VoLCA REST API.

    Usage::

        c = Client(db="agribalyse-3.2", password="1234")
        plants = c.search_activities(name="at plant")
        chain = c.get_supply_chain(plants[0].process_id, name="at farm")

    Substitutions can be passed to ``get_supply_chain``, ``get_inventory``,
    and ``get_impacts`` to compute results with a different upstream
    supplier — fast::

        subs = [{"from": old_pid, "to": new_pid, "consumer": consumer_pid}]
        result = c.get_impacts(pid, method_id=mid, substitutions=subs)
    """

    def __init__(
        self,
        base_url: str = "http://localhost:8080",
        db: str = "",
        password: str = "",
    ):
        self.base_url = base_url.rstrip("/")
        self.db = db
        self._session = requests.Session()
        self._session.headers["Accept"] = "application/json"
        if password:
            self._session.headers["Authorization"] = f"Bearer {password}"
        # Lazily fetched on first _call() invocation.
        self._operations: dict[str, _Operation] | None = None

    # -- Spec / dispatch plumbing --

    def _load_operations(self) -> dict[str, _Operation]:
        """Fetch the OpenAPI spec and build the dispatch table (cached)."""
        if self._operations is None:
            spec = self._json(self._session.get(f"{self.base_url}/api/v1/openapi.json"))
            self._operations = _parse_spec(spec)
        return self._operations

    def refresh_stubs(self) -> None:
        """Fetch the OpenAPI spec from the server and refresh the dispatch table.

        Also regenerates the `.pyi` type stubs in the installed pyvolca
        package directory so IDE autocomplete reflects the current engine.
        Useful when the engine is upgraded without reinstalling pyvolca.
        """
        spec = self._json(self._session.get(f"{self.base_url}/api/v1/openapi.json"))
        self._operations = _parse_spec(spec)
        from . import _stub_gen
        _stub_gen.write_stubs_for_spec(spec)

    def _call(
        self,
        operation_id: str,
        *,
        substitutions: list[dict] | None = None,
        **kwargs: Any,
    ) -> Any:
        """Dispatch an OpenAPI operation by ``operationId``.

        Path captures and query parameters come from the spec. Python
        kwarg names are canonicalized against the spec's parameter list
        (see :func:`_candidate_wire_names`). ``db_name`` defaults to the
        instance's ``self.db`` when the operation expects ``dbName`` and
        it wasn't explicitly passed.

        If ``substitutions`` is given and the spec's path supports POST
        with a ``SubstitutionRequest`` body, the operation is upgraded
        from GET to POST.
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

        # Drop None-valued kwargs — they shouldn't become query string entries.
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

        # Upgrade to POST when substitutions are supplied.
        method = op.method
        body: dict | None = None
        if substitutions:
            method = "POST"
            body = _substitution_body(substitutions)

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
                        f" (redirected from {r.history[0].url} — "
                        "auth headers are dropped on redirect, try using https://)"
                    )
                else:
                    hint = " (got HTML — is the URL correct?)"
            raise VoLCAError(
                f"Non-JSON response for {r.request.method} {r.url} "
                f"(status {r.status_code}){hint}",
                status_code=r.status_code,
                body=r.text,
            ) from None

    # -- Session state --

    def use(self, db_name: str) -> "Client":
        """Return a new client targeting a different database (shares session)."""
        c = Client.__new__(Client)
        c.base_url = self.base_url
        c.db = db_name
        c._session = self._session
        c._operations = self._operations  # share the dispatch table
        return c

    def get_version(self) -> dict:
        """Return server version info (version, gitHash, gitTag, buildTarget).

        Uses a direct HTTP call — ``/api/v1/version`` has no operationId
        since it predates the Resources ADT.
        """
        return self._json(self._session.get(f"{self.base_url}/api/v1/version"))

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
    # ``client.call(operation_id, **kw)`` — see the ``call`` method below.

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
        # No operationId (infrastructure endpoint). Direct HTTP.
        return self._json(self._session.post(f"{self.base_url}/api/v1/db/{db_name}/load"))

    def unload_database(self, db_name: str) -> dict:
        return self._json(self._session.post(f"{self.base_url}/api/v1/db/{db_name}/unload"))

    def list_presets(self) -> list[dict]:
        """List classification presets configured in this instance."""
        return self._call("list_presets")

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
        limit: int | None = None,
        offset: int = 0,
        exact: bool = False,
    ) -> list[Activity]:
        raw = self._call(
            "search_activities",
            name=name,
            geo=geo,
            product=product,
            preset=preset,
            classification=classification,
            classification_value=classification_value,
            limit=limit,
            offset=offset,
            exact=exact,
        )
        return [Activity.from_json(a) for a in raw["results"]]

    def search_flows(self, query: str | None = None, *, limit: int | None = None) -> list[dict]:
        raw = self._call("search_flows", q=query, limit=limit)
        return raw["results"]

    def list_classifications(self) -> list[dict]:
        """List classification systems and their values for the current database."""
        return self._call("list_classifications")

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
        substitutions: list[dict] | None = None,
        include_edges: bool | None = None,
    ) -> SupplyChain:
        """Get the flat supply chain of an activity.

        Args:
            max_depth: Max hops from root. 1 = direct inputs only.
            classification_filters: Restrict entries to those matching any
                of the given ClassificationFilter triples. Multiple filters
                are AND-combined by the server.
            substitutions: When provided, the call is upgraded to POST and
                the scaling vector is recomputed with the substituted
                suppliers.
        """
        classifications = [f.system for f in classification_filters or []]
        classification_values = [f.value for f in classification_filters or []]
        classification_modes = [f.mode for f in classification_filters or []]
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
            include_edges=include_edges,
            substitutions=substitutions,
        )
        return SupplyChain.from_json(raw)

    # -- Aggregate primitive --

    def aggregate(
        self,
        process_id: str,
        scope: str,
        *,
        is_input: bool | None = None,
        max_depth: int | None = None,
        filter_name: str | None = None,
        filter_name_not: list[str] | str | None = None,
        filter_unit: str | None = None,
        preset: str | None = None,
        filter_classification: list[ClassificationFilter] | None = None,
        filter_target_name: str | None = None,
        filter_is_reference: bool | None = None,
        group_by: str | None = None,
        aggregate: str | None = None,
    ) -> AggregateResult:
        """SQL-group-by aggregation over direct exchanges, supply chain, or biosphere flows.

        Args:
            scope: ``"direct"`` | ``"supply_chain"`` | ``"biosphere"``.
            group_by: omit for a single-bucket result (just the totals).
                Supported keys: ``"name"``, ``"flow_id"``, ``"name_prefix"``,
                ``"unit"``, ``"location"``, ``"target_name"``,
                ``"classification.<system>"``.
            aggregate: ``"sum_quantity"`` (default), ``"count"``, or ``"share"``.
        """
        # filter_classification goes over the wire as "System=Value[:exact]" strings.
        if filter_classification:
            filter_strings = [
                f"{f.system}={f.value}" + (":exact" if f.mode == "exact" else "")
                for f in filter_classification
            ]
        else:
            filter_strings = None
        # filter_name_not: accept list or comma-string, send as comma-string.
        if isinstance(filter_name_not, list):
            filter_name_not_csv: str | None = ",".join(filter_name_not)
        else:
            filter_name_not_csv = filter_name_not
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
        limit: int | None = None,
        max_depth: int | None = None,
        include_edges: bool = False,
    ) -> ConsumersResponse:
        """Find all activities that transitively consume this supplier.

        Args:
            max_depth: Max hops from supplier. 1 = direct consumers only.
            classification_filters: ClassificationFilter entries restricting
                the results. Multiple filters are AND-combined by the server.
                Mode is ``"exact"`` or ``"contains"``.
            include_edges: When True, the response carries every technosphere
                edge whose endpoints are both reachable from the supplier.
                Callers can walk these to reconstruct supplier→consumer paths
                without a second ``get_path_to`` round-trip.

        Returns a :class:`ConsumersResponse` whose ``consumers`` attribute is
        the paginated consumer list and whose ``edges`` attribute carries the
        traversal subgraph (empty by default).
        """
        classifications = [f.system for f in classification_filters or []]
        classification_values = [f.value for f in classification_filters or []]
        classification_modes = [f.mode for f in classification_filters or []]
        raw = self._call(
            "get_consumers",
            process_id=process_id,
            name=name,
            location=location,
            product=product,
            preset=preset,
            classification=classifications or None,
            classification_value=classification_values or None,
            classification_mode=classification_modes or None,
            limit=limit,
            max_depth=max_depth,
            include_edges=include_edges,
        )
        return ConsumersResponse.from_json(raw)

    def get_path_to(self, process_id: str, target: str) -> PathResult:
        """Find the shortest upstream path from process to first activity whose name matches target.

        Returns a PathResult whose path is ordered root → target. Each step
        includes cumulative_quantity, scaling_factor, and (except the root)
        local_step_ratio.
        """
        return PathResult.from_json(
            self._call("get_path_to", process_id=process_id, target=target)
        )

    # -- Tree (SPA-only endpoint, no operationId — direct HTTP) --

    def get_tree(self, process_id: str) -> dict:
        """Fetch the recursive activity tree used by the analysis SPA.

        ``/tree`` has no operationId in the OpenAPI spec — it's kept for the
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
        substitutions: list[dict] | None = None,
    ) -> dict:
        return self._call(
            "get_inventory",
            process_id=process_id,
            flow=flow,
            limit=limit,
            substitutions=substitutions,
        )

    def get_impacts(
        self,
        process_id: str,
        method_id: str,
        *,
        collection: str = "methods",
        top_flows: int | None = None,
        substitutions: list[dict] | None = None,
    ) -> dict:
        """Compute impact assessment (LCIA) scores for an activity.

        Historically called ``get_lcia``. Internal types still use the LCIA
        acronym.

        Args:
            collection: Method collection name. Defaults to ``"methods"`` for
                single-method calls; most engines expose methods under a
                single collection.
            top_flows: Max top contributing flows to return (default 5).
        """
        return self._call(
            "get_impacts",
            process_id=process_id,
            collection=collection,
            method_id=method_id,
            top_flows=top_flows,
            substitutions=substitutions,
        )

    # -- Methods --

    def list_methods(self) -> list[dict]:
        return self._call("list_methods")

    def get_flow_mapping(self, method_id: str) -> dict:
        """Get the characterization-factor-to-database-flow mapping coverage."""
        return self._call("get_flow_mapping", method_id=method_id)

    def get_characterization(
        self,
        method_id: str,
        *,
        flow: str | None = None,
        limit: int | None = None,
    ) -> dict:
        """Look up characterization factors for a method matched to database flows."""
        return self._call("get_characterization", method_id=method_id, flow=flow, limit=limit)

    def get_contributing_flows(
        self,
        process_id: str,
        method_id: str,
        *,
        collection: str = "methods",
        limit: int | None = None,
    ) -> dict:
        """Which elementary flows drive a given impact category."""
        return self._call(
            "get_contributing_flows",
            process_id=process_id,
            collection=collection,
            method_id=method_id,
            limit=limit,
        )

    def get_contributing_activities(
        self,
        process_id: str,
        method_id: str,
        *,
        collection: str = "methods",
        limit: int | None = None,
    ) -> dict:
        """Which upstream activities drive a given impact category."""
        return self._call(
            "get_contributing_activities",
            process_id=process_id,
            collection=collection,
            method_id=method_id,
            limit=limit,
        )


def _resolve_wire_name(py_name: str, op: _Operation) -> str | None:
    """Match a Python kwarg name to a spec parameter name, or return None."""
    spec_params = op.wire_names
    for candidate in _candidate_wire_names(py_name):
        if candidate in spec_params:
            return candidate
    return None
