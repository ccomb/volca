# pyvolca

Python client for [VoLCA](https://github.com/ccomb/volca) — Life Cycle Assessment engine over Agribalyse and ecoinvent.

> **Full guide and tutorials**: <https://volca.run/docs/guides/pyvolca/>
> **Issues / source**: <https://github.com/ccomb/volca>

## Install

```bash
pip install pyvolca
```

Requires Python ≥ 3.10 and a running VoLCA engine. Use `Server` (below) to run one as a child process, or point `Client` at any reachable instance.

## First choose: connect to an existing server, or start one locally

`pyvolca` is only the Python client library. It does not contain the VoLCA databases and it does not install the VoLCA engine binary.

Most users should start with one of these two modes:

- **You already have access to a VoLCA server** (for example a hosted server prepared by someone else): use `Client` only. You do not need `volca.toml`, and you do not need to install the VoLCA server locally.
- **You want Python to start a local VoLCA engine process for you**: use `download()` once to fetch the VoLCA engine binary and reference data into the shared volca install dir (see [Where artefacts are installed](#where-artefacts-are-installed)), then use `Server` to start it from Python. `volca.toml` is still a normal file path passed to `Server(config=...)`; put it in your project directory, or pass an absolute path. Do not put it inside your virtualenv or inside `site-packages`.

For a hosted server, the minimal connection looks like this:

```python
# no-test  — replace with your real hosted VoLCA server URL and credentials.
from volca import Client

c = Client(
    base_url="https://your-volca-server.example.com",
    db="agribalyse-3.2",
    password="your-api-token-or-password",
)

print(c.list_databases())
```

Use `download()` + `Server` only when you deliberately want to download and launch the engine from Python:

```python
# no-test  — downloads the engine and needs a real engine config/database.
from volca import Client, Server, download

installed = download()  # cached after the first run

with Server(config="./volca.toml", binary=str(installed.binary)) as srv:
    c = Client(base_url=srv.base_url, db="agribalyse-3.2", password=srv.password)
    print(c.list_databases())
```

In this local mode, `download()` stores the engine binary and reference data in the shared volca install dir (see below). `Server(config="./volca.toml")` still means “read `./volca.toml` relative to the current working directory”.

### Where artefacts are installed

`download()` writes to the same OS-native location as the `install.sh` / `install.ps1` shell installers, so any of the three tools populate the same directory:

| Platform | Default install root |
|---|---|
| Linux   | `${XDG_DATA_HOME:-~/.local/share}/volca/` |
| macOS   | `~/Library/Application Support/volca/` |
| Windows | `%LOCALAPPDATA%\volca\` |

Override with `VOLCA_HOME=/full/path` (full path; skips OS detection).

If you ran `install.sh` or `install.ps1` first, `Server()` finds the installed engine without an extra `download()` call. If you previously used `pyvolca < 0.4` it cached artefacts under `<user_cache_dir>/pyvolca/` (Linux: `~/.cache/pyvolca/`); that directory is no longer read and can be removed (`rm -rf ~/.cache/pyvolca`).

## Local managed-server quick start

```python
# no-test  — needs a real engine; the snippets below run against a mocked Client.
from volca import Client, Server

with Server(config="volca.toml") as srv:
    c = Client(base_url=srv.base_url, db="agribalyse-3.2", password=srv.password)
    plants = c.search_activities(name="wheat flour, at plant", limit=5)
    chain = c.get_supply_chain(plants[0].process_id, name="at farm")
    score = c.get_impacts(plants[0].process_id, method_id=c.list_methods()[0]["methodId"])
```

This example starts a local engine process from Python. `Server` reads `port` and `password` from the TOML config. The engine self-stops after `idle_timeout` seconds without traffic (default 5 min).

> Examples below assume `c` is a `Client` instance — construct it with the snippet above, or against an already-running server: `c = Client(base_url="http://localhost:8080", db="agribalyse-3.2", password="…")`.

## Discover what's available

> *Which databases are loaded? Which LCIA methods can I score against? What classification systems can I filter on?*

```python
for db in c.list_databases():
    print(f"  {db.name} [{db.status}]: {db.activity_count} activities")

for m in c.list_methods()[:5]:
    print(f"  {m['methodId']}  {m['name']} [{m['unit']}]")
```

Other listings: `c.list_classifications()` returns the classification systems and their values for the current database; `c.list_presets()` returns named filter presets configured in the engine. Use `c.load_database(name)` / `c.unload_database(name)` to manage memory if a database isn't auto-loaded.

## Find an activity

> *Which activity in the database represents the product I want to assess?*

```python
plants = c.search_activities(name="wheat flour, at plant", limit=5)
for a in plants:
    print(f"{a.process_id}  {a.name} ({a.location})")
```

Each `Activity` carries `process_id`, `name`, `location`, `product`, `product_amount`, `product_unit`. Narrow the query with `geo="FR"`, `classification=`/`classification_value=` (ISIC/CPC), or set `exact=True` for an exact-name match. To search by flow name (technosphere products and biosphere flows) instead of activity name, use `c.search_flows(query=...)`.

## Inspect an activity

> *What goes into making this product? What does it emit? What's its reference unit?*

```python
detail = c.get_activity(plants[0].process_id)
for ex in detail.technosphere_inputs:
    print(f"{ex.amount:.4g} {ex.unit} of {ex.flow_name} ← {ex.target_activity}")
```

`get_activity` returns a typed `ActivityDetail`. Use `.inputs` / `.outputs` / `.technosphere_inputs` to filter the exchanges; each entry is an `Exchange` — either a `TechnosphereExchange` (an input or output of an intermediate product) or a `BiosphereExchange` (resource extracted or pollutant emitted).

## Trace the upstream supply chain

> *What's the full upstream chain — every ingredient, recursively, down to the farm or mine?*

```python
chain = c.get_supply_chain(plants[0].process_id, name="at farm", limit=20)
print(f"{chain.filtered_activities} of {chain.total_activities} upstream activities match 'at farm'")
for entry in chain.entries[:5]:
    print(f"  {entry.quantity:.4g} {entry.unit} of {entry.name} ({entry.location})")
```

For *"how exactly does this root reach a specific upstream supplier?"*, use `get_path_to(process_id, target=...)` — returns a `PathResult` of ordered `PathStep`s root → target with cumulative quantities and step ratios.

## Find downstream consumers

> *Where is this supplier used? Which products depend on it?*

```python
result = c.get_consumers(plants[0].process_id, max_depth=2, limit=10)
for cons in result.consumers:
    print(f"  depth={cons.depth}  {cons.name} ({cons.location})")
```

Returns a `ConsumersResponse` with `consumers`, pagination, and (when `include_edges=True`) the technosphere edges so callers can reconstruct supplier→consumer paths without a second round trip. Pass `classification_filters=[...]` to restrict to a category.

## Compute the life-cycle inventory

> *What are the cumulative biosphere flows (CO₂, water, methane, …) per functional unit, before applying any characterization method?*

```python
inv = c.get_inventory(plants[0].process_id, limit=20)
# inv is a raw dict — see the OpenAPI spec for the full shape.
# Substitutions are accepted: c.get_inventory(pid, substitutions=[...])
```

The inventory is what every LCIA method runs on top of. If you only need *grouped* views (by name, location, classification, etc.), reach for `c.aggregate(scope="biosphere", group_by=...)` instead — same data, summarized.

## Compute environmental impacts (LCIA)

> *What's the carbon footprint of this product? Which emissions dominate the score?*

```python
score = c.get_impacts(plants[0].process_id, method_id="EF3.1-climate-change", top_flows=5)
print(f"{score.score:.4g} {score.unit}")
for c_flow in score.top_contributors:
    print(f"  {c_flow.share_pct:.1f}%  {c_flow.flow_name}")
```

`LCIAResult` carries the score, unit, optional `normalized_score` / `weighted_score` (in Pt), and the top contributing biosphere flows with their `share_pct`.

> *Compute every impact category in one go — climate, water, land use, …*

```python
batch = c.get_impacts_batch(plants[0].process_id)
for r in batch.results:
    print(f"  {r.category}: {r.score:.4g} {r.unit}")
if batch.single_score is not None:
    print(f"PEF single score: {batch.single_score:.4g} {batch.single_score_unit}")
```

`LCIABatchResult` also surfaces formula-based scoring sets (PEF, ECS…) via `scoring_results` and `scoring_indicators`, so you can render a per-indicator chart alongside the aggregate single score.

## Drill into what drives a single impact

> *I have a climate-change score. Which biosphere flows account for it? Which upstream activities?*

`get_impacts(...).top_contributors` already returns the top biosphere flows for a single LCIA call. For a deeper or differently-bounded view — and for the *activity* attribution view — use the standalone drill-down endpoints:

```python
flows = c.get_contributing_flows(
    plants[0].process_id,
    method_id="EF3.1-climate-change",
    limit=10,
)
acts = c.get_contributing_activities(
    plants[0].process_id,
    method_id="EF3.1-climate-change",
    limit=10,
)
# Both return raw dicts — the shape is documented in the OpenAPI spec.
```

> *Which characterization factors does a method apply, and to which database flows?*

```python
char = c.get_characterization(method_id="EF3.1-climate-change", limit=20)
```

Useful for sanity-checking method coverage or building custom indicators on top of the engine's mapping.

## Aggregate flows by group

> *What are the top emitting substances? How do flows break down by category, location, or classification?*

```python
agg = c.aggregate(
    plants[0].process_id,
    scope="biosphere",
    group_by="name",
    aggregate="sum_quantity",
)
for g in agg.groups[:5]:
    print(f"  {g.quantity:.4g} {g.unit or ''} of {g.key}")
```

`scope` selects what to aggregate over: `"direct"` (just this activity's exchanges), `"supply_chain"` (cumulative upstream), or `"biosphere"` (all elementary flows). `group_by` accepts `"name"`, `"flow_id"`, `"unit"`, `"location"`, `"target_name"`, or `"classification.<system>"`.

## Compare two activities

> *How does variant A differ from variant B? Which inputs change?*

```python
from volca import compare_activities

diff = compare_activities(c, plants[0].process_id, plants[1].process_id, scope="direct")
print(f"  matched: {len(diff.matched)}, only-left: {len(diff.left_only)}, only-right: {len(diff.right_only)}")
for row in diff.matched[:3]:
    print(f"    {row.key}: {row.left:.4g} → {row.right:.4g}  (Δ={row.delta:+.4g})")
```

A client-side merge over two `aggregate` calls. Groups by `flow_id` (default) so matching is stable across naming variants. Pass `scope="supply_chain"` to compare cumulative inputs instead of direct exchanges.

## Run counterfactuals (substitutions)

> *What if I used organic wheat instead of conventional? Recycled aluminium instead of virgin? — without reloading the database.*

The engine applies a Sherman–Morrison rank-1 update, so substitutions are fast regardless of database size. Works on `get_supply_chain`, `get_inventory`, and `get_impacts`.

```python
subs = [{
    "from": "old-supplier-pid",      # the activity to replace
    "to":   "new-supplier-pid",      # the replacement
    "consumer": "consumer-pid",      # the activity that directly uses the old supplier
}]
score = c.get_impacts(plants[0].process_id, method_id="EF3.1-climate-change", substitutions=subs)
```

Multiple substitutions chain in one call — the `consumer` field disambiguates *where* in the chain each swap applies.

## Handle errors

> *The activity doesn't exist, the engine is down, or the request is malformed — what do I catch?*

```python
from volca import VoLCAError

try:
    score = c.get_impacts("nonexistent-pid", method_id="EF3.1-climate-change")
except VoLCAError as e:
    print(f"  failed: {e.status_code} — {e.body[:80]}")
```

`VoLCAError.status_code` is the HTTP status when the engine returned one; `body` is the raw response body.

## Switch databases

> *I want to run the same workflow against ecoinvent instead of Agribalyse — without rebuilding the client.*

```python
ei = c.use("ecoinvent-3.10")
ei_results = ei.search_activities(name="electricity, high voltage")
```

`Client.use(db_name)` returns a new `Client` targeting a different database while sharing the HTTP session and dispatch table — no spec re-fetch.

## Refresh IDE autocomplete after upgrading the engine

> *I just upgraded the VoLCA server. How do I get my editor to see the new endpoints?*

```python
c.refresh_stubs()
```

Pyvolca dispatches dynamically against the engine's OpenAPI spec, so it ships without `.pyi` stubs. `refresh_stubs()` refetches the spec and writes stubs into the installed package directory; restart your language server to pick them up.

## API reference

<!-- BEGIN: api-reference -->
_This reference is generated from the installed package. Run `python scripts/gen_api_md.py` to regenerate._

## Classes

### `Client`

HTTP client for the VoLCA REST API.

Usage::

    c = Client(db="agribalyse-3.2", password="1234")
    plants = c.search_activities(name="at plant")
    chain = c.get_supply_chain(plants[0].process_id, name="at farm")

Substitutions can be passed to ``get_supply_chain``, ``get_inventory``,
and ``get_impacts`` to compute results with a different upstream
supplier — fast::

    subs = [{"from": old_pid, "to": new_pid, "consumer": consumer_pid}]
    result = c.get_impacts(pid, method_id=mid, substitutions=subs)

**Constructor**: `Client(base_url: str = 'http://localhost:8080', db: str = '', password: str = '')`

**Methods**:

- `aggregate(process_id: str, scope: str, *, is_input: bool | None = None, max_depth: int | None = None, filter_name: str | None = None, filter_name_not: list[str] | str | None = None, filter_unit: str | None = None, preset: str | None = None, filter_classification: list[ClassificationFilter] | None = None, filter_target_name: str | None = None, filter_is_reference: bool | None = None, group_by: str | None = None, aggregate: str | None = None) -> AggregateResult` — SQL-group-by aggregation over direct exchanges, supply chain, or biosphere flows.
- `call(operation_id: str, **kwargs) -> Any` — Escape hatch: call any OpenAPI operation by operationId.
- `get_activity(process_id: str) -> ActivityDetail` — Fetch an activity's full detail.
- `get_characterization(method_id: str, *, flow: str | None = None, limit: int | None = None) -> dict` — Look up characterization factors for a method matched to database flows.
- `get_consumers(process_id: str, *, name: str | None = None, location: str | None = None, product: str | None = None, preset: str | None = None, classification_filters: list[ClassificationFilter] | None = None, limit: int | None = None, max_depth: int | None = None, include_edges: bool = False) -> ConsumersResponse` — Find all activities that transitively consume this supplier.
- `get_contributing_activities(process_id: str, method_id: str, *, collection: str = 'methods', limit: int | None = None) -> dict` — Which upstream activities drive a given impact category.
- `get_contributing_flows(process_id: str, method_id: str, *, collection: str = 'methods', limit: int | None = None) -> dict` — Which elementary flows drive a given impact category.
- `get_flow_mapping(method_id: str) -> dict` — Get the characterization-factor-to-database-flow mapping coverage.
- `get_impacts(process_id: str, method_id: str, *, collection: str = 'methods', top_flows: int | None = None, substitutions: list[dict] | None = None) -> LCIAResult` — Compute the LCIA score for a single impact category on an activity.
- `get_impacts_batch(process_id: str, *, collection: str = 'methods', substitutions: list[dict] | None = None) -> LCIABatchResult` — Compute LCIA for every impact category in a collection, in one call.
- `get_inputs(process_id: str) -> list[Exchange]` — Return the input exchanges of an activity (richer metadata than ``get_activity``).
- `get_inventory(process_id: str, *, flow: str | None = None, limit: int | None = None, substitutions: list[dict] | None = None) -> dict` — Compute the life-cycle inventory (cumulative biosphere flows) for an activity.
- `get_outputs(process_id: str) -> list[Exchange]` — Return the output exchanges of an activity. See :meth:`get_inputs` for notes.
- `get_path_to(process_id: str, target: str) -> PathResult` — Find the shortest upstream path from process to first activity whose name matches target.
- `get_supply_chain(process_id: str, *, name: str | None = None, location: str | None = None, limit: int | None = None, min_quantity: float | None = None, max_depth: int | None = None, preset: str | None = None, classification_filters: list[ClassificationFilter] | None = None, substitutions: list[dict] | None = None, include_edges: bool | None = None) -> SupplyChain` — Get the flat supply chain of an activity.
- `get_tree(process_id: str) -> dict` — Fetch the recursive activity tree used by the analysis SPA.
- `get_version()` — Return server version info (version, gitHash, gitTag, buildTarget).
- `list_classifications()` — List classification systems and their values for the current database.
- `list_databases()` — List every database declared in the engine config.
- `list_methods()` — List every LCIA method available in the engine.
- `list_presets()` — List classification presets configured in this instance.
- `load_database(db_name: str) -> dict` — Load a database into memory so it answers queries.
- `refresh_stubs()` — Fetch the OpenAPI spec from the server and refresh the dispatch table.
- `search_activities(name: str | None = None, *, geo: str | None = None, product: str | None = None, preset: str | None = None, classification: str | None = None, classification_value: str | None = None, limit: int | None = None, offset: int = 0, exact: bool = False) -> list[Activity]` — Search activities in the current database.
- `search_flows(query: str | None = None, *, limit: int | None = None) -> list[dict]` — Search flows (technosphere products and biosphere flows) in the current database.
- `unload_database(db_name: str) -> dict` — Unload a database from memory to free RAM. The disk copy is kept.
- `use(db_name: str) -> 'Client'` — Return a new client targeting a different database (shares session).

### `Server`

Manages the VoLCA server process.

Usage::

    with Server(config="volca.toml") as srv:
        client = Client(base_url=srv.base_url, db="agribalyse-3.2", password=srv.password)
        activities = client.search_activities(name="at plant")

**Constructor**: `Server(config: str = 'volca.toml', port: int = 0, binary: str = 'volca')`

**Properties**:

- `base_url` — (no docstring)

**Methods**:

- `is_alive()` — Health check — GET /api/v1/db, return True if 200.
- `start(idle_timeout: int = 300, wait_timeout: int = 120) -> None` — Spawn the engine process if it is not already serving, and wait until ready.
- `stop()` — Stop the server via shutdown endpoint, then terminate process.

## Exceptions

### `DownloadError`

Raised when the download or verification fails.

### `VoLCAError`

Error from the VoLCA API.

**Constructor**: `VoLCAError(message: str, status_code: int | None = None, body: str = '')`

## Data types

### `Activity`

| Field | Type | Default |
|-------|------|---------|
| `process_id` | `str` | — |
| `name` | `str` | — |
| `location` | `str` | — |
| `product` | `str` | — |
| `product_amount` | `float` | — |
| `product_unit` | `str` | — |

### `ActivityDetail`

Typed wrapper around the JSON returned by GET /activity/{pid}.

Use the .inputs / .outputs / .technosphere_inputs convenience properties
instead of walking the raw exchanges list.

| Field | Type | Default |
|-------|------|---------|
| `process_id` | `str` | — |
| `name` | `str` | — |
| `location` | `str` | — |
| `unit` | `str` | — |
| `description` | `list[str]` | — |
| `classifications` | `dict[str, str]` | — |
| `reference_product` | `str \| None` | — |
| `reference_product_amount` | `float \| None` | — |
| `reference_product_unit` | `str \| None` | — |
| `all_products` | `list[Activity]` | — |
| `exchanges` | `list[Union[TechnosphereExchange, BiosphereExchange]]` | — |

### `ActivityDiff`

Result of ``compare_activities``.

| Field | Type | Default |
|-------|------|---------|
| `scope` | `str` | — |
| `group_by` | `str` | — |
| `matched` | `list[ActivityDiffRow]` | list() |
| `left_only` | `list[ActivityDiffRow]` | list() |
| `right_only` | `list[ActivityDiffRow]` | list() |

### `ActivityDiffRow`

One matched or unmatched flow in an activity comparison.

| Field | Type | Default |
|-------|------|---------|
| `key` | `str` | — |
| `left` | `float \| None` | — |
| `right` | `float \| None` | — |
| `unit` | `str \| None` | — |

### `AggregateGroup`

One bucket inside an AggregateResult.

| Field | Type | Default |
|-------|------|---------|
| `key` | `str` | — |
| `quantity` | `float` | — |
| `count` | `int` | — |
| `unit` | `str \| None` | None |
| `share` | `float \| None` | None |

### `AggregateResult`

Result of a Client.aggregate() call.

``filtered_total`` is the sum across all items matching the filters (the
top-level number). ``groups`` is the per-bucket breakdown when ``group_by``
was set; empty otherwise.

| Field | Type | Default |
|-------|------|---------|
| `scope` | `str` | — |
| `filtered_total` | `float` | — |
| `filtered_unit` | `str \| None` | — |
| `filtered_count` | `int` | — |
| `groups` | `list[AggregateGroup]` | list() |

### `BiosphereExchange`

An exchange with the environment (resource extraction or emission).

| Field | Type | Default |
|-------|------|---------|
| `flow_name` | `str` | — |
| `flow_category` | `str` | — |
| `amount` | `float` | — |
| `unit` | `str` | — |
| `is_input` | `bool` | — |
| `comment` | `str \| None` | None |
| `is_biosphere` | `bool` | True |

### `ClassificationFilter`

Filter a supply-chain/consumers query by a classification (system, value, mode).

Matches one classification system entry (e.g. ("Category", "Agricultural\Food",
"exact")). Mode is "exact" (case-insensitive equality) or "contains" (substring).
Multiple filters are AND-combined by the server.

| Field | Type | Default |
|-------|------|---------|
| `system` | `str` | — |
| `value` | `str` | — |
| `mode` | `str` | 'contains' |

### `ConsumerResult`

Activity that consumes a given supplier, with BFS depth.

| Field | Type | Default |
|-------|------|---------|
| `process_id` | `str` | — |
| `name` | `str` | — |
| `location` | `str` | — |
| `product` | `str` | — |
| `product_amount` | `float` | — |
| `product_unit` | `str` | — |
| `depth` | `int` | — |
| `classifications` | `dict[str, str]` | dict() |

### `ConsumersResponse`

Reverse supply chain (/consumers) — paginated consumer list plus
optional edge set. Mirrors :class:`SupplyChain` so callers have a
uniform {entries, edges} shape in both traversal directions.
``edges`` is populated only when ``include_edges=True``.

| Field | Type | Default |
|-------|------|---------|
| `consumers` | `list[ConsumerResult]` | — |
| `total` | `int` | — |
| `offset` | `int` | — |
| `limit` | `int` | — |
| `has_more` | `bool` | — |
| `search_time_ms` | `float` | — |
| `edges` | `list[SupplyChainEdge]` | list() |

### `DatabaseInfo`

One entry of :meth:`Client.list_databases`.

``depends_on`` names the databases this one links against for cross-DB
flow resolution — mirrors the ``dependsOn`` list surfaced by the relink
endpoint. Derived from the engine's declared topology, not runtime state.

| Field | Type | Default |
|-------|------|---------|
| `name` | `str` | — |
| `display_name` | `str` | — |
| `status` | `str` | — |
| `path` | `str` | — |
| `load_at_startup` | `bool` | False |
| `is_uploaded` | `bool` | False |
| `activity_count` | `int` | 0 |
| `description` | `str \| None` | None |
| `format` | `str \| None` | None |
| `depends_on` | `list[str]` | list() |

### `FlowContribution`

Top contributing elementary flow for an impact category.

Emitted inside ``LCIAResult.top_contributors``.

| Field | Type | Default |
|-------|------|---------|
| `flow_name` | `str` | — |
| `contribution` | `float` | — |
| `share_pct` | `float` | — |
| `flow_id` | `str` | — |
| `category` | `str` | — |
| `cf_value` | `float` | 0.0 |
| `compartment` | `str \| None` | None |

### `Installed`

Result of :func:`download`.

| Field | Type | Default |
|-------|------|---------|
| `binary` | `Path` | — |
| `data_dir` | `Path` | — |
| `version` | `str` | — |
| `data_version` | `str` | — |

### `LCIABatchResult`

Batch LCIA: every impact category in a method collection, for one activity.

Returned by :meth:`Client.get_impacts_batch`. Carries the per-method
impact results plus any formula-based scoring sets configured in the
engine TOML (PEF, ECS, or any named set).

``scoring_indicators`` gives the per-variable normalized-weighted
breakdown of each scoring set — already multiplied by the set's
``displayMultiplier`` and expressed in its display unit (see
:class:`ScoringIndicator`). Lets callers render per-indicator charts
alongside the aggregate ``scoring_results``.

| Field | Type | Default |
|-------|------|---------|
| `results` | `list[LCIAResult]` | — |
| `single_score` | `float \| None` | None |
| `single_score_unit` | `str \| None` | None |
| `norm_weight_set_name` | `str \| None` | None |
| `available_nw_sets` | `list[str]` | list() |
| `scoring_results` | `dict[str, dict[str, float]]` | dict() |
| `scoring_units` | `dict[str, str]` | dict() |
| `scoring_indicators` | `dict[str, dict[str, ScoringIndicator]]` | dict() |

### `LCIAResult`

LCIA score for one impact category on one activity.

Returned directly by :meth:`Client.get_impacts`, and nested inside
:class:`LCIABatchResult.results` (one entry per impact category).

| Field | Type | Default |
|-------|------|---------|
| `method_id` | `str` | — |
| `method_name` | `str` | — |
| `category` | `str` | — |
| `damage_category` | `str` | — |
| `score` | `float` | — |
| `unit` | `str` | — |
| `mapped_flows` | `int` | — |
| `functional_unit` | `str` | — |
| `normalized_score` | `float \| None` | None |
| `weighted_score` | `float \| None` | None |
| `top_contributors` | `list[FlowContribution]` | list() |

### `PathResult`

Shortest upstream path from a root process to a matching activity.

| Field | Type | Default |
|-------|------|---------|
| `path` | `list[PathStep]` | — |
| `path_length` | `int` | — |
| `total_ratio` | `float` | — |

### `PathStep`

One step in the supply chain path returned by get_path_to.

Note: the /path endpoint emits snake_case JSON directly (built via
aeson's `object [...]` rather than generic ToJSON), so it bypasses
the engine's stripLowerPrefix transform.

| Field | Type | Default |
|-------|------|---------|
| `process_id` | `str` | — |
| `name` | `str` | — |
| `location` | `str` | — |
| `unit` | `str` | — |
| `cumulative_quantity` | `float` | — |
| `scaling_factor` | `float` | — |
| `local_step_ratio` | `float \| None` | None |

### `ScoringIndicator`

One per-variable entry inside ``LCIABatchResult.scoring_indicators``.

``value`` is pre-multiplied by the scoring set's ``displayMultiplier``
(configured in the scoring TOML) and expressed in the set's display unit.
``category`` names the impact category the variable was resolved from.

| Field | Type | Default |
|-------|------|---------|
| `category` | `str` | — |
| `value` | `float` | — |

### `SupplyChain`

| Field | Type | Default |
|-------|------|---------|
| `root` | `Activity` | — |
| `total_activities` | `int` | — |
| `filtered_activities` | `int` | — |
| `entries` | `list[SupplyChainEntry]` | list() |
| `edges` | `list[SupplyChainEdge]` | list() |

### `SupplyChainEdge`

`from`/`to` are Python keywords, so they're stored under from_id/to_id.

| Field | Type | Default |
|-------|------|---------|
| `from_id` | `str` | — |
| `to_id` | `str` | — |
| `amount` | `float` | — |

### `SupplyChainEntry`

| Field | Type | Default |
|-------|------|---------|
| `process_id` | `str` | — |
| `name` | `str` | — |
| `location` | `str` | — |
| `quantity` | `float` | — |
| `unit` | `str` | — |
| `scaling_factor` | `float` | — |
| `classifications` | `dict[str, str]` | dict() |

### `TechnosphereExchange`

An exchange with another activity (input or output of an intermediate product).

Built from an `ExchangeWithUnit` envelope: outer fields like flowName/unitName
live next to an inner `exchange` object (the discriminated `Exchange` sum).

| Field | Type | Default |
|-------|------|---------|
| `flow_name` | `str` | — |
| `flow_category` | `str` | — |
| `amount` | `float` | — |
| `unit` | `str` | — |
| `is_input` | `bool` | — |
| `is_reference` | `bool` | — |
| `target_activity` | `str \| None` | — |
| `target_location` | `str \| None` | — |
| `target_process_id` | `str \| None` | — |
| `comment` | `str \| None` | None |
| `is_biosphere` | `bool` | False |

## Functions

### `compare_activities(client: Client, pid_left: str, pid_right: str, *, scope: str = 'direct', group_by: str = 'flow_id', is_input: bool | None = True, **aggregate_kwargs) -> ActivityDiff`

Diff two activities by flow_id (default) at the requested scope.

Returns three lists:
- ``matched``: flows present in both activities (with left, right, delta).
- ``left_only``: flows present only in the left activity.
- ``right_only``: flows present only in the right activity.

Default ``is_input=True`` restricts the comparison to inputs, which is the
common case for "what does this variant consume differently?". Pass
``is_input=None`` to include outputs as well.

### `download(version: Optional[str] = None, repo: str = 'ccomb/volca', *, force: bool = False) -> Installed`

Download the volca binary + data bundle for the current platform.

Idempotent: if both artefacts are already extracted under the install
root and ``force=False``, returns immediately without network.

Args:
    version: GH Release tag (``v0.7.0``); ``None`` resolves the latest.
    repo: GitHub repo slug. Default ``ccomb/volca``.
    force: Re-download even if the install root looks complete.

Returns:
    :class:`Installed` with the resolved paths and versions.

## Type aliases

### `Exchange`

Type alias: `Union[TechnosphereExchange, BiosphereExchange]`.


<!-- END: api-reference -->

## See also

- Full guide and tutorials: <https://volca.run/docs/guides/pyvolca/>
- VoLCA engine: <https://github.com/ccomb/volca>
- Examples folder: [`examples/`](examples/)

## License

Apache-2.0
