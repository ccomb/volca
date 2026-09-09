# pyvolca

Python client for [VoLCA](https://github.com/ccomb/volca), the Life Cycle Assessment engine over Agribalyse and ecoinvent.

> **Full guide and tutorials**: <https://www.volca.run/docs/python/>  
> **Issues / source**: <https://github.com/ccomb/volca>  
> **Changelog**: <https://github.com/ccomb/volca/blob/main/pyvolca/CHANGELOG.md>

## Install

```bash
pip install pyvolca
```

Requires Python ≥ 3.10 and a running VoLCA engine. Use `Server` (below) to run one as a child process, or point `Client` at any reachable instance.

## Compatibility

pyvolca speaks a range of revisions of the engine's JSON wire format; the engine advertises its revision as `wireVersion` on `/api/v1/version`. pyvolca checks it the first time it talks to the engine: too old fails with a clear error, newer than this client knows warns, and a capability that needs a newer wire than the engine speaks refuses to run instead of letting the engine misread the request. pyvolca and engine version numbers move independently: `wireVersion` carries compatibility, not the version numbers.

The other direction is a promise about pyvolca's own names. A name this client publishes does not vanish under a working script: when one is renamed, the old spelling keeps working and warns, saying what replaced it and that it takes the same arguments. A retired name is dropped at a major version, never in the release that introduces its replacement, so upgrading tells you what to edit rather than breaking on the line that used it. Python shows a `DeprecationWarning` only when it is raised from your top-level script, so run with `-W default::DeprecationWarning` to see the ones raised from your own modules and packages too.

<!-- BEGIN: compatibility -->

_Generated from `volca._compat`: run `python scripts/gen_api_md.py` to regenerate._

This build of **pyvolca 0.11.0** speaks wire formats **2 to 23** and requires a VoLCA engine **≥ v0.9.1**; a capability gated on a newer wire than the engine speaks refuses to run with a clear error. A name this build has retired keeps working until pyvolca **1.0**.

<!-- END: compatibility -->

## First choose: connect to an existing server, or start one locally

`pyvolca` is only the Python client library. It does not contain the VoLCA databases and it does not install the VoLCA engine binary.

Most users should start with one of these two modes:

- **You already have access to a VoLCA server** (for example a hosted server prepared by someone else): use `Client` only. You do not need `volca.toml`, and you do not need to install the VoLCA server locally.
- **You want Python to start a local VoLCA engine process for you**: use `download()` once to fetch the VoLCA engine binary and reference data into the shared volca install dir (see [Where artefacts are installed](#where-artefacts-are-installed)), then use `Server` to start it from Python. `volca.toml` is still a normal file path passed to `Server(config=...)`; put it in your project directory, or pass an absolute path. Do not put it inside your virtualenv or inside `site-packages`.

For a hosted server, the minimal connection looks like this:

```python
# no-test: replace with your real hosted VoLCA server URL and credentials.
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
# no-test: downloads the engine and needs a real engine config/database.
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
# no-test: needs a real engine; the snippets below run against a mocked Client.
from volca import Client, Server

with Server(config="volca.toml") as srv:
    c = Client(base_url=srv.base_url, db="agribalyse-3.2", password=srv.password)
    plants = c.search_activities(name="wheat flour, at plant", limit=5)
    chain = c.get_supply_chain(plants[0].process_id, name="at farm")
    score = c.get_impacts(plants[0].process_id, method_id=c.list_methods()[0].id)
```

This example starts a local engine process from Python. `Server` reads `port` and `password` from the TOML config. The engine self-stops after `idle_timeout` seconds without traffic (default 5 min).

> Examples below assume `c` is a `Client` instance: construct it with the snippet above, or against an already-running server: `c = Client(base_url="http://localhost:8080", db="agribalyse-3.2", password="…")`.

## Discover what's available

> *Which databases are loaded? Which LCIA methods can I score against? What classification systems can I filter on?*

```python
for db in c.list_databases():
    print(f"  {db.name} [{db.status}]: {db.activity_count} activities")

for m in c.list_methods()[:5]:
    print(f"  {m.id}  {m.name} [{m.unit}]")
```

Other listings: `c.list_classifications()` returns the classification systems and their values for the current database; `c.list_presets()` returns named filter presets configured in the engine. Use `c.load_database(name)` / `c.unload_database(name)` to manage memory if a database isn't auto-loaded.

## Find an activity

> *Which activity in the database represents the product I want to assess?*

```python
plants = c.search_activities(name="wheat flour, at plant", page_size=5)
print(f"{len(plants)} matches; showing page 1 ({plants.page_size} items)")
for a in plants:
    print(f"{a.process_id}  {a.activity_name} → {a.product_name} ({a.location})")
```

`search_activities` returns a `SearchResults[Activity]`: a paginated wire envelope. Iterate it to walk every match across all pages (subsequent pages fetched on demand, then cached so re-iteration is free); `len(results)` is the server-reported total. Use `results.page(n, page_size=M)` for explicit page access, or pass `page=N` + `page_size=M` to jump straight to a page (both are required together; `page=` alone is rejected since the offset can't be derived without committing to a page size). Each `Activity` is a process, an `(activity, product)` pair, carrying `process_id`, `activity_name`, `location`, `product_name`, `product_amount`, `product_unit`. A process has no name of its own; compose a label from `activity_name` + `product_name`. Narrow the query with `geo="FR"`, `classification=`/`classification_value=` (ISIC/CPC), or set `exact=True` for an exact-name match. To search by flow name instead of activity name, use `c.search_flows(query=...)`: it answers with every kind of flow, and `kind="technosphere"`, `"biosphere"` or `"waste"` keeps one.

## Inspect an activity

> *What goes into making this product? What does it emit? What's its reference unit?*

```python
detail = c.get_activity(plants[0].process_id)
for ex in detail.technosphere_inputs:
    print(f"{ex.amount:.4g} {ex.unit} of {ex.flow_name} ← {ex.target_activity_name}")
```

`get_activity` returns a typed `ActivityDetail`. Use `.inputs` / `.outputs` / `.technosphere_inputs` to filter the exchanges; each entry is an `Exchange`: either a `TechnosphereExchange` (an input or output of an intermediate product) or a `BiosphereExchange` (resource extracted or pollutant emitted).

## Trace the upstream supply chain

> *What's the full upstream chain: every ingredient, recursively, down to the farm or mine?*

```python
chain = c.get_supply_chain(plants[0].process_id, name="at farm", limit=20)
print(f"{chain.filtered_activities} of {chain.total_activities} upstream activities match 'at farm'")
for entry in chain.entries[:5]:
    print(f"  {entry.quantity:.4g} {entry.unit} of {entry.activity_name} ({entry.location})")
```

For *"how exactly does this root reach a specific upstream supplier?"*, use `get_path_to(process_id, target=...)`, which returns a `PathResult` of ordered `PathStep`s root → target with cumulative quantities and step ratios.

## Find downstream consumers

> *Where is this supplier used? Which products depend on it?*

```python
result = c.get_consumers(plants[0].process_id, max_depth=2, page_size=10)
for cons in result.consumers:
    print(f"  depth={cons.depth}  {cons.activity_name} ({cons.location})")
```

Returns a `ConsumersResponse` whose `consumers` field is a `SearchResults[ConsumerResult]`, with the same paginated iterator semantics as `search_activities`. When `include_edges=True`, `result.edges` carries the technosphere edges so callers can reconstruct supplier→consumer paths without a second round trip. Pass `classification_filters=[...]` to restrict to a category.

## Compute the life-cycle inventory

> *What are the cumulative biosphere flows (CO₂, water, methane, …) per functional unit, before applying any characterization method?*

```python
inv = c.get_inventory(plants[0].process_id, limit=20)
for f in inv.flows[:5]:
    print(f"  {f.quantity:.4g} {f.unit_name}  {f.flow_name}")
print(f"  {inv.statistics.emission_quantity:.4g} emissions / "
      f"{inv.statistics.resource_quantity:.4g} resources")
# Substitutions are accepted: c.get_inventory(pid, substitutions=[...])
```

`InventoryResult` carries the typed `flows` list (one `InventoryFlow` per row) plus a `statistics` roll-up with per-direction totals and `top_categories`. The inventory is what every LCIA method runs on top of. If you only need *grouped* views (by name, location, classification, etc.), reach for `c.aggregate(scope="biosphere", group_by=...)` instead: same data, summarized.

## Compute environmental impacts (LCIA)

> *What's the carbon footprint of this product? Which emissions dominate the score?*

```python
score = c.get_impacts(plants[0].process_id, method_id="Climate change", top_flows=5)
print(f"{score.score:.4g} {score.unit}")
for c_flow in score.top_contributors:
    print(f"  {c_flow.share_pct:.1f}%  {c_flow.flow_name}")
```

`method_id` takes a method UUID or its name: a name is resolved against the loaded methods, which also settles which collection carries it. Pass `collection=` only to pin one when several are loaded.

`LCIAResult` carries the score, unit, optional `normalized_score` / `weighted_score` (in Pt), and the top contributing biosphere flows with their `share_pct`.

> *Compute every impact category in one go: climate, water, land use, …*

```python
batch = c.get_impacts_batch(plants[0].process_id)
for r in batch.results:
    print(f"  {r.category}: {r.score:.4g} {r.unit}")
if batch.single_score is not None:
    print(f"PEF single score: {batch.single_score:.4g} {batch.single_score_unit}")
```

There is no method to name here, so the collection has to come from somewhere: with one loaded it is that one, and with several the call refuses and names them, rather than scoring against a collection you did not pick. Pass `collection=` to say which.

`LCIABatchResult` also surfaces formula-based scoring sets (PEF, ECS…) via `scoring_results` and `scoring_indicators`, so you can render a per-indicator chart alongside the aggregate single score.

## Drill into what drives a single impact

> *I have a climate-change score. Which biosphere flows account for it? Which upstream activities?*

`get_impacts(...).top_contributors` already returns the top biosphere flows for a single LCIA call. For a deeper or differently-bounded view (and for the *activity* attribution view) use the standalone drill-down endpoints:

```python
flows = c.get_contributing_flows(
    plants[0].process_id,
    method_id="Climate change",
    limit=10,
)
for f in flows.top_flows:
    print(f"  {f.share_pct:.1f}%  {f.flow_name}")

acts = c.get_contributing_activities(
    plants[0].process_id,
    method_id="Climate change",
    limit=10,
)
for a in acts.activities:
    print(f"  {a.share_pct:.1f}%  {a.activity_name} ({a.location})")
```

`ContributingFlows.top_flows` and `ContributingActivities.activities` are typed lists; both carriers also expose `method`, `unit`, and `total_score`. Note: the engine doesn't report a total count for these endpoints, so neither result derives a `has_more` flag; pass a generous `limit` and inspect the `share_pct` totals if you need exhaustive coverage.

> *Which characterization factors does a method apply, and to which database flows?*

```python
char = c.get_characterization(method_id="Climate change", limit=20)
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

> *What if I used organic wheat instead of conventional? Recycled aluminium instead of virgin? All without reloading the database.*

The engine applies a Sherman–Morrison rank-1 update, so substitutions are fast regardless of database size. Works on `get_supply_chain`, `get_inventory`, and `get_impacts`.

```python
subs = [{
    "from": "old-supplier-pid",      # the activity to replace
    "to":   "new-supplier-pid",      # the replacement
    "consumer": "consumer-pid",      # the activity that directly uses the old supplier
}]
score = c.get_impacts(plants[0].process_id, method_id="Climate change", substitutions=subs)
```

Multiple substitutions chain in one call; the `consumer` field disambiguates *where* in the chain each swap applies.

## Handle errors

> *The activity doesn't exist, the engine is down, or the request is malformed: what do I catch?*

```python
from volca import VoLCAError

try:
    score = c.get_impacts("nonexistent-pid", method_id="Climate change")
except VoLCAError as e:
    print(f"  failed: {e}")
```

`VoLCAError.status_code` is the HTTP status when the engine returned one, and `body` the raw response body. Both are empty when the client refuses on its own, which is what happens to a method name nothing carries or a collection you had to choose: print the exception itself and you get either explanation.

## Switch databases

> *I want to run the same workflow against ecoinvent instead of Agribalyse, without rebuilding the client.*

```python
ei = c.use("ecoinvent-3.10")
ei_results = ei.search_activities(name="electricity, high voltage")
```

`Client.use(db_name)` returns a new `Client` targeting a different database while sharing the HTTP session and dispatch table, with no spec re-fetch.

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

### `AggregateOp`

How values are reduced within a bucket.

``SUM_QUANTITY``: sum of quantities (default). ``COUNT``: number of
matching entries. ``SHARE``: each bucket's percentage of the filtered
total (0..100).

### `AggregateScope`

What the ``/aggregate`` primitive groups over.

``DIRECT``: direct exchanges of the activity. ``SUPPLY_CHAIN``: the
upstream activities reachable via cumulative flow. ``BIOSPHERE``: only
biosphere flows in the supply chain. ``CONSUMPTION``: every scaled
technosphere edge (who consumes what, in scaled units); the scope that
answers "total X consumed upstream" without double counting, via
``filter_consumer_not``.

### `BioDirection`

Direction of a biosphere exchange.

``RESOURCE``: extraction from the environment (input).
``EMISSION``: release to the environment (output).

Lookup is case-insensitive (``BioDirection("emission")`` works): the
engine reads the wire value that way, so the client should not be
stricter than the server it speaks for.

### `ClaimKind`

How a source file designates the supplier of an exchange.

``PRODUCT``: the product row itself, its flow and where it has one its
location - SimaPro, ILCD, and a Brightway row naming no activity.
``ID``: the supplier activity's own identifier, as EcoSpold 2 writes it.
``NAME``: the supplier activity by name, from a Brightway ``name`` column.
``DATASET_NUMBER``: the number the supplier dataset is published under,
as EcoSpold 1 writes it.

### `Client`

HTTP client for the VoLCA HTTP API.

Usage::

    c = Client(db="agribalyse-3.2", password="1234")
    plants = c.search_activities(name="at plant")
    chain = c.get_supply_chain(plants[0].process_id, name="at farm")

Substitutions can be passed to ``get_supply_chain``, ``get_inventory``,
and ``get_impacts`` to compute results with a different upstream
supplier, fast::

    subs = [{"from": old_pid, "to": new_pid, "consumer": consumer_pid}]
    result = c.get_impacts(pid, method_id=mid, substitutions=subs)

**Constructor**: `Client(base_url: str = 'http://localhost:8080', db: str = '', password: str = '')`

#### Methods

##### `Client.add_dependency(dep_name: str, db_name: str | None = None) -> dict`

Declare ``dep_name`` as a dependency of the target database.

Returns the engine's ``DatabaseSetupInfo`` dict describing the updated
dependency topology.

##### `Client.aggregate(process_id: str, scope: AggregateScope | str, *, is_input: bool | None = None, max_depth: int | None = None, filter_name: str | None = None, filter_name_not: list[str] | str | None = None, filter_unit: str | None = None, preset: str | None = None, filter_classification: list[ClassificationFilter] | None = None, filter_target_name: str | None = None, filter_consumer: str | None = None, filter_consumer_not: list[str] | str | None = None, filter_is_reference: bool | None = None, group_by: str | None = None, aggregate: AggregateOp | str | None = None) -> AggregateResult`

SQL-group-by aggregation over direct exchanges, supply chain, or biosphere flows.

Args:
    scope: `AggregateScope` member (``DIRECT`` / ``SUPPLY_CHAIN``
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
    aggregate: `AggregateOp` member or wire string
        (``"sum_quantity"`` by default, ``"count"``, or ``"share"``).

##### `Client.call(operation_id: str, **kwargs) -> Any`

Escape hatch: call any OpenAPI operation by operationId.

Returns the raw JSON (no dataclass wrapping). Use this for
operations that don't have an ergonomic wrapper yet, or for new
endpoints added after the installed pyvolca was released.

##### `Client.compute_sensitivity(process_id: str, method_id: str, perturbations: list[dict], *, collection: str | None = None) -> SensitivityResult`

How much one impact score moves when technosphere links are perturbed.

Each perturbation is a dict
``{"consumer": pid, "supplier": pid, "delta": -0.05, "label"?: str}``:
``delta`` is *relative* (the coefficient becomes ``a * (1 + delta)``,
so ``-1.0`` removes the link). Returns the ``baseline`` `LCIAResult`
plus one `PerturbedResult` per perturbation, each carrying
either the perturbed impact and its delta, or an ``error`` string when
that perturbation could not be resolved. ``method_id`` takes a method
name as well as a UUID, and ``collection`` is read off the resolved
method unless you pin it.

##### `Client.copy_database(new_name: str, db_name: str | None = None) -> dict`

Copy a loaded database in memory under a new name.

``new_name`` is a path segment; the source defaults to ``self.db``.
Returns the engine's ``ActivateResponse`` dict
(``{"success", "message", "database"?}``). Raises VoLCAError if the
engine reports ``success=false``.

##### `Client.count_search_matches(query: str) -> SearchCounts`

How many processes, products and flows a query matches.

One call rather than three searches, for a search box that labels its
tabs with counts. The three are disjoint and together cover the
database.

Args:
    query: The search term. Required: an empty box has nothing to
        count, and the engine refuses a blank one rather than
        answering three zeros, which would read as "this database has
        nothing".

##### `Client.create_activities(activities: list[ActivityInput] | ActivityInput, db_name: str | None = None) -> dict`

Write new activities into a database that can hold them.

Each activity's ``process_id`` is minted by the engine from its name,
its location, and its product name and unit (you do not choose it),
and comes back in ``written``. Writing the same activity twice is therefore
a conflict, not a second row; use `replace_activity` to correct
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

##### `Client.delete_activities(*, name: str = '', location: str = '', product: str = '', classifications: list[dict | tuple] | None = None, exact: bool = False, keep: list[str] | None = None, extra: list[str] | None = None, ids: list[str] | None = None, db_name: str | None = None) -> dict`

Delete activities selected by filter, or exactly the ``ids`` list.

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

##### `Client.delete_database(db_name: str | None = None) -> dict`

Delete a database entirely: unload it and remove its uploaded files.

Returns the ``ActivateResponse`` dict; raises VoLCAError on
``success=false``.

##### `Client.delete_method_collection(name: str) -> dict`

Delete a method collection: unload it and remove its staged file.

##### `Client.delete_reference_data(kind: RefDataKind, name: str) -> dict`

Delete a reference-data set of ``kind`` and remove its staged file.

##### `Client.derive_database(new_name: str, allocation: str, db_name: str | None = None) -> dict`

Read a database's sources again under another allocation key.

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

##### `Client.download_flow_synonyms(name: str) -> bytes`

Download a flow-synonyms set as its raw CSV bytes.

Raises VoLCAError on an HTTP error (e.g. the set does not exist).

##### `Client.edit_exchanges(process_id: str, *, remove: Sequence[ExchangeSelector] = (), set_amounts: Sequence[SetAmount] = (), add_inputs: Sequence[TechInput] = (), add_biosphere: Sequence[BioExchange] = (), add_waste_outputs: Sequence[WasteOutput] = (), db_name: str | None = None) -> dict`

Change what one activity consumes and emits, keeping the activity.

This reaches what `replace_activity` cannot: an activity that came
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

##### `Client.ensure_database(source: str | Path | bytes, name: str | None = None) -> str`

Idempotently make the archive at ``source`` a loaded database.

The one-call form of the upload lifecycle: match by display name
(default: the file's stem), upload only when absent, finalize the
staged copy, load if unloaded. Returns the slug every later call
targets: run it at the top of a script and it converges on the same
loaded database every time instead of re-uploading. A match that is
already loaded, even partially linked, is left untouched.

A staged copy that is not ready to finalize raises VoLCAError naming
the blocker (missing suppliers, no activities parsed); fix it with
`add_dependency` or `set_data_path`, then
`finalize_database`. The gate also holds on re-runs: an upload
left staged by an earlier failed run goes through the same readiness
check instead of being loaded half-linked.

##### `Client.explain_cf(method_id: str, flow_id: str) -> ExplainCFResult`

Explain why one flow scores with the characterization factor it does.

``result.explanation`` is a list of sentences written by the engine:
show them as they are. The structured fields say the same thing in a
form you can compare or filter on, and ``result.steps_tried`` lists the
rungs the cascade walked before the one that answered.

##### `Client.export_database(fmt: str, db_name: str | None = None) -> bytes`

Export a loaded database, returning the serialized bytes.

``fmt`` is one of ``simapro|ecospold1|ecospold2|ilcd|brightway``,
validated client-side; an unknown value raises VoLCAError before any
request. Single-file formats carry their bytes directly; EcoSpold 2 /
ILCD multi-file trees come back zipped.

The engine streams the payload as raw bytes. Best-effort approximation
warnings arrive in the ``X-Volca-Export-Warnings`` response header
(percent-encoded, newline-joined) and are surfaced through
`warnings`. Raises VoLCAError on an HTTP error.

##### `Client.export_method_collection(name: str, fmt: str = 'simapro') -> bytes`

Export a loaded method collection, returning the serialized bytes.

``fmt`` names the target format: ``simapro`` (SimaPro method CSV),
``csv`` (columnar CSV, one column per impact category, the
spreadsheet view), ``openlca`` (a zip of openLCA JSON-LD impact
categories), or ``ilcd`` (a zip of an ILCD LCIA-method package,
one method dataset per impact category plus its flow datasets).
Projection warnings (anything the format cannot
carry faithfully) arrive in the ``X-Volca-Export-Warnings`` response
header and are surfaced through `warnings`. Raises VoLCAError
on an HTTP error, including a collection that is not loaded.

##### `Client.export_to_file(fmt: str, out_path: str, db_name: str | None = None) -> None`

Export a database (see `export_database`) and write it to a file.

##### `Client.finalize_database(db_name: str | None = None) -> dict`

Build matrices for a staged database and load it (``ActivateResponse``).

Call after dependencies resolve (`get_setup` reports
``isReady``). Raises VoLCAError if the engine reports ``success=false``
(e.g. unresolved suppliers).

##### `Client.get_activity(process_id: str) -> ActivityDetail`

Fetch an activity's full detail.

Returns a typed ActivityDetail. Use ``act.inputs`` / ``act.outputs`` /
``act.technosphere_inputs`` to filter exchanges instead of walking
``act.exchanges`` directly.

##### `Client.get_characterization(method_id: str, *, flow: str | None = None, limit: int | None = None) -> CharacterizationResult`

Look up characterization factors for a method matched to database flows.

Returns a `CharacterizationResult` carrying ``matches`` (total
rows the filter selected) and ``shown`` (rows actually returned under
``limit``). Check ``result.has_more`` to detect truncation.

##### `Client.get_collection_coverage(collection: str, db_name: str | None = None) -> CollectionCoverage`

How much of a database a whole method collection characterizes.

Counts the distinct emission and resource flows at least one of the
collection's methods resolves a factor for, with the same lookup
scoring uses. Distinct across methods: their factors overlap, so the
per-method figures from `get_mapping_status` do not add up to
this number.

##### `Client.get_consumers(process_id: str, *, name: str | None = None, location: str | None = None, product: str | None = None, preset: str | None = None, classification_filters: list[ClassificationFilter] | None = None, page: int | None = None, page_size: int | None = None, limit: int | None = None, offset: int | None = None, max_depth: int | None = None, sort: str | None = None, order: str | None = None, include_edges: bool = False) -> ConsumersResponse`

Find all activities that transitively consume this supplier.

Args:
    max_depth: Max hops from supplier. 1 = direct consumers only.
    classification_filters: ClassificationFilter entries restricting
        the results. Multiple filters are AND-combined by the server.
        Mode is `MatchMode.EXACT` or `MatchMode.CONTAINS`.
    sort: Sort key: ``"name"``, ``"location"``, ``"product"``,
        ``"amount"``, or ``"unit"``. Default orders by depth.
    order: ``"desc"`` to reverse; ascending otherwise.
    include_edges: When True, the response carries every technosphere
        edge whose endpoints are both reachable from the supplier.
        Callers can walk these to reconstruct supplier→consumer paths
        without a second ``get_path_to`` round-trip.

Returns a `ConsumersResponse` whose ``consumers`` attribute is
a `SearchResults[ConsumerResult]` (iterate it to walk every
consumer across all pages) and whose ``edges`` attribute carries
the traversal subgraph (empty by default).

##### `Client.get_contributing_activities(process_id: str, method_id: str, *, collection: str | None = None, limit: int | None = None) -> ContributingActivities`

Which upstream activities drive a given impact category.

Same engine-side limitation as `get_contributing_flows`: no
total exposed, so ``has_more`` cannot be derived. Inspect
``share_pct`` totals to gauge coverage. ``method_id`` takes a method
name as well as a UUID, and ``collection`` is read off the resolved
method unless you pin it.

##### `Client.get_contributing_flows(process_id: str, method_id: str, *, collection: str | None = None, limit: int | None = None) -> ContributingFlows`

Which elementary flows drive a given impact category.

Returns a `ContributingFlows`. Caveat: the engine does not
report the total flow count, so pyvolca cannot derive ``has_more``
from the response. Pass a generous ``limit`` if you need exhaustive
coverage and inspect ``share_pct`` totals. ``method_id`` takes a method
name as well as a UUID, and ``collection`` is read off the resolved
method unless you pin it.

##### `Client.get_flow(flow_id: str, db_name: str | None = None) -> FlowDetail`

Detail of one flow: its record, unit, and how many exchanges use it.

##### `Client.get_flow_activities(flow_id: str, db_name: str | None = None, *, role: str | None = None) -> list[Activity]`

Activities on one side of a flow, or both.

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

##### `Client.get_flow_mapping(method_id: str) -> FlowMapping`

Get the characterization-factor-to-database-flow mapping coverage.

`FlowMapping.coverage_pct` summarises how many of the DB's
biosphere flows the method has a CF for; ``flows`` is the per-flow
breakdown including unmatched rows (``cf_value=None``).

##### `Client.get_impacts(process_id: str, method_id: str, *, collection: str | None = None, top_flows: int | None = None, substitutions: list[SubstitutionLike] | None = None) -> LCIAResult`

Compute the LCIA score for a single impact category on an activity.

Use `get_impacts_batch` to retrieve every category in a method
collection at once (and any configured scoring sets).

Args:
    method_id: A method UUID, or the method's name ("Water use");
        a name is resolved against the engine's loaded methods.
    collection: Method collection name. Left out, it is read off the
        resolved method, so the caller needs to know only the method.
    top_flows: Max top contributing flows to return (default 5).

##### `Client.get_impacts_batch(process_id: str, *, collection: str | None = None, substitutions: list[SubstitutionLike] | None = None, exclude_long_term: bool | None = None) -> LCIABatchResult`

Compute LCIA for every impact category in a collection, in one call.

The response carries the per-method `LCIAResult` list plus any
formula-based scoring sets declared in the engine config (PEF, ECS…).
``scoring_indicators`` gives the per-variable breakdown of each
scoring set, pre-multiplied by the set's ``displayMultiplier``.
``exclude_long_term`` drops long-term emissions before scoring, the
same switch `score_activities` carries.

Left without a ``collection``, the call runs against the only loaded
one, and refuses when several are loaded rather than picking one.

Uses a direct HTTP call: the batch endpoint has no operationId in the
OpenAPI spec (the dispatcher primary is the single-method variant), so
this wrapper bypasses ``_call`` and builds the URL itself.

##### `Client.get_inputs(process_id: str) -> list[Exchange]`

Return the input exchanges of an activity (richer metadata than ``get_activity``).

Uses a direct HTTP call because ``/inputs`` has no operationId
(it's a non-Resources auxiliary endpoint).

##### `Client.get_inventory(process_id: str, *, flow: str | None = None, limit: int | None = None, substitutions: list[SubstitutionLike] | None = None) -> InventoryResult`

Compute the life-cycle inventory (cumulative biosphere flows) for an activity.

Returns an `InventoryResult` with the per-elementary-flow
totals scaled to one functional unit of the activity's reference
product. Use `get_impacts` to apply a characterization method
to the inventory; use `aggregate` with ``scope="biosphere"``
for grouped views.

Args:
    flow: Substring filter on flow name.
    limit: Cap on returned flow rows. (Server returns full inventory
        otherwise; the engine doesn't paginate this endpoint.)
    substitutions: Upstream supplier swaps; see `get_supply_chain`.

##### `Client.get_mapping_status(method_id: str, db_name: str | None = None) -> MappingStatus`

How well a method's factors map onto a database's biosphere flows.

Reports the cascade breakdown (matched by UUID / CAS / name / synonym),
the ``coverage`` fraction, and the ``unmapped_flows`` still without a CF.

##### `Client.get_method(method_id: str) -> MethodDetail`

Detail of one LCIA method: unit, category, methodology, factor count.

##### `Client.get_method_factors(method_id: str) -> list[MethodFactor]`

The characterization factors of a method (flow, direction, value).

##### `Client.get_outputs(process_id: str) -> list[Exchange]`

Return the output exchanges of an activity. See `get_inputs` for notes.

##### `Client.get_path_to(process_id: str, target: str) -> PathResult`

Find the shortest upstream path from process to first activity whose name matches target.

Returns a PathResult whose path is ordered root → target. Each step
includes cumulative_quantity, scaling_factor, and (except the root)
local_step_ratio.

##### `Client.get_setup(db_name: str | None = None) -> dict`

Setup status of a staged or loaded database (``DatabaseSetupInfo``).

Key fields: ``isReady`` (can it be finalized/loaded), ``missingSuppliers``
and ``unresolvedLinks`` (unmet cross-database links), ``dependencies``
(declared deps), ``dataPath`` / ``availablePaths`` (the selected data
file and the alternatives, see `set_data_path`), ``completeness``,
and ``supplierAmbiguities``: the inputs several activities of one
dependency answered equally well, each naming the product asked for,
the activity linked to and how many tied, so the supplier meant can be
named rather than guessed (wire revision 23).

##### `Client.get_stats()`

Return the engine's runtime statistics (memory use, loaded sizes).

Keys are already snake_case on the wire, so this returns the raw dict.

##### `Client.get_supply_chain(process_id: str, *, name: str | None = None, location: str | None = None, limit: int | None = None, min_quantity: float | None = None, max_depth: int | None = None, preset: str | None = None, classification_filters: list[ClassificationFilter] | None = None, sort: str | None = None, order: str | None = None, substitutions: list[SubstitutionLike] | None = None, include_edges: bool | None = None) -> SupplyChain`

Get the flat supply chain of an activity.

Returns a `SupplyChain`. Check ``result.has_more`` to detect
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
        suppliers. Accepts `Substitution` (preferred) or the
        legacy ``{"from", "to", "consumer"}`` dict form; ``consumer``
        is optional: omit it for a global swap.

##### `Client.get_synonym_groups(name: str) -> list[list[str]]`

Return the synonym groups of a flow-synonyms set (lists of aliases).

##### `Client.get_tree(process_id: str) -> dict`

Fetch the recursive activity tree used by the analysis SPA.

``/tree`` has no operationId in the OpenAPI spec; it's kept for the
SPA's lazy-expanding graph widget and intentionally not exposed as
a Resource. Included here as a direct HTTP call for scripts that
need the same shape.

##### `Client.get_version()`

Return server build metadata: version, git hash/tag, build target.

Uses a direct HTTP call: ``/api/v1/version`` has no operationId
since it predates the Resources ADT.

##### `Client.list_classifications()`

List classification systems and their values for the current database.

``ClassificationSystem.activity_count`` tells how widely each system
is populated, useful for picking a filter dimension with enough
signal.

##### `Client.list_databases()`

List every database declared in the engine config.

The typed entries carry ``depends_on``, so callers can derive
cross-DB dependency sets from declared topology rather than
hardcoding allowlists.

##### `Client.list_method_collections()`

List every method collection the engine knows (loaded or staged).

Each entry carries ``name``, ``displayName``, ``status``,
``methodCount`` and ``format``.

##### `Client.list_methods()`

List every LCIA method available in the engine.

Each `Method` carries ``id``, ``name``, ``category``, ``unit``,
``factor_count``, and the parent ``collection``. Every ``method_id``
argument takes either, so this list is for browsing, not for looking
up an id before a call.

##### `Client.list_presets()`

List classification presets configured in this instance.

Each `Preset` carries its ``filters`` (list of
`PresetFilter` triples). Apply by passing ``preset=p.name``
to filtering endpoints.

##### `Client.list_reference_data(kind: RefDataKind) -> list[dict]`

List reference-data sets of one ``kind`` (loaded, staged, or built-in).

Each entry carries ``name``, ``displayName``, ``status``, ``isAuto``
(a built-in bundled set) and ``entryCount``.

##### `Client.load_database(db_name: str) -> dict`

Load a database into memory so it answers queries.

Declared dependencies are loaded first; has no effect if the
database is already loaded.

##### `Client.load_method_collection(name: str) -> dict`

Load a staged method collection so its methods become available.

##### `Client.load_reference_data(kind: RefDataKind, name: str) -> dict`

Load a staged reference-data set of ``kind`` into memory.

##### `Client.refresh_stubs()`

Fetch the OpenAPI spec from the server and refresh the dispatch table.

Also regenerates the `.pyi` type stubs in the installed pyvolca
package directory so IDE autocomplete reflects the current engine.
Useful when the engine is upgraded without reinstalling pyvolca.

This is the explicit "the engine was upgraded" path, the likeliest
place to meet a wire *change*, so it forgets the cached wire and
re-runs the gate against the live engine before fetching a spec
pyvolca can't decode. Without the reset, a client that first met an
older engine would keep refusing wire-gated capabilities after an
in-place upgrade.

##### `Client.relink(dep_db: str, mapping_csv: str, db_name: str | None = None) -> dict`

Re-link a database against a dependency using a name→name alias CSV.

``mapping_csv`` is the CSV *text* (header row + source/target columns),
sent inline so the engine needs no filesystem access. Returns the
``RelinkResponse`` dict (``{"dbName", "unresolvedBefore",
"unresolvedAfter", "crossDBLinks", "dependsOn"}``).

##### `Client.relink_from_file(dep_db: str, mapping_path: str, db_name: str | None = None) -> dict`

Read a mapping CSV file and call `relink` with its text.

##### `Client.remove_dependency(dep_name: str, db_name: str | None = None) -> dict`

Remove ``dep_name`` from the target database's dependencies.

Returns the updated ``DatabaseSetupInfo`` dict.

##### `Client.replace_activity(process_id: str, activity: ActivityInput, db_name: str | None = None) -> dict`

Rewrite one activity the database already holds, keeping its identity.

``process_id`` must be the identity ``activity`` mints to; that is,
the name, the location, and the product name and unit must be the ones
the row already has, spelling and case included. Change any of those and you are describing a different
activity, which the engine refuses rather than writing to a second row;
create that one and delete the old one instead.

Returns the same shape as `create_activities`.

##### `Client.resolve_activities(names: Iterable[str], *, by: Literal['name', 'product'] = 'name', geo: str | None = None, exact: bool = True, limit: int = 5, workers: int = 8) -> dict[str, list[Activity]]`

Resolve a batch of names to their matching activities, concurrently.

One `search_activities` call per unique name, fanned out over
``workers`` threads on the client's HTTP session. This replaces the
two patterns scripts keep hand-rolling: downloading the whole
database to build a name→process_id dict, and per-name thread pools.

The result maps every input name to its matches: the mapping is
total, so misses are visible, never silently dropped:

* ``[]``: no match; the name does not resolve.
* one `Activity`: unambiguous; ``matches[0].process_id``.
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

##### `Client.score_activities(process_ids: list[str], *, collection: str | None = None, top_flows: int | None = None, exclude_long_term: bool | None = None) -> BatchScores`

Score many processes in one call (every category of a collection each).

Returns a `BatchScores`: ``results`` holds one
`ScoredActivity` per process the engine could compute, while
``not_found`` / ``invalid`` list the ids it could not resolve; inspect
them, a partial result is not an error. ``top_flows`` caps the top
contributors per category; ``exclude_long_term`` drops long-term
emissions from the totals. Left without a ``collection``, the call runs
against the only loaded one, and refuses when several are loaded rather
than picking one.

##### `Client.search_activities(name: str | None = None, *, geo: str | None = None, product: str | None = None, preset: str | None = None, classification: str | None = None, classification_value: str | None = None, classification_match: MatchModeLike | None = None, page: int | None = None, page_size: int | None = None, limit: int | None = None, offset: int | None = None, sort: str | None = None, order: str | None = None, exact: bool = False) -> SearchResults[Activity]`

Search activities in the current database.

All filters are AND-combined and case-insensitive. ``name`` and
``product`` match by substring unless ``exact=True``.

Returns a paginated `SearchResults`: iterate it to walk
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
        `MatchMode.CONTAINS` (default, substring) or
        `MatchMode.EXACT` (case-insensitive equality). Ignored
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
    `SearchResults[Activity]`, iterable across all pages.

##### `Client.search_flows(query: str | None = None, *, kind: str | None = None, page: int | None = None, page_size: int | None = None, limit: int | None = None, offset: int | None = None, sort: str | None = None, order: str | None = None) -> SearchResults[Flow]`

Search flows in the current database.

Three kinds of flow answer, and `Flow.kind` says which each one
is: a technosphere product one activity makes and another consumes, a
biosphere substance exchanged with nature, or a waste.

Returns a paginated `SearchResults[Flow]`: iterate to walk
every match across all pages, or use ``.page(n)`` for explicit
access. See `search_activities` for the pagination contract.

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
        `count_search_matches` reports as ``flows``. One kind
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

##### `Client.set_data_path(path: str, db_name: str | None = None) -> dict`

Choose which data file a staged multi-file archive should use.

``path`` must be one of the ``availablePaths`` reported by
`get_setup`, relative to the upload directory. Returns the
updated ``DatabaseSetupInfo`` dict.

##### `Client.unload_database(db_name: str) -> dict`

Unload a database from memory to free RAM. The disk copy is kept.

Refused if another loaded database still depends on it.

##### `Client.unload_method_collection(name: str) -> dict`

Unload a method collection from memory (the staged file is kept).

##### `Client.unload_reference_data(kind: RefDataKind, name: str) -> dict`

Unload a reference-data set of ``kind`` from memory.

##### `Client.upload_database(source: str | Path | bytes, name: str, *, description: str | None = None) -> dict`

Upload a database archive; stage it under a generated slug.

``source`` is a path to a ZIP / CSV / XLSX archive (or its raw
``bytes``); ``name`` is the display name. The engine auto-detects the
format (EcoSpold 1/2, SimaPro CSV, ILCD, OpenLCA JSON-LD, Brightway
Excel) and stages the database without loading it.

Returns the ``UploadResponse`` dict
(``{"success", "message", "slug", "format"}``); ``slug`` is the name
every later call targets. Then inspect `get_setup`, wire missing
dependencies with `add_dependency`, and call
`finalize_database` to build matrices and load it.

Raises VoLCAError on any rejection (uploads disabled on the plan, size
cap exceeded, unreadable archive); the engine reports these in-band
with HTTP 200 and ``success=false``.

##### `Client.upload_method_collection(source: str | Path | bytes, name: str, *, description: str | None = None) -> dict`

Upload an ILCD method file as a staged method collection.

``source`` is a path to the method archive (or its raw ``bytes``).
Same streamed-body + query-param shape as `upload_database`;
returns the ``UploadResponse`` dict and raises VoLCAError on rejection.

##### `Client.upload_reference_data(kind: RefDataKind, source: str | Path | bytes, name: str, *, description: str | None = None) -> dict`

Upload a reference-data CSV of ``kind`` as a staged set.

``source`` is a path to the CSV (or its raw ``bytes``). Same
streamed-body + query-param shape as `upload_database`.

##### `Client.use(db_name: str) -> 'Client'`

Return a new client targeting a different database.

Shares the underlying HTTP session, dispatch table, and any other
Client-level state with the original; only ``db`` is overridden.
New fields added to `Client.__init__` propagate automatically
(no manual mirror to keep in sync).

### `DatabaseStatus`

Lifecycle state of a database in the engine.

``UNLOADED``: declared in the engine config but not yet loaded.
``PARTIALLY_LINKED``: loaded, but some cross-DB flow references could
not be resolved against currently-loaded dependencies.
``LOADED``: loaded and fully linked.

Inherits from `str`, so ``dataclasses.asdict(db)["status"]``
serialises as the bare wire string.

### `MatchMode`

How a `ClassificationFilter` value is compared against the entry.

``EXACT``: case-insensitive equality. ``CONTAINS``: case-insensitive
substring. Inherits from `str` so ``json.dumps(MatchMode.EXACT)``
and ``dataclasses.asdict(filter)["mode"]`` both serialise as the bare
string ``"exact"`` / ``"contains"``.

### `Server`

Manages the VoLCA server process.

Usage::

    with Server(config="volca.toml") as srv:
        client = Client(base_url=srv.base_url, db="agribalyse-3.2", password=srv.password)
        activities = client.search_activities(name="at plant")

**Constructor**: `Server(config: str | None = 'volca.toml', port: Union[int, Literal['auto']] = 0, binary: str = 'volca')`

#### Properties

##### `base_url`

``http://localhost:<port>``, pass to `Client(base_url=…)`.

Always loopback: the managed server only listens locally.

#### Methods

##### `Server.is_alive()`

Health check: GET /api/v1/db, return True if 200.

##### `Server.start(idle_timeout: int = 300, wait_timeout: int = 120) -> None`

Spawn the engine process if it is not already serving, and wait until ready.

Args:
    idle_timeout: Seconds without use before the engine shuts itself
        down. Default 5 min. An API request or a matrix solve counts
        as use; an MCP client merely staying connected does not.
    wait_timeout: How long to poll for the server to become healthy
        before raising `TimeoutError`.

No-op if a healthy server is already reachable on ``base_url``.

##### `Server.stop()`

Stop the server via shutdown endpoint, then terminate process.

### `TechRole`

Role a technosphere exchange plays within its host activity.

``REFERENCE_PRODUCT``: the activity's reference output product.
``COPRODUCT``: a product output the source left unallocated; an activity
still carrying one is refused a score (see the ``unallocated`` check of
the quality report).
``AVOIDED_PRODUCT``: a substitution, the product the activity displaces;
a credit on that product's producer (wire revision 14).
``REFERENCE_INPUT``: the reference input (in waste-treatment activities).
``INPUT``: any other technosphere input.

### `WasteRole`

What a waste line does within its activity.

``TREATS_WASTE``: an input, so this activity is the one treating it.
``SENT_TO_TREATMENT``: an output whose treatment was found.
``FINAL_WASTE_FLOW``: an output naming no treatment, so nothing treats it.
``TREATMENT_NOT_LOADED``: an output naming a treatment no loaded database
ships, so its burden is missing rather than accounted for.

The last two both arrive with no target, which is why the role is stated
rather than worked out from the target fields.

## Exceptions

### `DownloadError`

Raised when the download or verification fails.

### `VoLCAError`

Error from the VoLCA API.

**Constructor**: `VoLCAError(message: str, status_code: int | None = None, body: str = '')`

## Data types

### `Activity`

One activity in a database: the row returned by /activities search.

``process_id`` is the engine's canonical address (``activityUUID_productUUID``)
and is what you pass to every detail endpoint (`Client.get_activity`,
`Client.get_supply_chain`, `Client.get_impacts`, …).
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

| Field | Type | Default |
|-------|------|---------|
| `process_id` | `str` | _required_ |
| `activity_name` | `str` | _required_ |
| `location` | `str` | _required_ |
| `product_name` | `str` | _required_ |
| `product_amount` | `float` | _required_ |
| `product_unit` | `str` | _required_ |
| `allocation_percent` | `float \| None` | None |
| `allocation_formula` | `str \| None` | None |
| `mass_allocation_percent` | `float \| None` | None |
| `block` | `str \| None` | None |
| `block_products` | `int \| None` | None |

### `ActivityContribution`

One upstream activity's contribution to an LCIA score.

Returned in `ContributingActivities.activities`. ``share_pct`` is
the percentage of the total impact this activity contributes (0..100).

| Field | Type | Default |
|-------|------|---------|
| `process_id` | `str` | _required_ |
| `activity_name` | `str` | _required_ |
| `product_name` | `str` | _required_ |
| `location` | `str` | _required_ |
| `contribution` | `float` | _required_ |
| `share_pct` | `float` | _required_ |

### `ActivityDetail`

Typed wrapper around the JSON returned by GET /activity/{pid}.

Use the .inputs / .outputs / .technosphere_inputs convenience properties
instead of walking the raw exchanges list.

``native_id`` is the identifier the source file gave the dataset block this
process was read from - SimaPro's ``Process identifier``, the number an
EcoSpold 1 dataset is published under - which is what a reader copies to
find the dataset back in the file it came from. It is ``None`` where the
format has none, where the format names a dataset by the UUID
``process_id`` already spells, and against an engine older than wire
revision 22.

| Field | Type | Default |
|-------|------|---------|
| `process_id` | `str` | _required_ |
| `activity_name` | `str` | _required_ |
| `location` | `str` | _required_ |
| `unit` | `str` | _required_ |
| `description` | `list[str]` | _required_ |
| `classifications` | `dict[str, str]` | _required_ |
| `product_name` | `str \| None` | _required_ |
| `product_amount` | `float \| None` | _required_ |
| `product_unit` | `str \| None` | _required_ |
| `all_products` | `list[Activity]` | _required_ |
| `exchanges` | `list[Union[TechnosphereExchange, BiosphereExchange, WasteExchange]]` | _required_ |
| `native_id` | `str \| None` | None |

#### Properties

##### `allocation_percent`

This process's own allocation share (0..100), or ``None``.

A multi-output process splits the parent activity's burden across its
co-products; every `all_products` entry carries its share. This
returns the share of *this* process (the entry whose ``process_id``
matches), and ``None`` for single-output processes.

##### `inputs`

Every input exchange: technosphere inputs and biosphere resources.

Equivalent to filtering `exchanges` by ``e.is_input``. Mixed
kinds: callers needing only one variant should use
`technosphere_inputs` or filter manually.

##### `is_allocated`

True iff the activity splits its burden across several co-products.

Reads the structured ``allocation_percent`` the engine sets on each
`all_products` entry (authoritative), not the description text.

##### `outputs`

Every output exchange: products and biosphere emissions.

Includes the reference product, coproducts (in allocated
activities), and all biosphere emissions.

##### `technosphere_inputs`

Only the technosphere inputs (ingredients from other activities).

Excludes biosphere inputs (resource extractions) and waste
outputs. The common case when answering "what does this activity
consume from upstream?".

### `ActivityDiff`

Result of ``compare_activities``.

| Field | Type | Default |
|-------|------|---------|
| `scope` | `str` | _required_ |
| `group_by` | `str` | _required_ |
| `matched` | `list[ActivityDiffRow]` | list() |
| `left_only` | `list[ActivityDiffRow]` | list() |
| `right_only` | `list[ActivityDiffRow]` | list() |

### `ActivityInput`

An activity as you write it: the body of `Client.create_activities`.

The inventory is three lists rather than one, so a field that means
something on a supplier link cannot be sent on an emission.

You do not choose the ``process_id``. The engine mints it from the name,
the location, and the product name and unit, which is what makes writing
the same activity twice a correction of one row rather than two. One
reference product per activity: coproducts and allocation are not supported
yet, and this type does not pretend they are.

| Field | Type | Default |
|-------|------|---------|
| `name` | `str` | _required_ |
| `location` | `str` | _required_ |
| `product_name` | `str` | _required_ |
| `product_amount` | `float` | _required_ |
| `product_unit` | `str` | _required_ |
| `description` | `list[str]` | list() |
| `inputs` | `list[TechInput]` | list() |
| `biosphere` | `list[BioExchange]` | list() |
| `waste_outputs` | `list[WasteOutput]` | list() |

### `ActivityDiffRow`

One matched or unmatched flow in an activity comparison.

| Field | Type | Default |
|-------|------|---------|
| `key` | `str` | _required_ |
| `left` | `float \| None` | _required_ |
| `right` | `float \| None` | _required_ |
| `unit` | `str \| None` | _required_ |

#### Properties

##### `delta`

right - left (0 if one side is missing).

### `AggregateGroup`

One bucket inside an AggregateResult.

| Field | Type | Default |
|-------|------|---------|
| `key` | `str` | _required_ |
| `quantity` | `float` | _required_ |
| `count` | `int` | _required_ |
| `unit` | `str \| None` | None |
| `share` | `float \| None` | None |

### `AggregateResult`

Result of a Client.aggregate() call.

``filtered_total`` is the sum across all items matching the filters (the
top-level number). ``groups`` is the per-bucket breakdown when ``group_by``
was set; empty otherwise.

| Field | Type | Default |
|-------|------|---------|
| `scope` | `AggregateScope` | _required_ |
| `filtered_total` | `float` | _required_ |
| `filtered_unit` | `str \| None` | _required_ |
| `filtered_count` | `int` | _required_ |
| `groups` | `list[AggregateGroup]` | list() |

### `BatchScores`

Result of `Client.score_activities` scoring many processes at once.

``results`` carries one `ScoredActivity` per process the engine
computed; ``not_found`` and ``invalid`` list the process ids it could not
resolve, and ``unscorable`` those that resolve but name an activity the
engine refuses to score (its quality report's ``unallocated`` check says
why; wire revision 14, empty from an older engine). A non-empty list is a
partial result to inspect, not a failure.

| Field | Type | Default |
|-------|------|---------|
| `results` | `list[ScoredActivity]` | _required_ |
| `not_found` | `list[str]` | _required_ |
| `invalid` | `list[str]` | _required_ |
| `unscorable` | `list[str]` | _required_ |

### `BioExchange`

One resource taken from the environment, or one emission released into it.

Name the flow one way or the other, never both: ``flow`` is the identifier
of a flow the database already has, which `Client.search_flows` and
`BiosphereExchange.flow_id` both return, while ``name`` with
``compartment`` and ``unit`` names one in words. Use the two constructors
rather than the fields, `from_id` and `from_name`, which is why
passing both or neither raises here instead of at the server.

A biosphere amount is never converted, so an exchange states its amount in
the flow's own unit.

| Field | Type | Default |
|-------|------|---------|
| `direction` | `BioDirection` | _required_ |
| `amount` | `float` | _required_ |
| `flow` | `str \| None` | None |
| `name` | `str \| None` | None |
| `compartment` | `str \| None` | None |
| `sub_compartment` | `str \| None` | None |
| `unit` | `str \| None` | None |
| `comment` | `str \| None` | None |

#### Methods

##### `BioExchange.existing(flow: str, direction: BioDirection | str, amount: float, *, unit: str | None = None, comment: str | None = None) -> BioExchange`

Retired name of `from_id`. Still works, and says so.

##### `BioExchange.from_id(flow: str, direction: BioDirection | str, amount: float, *, unit: str | None = None, comment: str | None = None) -> BioExchange`

An exchange on a flow the database already declares, by its identifier.

`Client.search_flows` is where identifiers come from.

##### `BioExchange.from_name(name: str, compartment: str, direction: BioDirection | str, amount: float, unit: str, *, sub_compartment: str | None = None, comment: str | None = None) -> BioExchange`

An exchange on the flow this name, compartment and unit address.

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

##### `BioExchange.introducing(name: str, compartment: str, direction: BioDirection | str, amount: float, unit: str, *, sub_compartment: str | None = None, comment: str | None = None) -> BioExchange`

Retired name of `from_name`. Still works, and says so.

It was named for what it did when a name reached nothing: bring the
flow into the database. It now reaches the flow the database already
declares under that name, and only introduces one when nothing does.

### `BiosphereExchange`

An exchange with the environment (resource extraction or emission).

``flow_id`` is what a line writes back when its words cannot address the
flow on their own: a name several flows answer to, or one whose source
recorded no compartment. Everything else restates as
`BioExchange.from_name` takes it.

| Field | Type | Default |
|-------|------|---------|
| `flow_name` | `str` | _required_ |
| `compartment` | `Compartment \| None` | _required_ |
| `amount` | `float` | _required_ |
| `unit` | `str` | _required_ |
| `direction` | `BioDirection` | _required_ |
| `flow_id` | `str` | _required_ |
| `comment` | `str \| None` | None |
| `is_biosphere` | `bool` | True |
| `is_waste` | `bool` | False |

#### Properties

##### `is_input`

True for resource extractions (``direction`` is ``RESOURCE``).

Biosphere inputs are resource extractions; outputs are emissions
to the environment.

##### `is_reference`

Always False: biosphere exchanges cannot be reference flows.

The reference flow defines the functional unit and is always a
technosphere product (see `TechnosphereExchange.is_reference`).

### `CharacterizationFactor`

One characterization factor matched against a database biosphere flow.

Returned in the ``factors`` list of `CharacterizationResult`.
``match_strategy`` records how the CF was matched to the DB flow
(``"uuid"``, ``"cas"``, ``"name"``, ``"synonym"``, ``"proxy"``).

| Field | Type | Default |
|-------|------|---------|
| `method_flow_name` | `str` | _required_ |
| `cf_value` | `float` | _required_ |
| `cf_unit` | `str` | _required_ |
| `direction` | `str` | _required_ |
| `db_flow_name` | `str` | _required_ |
| `flow_id` | `str` | _required_ |
| `flow_unit` | `str` | _required_ |
| `category` | `str` | _required_ |
| `match_strategy` | `str` | _required_ |
| `compartment` | `str \| None` | None |

### `CharacterizationResult`

Result of `Client.get_characterization`.

The engine truncates ``factors`` to ``shown`` rows (server-side ``limit``).
``matches`` is the unfiltered total: use `has_more` to detect when
the slice is incomplete.

| Field | Type | Default |
|-------|------|---------|
| `method` | `str` | _required_ |
| `unit` | `str` | _required_ |
| `matches` | `int` | _required_ |
| `shown` | `int` | _required_ |
| `factors` | `list[CharacterizationFactor]` | list() |

#### Properties

##### `has_more`

True when the server truncated below ``matches``.

### `ClassificationFilter`

Filter a supply-chain/consumers query by a classification (system, value, mode).

Matches one classification system entry, e.g.
``ClassificationFilter("Category", "Agricultural\\Food", "exact")`` or
``ClassificationFilter("Category", "Agricultural\\Food", MatchMode.EXACT)``.
Multiple filters are AND-combined by the server.

| Field | Type | Default |
|-------|------|---------|
| `system` | `str` | _required_ |
| `value` | `str` | _required_ |
| `mode` | `MatchMode` | <MatchMode.CONTAINS: 'contains'> |

### `ClassificationSystem`

One classification system declared by a database.

``values`` are the distinct entries in this system; ``activity_count`` is
how many activities carry at least one classification under this system
(helps callers pick a worthwhile filter dimension).

| Field | Type | Default |
|-------|------|---------|
| `name` | `str` | _required_ |
| `values` | `list[str]` | list() |
| `activity_count` | `int` | 0 |

### `Compartment`

Biosphere compartment (medium + optional subcompartment).

Frozen so it's hashable and immutable, so callers can use it as a dict key
when grouping flows by compartment, and accidental mutation is rejected.

| Field | Type | Default |
|-------|------|---------|
| `name` | `str` | _required_ |
| `sub` | `str \| None` | None |

### `ConsumerResult`

Activity that consumes a given supplier, with BFS depth.

| Field | Type | Default |
|-------|------|---------|
| `process_id` | `str` | _required_ |
| `activity_name` | `str` | _required_ |
| `location` | `str` | _required_ |
| `product_name` | `str` | _required_ |
| `product_amount` | `float` | _required_ |
| `product_unit` | `str` | _required_ |
| `depth` | `int` | _required_ |
| `classifications` | `dict[str, str]` | dict() |

### `ConsumersResponse`

Reverse supply chain (/consumers): paginated consumer list plus
optional edge set. Mirrors `SupplyChain` so callers have a
uniform {entries, edges} shape in both traversal directions.

``consumers`` is a `SearchResults[ConsumerResult]`: iterate it
to walk every consumer across all pages. ``edges`` is populated only
when ``include_edges=True``.

| Field | Type | Default |
|-------|------|---------|
| `consumers` | `SearchResults[ConsumerResult]` | _required_ |
| `edges` | `list[SupplyChainEdge]` | list() |

#### Methods

##### `ConsumersResponse.from_json(d: dict, *, fetch: Optional[Callable[[int, int | None], dict]] = None) -> ConsumersResponse`

Parse the /consumers wire envelope.

``fetch`` is a page fetcher returning the inner ``results`` envelope
for ``(offset, limit)``, used by ``SearchResults`` for lazy
iteration. The client wires this so users get pagination for free;
callers building ConsumersResponse manually (e.g. tests) can omit
it and the resulting SearchResults is "detached" (one page only).

### `ContributingActivities`

Top upstream activities driving an LCIA score.

Same engine-side limitation as `ContributingFlows`: the server
reports no total, so pyvolca cannot derive ``has_more``. Pass a generous
``limit`` and inspect ``share_pct`` if exhaustive coverage matters.

| Field | Type | Default |
|-------|------|---------|
| `method` | `str` | _required_ |
| `unit` | `str` | _required_ |
| `total_score` | `float` | _required_ |
| `activities` | `list[ActivityContribution]` | list() |

### `ContributingFlows`

Top elementary flows driving an LCIA score.

Note: the engine does not report a total: ``top_flows`` is whatever the
server returned under ``limit``, but pyvolca cannot tell whether more
flows were truncated. If you need exhaustive coverage, pass a generous
``limit`` and inspect ``share_pct`` totals.

| Field | Type | Default |
|-------|------|---------|
| `method` | `str` | _required_ |
| `unit` | `str` | _required_ |
| `total_score` | `float` | _required_ |
| `top_flows` | `list[FlowContribution]` | list() |

### `DatabaseInfo`

One entry of `Client.list_databases`.

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

| Field | Type | Default |
|-------|------|---------|
| `name` | `str` | _required_ |
| `display_name` | `str` | _required_ |
| `status` | `DatabaseStatus` | _required_ |
| `path` | `str` | _required_ |
| `load_at_startup` | `bool` | False |
| `is_uploaded` | `bool` | False |
| `activity_count` | `int` | 0 |
| `description` | `str \| None` | None |
| `format` | `str \| None` | None |
| `depends_on` | `list[str]` | list() |
| `allocation` | `str \| None` | None |
| `source` | `str \| None` | None |

### `ExchangeSelector`

Which lines of an inventory an edit is about.

``kind`` is ``"input"``, ``"waste"`` or ``"biosphere"``. The first two name
their provider by process id; the third names its flow by identity. There
is no kind for the reference product or a coproduct: changing those changes
what the activity *is*, which is not what an inventory edit does.

A selector may name several lines, and then it applies to all of them;
`Client.edit_exchanges` reports how many. Naming none is refused by
the engine rather than passed off as done.

| Field | Type | Default |
|-------|------|---------|
| `kind` | `str` | _required_ |
| `provider` | `str \| None` | None |
| `flow` | `str \| None` | None |

#### Methods

##### `ExchangeSelector.biosphere_flow(flow: str) -> ExchangeSelector`

A biosphere exchange, by flow id.

##### `ExchangeSelector.input_from(provider: str) -> ExchangeSelector`

A technosphere input, by the process id of what supplies it.

##### `ExchangeSelector.waste_to(provider: str) -> ExchangeSelector`

A waste output, by the process id of the treatment it goes to.

### `ExplainCFResult`

Result of `Client.explain_cf`.

``explanation`` is written by the engine: show it as it is rather than
rewording the codes. The structured fields are for comparing, filtering or
linking. ``outcome`` is ``"characterized"``, ``"conversion_refused"`` (a
factor was found but the flow's unit cannot be converted to its basis, so
the flow scores nothing) or ``"no_factor"``.

| Field | Type | Default |
|-------|------|---------|
| `method` | `str` | _required_ |
| `method_unit` | `str` | _required_ |
| `flow` | `ExplainedFlow` | _required_ |
| `outcome` | `str` | _required_ |
| `explanation` | `list[str]` | list() |
| `match` | `ExplainedMatch \| None` | None |
| `steps_tried` | `list[ExplainedStep]` | list() |
| `regional_factor_count` | `int` | 0 |

### `ExplainedFlow`

The flow an explanation is about, as the cascade sees it.

| Field | Type | Default |
|-------|------|---------|
| `id` | `str` | _required_ |
| `name` | `str` | _required_ |
| `unit` | `str` | _required_ |
| `category` | `str` | _required_ |
| `compartment` | `str \| None` | None |
| `cas` | `str \| None` | None |

### `ExplainedMatch`

The factor that was served, and where it came from.

| Field | Type | Default |
|-------|------|---------|
| `rung` | `str` | _required_ |
| `cf_value` | `float` | _required_ |
| `cf_unit` | `str` | _required_ |
| `method_flow_name` | `str` | _required_ |
| `match_strategy` | `str` | _required_ |
| `method_cas` | `str \| None` | None |
| `unit_conversion` | `str \| None` | None |
| `refusal` | `str \| None` | None |

### `ExplainedStep`

One rung of the factor-matching cascade, and what it made of the flow.

| Field | Type | Default |
|-------|------|---------|
| `rung` | `str` | _required_ |
| `result` | `str` | _required_ |
| `veto` | `str \| None` | None |

### `Flow`

One flow as returned by /flows.

Mirrors the server's `FlowSearchResult`. ``kind`` says which of the
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

| Field | Type | Default |
|-------|------|---------|
| `id` | `str` | _required_ |
| `name` | `str` | _required_ |
| `category` | `str` | _required_ |
| `unit_name` | `str` | _required_ |
| `kind` | `str \| None` | None |
| `compartment` | `str \| None` | None |
| `synonyms` | `dict[str, list[str]]` | dict() |
| `producer_count` | `int \| None` | None |

### `FlowContribution`

Top contributing elementary flow for an impact category.

Emitted inside ``LCIAResult.top_contributors``.

| Field | Type | Default |
|-------|------|---------|
| `flow_name` | `str` | _required_ |
| `contribution` | `float` | _required_ |
| `share_pct` | `float` | _required_ |
| `flow_id` | `str` | _required_ |
| `category` | `str` | _required_ |
| `cf_value` | `float` | 0.0 |
| `compartment` | `str \| None` | None |
| `match_kind` | `str \| None` | None |

### `FlowDetail`

Detail of one flow, returned by `Client.get_flow`.

``flow`` is the raw flow record: a tagged union (technosphere product,
biosphere flow, waste flow, or unresolved) whose shape depends on its
kind, kept as a dict rather than forced into one dataclass.
``usage_count`` is how many exchanges reference it.

| Field | Type | Default |
|-------|------|---------|
| `flow` | `dict` | _required_ |
| `unit_name` | `str` | _required_ |
| `usage_count` | `int` | _required_ |

### `FlowMapping`

CF-coverage report for one method against the current database.

``matched_flows / total_flows`` is the coverage ratio: how many of the
database's biosphere flows have a CF in this method. Mirrors the engine
response of `Client.get_flow_mapping`.

| Field | Type | Default |
|-------|------|---------|
| `method_name` | `str` | _required_ |
| `method_unit` | `str` | _required_ |
| `total_flows` | `int` | _required_ |
| `matched_flows` | `int` | _required_ |
| `flows` | `list[FlowMappingEntry]` | list() |

#### Properties

##### `coverage_pct`

Matched fraction expressed as 0..100. Returns 0 when total is 0.

### `FlowMappingEntry`

One DB biosphere flow and the CF (if any) assigned to it.

``cf_value`` is ``None`` when this DB flow has no characterization factor
in the method: that flow contributes 0 to the score for the method.
``match_strategy`` records how the mapping was resolved (``"uuid"``,
``"cas"``, ``"name"``, ``"synonym"``, ``"proxy"``).

| Field | Type | Default |
|-------|------|---------|
| `flow_id` | `str` | _required_ |
| `flow_name` | `str` | _required_ |
| `flow_category` | `str` | _required_ |
| `cf_value` | `float \| None` | None |
| `cf_flow_name` | `str \| None` | None |
| `match_strategy` | `str \| None` | None |

### `Installed`

Result of `download`.

| Field | Type | Default |
|-------|------|---------|
| `binary` | `Path` | _required_ |
| `data_dir` | `Path` | _required_ |
| `version` | `str` | _required_ |
| `data_version` | `str` | _required_ |

### `InventoryFlow`

One row of an inventory: a biosphere flow scaled to the functional unit.

``is_emission`` distinguishes outputs (releases) from inputs (resource
extraction). ``flow_id`` is the database UUID; ``compartment`` is the
medium label (e.g. ``"air/urban air"``) when the source dataset declared
one. ``category`` is the engine-normalised category used for grouping.

| Field | Type | Default |
|-------|------|---------|
| `flow_id` | `str` | _required_ |
| `flow_name` | `str` | _required_ |
| `quantity` | `float` | _required_ |
| `unit_name` | `str` | _required_ |
| `is_emission` | `bool` | _required_ |
| `category` | `str` | _required_ |
| `compartment` | `str \| None` | None |

### `InventoryResult`

Life-cycle inventory of an activity: cumulative biosphere flows.

Returned by `Client.get_inventory`. The engine does not paginate:
``flows`` is the full inventory (filtered by ``flow=`` substring when
requested). ``statistics`` carries the per-direction roll-ups and the
most-populated categories.

``root`` is the activity the inventory was computed for. ``total_flows``,
``emission_flows``, ``resource_flows`` mirror the engine's metadata block.

| Field | Type | Default |
|-------|------|---------|
| `root` | `Activity` | _required_ |
| `total_flows` | `int` | _required_ |
| `emission_flows` | `int` | _required_ |
| `resource_flows` | `int` | _required_ |
| `flows` | `list[InventoryFlow]` | _required_ |
| `statistics` | `InventoryStatistics` | _required_ |

### `InventoryStatistics`

Roll-up totals of an inventory result.

``emission_quantity`` and ``resource_quantity`` are sums by direction;
``total_quantity`` is the sum of absolute values. ``top_categories``
lists ``(category_name, flow_count)`` pairs ordered by frequency.

| Field | Type | Default |
|-------|------|---------|
| `total_quantity` | `float` | _required_ |
| `emission_quantity` | `float` | _required_ |
| `resource_quantity` | `float` | _required_ |
| `top_categories` | `list[tuple[str, int]]` | list() |

### `LCIABatchResult`

Batch LCIA: every impact category in a method collection, for one activity.

Returned by `Client.get_impacts_batch`. Carries the per-method
impact results plus any formula-based scoring sets configured in the
engine TOML (PEF, ECS, or any named set).

``scoring_indicators`` gives the per-variable normalized-weighted
breakdown of each scoring set, already multiplied by the set's
``displayMultiplier`` and expressed in its display unit (see
`ScoringIndicator`). Lets callers render per-indicator charts
alongside the aggregate ``scoring_results``.

| Field | Type | Default |
|-------|------|---------|
| `results` | `list[LCIAResult]` | _required_ |
| `single_score` | `float \| None` | None |
| `single_score_unit` | `str \| None` | None |
| `norm_weight_set_name` | `str \| None` | None |
| `available_nw_sets` | `list[str]` | list() |
| `scoring_results` | `dict[str, dict[str, float]]` | dict() |
| `scoring_units` | `dict[str, str]` | dict() |
| `scoring_indicators` | `dict[str, dict[str, ScoringIndicator]]` | dict() |

### `LCIAResult`

LCIA score for one impact category on one activity.

Returned directly by `Client.get_impacts`, and nested inside
`LCIABatchResult.results` (one entry per impact category).

``functional_unit`` is the basis ``score`` is reported against, and it is
always one unit of the reference product (``"1.00 kg of cream"``) however
much of it the source says the process produces.

| Field | Type | Default |
|-------|------|---------|
| `method_id` | `str` | _required_ |
| `method_name` | `str` | _required_ |
| `category` | `str` | _required_ |
| `damage_category` | `str` | _required_ |
| `score` | `float` | _required_ |
| `unit` | `str` | _required_ |
| `mapped_flows` | `int` | _required_ |
| `functional_unit` | `str` | _required_ |
| `normalized_score` | `float \| None` | None |
| `weighted_score` | `float \| None` | None |
| `top_contributors` | `list[FlowContribution]` | list() |

### `MappingStatus`

How a method's factors map onto a database's biosphere flows.

Returned by `Client.get_mapping_status`. The ``mapped_by_*`` counts
break the match cascade down by stage (UUID, then CAS, then name, then
synonym); ``coverage`` is the matched percentage (0–100), and
``unmapped_flows`` lists the factors still without a database flow.

Parsed by hand rather than via the snake-case mixin because the acronym
runs (``mappedByUUID``, ``mappedByCAS``, ``dbBiosphereCount``) do not
survive the generic camelCase→snake_case conversion.

| Field | Type | Default |
|-------|------|---------|
| `method_id` | `str` | _required_ |
| `method_name` | `str` | _required_ |
| `total_factors` | `int` | _required_ |
| `mapped_by_uuid` | `int` | _required_ |
| `mapped_by_cas` | `int` | _required_ |
| `mapped_by_name` | `int` | _required_ |
| `mapped_by_synonym` | `int` | _required_ |
| `unmapped` | `int` | _required_ |
| `coverage` | `float` | _required_ |
| `db_biosphere_count` | `int` | _required_ |
| `unique_db_flows_matched` | `int` | _required_ |
| `unmapped_flows` | `list[UnmappedFlow]` | _required_ |

### `Method`

One LCIA method, returned by `Client.list_methods`.

Pass ``id`` (or ``name``, which the client resolves against the loaded
methods) wherever a ``method_id`` is asked for. ``collection`` is the
parent method collection (e.g. ``"ef-31"``); the client reads it off the
resolved method, so it is worth passing to `Client.get_impacts` /
`Client.get_impacts_batch` only to pin one of several loaded.

| Field | Type | Default |
|-------|------|---------|
| `id` | `str` | _required_ |
| `name` | `str` | _required_ |
| `category` | `str` | _required_ |
| `unit` | `str` | _required_ |
| `factor_count` | `int` | _required_ |
| `collection` | `str` | _required_ |

### `MethodDetail`

Detail of one LCIA method, returned by `Client.get_method`.

``factor_count`` is the number of characterization factors; ``methodology``
and ``description`` are free-text metadata the source may or may not carry.

| Field | Type | Default |
|-------|------|---------|
| `id` | `str` | _required_ |
| `name` | `str` | _required_ |
| `unit` | `str` | _required_ |
| `category` | `str` | _required_ |
| `factor_count` | `int` | _required_ |
| `description` | `str \| None` | None |
| `methodology` | `str \| None` | None |

### `MethodFactor`

One characterization factor of a method (`Client.get_method_factors`).

``direction`` is the flow direction the factor applies to; ``value`` is the
factor in the method's unit per the flow's unit. A method routinely holds
several factors sharing one ``flow_name`` (the same substance emitted to
air vs. water, or one regionalized factor per ``location``), so
``compartment``, ``location`` and ``unit`` are what tell them apart.
Each is ``None`` when the source method does not carry that axis, or
when the engine predates these fields.

| Field | Type | Default |
|-------|------|---------|
| `flow_ref` | `str` | _required_ |
| `flow_name` | `str` | _required_ |
| `direction` | `str` | _required_ |
| `value` | `float` | _required_ |
| `unit` | `str \| None` | None |
| `compartment` | `str \| None` | None |
| `location` | `str \| None` | None |

### `PathResult`

Shortest upstream path from a root process to a matching activity.

| Field | Type | Default |
|-------|------|---------|
| `path` | `list[PathStep]` | _required_ |
| `path_length` | `int` | _required_ |
| `total_ratio` | `float` | _required_ |

### `PathStep`

One step in the supply chain path returned by get_path_to.

Note: the /path endpoint is hand-built (aeson `object [...]`) but now
emits camelCase keys (``processId``, ``activityName``,
``cumulativeQuantity``, …) like the rest of the API.

| Field | Type | Default |
|-------|------|---------|
| `process_id` | `str` | _required_ |
| `activity_name` | `str` | _required_ |
| `location` | `str` | _required_ |
| `unit` | `str` | _required_ |
| `cumulative_quantity` | `float` | _required_ |
| `scaling_factor` | `float` | _required_ |
| `local_step_ratio` | `float \| None` | None |

### `PerturbedResult`

One perturbation outcome from `Client.compute_sensitivity`.

The engine flattens an ``Either`` on the wire: a success carries
``impact`` and ``delta_impact`` (with ``error`` None), a failure carries
``error`` (with the other two None). ``perturbation`` echoes the request
entry (including its ``label`` if one was supplied), so results correlate
without an out-of-band index.

| Field | Type | Default |
|-------|------|---------|
| `perturbation` | `dict` | _required_ |
| `impact` | `LCIAResult \| None` | _required_ |
| `delta_impact` | `float \| None` | _required_ |
| `error` | `str \| None` | _required_ |

### `Preset`

A named classification preset declared in the engine config.

Apply by passing ``preset=preset.name`` to filtering endpoints (the engine
expands it server-side into the ``filters`` triples).

| Field | Type | Default |
|-------|------|---------|
| `name` | `str` | _required_ |
| `label` | `str` | _required_ |
| `description` | `str \| None` | _required_ |
| `filters` | `list[PresetFilter]` | list() |

### `PresetFilter`

One filter triple inside a `Preset`.

| Field | Type | Default |
|-------|------|---------|
| `system` | `str` | _required_ |
| `value` | `str` | _required_ |
| `mode` | `MatchMode` | <MatchMode.CONTAINS: 'contains'> |

### `ScoredActivity`

One process's batch impacts inside a `BatchScores`.

``impacts`` is the same `LCIABatchResult` that
`Client.get_impacts_batch` returns for a single process.

| Field | Type | Default |
|-------|------|---------|
| `process_id` | `str` | _required_ |
| `activity_name` | `str` | _required_ |
| `impacts` | `LCIABatchResult` | _required_ |

### `ScoringIndicator`

One per-variable entry inside ``LCIABatchResult.scoring_indicators``.

``value`` is pre-multiplied by the scoring set's ``displayMultiplier``
(configured in the scoring TOML) and expressed in the set's display unit.
``category`` is the indicator's display name: the scoring set's
``labels`` entry when one is configured (typically for computed
variables), otherwise the impact category the variable was resolved
from, or as a last resort the raw variable key.

| Field | Type | Default |
|-------|------|---------|
| `category` | `str` | _required_ |
| `value` | `float` | _required_ |

### `SearchCounts`

How many processes, products and flows one query matches.

The three are disjoint and together cover the database, so they partition
what a query found. Use them to tell which of the three a term is really
about before listing any of them: a term matching 2 processes and 300
flows is a substance name, not a product.

Needs engine wire revision 17.

| Field | Type | Default |
|-------|------|---------|
| `processes` | `int` | _required_ |
| `products` | `int` | _required_ |
| `flows` | `int` | _required_ |

### `SearchResults`

Paginated wire envelope, mirrors Haskell ``SearchResults a``.

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

| Field | Type | Default |
|-------|------|---------|
| `results` | `list[~T]` | _required_ |
| `total` | `int` | _required_ |
| `offset` | `int` | _required_ |
| `limit` | `int` | _required_ |
| `has_more` | `bool` | _required_ |
| `search_time_ms` | `float` | _required_ |
| `_fetch` | `Optional[Callable[[int, int \| None], dict]]` | None |
| `_parse` | `Optional[Callable[[dict], ~T]]` | None |
| `_fetched` | `list[~T]` | list() |
| `_exhausted` | `bool` | False |

#### Properties

##### `page_size`

Server-applied limit (page size for further fetches).

#### Methods

##### `SearchResults.from_raw(raw: dict, *, parse: Callable[[dict], ~T], fetch: Optional[Callable[[int, int | None], dict]] = None) -> SearchResults[T]`

Build from the wire envelope.

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

##### `SearchResults.page(n: int, *, page_size: int | None = None) -> SearchResults[T]`

Fetch a specific page (1-based). Returns a fresh SearchResults.

``page_size`` overrides the current ``limit`` for the fetched page;
the returned envelope's ``limit`` reflects what the server actually
applied.

### `SensitivityResult`

Sensitivity analysis: baseline impact plus one entry per perturbation.

Returned by `Client.compute_sensitivity`. ``perturbed`` preserves
the order of the requested perturbations.

| Field | Type | Default |
|-------|------|---------|
| `baseline` | `LCIAResult` | _required_ |
| `perturbed` | `list[PerturbedResult]` | _required_ |

### `ServerVersion`

Server build metadata returned by `Client.get_version`.

``git_tag`` is None for untagged dev builds. ``build_target`` names the
platform triple the binary was compiled for (e.g. ``"x86_64-linux"``).
``wire_version`` is the engine's advertised JSON wire-format revision, or
None for engines that predate it (everything up to v0.7.x).
``data_version`` names the reference-data bundle the engine reads; None
when it reads none, or for engines that predate the field. Two engines of
one version giving two scores for one calculation differ here.

| Field | Type | Default |
|-------|------|---------|
| `version` | `str` | _required_ |
| `git_hash` | `str` | _required_ |
| `git_tag` | `str \| None` | _required_ |
| `build_target` | `str` | _required_ |
| `wire_version` | `int \| None` | None |
| `data_version` | `str \| None` | None |

### `SetAmount`

The lines to restate, and what to restate them to.

| Field | Type | Default |
|-------|------|---------|
| `select` | `ExchangeSelector` | _required_ |
| `amount` | `float` | _required_ |

### `Substitution`

Replace one supplier with another in the upstream supply chain.

All fields are process_ids. ``consumer`` identifies which downstream
consumer's input to rewrite, scoping the swap to one edge: the same
upstream supplier can be replaced by different alternatives in different
parts of the tree. Omit it (leave ``None``) to apply the swap globally,
replacing the supplier on every consumer at once.

Frozen so callers can put it in a set / dict key and re-use the same
substitution across multiple calls without aliasing risk.

| Field | Type | Default |
|-------|------|---------|
| `from_pid` | `str` | _required_ |
| `to_pid` | `str` | _required_ |
| `consumer` | `str \| None` | None |

#### Methods

##### `Substitution.to_wire()`

Serialise to the wire shape consumed by SubstitutionRequest.

### `SupplierClaim`

What the source said about an exchange's supplier, before linking.

Every format designates a supplier differently, and linking overwrites
what it resolved to; this is the claim itself, which linking never
rewrites, so a reader can tell what the file asked for from what the
engine found. ``value`` is the identifier, name or dataset number the
claim carries, and is ``None`` for `ClaimKind.PRODUCT`, which names
the supplier by the product row and carries nothing else.

Wire revision 23; ``None`` on a technosphere exchange from an older engine.

| Field | Type | Default |
|-------|------|---------|
| `kind` | `ClaimKind` | _required_ |
| `value` | `str \| int \| None` | None |

### `SupplyChain`

Flat supply chain of an activity.

``total_activities`` is the unfiltered upstream count; ``filtered_activities``
is what remains after the server applies ``classification_filters`` /
``min_quantity`` / ``preset``. ``entries`` is the slice the server actually
returned; it may be shorter than ``filtered_activities`` when ``limit``
truncates. Use `has_more` to detect that case rather than comparing
lengths by hand.

| Field | Type | Default |
|-------|------|---------|
| `root` | `Activity` | _required_ |
| `total_activities` | `int` | _required_ |
| `filtered_activities` | `int` | _required_ |
| `entries` | `list[SupplyChainEntry]` | list() |
| `edges` | `list[SupplyChainEdge]` | list() |

#### Properties

##### `has_more`

True when the server truncated ``entries`` below ``filtered_activities``.

Surfacing this lets callers detect silent truncation: if you passed
``limit=100`` and ``filtered_activities`` is 500, downstream LCA work
would be wrong without flagging the gap.

### `SupplyChainEdge`

A consumer→supplier link in the supply chain.

``from``/``to`` are Python keywords, so the process ids are stored under
``from_id``/``to_id``. ``from_db``/``to_db`` carry each endpoint's database
name, which is required to route edges across databases (the same process
id can exist in more than one loaded DB).

| Field | Type | Default |
|-------|------|---------|
| `from_id` | `str` | _required_ |
| `from_db` | `str` | _required_ |
| `to_id` | `str` | _required_ |
| `to_db` | `str` | _required_ |
| `amount` | `float` | _required_ |

### `SupplyChainEntry`

One activity in a `SupplyChain.entries` list.

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
second `Client.get_activity` round trip.
``depth`` is the BFS shortest-path distance from the queried root
(0 = the root itself), ``upstream_count`` the number of direct
consumers of this activity inside the chain, and ``database_name``
the database the entry lives in (they differ across linked databases).

| Field | Type | Default |
|-------|------|---------|
| `process_id` | `str` | _required_ |
| `database_name` | `str` | _required_ |
| `activity_name` | `str` | _required_ |
| `location` | `str` | _required_ |
| `quantity` | `float` | _required_ |
| `unit` | `str` | _required_ |
| `scaling_factor` | `float` | _required_ |
| `depth` | `int` | _required_ |
| `upstream_count` | `int` | _required_ |
| `classifications` | `dict[str, str]` | dict() |

### `TechInput`

One product an activity consumes, named by the process that supplies it.

``provider`` is a ``process_id`` (``activityUUID_productUUID``, or a bare
activity UUID when that activity has a single product), the same address
every read endpoint hands out. The flow follows from the supplier, so it is
never stated separately. ``unit`` defaults to the supplier's own reference
unit; another one is fine as long as it converts.

| Field | Type | Default |
|-------|------|---------|
| `provider` | `str` | _required_ |
| `amount` | `float` | _required_ |
| `unit` | `str \| None` | None |
| `comment` | `str \| None` | None |

### `TechnosphereExchange`

An exchange with another activity. Carries no compartment: the
producing activity's classifications describe the product taxonomy.

``supplier_claim`` is how the source designated the supplier, which is not
the same question as which activity it resolved to: ``target_process_id``
is the answer, this is what was asked (wire revision 23).

| Field | Type | Default |
|-------|------|---------|
| `flow_name` | `str` | _required_ |
| `amount` | `float` | _required_ |
| `unit` | `str` | _required_ |
| `role` | `TechRole` | _required_ |
| `target_activity_name` | `str \| None` | _required_ |
| `target_location` | `str \| None` | _required_ |
| `target_process_id` | `str \| None` | _required_ |
| `comment` | `str \| None` | None |
| `supplier_claim` | `SupplierClaim \| None` | None |
| `is_biosphere` | `bool` | False |
| `is_waste` | `bool` | False |

#### Properties

##### `is_input`

True for technosphere inputs (``role`` is ``INPUT`` or ``REFERENCE_INPUT``).

Lets callers split exchanges into inputs vs. outputs without
knowing the four-role taxonomy.

##### `is_reference`

True for reference roles (``REFERENCE_PRODUCT`` / ``REFERENCE_INPUT``).

The reference exchange is the one that defines the activity's
functional unit, the basis the LCA result is normalised to.

### `UnmappedFlow`

A method factor with no matching database flow (in `MappingStatus`).

| Field | Type | Default |
|-------|------|---------|
| `flow_ref` | `str` | _required_ |
| `flow_name` | `str` | _required_ |
| `direction` | `str` | _required_ |
| `compartment` | `str \| None` | None |

### `WasteExchange`

An exchange of a waste flow with a treatment activity.

Shares the technosphere matrix with product flows but tracked as its own
kind so callers can tell a "waste sent to landfill" output apart from a
product input. A waste output no treatment is found for contributes zero
impact, the same cut-off semantics as an orphan technosphere input;
``role`` says whether that is because nothing treats it or because the
treatment it names was not loaded.

| Field | Type | Default |
|-------|------|---------|
| `flow_name` | `str` | _required_ |
| `amount` | `float` | _required_ |
| `unit` | `str` | _required_ |
| `is_input` | `bool` | _required_ |
| `target_activity_name` | `str \| None` | _required_ |
| `target_location` | `str \| None` | _required_ |
| `target_process_id` | `str \| None` | _required_ |
| `comment` | `str \| None` | None |
| `role` | `WasteRole \| None` | None |
| `supplier_claim` | `SupplierClaim \| None` | None |
| `is_biosphere` | `bool` | False |
| `is_waste` | `bool` | True |

#### Properties

##### `is_reference`

Always False: waste flows never define an activity's functional unit.

Treatment activities have a ``ReferenceInput`` instead, exposed
via `TechnosphereExchange`.

### `WasteOutput`

One residue an activity hands to a treatment process.

``provider`` names that treatment process, exactly as a `TechInput`
names its producer.

| Field | Type | Default |
|-------|------|---------|
| `provider` | `str` | _required_ |
| `amount` | `float` | _required_ |
| `unit` | `str \| None` | None |
| `comment` | `str \| None` | None |

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
    `Installed` with the resolved paths and versions.

## Type aliases

### `Exchange`

Type alias: `Union[TechnosphereExchange, BiosphereExchange, WasteExchange]`.

### `RefDataKind`

Type alias: `Literal['flow-synonyms', 'compartment-mappings', 'units']`.

<!-- END: api-reference -->

## See also

- Full guide and tutorials: <https://www.volca.run/docs/python/>
- VoLCA engine: <https://github.com/ccomb/volca>
- Runnable examples: <https://www.volca.run/examples/>

## License

Apache-2.0
