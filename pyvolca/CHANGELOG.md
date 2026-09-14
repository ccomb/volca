# Changelog

All notable changes to **pyvolca** are documented here. Versions follow
[semver](https://semver.org/); breaking changes bump the minor while we're
in 0.x.

The next-release section is **drafted by [git-cliff](https://git-cliff.org/)**
from commits that touched `pyvolca/` since the last `pyvolca-v*` tag, then
hand-edited for narrative and breaking-change callouts (`cliff.toml` only
groups commits by Conventional Commits prefix, and it doesn't know which renames
are user-visible breaks). Workflow:

```bash
cd pyvolca
git cliff --unreleased                        # preview against current HEAD
git cliff --unreleased --tag pyvolca-v0.X.Y   # render as a released section
```

Then paste the rendered block at the top of this file and tighten wording.

## [0.11.0] - 2026-09-02

The minor is for two additions a strict reader cannot ignore: the exchange
role gains a value, and a batch result gains a field.

### Added

- `TechRole.AVOIDED_PRODUCT`: a substitution, the product the activity
  displaces, credited to that product's producer. Code that matches the four
  previous roles exhaustively has a fifth case to answer.
- `BatchScores.unscorable`, the process ids that resolve but name an activity
  the engine refuses to score; the `unallocated` check of its quality report
  says why, which this build reads through `Client.call`. An older engine
  sends none and the field reads empty.
- `UnmappedFlow.compartment`, the compartment the method row states, so a
  vocabulary gap is visible without reading the engine log. It is None when
  the row states none, and when the engine predates wire revision 13.

### Changed

- This build knows wire revision 14, so it no longer warns that an engine
  running v0.12.0 speaks a format newer than it understands. Revision 13 adds
  the compartment an unmapped factor of the mapping report carries; revision
  14 adds the `AvoidedProduct` exchange role, the `unallocated` quality check,
  and the declared share and classification a technosphere exchange now
  carries on the wire, which this build passes through rather than decoding
  into fields of its own. The oldest revision this client accepts is still 2,
  and it still runs against any engine from v0.9.1 on.
- `COPRODUCT` now describes a product output the source left unallocated, and
  an activity still carrying one is refused a score. The value did not move;
  what it means did.
- The match strategy a mapping or a characterization factor reports lists
  `proxy` where it listed `fuzzy`. No matcher ever returned `fuzzy`, and the
  engine has removed it.

## [0.10.1] - 2026-08-28

### Changed

- This build knows wire revision 12, so it no longer warns that an engine
  running v0.11.0 speaks a format newer than it understands. Revision 12 adds
  the node type a tree export reports for a link that names a row no loaded
  database holds. Nothing else moves: the oldest revision this client accepts
  is still 2, and it still runs against any engine from v0.9.1 on.
- The documentation link points at `www.volca.run`, which is the host that
  answers. `volca.run` without the prefix does not, so the link shown on PyPI,
  the one in the README and the one in the package docstring all led nowhere.

## [0.10.0] - 2026-08-22

The minor is for one rename: `Decomposition.electricity_kwh` is now
`electricity_mj`. Everything else here is additive or keeps its old name
working.

### Changed

- **`Decomposition.electricity_kwh` is now `electricity_mj`**, and holds
  megajoules rather than kilowatt hours. The engine measures energy in
  megajoules from v0.10.0, so the electricity a decomposition reports is
  read back with `filter_unit="MJ"`. A script reading the old attribute
  raises `AttributeError` rather than returning a number in the wrong unit,
  which is the reason this is a rename and not a silent change of contents.
- `BioExchange.existing` and `BioExchange.named` are now
  `BioExchange.from_id` and `BioExchange.from_name`, named for the one thing
  that separates them: how the exchange designates its flow. `existing`,
  `named` and `introducing` keep working and warn, naming their replacement,
  until pyvolca 1.0.
- A method can be asked for by name as readily as by UUID, and the
  collection carrying it no longer has to be named: `get_impacts(pid, "Water
  use")` scores. `collection` lost its old default of `"methods"`, a name
  nothing ever loads, which is why that call used to fail whatever you
  passed; a whole-collection call runs against the only loaded collection.
  Nothing is guessed: an unknown method, a name two collections carry, or
  several collections loaded with none named raises before the request
  leaves, naming the candidates. Such a refusal carries no HTTP status, so
  read the exception itself rather than `status_code`.
- `search_flows` answers with every kind of flow and says which each one is.
  It used to be documented as returning biosphere flows, which was never
  true.

### Added

- `search_flows(kind=...)` keeps one kind of flow: `"technosphere"`,
  `"biosphere"` or `"waste"`. `Flow.kind` says which each result is. A value
  that is none of the three is refused before the request leaves rather than
  quietly ignored, which would have read as "no flow of that kind exists".
  Needs an engine >= v0.10.0.
- `WasteExchange.role` says what a waste line does, as a `WasteRole`:
  `TREATS_WASTE` for a line the activity treats, `SENT_TO_TREATMENT` for an
  output whose treatment was found, `FINAL_WASTE_FLOW` for one naming no
  treatment, and `TREATMENT_NOT_LOADED` for one naming a treatment no loaded
  database ships. The last two both used to arrive as a waste with no
  target, so a complete end-of-life flow and a missing dependency read
  alike. `None` against an engine older than v0.10.0, which does not say.
  Needs an engine >= v0.10.0 for a value.
- `ServerVersion.data_version` names the reference-data bundle the engine
  reads. Two engines of one version answering differently differ on this
  field. `None` when the engine ships no bundle, or is too old to say.
  Needs an engine >= v0.10.0.
- A database's two quality reports can be taken as CSV.
  Needs an engine >= v0.10.0.
- The PyPI listing links the changelog.

### Fixed

- A biosphere exchange naming its flow in words reaches the flow the
  database already declares under that name and compartment, instead of
  always creating one. Writing `Nitrogen, total` in water used to mint a
  second flow of that name, which no characterization method knows, so the
  emission scored as zero beside the curated one it was meant to be. A name
  nothing answers to still brings a flow into the database with the warning
  it always carried, and a name two flows answer to is refused with both
  identifiers. This is engine-side; the client change is that
  `BioExchange.from_name` is the documented way to reach it.
- A flow name found by a search is found again by a filter. Both sides now
  read a query the same way: every word, in any order, punctuation optional.

## [0.9.2] - 2026-08-04

### Added

- `Client.create_activities` and `Client.replace_activity` write activities
  into a database you uploaded or copied, reading the same document the HTTP
  API and the command line take. You never supply a process id: it is derived
  from the description, so writing the same description twice corrects one
  activity rather than making two. Needs an engine >= v0.9.5.
- `Client.edit_exchanges` adjusts what one activity consumes and emits
  without re-describing it: lines to remove, amounts to restate, lines to
  add, each named the way you already read it: an input by its provider, a
  waste output by its treatment, an emission by its flow. Everything the edit
  does not name stays as it is. Needs an engine >= v0.9.5.
- `Client.explain_cf` replays the factor lookup for one flow and returns the
  engine's own sentences: which rung of the cascade found the factor, which
  line of the method it came from, and how the amount was carried onto the
  factor's basis. Needs an engine >= v0.9.5.
- The PyPI listing carries keywords (`lca`, `life-cycle-assessment`,
  `life-cycle-inventory`, `lcia`, `environmental-data`) and the classifiers
  that place it: research audience, OS independent, and the Python versions it
  supports. Searching PyPI for "life cycle assessment" found nothing
  before this.

### Changed

- Flow search results carry a `compartment` field next to `category`, which
  has always held the medium alone, so same-named flows (Agribalyse carries
  seven `Deltamethrin`) can be told apart.

### Fixed

- The Documentation link on the PyPI page pointed at a page that does not
  exist and answered 404. It now points at <https://volca.run/docs/python/>,
  where the guide has been all along. The same dead address was in the
  package's own module docstring, so `help(volca)` sent people there too.

## [0.9.1] - 2026-08-01

### Added

- `get_impacts_batch(..., exclude_long_term=True)` drops long-term emissions
  before scoring, the switch `score_activities` already carried. The engine
  route has always accepted it and only this wrapper had no way to say so, so
  scoring a whole list and scoring one process could not be asked the same
  question.

### Changed

- `Server.start(idle_timeout=…)` counts real use rather than traffic. Against
  an engine >= v0.9.4, an API request or a matrix solve holds the server open,
  while an MCP client that merely stays connected does not, so an assistant left
  running overnight no longer keeps the process alive on its own.

## [0.9.0] - 2026-07-31

### Changed (breaking)

- `SupplyChainEntry` gains three **required** fields the engine has emitted
  since v0.6.0 and pyvolca silently dropped at decoding: `depth` (BFS
  shortest-path distance from the queried root, 0 = the root itself),
  `upstream_count` (direct consumers of the entry inside the chain), and
  `database_name` (which loaded database the entry lives in). Every engine
  this client accepts (wire revision >= 2, i.e. >= v0.9.1) emits them, so
  they are required without defaults: the type guarantees what the wire
  guarantees, like `ConsumerResult.depth` already did. **Breaking only for
  code that constructs `SupplyChainEntry` by hand** (mocks, fixtures): add
  the three fields. Code that reads decoded entries just gains data: a
  filtered `get_supply_chain` response is no longer flat, `depth` finally
  tells a packaging system from the stage that consumes it without a
  hand-rolled traversal.

### Added

- `export_method_collection(name, fmt=…)` exports a loaded method
  collection as SimaPro method CSV (`simapro`), columnar CSV (`csv`),
  openLCA JSON-LD (`openlca`), or an ILCD LCIA-method package (`ilcd`),
  returning the serialized bytes.
- `get_collection_coverage` reports how much of a database a whole method
  collection characterizes (typed `CollectionCoverage`), counting coverage
  the way scoring counts it.
- The client now understands wire revision 4 (engines >= v0.9.4, which
  advertise the quality-report routes); nothing changes against older
  engines.

## [0.8.2] - 2026-07-15

### Added

- `delete_activities(ids=[...])` deletes exactly the named processes, with no more
  deliberately unsatisfiable filter plus `extra`. Needs an engine speaking
  wire revision 3 (>= v0.9.3): an older one would silently drop the unknown
  key and read the request as an empty filter ("everything"), so pyvolca
  refuses to send it there instead of letting the engine guess. `keep` and
  `extra` compose with `ids`; the filter arguments do not.
- `Server(config=None)` starts the engine without any config file: built-in
  defaults, no databases (needs an engine >= v0.9.3). Scripts that only
  upload or convert no longer have to write an empty TOML. A config *path*
  that does not exist now fails loudly at `start()` instead of the engine
  dying behind the scenes. A typo must never silently become "all defaults".
- `Server.start()` fails fast with the exit code when the spawned engine dies
  before serving, instead of hanging until the readiness timeout, in both
  fixed-port and `port="auto"` modes.
- `Client.ensure_database(source, name=…)`: the one-call, idempotent form of
  the upload lifecycle. It matches by display name (default: the file's
  stem), uploads only when absent, finalizes the staged copy, loads if
  unloaded, and returns the slug. Run it at the top of a script and it
  converges on the same loaded database every time instead of re-uploading;
  the list → match → upload → finalize → load state machine that every
  script hand-rolled is now one line. A copy that is not ready to finalize
  raises with the concrete blocker (missing suppliers, no activities
  parsed), including an upload left staged by an earlier failed run, which
  goes through the same readiness gate instead of being loaded half-linked.
- `Client.resolve_activities(names)` resolves a batch of activity (or
  product) names to their matching activities with concurrent searches.
  The returned mapping is total (a name that doesn't resolve maps to an
  empty list instead of disappearing), and replaces the two patterns
  scripts kept hand-rolling: downloading the whole database to build a
  name→process_id dict, and per-name thread pools.
- `search_activities`, `search_flows`, `get_supply_chain`, and
  `get_consumers` take optional `sort=` / `order=` keyword arguments,
  forwarding to the engine's ordering support. Left unset, nothing changes.
- `MethodFactor` gains `compartment`, `location`, and `unit`: the axes
  that distinguish factors sharing one `flow_name` (the same substance
  emitted to air vs. water, or one regionalized factor per location).
  `None` when the source method has no such axis, or the engine predates
  these fields.
- `aggregate` gains a `consumption` scope (`AggregateScope.CONSUMPTION`)
  with `filter_consumer` / `filter_consumer_not`: total upstream demand
  for a flow (electricity, heat, grass…) without the double counting that
  summing cumulative production gives when a flow crosses several
  transformation steps.

### Changed

- pyvolca now understands wire revision 3 while still accepting wire-2
  engines (v0.9.1 / v0.9.2): everything works against them except the
  revision-gated `ids` selection, which fails with a clear message. An
  engine newer than wire 3 still triggers the upgrade warning.

## [0.8.1] - 2026-07-14

### Added

- `Server(port="auto")` asks the engine (v0.9.2 or newer) to bind an
  OS-assigned loopback port atomically and reads the bound port from
  `VOLCA_PORT=N`; managed servers no longer need to select a port with a
  reserve-then-release race.

### Fixed

- Binary database exports work again: 0.8.0 forgot to send the
  `Accept: application/octet-stream` header the engine's raw-bytes export
  requires, so every `export_database` call failed with HTTP 406. The client
  now sends it.

### Changed

- The runnable examples left this repository and now live at
  <https://www.volca.run/examples/>.

## [0.8.0] - 2026-07-14

### Added

- **Upload a database from Python.** `upload_database` streams an archive to a
  running engine, then `get_setup`, `set_data_path`, `finalize_database`, and
  `delete_database` drive it through the staged setup, so a notebook can now feed
  the engine and score against its own data without leaving Python.
- **Batch and sensitivity analysis** to match the MCP tools: `score_activities`
  scores many processes at once, `compute_sensitivity` varies an input and
  reports the swing. Both return typed results (`BatchScores`,
  `SensitivityResult`).
- **Method collections**: list, load, unload, delete, and upload the
  characterization-method sets the engine applies.
- **Reference data**: read and manage flow synonyms, compartment mappings, and
  units (selected by a validated `kind`), plus synonym groups and a CSV export.
- **Detail lookups**: `get_flow`, `get_flow_activities`, `get_method`,
  `get_method_factors`, `get_mapping_status`, and `get_stats` for inspecting a
  single flow, method, or the loaded databases.

The client now covers the engine's full HTTP surface, closing a gap where 25 of
roughly 60 endpoints were reachable.

## [0.7.2] - 2026-07-08

### Added

- `Client.search_activities` gains a `classification_match` argument
  (`MatchMode.CONTAINS` default, or `MatchMode.EXACT`). A classification filter
  can now require exact equality instead of substring: the match mode the
  engine and MCP already accept, finally reachable from the typed client.

## [0.7.1] - 2026-07-07

### Fixed

- `Server` no longer mistakes a directory named `volca` in the working tree for
  the engine binary. The lookup now checks for a *file*, so running a script
  from a source checkout (which has a `volca/` package directory) starts the
  downloaded binary instead of timing out on an unexecutable path.

## [0.7.0] - 2026-06-24

Co-product allocation shares are now visible from the typed client.

### Added

- `Activity.allocation_percent` / `Activity.allocation_formula`: a co-product's
  share of its parent activity's burden. They populate every
  `ActivityDetail.all_products` entry, so a multi-output process (e.g. a cheese
  that also yields whey, cream and permeate) shows how its impact is split across
  products. `None` for single-output processes.
- `ActivityDetail.allocation_percent`: the share of the process you fetched.

### Changed

- `ActivityDetail.is_allocated` now reads the structured shares on
  `all_products` instead of scraping the description text: more reliable, and it
  matches the factor the engine actually applied (the description's allocation
  comment is rounded and can drift, e.g. 52.62% in text vs 51.4% applied).
- `agribalyse.is_allocated` and `agribalyse.decompose` enumerate co-products
  from those structured shares the same way, keeping the description-text parse
  as a fallback for older Agribalyse databases without structured shares.

## [0.6.0] - 2026-06-21

Breaking: an activity's name fields are renamed for clarity. Needs the companion engine release (the wire keys changed too).

### Breaking changes

- `Activity.name` → `activity_name` and `Activity.product` → `product_name`, and the same on `ConsumerResult`, `SupplyChainEntry`, `PathStep`, and `ActivityDetail` (whose `reference_product` / `reference_product_amount` / `reference_product_unit` become `product_name` / `product_amount` / `product_unit`). A technosphere exchange's `target_activity` is now `target_activity_name`.
- Why: an activity can yield several products, so the *name* belongs to the activity (shared across its products) while the *product* is what tells them apart. The pair `name` / `product` invited mixing the two. A "process" is an `(activity, product)` pair, addressed by `process_id`; it has no name of its own (compose a label from `activity_name` + `product_name`).
- Migration: `.name` → `.activity_name`, `.product` → `.product_name`, `.reference_product` → `.product_name`, `.target_activity` → `.target_activity_name`.

## [0.5.1] - 2026-05-31

Two bug fixes for engine 0.7.0. No API change, just upgrade.

### Fixes

- Substitutions now reach the engine. The client was sending field names
  0.7.0 no longer recognised, so substitutions were silently ignored; it
  now sends the names the engine expects. (#108)
- Cross-database supply-chain edges now resolve. Each edge keeps its source
  and target database, so a process id that exists in more than one loaded
  database is routed to the right one. (#108)

## [0.5.0] - 2026-05-27

Three convergent themes: surface pagination truthfully (no silent
truncation), wire enums as enums, and type the high-traffic dicts. Every
public method now returns a typed dataclass, every paginated endpoint
exposes the envelope, and every enum-shaped string is an enum.

If you need a brand-new engine field that pyvolca doesn't model yet, use
the escape hatch ``c.call("operation_id", ...)``, which returns the raw JSON
dict.

### Breaking changes: pagination surfacing

- **`SupplyChain.has_more`** is a new derived property. When
  ``len(entries) < filtered_activities`` the server truncated the
  result; downstream LCA work on a truncated chain would be silently
  wrong. Callers should check this flag.
- **`get_characterization`** now returns a typed
  ``CharacterizationResult`` with ``matches`` / ``shown`` / ``has_more``
  derived. Previously returned a bare dict.
- **`SearchResults.from_raw`** is strict when a fetch callback is
  wired: missing wire keys (``total``, ``offset``, ``limit``,
  ``hasMore``) raise instead of silently defaulting to a truncated
  total. Test fixtures (``fetch=None``) keep permissive defaults.
- **`SearchResults.__iter__`** now raises ``RuntimeError`` when the
  server returns ``hasMore=True`` with no items. Previously it
  silently stopped, which let callers consume an incomplete result set
  without learning the engine's pagination contract was broken.

### Breaking changes: StrEnums (renamed string fields)

All stringly-typed enum-shaped fields are now ``str`` subclasses so
equality with the raw wire string still works, but typos fail at
construction.

- **`DatabaseStatus`**: ``DatabaseInfo.status`` is now
  ``DatabaseStatus.LOADED`` / ``UNLOADED`` / ``PARTIALLY_LINKED``.
- **`TechRole`** (was ``Literal``): ``TechnosphereExchange.role`` is
  now ``TechRole.INPUT`` / ``REFERENCE_PRODUCT`` / etc.
- **`BioDirection`** (was ``Literal``): ``BiosphereExchange.direction``
  is now ``BioDirection.RESOURCE`` / ``EMISSION``.
- **`AggregateScope`**: ``AggregateResult.scope`` and the
  ``aggregate(scope=)`` argument are now ``AggregateScope.DIRECT`` /
  ``SUPPLY_CHAIN`` / ``BIOSPHERE``. Raw strings still accepted on the
  argument for one-liners, but the return value is always the enum.
- **`AggregateOp`**: ``aggregate(aggregate=)`` is now
  ``AggregateOp.SUM_QUANTITY`` / ``COUNT`` / ``SHARE``.

### Breaking changes: typed list returns

Previously bare ``dict`` / ``list[dict]`` returns, now typed dataclasses.

- **`Client.list_methods`** → ``list[Method]`` (carries ``id``,
  ``name``, ``category``, ``unit``, ``factor_count``, ``collection``).
- **`Client.list_classifications`** → ``list[ClassificationSystem]``.
- **`Client.list_presets`** → ``list[Preset]`` with structured
  ``filters: list[PresetFilter]``.
- **`Client.get_version`** → ``ServerVersion``.
- **`Client.get_inventory`** → ``InventoryResult`` with ``root``,
  ``flows: list[InventoryFlow]``, ``statistics: InventoryStatistics``.
- **`Client.get_flow_mapping`** → ``FlowMapping`` with derived
  ``coverage_pct``.
- **`Client.get_contributing_flows`** → ``ContributingFlows``.
- **`Client.get_contributing_activities`** → ``ContributingActivities``.

Caveat: the engine does not report a total count for the contributing
endpoints, so pyvolca cannot derive ``has_more`` for them. Pass a
generous ``limit`` if exhaustive coverage matters.

### Breaking changes: typed substitutions

- **`Substitution`** is a new frozen dataclass replacing the
  ``{"from", "to", "consumer"}`` dict form. ``get_supply_chain``,
  ``get_inventory``, ``get_impacts``, and ``get_impacts_batch`` now
  accept ``list[Substitution]`` (preferred) or the legacy dict (for
  back-compat one-liners). The dict form validates the three required
  keys locally, so typos like ``"comsumer"`` raise before hitting the
  engine.

### Other notes

- **`Client.use(db_name)`** is now implemented via ``__dict__.copy()``
  so attributes added to ``__init__`` propagate automatically (no
  manual mirror to keep in sync).

## [0.4.0] - 2026-05-26

Engine wire format changed in three independent PRs since 0.3.1, and this
release realigns the client. **Any pyvolca ≤ 0.3.1 will raise `KeyError`
on most `get_activity` calls against a current engine.**

### Breaking changes

- **`TechnosphereExchange.is_input` / `is_reference` are now derived
  properties**, computed from the new `role: TechRole` field
  (`"ReferenceProduct" | "Coproduct" | "ReferenceInput" | "Input"`).
  Constructors take `role=` instead. (#73, #76)
- **`BiosphereExchange.is_input` is now a derived property**, computed
  from the new `direction: BioDirection` field
  (`"Resource" | "Emission"`). Constructors take `direction=` instead.
  The `flow_category` field is gone, and biosphere flows now carry a
  structured `compartment: Compartment | None` (medium + optional
  subcompartment). (#73, #76)
- **`TechnosphereExchange.flow_category` removed.** Product taxonomy
  lives on the producing activity's classifications, not on the exchange.
  (#73)
- **`ExchangeDetail.flow` is now a tagged sum**:
  `{"kind": "technosphere" | "biosphere" | "waste", "flow": <flow>}`.
  Hand-rolled callers that walked `ed["flow"]` flat must unwrap one level.
  (#76, #83)
- **`list_methods` returns `id` instead of `methodId`** on each method
  dict; the rename mirrors the engine's stripped JSON convention. (#73)
- **Download / install root moved** from `user_cache_dir("pyvolca")` to
  `user_data_dir("volca", appauthor=False)`, shared with the shell and
  PowerShell installers. Helpers `cached_binary` / `cached_data_dir` are
  renamed `installed_binary` / `installed_data_dir`. `$VOLCA_HOME`
  overrides the auto-detected root. (#21)

### Added

- **`WasteExchange`**: third top-level `Exchange` variant matching the
  engine's `WasteFlow`. Activities containing waste exchanges (typical in
  EcoSpold2, ILCD, and SimaPro `Final waste flows`) now parse without a
  `ValueError`. Carries `flow_name`, `amount`, `unit`, `is_input`,
  `target_activity`, `target_location`, `target_process_id`, `comment`,
  plus `is_waste = True` for duck-typing dispatch. Exported from
  `volca`. (#83)
- **`Compartment`** dataclass (frozen, hashable) and **`TechRole`** /
  `BioDirection` literal aliases exported from `volca`, so callers can
  match on roles / directions without redefining the type. (#73)
- **`installed_binary()` semver scan**: falls back to scanning
  `$VOLCA_HOME` for the highest `X.Y.Z` dir containing the binary when
  `latest.json` is absent. Lets `Server()` pick up engines installed via
  `install.sh` / `install.ps1`, which don't write a manifest. (#21)

### Changed

- Internal: `parse_exchange_detail` validates that the flow envelope's
  `kind` matches the inner `Exchange` tag and raises if they disagree,
  instead of silently dropping the discriminator. (#76)
- README rewritten to lead with the "hosted server vs local engine"
  choice and explain where artefacts are installed. (#21)

## [0.3.1] - 2026-04-29

### Added

- `download()` helper fetches the matching VoLCA engine binary + data
  bundle from GitHub Releases, verifies SHA-256, and extracts under the
  per-user cache. Idempotent + concurrency-safe (one fcntl/msvcrt lock
  per cache root). `Server()` picks up the cached binary automatically.
  (#7)

## [0.3.0] - 2026-04-24

### Added

- PyPI packaging metadata (`pyproject.toml`, classifiers, LICENSE,
  trusted publishing). (#3, #4)
- Dedicated CI workflow for the pyvolca package.

## [0.2.0] - earlier

### Added

- `VoLCAError` with clear diagnostics for non-JSON responses.
- Typed dataclasses for the API surface (`Activity`, `ActivityDetail`,
  `SupplyChain`, `LCIAResult`, `LCIABatchResult`, …).
- `compare_activities` helper.
- Agribalyse decompose helpers.
- `Server` child-process wrapper.
