# VoLCA

**VoLCA** is a Life Cycle Assessment engine that turns LCA databases into inspectable, queryable answers - fast.

It loads EcoSpold2, EcoSpold1, SimaPro CSV, ILCD process, and Brightway Excel databases, builds supply chain dependency trees, computes life cycle inventories using sparse matrix algebra, and applies characterization methods for impact assessment. Everything runs in-memory against your own data.

## What It Does

- **Browse** activities and flows across multiple databases
- **Explore** supply chain trees, force-directed dependency graphs, downstream consumers, shortest-path routing, and supply chain analysis by sector classification
- **Compute** life cycle inventories (LCI) and impact scores (LCIA) - single method or whole collection - with per-flow and per-activity contribution breakdowns
- **What-if substitutions** - swap an upstream activity (or a cross-database supplier) and recompute inventory and impacts in a single call
- **Normalize and weight** LCIA results with Raw / Normalized / Weighted view toggle; compute a single-score in Pt when normalization-weighting data is available
- **Map** method characterization factors to database flows with a 4-step cascade (UUID → name → synonym → CAS) and coverage statistics
- **Link** databases across nomenclatures (e.g., a sector database referencing Agribalyse)
- **Upload** databases and method collections via the API, without touching config files

---

## Key Features

- **Multiple database formats**: EcoSpold2 (.spold), EcoSpold1 (.xml), SimaPro CSV, ILCD process datasets, Brightway Excel (.xlsx)
- **Archive support**: Load databases directly from .zip, .7z, .gz, or .xz archives - no manual extraction
- **Cross-database linking**: Resolve supplier references across databases, with configurable dependencies and topological load ordering. EcoSpold2 inputs link to a loaded background by exact `activityLinkId` identity (so a partial import resolves against its matching release), falling back to attribute matching - flagged as approximate - when the background is a different release
- **Cross-DB what-if substitutions**: Swap an upstream activity at any depth - including suppliers in dependency databases - and recompute inventory and impacts through one endpoint
- **LCIA method collections**: Load ILCD method packages (ZIP or directory), SimaPro method CSV exports, openLCA JSON-LD impact categories, or tabular CSV from config; export any loaded collection back as SimaPro method CSV, columnar CSV (one column per impact category - the spreadsheet view), an openLCA JSON-LD zip, or an ILCD method package zip
- **Normalization and weighting**: Batch LCIA computes normalized and weighted scores per category and a single aggregated score (Pt) when NW data is present in the method collection
- **Contribution analysis**: Per-flow and per-activity contributions to any LCIA score, ranked by share
- **Flow mapping engine**: 4-step matching cascade (UUID → name → synonym → CAS) with per-strategy coverage statistics
- **Activity classifications**: ISIC, CPC, and category fields parsed from EcoSpold1/2 and ILCD, with named TOML presets for common filter bundles
- **Per-exchange comments**: Free-text exchange comments extracted from EcoSpold1/2, SimaPro, and ILCD and surfaced in API responses
- **Fuzzy search**: Trigram-based typo and stem tolerance on activity and supply-chain name filters
- **Auto-extracted synonyms**: Synonym pairs extracted automatically from loaded databases and method packages, available for toggling and download
- **Reference data management**: Flow synonyms, compartment mappings, and unit definitions can be configured in TOML, uploaded via the API, or toggled independently
- **Fast cache**: Per-database cache co-located with the source path, rebuilt when the cache schema changes or when the unit table or location aliases it was built with are no longer the ones in force (figures in the Performance table below)
- **Optional access control**: Single-code login with cookie-based session

---

## Performance

Measured on Ecoinvent 3.12 (26 533 activities). The figures are rounded hard on
purpose: what carries from one machine to another is the order of magnitude and
the ratio between two rows, never the number itself.

| Phase | Ecoinvent 3.12 |
|---|---|
| First load, reading the publisher's files | tens of seconds |
| Later loads, reading the cache written beside them | a second or two |
| First computation after a load, which pays the factorisation | seconds |
| Later computations (inventory, impact score) | a fraction of a second |
| Scoring every activity of the database under one method | minutes |

The factorisation is computed at the first computation, never at startup, and
kept for the lifetime of the server. The first request after a load therefore
pays it whichever way the database was read, from the publisher's files or from
the cache, and every request after that reuses it.

Scoring many activities goes through one multi-RHS solve rather than one solve
per activity, which is why a whole database is minutes rather than hours.

Memory is not a fixed cost per database. The runtime holds one allocation area
per core and returns memory to the system lazily, so the same database looks
heavier on a machine with many cores and memory to spare, and is collected
harder on a small one. What a loaded database has to hold is well under what
the process shows at its peak.

---

## Getting Started

### Web Server

```bash
./build.sh

# Start the server
volca --config volca.toml server --port 8080
# Open http://localhost:8080
```

### Command Line

The CLI is a lightweight HTTP client that connects to a running server (~0.2s per command). Start the server once, then query it freely:

```bash
# Start server (loads databases into memory once)
volca --config volca.toml server --port 8080

# In another terminal - all commands talk to the server via HTTP
volca --config volca.toml activities --name "electricity" --geo "FR"
volca --config volca.toml --db agribalyse inventory "12345678-..."
volca --config volca.toml --db agribalyse impacts "12345678-..." --method METHOD_UUID
```

### Interactive REPL

```bash
# Launch REPL (auto-starts the server if not running)
volca --config volca.toml repl

# Inside the REPL:
# volca> activities --name "wheat"
# volca> use agribalyse
# volca[agribalyse]> inventory UUID
# volca[agribalyse]> :format table
# volca[agribalyse]> impacts UUID --method METHOD_UUID
# volca[agribalyse]> :help
# volca[agribalyse]> :quit
```

---

## Configuration

A TOML config file enables multi-database setups, method collections, and reference data:

```toml
# Single-path reference data (all optional). Top-level keys stay above the
# first [section] header: below one they parse as members of that section,
# which startup reports by name.
# geographies = "data/geographies.csv"         # code,display_name,parents
# chem-synonyms = "data/chem_synonyms.csv"     # PubChem snapshot for the suggester
# substance-edges = "data/substance_edges.csv" # typed flow-correspondence edges

[server]
port = 8080
host = "127.0.0.1"             # interface to listen on; "0.0.0.0" answers the
                               # network over IPv4, "::" over IPv6
password = "mysecret"          # optional - omit to disable auth
name = "lab-archive"           # optional - how this server introduces itself over MCP

[[databases]]
name = "agribalyse-3.2"
displayName = "Agribalyse 3.2"
path = "DBs/AGB32_final.CSV"
description = "SimaPro CSV"
load = true

[[databases]]
name = "my-sector-db"
displayName = "Sector DB"
path = "DBs/sector.CSV"
depends = ["agribalyse-3.2"]   # loads agribalyse first, then links
load = true
default = false                # preselected database in the UI
deletable = false              # allow deletion via the API
geography_policy = "global"    # CF regionalization: exact | parent | global
allocation = "declared"        # how a multi-output block is divided:
                               # declared | dry mass | wet mass. Naming a mass
                               # recomputes the shares from it instead of
                               # reading the ones the source states; to have
                               # both, configure the same path twice, or ask
                               # for the second one at runtime with
                               # POST /db/{name}/derive/{newName}?allocation=
# locationAliases = { "FR" = "France" }   # per-database location renames

[[methods]]
name = "EF-3.1"
path = "DBs/EF-v3.1.zip"      # ILCD method package (ZIP or directory)

# SimaPro method CSV exports and tabular CSV are also accepted:
# path = "DBs/EF3.1_methods.csv"

# Optional scoring sets on a method collection: named variables per impact
# category, computed expressions, then normalization + weighting factors
# producing a single score (see "Normalization and weighting" above).
#   [[methods.scoring]]
#   name = "my-score"
#   unit = "Pt"
#   displayMultiplier = 1e6
#     [methods.scoring.variables]      # var = exact category name
#     cch = "Climate change"
#     [methods.scoring.computed]       # derived variables (expressions)
#     etf = "2 * etfo + etfi"
#     [methods.scoring.normalization]  # per-variable factors
#     cch = 7553.08
#     [methods.scoring.weighting]
#     cch = 0.2106
#
# global-methods = ["Water use", ...] de-regionalizes the named methods:
# their region-tagged CFs are dropped so the method's global (unlocated)
# CF is the single answer - for matching references that flattened
# spatial factors. A name matching no method logs a warning.
#
# Optional patches adjust matched characterization factors at load time.
# Selector fields combine with AND; at least one is required. Exactly one
# of `scale` (multiply) or `set-value` (replace) per patch. A patch that
# matches no factor logs a warning at load time.
#   [[methods.patches]]
#   description = "example: -40% on Uranium in Resource use, fossils"
#   match = { category = "Resource use, fossils", flow-name-prefix = "Uranium" }
#   # other selectors: flow-name (exact), cas, subcompartment-contains
#   scale = 0.6

# Reference data: the flow synonyms, compartment mapping, units and energy
# densities (and the geographies) are built into the engine. A kind this file
# says nothing about runs on the built-in table; a kind it lists is exactly
# what it lists. Name the built-in ("Default flow synonyms", "Default
# compartment mapping", "Default units", "Default energy densities") with a
# path to replace it with your own file, with no path to keep it beside your
# own, or with no path and `active = false` to switch it off.
[[flow-synonyms]]
name = "Default flow synonyms"
path = "my-flows.csv"          # CSV: name1,name2[,direction[,cas[,note]]]

[[compartment-mappings]]
name = "Default compartment mapping"
active = false

[[classification-presets]]     # named filter bundles for classifications
name = "agriculture"
label = "Agriculture"
filters = [{ system = "ISIC", value = "01", mode = "contains" }]  # mode: exact | contains

# [hosting] tunes upload/API limits when the engine runs behind a manager:
# max_uploads, max_upload_mb, max_loaded_uploads, api_access,
# upgrade_upload, upgrade_api, upgrade_vm_size, read_only, read_only_message
```

`max_uploads` bounds how many databases of their own a caller may keep, and
`max_loaded_uploads` how many of those may sit in memory at once. Both count
only uploaded databases - the ones the config declares are what an uploaded
inventory links against, so counting them would forbid the very thing
uploading is for. A copy spends the same budget as an upload. Negative means
unlimited; with no `[hosting]` section (local, CLI, desktop) neither applies.

`read_only = true` makes the instance answer every analysis request and refuse
every state change: loading and unloading, uploads, deletes, copies, relinks,
dependency edits - and `POST /api/v1/shutdown` and `/api/v1/idle-timeout/{n}`,
which decide how long the process lives. Refusals are `403` on REST and tool
errors on MCP; nothing is silently ignored. This is what makes a single
instance safe to put in front of many unrelated callers, none of whom should be
able to change the working set or end the server for the others. Every refusal
carries one sentence explaining the stance; `read_only_message` replaces it
with the operator's own words, and `GET /api/v1/hosting` reports both flags so
a client can say so before attempting a change.

The `depends` field ensures dependency databases load first and their flows are available for cross-database linking. Setting `load = true` on a database transitively loads all its dependencies.

A database's dependency set is **pinned**: it is seeded automatically when the database is first staged (the minimal set of supplier databases needed to resolve its links), and from then on it is authoritative. A plain `relink` re-resolves links *within* the pinned set only - it never silently adds another loaded database. Edit the pin explicitly with `add-dependency` / `remove-dependency`, then `finalize`; the new set is written to the matrix cache and reused on every later open. This is how you restrict a consumer (e.g. an inventory built against a single Agribalyse version) to exactly the supplier databases it should depend on, even while other versions stay loaded for other consumers. (The one exception is a *mapping* relink - `relink` with a `depDb` and an alias CSV - which pins that chosen dependency in-memory if it isn't already, so a `copy → delete → relink` pipeline composes in one pass; links to the other pinned dependencies are preserved, not dropped.)

---

## REST API

All per-database resources are scoped under `/api/v1/db/{dbName}/`. POST variants of the analysis endpoints accept a `SubstitutionRequest` body for what-if scenarios:

```
# Activity inspection
GET    /api/v1/db/{dbName}/activity/{processId}                          Activity details
GET    /api/v1/db/{dbName}/activity/{processId}/flows                    Exchanges as flow summaries
GET    /api/v1/db/{dbName}/activity/{processId}/inputs                   Input exchanges
GET    /api/v1/db/{dbName}/activity/{processId}/outputs                  Output exchanges
GET    /api/v1/db/{dbName}/activity/{processId}/reference-product        Reference product detail

# Supply chain
GET    /api/v1/db/{dbName}/activity/{processId}/tree                     Supply chain tree
GET    /api/v1/db/{dbName}/activity/{processId}/graph?cutoff=            Force-directed graph
GET    /api/v1/db/{dbName}/activity/{processId}/supply-chain             Flat supply-chain table (filters, sort, paging)
POST   /api/v1/db/{dbName}/activity/{processId}/supply-chain             Same, with substitutions applied
GET    /api/v1/db/{dbName}/activity/{processId}/consumers                Downstream activities consuming this one
GET    /api/v1/db/{dbName}/activity/{processId}/path-to?target=          Shortest supply-chain path to a target activity
GET    /api/v1/db/{dbName}/activity/{processId}/aggregate                SQL-style group/filter on exchanges, supply chain, biosphere, or consumption edges

# Inventory and impacts
GET    /api/v1/db/{dbName}/activity/{processId}/inventory                Life cycle inventory (LCI)
POST   /api/v1/db/{dbName}/activity/{processId}/inventory                LCI with substitutions
GET    /api/v1/db/{dbName}/activity/{processId}/impacts/{collection}     Batch LCIA across a method collection (NW + single-score when available)
POST   /api/v1/db/{dbName}/activity/{processId}/impacts/{collection}     Batch LCIA with substitutions
GET    /api/v1/db/{dbName}/activity/{processId}/impacts/{collection}/{methodId}   LCIA score for one method (with top-flows)
POST   /api/v1/db/{dbName}/activity/{processId}/impacts/{collection}/{methodId}   Same, with substitutions
GET    /api/v1/db/{dbName}/activity/{processId}/contributing-flows/{collection}/{methodId}        Top biosphere flows by score share
GET    /api/v1/db/{dbName}/activity/{processId}/contributing-activities/{collection}/{methodId}   Top upstream activities by score share
POST   /api/v1/db/{dbName}/impacts/{collection}                          Batch-impacts for many activities (multi-RHS solve)

# Search and reference data
GET    /api/v1/db/{dbName}/activities?name=&geo=&product=&preset=&classification=&sort=  Search activities
GET    /api/v1/db/{dbName}/flows?q=&lang=&kind=                          Search flows
GET    /api/v1/db/{dbName}/flow/{flowId}                                 Flow details
GET    /api/v1/db/{dbName}/flow/{flowId}/activities?role=                Activities on one side of a flow (producer, consumer, any)
GET    /api/v1/db/{dbName}/classifications                               Classification systems available in this DB
GET    /api/v1/db/{dbName}/method/{methodId}/mapping                     Mapping coverage stats
GET    /api/v1/db/{dbName}/method/{methodId}/flow-mapping                Per-flow mapping detail
GET    /api/v1/db/{dbName}/method/{methodId}/characterization?flow=      Characterization factors for one DB flow

# Database management
GET    /api/v1/db                                                        List databases (status + dependencies)
POST   /api/v1/db/upload                                                 Upload a database archive
POST   /api/v1/db/{dbName}/load                                          Load a configured database
POST   /api/v1/db/{dbName}/unload                                        Unload (keep config, free memory)
POST   /api/v1/db/{dbName}/derive/{newName}?allocation=                  Read the same sources under another allocation key
POST   /api/v1/db/{dbName}/relink                                        Re-resolve cross-DB links (optional JSON body: depDb + mappingCsv for an alias relink)
GET    /api/v1/db/{dbName}/gap-report                                    Supplier-gap report (what is still unsupplied after linking)
GET    /api/v1/db/{dbName}/quality-report                                Dataset-soundness report (what is malformed in the database)
GET    /api/v1/db/{dbName}/quality-report.csv                            The same report as a downloadable file
GET    /api/v1/db/{dbName}/computed-quality-report                       Computed checks (what the database computes, judged against its own norms)
GET    /api/v1/db/{dbName}/computed-quality-report.csv                   The same report as a downloadable file
POST   /api/v1/db/{dbName}/finalize                                      Finalize cross-DB linking
DELETE /api/v1/db/{dbName}                                               Delete a database
GET    /api/v1/db/{dbName}/setup                                         Setup info (path, dependencies)
POST   /api/v1/db/{dbName}/add-dependency/{depName}                      Add a dep
POST   /api/v1/db/{dbName}/remove-dependency/{depName}                   Remove a dep
POST   /api/v1/db/{dbName}/set-data-path                                 Repoint the source path

# Methods and method collections
GET    /api/v1/methods                                                   List individual methods (flattened)
GET    /api/v1/method/{methodId}                                         Method details
GET    /api/v1/method/{methodId}/factors                                 Characterization factors
GET    /api/v1/method-collections                                        List method collections
POST   /api/v1/method-collections/{name}/load                            Load a method collection
POST   /api/v1/method-collections/{name}/unload                          Unload a method collection
POST   /api/v1/method-collections/upload                                 Upload a method package
DELETE /api/v1/method-collections/{name}                                 Delete a method collection

# Reference data (synonyms / compartments / units share the same shape)
GET    /api/v1/{flow-synonyms|compartment-mappings|units}                List
POST   /api/v1/{flow-synonyms|compartment-mappings|units}/{name}/load    Activate
POST   /api/v1/{flow-synonyms|compartment-mappings|units}/{name}/unload  Deactivate
POST   /api/v1/{flow-synonyms|compartment-mappings|units}/upload         Upload
DELETE /api/v1/{flow-synonyms|compartment-mappings|units}/{name}         Delete
GET    /api/v1/flow-synonyms/{name}/groups                               Browse synonym groups
GET    /api/v1/flow-synonyms/{name}/download                             Download synonym CSV

# Server
GET    /api/v1/classification-presets                                    Named classification filter bundles (from TOML)
GET    /api/v1/version                                                   Server version
GET    /api/v1/stats                                                     Runtime stats (memory)
GET    /api/v1/hosting                                                   Hosting config (managed instances)
GET    /api/v1/logs?since=                                               Server logs
POST   /api/v1/auth                                                      Login (returns session cookie)
```

Per-exchange data on inventory and impact responses includes `exComment` - the free-text comment (`generalComment` / `<comment>`) attached to each exchange in the source dataset, when present.

The `impacts/{collection}` response includes per-category `normalizedScore` and `weightedScore` fields (when normalization-weighting data is present in the method collection), plus a `singleScore` sum in Pt.

### OpenAPI spec

The full OpenAPI 3.0 specification is served at runtime:

- **`GET /api/v1/openapi.json`** - machine-readable spec (for code generation, tooling)
- **`GET /api/v1/docs`** - Swagger UI (interactive browser)

Use these to build your own frontend, generate a typed client, or explore the API interactively.

---

## MCP Server

VoLCA exposes an [MCP (Model Context Protocol)](https://modelcontextprotocol.io/) endpoint at `POST /mcp`, making LCA data queryable by AI assistants (Claude, Cursor, etc.) natively.

Configure it in your MCP client:

```json
{
  "mcpServers": {
    "volca": {
      "url": "http://localhost:8080/mcp"
    }
  }
}
```

Available tools - auto-derived at runtime from the single resource registry (`src/API/Resources.hs`) shared with the REST API and OpenAPI spec, so the three *served* surfaces never drift. This table is a hand-written copy; `volca dump-mcp-tools` prints the authoritative list:

| Tool | Description |
|------|-------------|
| `list_databases` | List loaded databases (with dependencies) |
| `list_presets` | List named classification filter presets |
| `list_geographies` | List geographies present in a database |
| `list_classifications` | List classification systems and values for a database |
| `list_methods` | List loaded impact assessment methods |
| `search_activities` | Search by name or source identifier, geography, product, classification, or preset |
| `search_flows` | Search flows, filtered by kind |
| `get_activity` | Activity details and exchanges (with comments) |
| `aggregate` | SQL-style group/filter on exchanges, supply chain, biosphere, or consumption edges |
| `get_supply_chain` | Flat upstream activity list with quantities and filters |
| `get_consumers` | Downstream activities that consume a given activity |
| `get_path_to` | Shortest supply-chain path from one activity to another |
| `get_inventory` | LCI biosphere flows (top N by quantity) |
| `get_impacts` | LCIA score for an activity and method (accepts substitutions) |
| `score_activity` | Full LCIA panel + every configured scoring set for one activity (replaces N×get_impacts calls) |
| `score_activities` | Same shape as score_activity, batched over N activities in one multi-RHS solve |
| `list_scoring_sets` | List formula-based scoring sets configured on every loaded method collection |
| `get_contributing_flows` | Top biosphere flows contributing to an LCIA score |
| `get_contributing_activities` | Top upstream activities contributing to an LCIA score |
| `get_flow_mapping` | CF-to-flow mapping coverage for a method |
| `get_characterization` | Characterization factors for a flow under a method |

Authentication uses the same password as the REST API.

---

## CLI Commands

### Global Options

| Option | Description |
|--------|-------------|
| `--config FILE` | TOML config file for multi-database setup (optional for `server` and `stop`, which run on built-in defaults without it) |
| `--url URL` | Server URL (default: from config; or set `VOLCA_URL`) |
| `--password PWD` | Server password (or set `VOLCA_PASSWORD`) |
| `--db NAME` | Database name to query |
| `--format FORMAT` | Output format: `pretty` (default), `json`, `table`, `csv` |
| `--jsonpath PATH` | Field holding the array to flatten for CSV output (e.g. `results`, `activity.exchanges`); only needed when a response carries several arrays |
| `--no-cache` | Disable caching (for development) |

### Modes of Operation

```bash
# Start server (loads databases - run once)
volca --config volca.toml server --port 8080

# Single HTTP command (connects to running server, ~0.2s)
volca --config volca.toml [--db NAME] COMMAND [OPTIONS]

# Interactive REPL (auto-starts server if not running)
volca --config volca.toml repl
```

### Search

```bash
volca activities --name "electricity" --geo "DE" --limit 10
volca activities --product "steel" --limit 10 --offset 20
volca flows --query "carbon dioxide" --limit 5
```

### Analysis

```bash
# Activity details
volca activity "12345678-..."

# Life cycle inventory
volca inventory "12345678-..."

# Impact assessment (--method takes a method UUID, not a file path)
volca impacts "12345678-..." --method METHOD_UUID

# Matrix export (Ecoinvent universal format - runs locally, not via HTTP)
volca export-matrices ./output_dir
```

The `tree`, `supply-chain`, `consumers`, `path-to`, `aggregate`, and contribution
endpoints are reachable via the REST API and MCP. Use `curl` or the OpenAPI spec
at `/api/v1/docs` to drive them while the CLI focuses on the most common
commands.

### Flow Mapping Diagnostics

```bash
# Summary: how well does a method match a database?
volca --db agribalyse mapping METHOD_UUID

# See every mapped CF with its match strategy (uuid/name/synonym/cas)
volca --db agribalyse mapping METHOD_UUID --matched

# List CFs that found no DB flow
volca --db agribalyse mapping METHOD_UUID --unmatched

# List DB biosphere flows with no CF
volca --db agribalyse mapping METHOD_UUID --uncharacterized

# Machine-readable output
volca --db agribalyse mapping METHOD_UUID --matched --format json
```

### Quality Reports

The engine renders these two reports as CSV itself, so `--format csv` writes a
file rather than a JSON dump. Load a database in one command, take its report
in the next:

```bash
volca --config volca.toml database load agribalyse
volca --config volca.toml --db agribalyse --format csv quality-report > quality.csv

# What the database computes, judged against its own norms (needs a method collection)
volca --config volca.toml --db agribalyse --format csv computed-quality-report --collection EF31 > computed.csv

# Worst findings only, as JSON
volca --config volca.toml --db agribalyse quality-report --limit 20
```

The same file over plain HTTP, named after the database it describes:

```bash
curl -OJ http://localhost:8080/api/v1/db/agribalyse/quality-report.csv
```

### Database and Method Management

```bash
# List, upload, delete databases
volca database                                  # list (default)
volca database upload mydb.7z --name "My DB"    # upload
volca database delete my-db                     # delete

# Edit and export loaded databases
volca database copy my-db my-db-v2              # copy under a new name
volca database relink my-db --to bg-db --mapping aliases.csv
volca database delete-activities my-db --name "electricity"  # delete filtered set
volca database delete-activities my-db --id UUID_UUID --id UUID_UUID  # delete exactly these
volca database export my-db --format simapro --out out.csv
#   formats: simapro | ecospold1 | ecospold2 | ilcd | brightway

# List, upload, export, delete method collections
volca method                                    # list (default)
volca method upload EF-3.1.zip --name "EF 3.1"  # upload
volca method export ef-31 --format simapro --out ef-31.csv  # export (formats: simapro | csv | openlca | ilcd)
volca method delete ef-31                        # delete
```

---

## API and CLI Feature Matrix

| Feature | REST API | CLI |
|---------|----------|-----|
| **Search** | | |
| Search activities | `GET /db/{db}/activities?name=&geo=&product=&preset=&classification=` | `activities --name --geo --product` |
| Search flows | `GET /db/{db}/flows?q=&lang=&kind=` | `flows --query --lang` |
| Classifications | `GET /db/{db}/classifications` | - |
| Classification presets | `GET /classification-presets` | - |
| **Analysis** | | |
| Activity details | `GET /db/{db}/activity/{id}` | `activity ID` |
| Supply chain tree | `GET /db/{db}/activity/{id}/tree` | - |
| Supply chain (flat) | `GET\|POST /db/{db}/activity/{id}/supply-chain` | - |
| Supply chain graph | `GET /db/{db}/activity/{id}/graph?cutoff=` | - |
| Downstream consumers | `GET /db/{db}/activity/{id}/consumers` | - |
| Path to target | `GET /db/{db}/activity/{id}/path-to?target=` | - |
| Aggregate | `GET /db/{db}/activity/{id}/aggregate` | - |
| Life cycle inventory | `GET\|POST /db/{db}/activity/{id}/inventory` | `inventory ID` |
| LCIA batch (collection) | `GET\|POST /db/{db}/activity/{id}/impacts/{collection}` | - |
| LCIA single method | `GET\|POST /db/{db}/activity/{id}/impacts/{collection}/{methodId}` | `impacts ID --method METHOD_UUID` |
| LCIA batch over many activities | `POST /db/{db}/impacts/{collection}` | - |
| Contributing flows | `GET /db/{db}/activity/{id}/contributing-flows/{collection}/{methodId}` | - |
| Contributing activities | `GET /db/{db}/activity/{id}/contributing-activities/{collection}/{methodId}` | - |
| Flow details | `GET /db/{db}/flow/{flowId}` | `flow FLOW_ID` |
| Flow activities | `GET /db/{db}/flow/{flowId}/activities` | `flow FLOW_ID activities` |
| **Flow Mapping** | | |
| Mapping coverage | `GET /db/{db}/method/{id}/mapping` | `flow-mapping METHOD_UUID` |
| Per-flow mapping | `GET /db/{db}/method/{id}/flow-mapping` | `flow-mapping METHOD_UUID --matched` |
| Characterization for flow | `GET /db/{db}/method/{id}/characterization?flow=` | - |
| Unmatched CFs | included in mapping response | `flow-mapping METHOD_UUID --unmatched` |
| Uncharacterized flows | - | `flow-mapping METHOD_UUID --uncharacterized` |
| **Quality** | | |
| Dataset soundness | `GET /db/{db}/quality-report[.csv]` | `quality-report [--limit N]` |
| Computed checks | `GET /db/{db}/computed-quality-report[.csv]` | `computed-quality-report [--collection NAME]` |
| **Database Management** | | |
| List databases | `GET /db` | `database` |
| Upload database | `POST /db/upload` | `database upload FILE --name NAME` |
| Load / unload | `POST /db/{name}/(load\|unload)` | - (use config `load = true`) |
| Relink / finalize | `POST /db/{name}/(relink\|finalize)` | `database relink DB --to DEP --mapping CSV` |
| Setup / dependencies | `GET /db/{name}/setup`, `POST .../{add,remove}-dependency/{dep}`, `POST .../set-data-path` | - |
| Copy database | `POST /db/{name}/copy/{newName}` | `database copy SRC NEW_NAME` |
| Re-key database | `POST /db/{name}/derive/{newName}?allocation=` | - |
| Delete activities (by filter or ids) | `POST /db/{name}/delete` | `database delete-activities DB [filters\|--id …]` |
| Export database | `POST /db/{name}/export` | `database export DB --format FMT --out FILE` |
| Delete database | `DELETE /db/{name}` | `database delete NAME` |
| **Method Management** | | |
| List methods | `GET /methods` | `methods` |
| Method details | `GET /method/{id}` | - |
| Method factors | `GET /method/{id}/factors` | - |
| List collections | `GET /method-collections` | `method` |
| Upload collection | `POST /method-collections/upload` | `method upload FILE --name NAME` |
| Load / unload collection | `POST /method-collections/{name}/(load\|unload)` | - |
| Delete collection | `DELETE /method-collections/{name}` | `method delete NAME` |
| Export collection (SimaPro CSV, columnar CSV, openLCA JSON-LD, or ILCD method package) | `POST /method-collections/{name}/export` | `method export NAME --format simapro --out FILE` |
| **Reference Data** | | |
| Flow synonyms | `GET /flow-synonyms` (+ load/unload/upload/delete/groups/download) | `synonyms` |
| Compartment mappings | `GET /compartment-mappings` (+ load/unload/upload/delete) | `compartment-mappings` |
| Units | `GET /units` (+ load/unload/upload/delete) | `units` |
| **Matrix Export** | | |
| Universal format | - | `export-matrices DIR` (local only) |
| Debug matrices | - | `debug-matrices ID --output FILE` (local only) |
| **Server** | | |
| Version / stats / hosting / logs | `GET /version`, `/stats`, `/hosting`, `/logs?since=` | - |
| Stop server | - | `stop` |
| REPL | - | `repl` |
| Login | `POST /auth` | - |

All API routes are prefixed with `/api/v1/`. A dash (-) means the feature is only available in one interface.

---


## Building

### Linux

Install dependencies, then build. MUMPS is built from source by `build-mumps.sh` if no system package is found:

```bash
# Debian/Ubuntu
sudo apt install build-essential gfortran python3 curl zlib1g-dev libblas-dev liblapack-dev upx-ucl

# Fedora
sudo dnf install gcc gcc-c++ gcc-gfortran make python3 curl zlib-devel blas-devel lapack-devel

# Arch Linux
sudo pacman -S base-devel gcc-fortran python curl zlib blas lapack
```

Install the [Haskell toolchain via GHCup](https://www.haskell.org/ghcup/), then:

```bash
./build.sh              # Build
./build.sh --test       # Build and run tests
```

`build.sh` also accepts:

- `--clean` / `--all` - discard `dist-newstyle/` before building
- `--coverage` - run tests with coverage and emit an HTML report
- `--static` - produce a statically-linked binary (Linux only)
- `--no-optimize` - skip `strip` and UPX. Use this when downstream
  tooling needs to rewrite the binary's dynamic load commands
  (`dylibbundler`, `install_name_tool` for the macOS `.app`).

A build that ships splits every object into one ELF section per top-level
symbol so the executable can be pruned to size, and that link is the one the
default `ld.bfd` is slowest at: about 22 seconds, where every alternative
takes one or two. Below `-O2` the split is off, since nothing prunes a test
suite and the sections would be pure cost, and the link is a few seconds
whatever the linker. Changing linker changes the link alone, so it costs
nothing on an already compiled tree:

```bash
cabal build exe:volca --ghc-options=-optl-fuse-ld=lld
```

`-fuse-ld=` looks for a plain `ld.lld`, `ld.mold` or `ld.gold` on the
`PATH`, and will not find a versioned `ld.lld-21`.

### macOS (Apple Silicon)

Tested on macOS 13 Ventura and later, arm64 only. The build pins
`MACOSX_DEPLOYMENT_TARGET=13.0` so binaries produced on a recent macOS still
load on Ventura.

```bash
# Xcode Command Line Tools (provides clang, ld64, the macOS SDK)
xcode-select --install

# Homebrew
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
eval "$(/opt/homebrew/bin/brew shellenv)"

# Native deps. gcc brings gfortran + libgfortran/libquadmath; openblas
# provides BLAS/LAPACK (Accelerate.framework's LAPACK ABI does not match
# what MUMPS emits). dylibbundler is needed only for the Tauri desktop bundle.
brew install ghcup gcc openblas python3 curl node rust elm upx

# Haskell toolchain
ghcup install ghc 9.12.4 --set
ghcup install cabal latest
source ~/.ghcup/env

./build.sh              # Build (compiles MUMPS from source, ~3 min one-time)
./build.sh --test       # Build and run tests
```

MUMPS sources are pinned via `MUMPS_VERSION` in `versions.env` and built
into `deps/mumps/` (cached across runs).

### Windows (MSYS2)

1. Install [MSYS2](https://www.msys2.org/) and open the "MSYS2 UCRT64" terminal
2. Install dependencies:
   ```bash
   pacman -S \
     mingw-w64-ucrt-x86_64-gcc \
     mingw-w64-ucrt-x86_64-gcc-fortran \
     mingw-w64-ucrt-x86_64-openblas \
     mingw-w64-ucrt-x86_64-cmake \
     mingw-w64-ucrt-x86_64-make \
     mingw-w64-ucrt-x86_64-python \
     mingw-w64-ucrt-x86_64-upx \
     make python git curl tar
   ```
3. Install [GHCup](https://www.haskell.org/ghcup/) for the compiler toolchain
4. Run:
   ```bash
   ./build.sh            # Same script as Linux/macOS - builds MUMPS from source
   ```

### Docker

```bash
docker build -f docker/Dockerfile -t volca .
docker run -p 8080:8080 -v /path/to/data:/data volca
```

---

## Testing

```bash
./build.sh --test

# One spec group, in a tree that script has already built
cabal test lca-tests --test-options="--match /Inventory/"
```

`./build.sh --test` is the way in: it generates `src/Version.hs`, compiles at
`-O0`, builds the `volca` executable and names it to the specs that start a
server. A bare `cabal test` from a fresh clone does none of those: it stops on
the missing `src/Version.hs`, and once that is generated the specs that want the
executable stop too. `AGENTS.md` says the rest.

Tests cover matrix construction (sign convention), inventory calculation (golden values), parsers (EcoSpold1/2, ILCD, SimaPro, Brightway Excel, classification fields), and matrix export format compliance.

---

## License & third-party software

VoLCA is licensed under the **Apache License 2.0** - see [LICENSE](LICENSE).

Third-party components bundled with or linked into VoLCA are inventoried in
[NOTICE](NOTICE) and [THIRD_PARTY_LICENSES.md](THIRD_PARTY_LICENSES.md). The
notable ones are MUMPS (CeCILL-C, version pinned in `versions.env`),
BLAS/LAPACK (BSD-3), and a number of
Haskell libraries (predominantly BSD-3 and MIT).

A running engine also exposes the same inventory as JSON at
`/api/v1/licenses` so any client can render it.
