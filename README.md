# VoLCA

**VoLCA** is a Life Cycle Assessment engine that turns LCA databases into inspectable, queryable answers — fast.

It loads EcoSpold2, EcoSpold1, SimaPro CSV, and ILCD process databases, builds supply chain dependency trees, computes life cycle inventories using sparse matrix algebra, and applies characterization methods for impact assessment. Everything runs in-memory against your own data.

## What It Does

- **Browse** activities and flows across multiple databases
- **Explore** supply chain trees, force-directed dependency graphs, downstream consumers, shortest-path routing, and supply chain analysis by sector classification
- **Compute** life cycle inventories (LCI) and impact scores (LCIA) — single method or whole collection — with per-flow and per-activity contribution breakdowns
- **What-if substitutions** — swap an upstream activity (or a cross-database supplier) and recompute inventory and impacts in a single call
- **Normalize and weight** LCIA results with Raw / Normalized / Weighted view toggle; compute a single-score in Pt when normalization-weighting data is available
- **Map** method characterization factors to database flows with a 4-step cascade (UUID → CAS → name → synonym) and coverage statistics
- **Link** databases across nomenclatures (e.g., a sector database referencing Agribalyse)
- **Upload** databases and method collections via the API, without touching config files

---

## Key Features

- **Multiple database formats**: EcoSpold2 (.spold), EcoSpold1 (.xml), SimaPro CSV, ILCD process datasets
- **Archive support**: Load databases directly from .zip, .7z, .gz, or .xz archives — no manual extraction
- **Cross-database linking**: Resolve supplier references across databases, with configurable dependencies and topological load ordering
- **Cross-DB what-if substitutions**: Swap an upstream activity at any depth — including suppliers in dependency databases — and recompute inventory and impacts through one endpoint
- **LCIA method collections**: Load ILCD method packages (ZIP or directory), SimaPro method CSV exports, or tabular CSV from config
- **Normalization and weighting**: Batch LCIA computes normalized and weighted scores per category and a single aggregated score (Pt) when NW data is present in the method collection
- **Contribution analysis**: Per-flow and per-activity contributions to any LCIA score, ranked by share
- **Flow mapping engine**: 4-step matching cascade (UUID → CAS → name → synonym) with per-strategy coverage statistics
- **Activity classifications**: ISIC, CPC, and category fields parsed from EcoSpold1/2 and ILCD, with named TOML presets for common filter bundles
- **Per-exchange comments**: Free-text exchange comments extracted from EcoSpold1/2, SimaPro, and ILCD and surfaced in API responses
- **Fuzzy search**: Trigram-based typo and stem tolerance on activity and supply-chain name filters
- **Auto-extracted synonyms**: Synonym pairs extracted automatically from loaded databases and method packages, available for toggling and download
- **Reference data management**: Flow synonyms, compartment mappings, and unit definitions can be configured in TOML, uploaded via the API, or toggled independently
- **Fast cache**: Per-database cache co-located with the source path, with automatic schema-based invalidation; large databases (26k activities) load in ~47s cold, ~3.4s cached
- **Optional access control**: Single-code login with cookie-based session

---

## Performance

All figures measured on Ecoinvent 3.12 (26 533 activities) on a 4-core machine.

| Phase | Cold start | Hot start |
|---|---|---|
| Startup (read all 26 533 EcoSpold files from disk) | ~50 s | — |
| Hot startup — load from cache | — | ~3.7 s |
| First computation after startup (inventory, impact score)¹ | ~90 ms | ~8.5 s |
| Next computations (inventory, impact score) | ~85 ms | ~85 ms |
| Batch of 200 computations | ~19 s total (10/sec) | ~19 s total (10/sec) |
| Computation of a modified process (upstream process substitution) | ~120 ms | ~110 ms |

¹ The matrix factorisation is deferred to the first computation and cached for the lifetime of the server. All subsequent requests reuse it.

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

# In another terminal — all commands talk to the server via HTTP
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
[server]
port = 8080
host = "127.0.0.1"
password = "mysecret"          # optional — omit to disable auth

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

[[methods]]
name = "EF-3.1"
path = "DBs/EF-v3.1.zip"      # ILCD method package (ZIP or directory)

# SimaPro method CSV exports and tabular CSV are also accepted:
# path = "DBs/EF3.1_methods.csv"

[[flow-synonyms]]
name = "Default flow synonyms"
path = "data/flows.csv"        # CSV with two columns: name1,name2
active = true

[[compartment-mappings]]
name = "Default compartment mapping"
path = "data/compartments.csv"
active = true

[[units]]
name = "Default units"
path = "data/units.csv"
active = true
```

The `depends` field ensures dependency databases load first and their flows are available for cross-database linking. Setting `load = true` on a database transitively loads all its dependencies.

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
GET    /api/v1/db/{dbName}/activity/{processId}/aggregate                SQL-style group/filter on exchanges, supply chain, or biosphere
GET    /api/v1/db/{dbName}/activity/{processId}/analyze/{analyzerName}   Run a registered plugin analyzer

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
GET    /api/v1/db/{dbName}/flows?q=&lang=                                Search flows
GET    /api/v1/db/{dbName}/flow/{flowId}                                 Flow details
GET    /api/v1/db/{dbName}/flow/{flowId}/activities                      Activities using a flow
GET    /api/v1/db/{dbName}/classifications                               Classification systems available in this DB
GET    /api/v1/db/{dbName}/method/{methodId}/mapping                     Mapping coverage stats
GET    /api/v1/db/{dbName}/method/{methodId}/flow-mapping                Per-flow mapping detail
GET    /api/v1/db/{dbName}/method/{methodId}/characterization?flow=      Characterization factors for one DB flow

# Database management
GET    /api/v1/db                                                        List databases (status + dependencies)
POST   /api/v1/db/upload                                                 Upload a database archive
POST   /api/v1/db/{dbName}/load                                          Load a configured database
POST   /api/v1/db/{dbName}/unload                                        Unload (keep config, free memory)
POST   /api/v1/db/{dbName}/relink                                        Re-resolve cross-DB links
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

Per-exchange data on inventory and impact responses includes `exComment` — the free-text comment (`generalComment` / `<comment>`) attached to each exchange in the source dataset, when present.

The `impacts/{collection}` response includes per-category `normalizedScore` and `weightedScore` fields (when normalization-weighting data is present in the method collection), plus a `singleScore` sum in Pt.

### OpenAPI spec

The full OpenAPI 3.0 specification is served at runtime:

- **`GET /api/v1/openapi.json`** — machine-readable spec (for code generation, tooling)
- **`GET /api/v1/docs`** — Swagger UI (interactive browser)

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

Available tools (auto-derived from a single resource registry shared with the REST API and OpenAPI spec, so the three surfaces never drift):

| Tool | Description |
|------|-------------|
| `list_databases` | List loaded databases (with dependencies) |
| `list_presets` | List named classification filter presets |
| `list_geographies` | List geographies present in a database |
| `list_classifications` | List classification systems and values for a database |
| `list_methods` | List loaded impact assessment methods |
| `search_activities` | Search by name, geography, product, classification, or preset |
| `search_flows` | Search biosphere flows |
| `get_activity` | Activity details and exchanges (with comments) |
| `aggregate` | SQL-style group/filter on exchanges, supply chain, or biosphere |
| `get_supply_chain` | Flat upstream activity list with quantities and filters |
| `get_consumers` | Downstream activities that consume a given activity |
| `get_path_to` | Shortest supply-chain path from one activity to another |
| `get_inventory` | LCI biosphere flows (top N by quantity) |
| `get_impacts` | LCIA score for an activity and method (accepts substitutions) |
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
| `--config FILE` | TOML config file for multi-database setup |
| `--url URL` | Server URL (default: from config; or set `VOLCA_URL`) |
| `--password PWD` | Server password (or set `VOLCA_PASSWORD`) |
| `--db NAME` | Database name to query |
| `--format FORMAT` | Output format: `pretty` (default), `json`, `table`, `csv` |
| `--jsonpath PATH` | Field to extract for CSV output (e.g., `srResults`) |
| `--no-cache` | Disable caching (for development) |

### Modes of Operation

```bash
# Start server (loads databases — run once)
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

# Matrix export (Ecoinvent universal format — runs locally, not via HTTP)
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

# See every mapped CF with its match strategy (uuid/cas/name/synonym)
volca --db agribalyse mapping METHOD_UUID --matched

# List CFs that found no DB flow
volca --db agribalyse mapping METHOD_UUID --unmatched

# List DB biosphere flows with no CF
volca --db agribalyse mapping METHOD_UUID --uncharacterized

# Machine-readable output
volca --db agribalyse mapping METHOD_UUID --matched --format json
```

### Database and Method Management

```bash
# List, upload, delete databases
volca database                                  # list (default)
volca database upload mydb.7z --name "My DB"    # upload
volca database delete my-db                     # delete

# List, upload, delete method collections
volca method                                    # list (default)
volca method upload EF-3.1.zip --name "EF 3.1"  # upload
volca method delete ef-31                        # delete
```

---

## API and CLI Feature Matrix

| Feature | REST API | CLI |
|---------|----------|-----|
| **Search** | | |
| Search activities | `GET /db/{db}/activities?name=&geo=&product=&preset=&classification=` | `activities --name --geo --product` |
| Search flows | `GET /db/{db}/flows?q=&lang=` | `flows --query --lang` |
| Classifications | `GET /db/{db}/classifications` | — |
| Classification presets | `GET /classification-presets` | — |
| **Analysis** | | |
| Activity details | `GET /db/{db}/activity/{id}` | `activity ID` |
| Supply chain tree | `GET /db/{db}/activity/{id}/tree` | — |
| Supply chain (flat) | `GET\|POST /db/{db}/activity/{id}/supply-chain` | — |
| Supply chain graph | `GET /db/{db}/activity/{id}/graph?cutoff=` | — |
| Downstream consumers | `GET /db/{db}/activity/{id}/consumers` | — |
| Path to target | `GET /db/{db}/activity/{id}/path-to?target=` | — |
| Aggregate | `GET /db/{db}/activity/{id}/aggregate` | — |
| Life cycle inventory | `GET\|POST /db/{db}/activity/{id}/inventory` | `inventory ID` |
| LCIA batch (collection) | `GET\|POST /db/{db}/activity/{id}/impacts/{collection}` | — |
| LCIA single method | `GET\|POST /db/{db}/activity/{id}/impacts/{collection}/{methodId}` | `impacts ID --method METHOD_UUID` |
| LCIA batch over many activities | `POST /db/{db}/impacts/{collection}` | — |
| Contributing flows | `GET /db/{db}/activity/{id}/contributing-flows/{collection}/{methodId}` | — |
| Contributing activities | `GET /db/{db}/activity/{id}/contributing-activities/{collection}/{methodId}` | — |
| Flow details | `GET /db/{db}/flow/{flowId}` | `flow FLOW_ID` |
| Flow activities | `GET /db/{db}/flow/{flowId}/activities` | `flow FLOW_ID activities` |
| **Flow Mapping** | | |
| Mapping coverage | `GET /db/{db}/method/{id}/mapping` | `flow-mapping METHOD_UUID` |
| Per-flow mapping | `GET /db/{db}/method/{id}/flow-mapping` | `flow-mapping METHOD_UUID --matched` |
| Characterization for flow | `GET /db/{db}/method/{id}/characterization?flow=` | — |
| Unmatched CFs | included in mapping response | `flow-mapping METHOD_UUID --unmatched` |
| Uncharacterized flows | — | `flow-mapping METHOD_UUID --uncharacterized` |
| **Database Management** | | |
| List databases | `GET /db` | `database` |
| Upload database | `POST /db/upload` | `database upload FILE --name NAME` |
| Load / unload | `POST /db/{name}/(load\|unload)` | — (use config `load = true`) |
| Relink / finalize | `POST /db/{name}/(relink\|finalize)` | — |
| Setup / dependencies | `GET /db/{name}/setup`, `POST .../{add,remove}-dependency/{dep}`, `POST .../set-data-path` | — |
| Delete database | `DELETE /db/{name}` | `database delete NAME` |
| **Method Management** | | |
| List methods | `GET /methods` | `methods` |
| Method details | `GET /method/{id}` | — |
| Method factors | `GET /method/{id}/factors` | — |
| List collections | `GET /method-collections` | `method` |
| Upload collection | `POST /method-collections/upload` | `method upload FILE --name NAME` |
| Load / unload collection | `POST /method-collections/{name}/(load\|unload)` | — |
| Delete collection | `DELETE /method-collections/{name}` | `method delete NAME` |
| **Reference Data** | | |
| Flow synonyms | `GET /flow-synonyms` (+ load/unload/upload/delete/groups/download) | `synonyms` |
| Compartment mappings | `GET /compartment-mappings` (+ load/unload/upload/delete) | `compartment-mappings` |
| Units | `GET /units` (+ load/unload/upload/delete) | `units` |
| **Plugins** | | |
| List plugins | — | `plugin list` |
| Run analyzer | `GET /db/{db}/activity/{id}/analyze/{name}` | — |
| **Matrix Export** | | |
| Universal format | — | `export-matrices DIR` (local only) |
| Debug matrices | — | `debug-matrices ID --output FILE` (local only) |
| **Server** | | |
| Version / stats / hosting / logs | `GET /version`, `/stats`, `/hosting`, `/logs?since=` | — |
| Stop server | — | `stop` |
| REPL | — | `repl` |
| Login | `POST /auth` | — |

All API routes are prefixed with `/api/v1/`. A dash (—) means the feature is only available in one interface.

---


## Building

### Linux / macOS

Install the MUMPS sparse solver and other dependencies, then build:

```bash
# Debian/Ubuntu
sudo apt install build-essential python3 curl zlib1g-dev libmumps-seq-dev

# macOS
brew install gcc python3 curl mumps

# Fedora
sudo dnf install gcc gcc-c++ make python3 curl zlib-devel MUMPS-devel

# Arch Linux
sudo pacman -S base-devel python curl zlib mumps
```

Install the [Haskell toolchain via GHCup](https://www.haskell.org/ghcup/), then:

```bash
./build.sh              # Build
./build.sh --test       # Build and run tests
```

### Windows (MSYS2)

1. Install [MSYS2](https://www.msys2.org/) and open the "MSYS2 UCRT64" terminal
2. Install dependencies:
   ```bash
   pacman -S mingw-w64-ucrt-x86_64-gcc mingw-w64-ucrt-x86_64-mumps make python git
   ```
3. Install [GHCup](https://www.haskell.org/ghcup/) for the compiler toolchain
4. Run:
   ```bash
   ./build.sh            # Same script as Linux/macOS
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

# Or manually
cabal test --test-show-details=streaming
```

Tests cover matrix construction (sign convention), inventory calculation (golden values), parsers (EcoSpold1/2, ILCD, SimaPro, classification fields), and matrix export format compliance.

---

## License

Apache License 2.0 — see LICENSE for details.
