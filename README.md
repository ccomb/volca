# fpLCA

**fpLCA** is a Life Cycle Assessment engine that turns LCA databases into inspectable, queryable answers — fast.

It loads EcoSpold2, EcoSpold1, and SimaPro CSV databases, builds supply chain dependency trees, computes life cycle inventories using sparse matrix algebra, and applies characterization methods for impact assessment. Everything runs in-memory against your own data.

## What It Does

- **Browse** activities and flows across multiple databases
- **Explore** supply chain trees and force-directed dependency graphs
- **Compute** life cycle inventories (LCI) and impact scores (LCIA) — single method or batch across a full collection
- **Map** method characterization factors to database flows with a 4-step cascade (UUID → CAS → name → synonym) and coverage statistics
- **Link** databases across nomenclatures (e.g., Agribalyse referencing Ecoinvent)
- **Upload** databases and method collections via the web UI, without touching config files

---

## Key Features

- **Multiple database formats**: EcoSpold2 (.spold), EcoSpold1 (.xml), SimaPro CSV
- **Archive support**: Load databases directly from .zip, .7z, .gz, or .xz archives — no manual extraction
- **Cross-database linking**: Resolve supplier references across databases, with configurable dependencies and topological load ordering
- **LCIA method collections**: Load ILCD method packages (ZIP or directory) from config or upload via UI; CSV format also supported
- **Flow mapping engine**: 4-step matching cascade (UUID → CAS → name → synonym) with per-strategy coverage statistics and inspection UI
- **Auto-extracted synonyms**: Synonym pairs extracted automatically from loaded databases and method packages, available for toggling and download
- **Reference data management**: Flow synonyms, compartment mappings, and unit definitions can be configured in TOML, uploaded via UI, or toggled independently
- **Web interface**: Multi-page Elm app with search, tree view, graph view, inventory, LCIA, method management, and reference data pages
- **Desktop application**: Native Windows/Linux app — no installation or configuration needed
- **REST API and CLI**: Scriptable interface for automation and integration
- **Fast cache**: Per-database cache with automatic schema-based invalidation; Ecoinvent loads in ~45s cold, ~2-3s cached
- **Optional access control**: Single-code login with cookie-based session

---

## Getting Started

### Desktop Application (Recommended)

Download and run the installer for Windows or Linux from the releases page. The desktop app bundles the complete engine and opens a browser-based UI automatically.

### Web Server

```bash
# Build (requires PETSc/SLEPc — see Building section)
./build.sh

# Run with a config file
cabal run fplca -- --config fplca.toml server --port 8081
# Open http://localhost:8081
```

### Command Line

```bash
# Search activities
fplca --config fplca.toml activities --name "electricity" --geo "DE"

# Supply chain tree
fplca --config fplca.toml tree "12345678-..." --tree-depth 3

# Life cycle inventory
fplca --config fplca.toml inventory "12345678-..."

# Impact assessment
fplca --config fplca.toml lcia "12345678-..." --method ./EF-3.1.xml

# Database and method management
fplca --config fplca.toml database upload mydb.7z --name "My Database"
fplca --config fplca.toml method upload EF-3.1.zip --name "EF 3.1"
```

---

## Configuration

A TOML config file enables multi-database setups, method collections, and reference data:

```toml
[server]
port = 8081
host = "127.0.0.1"
password = "mysecret"          # optional — omit to disable auth

[[databases]]
name = "ecoinvent-3.12"
displayName = "Ecoinvent 3.12"
path = "DBs/ecoinvent3.12.7z"  # archives supported natively
description = "Cutoff System Model, EcoSpold2"
load = true

[[databases]]
name = "agribalyse-3.2"
displayName = "Agribalyse 3.2"
path = "DBs/AGB32_final.CSV"
description = "SimaPro CSV"
load = false

[[databases]]
name = "my-sector-db"
displayName = "Sector DB"
path = "DBs/sector.CSV"
depends = ["agribalyse-3.2"]   # loads agribalyse first, then links
load = true

[[methods]]
name = "EF-3.1"
path = "DBs/EF-v3.1.zip"      # ILCD method package (ZIP or directory)

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

The `depends` field ensures dependency databases load first and their flows are available for cross-database linking. Setting `load = true` on a database transitively loads all its dependencies. Reference data sections (`flow-synonyms`, `compartment-mappings`, `units`) can also be uploaded and toggled at runtime via the web UI.

---

## Web Interface

| Page | What it shows |
|------|--------------|
| Activities | Searchable list with name, geography, and product filters |
| Tree | Hierarchical upstream dependency view |
| Graph | Force-directed network with configurable cutoff |
| Inventory | Environmental flows split into Emissions and Resources |
| LCIA | Impact scores per category with method picker; single or batch across a collection |
| Methods | Load, unload, upload, and inspect ILCD method collections |
| Flow Mapping | Per-method CF coverage: matched by UUID/CAS/name/synonym, unmapped list |
| Databases | Load, unload, upload, and configure cross-database links |
| Database Setup | Data path picker, dependency editor, linking diagnostics |
| Flow Synonyms | Manage synonym sets; browse groups, download auto-extracted pairs |
| Compartment Mappings | Manage compartment name mappings |
| Units | Manage unit definitions for dimensional analysis |

---

## REST API

All per-database resources are scoped under `/api/v1/database/{name}/`:

```
GET    /api/v1/database/{name}/activity/{id}                  Activity details
GET    /api/v1/database/{name}/activity/{id}/tree             Supply chain tree
GET    /api/v1/database/{name}/activity/{id}/inventory        Life cycle inventory
GET    /api/v1/database/{name}/activity/{id}/graph?cutoff=    Supply chain graph
GET    /api/v1/database/{name}/activity/{id}/lcia/{methodId}  LCIA score (single method)
GET    /api/v1/database/{name}/activity/{id}/lcia-batch/{col} LCIA batch (all categories)
GET    /api/v1/database/{name}/activities?name=&geo=&product= Search activities
GET    /api/v1/database/{name}/flows?q=&lang=                 Search flows
GET    /api/v1/database/{name}/flow/{flowId}                  Flow details
GET    /api/v1/database/{name}/flow/{flowId}/activities       Activities using a flow
GET    /api/v1/database/{name}/method/{id}/mapping            Mapping coverage stats
GET    /api/v1/database/{name}/method/{id}/flow-mapping       Per-flow mapping detail
GET    /api/v1/database                                       List databases and status
POST   /api/v1/database/upload                                Upload a database archive
POST   /api/v1/database/{name}/load                           Load a configured database
POST   /api/v1/database/{name}/finalize                       Finalize cross-DB linking
DELETE /api/v1/database/{name}                                Delete a database
GET    /api/v1/methods                                        List individual methods
GET    /api/v1/method/{id}                                    Method details
GET    /api/v1/method/{id}/factors                            Characterization factors
GET    /api/v1/method-collections                             List method collections
POST   /api/v1/method-collections/{name}/load                 Load a method collection
POST   /api/v1/method-collections/{name}/unload               Unload a method collection
POST   /api/v1/method-collections/upload                      Upload a method package
DELETE /api/v1/method-collections/{name}                      Delete a method collection
GET    /api/v1/flow-synonyms                                  List synonym sets
POST   /api/v1/flow-synonyms/{name}/load                      Activate a synonym set
POST   /api/v1/flow-synonyms/{name}/unload                    Deactivate a synonym set
GET    /api/v1/flow-synonyms/{name}/groups                    Browse synonym groups
GET    /api/v1/flow-synonyms/{name}/download                  Download synonym CSV
GET    /api/v1/compartment-mappings                           List compartment mappings
GET    /api/v1/units                                          List unit definitions
POST   /api/v1/auth                                           Login (returns session cookie)
```

---

## CLI Commands

### Global Options

| Option | Description |
|--------|-------------|
| `--config FILE` | TOML config file for multi-database setup |
| `--db NAME` | Database name to query (from config file) |
| `--format FORMAT` | Output format: `json` (default), `csv`, `table`, `pretty` |
| `--jsonpath PATH` | Field to extract for CSV output (e.g., `srResults`) |
| `--tree-depth N` | Maximum tree depth (default: 2) |
| `--no-cache` | Disable caching (for development) |

### Search

```bash
# Find activities by name, geography, product
fplca activities --name "electricity" --geo "DE" --limit 10
fplca activities --product "steel" --limit 10 --offset 20

# Find flows by keyword
fplca flows --query "carbon dioxide" --limit 5
```

### Analysis

```bash
# Activity details
fplca activity "12345678-..."

# Supply chain tree
fplca tree "12345678-..." --tree-depth 3

# Life cycle inventory
fplca inventory "12345678-..."

# Impact assessment
fplca lcia "12345678-..." --method ./EF-3.1.xml

# Matrix export (Ecoinvent universal format)
fplca export-matrices ./output_dir
```

### Flow Mapping Diagnostics

```bash
# Summary: how well does a method match a database?
fplca --db "Agribalyse 3.2" mapping METHOD_UUID

# See every mapped CF with its match strategy (uuid/cas/name/synonym)
fplca --db "Agribalyse 3.2" mapping METHOD_UUID --matched

# List CFs that found no DB flow
fplca --db "Agribalyse 3.2" mapping METHOD_UUID --unmatched

# List DB biosphere flows with no CF
fplca --db "Agribalyse 3.2" mapping METHOD_UUID --uncharacterized

# Machine-readable
fplca --db "Agribalyse 3.2" mapping METHOD_UUID --matched --format json
```

### Database and Method Management

```bash
# List, upload, delete databases
fplca database                                  # list (default)
fplca database upload mydb.7z --name "My DB"    # upload
fplca database delete my-db                     # delete

# List, upload, delete method collections
fplca method                                    # list (default)
fplca method upload EF-3.1.zip --name "EF 3.1"  # upload
fplca method delete ef-31                        # delete
```

---

## API and CLI Feature Matrix

| Feature | REST API | CLI |
|---------|----------|-----|
| **Search** | | |
| Search activities | `GET /database/{db}/activities?name=&geo=&product=` | `activities --name --geo --product` |
| Search flows | `GET /database/{db}/flows?q=&lang=` | `flows --query --lang` |
| **Analysis** | | |
| Activity details | `GET /database/{db}/activity/{id}` | `activity ID` |
| Supply chain tree | `GET /database/{db}/activity/{id}/tree` | `tree ID --depth N` |
| Supply chain graph | `GET /database/{db}/activity/{id}/graph?cutoff=` | — |
| Life cycle inventory | `GET /database/{db}/activity/{id}/inventory` | `inventory ID` |
| LCIA (single method) | `GET /database/{db}/activity/{id}/lcia/{methodId}` | `lcia ID --method FILE` |
| LCIA batch | `GET /database/{db}/activity/{id}/lcia-batch/{col}` | — |
| Flow details | `GET /database/{db}/flow/{flowId}` | `flow FLOW_ID` |
| Flow activities | `GET /database/{db}/flow/{flowId}/activities` | `flow FLOW_ID activities` |
| **Flow Mapping** | | |
| Mapping coverage | `GET /database/{db}/method/{id}/mapping` | `mapping METHOD_UUID` |
| Per-flow mapping | `GET /database/{db}/method/{id}/flow-mapping` | `mapping METHOD_UUID --matched` |
| Unmatched CFs | included in mapping response | `mapping METHOD_UUID --unmatched` |
| Uncharacterized flows | — | `mapping METHOD_UUID --uncharacterized` |
| **Database Management** | | |
| List databases | `GET /database` | `database` |
| Upload database | `POST /database/upload` | `database upload FILE --name NAME` |
| Load database | `POST /database/{name}/load` | — (use config `load = true`) |
| Delete database | `DELETE /database/{name}` | `database delete NAME` |
| Finalize linking | `POST /database/{name}/finalize` | — |
| **Method Management** | | |
| List methods | `GET /methods` | `methods` |
| Method details | `GET /method/{id}` | — |
| Method factors | `GET /method/{id}/factors` | — |
| List collections | `GET /method-collections` | `method` |
| Upload collection | `POST /method-collections/upload` | `method upload FILE --name NAME` |
| Delete collection | `DELETE /method-collections/{name}` | `method delete NAME` |
| **Reference Data** | | |
| Flow synonyms | `GET /flow-synonyms` | `synonyms` |
| Compartment mappings | `GET /compartment-mappings` | `compartment-mappings` |
| Units | `GET /units` | `units` |
| **Matrix Export** | | |
| Universal format | — | `export-matrices DIR` |
| Debug matrices | — | `debug-matrices ID --output FILE` |
| **Auth** | | |
| Login | `POST /auth` | — |

All API routes are prefixed with `/api/v1/`. A dash (—) means the feature is only available in one interface.

---

## Building

### Linux / macOS

```bash
./build.sh              # Download PETSc/SLEPc and build everything
./build.sh --test       # Build and run tests
./build.sh --desktop    # Build desktop application
```

The build script downloads and compiles PETSc and SLEPc automatically if not already present.

### Windows (MSYS2)

1. Install [MSYS2](https://www.msys2.org/) and open the "MSYS2 UCRT64" terminal
2. Install dependencies:
   ```bash
   pacman -S make python git \
             mingw-w64-ucrt-x86_64-gcc \
             mingw-w64-ucrt-x86_64-gcc-fortran \
             mingw-w64-ucrt-x86_64-cmake \
             mingw-w64-ucrt-x86_64-openblas \
             mingw-w64-ucrt-x86_64-msmpi \
             mingw-w64-ucrt-x86_64-zlib
   ```
3. Install [GHCup](https://www.haskell.org/ghcup/) for the compiler toolchain
4. Run:
   ```bash
   ./build.sh            # Same script as Linux/macOS
   ./build.sh --desktop  # Builds Windows installer (.exe)
   ```

### Docker

```bash
docker build -t fplca .
docker run -p 8081:8081 -v /path/to/data:/data fplca
```

---

## Performance

Databases are loaded entirely into memory. A schema-aware cache (`.bin.zst` per database, stored in `cache/`) makes subsequent startups fast:

| Database | Cold load | Cached load |
|----------|-----------|-------------|
| Ecoinvent 3.12 (25k activities) | ~45s | ~2-3s |
| Small sector database | ~2s | <0.5s |

Matrix solving uses PETSc sparse solvers. Inventory computation for a typical supply chain takes under 15 seconds on a large database.

---

## Testing

```bash
./build.sh --test

# Or manually
export LD_LIBRARY_PATH="petsc-3.24.2/arch-linux-c-opt/lib:slepc-3.24.1/arch-linux-c-opt/lib"
cabal test --test-show-details=streaming
```

Tests cover matrix construction (sign convention), inventory calculation (golden values), parsers, and matrix export format compliance.

---

## License

GNU Affero General Public License 3.0 or later — see LICENSE for details.
