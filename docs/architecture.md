# VoLCA Engine Architecture

VoLCA is a Life Cycle Assessment (LCA) engine written in Haskell. It loads LCA
databases (EcoSpold2, EcoSpold1, SimaPro CSV, ILCD, Brightway Excel), builds sparse
technosphere/biosphere matrices, and computes life-cycle inventories (LCI) and impact
scores (LCIA) - **entirely in memory**.

The project compiles into a **single binary** (`volca`). The HTTP server is the heart
of the system; the CLI and REPL are just thin HTTP clients.

## Overview

```
                                  CLIENTS
   ┌──────────┬───────────┬──────────────┬──────────────┬──────────────┐
   │   CLI    │   REPL    │  any custom  │  MCP client  │  pyvolca /   │
   │  (HTTP)  │  (HTTP)   │   web UI     │ (Claude/GPT) │  curl        │
   └────┬─────┴─────┬─────┴──────┬───────┴──────┬───────┴──────┬───────┘
        └───────────┴────────────┴──── HTTP ────┴──────────────┘
                                   │
 ══════════════════════════════════▼══════════════ "volca server" process
   ┌─────────────────────────────────────────────────────────────────┐
   │  EDGE      API.Auth  - session cookie, single-code login (opt.) │
   ├──────────────┬─────────────┬───────────────┬────────────────────┤
   │ API          │ MCP server  │ Static files  │ OpenAPI / licenses │
   │ /api/v1/*    │ API.MCP     │ for embedded  │ API.OpenApi        │
   │ API.Routes   │ (AI tools)  │ custom web UI │                    │
   └──────┬───────┴──────┬──────┴───────────────┴────────────────────┘
          └──────────────┘
                  │
   ┌──────────────▼──────────────────────────────────────────────────┐
   │  SERVICE     Service.hs · Service/Aggregate.hs                  │
   │  inventory · impacts · tree · graph · supply-chain · consumers  │
   │  contributions · what-if (substitutions) · batch                │
   └──────────────┬──────────────────────────────────────────────────┘
                  │
   ┌──────────────▼──────────────────────────────────────────────────┐
   │ ENGINE CORE   (in memory, with Software Transactional Memory)   │
   │                                                                 │
   │ Database.Manager ──► TVar [LoadedDatabase]   ◄── Config (TOML)  │
   │        │                                                        │
   │        ├─ Types.hs        domain model (Activity/Flow/Exchange) │
   │        ├─ Database.MatrixBuild  builds the sparse A / B matrices│
   │        ├─ Matrix + SharedSolver   (I−A)⁻¹·f = s   then   B·s = g│
   │        ├─ Method.Mapping  LCIA: CF cascade UUID→name→synonym→CAS│
   │        ├─ Search          BM25 (full-text) + Fuzzy (trigrams)   │
   │        ├─ Tree / Expr / UnitConversion / SynonymDB              │
   │        └─ CrossLinking    cross-database supplier resolution    │
   └─────┬─────────────────────────────────────────────────┬─────────┘
         │ at load                                         │ at solve
   ┌─────▼──────────────────────────────────┐     ┌────────▼──────────┐
   │  INGESTION                             │     │  NATIVE           │
   │  Parsers : EcoSpold2 · EcoSpold1 ·     │     │  Direct sparse    │
   │   SimaPro CSV · ILCD · Brightway .xlsx │     │  MUMPS solver     │
   │  Methods : ILCD / CSV / olca packages  │     │  (Fortran)        │
   │  Archives : zip · 7z · gz · xz         │     │  via mumps-hs     │
   │  Cache : Data.Store, schema-invalidated│     │  (FFI / cbits)    │
   └────────────────────────────────────────┘     └───────────────────┘

   ┌──────────────────── CROSS-CUTTING ───────────────────────────────┐
   │  Progress (structured logs)   ·   Version                        │
   └──────────────────────────────────────────────────────────────────┘
```

## Entry points

`app/Main.hs` dispatches on the command:

| Command                   | Role                                                              |
|---------------------------|-------------------------------------------------------------------|
| `volca server`            | Launches the HTTP server (Warp + Servant), loads databases into memory |
| `volca repl`              | Interactive REPL - auto-starts the server if needed               |
| `volca <command>`         | Thin CLI client: queries the server over HTTP (~0.2 s/command)    |
| `volca dump-openapi`      | Emits the OpenAPI specification on stdout                         |
| `volca dump-mcp-tools`    | Emits the MCP tool definitions on stdout                         |
| `volca dump-config-schema` | Emits the names of the keys a configuration file may carry       |
| `volca stop`              | Stops the running server                                         |

## Two key flows

### 1 - Startup / loading a database

`volca.toml` → `Database.Manager` → for each database: archive extraction →
format parser → domain model → `Database.MatrixBuild` assembles the sparse **A**
(technosphere) and **B** (biosphere) triples (`Database.hs` composes the builders
and adds progress reporting) → `Data.Store` serialisation into a cache co-located
with the source file.

The cache is invalidated automatically when the schema changes. Cold vs. warm
figures live in the README's Performance table (single source for benchmarks).

### 2 - Computing an impact

HTTP request → `API.Auth` → Servant handler (`API.Routes`) → `Service` → `SharedSolver`.

The **MUMPS LU factorisation is lazy**: it is computed only on the first `solve`,
guarded by an `MVar` for thread safety, then reused for the whole lifetime of the
server. We solve the scaling vector `s`, then the inventory `g = B·s`, then
`Method.Mapping` applies the characterisation factors (CFs) to obtain the LCIA score.

Equations solved:

```
(I − A)⁻¹ · f = s      A = technosphere matrix, f = final demand, s = scaling
        B · s = g      B = biosphere matrix,    g = inventory (LCI)
       CF · g = score  characterisation factors → impact (LCIA)
```

## Zoom: inside the core engine

The core engine is where databases live in memory and where every LCA number is
produced. It has two halves: an **immutable, cacheable data model** (`Database`) and
a small amount of **mutable runtime state** (`Database.Manager` + `SharedSolver`).

### State model

```
 Database.Manager  ──  mutable state hub, every field an STM TVar
 ┌────────────────────────────────────────────────────────────────────────────┐
 │  dmLoadedDbs        TVar (Map Text LoadedDatabase)  one entry per loaded DB│
 │  dmLoadedMethods    TVar (Map Text MethodCollection) parsed CFs + NW data  │
 │  dmIndexedDbs       TVar (Map Text IndexedDatabase)  cross-DB link indexes │
 │  dmStagedDbs        TVar (Map Text StagedDatabase)   parsed, not yet linked│
 │  dmLoaded{FlowSyns,CompMaps,UnitDefs}   reference data (toggleable)        │
 │  dmMethod{Mapping,Tables,SetTables}Cache  memoised LCIA lookup tables      │
 │  dmGeographies  Map code → (name, parents)                                 │
 └─────────────────────────────────┬──────────────────────────────────────────┘
                                   │  one value per loaded database
                                   ▼
 ┌──────────────────────────  LoadedDatabase  ──────────────────────────────┐
 │                                                                          │
 │  ┌──────────────  Database  (immutable · serialised to cache)  ────────┐ │
 │  │  DOMAIN        dbActivities : Vector Activity                       │ │
 │  │                dbBioFlows · dbTechFlows · dbUnits                   │ │
 │  │  INTERNING     dbProcessIdTable : ProcessId ↔ (activityUUID,        │ │
 │  │                productUUID)   - Int32 keys for matrix indexing      │ │
 │  │  INDEXES       dbIndexes : by name / geo / flow / category          │ │
 │  │                dbBM25Index · dbProductSearchIndex   (built at load) │ │
 │  │  SPARSE        dbTechnosphereTriples → A   (activities × activities)│ │
 │  │  MATRICES      dbBiosphereTriples    → B   (bioflows  × activities) │ │
 │  │  CROSS-DB      dbCrossDBLinks · dbDependsOn · dbLinkingStats        │ │
 │  │  LCIA HELPERS  dbFlowsByName · dbFlowsByCAS · dbSynonymDB (runtime) │ │
 │  └─────────────────────────────────────────────────────────────────────┘ │
 │                                                                          │
 │  ┌──────────────  SharedSolver  (mutable · runtime only)  ─────────────┐ │
 │  │  solverLock             : MVar ()      serialises factorisation     │ │
 │  │  solverFactorizationVar : MVar (Maybe MatrixFactorization)          │ │
 │  │      └─ lazy LU factorisation of the (I − A) system matrix,         │ │
 │  │         computed on first solve, then reused for the process life   │ │
 │  └─────────────────────────────────────────────────────────────────────┘ │
 │                                                                          │
 │  ldConfig : DatabaseConfig                                               │
 └──────────────────────────────────────────────────────────────────────────┘
```

Only the `Database` fields above the dashed line are serialised by the `Data.Store`
cache. Runtime-only fields (factorisation, synonym DB, name/CAS indexes, BM25 index)
are rebuilt on load - cheap compared to parsing.

### Computation pipeline

A single impact request walks the LCA equations left to right. The MUMPS
factorisation is built once (lazily) and every later request reuses it.

```
 ProcessId  (Int32, interned from the activity/product UUID pair)
    │
    │  buildDemandVectorFromIndex
    ▼
  f   final demand vector  (1.0 at the target activity, 0 elsewhere)
    │
    │  SharedSolver.solveWithSharedSolver          A = technosphere matrix
    │     ⇒  (I − A) · s = f       solved by MUMPS LU factorisation (reused)
    ▼
  s   scaling vector        (how much each activity must run)
    │
    │  applyBiosphereMatrix                        B = biosphere matrix
    │     ⇒  g = B · s
    ▼
  g   life-cycle inventory (LCI)  ───────────────────────────► InventoryExport
    │
    │  Method.Mapping.computeLCIAScoreFromTables
    │     ⇒  score = Σ  CF(flow) · g(flow)
    ▼
  LCIA score  (+ per-flow / per-activity contributions) ─────► LCIAResult
```

`Service.hs` orchestrates this; `Matrix.hs` owns the linear algebra; `SharedSolver.hs`
guards the factorisation and exposes cached scaling/inventory helpers. Batch requests
take the same path with a single multi-RHS solve (`computeInventoryMatrixBatch`).

### LCIA mapping cascade

A method collection ships characterisation factors (CFs) keyed by its own flow
nomenclature. They must be matched to the loaded database's biosphere flows. The
match is a **first-hit-wins cascade** (`Method.Mapping`, default `UUID → Name
→ Synonym → CAS`, the CAS number as last resort):

```
  MethodCF  (one CF from the method collection)        DB biosphere flows
       │                                                      │
       ▼                                                      ▼
   ┌────────────────────  Method.Mapping cascade  ──────────────────────┐
   │  ① ByUUID     CF target UUID == flow UUID      (dbBioFlows)        │
   │  ② ByName     normalised-name match            (dbFlowsByName)     │
   │  ③ BySynonym  synonym expansion then name match (dbSynonymDB)      │
   │  ④ ByCAS      CAS number match                 (dbFlowsByCAS)      │
   └─────────────────────────────────┬───────────────────────────────────┘
       first strategy that hits wins → (Flow, MatchStrategy)
                                     │  computeMappingStats → coverage %
                                     ▼
   MethodTables   mtExactCF · mtFallbackCF · mtRegionalizedCF · mtBroadcast
       │  built once by buildMethodTables, memoised in dmMethodTablesCache
       ▼
   score = Σ  CF(flow) · g(flow)
```

`MethodTables` collapses the cascade and absorbs unit conversion into plain `Map`
lookups, so scoring itself is a fast dot product over the inventory `g`.

### Normalization, weighting and single scores

Two mechanisms turn per-category LCIA results into comparable numbers:

- **Built-in NW sets** - a method collection may ship its own
  normalization/weighting data (`mcNormWeightSets`, parsed by
  `Method.ParserNW`); batch LCIA then returns Raw / Normalized / Weighted
  views and an aggregated single score (Pt).
- **Formula-based scoring sets** - configured per collection in TOML
  (`[[methods.scoring]]`, decoded in `Config`, stored as `mcScoringSets`).
  `Method.Types.computeFormulaScores` evaluates one against the raw
  category scores: ① bind short variables to category names, ② resolve
  computed variables (`Expr` formulas, e.g. `etf = "2 * etfo + etfi"`),
  ③ apply `raw / normalization × weighting` per variable, ④ evaluate the
  score formulas over that environment, and scale everything by
  `displayMultiplier`. Missing data surfaces as an error, never as a
  silent zero.

Scoring sets are what `list_scoring_sets` / `score_activity` /
`score_activities` expose over REST and MCP.

### What-if and cross-database solving

- **What-if substitution** - `Matrix.perturbA` / `perturbABatch` apply an upstream
  activity swap to the `A` matrix and the system is re-solved, yielding a modified
  inventory and impacts in one call (~120 ms) without touching the original database.
- **Cross-DB solving** - when a database declares `dbCrossDBLinks` (e.g. a sector
  database referencing Agribalyse), the root scaling vector feeds supplier demand
  vectors into each dependency database. Each dependency runs its own multi-RHS
  solve; `SharedSolver` recurses through the dependency DAG (depth-capped at 10) and
  merges per-database inventories with `M.unionWith (+)`.

## Module reference

| Module / folder             | Role                                                                 |
|------------------------------|----------------------------------------------------------------------|
| `app/Main.hs`               | Entry point: CLI / server / REPL dispatch                            |
| `API/Resources.hs`          | Resource registry: one `Resource` per operation; REST, MCP and OpenAPI derive from it |
| `API/Routes.hs`             | Servant `/api/v1/*` route definitions and handlers                  |
| `API/Auth.hs`               | Authentication middleware (session cookie, single-code login)        |
| `API/MCP.hs`                | MCP server: tools exposed to AI assistants                          |
| `API/OpenApi.hs`            | OpenAPI specification generation                                    |
| `CLI/`                      | HTTP client, argument parser, REPL                                  |
| `Config.hs`                 | TOML configuration loading                                          |
| `Service.hs`, `Service/`    | Orchestration: inventory, impacts, tree, graph, supply-chain, etc.   |
| `Database/Manager.hs`       | Loaded-database state (`TVar`), load/unload, methods, refdata        |
| `Database/Loader.hs`        | Loading and dispatch to parsers                                     |
| `Database/CrossLinking.hs`  | Cross-database supplier resolution, topological order               |
| `Database/Upload.hs`        | Database upload via the API                                         |
| `Database/MatrixBuild.hs`   | Sparse A / B matrix construction, normalisation (all numerical work) |
| `Database.hs`               | Composes the matrix builders, adds progress reporting               |
| `Types.hs`                  | Domain model (Activity, Flow, Exchange, Database) - sum types        |
| `Matrix.hs`                 | Matrix LCA computations via the MUMPS solver                        |
| `SharedSolver.hs`           | Lazy thread-safe LU factorisation, caches, back-substitution        |
| `mumps-hs/`                 | FFI bindings to the MUMPS direct sparse solver (Fortran)            |
| `EcoSpold/`, `SimaPro/`, `ILCD/`, `BrightwayExcel/` | One namespace per database format, each with a `Parser` (read, wired into `Database/Loader.hs`) and a `Writer` (export, wired into `Database/Export.hs`) |
| `Method/Parser*.hs`         | Method collection loading (ILCD, CSV, SimaPro, olca)                |
| `Method/FlowResolver.hs`    | Parses ILCD flow XMLs to enrich MethodCFs (name, compartment, CAS)  |
| `Database/Edit.hs`, `Database/Export.hs`, `Database/RelinkMapping.hs` | Database write toolkit: delete/copy, export to any format, relink via alias CSV |
| `SubstanceRegistry.hs`      | Canonical flow registry: equivalence classes over flow-identity assertions |
| `Method/Mapping.hs`         | CF matching cascade (UUID → name → synonym → CAS), LCIA scoring     |
| `Search/BM25.hs`            | BM25 full-text search index                                         |
| `Search/Fuzzy.hs`           | Trigram fuzzy search                                                |
| `Tree.hs`                   | Supply-chain tree construction (loop-aware)                         |
| `Expr.hs`                   | Exchange formula / parameter evaluation                             |
| `UnitConversion.hs`         | Unit conversion                                                     |
| `SynonymDB.hs`              | Flow-name synonym registry (curated CSV; auto-extracted candidates are opt-in) |
| `Progress.hs`               | Structured progress logs and reports                                |

## Structuring principles

- **Pure core, effects at the edges** - parsers, the MUMPS FFI and I/O are confined;
  the computation is pure.
- **Impossible states made impossible** - loading statuses, domain model and states
  as sum types, with no `wildcard` pattern on sums.
- **Zero crashes** - `Either Text a` / `Maybe` propagated; no `error`, `undefined`,
  or partial function. No silent fallback value: missing data surfaces as an explicit
  error.
- **Simultaneous multi-database** - several databases loaded in parallel, with
  cross-nomenclature flow linking (`CrossLinking`).
- **Schema-invalidated cache** - co-located with the source, transparent.
