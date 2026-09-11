# AGENTS.md

Guidance for AI agents working in the VoLCA engine repository.

## What this is

VoLCA is an in-memory Life Cycle Assessment (LCA) engine in Haskell (GHC 9.12.4,
package `volca`, Apache-2.0). It loads EcoSpold 1/2, SimaPro CSV, ILCD, and Excel
databases, builds supply-chain dependency trees, computes life-cycle inventories (LCI)
via sparse-matrix algebra, and applies characterization methods for impact assessment.
Multiple databases load simultaneously with cross-database flow linking and what-if
substitutions. It exposes a Servant REST API and an MCP server, plus a CLI/REPL.

See `README.md` for the full feature spec.

## Glossary

- **LCI** — life-cycle inventory: the raw physical flows (emissions, resources) for a product.
- **LCIA** — impact assessment: LCI flows × characterization factors → impact scores.
- **CF** — characterization factor: one flow's contribution to one impact category.
- **Activity** — a real-world transformation (`activityUUID`); may be **multi-output**. Owns `activity_name` and `all_products`. A *grouping* — never scored directly.
- **Product** — a reference output (a technosphere flow). Owns `product_name`.
- **Process** — a **single-output** unit = one `(activity, product)` pair, addressed by `process_id = activityUUID_productUUID`. Carries `activity_name` + `product_name`; **it has no name of its own** — a display label is generated on demand (`activity_name` + `product_name`), never stored or emitted as `process_name`. **This is the unit you search, get, and score.**
- **Exchange / Flow** — one input-output line / the substance or product exchanged.

**Field names: `process_id`, `activity_name`, `product_name`. There is no `process_name`** — it would be a derived value, so the consumer generates the label. The word "activity" is overloaded in the field: ecoinvent/Brightway call a single-output dataset an "activity" (with a reference product); SimaPro/ILCD/ISO call it a "(unit) process". VoLCA keeps both crisp — **activity = the grouping, process = the (activity, product) unit.** The API *verbs* are activity-named by ecoinvent convention but all operate on **processes**: **`search_activities` / `get_activity` / `score_activities` / `get_contributing_activities` return one process per `(activity, product)`.** `search_activities` filters by `activity_name` and/or `product_name` and always returns processes — to find the lowest-impact way to make a product, `search_activities(product=X)` → processes → `score_activities` → compare. (SimaPro conflates the two: its process name is often empty and the activity name leaks into the product string; VoLCA keeps the two fields separate and surfaces the gap honestly rather than fabricating a name.)

## Commands

```bash
./gen-version.sh        # Generate src/Version.hs and src/Builtin/Literals.hs (git metadata, built-in reference data) — REQUIRED before a native build
./build.sh              # Build the engine
./build.sh --test       # Build + run the test suite
./build.sh --coverage   # Build + tests + HTML coverage report
cabal test lca-tests --test-show-details=streaming        # Run tests directly
cabal test lca-tests --test-options="--match /Inventory/" # Run a single spec group
```

`src/Version.hs` is generated, not committed — a native `cabal build`/`cabal test`
fails without running `./gen-version.sh` first. Tests use hspec + hspec-discover;
spec modules are `*Spec.hs` under `test/` and auto-discovered. New behavior needs a
`*Spec.hs`; integration fixtures (sample databases, golden values) live in `test-data/`.

## Module spine

| Area | Modules | Role |
|------|---------|------|
| Domain | `Types` | Central domain types (Activity, Exchange, Flow, …) |
| Solve | `Matrix`, `SharedSolver` | Sparse technosphere/biosphere matrices; MUMPS-backed LCI solve |
| Formats | `EcoSpold.*`, `SimaPro.*`, `ILCD.*`, `BrightwayExcel.*`, `Method.Parser*` | One namespace per database format, each with a `Parser` (wired into `Database.Loader`) and a `Writer` (wired into `Database.Export`); method readers are `Method.Parser*` |
| Methods | `Method.Mapping`, `Method.FlowResolver`, `Method.ChemSynonyms` | CF mapping (UUID→name→synonym→CAS cascade), flow resolution |
| Database | `Database.*` (`Loader`, `CrossLinking`, `MatrixBuild`, `Manager`) | Load, cross-link, build matrices, manage lifecycle |
| Analysis | `Service`, `Service.Aggregate`, `Tree` | Analysis ops, grouping, supply-chain traversal |
| Search | `Search.BM25`, `Search.Fuzzy`, `Search.Normalize` | Activity/flow search |
| API | `API.Resources`, `API.Routes`, `API.MCP` | `API.Resources` holds the resource registry (one `Resource` per operation); REST routes, MCP tools and OpenAPI derive from it |
| CLI | `CLI.*`, `app/Main.hs` | Arg parsing, commands, HTTP client, REPL; entrypoint (server/client/repl modes) |

A Python client lives in `pyvolca/` (own `pyproject.toml`); the MUMPS binding in
`mumps-hs/`. The core is otherwise pure Haskell.

## Where to start

- **A new MCP tool or REST endpoint** → `API.Resources` holds the resource registry that drives REST, MCP and OpenAPI; add one `Resource` there, not in N places.
- **A new database format** → a new namespace with its `Parser` / `Writer` pair (e.g. `Foo.Parser`, `Foo.Writer`), wired into `Database.Loader` and `Database.Export`.
- **Wrong or empty LCI numbers** → `Database.MatrixBuild` plus `Matrix` / `SharedSolver`; check supplier resolution in `Database.CrossLinking`.
- **Characterization mismatches** → `Method.Mapping` (the UUID→name→synonym→CAS cascade) and `Method.FlowResolver`.
- **Search relevance** → `Search.BM25` / `Search.Fuzzy` / `Search.Normalize`.

## Engineering rules

### Code safety
- **Make impossible states impossible** — use the type system.
- **Name domain values with their own type, not a bare primitive.** When a `Text`/`String`/`Int`/`Double` carries domain meaning that crosses more than one signature — database names, flow and activity UUIDs, units, amounts — wrap it in a `newtype` (with a smart constructor where validation applies). This is why `RootDb`/`ThisDb` are newtypes over `Text` and not raw `Text`, so two database names can't be silently swapped. Two guard-rails: don't wrap values that never leave a single expression (boilerplate ≠ safety), and when a comment is *enumerating* the valid strings, reach for a **sum type**, not a newtype.
- **No wildcard patterns on sum types** — exhaustive matches only. They hide incomplete matches from the compiler.
- **No runtime crashes**: never use `error`, `throw`, `undefined`, or partial functions (`head`, `fromJust`). Functions that can fail must return `Either Text a` or `Maybe a`, never crash.
- **No silent errors or silent misbehaviour**: never return zero, empty, or a fallback value when a lookup fails, a unit conversion can't be done, or data is missing. Surface it: `Either Text a` propagated to a 4xx/5xx, a `reportProgress Warning` log line, or a `toolError` in MCP. A score that silently undercounts is worse than a clear failure — the consumer can't tell something is wrong.
- **One reading: take it and say so. Several: stop.** Between guessing and refusing what is plain there is a rule, and it is the condition for being both transparent and right. When the evidence leaves exactly one possible reading, take it and report the choice where the user reads it. When it leaves more than one, refuse and name the candidates it could not choose between. A unit written `KWH` can only mean `kWh`, so the load continues and says the spelling was wrong; a unit written `mj` against a table holding both `mJ` and `MJ` is a coin toss, so the load stops and the answer comes from the data or the config, never from a heuristic that ranks the candidates. Neither half stands alone: a choice taken without a word is the silent misbehaviour above, and refusing something unambiguous moves work to the user for nothing.
- **An index asserts that its key determines its value.** `fromList` silences duplicate keys, so a key that is only part of a wider identity (an activity UUID taken from the (activity, product) pair, a normalised name taken from a name) quietly keeps one row and drops the rest. Either the key really is the primary key, or the honest type is `Map k (NonEmpty v)` and every reader has to say what it does with several.
  In the modules that read a file, `hlint` enforces this: `Data.Map.fromList` is an error there, and `.hlint.yaml` carries one baseline per site where the key really does determine its value, with what makes that true. Fix the site or add the baseline with its reason - there is no third option, and the baseline is checked against a function that exists.
- **A comment that says "arbitrary", "the first match" or "one of them" is describing a bug, not a behaviour.** Put the multiplicity in the type, or open an issue.
- Builds are `-Wall`-clean — introduce no new warnings (incomplete patterns, unused binds). This is what backs the exhaustive-match rule above.

### Code style
Every point below is checkable on the function in front of you, whether you are
writing it or reading someone else's.

**The signature**
- Avoid long function signatures. More than four positional parameters: gather them in a product type. Two neighbours of the same type: a caller can swap them and the compiler will not notice, so newtype one of them.
- A tuple of three or more, or any tuple that crosses a signature, becomes a record with named fields. `(Text, Maybe Text, Maybe Text, Double)` says nothing at the call site.
- A `Bool` argument reads as `f True` where it is called, which says nothing. Two constructors named after what they mean.
- `IO a` for something that can fail becomes `IO (Either Text a)`, propagated. Never a fallback value standing in for a failure, per Code safety.
- Each type should have a sensible domain or technical meaning.

**The body**
- **Pure domain, effectful edges.** Concretely: ask which lines of an IO function would still make sense with no `IO` in sight, then hoist exactly those into a named, signed function. It is the biggest single win on most functions, and it usually leaves the effectful remainder flat enough to read in one pass.
- One `let` in a `do` block is fine. Three or more means the function is doing several things: a `let` is a computation with a name and no type, so move it to a `where` with a signature, or out to the top level.
- Don't overload functions or write diagonal code (deeply nested, staircase-shaped logic). Past four levels of indentation: guards rather than nested `if`, `maybe`/`either`/`fromMaybe` rather than a `case` on `Maybe`/`Either`, `when`/`unless` rather than `if ... then ... else pure ()`, an early exit through `Either` rather than an `else do` that swallows the rest.
- A `case` inside a `case`: match the pair, or name the intermediate type the two cases are really about.
- Every `where` helper carries its type signature. It is one line, and it is the documentation.
- Prefer short point-free style over explicit case pattern matching, when it shortens the read and not when it encrypts it. `fromMaybe []` on a value that is genuinely optional yes, a four-stage composition through `flip` no, and never a `fromMaybe` that stands in for a failure.
- Comments carry *why*. A comment restating what the line does is noise.
- **A name a caller types is short, expressive, and explains itself where it is used.** Judge it by the line that calls it, not the line that defines it. Name the axis that actually separates a family of siblings: `BioExchange.from_id` and `BioExchange.from_name` say which of the two ways the flow is designated, where `existing` and `named` each answered a different question. Leave the verb out when nothing is being done - a constructor builds a value, it does not add one. This holds for every name someone outside this repo types: pyvolca's classes, methods and constructors, and the operations, fields and parameters in `API.Resources` that REST and MCP derive from.
- Use advanced Haskell patterns when they improve expressivity and reduce line count.

### Design philosophy
- Think like "Out Of The Tar Pit": most complexity is accidental. Minimize mutable state, keep logic declarative, separate state / control / computation.
- Simplicity: perfection = nothing left to remove. Avoid over-engineering and cognitive load.
- **A shape on the wire, once published, stays until the next minor (major from `v1.0.0` on).** Between two of those a change is additive: a new field, route or tool, a new wire revision (`wireVersion` counts what the engine learnt to say, so pyvolca can gate a capability on it, never what it stopped saying). A rename ships the new shape, keeps the old one, and says in the CHANGELOG which release drops it, no sooner than that next minor. `docs/release.md` says how it is numbered.
- **`data/` has its own number, `data/VERSION`**, moved in the PR that changes a file under `data/`, and named in that PR's CHANGELOG entry ("data version 3") the way a wire revision is. It names the release asset, the directory the installers extract into and the `dataVersion` an engine reports, so a number left behind makes two different bundles pass for one; `scripts/check-data-version.sh` refuses it in CI. The engine's own number never moves for data.
- **A published Python name is not free in the same way.** Renaming or removing one in `pyvolca` costs its users an afternoon of edits with nothing to point at: their script does not fail on a version check, it fails on the line that used the name. So ship the new name and keep the old one as an alias that warns and says what replaced it (`volca._compat.warn_renamed`), and drop it no sooner than the release `volca._compat.RENAMED_REMOVED_IN` names, never in the one that introduces the replacement.
- Use language servers for fast diagnostics: HLS for Haskell, pyright for the `pyvolca/` Python client.
- Fix a diagnostic the moment you see it, even a pre-existing one you only surfaced in passing — don't defer it to "later" or leave it because it predates your change. A separate small commit keeps it out of your feature's scope.

### Open-source boundary
- This engine stands alone — keep deployment/SaaS concerns and any customer- or product-specific names out of code, comments, and PRs.
- Name things after the standard or format, not the vendor or tool that produces them.
- **The same holds for prose as for code.** An issue, a pull request or a commit message names the *shape* of data that broke (an EcoSpold 2 activity with two reference products, a CSV row carrying an avoided product), never the database it was found in nor the organisation publishing it. The engine reads formats, not brands, and a bug named after one database is already wrong the day the same shape turns up in another. Same rule for branch names and test fixtures.

### Formatting
- Haskell is formatted with `fourmolu` — version pinned in `versions.env` (`FOURMOLU_VERSION`), style in `fourmolu.yaml`. You don't run it by hand:
  - CI fails any PR with unformatted Haskell.
  - A repo pre-commit hook auto-formats staged `.hs` — enable once per clone: `git config core.hooksPath .githooks`.
  - Claude Code also auto-formats `.hs` on edit (`.claude/settings.json`).

### Documentation
- **Re-read this file + README before each PR.** Update them when: (a) you hit a statement contradicting the code, (b) a new convention appears, (c) a command or a config key changes. NON-trigger: adding a module, tool, or endpoint that follows the stated conventions — the rules cover it, no list to grow.
- Never write hand-maintained counts, versions, or enumerations the code already knows — state the naming rule and point at the source of truth (`versions.env`, `API.Resources`, `volca dump-mcp-tools`, `volca dump-config-schema`).

### Commits & PRs
- NEVER use `git add -A` — always add specific files explicitly.
- **Keep commit messages tight**: explain *why* the change was made and any non-obvious technical choices; don't restate the diff. Subject line + a few short paragraphs max.
- **Atomic commits — one subject per commit.** If the message needs "and also", split it.
- One PR = one subject.
- **A pull request description has four parts and stops there**: the problem, the solution as the diff leaves it, any remarks worth a reader's attention, and how to test or use the result. Not the route you took to get there, not a commit by commit account, not the measurements you made and discarded on the way. Title them however you like; leave none out and add nothing else. A skill that opens its own pull requests prescribes its own body, and that prescription wins.
