# Configuration reference

Every key the engine reads from its TOML configuration, with what it does. This
file changes with the code: a pull request that adds, removes or changes a
configuration key describes it here. The user guide on the site explains the
same configuration with examples:
<https://www.volca.run/docs/guides/configuration/>. The commented
[`volca.toml`](../volca.toml) is a starting point, not this reference.

A TOML config file enables multi-database setups, method collections, and reference data:

```toml
# Single-path reference data (all optional). Top-level keys stay above the
# first [section] header: below one they parse as members of that section,
# which startup reports by name.
# geographies = "data/geographies.csv"         # code,display_name,parents
# chem-synonyms = "data/chem_synonyms.csv"     # formulas and names → CAS; replaces the built-in list
# substance-edges = "data/substance_edges.csv" # typed flow-correspondence edges

[server]
port = 8080
host = "127.0.0.1"             # interface to listen on; "0.0.0.0" answers the
                               # network over IPv4, "::" over IPv6
password = "mysecret"          # optional – omit to disable auth
name = "lab-archive"           # optional – how this server introduces itself over MCP
usage_log = true               # optional – keep a line per computation, see "Usage log"

[[databases]]
name = "agribalyse-3.2"
displayName = "Agribalyse 3.2"
path = "DBs/AGB32_final.CSV"
description = "SimaPro CSV"
load = true

[[databases]]
name = "my-sector-db"
displayName = "Sector DB"      # the name shown; an upload is renamed with
                               # PUT /db/{name}/display-name, its name staying
description = "Food sector"    # changed on an upload with PUT /db/{name}/description
path = "DBs/sector.CSV"
depends = ["agribalyse-3.2"]   # loads agribalyse first, then links; a score
                               # whose chain reaches a process nobody ships
                               # is then refused, that process being taken
                               # for one of the dependency's
load = true
default = false                # preselected database in the UI
deletable = false              # allow deletion via the API
geography_policy = "global"    # CF regionalization: exact | parent | global
allocation = "declared"        # how a multi-output block is read:
                               # declared | none. none keeps a block of several
                               # products whole, as the parent of databases
                               # divided by a system model (by price, dry mass
                               # or wet mass), which are derived at runtime with
                               # POST /db/{name}/derive/{newName}
licence = "CC-BY-4.0"          # the licence it is published under, by SPDX identifier:
                               # CC0-1.0 | CC-BY-4.0 | CC-BY-SA-4.0 | CC-BY-NC-4.0 |
                               # ODbL-1.0 | etalab-2.0. Its permissions follow from
                               # its text. To refuse more, write an own licence
                               # instead of `licence`:
# licence_text = "..."         #   its text, shown on its setup
# refuses = ["download"]       #   inventory | scores | download | results | resell |
                               #   paid-applications. The engine enforces three:
                               #   download (export, literature files); inventory
                               #   (exchange amounts, supply chain, tree, path to a
                               #   supplier: refused,
                               #   and an activity names its exchanges without
                               #   amounts); scores (contributing flows and
                               #   processes, sensitivity: refused, and a
                               #   score keeps its total and categories). The rest
                               #   is shown as the reader's commitment. Each level
                               #   rebuilds the one above, so refusing scores
                               #   refuses inventory and download too, and
                               #   refusing inventory refuses download.
                               #   Reached as another database's dependency,
                               #   it holds the same: refusing scores, it is
                               #   one line of the score, "dep: 42 %";
                               #   refusing inventory, the aggregated
                               #   inventory is refused and the supply chain
                               #   counts its processes in one line
# attribution = true           #   results must name the publisher (the default)
# accept = true                #   readers accept its text before reading it, or
                               #   anything built on it: refused until they send
                               #   the text's fingerprint in the volca_terms
                               #   cookie, accept in the form their
                               #   assistant shows when its client shows
                               #   forms, or have it call the MCP tool
                               #   accept_terms. New text, asked again. Off by
                               #   default
# usage = "anonymous"          #   counts each computation on its release in the
                               #   usage log, for its publisher: "anonymous"
                               #   counts only, "organizations" adds each
                               #   reader's organization and email domain.
                               #   Needs accept = true, since readers accept the
                               #   clause with the text. Absent, nothing is counted
                               # A copy is served under its source's licence; an
                               # upload's owner sets its own with PUT /db/{name}/licence
# release = { name = "ecoinvent", version = "3.12", system_model = "Allocation, cut-off by classification" }
                               # which published database this is; name and version
                               # both required. An upload's owner declares it with
                               # PUT /db/{name}/release (null clears it), and its
                               # setup shows the system models its activities state.
                               # A copy declares its own: it is made to be changed
# locationAliases = { "FR" = "France" }   # per-database location renames

# Optional patches adjust the amounts a database states, at load time.
# Selector fields combine with AND; at least one is required. Exactly one of
# `scale` (multiply) or `set-value` (replace) per patch. A patch reaches the
# inputs, the emissions, the waste sent to treatment and the avoided products
# of a process, never the rows saying what it makes. A `set-value` is in the
# unit the engine records the row in (a SimaPro row written in g is recorded
# in kg), and applies to each process a multi-output block was divided into. A patch matching no exchange logs a
# warning at load time, and a database whose patch list changed is rebuilt
# from source rather than read from its matrix cache.
#   [[databases.patches]]
#   description = "example: the preservative a trellis is no longer treated with"
#   match = { product-name-contains = "trellis", flow-name-contains = "creosote" }
#   # other selectors: activity-name-contains, location (exact), flow-name (exact)
#   set-value = 0.0

[[methods]]
name = "EF-3.1"
path = "DBs/EF-v3.1.zip"      # ILCD method package (ZIP or directory)
default = true                 # the collection a reader is offered first (at most one)

# SimaPro method CSV exports and tabular CSV are also accepted:
# path = "DBs/EF3.1_methods.csv"

# An openLCA method package holds several methods; package-method names the one
# to read, with its normalisation and weighting sets:
# path = "DBs/openLCA LCIA Methods.zip"
# package-method = "EF 3.1 Method (adapted)"

# "plain-indicators" (land occupied, water used, fossil CO2, methane, primary
# energy, waste heat, cadmium, every factor 1.0) is built into the engine and
# loaded beside the collections listed here. Name it with a path to replace it,
# or with no path to switch it off or to give it scoring sets and patches:
#   [[methods]]
#   name = "plain-indicators"
#   active = false

# Optional scoring sets on a method collection: named variables per impact
# category, computed expressions, then normalization + weighting factors
# producing a single score (see www.volca.run/docs/guides/single-score/).
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
# CF is the single answer – for matching references that flattened
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
filters = [{ system = "ISIC", value = "01", mode = "contains" }]  # mode: exact | contains | starts-with

[[skills]]                     # a skill served to assistants over MCP
name = "local-footprint"       # lowercase letters, numbers and hyphens
description = "Footprint of a food product with the database this instance holds."
text = '''
# Footprint of a food product
1. Search the process with `search_activities`...
'''

# [hosting] tunes upload/API limits when the engine runs behind a manager:
# max_uploads, max_upload_mb, max_loaded_uploads, api_access,
# upgrade_upload, upgrade_api, upgrade_vm_size, read_only, read_only_message,
# max_batch_activities, max_top_flows, max_concurrent_scoring
```

`max_uploads` bounds how many databases of their own a caller may keep, and
`max_loaded_uploads` how many of those may sit in memory at once. Both count
only uploaded databases – the ones the config declares are what an uploaded
inventory links against, so counting them would forbid the very thing
uploading is for. A copy spends the same budget as an upload. Negative means
unlimited; with no `[hosting]` section (local, CLI, desktop) neither applies.

`read_only = true` makes the instance answer every analysis request and refuse
every state change: loading and unloading, uploads, deletes, copies, relinks,
dependency edits – and `POST /api/v1/shutdown` and `/api/v1/idle-timeout/{n}`,
which decide how long the process lives. Refusals are `403` on REST and tool
errors on MCP; nothing is silently ignored. This is what makes a single
instance safe to put in front of many unrelated callers, none of whom should be
able to change the working set or end the server for the others. Every refusal
carries one sentence explaining the stance; `read_only_message` replaces it
with the operator's own words, and `GET /api/v1/hosting` reports both flags so
a client can say so before attempting a change.

`max_batch_activities` and `max_top_flows` bound what one scoring request may
ask for, which matters on the same kind of instance: scoring every activity of
a large database holds all the cores for about a minute, and the other callers
wait behind it. The first caps how many activities a batch score covers, and
so also refuses the computed quality report on a database with more
activities than that, since the report scores all of them; the second caps
the top contributing flows a batch asks for per activity. Both hold on REST
and MCP alike, and a refusal says the limit and what the request asked for.
Absent, they set no limit. `GET /api/v1/hosting` reports both (`null` for no
limit), so a client can size its requests instead of learning the limit from a
refusal.

`max_concurrent_scoring` bounds how many scoring requests compute at once;
the others wait their turn rather than being refused. Every request in
flight holds its solutions in memory, and many at once can exhaust it. Since
nothing is refused, the hosting route does not report it.

The `depends` field ensures dependency databases load first and their flows are available for cross-database linking. Setting `load = true` on a database transitively loads all its dependencies.

A database's dependency set is **pinned**: it is seeded automatically when the database is first staged (the minimal set of supplier databases needed to resolve its links), and from then on it is authoritative. A plain `relink` re-resolves links *within* the pinned set only – it never silently adds another loaded database. Edit the pin explicitly with `add-dependency` / `remove-dependency`, then `finalize`; a dependency that would close a loop, or a database depending on itself, is refused, as the configuration file refuses it; the new set is written to the matrix cache and reused on every later open. This is how you restrict a consumer (e.g. an inventory built against a single Agribalyse version) to exactly the supplier databases it should depend on, even while other versions stay loaded for other consumers. (The one exception is a *mapping* relink – `relink` with a `depDb` and an alias CSV – which pins that chosen dependency in-memory if it isn't already, so a `copy → delete → relink` pipeline composes in one pass; links to the other pinned dependencies are preserved, not dropped.)

A database uploaded as a **package** (the zip `export --package ro-crate`
writes) arrives with what its description says: the licence it is published
under, and the releases of the databases it was built on, kept in its
`meta.toml` as `[[requires]]` blocks. The upload is refused when the export it
carries does not match the digest the description gives. Such a database links
only to a database whose declared release is one it requires, and the
databases that are get loaded before it is staged; with ecoinvent 3.11 and
3.12 both loaded, a model built on 3.12 links to 3.12 alone. Its setup lists
each required release as `satisfied` (with the databases of that release),
`missing` (with the databases of the same name at another version, which may
stand in for it) or `substituted`. A reader who accepts another database in
place of a missing release says so with
`POST /db/{name}/accept-substitution` (`{"release": {...}, "database": "ei-311"}`):
the choice is written to its `meta.toml`, the setup links again within it, and
scores computed on it are no longer the author's.

## Skills

A skill is a set of instructions an assistant loads when a question calls
for it, or when the person types `/<server>:<name>`. The engine serves its
skills over MCP, to clients of the 2026-07-28 revision that read the skills
extension; other clients see none, and the tools work as before.

The engine has built-in skills: `skills/list` names them, and their texts
are in `src/Skill/Builtin.hs`. Each `[[skills]]` entry adds a skill,
or replaces the built-in one of the same name. The engine writes the file's
header from `name` and `description`, so the description must be printable
on one line and cannot hold `---`; a name or description that breaks these
rules stops the engine at start, with the reason. Changing a skill takes a
restart.

## Usage log

With `usage_log = true` under `[server]`, the engine keeps one line for each computation on a process: reading it (its exchanges, supply chain, path, consumers), its inventory, its scores, what weighs in a score, or a comparison. A line names the kind, the database and process, what the database calls the process (`heldAs`: its activity name, product name and location; null once the database no longer holds it), the time and the reader, copied from the request's `Volca-Reader` header when one is sent. Only computations that answered are kept, from REST and MCP alike, and only when a database read asks its computations counted (`usage` beside `accept = true`, so its readers accepted the clause): without a release nothing says whose data was read, and without the clause nobody asked for a count. The databases read are the one computed on and every database it depends on, transitively, a copy or a re-keyed database counting as a read of the one whose files it reads. `counted` lists the releases of those that ask, and only those, each with `disclosure`: `anonymous`, or `organizations` when the publisher sees the reader's organization and email domain. A batch keeps one line per process it scored, and a comparison one per side.

The lines live in memory. `GET /api/v1/usage?after=N` returns those numbered after `N` (a page at a time, `more` saying whether others wait) with the `boot` they belong to; each start of the engine draws a new one and numbers from 1 again. `DELETE /api/v1/usage?boot=B&through=N` forgets the lines a collector kept, and is refused with 409 for a `boot` from another start. A read-only engine answers both, since the log is not its data, and neither keeps the engine from going idle. When nobody collects, the oldest lines go past 100,000 and the engine says how many. The engine trusts the header as sent: the server in front of it decides who a reader is, and is the one to strip the header from what callers send.

A change to a database, a method collection or the scenarios of a database is signed the same way, whether or not the usage log is on: with `Volca-Reader` and `Volca-Author-Handle` (the account's public name) both sent, the journal line keeps `"by": {"account", "handle"}`, the history returns the name as `by`, and an export's `CHANGES.md` says `By: @name`. Without either, the line is unsigned. The server in front strips both headers from what callers send.
