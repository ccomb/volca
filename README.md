# VoLCA

**VoLCA** is an open-source Life Cycle Assessment engine. It loads LCA
databases, computes their inventories and impact scores, and lets you trace
every number back to the data it comes from. It runs in memory, on your own
machine or server, and answers from the command line, over HTTP, from Python
and through MCP for AI assistants.

**[Documentation](https://www.volca.run/docs/)** ·
**[Runnable examples](https://www.volca.run/examples/)** ·
**[Use cases](https://www.volca.run/use-cases/)** ·
**[Try the hosted app](https://app.volca.run/)** ·
[Website](https://www.volca.run/)

## What the engine does

- **Reads the main LCA formats**: EcoSpold 2, EcoSpold 1, SimaPro CSV, ILCD,
  openLCA JSON-LD and Brightway Excel, straight from a `.zip`, `.7z`, `.gz` or
  `.xz` archive.
- **Holds several databases at once and links them**, even across
  nomenclatures: a sector database can buy its ingredients from Agribalyse.
- **Walks the supply chain**: upstream tree and graph, a flat upstream list with
  filters, the consumers downstream, the shortest path to a supplier, and a
  breakdown by ISIC or CPC classification.
- **Computes inventories and impact scores** with sparse matrix algebra, with
  the contribution of each flow and each upstream process, and scores a whole
  database in one solve.
- **Answers what-if questions**: swap a supplier at any depth, even in another
  database, or score a process at other parameter values, and save those
  values as named scenarios.
- **Works with method collections**: reads ILCD packages, SimaPro method CSV,
  openLCA and tabular CSV, turns impact scores into single scores through
  normalization and weighting, copies a collection to change its factors with a
  journal and an undo, and compares two collections factor by factor.
- **Explains its flow mapping**: how a database flow meets a characterization
  factor is written in data files you can read and change, with coverage
  statistics and an `explain-cf` that says why a flow has, or lacks, a factor.
- **Edits databases**: change a process, delete a set of processes, copy a
  database, or divide multi-output processes by price or by mass. Every change
  is kept in a journal and replayed at the next load.
- **Checks quality**: what is malformed in a database, what it computes against
  its own norms, and what is left without a supplier after linking.
- **Exports** to SimaPro CSV, EcoSpold 1 and 2, ILCD, Brightway and openLCA,
  or as a package another engine can open when your model is built on a
  database you may not hand over.
- **Honours licences**: a database can refuse downloads, inventories or detailed
  scores, and ask its readers to accept its terms first.

## What you can build on it

The engine is a computation layer: you bring the interface. The
[hosted VoLCA app](https://app.volca.run/) is one example of what can be built
on it: a web interface over a managed instance of the engine. The
[desktop app](https://www.volca.run/desktop/) is another, the same interface on
your own machine. The [runnable examples](https://www.volca.run/examples/) are
Python scripts that import, substitute, compare and export through the engine,
and [Build your own frontend](https://www.volca.run/build-your-own-frontend/)
shows how to put a product of your own on top of it.

## Install

Linux and macOS (read the script before you run it):

```bash
curl -fsSL https://raw.githubusercontent.com/ccomb/volca/main/install.sh | sh
```

Windows (PowerShell):

```powershell
iwr -useb https://raw.githubusercontent.com/ccomb/volca/main/install.ps1 | iex
```

The installer downloads the latest release and its reference data, checks both
against their checksums, and puts a `volca` command in `~/.local/bin` (on
Windows, `%LOCALAPPDATA%\volca\bin`). The
binaries are also on the [releases page](https://github.com/ccomb/volca/releases).
The Python client is `pip install pyvolca`. To build from source, see
[docs/building.md](docs/building.md).

## Getting started

Describe your databases and method collections in a TOML file. The
[`volca.toml`](volca.toml) of this repository is a commented starting point;
the smallest useful one names a database and a method collection:

```toml
[[databases]]
name = "agribalyse"
path = "DBs/agribalyse.csv"
load = true

[[methods]]
name = "EF-3.1"
path = "DBs/EF-3.1.zip"
```

Start the server once: it loads the databases into memory. Then query it from
another terminal, or interactively:

```bash
volca --config volca.toml server --port 8080
# The API, to explore in a browser: http://localhost:8080/api/v1/docs

volca --config volca.toml --db agribalyse activities --name "wheat" --geo FR
volca --config volca.toml --db agribalyse inventory PROCESS_ID
volca --config volca.toml --db agribalyse impacts PROCESS_ID --method "Climate change"

volca --config volca.toml repl
```

To connect an AI assistant, point its MCP client at the same server:

```json
{ "mcpServers": { "volca": { "url": "http://localhost:8080/mcp" } } }
```

The [CLI first steps](https://www.volca.run/docs/first-steps/cli/) walk
through the same thing with a public database you can download.

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

Memory is not a fixed cost per database. The collector copies, so it keeps room
to copy into and hands it back to the system lazily: the process holds two to
three times what the loaded database itself has to hold, and it holds it longer
on a machine with memory to spare than on a small one. Size a machine on the
peak, not on what the data weighs.

## Documentation

The full documentation is on the site, at
**[www.volca.run/docs](https://www.volca.run/docs/)**:

- [Ways to use VoLCA](https://www.volca.run/docs/guides/entry-points/): the
  hosted app, the desktop app, the CLI, the HTTP API, Python or MCP;
- [Configuration](https://www.volca.run/docs/guides/configuration/): every key
  of `volca.toml`;
- [CLI](https://www.volca.run/docs/cli/),
  [HTTP API](https://www.volca.run/docs/api/) and its
  [generated reference](https://www.volca.run/docs/api/reference/),
  [MCP tools](https://www.volca.run/docs/mcp/tools/),
  [Python client](https://www.volca.run/docs/python/);
- guides on [method collections](https://www.volca.run/docs/guides/lcia-methods/),
  [flow mapping](https://www.volca.run/docs/guides/flow-mapping/) and
  [editing databases](https://www.volca.run/docs/guides/editing-databases/).

The engine also describes itself: a running server answers its OpenAPI
specification at `GET /api/v1/openapi.json`, and `volca dump-mcp-tools` prints
its MCP tools.

For contributors, this repository holds what changes with the code:
[docs/building.md](docs/building.md) (building and testing),
[docs/architecture.md](docs/architecture.md),
[docs/flow-matching.md](docs/flow-matching.md) (the exact rule a flow follows to
meet a characterization factor) and [AGENTS.md](AGENTS.md) (the conventions).

## Contributors

VoLCA is written by Christophe Combelles, with contributions from:

- [Michiel De Backker](https://github.com/backkem): reading openLCA packages of schema versions 4 and 5, and linking a MUMPS built from source.

## License & third-party software

VoLCA is licensed under the **Apache License 2.0** – see [LICENSE](LICENSE).

Third-party components bundled with or linked into VoLCA are inventoried in
[NOTICE](NOTICE) and [THIRD_PARTY_LICENSES.md](THIRD_PARTY_LICENSES.md): they
are Haskell libraries, predominantly BSD-3 and MIT.

A running engine also exposes the same inventory as JSON at
`/api/v1/licenses` so any client can render it.
