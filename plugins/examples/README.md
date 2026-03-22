# VoLCA Plugin Examples

Example plugins demonstrating the VoLCA external plugin protocol.

## Plugin Handle Types

VoLCA has 8 pluggable handle types covering the full LCA pipeline:

```
[Import] → [Transform] → [Map] → [Validate] → Compute → [Analyze] → [Report]
                                                 (core)
                          [Search] (orthogonal)
```

| Type | Built-ins | Purpose |
|------|-----------|---------|
| **importer** | ecospold2, ecospold1, simapro-csv, ilcd | Format detection and data loading |
| **mapper** | uuid, cas, name, synonym | Match method CFs to database flows |
| **searcher** | name-searcher, cas-searcher | Query loaded data by name or CAS |
| **analyzer** | lcia, hotspot | Post-computation analysis |
| **exporter** | ecoinvent-matrix, debug-matrix | Export data to files |
| **reporter** | json, csv, table, pretty | Format output for display |
| **transform** | *(none built-in)* | Modify data before computation |
| **validator** | *(none built-in)* | Check data quality pre/post computation |

List all registered plugins:
```bash
./volca.sh --config volca.toml plugin list --format json
```

## Protocol

External plugins communicate via **JSON on stdin/stdout**:

1. VoLCA spawns your executable
2. Sends a single JSON object on stdin
3. Reads a single JSON object from stdout
4. Plugin exits

### Request format

```json
{"action": "<action-type>", ...action-specific fields...}
```

### Action types by plugin type

| Plugin type | Action     | Extra fields                                     |
|-------------|------------|--------------------------------------------------|
| mapper      | `match`    | `query: {name, uuid, cas}`                       |
| reporter    | `report`   | `data: <any JSON>`                               |
| exporter    | `export`   | `output: "/path/to/output"`                      |
| analyzer    | `analyze`  | *(none currently)*                               |
| validator   | `validate` | *(none currently)*                               |
| searcher    | `search`   | `query: {text, limit}`                           |

### Mapper response format

```json
{"uuid": "flow-uuid-string", "strategy": "fuzzy"}
```

### Response format

Each plugin type returns its own response shape. See individual examples.

## Examples

### `fuzzy-match.py` — Mapper plugin (Python)

Fuzzy string matching for flow names. Uses `thefuzz` if installed, falls back to substring matching.

```bash
# Test standalone
echo '{"action":"match","query":{"name":"carbon dioxide"}}' | python3 fuzzy-match.py

# Configure in volca.toml
# [[plugin]]
# name = "fuzzy-mapper"
# type = "mapper"
# path = "plugins/examples/fuzzy-match.py"
# priority = 25
```

### `uppercase-reporter.sh` — Reporter plugin (Shell)

Trivial shell reporter that upper-cases all string values in JSON output. Requires `jq`.

```bash
# Test standalone
echo '{"action":"report","data":{"name":"test"}}' | bash uppercase-reporter.sh

# Configure in volca.toml
# [[plugin]]
# name = "uppercase"
# type = "reporter"
# path = "plugins/examples/uppercase-reporter.sh"
# format-id = "upper"
# mime-type = "application/json"
```

## Writing your own plugin

1. Create an executable (any language) that reads JSON from stdin and writes JSON to stdout
2. Add a `[[plugin]]` section to your `volca.toml`
3. Specify `name`, `type`, and `path` (relative to working directory or absolute)
4. Optional: `priority` (for ordered types like mapper/transform/searcher), `format-id`, `mime-type`
5. Set `enabled = false` to disable a built-in plugin by name

## API Endpoints

### Analyze endpoint

```
GET /api/v1/db/{dbName}/activity/{processId}/analyze/{analyzerName}
```

Available analyzers: `lcia`, `hotspot`. The hotspot analyzer returns top-20 flows by contribution to each loaded impact category.
