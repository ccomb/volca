# fpLCA Plugin Examples

Example plugins demonstrating the fpLCA external plugin protocol.

## Protocol

External plugins communicate via **JSON on stdin/stdout**:

1. fpLCA spawns your executable
2. Sends a single JSON object on stdin
3. Reads a single JSON object from stdout
4. Plugin exits

### Request format

```json
{"action": "<action-type>", ...action-specific fields...}
```

### Action types by plugin type

| Plugin type | Action    | Extra fields                                    |
|------------|-----------|--------------------------------------------------|
| mapper     | `match`   | `query: {name, cas, compartment}`                |
| reporter   | `report`  | `data: <any JSON>`                               |
| exporter   | `export`  | `output: "/path/to/output"`                      |
| analyzer   | `analyze` | *(none currently)*                               |
| validator  | `validate`| *(none currently)*                               |

### Response format

Each plugin type returns its own response shape. See individual examples.

## Examples

### `fuzzy-match.py` — Mapper plugin (Python)

Fuzzy string matching for flow names. Uses `thefuzz` if installed, falls back to substring matching.

```bash
# Test standalone
echo '{"action":"match","query":{"name":"carbon dioxide"}}' | python3 fuzzy-match.py

# Configure in fplca.toml
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

# Configure in fplca.toml
# [[plugin]]
# name = "uppercase"
# type = "reporter"
# path = "plugins/examples/uppercase-reporter.sh"
# format-id = "upper"
# mime-type = "application/json"
```

## Writing your own plugin

1. Create an executable (any language) that reads JSON from stdin and writes JSON to stdout
2. Add a `[[plugin]]` section to your `fplca.toml`
3. Specify `name`, `type`, and `path` (relative to working directory or absolute)
4. Optional: `priority` (for ordered types like mapper/transform), `format-id`, `mime-type`
5. Set `enabled = false` to disable a built-in plugin by name
