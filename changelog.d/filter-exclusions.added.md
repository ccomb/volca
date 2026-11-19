- A search for activities can leave activities out. In a search address,
  `exclude-name` and `exclude-location`, each of which may be repeated, remove
  the activities a name or a place finds, and a classification filter can be
  written `not-contains` or `not-exact`, which removes what it matches where
  the other modes keep it. The supply chain and the consumers read the same.
  An assistant over MCP gets the same from `search_activities`, through
  `exclude_name`, `exclude_location` and a list of classification filters that
  each give a system, a value and a mode; a search made only of exclusions
  needs no name. A place excluded removes every place that answers for it.
- Deleting activities by selection can leave activities out the same way, with
  `excludeName` and `excludeLocation` in the request, `--exclude-name` and
  `--exclude-location` on the command line, and `exclude_name` and
  `exclude_location` in pyvolca, and it can say what it would do before doing
  it. With `dryRun`, `--dry-run` or `dry_run=True` the engine answers the same
  counts and the same refusals as the real call, and changes nothing.
- A delete can keep the activities the remaining ones still use. With
  `spare` true, every selected activity that one of the remaining activities
  buys from, however far up its supply chain, is kept, so that no remaining
  activity loses a supplier and has its line handed to another database. The
  response says how many were kept in `spared`.
