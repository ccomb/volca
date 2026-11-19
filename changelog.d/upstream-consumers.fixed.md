- The supply chain listed a process of a dependency database twice when the
  chain reached that database by two routes, each line with part of the
  quantity. It now lists it once, with the whole quantity.
- The consumer count of a supply chain row now counts processes, not lines: a
  process buying on two lines counts once. It also counts the consumers in
  another database, so a supplier bought only from another database no longer
  shows 0.
- The graph of a supply chain no longer receives the same link twice when a
  process buys from another on two lines, or when a dependency database is
  reached by two routes.
