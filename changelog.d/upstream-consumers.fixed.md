- The upstream list showed a process of a dependency database twice when it
  reached that database by two routes, each line with part of the quantity.
  It now shows it once, with the whole quantity, and the minimum quantity and
  the maximum depth are read on that whole quantity.
- The total of upstream processes counted such a process twice.
- The consumer count of an upstream row now counts processes, not lines: a
  process buying on two lines counts once. It also counts the consumers in
  another database, so a supplier bought only from another database no longer
  shows 0.
- The graph of the upstream processes no longer receives the same link twice
  when a process buys from another on two lines, or when a dependency
  database is reached by two routes.
