- A calculation that adds up a process's supply chain is refused when the
  chain reaches an input that only a database not loaded would supply: a
  product no loaded database makes under that name, unit or location, a link
  to a database not loaded or to a process it no longer holds, or, in a
  database that declares a dependency, a process nobody ships. Scores,
  contributions, groups, sensitivity, reduced models, inventories and the
  supply-chain, biosphere and consumption aggregates answer 422 with a
  sentence that starts "Refused: the chain of" and names the largest of those
  inputs; the supply chain, the graph and the direct aggregate still answer
  and name them all. Loading the database that supplies them lifts the
  refusal. Before, such inputs counted as zero.
- The inputs a publisher left unsupplied on purpose (a dataset its release
  did not ship, a link a curator declined, a waste without a treatment) still
  count as zero, and every answer still names them.
- A batch of scores lists the refused processes in `refused`, beside
  `unsolvable`, and scores the others. The computed quality report counts
  them with the processes that have no score.
- A database is listed as partially linked only when some input it asks for
  would refuse a calculation, so the list and the refusals agree.
- The `compute_sensitivity` tool scores through the databases the chain
  reaches, as the sensitivity route does, instead of the root's database
  alone.
- Wire revision 124; pyvolca raises `ChainRefused` and lists a batch's
  refused processes in `BatchScores.refused`.
