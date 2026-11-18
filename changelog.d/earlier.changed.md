- The supply chain, the path to a supplier and every scope of an aggregate now
  count for one unit of the product asked, as the impacts and the inventory
  already did. Their numbers change for any process whose declared amount is
  not 1: a process written for 10 kg of cheese now reports a tenth of what it
  did in its supply chain, in the path to a supplier, and in every scope of an
  aggregate but `biosphere`, which already counted for one unit. The `direct`
  scope of an aggregate divides as the others do, by what the process makes
  net of what it consumes of its own product: a process making 1 kg and
  eating 0.1 kg of it is counted for one of the 0.9 kg it delivers. The fields
  holding these numbers are renamed (`quantity` becomes `quantityPerUnit`,
  `cumulativeQuantity` becomes `cumulativeQuantityPerUnit`, `filteredTotal`
  becomes `filteredTotalPerUnit`), so a client written for the old numbers
  stops instead of reading new ones under old names. The amount an activity
  declares for its own product is unchanged (`productAmount`), and so are the
  amounts of its exchanges.
- Every computed response carries `functionalUnit`, the amount, unit and
  product it counts for, where the scores carried a text such as "1 kg". Wire
  revision 113.
- A database whose processes each list thousands of elementary flows, as a
  package of aggregated inventories does, takes less memory once loaded: the
  index of the processes using each flow is ten times smaller. Database caches
  are rebuilt once on the first load after the upgrade.
- Such a database also takes far less memory to hold and to load. Its
  biosphere lines that carry no comment, no data quality score and no formula,
  which is nearly all of them, are kept in columns where each was an object of
  its own. A package of 4,600 aggregated processes and 29 million lines takes
  1.7 GB once loaded where it took 5.5. Its first load needs 14 GB of the
  machine's memory where it needed 25, and it reloads from its cache in
  4 seconds where it took 31. Nothing changes in what the engine answers or
  exports.
- The first load of an ILCD package builds each process as soon as its file is
  read, where it first read every file and kept them all. The same package now
  loads the first time in 6 GB of the machine's memory where it needed 14, and
  in under 4 minutes where it took nearly 6.
- A method given as an archive is read from its files once. What they come to
  is kept in a file inside the directory the archive is extracted to, and
  later starts read that file: the Environmental Footprint 3.1 package of the
  JRC took 8 seconds of every start, and takes a quarter of a second. An engine
  started on a new data directory also worked out the method's candidate
  synonyms again, 7 seconds more; they now come from the same file. With that
  method and one database, the engine answers 2 seconds after it starts where
  it took 9, or 16 on a new data directory. Each new version of the engine
  reads its methods from their files once more, and `--no-cache` reads them at
  every start.
- The descriptions of the tools use one word for each notion: process,
  consumer, supplier, amount, upstream, emissions and resources, impact
  category, method collection and swap. They used several for the same thing,
  such as activity and process, or method and impact category. No tool,
  parameter or field is renamed.
