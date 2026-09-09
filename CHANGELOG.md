# Changelog

## [Unreleased]

### Added
- pyvolca reads what the last four wire revisions added, and stops warning that
  the engine is newer than it is. A technosphere or waste exchange carries its
  `supplier_claim`, how the source designated its supplier before linking
  answered: by the product row, by an activity identifier, by an activity name,
  or by the number a dataset is published under. An activity detail carries its
  `native_id`, the identifier its source file gave the block it was read from.
  A database listing carries the `allocation` key its blocks were divided by
  and the `source` it was derived from. The setup report's supplier
  ambiguities are documented where a reader looks for them.

### Changed
- An exchange that resolved to no supplier now reports `activityLinkId` as
  null. It used to report the all-zero UUID, a value that reads as an
  identifier and is not one: every consumer had to know that one UUID means
  "none", and one that did not know charged a link to a process that does not
  exist. The engine no longer has a way to write it. Redefining a published
  field is what makes the next release a minor rather than a patch.
- An exchange no longer carries a process link, and responses no longer send
  `processLinkId`. The field held a matrix row index, a number minted when the
  matrices are built and renumbered whenever they are rebuilt, so no reader of
  a source file could ever fill one and none ever did: the key was null in
  every response the engine has ever sent. The four places that asked whether a
  link resolved that way now ask the one question there was ever an answer to.
- What a source says about the supplier of an exchange is now recorded as what
  it is. Every format designates a supplier differently: EcoSpold 2 by the
  activity's identifier, EcoSpold 1 by the number its dataset is published
  under, a Brightway workbook by the activity's name, SimaPro and ILCD by the
  product line itself. All of them had to say it through the same field that
  linking then overwrites with what it resolved to, so the question disappeared
  the moment it was answered, and two formats had a channel of their own
  alongside. An exchange now carries that claim in a field of its own, which
  linking never rewrites, and reports it as `supplierClaim`, which replaces the
  `supplierActivity` of the same release, never shipped. Caches built
  before this are rebuilt on the next load. Wire revision 23.

### Fixed
- A SimaPro export now writes a substitution once. The `Materials/fuels` section
  holds inputs, but the line that built it excluded only the reference product, so
  a coproduct and an avoided product were written there as well as under the header
  each already has. On re-import the duplicated credit met an equal input and
  cancelled, so an activity that exists to record an avoided burden recorded
  nothing, and every chain standing on it read heavier. A round trip of a large
  database moved amounts on more than half of a sample of twenty inventories, by up
  to a factor of three.
- A Brightway Excel inventory now links to the supplier activity its rows name.
  A workbook of that format names each supplier on two columns, the activity
  and the product it makes, and only the product was kept. In a released
  background database, "electricity, medium voltage" in GLO is the reference
  product of 27 different activities, so the product name alone could not say which one a row
  meant and the ranking returned whichever it happened to return. Most of the
  others are cut-off co-outputs that carry no burden, so a wrong pick was quiet
  as well as wrong. The activity name is now read, matched first, and written
  back out on export. It also decides between the candidates a synonym group
  returns, where the dependency spells the product otherwise, and it decides
  inside the workbook as well: a product several of the file's own activities
  make is linked to the one the row names, and when no row names one the load
  report says which activity was chosen and how many produced that product.
  Caches built before this are rebuilt on the next load. Wire revision 23.
- A cross-database link whose candidates tie inside one dependency now says so.
  Several activities scoring the same means nothing in the data chose between
  them, and until now the load said nothing while picking one. The load report
  and the database setup report both list those inputs, the activity linked to
  and how many tied, so the supplier meant can be named. Ties across two
  dependencies were already reported; this is the tie inside one. Wire
  revision 23.
- Loading an EcoSpold 1 or EcoSpold 2 file now says when the file named neither
  its activity nor its reference unit. Such a dataset still loads, under a
  placeholder no reader downstream can tell from a name the file really
  carried, so a database could show an activity called "Unknown Activity"
  measured in "UNKNOWN_UNIT" with nothing in the log to say where it came from.
  Both readers name the file they were reading when they say it, which the
  EcoSpold 2 reader did not do for the remarks it already made.
- An EcoSpold 1 database no longer reports supplier gaps that cannot exist.
  A file of that format files waste with no modelled treatment under a
  category of its own, on an input group, because the format has only four
  flow types and this is a fifth. The loader read the input direction
  literally and turned every such row into a demand waiting for a supplier,
  which no database can ever close: the completeness report filled with edges
  that had nothing to point at. Those rows are elementary flows now, of the
  same medium the equivalent rows of the other formats already use, so a
  method characterizes them and the report only counts what a supplier could
  answer. An export writes them the way it reads them: waste that names no
  treatment goes out as an inventory flow, waste that is consumed by one goes
  out as a technosphere input, and an output that names a treatment is refused
  rather than written with the link silently dropped. Two things change for a
  reader beyond the gap report: such a row no longer carries a waste role, so
  the activity holding it stops being described as treating waste; and in a
  file this engine wrote itself, where the row was on the output side, it used
  to be matched by name to a treatment carried by another loaded database, and
  an elementary flow is matched against methods instead. Caches built before
  this are rebuilt on the next load.
- Two databases read from two formats now describe one physical flow the same
  way. Every format spells a compartment's medium differently: an EcoSpold 2
  file writes `natural resource`, an EcoSpold 1 file writes `resource`, a
  SimaPro file heads the section `Raw`. Each was stored as written, so the same
  extraction carried a different medium depending on which file it came from,
  and every reader, writer and comparison that needed them to agree carried its
  own list of spellings. The medium is now one value with one name, read at the
  boundary and written back in each format's own vocabulary, so a database
  exported and read again comes back with the medium it went out with. Two
  consequences are visible: an inventory from a SimaPro or EcoSpold 1 source
  now reports `natural resource` where it used to report `resource`, and the
  elementary flows of such a database change identity, since the medium is part
  of it. Caches built before this are rebuilt on the next load.
- A technosphere input whose amount is zero is now kept. A SimaPro or Brightway
  Excel file writes one when the author disabled an input or a parameter
  evaluates to zero in this scenario, and dropping it lost what the file said
  without a word, while the loader went on reporting a link rate over the rows
  that survived. No computed number moves, a zero coefficient having no weight
  in any matrix; an export now carries the rows its source carried.
- Waste that leaves a system with no treatment modelled for it is now
  characterized. A SimaPro CSV files such waste in a section of its own, and a
  method that scores it writes the same word in its compartment column. The
  loader was putting those rows on the technosphere waste side instead, where
  no method looks and where a row with no producer never reaches an inventory
  at all: a database could carry thousands of them and still score zero on a
  waste category the method covers. They are elementary flows now, of the
  medium the section is named after, and the exports that have a section for
  them still write them there. The same factor also reaches the equivalent
  flows of an EcoSpold 2 source, whose compartment table gained one line
  (data version 4) pairing its waste indicators with that medium; its
  indicators that count recovered energy or secondary materials are left where
  they are, being nothing of the kind. On a database read from those formats
  these rows now appear as inventory flows rather than waste exchanges, so a
  waste-flow search no longer returns them and a cut-off report no longer
  counts them. One export loses out: ILCD has no category that reads back as
  this medium, so a database carrying such flows is now refused at that export
  rather than written in a shape the reader turns into something else. Caches
  built before this are rebuilt on the next load.
- A waste treatment is published in the universal matrix format with the sign
  the format uses. That format is unnormalised: a coefficient is positive when
  the activity produces and negative when it consumes. A treatment records the
  waste it treats as its reference product with a negative amount, and the
  engine's own coefficients are normalised by that reference, so the whole
  column came out reversed while only its diagonal was corrected. A solver
  reading the export saw an activity that consumed waste on the diagonal and
  produced its own inputs everywhere else.
- An export of a large database reaches the client again. The warnings an
  export collects travel in a response header, one line per warning, and a
  database with thousands of multi-output activities produced a header longer
  than an HTTP client will read: the request died on the header and the archive,
  complete on the server, never arrived. The header now carries as many
  warnings as fit and ends with a line saying how many more there were.
- A Brightway Excel export can now be opened by the tools it is written for.
  The `.xlsx` was missing the manifest a spreadsheet reader looks up before
  anything else, so openpyxl, bw2io and Excel all refused the file even though
  the sheet inside it was complete. VoLCA's own importer reads the sheet
  directly and never needed the manifest, which is why the file looked fine
  from here.
- An EcoSpold 2 database no longer reports supplier gaps that cannot exist.
  A source of that format declares a handful of inventory indicators, which
  are elementary flows recording how much waste an activity accounts for so
  that a method can characterize the total. The loader read the word "waste"
  in their compartment and moved them to the technosphere waste side, where a
  dataset that writes one as an input reads as a process demanding a supplier
  no activity can be. Two of them are written that way throughout, which was
  enough to leave thousands of unsupplied edges on a database that is in fact
  complete. They are elementary flows again: they appear in the inventory
  where a method can characterize them, and the gap report is empty. Caches
  built before this are rebuilt on the next load.
- The functional unit reported beside a score now says what the score is
  actually per: one unit of the reference product. It used to repeat the
  amount the source declares the process produces, which for a coproduct is
  rarely one: the cream of an Agribalyse cheese block is stated at
  0.0686462 kg, so the score came back labelled `0.07 kg of cream` while the
  number itself was per kilogram, every coefficient of the column having been
  divided by that amount. The same applies to a reference product ingested in
  a canonical unit, a 1 kWh product read as 3.6 mj and scored per megajoule.
  The Python client's documentation said the same thing wrongly and now
  states that `product_amount` describes what the block produces, not the
  basis of the score.
- An EcoSpold 2 dataset whose block a key divides into several processes now
  says that only one of them will be kept. A process is identified by its file
  name there, which names a single product, so the coproducts a key had just
  separated all claimed the same identity and the registry kept the last one:
  a database quietly missing the very products the key was named to produce.
- A released Linux binary now reports the commit it was built from, and a
  release binary the tag it carries. Both used to come out as `unknown` and
  an empty tag on Linux alone (`volca --version`, `GET /api/v1/version`),
  because that build runs in a container without git and had nothing to
  read; the macOS and Windows binaries were already correct.

### Fixed
- Writing an activity whose supplier lives in a dependency database no longer
  produces an exchange that contributes nothing. The write was accepted, but
  the supplier's product flow was never copied into the database being
  written, and cross-database relinking reads the consumer's own flow table:
  finding nothing there it dropped the exchange in silence, leaving an empty
  supply chain and a score of zero. The flow is now copied in with its unit
  remapped, exactly as a biosphere flow from a dependency already was, and a
  product stated in a unit the writing database does not have is refused
  rather than copied. The same applies to an inventory edit that adds such a
  line, and to replaying one from a journal.

  An amount to a supplier in another database must now be stated in that
  supplier's product's own unit. A cross-database link carries the product
  flow's unit rather than the exchange's, and the amount is converted from it,
  so a convertible-but-different unit would have entered the matrix as a
  number in the wrong one (two tonnes demanded as two kilograms) where the
  same exchange to a local supplier is converted. It is refused, naming the
  unit to restate it in.

### Added
- An activity now reports the identifier its source file gave it, beside its
  location: SimaPro's `Process identifier`, which on Agribalyse reads
  `AGRIBALU000000003103728`, and the number an EcoSpold 1 dataset is published
  under. It is what a reader copies to find the dataset back in the file it
  came from. EcoSpold 2 and ILCD name a dataset by a UUID, which is already the
  first half of every process id, so they report nothing new here. Wire
  revision 22.
- Searching a process by the identifier its source file gave it now lands on
  that dataset alone: type a SimaPro `Process identifier` into the search and
  you get the block it names, every product of it, and nothing that merely
  resembles it. Half an identifier names no dataset and searches as it always
  did, because the codes of one database share their prefix and answering half
  the database to somebody who named one dataset would be no answer at all.
- Searching a process by the identifier its source file gave it now brings that
  dataset to the top: type a SimaPro `Process identifier` into the search, whole
  or just the part that tells it apart, and the block it names comes first, all
  of its products, ahead of the ordinary results rather than instead of them.
  Nobody types one whole, so four characters are enough: on the SimaPro export
  of ecoinvent 3.9.1, all 21 456 identifiers are 23 characters of which the
  first 18 are identical, and the last five are already unique. A fragment
  shorter than four characters is read as a word, since below that nothing ever
  singles out a dataset.
- A database can now be asked for another allocation key without editing the
  configuration file: `POST /api/v1/db/{name}/derive/{newName}?allocation=wet
  mass`, the `derive_database` tool, and `Client.derive_database` in pyvolca.
  Until now naming a key meant writing a second `[[databases]]` entry and
  restarting, which nobody hosting an engine can do and which cost the person
  running one their own file. The source is untouched and both stay loadable,
  so the two allocations of one study can be compared side by side.

  This is a load, not a copy: the key decides the inventory of every process a
  load produces, so the sources are read again. Seconds to minutes on a large
  database, against the milliseconds a copy takes, and it answers with what a
  load answers with.

  A key that divides no block of the source is refused, naming both numbers:
  such a load is that database under a second name, costing a second database
  in memory and a second matrix cache on disk. So is the key the source
  already reads under, before the load rather than after it: that duplicate is
  the one no count of divided blocks can see, since dividing the same blocks a
  second time divides them just as well. An unreadable key is refused with the
  list of keys there are, never read as `declared` -- in a `meta.toml` either,
  where it now leaves the database out of the listing and says so, rather than
  handing back declared shares under a name promising otherwise.

  A database's recorded source is the same before and after a restart: a copy
  named none in memory and its own source's name once read back.

  The key a database was divided under, and the source whose files it reads,
  now travel with its status, so a column of shares can say whether the source
  stated them or a property recomputed them. Wire revision 20.
- An activity summary now names the source block the process came out of, and
  says how many products that block holds. A listing showing the five
  coproducts of one dairy block had no way to gather them: the process id
  names the product, not the block it was written in. The block name is for
  comparing
  and never for reading, and the count is what lets a page say it is showing
  only part of a block rather than the whole of it. Wire revision 21.
- A flow search can name several kinds at once: `kind=biosphere,waste` keeps
  what is exchanged with nature or discarded and nothing else, in one listing.
  `search-counts` already answers that pair as a single number, and there was
  no way to list it: a search box putting a count beside a tab had to show a
  count and a table that disagreed. One name it cannot read refuses the whole
  parameter, the way one unreadable name always did. Wire revision 19; a
  single kind is written exactly as before.
- A database can be divided on the mass of its products instead of on the
  shares its source declares. A database entry names the key it is read
  under: `allocation = "declared"` (the default, unchanged), `"dry mass"` or
  `"wet mass"`. The Abondance cheese block, whose source divides it on dry
  matter, gives 51 % to the cheese under its own key and 12 % under its wet
  mass: the two are different questions, and the key says which one is being
  answered.

  A line the source states a property for is believed. Where it states none, a
  line already written in a mass unit is its own wet mass, which is what makes
  the key computable on the formats that carry no property table at all
  (SimaPro, ILCD, EcoSpold 1). A dry mass is never read from an amount:
  nothing in a wet kilogram says how much of it is water. A block the key
  cannot weigh is not divided at all and has no column in the matrix, the same
  refusal a block stating no share gets: asking for a key a database cannot
  provide costs those blocks rather than inventing numbers for them.

  A process with a single product output is left as its source wrote it. There
  is nothing there to divide, and its declared share is the only statement in
  existence about the outputs the file does not carry.

  A product its source declares at 0 % stays at 0 %, and the key divides what
  is left among the others. That zero is a modelling decision, usually a
  residue its author took out of the block, and recomputing it into a share
  would undo the decision without saying so.

  The key belongs to the load, so the same source under two keys is two
  databases: configure the same path twice, under two names, to compare them
  side by side. A cache records the key it was built under and is not served
  to a load asking for another.
- A technosphere exchange now reports the physical properties its source
  states of that line, the dry and wet mass (`properties`). EcoSpold 2 writes
  them per unit of the line, so a board of 1 m3 declaring 614.4 kg of dry
  matter per m3 is reported as 614.4 kg; a property stated in something that
  is not a mass keeps its own unit rather than being read as kilograms, and a
  property this engine has no field for is dropped rather than guessed at.
  This is the material an allocation key other than the one the source
  declares is computed from. Wire revision 18.

  Every database cache is rebuilt once on the first load after this release:
  the cached exchanges say less than the loader now reads.
- A flow search result now reports how many activities make the flow
  (`producerCount`), which is the number of ways the database offers to
  produce it: one on most of a French agricultural database, up to a few
  thousand on a large industrial one. It is absent on a flow no activity can
  produce, rather than zero, which would be the different statement that
  nothing makes it.
- The activities of a flow can be asked for one side of it: `?role=producer`
  lists the activities that make it, `?role=consumer` those that use it. The
  route answered both at once before, which is two questions in one list.
  Asking with no `role` still answers both. Wire revision 16.
- An ILCD dataset that states an allocation key is now read with it. The
  format carries a fraction per product output, which the parser dropped, so
  a multi-output ILCD dataset had no shares to split on and was refused as
  unallocated. Only the fraction a product allocates to itself is read: the
  format also allows a matrix allocating one exchange to another product, and
  a dataset written that way is still refused rather than split on a number
  that meant something else.
- A new route and MCP tool answer how many processes, products and flows one
  query matches, in a single call (`db/{dbName}/search-counts?q=`,
  `count_search_matches`). The three are disjoint and together cover the
  database, so the counts partition what a query found: a term matching 2
  processes and 300 flows is a substance name, not a product. A search box
  that labels three tabs with counts no longer costs three searches per
  keystroke. A blank query is refused rather than answered with three zeros,
  which would read as "this database has nothing", and so is one made only of
  punctuation, which reaches the matchers as no words at all. The counts take
  the same `sort` and `exact` the listing will use, because those decide which
  matcher runs and therefore how many rows there are. Wire revision 17.
- The products of a multi-output block now each report what their share
  would be if the allocation key were their mass (`massAllocationPercent`), beside
  the share the source declared. Nothing is split and nothing is scored
  differently: it is a second column to read against the first, and the
  impact per kilo under one key over the other is the quotient of the two
  numbers. The Abondance cheese block of a French agricultural database
  declares 51.4 % to the cheese where its mass is 11.7 % of the block, so
  a kilo of that cheese carries 4.4 times more under the declared key than
  under the mass. Amounts are converted to kilograms before being summed,
  and a block whose products are not all stated in a mass reports nothing
  rather than a number read off mismatched units. Wire revision 15.

## [0.12.0] - 2026-09-02

### Added
- Reading an EcoSpold 1 database from its files now reports, as a warning,
  each input whose dataset number and declared location name two different
  datasets of the same product. A database served from its cache is not
  re-read, so the report appears once, when the cache is built. The number is what the file links and it still wins, as it
  did before; the report names the location declared, the location the number
  resolved to and the number itself, so the contradiction is visible without
  an external diff. BAFU 2026 v1 has eleven, most of them power plants whose
  gas input carries the number of their own country's supply under an `RER`
  label, and the published BAFU results follow the number.
- Each unmapped factor of the method mapping report now carries the
  compartment the method states (`compartment`), so a consumer can see a
  vocabulary gap without reading the log. Wire revision 13.

### Fixed
- A database's matrix cache (`volca.cache.<name>.bin.zst`, beside its
  source) is now trusted only when the unit table and the location aliases
  it was built with are the ones in force. It used to be read back
  regardless, so a cache built while the unit table was missing kept every
  exchange it could not convert unlinked at every later start (one SimaPro
  database: 1,335 unlinked from such a cache, none from a fresh read). The
  log now says which of the two changed, and every existing cache is
  rebuilt once.
- The method mapping report no longer counts as matched a factor whose only
  name hit is filed under another compartment vocabulary ("air" against
  "emissions to air"). Such a method used to report 82% coverage and score
  every activity at zero; it now reports those factors unmapped, and the log
  says which two vocabularies never met and that a `[[compartment-mappings]]`
  table bridges them. Reported by @mklarmann (#346), whose independent
  Brightway comparison showed the engine was right and the report was not.
- A database whose inventory comes from a database it depends on is now
  characterized with the same factors as that database itself. The cascade
  that ties a method's factors to a database's flows was built over the root
  database's own flows alone, while scoring reads the merged inventory of the
  whole cross-database solve. A dependency's flow therefore only ever reached
  the rungs that need no flow to point at: never the synonym bridge, never the
  proxy edges, never the regional projection. Measured on eight processes
  written against Agribalyse, that lost 13% of fossil climate change; and
  because a database with no regional factor of its own is scored on the flat
  path, it also put water use 78 times too high, land use at zero and fossil
  resource use at 44% of its value. The mapping is now built over the flows
  the database reaches, its dependencies' included, and the scores match the
  dependency's own. A database that depends on none is unaffected.
- A database taking part in a cross-database regionalized score no longer
  contributes zero when it carries no regional factor for the method. Its
  emissions were dropped whole rather than scored against the broadcast
  factors, which is where a flow with no regional factor belongs.

### Changed
- The four reference tables (flow synonyms, compartment mapping, units,
  energy densities) and the geographies are built into the engine. A kind a
  configuration says nothing about runs on the built-in table; a kind it
  lists is exactly what it lists, so a configuration written before this
  keeps its tables and its results. It names the built-in with a path to
  replace it with a file, with no path to keep it beside its own, or with no
  path and `active = false` to switch it off. An engine run with no
  configuration at all now characterizes an EcoSpold 1 database correctly
  instead of scoring it at zero. The data bundle a release ships is unchanged
  and still honoured through `VOLCA_DATA_DIR` for configurations that point
  into it; the version route reports the built-in data version when the flow
  registry is the built-in one, and the bundle's own otherwise, as before.
  Reported by @mklarmann (#348).
- An activity the engine cannot allocate is now refused a score, and says so,
  where it used to be scored on its reference product alone with the other
  products' shares silently dropped. The refusal is visible in three places
  with one wording: a warning when the database loads, the new `unallocated`
  check of the quality report, and the answer to any request that would score
  it (HTTP 422; the tools return the same text). The activity itself still
  loads and reads as before. What triggers it: a product output the source
  declares no share for, which happens on an unlinked (multi-output)
  EcoSpold 2 dataset or a multi-output ILCD or Brightway process; and an
  activity without exactly one
  reference exchange, which the EcoSpold parsers used to drop at parse time.
  None of the databases shipped as examples trigger it. Nothing changes in
  any score: a SimaPro block's products are split into one process per
  product exactly as before, each product's declared share applied as
  written, whether or not the block's shares sum to 100.
- A SimaPro "Avoided products" row now carries its own role, `AvoidedProduct`,
  instead of being reported as a `Coproduct`. It is a substitution, a credit
  on the producer of the product it displaces, and its matrix entry is what
  it always was. The EcoSpold writers write it as the negative input ecoinvent
  itself uses for a substitution, the Brightway writer as a `substitution`
  row, and the Brightway reader now reads such a row instead of skipping it.
  A client that decodes the exchange role as an enumeration has to learn the
  new value; this is wire revision 14.
- A product row's declared share now travels on the exchange that carries the
  product (`share`, with the percentage and the raw formula), and so does the
  category the row was filed under (`classification`). A process split from
  a SimaPro block keeps the share of its own product row, which is what the
  `allocationPercent` and `allocationFormula` of an activity summary now
  read; they report the same numbers as before. A process whose product row
  had a category of its own keeps it, where it used to inherit the block's:
  measured on Agribalyse 3.2, 31 blocks file a coproduct under another
  category than their reference product, and the split processes now say so.
- A database whose regional factors all come from a database it depends on is
  now scored on the regionalized path, like the database it mirrors. Two
  consequences, both of them that path's existing behaviour reaching a new set
  of databases: `exclude_long_term` is ignored there, so a request that used it
  against such a database stops filtering; and the per-flow shares reported
  next to a score are still computed region-blind, so they no longer add up to
  a regionalized total. Both were already the case for a database carrying its
  own regional factors.
- A compartment a method row states is now a condition on the flow it
  characterizes, not a preference. When no flow of the row's name sits in the
  medium it names, the name matcher says so and the cascade tries the next one,
  where it used to answer with the first flow of that name whatever its medium
  and stop there. A subcompartment is met exactly rather than by containment,
  so a row written for "low. pop." is no longer the exact match of a flow at
  "low. pop., long-term"; such a flow still takes the row's medium-level value,
  as it does at scoring time. Measured on Agribalyse 3.2 against Environmental
  Footprint 3.1, over 137125 flow readings in 25 categories, four readings move
  and all four are gains: "Fluorochloridone" now picks up its factor through
  CAS, which the method spells "Flurochloridone", where the name matcher used
  to answer with a flow in the wrong compartment and leave it uncharacterized.
- A supply chain names the row that supplies an input, or names none. When
  several rows produce one product flow, the display path used to elect the one
  whose unit was dimensionally compatible with the input. That rule was written
  when the unit was part of a flow identifier and two spellings of one product
  were two flows; every row is now recorded in the reference unit of its
  dimension, so the rivals share a unit and the rule elects nobody anyway.
- What identifies a dataset read from a SimaPro CSV or a Brightway Excel
  workbook is now what the file publishes, not what the engine calls things.
  Two problems came from the old rule. The unit was part of a flow identifier,
  so renaming a reference unit in the engine's own table moved about twelve
  percent of Agribalyse process ids in release 0.10.0 without a single number
  changing. And an activity was named after its process name, so two exports of
  one Agribalyse that disagree on the case of a product name gave the same
  dataset two identifiers on two servers. From now on an activity is named by
  the "Process identifier" its block publishes, and a flow by its name folded in
  case and its compartment. The identifiers of these two formats therefore move
  once more. Converting a stored list means asking the engine for every activity
  it holds and recomputing the ids each one used to answer to: a uuid5 cannot be
  inverted, so that is the only direction that works.
  EcoSpold 1 and 2 and ILCD are untouched: they carry identifiers of their own.
- Every row of these two formats is recorded in the reference unit of its
  dimension, where only the reference product was before. An input written in
  grams is held in kilograms, one written in kWh in MJ, and displays and
  exports show it in that unit. No score moves: the matrix already converted
  what it summed. Two rows that would land on one flow in units no conversion
  relates, an energy against a mass, are now refused by name rather than one of
  them being dropped in silence.
- The SimaPro writer emits the "Process identifier" line it always read.
  Exporting a database and reading it back used to rename every process in it.
- An input is no longer answered with a supplier the file did not name. When a
  product name matched no reference product, the engine fell back to a prefix of
  that name: the text before the first `//`, ` {`, ` [` or ` |`. What follows
  those separators is the geography and the model variant, which is exactly what
  tells two producers apart, so `Urea {RoW}| urea production` was answered with
  whichever activity named `Urea …` the map happened to hold last. Measured on
  ten SimaPro exports the rule earns nothing: on seven of them every input
  already has an exact producer, and it fires on ten rows of one Agribalyse 4.0
  export and one row of Ginko, eight of those eleven choosing among several
  candidates. Between databases it is worse: of the 169 lines pastoeco resolves
  in Agribalyse 3.2, 148 match by name and 21 by prefix, 17 of them ambiguously,
  one choosing among 148 electricity markets. The rule is gone on both sides. An
  input nobody supplies stays unlinked, and the cross-database linker gets its
  turn on it.
- Scores of a SimaPro database fall where an invented supplier used to be
  counted. On the Agribalyse 4.0 export of 13 May 2026, ten inputs name
  ecoinvent unit processes the export does not carry: four French fertiliser
  mixes and one lorry. They were being answered with a market of another
  geography, so every product fertilised in France carried a burden its file
  never asked for. Over a sample of 250 activities half the category readings
  move, and every one of them moves down, by 0.5% at the median and by up to
  56%. The engine reports those ten as unlinked, where it used to report 169113
  of 169113 resolved.
- When several activities produce one product name, the file says which one to
  use: a retired block is filed under an obsolete category, and the block still
  in service supplies. On the Agribalyse 4.0 export of 13 May 2026 that settles
  all ten of its duplicated products, which are the ten coproducts of a pork
  slaughterhouse block shipped twice, once under `Autres\Obsolete`. Until now
  the retired copy won, because "whitout" sorts before "without". The category
  is a convention of the tool that writes these files, which shows retired
  processes under an `Obsolete` subcategory and warns whenever a calculation
  reaches one: 1036 of Agribalyse 4.0's 22822 product rows carry it, and 2551
  of ecoinvent 3.11's 28594. Two blocks the file gives no way to tell apart are
  still ordered by activity name then by location, never by identifier: a
  change in how identity is minted must not move a supply chain.
- Scores of a SimaPro database move where a retired block was supplying.
  Measured on Agribalyse 4.0 over 1062 activities, 771 of them in the pork
  chain and 291 drawn at random, 11453 of 26550 category readings move, on 800
  activities: 9739 rise and 1714 fall, by 1.1% at the median and 1.5% at the
  ninth decile. The largest is black pudding, at 2.8x on several categories: it
  is mostly pork blood, and blood is one of the ten coproducts the two blocks
  allocate differently.
- The quality report says when two activities declare one product, since only
  one of them can supply it. The duplicate-activities check cannot see these,
  because it groups on the activity name and the two spell theirs differently.
  On the Agribalyse 4.0 export of 13 May 2026 it finds twenty, and they are one
  case: the pork slaughterhouse block shipped twice, once current and once
  retired under `Autres\Obsolete`, their process names a typo apart ("whitout"
  against "without"), declaring the same ten products.
- The quality report says when an input's only producer is a dataset the source
  retired. Such a dataset still carries its exchanges and still computes, so
  the score is a number; it is the superseded number, and its author expects it
  to be replaced. This is the warning the tool that writes these files raises
  when a calculation reaches one. On the Agribalyse 4.0 export of 13 May 2026
  it names 874 inputs. A product a retired block and a live block both declare
  is not named: the live one supplies it.
- The quality report gains the other half of a pair it only had one side of: an
  input naming a product no reference product of this database supplies, beside
  the reference product nothing consumes. Expected of a foreground database,
  which draws its background from another; a hole in one meant to stand alone.
  On the Agribalyse 4.0 export of 13 May 2026 it names the nine that were being
  filled with a supplier nobody asked for.
- Every database cache is rebuilt on first load.
- Scoring a batch of activities is faster on a database whose inventories
  carry many flows no method characterizes: on Agribalyse 3.2 with EF 3.1 a
  batch of 500 activities takes about 25 seconds instead of about 70. Such a
  flow used to be looked up again by name, for every method and every
  activity, although the tables had already found it carries no factor; it
  is now skipped. No score changes.

### Removed
- The fuzzy match strategy, which nothing produced. No matcher ever returned
  it, so its counter was always zero and its label never appeared; what it did
  do was swallow an unknown strategy name, which read back as "fuzzy" instead
  of being refused. The `byFuzzy` line disappears from the CLI's method mapping
  report and its JSON. The HTTP API never carried it.

## [0.11.0] - 2026-08-28

### Fixed
- An exchange now names the coproduct it actually asks for. A dataset whose
  production is allocated is written as one activity per coproduct, all
  sharing one activity identifier, and the engine kept an index from that
  identifier to a single one of those activities. So an input for a cheese
  could come back naming the whey permeate produced alongside it, and the
  same swap reached the tree export, the list of activities that use a flow,
  and the matrix debug export. Scores were never affected: they are computed
  from the pair (activity, product), which is the resolution everything now
  uses. What was affected is everything a client does with the identifier it
  was given back, starting with asking for that activity, or substituting it.
  Three visible consequences: an activity written as several coproducts is
  now several nodes in a tree instead of one, an input whose declared supplier
  is in no loaded database is a node of its own type (`MissingNode`) rather
  than a branch that vanished, and a loop node's `loopTarget` is the process
  identifier the schema always said it was rather than a bare activity
  identifier. Every database cache is rebuilt on first load.
- The wire revision moves to 12. A tree export can now report a node type a
  client has not seen, for a link naming a row no loaded database holds, so a
  client has to be able to tell a new engine from an old one before asking.
  pyvolca knows revision 12 and no longer warns that the engine is newer than
  it is.
- Asking for an activity by its identifier alone no longer answers with one of
  its coproducts, and says so rather than reporting the activity as missing. The same index that misdirected exchanges also served the
  bare activity identifier the API, the CLI and the edit journal accept, and it
  held a single row per activity, so an allocated activity resolved to whichever
  coproduct was recorded last. Such an identifier is now refused rather than
  answered wrongly, which is what the code beside it already claimed to do: it
  names one row only when the activity was written as one. The refusal says the
  activity is there and asks for the product alongside it, with a 400 rather
  than the 404 that would send the caller looking for data the engine holds.
  Every database cache is rebuilt on first load.
- An input that names only the product it consumes no longer gets a supplier
  drawn at random. The same product is often made by several activities, one
  per geography, and the index from a product to its producer kept only one of
  them, so such an input was attached to whichever producer came last. It is
  now left unresolved when the product alone does not say which activity is
  meant, the same rule the name-and-unit fallback beside it already applied.
  Scores are unaffected: they never read that index.
- Loading an EcoSpold 1 database no longer attaches an input to a supplier in
  the wrong country. When an input names its product but no location, the
  loader looked the product name up in an index that kept one dataset per name.
  An EcoSpold 1 product name carries no location, so one name covers every
  geography the product is made in: in a 11 947 dataset database, 787 names
  cover 4 526 datasets, and the location always tells them apart. The index now
  keeps all of them and the loader leaves the input unresolved, and says so in
  the unlinked report, when the name alone does not say which dataset is meant.
- Exporting an allocated activity to EcoSpold 1 no longer moves the links that
  point at it. Each coproduct is written as its own dataset with its own
  number, and an input naming that supplier now carries the number of the
  coproduct it asks for. It carried whichever coproduct was written last, so
  reading the exported file back attached the input to the wrong product.
- Comparing two impacts adds up the flows it treats as one instead of keeping
  one of them. The comparison aligns the two databases on a flow's name,
  medium and subcompartment, because their identifiers are unrelated by
  construction. Two flows in one database can carry the same three, and only
  the last was kept, so its neighbour's contribution vanished from the
  comparison or was reported as present on one side only. Both sides are now
  read from the same summed totals, and the largest contributions are chosen
  on those totals too, so a flow split across two lines that together lead the
  comparison is no longer left out of it.
- Loading a Brightway workbook now says when a column heading appears twice.
  Only one such column is read on any given row, and the others were dropped
  without a word.
- A `GET /mcp` is now refused with 405 instead of answered with an empty
  stream. That stream is how a server speaks to a client unprompted; VoLCA
  never speaks first, so it was returned already closed, which a client reads
  as a dropped connection and reconnects at once. Server and client then loop
  for as long as both are up: one such pair sent 71 644 requests in 21 hours,
  and on an engine holding several gigabytes of loaded data each wake cost a
  full garbage collection, so the engine burned two and a half cores doing
  nothing. A 405 says there is no stream to open, and the client stops
  asking, and it is what the protocol asks of a server that offers no such
  stream. This reads as a fix and not a removal: no working client relied
  on the old answer, the one observed behaviour was the loop.

## [0.10.0] - 2026-08-22

### Added
- `GET /api/v1/version` now says which reference-data bundle the engine reads,
  as `dataVersion`, and pyvolca's `ServerVersion` carries it as
  `data_version`. Two engines of the same version that give two scores for the
  same calculation differ there, and that is where to look first. An engine
  configured with no bundle reports `null`. The number is `data/VERSION`, and a
  check now run on every pull request and before every release keeps it true:
  the last two releases both shipped `volca-data-2.tar.gz` with different
  contents, so two engines reading different data would have answered the
  same `dataVersion`. This is data version 3, and wire revision 11.
- Every waste line of an activity now says what it does, as `wasteRole`. A
  consumer had to work it out from the target being absent, and that reading
  runs two opposite statements together: a waste nothing treats, which is a
  complete description of an end-of-life flow, and a waste whose named
  treatment is in no loaded database, which is a gap in what was loaded. Both
  arrive with no target, and calling the second one final says the burden is
  accounted for when it is missing. The four values are `TreatsWaste` for a
  line the activity treats, `SentToTreatment` for one whose treatment was
  found, `FinalWasteFlow` for an output naming no treatment, and
  `TreatmentNotLoaded` for one naming a treatment that is nowhere to be found.
  Only the engine holds both facts, so the engine now states the role instead
  of leaving it to be guessed. This is wire revision 10.
- A flow search now says what kind of flow each result is, and can be asked for
  one kind alone. Three different things answer to a name: a technosphere flow,
  meaning a product one activity makes and another consumes; a biosphere flow,
  meaning a substance exchanged with nature; and a waste flow. Searching for
  "tap water" or "biowaste" returned all of them mixed together, with nothing
  saying which was which: the only hint was an empty category, which a
  technosphere flow and a waste flow both have. Every result now carries
  `kind`, and `kind=technosphere | biosphere | waste` keeps one of them:
  `search_flows(query="water", kind="biosphere")` for the substance, and
  nothing else. A value that is none of the three is refused rather than
  quietly ignored, which would have read as "no flow of that kind exists".
  The tool description said the search returned biosphere flows, which was
  never true and is now corrected. This is wire revision 9.
- An activity now carries the provenance its dataset states about itself, and
  `GET /api/v1/activity/{id}` reports it as `documentation`: the source it was
  published in, the technology and period it describes, how it was sampled, and
  the reviews it passed. EcoSpold files carry a whole dossier next to the
  general comment - an EcoSpold 1 dataset names its bibliography and the report
  it came from ("ecoinvent report No. 1"), an EcoSpold 2 dataset names its
  author, year and reviewers - and none of it was read, so an analyst asking
  where a number comes from had to open the source file. Each section keeps the
  name its format gives it, and a section the dataset left blank is not
  reported.
  Three things it does not do. The full title of an ecoinvent report lives in
  `MasterData/Sources.xml`, which is not read, so an EcoSpold 2 dataset reports
  its author, year and pages rather than the report's title. The report the
  ecoinvent build process files as a review under the name `[System]` is left
  out, because it is kilobytes of mass-balance warnings per dataset written for
  that process rather than for a reader; a review signed by a person is kept
  whether or not they wrote anything beyond their name and the date. And a
  field an exporter filled with its own placeholder for absence counts as
  blank: openLCA writes the literal `<null>`, and reporting that as what a
  dataset says about its geography would be worse than saying nothing. Only the
  placeholder alone is read that way, since "none" and "not known" are
  statements a person wrote.
  Exporting a database does not write these sections back, the same way it has
  never written anything the general comment does not hold.
- The two quality reports of a database can now be taken as a CSV file, from
  the command line or from a plain web address. Load a database with one
  command and take its report with the next:
  `volca --config volca.toml --db agribalyse --format csv quality-report >
  quality.csv`, or `curl -OJ .../api/v1/db/agribalyse/quality-report.csv`. One
  row per finding, one column per thing a finding says. `--limit` keeps the
  worst findings of each check; without it the file holds them all. This is
  wire revision 8.
- `volca dump-config-schema` prints the keys a configuration file may carry, by
  name, as JSON, the way `dump-mcp-tools` prints the assistant tools. Writing
  about this file has meant reading the decoders, so anything written about it
  drifts quietly; now there is a list to check a text against.
- A hosted server can refuse changes in its operator's own words:
  `read_only_message` under `[hosting]` replaces the default read-only
  sentence on every surface (the REST API, the MCP tools, and the shutdown
  endpoint). `GET /api/v1/hosting` reports the message alongside `read_only`,
  so a client can explain the situation before a change is even attempted.
  The default sentence now opens with "This engine is configured read-only"
  instead of "This instance is read-only", naming who to talk to about it.

### Changed

- A biosphere exchange that names its flow in words now reaches the flow the
  database already declares under that name and compartment, instead of always
  creating one. Writing an emission the way an inventory shows it, `Nitrogen,
  total` in water, used to mint a second flow of that name; no characterization
  method knows that new flow, so the emission scored as zero beside the curated
  one it was meant to be. Only a flow identifier reached the curated flow, and
  nothing in a written inventory shows identifiers. A name nothing answers to
  still brings a flow into the database, with the warning it always carried,
  and a name two flows answer to is refused with both identifiers so the
  exchange can name the one it means. Two flows of one name recorded in
  different units, an energy carrier in kg and in MJ, are told apart by the
  unit the exchange states. The refusal for a name written into the identifier
  field now says where identifiers come from and how a name is written instead.
  A biosphere line reads back with its flow's identifier as well, so a line
  whose words cannot address one flow can still be restated from what the read
  hands back.
  This changes what a database written before now means. Its journal records
  what the author wrote, not what it resolved to, so a line that minted a twin
  of a curated flow last month now reaches the curated flow on the next load,
  and the database's scores move accordingly. A line naming several flows in a
  unit none of them uses, which used to mint a twin in silence, now refuses,
  and refuses the load with it.
- Energy is now measured in megajoules rather than joules. A database imported
  from SimaPro or Brightway Excel records its reference product in the
  canonical unit of that product's dimension, and for energy that unit was the
  joule, so a market for low-voltage electricity delivering one kilowatt hour
  was written down as `3600000 j`, and every amount drawn from it read the same
  way: the electricity a pig farm consumes was `5.07e10 j` where it now reads
  `50652 mj`. Megajoules are the unit those numbers are compared in and
  reasoned about everywhere else in the field.
  Nothing about the inventory moves. A unit conversion has always been a ratio
  between two entries of the unit table, so rescaling the whole energy column
  leaves every ratio where it was: scoring a kilogram of wheat or of cheese
  against EF 3.1 gives the same answer to the last digit a double holds, which
  was measured on two engines built from the same source and differing only in
  that table. Two narrow exceptions are worth knowing. A characterization
  factor whose own unit the table does not know, one written "MJ-Eq" or "MJ
  deprived" rather than plain "MJ", is read against the reference unit of the
  flow it applies to, so on an energy flow it is now read per megajoule rather
  than per joule and its score changes by a factor of a million. That is the
  same correction radioactivity received when kBq became the reference unit,
  and the new number is the right one; no shipped method writes a unit that
  way, so it is a method of your own that would move. And a ratio that used to
  land on a round double may now land one bit away from it, a kilowatt hour
  reading 3600000.0000000005 joules rather than 3600000, since the two factors
  it divides are no longer whole numbers.
  What does move is the functional unit itself, which is the point rather than
  a side effect: an activity that *sells* energy is now scored per megajoule
  instead of per joule, so its score is a million times larger and says the
  same thing. French low-voltage electricity reads 0.021 kg of fossil CO2 per
  MJ where it used to read 2.1e-8 per J.
  Which unit of a dimension is the reference one is now stated rather than
  inferred. A unit spelled two ways carries the same factor, so several names
  compete for the role ("mj" and "megajoule"), and the winner used to be
  whichever sorted first alphabetically. It is now the shortest spelling, which
  is the symbol and not the word. Energy was not the only dimension the
  alphabet was deciding badly. Five others get a new reference name: a volume
  is recorded as `m3` rather than `cubic meter`, a count of things as `p`
  rather than `dimensionless`, a land occupation as `m2a` rather than `m2*a`,
  a freight transport as `kgm` rather than `kg*m`, and a price as `eur` rather
  than `dollar`. All five carry the same factor as the spelling they replace,
  so no amount changes with them. The price one only renames an older defect
  rather than fixing it: seven currencies sit in the table at a factor of 1.0,
  so any of them converts to any other at par, and the reference name says
  which label the amount is written under, not which currency it is. A test now
  pins the reference unit of every dimension, so moving one is a decision and
  not an accident.
  One consequence to know about: the identifier of a product flow is derived
  from the unit that product is measured in, so an activity whose reference
  product is energy, a volume, a count, a land occupation, a freight transport
  or a price now answers to a new `process_id`. A process id of such an
  activity written down before this release no longer resolves; search for the
  activity again to get the current one. Databases already on disk rebuild
  their cache on the next load.
  The same is true of a process id recorded inside a database: an uploaded
  database whose edits are kept in a journal replays them by process id, so an
  edit recorded against one of those activities no longer finds its target and
  the database refuses to load, naming the id it could not resolve. Those edits
  have to be made again against the current ids.
- The shipped `volca.toml` names the settings it never mentioned: `[hosting]`
  and its nine limits, `chem-synonyms`, `substance-edges`, `[server] name`,
  and the three method fields that carry a single score (`scoring`, `patches`,
  `global-methods`). It also stops promising more than the engine does:
  `VOLCA_PASSWORD` is read only when neither `--password` nor the file sets
  one, so it cannot rotate a password written there, and `api_access` is
  reported for whatever fronts the server rather than enforced by it.
- In pyvolca, a method is asked for by name as readily as by UUID, and the
  collection carrying it no longer has to be named: `get_impacts(pid, "Water
  use")` now scores. `collection` lost its old default of `"methods"`, a name
  nothing ever loads, which is why that call used to fail whatever you passed;
  a whole-collection call (`get_impacts_batch`, `score_activities`) runs
  against the only loaded collection. Nothing is guessed: an unknown method, a
  name two collections carry, or several collections loaded with none named
  raises before the request leaves, naming the candidates. Such a refusal
  carries no HTTP status, so read the exception itself rather than
  `status_code`.
- Asking for a method collection that is not loaded now tells you which ones
  are. The refusal used to read "Collection not loaded: methods" and nothing
  more, which looks like a broken engine when it is really the wrong name: a
  collection is named in the configuration file (`[[methods]] name`), and the
  caller had no way to learn those names from the refusal itself. They now
  follow on a second line, as the assistant tools already said them.
- Startup now says which keys of the configuration file nothing reads. A
  configuration is a list of things the engine looks for, so anything it does
  not look for was dropped without a word, however it got that way: written
  under the wrong section header (`geographies` below `[server]` becomes
  `server.geographies`), spelled with the wrong separator, or named the way an
  older release named it. Each unread key gets a line naming its path, and
  `[[databases]] active` is named as having become `load`, which is why a
  configuration written before that rename started up loading nothing at all.
  A warning rather than a refusal: a key from a later release should not stop
  an older engine.
- **The server now listens on the address `[server] host` names, and stops
  answering the network unless it is told to.** That setting had never reached
  the socket: whatever it said, the server accepted on every IPv4 interface, so
  a configuration written to keep an engine on its own machine did not, while
  the password is off by default. The documented default, `127.0.0.1`, is now
  real. A deployment that relied on the old reach without asking for it has to
  ask: `host = "0.0.0.0"` answers the network over IPv4, `"::"` over IPv6. The
  startup banner names the address it bound, so the one case that cannot be
  honoured - `--port 0` takes a free port on loopback - is visible rather than
  silent. The command line reaches such a server at this machine rather than at
  the wildcard, which is not an address anything can connect to.
- The location hierarchy the regionalized scoring path uses is built once at
  startup instead of being rebuilt from the geography table on every call.
  It was two full passes over that table for each scoring request, and each
  method of a panel paid it again, even when the characterization tables
  themselves came back from their cache. The geographies file itself was read
  once at startup before this change as well.
- Startup now refuses a configuration in which two classification presets or
  two method collections share a name, the same way it already refused two
  databases sharing one. Both are looked up by name, so the duplicate would
  have silently shadowed one of its bearers; the error names the offenders
  instead.
- Every download now includes `THIRD-PARTY-LICENSES.md`, naming the numerical
  libraries built into the program and their terms. The Windows zip also
  carries the full licence texts of the runtime libraries beside it.
- Assistant answers now carry their `web_url` deep links when the engine runs
  behind a reverse proxy that serves the web interface upstream. The proxy
  declares itself with the standard `X-Forwarded-Prefix` header; the links then
  carry that prefix and the forwarded protocol. Before, an engine running
  without a bundled frontend emitted no links at all, even when a proxy in
  front of it served those very pages.

### Fixed

- `--jsonpath` now selects what `--format csv` flattens, instead of being
  required and then ignored. The flag was mandatory for CSV and had no effect
  on it: whatever you named, the command guessed the array to flatten by
  looking for the one array field of the response, and a search response holds
  three, so the guess failed and the whole JSON document was printed with exit
  code 0 where a table had been asked for. The path the help text already
  documented is now the one that is read (`srResults`,
  `piActivity.pfaExchanges`), and a path naming nothing, or naming something
  that is not an array, is refused with what was found and which fields exist
  rather than silently falling back.

- A waste output that names its treatment now reaches that treatment when it
  lives in another loaded database. The step that links a database to the ones
  it depends on looked at waste outputs naming no treatment at all, and skipped
  every output that named one, on the assumption that a named treatment is
  always in the same file. That holds for an imported file and not for a
  database written by hand, where naming the treatment is exactly how a waste
  output is written: the waste was silently cut off and its burden counted as
  zero. The link now decides which match applies, not whether the search
  happens: an output naming a treatment is matched on that activity's identity,
  one naming none on the waste flow itself, and neither falls back on the other,
  since substituting a treatment found by name for the one the author named
  would charge an activity nobody asked for. An output the database resolves in
  place is still left to the matrix, so nothing is charged twice.
- A waste output now names the activity that treats it. An exchange records the
  link to its treatment exactly as an input records the link to its supplier,
  but the activity view read that link on the input side only, so a waste output
  answered with no target whatever it was linked to. Anything reading a waste
  output with no target as a final waste flow, which is what a waste nothing
  treats is, then described every linked one as final. The treatment is read off
  the pair the matrix routes the waste on, activity and flow together, so the
  row named is the one the score charged and never another product of the same
  treatment. Databases written by hand are where this shows most, since
  authoring always records the treatment a waste output goes to; an imported
  file that states a link on a waste output is read the same way, which the
  EcoSpold 2 reader carries through as written.
- A Brightway Excel workbook loads from a directory, not only when its own
  path is named. The engine reads five database formats but the step that
  decides what a source directory holds knew four, so an uploaded `.xlsx`,
  which arrives extracted into a directory, was refused with "No supported
  database files found", listing the four formats it did know. The list in
  that sentence is now read off the same place the detection is, so a format
  the engine reads cannot go missing from what it says it reads.
- A substance is now recognised by its CAS number however the source spelled
  it. A CAS reads `registry-group-check`, and only the registry number is ever
  zero-padded: the group is two digits and the check digit one. One of the two
  readers stripped zeros from every segment, turning formaldehyde's `50-00-0`
  into `50-0-0`, so which spelling a value carried depended on which parser had
  read it, and the two never met. Every substance whose group segment begins
  with a zero was affected. Both sides of the bridge now canonicalize, so a
  padded and an unpadded spelling of one substance meet whichever parser
  produced them. **Scores change** where a factor was previously missed: an
  impact that silently counted nothing for such a substance now counts it. A
  `cas` selector in a method patch written as `50-0-0` no longer matches
  anything and should be rewritten as `50-00-0`. A CAS made only of zeros and
  dashes is read as "no CAS stated" rather than as a substance every CAS-less
  flow shares.
- A flow name that finds something in a search now finds the same thing in a
  filter. `get_inventory` and `get_activity` matched the whole query as one
  piece of text, so the name read off a search came back empty as soon as the
  words were retyped without the punctuation: `carbon dioxide fossil` found
  `Carbon dioxide, fossil` in the search and nothing in the inventory of the
  activity emitting it, which reads like an activity that emits no CO2. Both
  filters, and the `--flow-filter` of the debug matrix export, now read a query
  the way the search does: every word, in any order, punctuation optional, and
  synonyms count wherever the flow behind the line is known.
  A filter keeps only the closest match, which a search does not have to do: it
  ranks a lookalike onto a later page, while a filter has no later page. So
  asking for `Carbon dioxide, fossil` as it is written returns exactly it, not
  it plus `Carbon dioxide, non-fossil`; dropping the punctuation returns
  everything the words reach, both of them, each under its own name. And a
  filter naming no word at all (blank, or punctuation only) now filters
  nothing, where it used to answer with an empty inventory.
  `get_inventory` also reports `matched_flows` next to `total_flows` and
  `shown_flows`, so a filter matching three hundred rows and showing fifty says
  so instead of looking like fifty matches.
  Two filters still match the whole query as one string. The characterization
  factor list shows the twenty largest factors of a method out of several
  thousand, so widening what matches without ranking it would push the flow
  asked about off the page. `aggregate`'s `filter_name` also feeds an exclusion
  list and activity names, which need their own answer.
- A password now guards the assistant protocol as well as the REST API.
  `authMiddleware` only ever protected `/api/`, so a server started with
  `password` set answered `/mcp` to anyone who asked - and `/mcp` reaches the
  same operations, including loading, uploading and deleting on a writable
  server. A password that closes one and leaves the other open reads as
  protection and is none. Static files and the login page stay public, so a
  browser can still reach the login screen.
- The Docker image's default configuration declares the chemical synonyms it
  ships. The file was copied into the image and named by nothing, so the
  flow-mapping suggester ran on an empty table, the same way `geographies` did
  before 0.9.6.
- The command line can again find the loaded database on its own, so `--db`
  is only needed when several are loaded. It was reading the database list
  under names the engine stopped using, and reported "No databases loaded on
  the server" for a server that had one. A database whose cross-database links
  did not all resolve now counts as well: it is in memory and answers queries.
  A list the command line cannot read now says so instead of reporting an
  empty one.
- `volca impacts UUID --method M` works again. It read the method list under
  the same stale names, so it answered "Method not found in loaded
  collections" for every method the server was serving.
- Searching flows now finds a flow whose name you didn't punctuate exactly.
  `water fossil` returned nothing at all while `water, fossil` returned eight
  results, because the whole query was looked up as one piece of text and the
  comma is part of the name. Every word of the query is now looked for on its
  own, in any order, so `fossil water` finds it too, and a word is still looked
  for inside longer words: `chlor` reaches `Trichloroethane`.
  Two consequences worth knowing. Results are no longer purely alphabetical:
  with no sort column asked for, the flows whose name carries the query as you
  typed it come first, then those carrying all its words, then the rest. This
  is what keeps the exact flow on the first page, since looking for words
  separately returns a good deal more than before. And a punctuated query is
  now a set of words rather than one string, so `2,4-D` also returns names
  holding a 2, a 4 and a d elsewhere. They sort below the flow actually named
  `2,4-D`.
  A search with an empty query returns nothing instead of the whole catalogue,
  which is what asking for no flow in particular already did.
- `--help` now describes every subcommand, including inside the REPL. Nine
  answered something else. `database upload`, `database delete`,
  `method upload` and `method delete` printed ``Invalid option `--help'`` and
  exited with an error, putting their usage on the error stream where anything
  capturing it got nothing: that is why four of the command pages on the
  website have been empty. `database list`, `method list` and
  `flow ... activities` quietly answered with their parent command's help page
  instead of their own. `dump-openapi` and `dump-mcp-tools` answered with the
  top-level one. In the REPL, every `--help` read as an unknown command,
  because a parser answers the flag by failing with the help text and the REPL
  kept only successes.
- The Docker image's default configuration now loads everything it ships.
  `geographies` was declared below `[server]`, where it parsed as
  `server.geographies` and was silently dropped, so a standalone container
  scored regionalized characterization factors with no geography hierarchy at
  all while its own CSV sat unused. Energy densities were not declared, so
  factors counted per MJ or per m3 could never meet a flow measured by mass
  or volume and their categories silently scored zero. And the built-in
  method's path resolved under `/app` instead of `/data`, so the image
  started with no method at all. One caveat for named volumes created by an
  older image: Docker never repopulates a non-empty volume, so copy
  `energy_density.csv` into it or recreate it.
- A database read from an EcoSpold 1 export now has one flow per substance.
  Each flow used to carry the dataset it happened to be read from, so the same
  substance became a separate flow in every dataset that mentions it: searching
  a 12 000 dataset export for `Water, fossil` returned eight identical lines,
  and one inventory of that export listed `Lead` 150 times. It held 27 935
  biosphere flows for 2 515 real substances. Totals stayed right, but nothing
  that groups by flow was readable. Two things follow on upgrade. The process
  id of an EcoSpold 1 activity changes, because its second half is the identity
  of its reference product, so anything holding one has to look it up again.
  And the cache of every database, whatever its format, is rebuilt from its
  source once, because one version number covers them all.
- A substance an EcoSpold 1 export records in two units stays two flows. One
  export writes waste heat in MJ in some datasets and in kWh in others, and
  natural gas in m3 and Nm3, 193 flows in all; an inventory row is summed
  without conversion, so merging those would have added MJ to kWh and reported
  the total under one of the two units.
- A cache the engine can no longer read is left on disk instead of deleted.
  A cache written by an older version reads the same way a corrupted one does,
  and both were deleted before rebuilding from source. A host that ships only
  the cache, with no source archive beside it, lost the database outright and
  failed to start from then on.
- A CAS number an EcoSpold 1 export declares in one dataset and omits in
  another is kept. It decides whether a flow can be matched to a
  characterization factor by CAS when its name does not match, and which of the
  two datasets was read first was an accident of file distribution.
- One malformed number in an EcoSpold 1 export no longer stops the whole load.
  A dataset or an exchange numbered with something that is not a number ended
  the read there; it is now treated as carrying no number, which the parser
  already handles.
- An EcoSpold 1 export the engine writes reads back the way it was written.
  Exchanges were numbered by their position in their dataset, and a number is
  what names a flow, so re-importing split one substance into one flow per
  position and merged two products that differed only by geography.
- An input of an EcoSpold 1 dataset is now resolved to its supplier through the
  dataset number the export itself points at. That number was read off the last
  numbered element in the dataset's metadata, which is the person who wrote it,
  not off the dataset, so the lookup found nothing and every input was matched
  on name and geography instead. A 12 000 dataset export offered 67 suppliers
  to that lookup where it should have offered 11 947. Inputs still all resolve,
  and a few now resolve to the dataset the export names rather than to another
  one carrying the same name.
- The macOS download for Apple Silicon now runs on a Mac that has no developer
  tools. It was built against the build machine's Homebrew copies of OpenBLAS
  and the Fortran runtime and looked for them at the same paths on yours, so it
  stopped at startup with `Library not loaded: .../libopenblas.0.dylib` unless
  you happened to have Homebrew and those exact formulas. Both are now built
  into the binary, as they already were on Linux. The Intel download still
  needs Homebrew: no way of building OpenBLAS into it produces a library that
  computes correctly there, and the cause sits upstream.
- The Windows download now carries the runtime libraries the program loads.
  Only `volca.exe` was in the zip, so it stopped with `libgfortran-5.dll
  introuvable` on a machine without MSYS2.
- A database read from a SimaPro export can be written back out as one. Long
  descriptions run to several paragraphs, and the export held them on a single
  line with SimaPro's own in-cell line break. Reading one turned that break
  back into a real newline, and the writer then refused the whole database
  because of it, so no such database could be exported at all. Descriptions are
  now written the way SimaPro writes them, and the paragraph breaks survive the
  round trip instead of being flattened to spaces. A line break in a name or a
  geography is still refused: there it means nothing and would tear the row
  apart on re-import.

## [0.9.5] - 2026-08-04

### Added
- You can adjust what an activity consumes and emits without re-describing it.
  `POST /api/v1/db/{db}/activity/{process-id}/exchanges`, `volca database
  edit-exchanges`, `edit_exchanges` in pyvolca and the same tool for an
  assistant all take the same short list: lines to remove, lines to restate,
  lines to add. This reaches activities that writing could not. An activity a
  database file brought in has an identity its parser minted, so no description
  addresses it — and a description could not carry back its classification,
  synonyms, parameters, pedigree or coproducts anyway. Naming only what changes
  leaves all of that exactly as it was. A line is named the way you already
  read it: an input by its provider, a waste output by its treatment, an
  emission by its flow. The reference product and any coproduct are out of
  reach, because changing those changes what the activity is. If a selector
  matches nothing, the edit is refused rather than reported as done; if it
  matches several lines, all of them change and the answer says how many. As
  with writing, a database the engine reads from its configuration is refused:
  copy it first. This is wire revision 7.

- You can ask why a flow scores with the characterization factor it does.
  `GET /api/v1/db/{db}/method/{method}/explain-cf/{flow}` and the `explain_cf`
  MCP tool replay the factor lookup for one flow and answer in sentences the
  engine writes itself: which rung of the cascade found the factor, which line
  of the method it came from, whether a synonym or a CAS number tied the two
  together, and how the amount was carried onto the factor's basis. The reply
  also lists the rungs tried before that one, including any refused because the
  flow's subcompartment forbids them. A contributions table gains the short
  version of the same answer: each flow now carries how its factor was found,
  so the whole table is annotated without asking per row. This is wire revision
  6.
- A factor found but unusable is no longer indistinguishable from no factor at
  all. When a flow's unit cannot be converted to the basis its factor is
  written in, the flow scores nothing, which used to look exactly like an
  uncharacterized flow; the reason is now recorded per flow, reported in the
  explanation, and named in the warning the engine already emitted.

- Activities can be written into a database you uploaded or copied, instead of
  only deleted from it. `POST /api/v1/db/{db}/activities` adds them,
  `PUT /api/v1/db/{db}/activity/{process-id}` rewrites one, and
  `volca database create-activities` / `replace-activity` do the same from the
  command line reading the same JSON document. You never supply a process id:
  it is derived from the activity name, location, product name and product
  unit, so writing the same description twice corrects one activity rather than
  making two. Writing is strict where importing is tolerant - a supplier that
  does not resolve, an amount that is not a finite number, a unit that cannot
  reach the supplier's are all refused, and a batch comes back with every
  complaint at once rather than one per round trip. A database the engine reads
  from its configuration is refused outright: it is background data the whole
  installation shares. This is wire revision 5.
- A server can say what it is called. `name` in `[server]` is repeated in the
  MCP handshake, both in `serverInfo` and in the first line of the
  instructions an assistant reads. Someone connecting several VoLCA servers
  at once, one per set of loaded databases, can now tell which one answered
  instead of guessing from the data.

### Changed
- Editing a database you uploaded or copied now survives a restart, whatever
  format it is stored in. Previously the sources and the matrix cache kept the
  pre-edit set, so a restart quietly brought every removed activity back. The
  edits are now recorded in a journal beside the database's own files and
  replayed when it is loaded again; the files themselves are never rewritten.
  That is what lets an EcoSpold 1, SimaPro CSV, ILCD or Brightway Excel
  database be edited at all: rewriting them would have worked only for
  EcoSpold 2, because the others derive each process identity from names,
  categories and the position of a dataset in its file, so saving would have
  given every activity a new identity at the next read. A copy now gets a
  directory of its own the moment it is made, holding its identity and its
  edits but no data - it reads the files of the database it was copied from,
  which can no longer be deleted while a copy still needs them. A configured
  database the engine only reads is still edited in memory alone, and the
  delete response says so with two fields, `transient` and `warnings`.

### Fixed
- A copy made from an edited database now keeps the source's edits. What was
  copied is the source as it stands, but those edits live in the source's
  journal and the copy's home had none, so the next load read the untouched
  files and quietly brought back everything the source had removed before the
  copy was made. The copy's journal now starts as a snapshot of the source's;
  edits the source makes afterwards stay the source's alone - the copy forks
  at the current point, as a branch should.
- Every path in a configuration file now means the same thing. A relative
  `path` under `[[databases]]`, `[[flow-synonyms]]`, `[[compartment-mappings]]`,
  `[[units]]`, `[[energy-densities]]` or `geographies` was read next to the
  configuration file, while the same key under `[[methods]]`, `chem_synonyms`
  or `substance_edges` was read next to wherever the engine happened to be
  started from. Two neighbouring lines of one file therefore pointed at two
  different places, and the usual way out was to write absolute paths, which
  makes the file describe one machine. All of them now follow the file, so a
  configuration and the data beside it can be moved or shared as one directory.
  Absolute paths are untouched.
- A flow search now tells same-named flows apart. Agribalyse 3.2 carries seven
  `Deltamethrin` flows that differ only by compartment; a search returned seven
  rows with the same name, medium and unit, in an order that interleaved them.
  Each result now carries its sub-compartment in a `compartment` field, next to
  `category`, which has always held the medium alone, and every sort order
  continues through the remaining displayed columns, so flows that look alike
  arrive adjacent and ordered instead of scattered.
- An uploaded database whose `meta.toml` recorded a path containing a backslash
  or a quote came back with that character doubled. The file escapes them as
  its format requires and nothing undid it on the way back, which on Windows
  meant a nested data path that resolved to nothing.
- An amount written as a signed power of ten is read instead of refused.
  SimaPro writes a scale factor that way — `1*10^-3*50` — and the engine
  understood `10^3` but not `10^-6`: the exponent was read by the
  exponentiation rule itself, which knows numbers but not signs, so the minus
  failed the whole expression and the amount fell back to whatever the cell
  began with. The exponent now goes through the same rule as any other signed
  operand. Exponentiation stays right-associative and still binds tighter than
  multiplication, so `2^3^2` is 512 and `-2^2` is −4.
- A regionalized factor now comes from the method's most specific line, not from
  whichever line the file happened to list last. When a method writes both a
  medium-level factor and one naming the flow's own subcompartment, and both
  carry a location, the two used to collide and the later row won: reordering
  the same file changed the score. The line that names the subcompartment wins,
  and between two of the same kind the larger factor does, as everywhere else.
- An emission to the sea is now characterized by an impact category that writes
  no sea-water factor of its own. A method file spells out a subcompartment only
  when its factor differs from the medium-level ("unspecified") one, so a
  category with nothing different to say about the sea writes nothing at all —
  and the engine was reading that silence as a refusal. Marine eutrophication is
  that case in EF 3.1: it writes no subcompartment line anywhere, because the
  JRC original gives it one and the same factor for fresh water, unspecified
  water and sea water (`nitrogen, total`: 1.0 in all three). Withholding its
  medium-level factor scored a nitrogen discharge to the sea as nothing, and a
  farmed salmon, which discharges most of its nitrogen straight into the sea,
  was scoring a fifth of its marine eutrophication. Categories that do
  distinguish the sea are untouched: they write the line, and the line wins.
  Which side a method lands on is now reported when it loads, so a category
  whose sea factors were lost on import can be told from one that never had any.
- A SimaPro amount cell the engine cannot read is now zero and warned about,
  instead of the number the cell happens to begin with. The reader used to stop
  at the first character that is not part of a number and keep what it had, so
  `1,5 kg` became 1.5 and `124902,34825322*1/Qp` — an expression whose parameter
  went missing — became a hundred and twenty-five thousand. An amount that is
  wrong by orders of magnitude and looks ordinary is the hardest kind to find;
  the import now names the cell it could not read and the value it used.
- A SimaPro amount written as a sum is now added up. An exporter may state a
  quantity in place, `0,45+0,247+,067`, and drop the integer part of a term.
  The reader wanted a digit before every decimal point, so one such term made
  the whole expression unreadable and the amount silently became the number it
  began with: 0.45 where the file says 0.764. In Agribalyse 3.2 this fell on
  the pesticide emission mixes — the shares of a mix stopped adding up to the
  kilogram they divide, and the freshwater ecotoxicity of the cereal crops
  built on them came out about 10% low. And when an amount really cannot be
  read, the import now says so: a warning names the process, the text it could
  not read, and the value used in its place.
- The MCP handshake announced version `0.6.0` whatever the build was; it now
  reports the running version.
- The dependency chosen for an uploaded database is now written into its
  `meta.toml`. It used to live only in memory and in the binary matrix cache,
  so a restart between choosing the dependency and finalizing the database lost
  it silently, and the database came back linked to nothing.

## [0.9.4] - 2026-08-01

### Added
- A quality report says what is malformed in a database, a question no score
  answers: an entry with two reference products, or coproducts allocated to
  90%, still computes — it just computes something wrong in silence.
  `GET /api/v1/db/{db}/quality-report` and the `get_quality_report` MCP tool
  run five structural checks: exactly one reference exchange per entry,
  coproduct percentages summing to 100 (±0.5 for source rounding), duplicate
  activities, non-finite amounts and zero reference amounts, and absent
  description, classification, location or unit. Each finding carries its
  severity, the activity it sits on and a readable detail; `limit` caps the
  list while `offenderCount` still covers all of it. A check with nothing to
  judge reports `applicable: false` rather than a passing zero. The report is
  a structural scan needing no matrices, so it answers on a staged database
  as well as a loaded one — which is when it is most worth reading.
- An instance can declare itself read-only. `read_only = true` in `[hosting]`
  makes it answer every analysis request and refuse every state change:
  loading and unloading, uploads, deletes, copies, relinks, dependency edits,
  and `POST /api/v1/shutdown` and `/api/v1/idle-timeout/{n}`, which decide how
  long the process lives. Refusals are `403` on REST and tool errors on MCP;
  nothing is silently ignored. This is what makes one instance safe to put in
  front of many unrelated callers, none of whom should be able to change the
  working set or end the server for the others.
- The `[hosting]` quotas now bound what a caller may keep. `max_uploads` caps
  how many databases of their own a caller holds, and `max_loaded_uploads` how
  many of those sit in memory at once; a copy spends the same budget as an
  upload. Both count uploaded databases only — the databases the config
  declares are what an uploaded inventory links against, so counting those
  would forbid the very thing uploading is for. Negative means unlimited, and
  where there is no `[hosting]` section at all (local runs, the CLI, the
  desktop app) neither applies. A refusal names what clears the way, unloading
  or deleting one, rather than stopping at a number.
- A method can now write an exception to one of its own wildcard rules: a
  substance starting with `!` takes its flows back out of the patterns declared
  for the same impact category. Some open families hold members that do not
  belong to the quantity the category counts, and no set of prefixes separates
  them — `Occupation, industrial area, benthos` is the sea floor and shares its
  prefix with a real factory yard. Writing the family out as a list instead
  would be the stale, per-database list that patterns exist to avoid. An
  exception that matches nothing is announced at load time, like a pattern that
  matches nothing. Exporting such a method to SimaPro, openLCA or ILCD leaves
  the exception rows out — those formats have no way to say "except this", and a
  row written as a flow would characterize exactly what it removes; VoLCA's own
  CSV keeps them and reads them back.
- The quality report now says when a geography was never declared. Nearly every
  process of some SimaPro databases leaves the `Geography` field at
  `Unspecified` — 97% of Agribalyse 3.2 — and the only geography left is the
  code inside the dataset name (`… {FR}`, `…//[RER]`, `…/CN U`). VoLCA reads
  it, which is what makes those databases usable, but the result is a reading of
  a name and not a declaration, and downstream the two are the same text. An
  EcoSpold dataset with no geography at all is filled in with `GLO`, likewise
  silently. The new `undeclaredGeography` check counts both, one finding per
  entry, so a maintainer can see how much of a database's geography is source
  data before treating it as such.
- A warning now reports factor lines that match a flow but cannot be
  converted into its unit — a per-kilogram factor against a flow measured in
  cubic metres, for instance. Refusing such a factor is correct (the
  dimensions do not agree), but the refusal used to be invisible: the flow
  simply scored zero, indistinguishable from a flow the method does not
  cover. Each affected method now says at load time how many flows are
  affected and names samples — for its global factors and its regionalized
  ones alike — so a silent undercount of this kind can no longer hide.
- The database quality report now flags exchange amounts too small to have
  been measured. Below 1e-27 — whatever the unit — a value is smaller than
  anything an instrument can produce (a hydrogen atom weighs 1.7e-27 kg), so
  it is a residue of computation wearing the costume of data. An ordinary
  exchange this small is a warning; a reference exchange this small is a
  danger, because normalization divides every other amount in the process
  by it.

- A characterization-coverage report tells database maintainers which flow
  names a method scores only through a name bridge. When a database names a
  substance differently from the method that characterizes it — `Bromomethane`
  versus `Methane, bromo-, Halon 1001`, say — VoLCA still scores the flow by
  matching on a synonym or CAS number. A tool that matches factors by their
  exact name, as many downstream consumers do, has no such bridge and scores
  that flow as zero without warning. The report lists each bridged flow grouped
  under the name the method itself uses, so the fix is a rename. It is available
  as the `get_characterization_coverage` MCP tool and at
  `GET /api/v1/db/{db}/characterization-coverage`, with one entry per loaded
  method collection so two method versions can be compared side by side.
- Method collections can be exported as an ILCD LCIA-method package (`ilcd`):
  a zip of one method dataset per impact category plus the flow datasets they
  reference (`lciamethods/` + `flows/`), which loads straight back. It carries
  the most metadata of any of the method export formats — methodology,
  description, per-factor direction, location and CAS all round-trip natively,
  the way a real EF package does. What ILCD's method profile cannot hold — a
  per-factor flow unit (it stores one reference unit per method), damage
  categories, normalization/weighting sets and formula scoring sets — is
  reported in export warnings, never dropped silently. Available through
  `POST /api/v1/method-collections/{name}/export` with `{"format": "ilcd"}`,
  `volca method export NAME --format ilcd`, and pyvolca's
  `export_method_collection(name, fmt="ilcd")`.
- The quality report flags individual allocation percentages outside the
  0-100% range, alongside the existing check that a block's percentages sum to
  100. A single factor can be out of range — a negative share, or more than the
  whole — while the block total still lands on 100.
- The quality report validates flow CAS numbers by their check digit: a CAS
  registry number confirms itself, so a corrupt one — which silently breaks
  the name-to-CAS bridge that matches flows across databases — is flagged. The
  zero-padded and canonical spellings both pass.
- The quality report flags oxygen-demand and organic-carbon measures recorded
  in a physically impossible order: within one entry BOD5 must not exceed COD,
  nor dissolved organic carbon exceed total. A reversed pair is a measurement
  or transcription error. Checked only where both members of a pair are
  present.
- The quality report checks that land transformation balances within each
  activity: the areas transformed *to* a use must match the areas transformed
  *from* another, since a parcel changed into one state was changed out of
  another. A gap beyond one percent — a dropped or mistyped line — is flagged,
  compared per unit so only comparable areas are summed.
- A computed-checks report joins the structural quality report:
  `GET /db/{db}/computed-quality-report` and the `get_computed_quality_report`
  MCP tool score every entry of a loaded database against a method collection
  and report per-category score outliers (median/MAD on a log scale within
  (category, reference-unit) groups — a mg-read-as-kg slip lands three orders
  of magnitude out), entries whose every score is zero, and negative category
  scores (info: avoided-production credits and waste treatment produce them
  legitimately). Separate from the structural report on purpose — that one
  stays identical on staged and loaded databases; this one needs the matrices
  and a loaded method collection.
- The quality report flags distinct activity names that merge under
  SimaPro's 80-character name truncation — each colliding name gets its own
  finding, so an export bound for SimaPro can be repaired before the names
  collapse into one process there.
- The quality report counts the exchanges that carry no pedigree scores, per
  entry — only in databases that carry pedigree scores at all, so formats
  that cannot publish them are not drowned in noise.
- The quality report lists the entries whose reference product nothing in
  the database consumes. Informational by nature: expected for a final
  product, a dangling intermediate in a background database.
- Method collections can be exported as openLCA JSON-LD (`openlca`): a zip
  of one `ImpactCategory` document per impact category, in the olca-schema
  archive layout, that loads straight back. Flow UUIDs, per-factor
  directions (`INPUT`/`OUTPUT`) and location codes are native to this
  format, so regionalized collections round-trip without the name-suffix
  projection the CSV formats use. What it cannot carry (methodology
  labels, damage categories, normalization/weighting and scoring sets) is
  reported in export warnings. The openLCA reader now also picks up the
  document-level `category` field as the impact category's group label,
  and method files load in a deterministic order on every machine.
- Method collections can be exported as columnar CSV — one column per
  impact category, one row per substance: the file you open in a
  spreadsheet. `POST /api/v1/method-collections/{name}/export` with
  `{"format": "csv"}`, `volca method export NAME --format csv`, or
  pyvolca's `export_method_collection(name, fmt="csv")`. Anything the
  format cannot carry (flow directions the compartment does not imply,
  damage categories, normalization/weighting sets, formula scoring sets)
  is reported in export warnings, never dropped silently.
- The columnar CSV method format itself grew the columns real methods
  need, read back by the parser and emitted by the writer: optional `cas`
  and `unit` key columns (real methods mix kg, m3 and MJ flows inside one
  category), and a `top/sub/qualifier` compartment path so subcompartment
  distinctions survive — in EF 3.1, nine factors out of ten are
  subcompartment-specific. Legacy files parse exactly as before, and
  quoted fields now work in these files too.
- Method collections can now be exported as SimaPro method CSV, the inverse
  of the SimaPro method import: `POST /method-collections/{name}/export`,
  `volca method export NAME --format simapro --out FILE`, and
  `export_method_collection` in the Python client. The file carries the
  collection's impact categories, damage categories and
  normalization/weighting sets, so a method imported in one format (for
  example an ILCD Environmental Footprint package) can be handed to a
  SimaPro user. Regionalized factors are written as name-suffixed substances
  (`Water, FR`) and land occupation/transformation factors under the `Raw`
  compartment — the conventions SimaPro method files use themselves; whatever
  the format cannot carry — a factor without a compartment, a factor whose
  direction the compartment column cannot express, formula scoring sets — is
  listed in the export warnings instead of being dropped silently.
- A collection-coverage endpoint:
  `GET /db/{db}/method-collection/{collection}/coverage` reports how many of
  a database's emission and resource flows at least one method of a
  collection characterizes, as a distinct count. No sum over the per-method
  figures can recover it, because a collection's methods overlap on the same
  flows. Exposed in pyvolca as `Client.get_collection_coverage`.

### Changed
- A server started with `--idle-timeout` now follows real work, not traffic.
  A connected MCP assistant polls its server all day (`ping`, `tools/list`),
  and those calls used to hold the server open with nobody behind them; now
  only an actual tool call counts. A matrix solve also counts as use in its
  own right, so an analysis that outlasts the request that asked for it no
  longer has the server exit underneath it.
- An EcoSpold 1 dataset published as `process_<uuid>.xml` (or `<uuid>.xml`)
  now keeps that identifier as its activity UUID instead of getting one
  minted from its name and location. Two releases of such a database can be
  compared dataset by dataset: a renamed dataset no longer reads as one
  deletion plus one creation. The process ids of these databases change once
  when the new engine first loads them, and their caches rebuild
  automatically. Files named any other way, and files holding several
  datasets, keep the minted UUID.
- The engine now advertises wire revision 4 on `/api/v1/version`. The three
  quality-report routes added since 0.9.3 arrived without a revision bump, so
  a client had no way to know whether an engine offered them: an engine too
  old to have the route answers 404, and so does an engine asked about a
  database it has not loaded. Clients gate on revision 4 and can tell the two
  apart. `pyvolca` understands the new revision.
- Log lines now say which database they belong to. When several databases
  load at once their lines used to interleave in one anonymous stream, so a
  page following one load would show another's progress. Each line of
  `GET /api/v1/logs` and of the `/api/v1/logs/stream` SSE feed is now a
  `{db, text}` object — `db` names the database whose operation emitted the
  line, or is null for lines that belong to no particular one. The terminal
  output is unchanged.

### Fixed
- Parametric coal flows no longer score zero energy. A flow carrying its
  calorific value in its name — `Coal, 26.4 MJ per kg`, `Coal, brown, 8 MJ per
  kg` — recovers an energy factor through its family, but coal splits into
  hard and brown and the fallback rightly refuses to pick between the two, so
  every parametric coal variant contributed nothing to fossil resource use
  without saying so. Four registry rows now attach each variant to its own
  family. The conversion still uses the calorific value written in the name,
  so the rows change which factor is found, never the energy accounted for.
- A classification preset that does not resolve is now refused instead of
  quietly filtering nothing. Asking a server for its raw agricultural products
  by a preset name it does not carry — a typo, or a config that never declared
  it — used to answer with every activity in the database, which reads like a
  result. The refusal names the presets the instance does carry. The MCP
  `aggregate` and `get_supply_chain` tools had a second form of the same
  problem: both advertise a `preset` parameter and neither ever read it, so
  even a valid preset was dropped there.
- "Land occupied", in the shipped Plain indicators method, no longer counts the
  sea. Its rule takes every `Occupation, …` flow, and that family holds the open
  ocean and the sea floor alongside fields and roads. Anything fed from the sea
  was reported as standing on it: one aggregated fish-meal process declares 679
  m²·year of `Occupation, sea and ocean`, and a farmed trout came out at 388
  m²·year of land — it now reads 0.87. A land crop is barely touched, since all
  it loses is the water a cargo ship crossed on its behalf: across Agribalyse's
  455 farm-gate products the median change is −0.004%, an apricot orchard goes
  from 6.96202 to 6.96198, and nothing anywhere rises. Sea, seabed and benthic
  occupation are now excepted; inland water bodies stay counted, a reservoir
  being a real surface somebody flooded.
- A region-tagged flow now gets the density that goes with the factor it
  borrows. When a method has no line for `Water, SERC`, VoLCA lends it the line
  written for `Water` — but that line can be denominated per kilogram while the
  flow is measured in cubic metres, and the density that bridges the two was
  only ever looked up under the flow's full name, region tag included. The tag
  was stripped to find the factor and not to find the density, so the flow ended
  up holding a factor of a dimension it could not reach, and scored nothing.
  Both lookups now strip it the same way.
- A density is now read in both directions. It relates two dimensions — mass to
  energy for a calorific value, mass to volume for a density proper — and a flow
  can meet a factor from either side of it, but only one side was handled: a
  flow in kilograms against a per-cubic-metre factor converted, while the same
  substance in cubic metres against a per-kilogram factor scored zero. A
  non-positive density is now refused outright rather than divided by, and the
  refusal is reported like any other.
- A SimaPro activity is now placed by the location its producer wrote down,
  rather than by one guessed from a name. SimaPro cuts its "Process name"
  field at 80 characters, which on a long name takes the `{FR}` tag off the
  end and leaves only a slash the name has for its own reasons — so
  "Bresaola … Already packed - PP/PE | No preparation" was filed under Peru,
  PE being the plastic. Reading a slash off the end of a name is how the
  WFLDB convention states a location and still works, but it is a reading of
  the name and now loses to a tag, wherever the tag sits. On Agribalyse 3.2
  this moves 73 activities, mostly to France, and retires the four
  "locations" that named no place: `PE` where it meant polyethylene,
  `F-Organic`, `F-Org(Farrrowin` and `Mid-western`. Names stop being cut
  short too, which separates 39 pairs of activities that until now shared one
  identity. Databases whose names really do end in a location — WFLDB, which
  relies on it 1876 times — are unaffected.
- Locations the databases actually use now have a place in the geography
  hierarchy. `data/geographies.csv` listed 91 codes; the databases use several
  hundred, so a Kenyan, Ukrainian or Brazilian-state activity had no wider
  location at all and its characterisation factors fell straight through to the
  global average with nothing said. The table now covers ecoinvent's whole
  vocabulary — every country in its UN subregion and continent, every province
  and grid inside the country it belongs to, and the regional aggregates the
  databases name. Containment comes from Natural Earth's public-domain country
  polygons rather than from hand-written guesses; codes whose membership is a
  judgement call, such as the aluminium industry's IAI areas, carry their
  region and nothing finer. The global codes close every fallback list, so a
  nearer regional factor always wins over the global average.
- A location whose name contains a comma is no longer cut in half. The
  geographies file was split on commas without regard for quoting, so
  `Europe, Western` — the location of 863 Agribalyse activities — parsed as a
  code called `Europe` followed by a stray field, and matched nothing. The file
  now goes through a real CSV reader and is decoded as UTF-8 whatever the
  system locale; a file it cannot read — bad quoting, bad encoding, duplicated
  codes — is reported instead of being quietly replaced by the built-in
  fallback table.

- A method's own per-unit factor lines no longer cancel each other. SimaPro
  names can bake the unit into the flow name — "Gas, natural/m3" and
  "Gas, natural/kg" are the same substance declared in two units, with two
  densities — and name normalization collapses both onto one key, where a
  single winner was crowned. The flow declared in the losing unit then read a
  dimensionally incompatible factor whose unit conversion silently zeroed its
  score: on a real SimaPro database, natural gas contributed nothing to
  fossil resource use, undercounting every gas-heated product by a factor of
  four. Unit-suffixed flows now match the factor line written for their exact
  name first, so each variant scores in its own unit; a lone variant still
  borrows its base resource's factor as before.
- The CAS bridge no longer guesses when one CAS number covers factor lines
  with different values. Water is the canonical case: every water flow shares
  one CAS, but a water-use method values each region differently and
  deliberately leaves rain, ocean and turbined water out — bridging them all
  to one arbitrary line (the world-average factor) made water scores explode
  on databases whose flows carry CAS numbers. Such a CAS class is now left to
  name matching alone; a CAS that identifies a single factor value still
  bridges. The refusal covers the per-location CAS bridge too, so an excluded
  flow cannot pick up a regional factor instead, and a flow that names its
  own region keeps that region's value rather than the consuming activity's.
  Region-located factor lines and subcompartment variants keep working as
  before — their variance is dispatched by location or arbitrated to the
  medium-level default, not guessed.
- Flows from a SimaPro CSV database now carry their CAS numbers. The parser
  used to leave every flow's CAS empty, so a characterization factor that
  could only reach its flow by CAS never matched on a SimaPro-sourced
  database. The CAS now comes from the file's own substance registry — the
  trailing blocks that list every substance with its CAS — filling about 89%
  of biosphere flows on a real export, so methods can match these flows by
  CAS instead of relying on name and synonym matching alone. A database
  cached before this change rebuilds its cache once on the next load, so
  already-imported databases pick the CAS up too instead of serving the
  old CAS-less flows forever.
- openLCA JSON-LD method files now load with the right factor directions,
  compartments, and reference unit — including files openLCA itself
  exported. Such files carry no per-factor direction, so every factor used
  to default to output — a resource factor (water withdrawal, land
  occupation) then matched against the wrong synonym view and could
  silently miss its flow. The direction now comes from the flow's category
  path (`resource/…`, `Raw materials`, `Land use` → input; `Emission to …`
  → output), or from the impact category's own direction when the path
  says nothing either way; a factor that states its direction explicitly
  is untouched. The parser also reads the olca-schema spellings a genuine
  export uses — the category path as a plain string on the flow reference
  (its `Elementary flows` root is dropped) and `refUnit` — where it
  previously only understood its own exporter's shape and silently lost
  the compartment and reference unit.
- Numbers in columnar CSV method files and normalization/weighting CSV
  files now parse exactly. The previous number parser drifted in the last
  decimal (`1.2227e-3` came back as `1.2227000000000002e-3`) — every such
  characterization or normalization factor was off by one ulp. A malformed
  value (`1,23` once imported as `1.0`, truncated at the comma) or a
  non-finite one (`NaN`) is now rejected instead of imported as a wrong
  number.
- Characterization factors loaded from an ILCD method package now parse
  exactly. The XML reader used that same drifting number parser, so a factor
  written as `0.0000010897906999999999` loaded one ulp off the value the file
  states; it now reads the correctly-rounded number.
- SimaPro CSV rows with quoted fields now parse correctly in files with
  Windows (CRLF) line endings. The carriage return left at the end of each
  line made the CSV reader give up and fall back to a naive split, which tore
  a quoted field apart at the separator it contains — a substance or category
  name like `"Ecotoxicity; freshwater"` landed in the wrong columns.
- The free-text comment on a SimaPro Products row (Agribalyse uses it for
  modelling notes such as edible fraction and raw-to-cooked ratios) now
  reaches the reference product and coproduct exchanges, so it shows up in
  activity details and exports like input and emission comments already did.
  Its data-quality pedigree prefix is decoded the same way too, and the
  SimaPro writer emits both back on the Products row instead of leaving the
  comment column empty.
- Multi-line SimaPro comments now display as real lines. SimaPro exports a
  multi-line comment on one physical line with an invisible control character
  (DEL, `\x7f`) between the lines; the parser now decodes it as a line break
  on every exchange comment, and the SimaPro writer encodes line breaks back
  the same way on export.
- The per-method `uniqueDbFlowsMatched` figure on the mapping-status endpoint
  now counts every database flow the method actually characterizes — probed
  with the same lookup scoring uses — instead of only the flows a factor
  resolved to directly. The old count missed every flow reached through a
  fallback (a factor covering a substance across many compartments counted as
  one flow), under-reporting a method's real reach several-fold on typical
  databases.

## [0.9.3] - 2026-07-15

### Changed
- The wire-format revision advertised on `/api/v1/version` is now `3`. Nothing
  breaks: every wire change in this release is additive, and existing clients
  keep working (pyvolca 0.8.x prints at most an upgrade hint). The revision
  exists so a client can tell whether the engine understands the new delete
  `ids` selection — an older engine would silently ignore the unknown key and
  treat the request as an empty filter, i.e. "everything", which is exactly
  the kind of guess a destructive operation must never make.

### Fixed
- `exact=true` on the activity search now applies to the `product=` filter
  too: it becomes a case-insensitive equality check on the reference product
  name, as it already was for names and geographies. Previously the flag was
  silently ignored for products, so an exact search could return near-miss
  substring matches.
- The `name=` filter on the supply-chain and consumers endpoints (REST and
  MCP) now filters. A name matching nothing previously disabled the filter
  and returned every entry — with a matching `filteredActivities` count — so
  a caller could not tell "no match" from "no filter". It now returns an
  empty result.
- A scoring integrity error (a regionalized score whose tables are internally
  inconsistent — mismatched lengths, absent weights) now fails the request
  with a 500 instead of silently scoring the category 0. A consumer could not
  tell that 0 from a real score. Coverage gaps are unaffected: an unmapped
  flow still contributes nothing and is reported as before. In sensitivity
  responses the error lands on the affected perturbation entry, which already
  carries per-entry errors.

### Added
- A supplier-gap report: `GET /db/{db}/gap-report` and the `get_gap_report`
  MCP tool list everything a database still demands but nothing supplies,
  after internal resolution and cross-database linking. Each missing product
  (name, location, unit) carries the blocking reason, how many consumer edges
  demand it, the distinct consumers, the total demanded amount, and the top
  consuming processes. It is the natural read right after a relink: it answers
  "what is missing to switch this database's background dependency?" without
  rebuilding the list by hand from the linking statistics. An optional `limit`
  keeps only the biggest gaps; the header counts always cover the full report.
- Delete-activities accepts an `ids` list to delete exactly the named
  processes, on the API (`"ids": [...]`) and the CLI (repeatable `--id`).
  Previously the only selection mode was a filter, so deleting a known list
  of processes required a deliberately unsatisfiable filter plus the `extra`
  list. `ids` cannot be combined with filter fields — an ambiguous request
  is refused, not guessed at.
- EcoSpold 2 `mathematicalRelation` formulas are now read. Dataset
  `<parameter>` variables are kept on the activity (value and raw formula),
  and each exchange formula is checked against the dataset's parameters and
  exchange variables as a consistency control. The amount stored in the file
  always stays authoritative: a formula that evaluates to a different value
  is reported as a divergence warning at load time, and the formulas that
  cannot be evaluated (unsupported functions, cross-dataset references) are
  summarized in one warning per dataset instead of being silently ignored.
- `volca server` starts without `--config`: it runs on the built-in defaults
  with no databases, ready to receive uploads or API-driven loads. Launchers
  no longer have to write an empty TOML file just to satisfy the flag. An
  explicit `--config` path that does not exist still fails loudly.
- Each factor listed by `GET /method/{id}/factors` now carries its
  compartment, location, and unit. A method routinely holds several factors
  for one substance name — emitted to air vs. water, or one per region — and
  without these fields such rows looked like duplicates.
- The `aggregate` primitive gains a `consumption` scope that answers "how much
  of X is consumed across the whole upstream chain" — total electricity or
  heat feeding a product, grass eaten by cattle. Each row is one scaled
  consumer→supplier link, so filtering by the consuming activity
  (`filter_consumer`, `filter_consumer_not`) avoids the double counting that
  summing cumulative production would give (for example counting the same
  electricity once per voltage level). Grouping by `consumer_name` shows who
  consumes what. Available on the REST endpoint, the MCP tool, and pyvolca.
- The relink mapping CSV's `source_location` and `target_location` columns
  now steer the linking instead of being informational. A row with a source
  location applies only to demands at that exact location (an exact row wins
  over a name-only row for the same name). A row with a target location
  designates the supplier literally: the link goes to that name at that
  location, bypassing the geography policy — so "a French process consuming
  Swiss cement: replace it with the French cement?" is answered row by row in
  the mapping. When nothing supplies the designated target, the relink
  reports a new `alias_target_missing` blocker (visible in the linking stats
  and the gap report) rather than silently falling back.

### Changed
- A mapping row now preempts the direct name cascade instead of being a
  last-resort retry. A curated row is a stronger statement of intent than a
  coincidental direct name match; names the mapping does not mention resolve
  exactly as before.
- The matrix-cache format changed with the new blocker (manual schema bump
  8 → 9): the first start after upgrading rebuilds each database cache once.

## [0.9.2] - 2026-07-14

### Added
- `server --port 0 --desktop` now asks the OS to bind an available loopback
  port and prints the actual `VOLCA_PORT=N` only after the listening socket is
  reserved. Launchers can therefore avoid reserve-then-release port races.
  With port 0 the server is only reachable from the local machine.

## [0.9.1] - 2026-07-11

### Added
- A characterization method declared in the TOML configuration can carry
  `[[methods.patches]]` blocks: declarative adjustments that rescale or
  replace the matched characterization factors every time the collection
  loads. A patch selects factors by impact category, flow name or prefix,
  CAS number, or subcompartment, and is re-applied to the freshly parsed
  source file on each load — so reloading never compounds it. A patch that
  matches no factor is reported at load time instead of being silently
  ignored.
- Bulk impact scoring — the `POST /db/{db}/impacts/{collection}` endpoint and
  the `score_activities` MCP tool — can now exclude long-term emissions, as
  scoring a single activity already could.

### Changed
- The wire-format revision advertised on `/api/v1/version` is now `2`, because
  the export download changed shape (below). pyvolca ≥ 0.7.2 requires an engine
  ≥ v0.9.1 and refuses older ones with a clear message; an older pyvolca gets a
  warning telling it to upgrade.
- Database exports download as raw bytes instead of base64-encoded JSON,
  matching how uploads already work — a third less data on the wire and far
  less memory on both ends for big files. Any export approximation warnings
  now travel in the `X-Volca-Export-Warnings` response header.

### Fixed
- Emissions to immediate groundwater are characterized again in ecotoxicity and
  human toxicity, inheriting the method's unspecified-water factor exactly like
  releases to a river or a lake. They previously scored zero — on a witness
  concrete process a single iron-ion flow was worth about 10% of the freshwater
  ecotoxicity score. Long-term groundwater keeps its explicit zero.
- Two flow-name synonym bridges (`Flupyrsulfuron-methyl sodium ↔
  Flupyrsulfuron-methyl`, `Pyrethrins ↔ Pyrethrum`) so EF 3.1 (adapted 1.03)
  characterizes flows it only lists under a sibling name, instead of silently
  scoring them zero in freshwater ecotoxicity.
- A database holding activities with several products can now be exported to
  ILCD. Each product becomes its own ILCD process, instead of the whole export
  being refused. This unblocks exporting databases read from SimaPro CSV, where
  two unrelated processes can share a name and so look like one multi-product
  activity.
- Exporting a large database to a zipped format (ILCD, EcoSpold 2) no longer
  stalls. The time spent packing the archive grew with the square of the number
  of files, so a full Agribalyse ILCD export — some fifty thousand files —
  exhausted memory and never returned.

## [0.9.0] - 2026-07-06

A characterization-accuracy release. EF 3.1 scores on Agribalyse and ecoinvent
now track the published references far more closely, the plugin framework that
carried no external users is gone, and a database can be loaded or unloaded from
every surface rather than only at start-up.

### Added
- A database can be loaded and unloaded from every surface — the REST API, the
  MCP server, the CLI, and the web UI — not only at server start-up.
- Scoring-set breakdowns can show a human-readable name for computed indicators
  (for example "Ecotoxicity, freshwater" instead of the raw key `etf`), via an
  optional `[methods.scoring.labels]` table in the scoring configuration. A
  label naming an unknown scoring variable is rejected when the configuration
  loads instead of being silently ignored.
- Scoring can optionally exclude long-term emissions — those a method releases
  beyond its time horizon — so results line up with inventories that account for
  them separately.
- A characterization method can be loaded from a bare `.csv` file, not only a
  zip archive; an unsupported file type now fails with a clear message instead
  of being silently misread.

### Changed
- Much closer EF 3.1 agreement with the published Agribalyse 3.2 and ecoinvent
  references, the sum of many corrections to how inventory flows are matched to
  characterization factors: CAS-guided matching with ambiguous bridges dropped,
  a curated and linted registry of name bridges for refrigerants and pesticides,
  a region-fallback chain for water flows, sub-compartment gating, a generalized
  density bridge, an ore-grade resource fallback, and a preference for the
  verbatim flow name when unit-suffixed homonyms collide. Many products that
  previously scored short — pesticide-heavy processes especially — are now
  characterized.
- Auto-extracted flow synonyms are an opt-in candidate set rather than always
  applied, so the shipped mapping rests on the curated bridges.

### Fixed
- Long-term emissions are characterized with the method's long-term factor
  rather than its default, and ionising radiation against its kBq reference unit.
- Water no longer collapses across regions: SimaPro factors are treated as
  name-regionalized rather than keyed on the consumer location.
- An unspecified chromium emission is treated as trivalent, and elemental metal
  emissions bridge to their ionic toxicity factor — correcting large human
  toxicity over-counts.
- EcoSpold 1 flow identity now includes the sub-category, and a SimaPro
  name-less multi-product block stays a single activity instead of splitting.

### Removed
- The plugin framework — its eight-handle registry, the `/analyze` REST
  endpoint, and the `plugin list` CLI command — is gone. It carried a single
  built-in implementation and no external users; flow-to-factor mapping is now a
  plain internal cascade.

### Performance
- Method tables are built once, off the request path, with a parallelized
  cascade and synonym-group memoization — a large speedup on the first scoring
  after a database loads.

## [0.8.1] - 2026-06-24

### Fixed
- Aggregate single scores (e.g. a PEF score) now compute correctly on JRC ILCD
  method collections such as EF 3.1. They previously failed with `Unknown
  variable` whenever a collection held several methods sharing one coarse
  damage category — for EF 3.1 the four climate-change methods, the freshwater
  ecotoxicity methods, and the resource-depletion methods each collapsed
  together, so their per-method scoring variables could not resolve.
  SimaPro-adapted methods were never affected.

## [0.8.0] - 2026-06-24

### Added
- A loaded database can be exported to any of the five supported formats —
  SimaPro CSV, EcoSpold 1, EcoSpold 2, ILCD, and Brightway Excel — from both the
  API and the CLI, so a database can be moved between tools or re-saved after edits.
- A loaded database can be edited in place: copy it under a new name, delete a
  filtered selection of activities, or relink it against a dependency through a
  name-to-name alias map.
- Activity records now carry separate `activity_name` and `product_name` fields,
  instead of the old `name`/`product` that blurred an activity and its reference
  product.
- A partial EcoSpold2 import (a handful of `.spold` files cut from a full
  database) now becomes analyzable by loading its matching ecoinvent background
  as a dependency: each input is linked to the exact background activity it
  names, by `activityLinkId` identity. Previously only nil-link inputs were
  resolved, so partial imports stayed unresolved however the background was
  loaded.
- When the loaded background is a *different* release than the import was cut
  from, the exact identity won't be present, so linking falls back to the usual
  attribute matching (name, location, unit). Those approximate links are flagged
  on the database setup view (and in the load log) so you can verify the
  dependency is the release you intended rather than trust a cross-version match
  as exact.
- `/api/v1/version` now reports a `wireVersion` integer. Clients read it at
  connect time to confirm they speak this engine's JSON format, so a version
  mismatch fails with a clear message instead of a confusing decode error.

### Changed
- Far broader EF 3.1 impact coverage: flows are matched by CAS number across
  naming schemes, a substance registry bridges nomenclatures, sub-compartments
  fall back sensibly, energy-carrier flows are characterized from their energy
  content, land use is regionalized with per-country factors, and a large synonym
  set links ecoinvent and Agribalyse flows to the method. Many products that
  previously scored short are now characterized.
- Large database uploads stream as raw bytes instead of base64-encoded JSON,
  cutting memory use and time on big files.
- A supplier substitution applies across every consumer of the substituted
  product, not only the process you queried.

### Fixed
- ecoinvent waste-treatment activities import and score correctly: the reference
  flow stays in the technosphere (these activities are no longer dropped) and
  cross-database waste keeps the correct sign.
- EcoSpold packages whose datasets live in a sub-directory now load.
- Requesting a well-formed but non-existent process returns a clear "activity not
  found" instead of a confusing error.
- A missing reference CSV is reported as an error instead of crashing the load.

## [0.7.0] - 2026-05-29

A month of engine work: a third flow kind for waste, regionalized impact
assessment, more input formats, and a hardening pass that turns silent
miscounts into explicit errors.

### Added
- Brightway Excel (`.xlsx`) inventories can now be loaded directly.
- Regionalized LCIA scoring via openLCA JSON-LD `ImpactCategory`, including
  uploading openLCA JSON-LD methods through the method pipeline.
- `WasteFlow` / `WasteExchange` as a third top-level flow kind, with an
  exact-match cross-database waste linker and explicit reporting of orphan
  (unlinked) waste.
- Per-database `geography_policy` controlling how activities are matched across
  databases during cross-DB linking.
- Sensitivity analysis: a rank-1 perturbation primitive and a sweep endpoint.
- SimaPro pedigree (uncertainty) matrix parsed and exposed through the API.
- Configurable per-instance upload size limit (hosting policy), enforced both in
  the upload handler and at the HTTP layer.
- macOS Intel (x86_64) engine build target and published release assets.
- One-liner installers for Linux, macOS, and Windows.
- MCP: batched LCIA and scoring sets, columnar `score_activities`, and
  source-native `activity_type` surfaced through search/score/get_activity.
- `/api/v1/licenses` endpoint plus `NOTICE` / `THIRD_PARTY_LICENSES`.

### Changed
- The cross-database dependency pin is now authoritative and persisted to cache,
  and databases auto-relink on every load.
- `Flow` split into `TechnosphereFlow` and `BiosphereFlow` (with `WasteFlow` as
  the third kind) so flow handling is total over the type system.
- Service errors are reported as 4xx, not 5xx: `InvalidUUID` → 400,
  `FlowNotFound` → 404, and cross-DB invariant breakages surface as client
  errors instead of 500s.
- Large LCIA speedups: batched multi-method scoring (~22× on PEF), precomputed
  per-activity weights for regionalized methods, and coalesced matrix solves.
- Docker images standardized on musl with a fully-static build and ARM64
  support.
- pyvolca: typed returns, string enums, and lazily paginated search/consumer
  results.

### Fixed
- Characterization no longer silently returns zero on a compartment,
  subcompartment, or unit mismatch — the gap is surfaced instead of undercounted.
- Regionalized LCIA returns a partial score on tainted columns rather than
  failing the whole computation.
- SimaPro: sign preserved on substitution (Materials/fuels) exchanges, reference
  amounts normalized to the canonical base unit, and split-location products
  exposed to the cross-DB linker.

## [0.6.0] - 2026-05-01

Packaging and distribution milestone (not previously recorded here).

### Added
- GitHub Actions build matrix producing release assets for Linux, macOS, and
  Windows, driven by a tag-based release pipeline with a relocatable data bundle.
- `pyvolca` published to PyPI, with per-exchange comments surfaced through the
  API and Python bindings.

### Changed
- SimaPro location extraction and reference-amount normalization improvements.

## [0.5.0] - 2026-02-02

### Added
- Desktop application (Tauri) for Windows and Linux with branded installer
- Console output panel with live log streaming in the web UI
- Loading screen shown while the backend starts in the desktop app
- MUMPS direct solver support on all platforms for faster matrix solving

### Changed
- Database upload now uses pure Haskell zip-archive (no external tools needed)
- Unified cross-platform build system (single bash script for Linux, macOS, Windows)
- Build dependency versions centralized in versions.env

### Fixed
- Console output panel showing empty in desktop app (optimized binary mismatch)
- Upload cancel now navigates back to databases list
- CSS and fonts bundled locally for offline use in desktop app

## [0.4.0] - 2026-01-18

### Added
- Database upload: load and unload your own EcoSpold databases (BYOL)
- Location aliases in configuration for targeted location overrides
- Production amount displayed in search results and activity header
- Product column in activity search results
- Database format column on databases page

### Changed
- Inventory page split into separate Resources and Emissions tables
- Redesigned left menu with white Explore/Lab sections
- Unified column order and shared ActivityRow component across activity tables

### Fixed
- EcoSpold1 exchanges without location now resolved via name lookup
- Zero-amount missing supplier exchange warnings suppressed
- Dynamic CPU detection for parallel loading (no more hardcoded worker count)
- Frontend minified with SWC for smaller bundles

## [0.3] - 2025-12-24

### Added
- Multi-database support with `--config volca.toml` configuration file
- EcoSpold1 parser for older LCA databases (Ecoinvent 2.x, BAFU)
- SimaPro CSV parser for Agribalyse
- LCIA impact assessment with method loading, flow mapping, and score computation
- Activity aliases configuration for resolving EcoSpold1 supplier links
- HTTP Basic Auth for API and web interface (`--password` or `VOLCA_PASSWORD`)
- Database management API endpoints (`/databases`, `/databases/{name}/activate`)
- LCIA methods API endpoint (`/methods`)
- Databases page in web UI with table layout
- LCIA tab in activity details view

### Changed
- Cache system now uses automatic schema-based invalidation (no manual version bumping)
- Cache filename simplified to `volca.cache.{dbName}.bin.zst`
- Per-database PETSc solver cache for instant database switching
- Web UI redesign: split details tabs into individual pages, sticky headers, improved left menu
- Database name included in URLs for bookmarkable multi-database views

### Fixed
- Double-click navigation and search focus issues
- Navigation history properly returns to search results
- Search removed 2-character minimum requirement

## [0.2] - 2025-12-04

### Added
- Details view with tabs for upstream activities, emissions, natural resources, and products
- Graph view with force-directed layout
- Activity search with multi-word filtering and pagination
- Products tab showing all outputs from multi-product activities
- URL routing for bookmarkable views

### Changed
- Renamed project from acv-engine to volca

## [0.1] - 2025-11-09

### Added
- Core LCA engine with EcoSpold2 XML parsing
- Matrix computation with PETSc/SLEPc
- REST API, CLI, and web interface with Tree and Inventory views
- Database caching for fast startup
