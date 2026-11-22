# How a flow meets a factor's subcompartment

This is the rule the engine follows, kept next to the code that applies it. A
pull request that changes the matching cascade describes its step here. The
user guide on the site explains the same rule with examples:
<https://www.volca.run/docs/guides/flow-mapping/>.

A method writes a factor per substance, medium and subcompartment; a database
files each flow under its own. The compartment mapping (`data/compartments.csv`,
replaced with `[[compartment-mappings]]`) holds two kinds of row, named in its
`kind` column:

- `same` (the default): one place written another way. SimaPro's `Air / low.
  pop.` is EcoSpold 2's `air / non-urban air or from high stacks`. The row
  rewrites a flow and a factor line alike, and never joins two places one
  vocabulary keeps apart.
- `if_absent`: two different places. `soil,forestry,,soil,non-agricultural,,if_absent`
  says that a flow emitted to forest soil reads the factor written for
  non-agricultural soil, but only under a method whose collection never writes
  forestry. The EF 3.1 package has no forest soil, so the row holds there;
  a method with forestry factors of its own keeps them. A comparison of two
  method collections reads the row's two places as one only when each
  collection writes one of them and never the other.

A flow reads a factor at these places, in this order, and nowhere else:

1. its own subcompartment;
2. the line its substance writes for the whole medium, with no
   subcompartment. That is what `(unspecified)` means in the SimaPro CSV format,
   whose reader keys it there. In an ILCD package, a JSON-LD package or this
   engine's CSV format, `unspecified` names one subcompartment like any other;
3. the place an `if_absent` row sends it to;
4. for a natural resource only, the line its own name, or a synonym of it,
   has at `unspecified`.

The fourth place exists because a resource's subcompartment says where a flow
list files it, not a different damage. Ecological Scarcity 2021 as published
for openLCA writes `Water, unspecified natural origin` and
`Occupation, tropical rain forest` at `unspecified` only, and counts them for a
database that files them `in water` and on `land`. An emission's subcompartment
does change what it does, so an emission never reads it. Only a line that
names the flow itself, by its name or a synonym `data/flows.csv` writes,
reaches the fourth place. A line that lends it another name's factor (one
sharing its CAS number, a proxy edge, the base substance of a region-suffixed
name) reaches the first three, the places an author wrote: never a guessed
place on top of a borrowed name. The same rule decides which database flow a
method line is attached to.

A region-suffixed name the method's collection writes itself, in any category,
is never lent another name's factor. Ecological Scarcity 2021 writes
`Water, river, RER` in "Water resources, net balance" and only `Water, river`
in "Water resources, evaporated": the regional flow counts in the first and
not in the second, so neither the base substance's line nor a line sharing its
CAS number reaches it there. `explain-cf` marks the step `declined` and says
why. A name an exclusion line writes counts as written, and so does one a
synonym of the written name gives. A line located at a region rather than
named with it does not.

A flow that states no subcompartment is an `unspecified` emission. The engine
borrows no factor across subcompartments beyond these places: a method that
means a sea or long-term emission to count for nothing writes that line.
This is how a score reads factors; comparing two method collections pairs
their lines by a reading of its own.
The same places decide which database flow a method line is matched to: the
flow that reads it first, and none when no flow reads it. A line whose name
ends in `*` and that is written at `unspecified` selects the flows filed at
`unspecified`, not those of every subcompartment. It also selects only the
flows its prefix names: the other names the registry gives them are not
added, so `Energy, *` takes `Energy, from uranium` but not `Uranium`. A line
naming the flow outranks the wildcard line that takes it.
`explain-cf` names the step that answered (`exact_name`, `compartment_default`,
`if_absent`, `resource_unspecified`, …).

A method can write two values for one flow at one place. An ILCD package
tells two such flows apart by their identifiers; written out as SimaPro CSV,
they share a name and a compartment and nothing else. Scoring reads one value
per place: the line that matched the flow more closely, and between two that
matched alike, the larger value. Synonyms lead to the same choice when two
names of a method reach one database flow. The log names each such place,
with every value written there and the one read.
