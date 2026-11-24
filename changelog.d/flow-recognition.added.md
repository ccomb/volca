- A flow search finds a substance however the database names it. Typed whole,
  a chemical formula finds the flows of that molecule (`N2O` finds a
  database's `Dinitrogen monoxide`), a name the synonym registry gives the
  same substance finds it too (`nitrous oxide`), and so does its CAS number,
  padded or not (`10024-97-2`, `010024-97-2`). A formula reaches every flow of
  the molecule: `CO2` finds fossil, biogenic and land-use carbon dioxide
  alike. The flows that are the substance come first, ahead of the names that
  only hold the text (`N2O` is also in the names of `LiMn2O4` battery
  products). Only the whole query is read these ways: `N2O emission` finds
  by its words alone, as before. The engine carries a short list of formulas,
  used when no `chem-synonyms` file is configured, or when the configured
  file is missing, which startup reports; the suggester of missing factors
  reads it too. Built-in data version 17.
- An assistant over MCP gets `get_flow_activities`: the processes on one side
  of a flow, counted, the first fifty listed. Wire revision 121.
