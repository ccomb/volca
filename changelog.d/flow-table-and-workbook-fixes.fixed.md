- 1,1,2-trifluoroethane (HFC-143) is no longer read as 1,1,1-trifluoroethane
  (HFC-143a): they are two molecules, and the factors a method gives the second
  do not hold for the first. Built-in data version 18.
- A Brightway workbook writes an elementary flow of unspecified subcompartment
  under its compartment alone ("air"), the way Brightway holds it, so its
  importer links the flow instead of leaving it out.
