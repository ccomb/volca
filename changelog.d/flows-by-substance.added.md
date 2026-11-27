- The contributing flows of an impact category can be gathered by substance:
  `by_substance=true` adds up the flows of one name and one medium, so fossil
  carbon dioxide to urban air, rural air and the stratosphere make one row,
  with the identifiers of its flows and their sub-compartments. The shares and
  `cover_percent` then read the rows gathered. Names are compared with their
  case folded, so two databases that spell a substance differently keep two
  rows (wire revision 126; `by_substance` in pyvolca's
  `get_contributing_flows`, the rows in `top_substances`).
