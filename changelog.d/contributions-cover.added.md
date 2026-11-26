- The contributing processes and flows of an impact category can stop at a
  share instead of a number of rows: `cover_percent=80` lists them, largest
  first in absolute value, until they cover more than 80 % of the sum of
  absolute contributions, which is how the PEF method picks the most relevant
  ones. Every row of a list of contributions also gives that share and the
  running total, beside the signed share it already had. The lines of a
  database whose licence keeps its detail count in that sum (wire revision
  125; `cover_percent` in pyvolca's `get_contributing_flows` and
  `get_contributing_activities`).
