- A score can be split between groups of processes, such as transport,
  electricity or fertilisers, wherever they sit in the supply chain. Each
  group is a name and a filter on the process name, the product, the location
  or a classification; a process goes to the first group it answers, and the
  activity asked about goes to none. Two readings are offered: by default a
  group counts the whole chain below the first of its processes met, and
  `own` counts only its processes' own emissions. Every impact category comes
  at once, and the groups, the rest and the databases whose detail is withheld
  add up to the score. A second request lists the processes of one group, or
  of the rest, heaviest first. Both are `POST` routes, `groups` and
  `group-members`, and an assistant over MCP gets them as
  `get_group_contributions` and `get_group_members`.
- A classification filter can be written `starts-with`, or `not-starts-with`,
  so that a code and everything below it are found together: `starts-with 49`
  finds `4923:Freight transport by road` and not `1049: …`. Search,
  deletion by selection and classification presets read it too.
