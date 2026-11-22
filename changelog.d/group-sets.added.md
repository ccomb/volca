- Groups come made ready for the classification systems a database carries:
  transport, electricity, heat and fuels, fertilisers, pesticides,
  agricultural machinery, infrastructure and waste by CPC code, and
  transport, energy and waste treatment by SimaPro category type. The
  `group-sets` route lists the sets a database's processes can answer, the
  first being the one to open with.
- From one process of a split, the engine proposes the groups it could start,
  narrowest first: its product, then each level of its classifications, such
  as `CPC 6511`, `CPC 651`, `CPC 65`, or `Transport\Road`, then `Transport`.
  Each comes with its part of the score once added at the end of the groups,
  how many processes it takes and the products of the heaviest; a group that
  takes nothing, or the same processes as a broader one, is left out. It is a
  `POST` to the `group-candidates` route, and an assistant over MCP gets both
  as `list_group_sets` and `get_group_candidates`. Wire revision 120.
