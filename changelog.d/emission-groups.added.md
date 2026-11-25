- A group can keep the emissions of a substance: "all the nitrous oxide",
  "the methane", or "the nitrous oxide of the fields" when a group names both
  processes and a substance. The substance is read as that substance (by the
  flow's name, a synonym, a chemical formula or a CAS: `methane` takes fossil
  and biogenic methane, never dichlorodifluoromethane), or, when asked, as any
  flow whose name contains the text; a compartment (air, water, soil,
  resource) narrows it or stands alone. Such groups split each process's own
  emissions, one emission going to the first group that takes it, so the
  groups and the rest still add up to the score. The whole-chain reading
  refuses them, and a request that names no reading reads them this way.
- The flows of one group, or of the rest, heaviest first, by a POST to the
  `group-flows` route and the `get_group_flows` tool.
- The groups a flow could start, by `candidate_flow` on the
  `group-candidates` route and the `get_group_candidates` tool: the
  substance in its compartment, then in any, then by its CAS. Wire revision
  122.
