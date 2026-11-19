- Wire revision 114. The request that deletes activities by selection must now
  say `spare`, true or false, with no default: the two answers delete
  different things, and a client that left it out would otherwise get one of
  them without choosing. Each classification entry in it carries `mode`, one
  of `contains`, `exact`, `not-contains` and `not-exact`, in place of `exact`,
  and the request refuses a field it does not know. The response counts what
  was kept in `spared`. Search addresses, the supply chain, the consumers and
  `search_activities` read the same four mode words, and a word that is none
  of them is now refused with the four named, where it used to read as
  `contains`, so a typo can no longer turn an exclusion into its opposite. A
  blank exclusion, which would remove every activity, is dropped.
- `volca database delete-activities` needs exactly one of `--spare-suppliers`
  and `--cut-suppliers`, and `delete_activities` in pyvolca needs `spare=`,
  both refusing an engine older than wire revision 114 before sending
  anything, since an older engine would not read the choice and a delete meant
  to spare would cut. pyvolca takes a `mode` in each classification entry, and
  still reads the older `exact` flag when no mode is given.
