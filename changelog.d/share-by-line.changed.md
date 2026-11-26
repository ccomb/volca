- An exchange's `share` now says `"byLine": true` for a product its source
  divides line by line, beside the `{"percent", "formula", "by"}` object of a
  declared share (wire revision 123). The process of such a product has a
  null `allocationPercent` and no `massAllocationPercent`, since it has no
  share to compare a mass with. pyvolca's `ActivityDetail.is_allocated` still
  says such a block is allocated.
