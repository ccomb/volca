- A process written or edited in a unit the database has no row for, but that
  the unit table knows, is accepted: a product stated per tonne in a database
  that records only kilograms used to be refused with "this database records no
  unit "t" nor one equal to it". The database gains a row for that unit, the
  same one each time the history is replayed, and a later line in another
  spelling of the same unit ("tonne" after "t") takes that row rather than a
  second one. An amount to a supplier in another database is still stated in
  that supplier's unit.
