- A dependency removed on a database's setup page, then unloaded, no longer
  comes back when the database is loaded from the list of databases. The
  database, left with inputs unresolved, was read again from its cache, which
  still named the dependency, and that dependency was loaded again, at every
  load. While the dependency stays loaded, the removal holds for that load
  only: the next one links to every loaded database again.
