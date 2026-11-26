- A dependency removed on a database's setup page no longer comes back when
  the database is loaded from the list of databases. Loaded with inputs left
  unresolved, the database was read again from its cache, which still named
  the dependency, and that dependency was loaded again, at every load.
