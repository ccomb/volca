- A database's dependencies are now a choice the engine keeps. Added or
  removed on the setup page, or set by a mapping relink, they hold at every
  later load, after a restart too, whatever else is loaded. A database that
  names none links, as before, to every database loaded beside it, and the
  engine keeps the ones it linked to. They are kept in `dependencies.toml`,
  in the data directory.
- `depends` in the configuration now names the databases a database links to,
  and no other: a supplier found in another loaded database is no longer
  linked.
- A database's matrix cache is no longer thrown away because it has inputs
  unresolved while other databases are loaded: it is linked again within its
  dependencies, without reading its source files.
