- An empty database of one's own can be created, to write processes into when
  no database at hand takes writes: an assistant does it with the MCP tool
  `create_database`, a program with `POST /api/v1/db/{newName}/create` and the
  body `{"depends": [...]}`, or pyvolca's `create_database()`. It holds no file:
  its processes are the ones written into it, kept in its journal and back
  after a restart, and their suppliers come from the loaded databases it
  depends on, at least one. It counts against the hosting quota as a copy
  does, and is deleted like any other database of one's own (wire revision
  129).
