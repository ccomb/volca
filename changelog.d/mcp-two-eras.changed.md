- The MCP server now speaks the 2026-07-28 revision of the protocol, which
  recent assistants try first: they open with `server/discover` instead of
  falling back to the older handshake. Clients that
  still open with `initialize` are answered as before, as 2025-03-26.
