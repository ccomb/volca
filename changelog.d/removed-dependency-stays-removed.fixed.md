- A dependency removed on a database's setup page no longer comes back. The
  database was read again from its source files whenever its cache still had
  inputs unresolved and another database was loaded, and linked to every
  database loaded, the removed one included; after a restart the choice was
  lost altogether.
