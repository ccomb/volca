- Finalizing an uploaded database that needs more memory than the engine may
  use now says so, and says the engine is stopping. It used to answer
  "Server exception: KilledByThreadManager (Just heap overflow)".
- The Docker image stops as soon as it is asked to. `docker stop` used to wait
  its ten seconds out and then kill the engine, and a machine shutting down
  with the container running waited for it up to a minute and a half: the
  engine ran as the container's first process, to which a stop request is
  never delivered. Stopping now takes a third of a second.
