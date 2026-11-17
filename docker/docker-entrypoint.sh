#!/bin/sh
# Docker entrypoint: compute RTS flags then exec volca with all arguments.
eval $(/app/rts-flags.sh)
# Under tini, not as the container's first process: the kernel drops a stop
# signal sent to the first process unless it has a handler for it, and the
# engine has none. `docker stop` then waited its ten seconds out and killed it.
exec tini -- volca $RTS_FLAGS "$@"
