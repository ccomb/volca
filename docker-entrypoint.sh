#!/bin/sh
# Docker entrypoint: compute RTS flags then exec volca with all arguments.
eval $(/app/rts-flags.sh)
exec volca $RTS_FLAGS "$@"
