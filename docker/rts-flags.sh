#!/bin/sh
# Compute GHC RTS flags based on available hardware.
# Usage: eval $(./rts-flags.sh)   → sets RTS_FLAGS

CORES=$(nproc 2>/dev/null || echo 4)

# Detect available memory: prefer Docker/cgroup limit over total host RAM
CGROUP_LIMIT=""
if [ -f /sys/fs/cgroup/memory.max ]; then
    # cgroup v2
    CGROUP_LIMIT=$(cat /sys/fs/cgroup/memory.max 2>/dev/null)
elif [ -f /sys/fs/cgroup/memory/memory.limit_in_bytes ]; then
    # cgroup v1
    CGROUP_LIMIT=$(cat /sys/fs/cgroup/memory/memory.limit_in_bytes 2>/dev/null)
fi

RAM_KB=$(grep MemTotal /proc/meminfo 2>/dev/null | awk '{print $2}')
RAM_MB=$((RAM_KB / 1024))

# If a cgroup limit is set and smaller than total RAM, use it
if [ -n "$CGROUP_LIMIT" ] && [ "$CGROUP_LIMIT" != "max" ] && [ "$CGROUP_LIMIT" -lt $((RAM_KB * 1024)) ] 2>/dev/null; then
    RAM_MB=$((CGROUP_LIMIT / 1024 / 1024))
fi

# Heap: ~12% of RAM, capped at 2G, minimum 512M
# Live data rarely exceeds 2GB even with multiple databases loaded
HEAP_MB=$((RAM_MB / 8))
[ $HEAP_MB -gt 2048 ] && HEAP_MB=2048
[ $HEAP_MB -lt 512 ] && HEAP_MB=512

# Nursery: -A is the size of ONE capability's allocation area, and -N gives one
# capability per core, so the RTS reserves CORES times this on its own.
# Multiplying by CORES here as well made the TOTAL grow with the square of the
# core count: on a 24-core host, 24 x 384MB, over nine gigabytes of nursery
# before a line was read. The per-capability value that landed on was measured,
# for throughput, on this very shape; what was never measured is what it costs
# in memory, and it is why a database that reads in 8GB on a small machine
# needed 19GB on a large one.
#
# The total is budgeted instead, at a thirty-second of RAM, each capability
# taking its share up to 128MB. The ceiling is where the trade stops paying,
# and it is a trade: reading one 661MB CSV on 24 cores, the peak falls from
# 15.8GB at 384MB per capability to 8.3GB at 128MB, 6.9GB at 64MB and 6.0GB at
# 16MB, while the load takes 38s, 44s, 58s and 81s. Memory is the criterion
# here; someone benchmarking speed alone will find the old value faster and
# should not put it back without reading the peak beside it.
#
# -H below is a second, elastic claim on the same area ("use whatever is left
# over to increase -A"), so -A is the operative size only once the live set has
# passed -H, which on any real database is the first few seconds of a load.
NURSERY_MB=$((RAM_MB / 32 / CORES))
[ $NURSERY_MB -gt 128 ] && NURSERY_MB=128
[ $NURSERY_MB -lt 16 ] && NURSERY_MB=16

# Max heap: 75% of RAM, minimum 2G
MAX_MB=$((RAM_MB * 3 / 4))
[ $MAX_MB -lt 2048 ] && MAX_MB=2048

# -n is left off, which is not the same as off: GHC's own default is 4m for any
# -A of 16m or larger, and the floor below keeps every shape there. The explicit
# 8m to 12m chunk went with the oversized area it was slicing; 4m is what the
# measurements above were taken with.
# -Fd1.0 (GHC 9.10+): return free heap blocks to the OS over ~1 idle period
# instead of holding them indefinitely after a parsing spike. Default decay
# (4.0) keeps RSS pinned near the peak for minutes.
# -I30: idle-time major GC only after 30 s of genuine inactivity. A major GC
# over a multi-GB compacting heap (-c) is a multi-second stop-the-world pause
# on a few-core VM. -I0.3 fired it after every sub-second gap between requests,
# so each interactive page load (/activities, /classifications) stalled ~4 s
# behind a GC. 30 s is longer than any inter-request gap, so the pause never
# lands during active use, yet memory is still released once the VM is idle.
# OOM protection is -M / -F1.5 / -c, all independent of -I.
RTS_FLAGS="+RTS -N -M${MAX_MB}M -H${HEAP_MB}M -A${NURSERY_MB}M -qg0 -c -F1.5 -Fd1.0 -I30 -RTS"

echo "RTS_FLAGS=\"$RTS_FLAGS\""

# Print summary to stderr for visibility
echo "RTS: ${CORES} cores, ${RAM_MB}MB RAM -> ${RTS_FLAGS}" >&2
