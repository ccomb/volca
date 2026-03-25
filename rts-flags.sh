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

# Heap: ~12% of RAM, capped at 4G, minimum 512M
HEAP_MB=$((RAM_MB / 8))
[ $HEAP_MB -gt 4096 ] && HEAP_MB=4096
[ $HEAP_MB -lt 512 ] && HEAP_MB=512

# Nursery: ~16MB per core, minimum 64M
NURSERY_MB=$((CORES * 16))
[ $NURSERY_MB -lt 64 ] && NURSERY_MB=64

# Chunk size: nursery / 32, minimum 8m
CHUNK_MB=$((NURSERY_MB / 32))
[ $CHUNK_MB -lt 8 ] && CHUNK_MB=8

# Max heap: 75% of RAM, capped at 24G, minimum 2G
# Leaves 25% for OS, PETSc, and co-located services
MAX_MB=$((RAM_MB * 3 / 4))
[ $MAX_MB -gt 24576 ] && MAX_MB=24576
[ $MAX_MB -lt 2048 ] && MAX_MB=2048

RTS_FLAGS="+RTS -N -M${MAX_MB}M -H${HEAP_MB}M -A${NURSERY_MB}M -n${CHUNK_MB}m -qg0 -c -I30 -RTS"

echo "RTS_FLAGS=\"$RTS_FLAGS\""

# Print summary to stderr for visibility
echo "RTS: ${CORES} cores, ${RAM_MB}MB RAM -> ${RTS_FLAGS}" >&2
