#!/bin/bash
# Download and build MUMPS sequential solver from source.
# Produces static libraries in $OUTPUT_DIR/lib/ and headers in $OUTPUT_DIR/include/.
#
# Shared between build.sh (Linux) and Dockerfile.
#
# Required env vars (from versions.env):
#   MUMPS_VERSION   - e.g. 5.8.1
#
# Optional env vars:
#   OUTPUT_DIR      - install prefix (default: ./deps/mumps)
#   BUILD_DIR       - where to download/compile (default: ./deps/build)
#   BLAS_LIBS       - BLAS/LAPACK link flags (default: -llapack -lblas)
#   JOBS            - parallel build jobs (default: nproc)

set -e

: "${MUMPS_VERSION:?MUMPS_VERSION is required}"
OUTPUT_DIR="${OUTPUT_DIR:-$(pwd)/deps/mumps}"
BUILD_DIR="${BUILD_DIR:-$(pwd)/deps/build}"
BLAS_LIBS="${BLAS_LIBS:--llapack -lblas}"
JOBS="${JOBS:-$(nproc 2>/dev/null || echo 4)}"

TARBALL="${BUILD_DIR}/MUMPS_${MUMPS_VERSION}.tar.gz"
SRCDIR="${BUILD_DIR}/MUMPS_${MUMPS_VERSION}"

# Already built?
if [[ -f "${OUTPUT_DIR}/lib/libdmumps_seq.a" ]]; then
    echo "[INFO] MUMPS ${MUMPS_VERSION} already built at ${OUTPUT_DIR}, skipping"
    exit 0
fi

mkdir -p "$BUILD_DIR"

# Download (ANL mirror first, mumps-solver.org as fallback)
if [[ ! -f "$TARBALL" ]]; then
    echo "[INFO] Downloading MUMPS ${MUMPS_VERSION}..."
    ANL="https://web.cels.anl.gov/projects/petsc/download/externalpackages/MUMPS_${MUMPS_VERSION}.tar.gz"
    OFFICIAL="https://mumps-solver.org/MUMPS_${MUMPS_VERSION}.tar.gz"
    if ! curl -L --fail --silent --show-error -o "$TARBALL" "$ANL"; then
        echo "[INFO] ANL mirror failed, trying mumps-solver.org..."
        curl -L --fail --show-error -o "$TARBALL" "$OFFICIAL" \
            || { echo "[ERROR] Failed to download MUMPS ${MUMPS_VERSION}" >&2; rm -f "$TARBALL"; exit 1; }
    fi
fi

# Extract
if [[ ! -d "$SRCDIR" ]]; then
    echo "[INFO] Extracting MUMPS..."
    tar xzf "$TARBALL" -C "$BUILD_DIR"
fi

cd "$SRCDIR"

# Write Makefile.inc for sequential, double-precision, static build.
#
# Key variables:
#   PLAT=_seq        - appends _seq suffix to all library names
#   LPORDDIR         - enables the internal PORD ordering library (required)
#   ORDERINGSF=-Dpord - activates PORD in Fortran/C code
#   LIBSEQNEEDED     - builds the stub MPI library (libmpiseq_seq.a)
#
# NOTE: AR, OUTC, OUTF must have trailing spaces — MUMPS Makefiles concatenate
# these variables directly with filenames: $(AR)$@, $(OUTC)file.o, $(OUTF)file.o
# Using printf (not heredoc) to guarantee trailing spaces are not stripped.
printf '%s\n' \
    'PLAT    = _seq' \
    'LIBEXT  = .a' \
    'OUTC    = -o ' \
    'OUTF    = -o ' \
    'RM      = /bin/rm -f' \
    'CC      = gcc' \
    'FC      = gfortran' \
    'FL      = gfortran' \
    'AR      = ar vr ' \
    'RANLIB  = ranlib' \
    "LIBBLAS = ${BLAS_LIBS}" \
    'SCALAP  =' \
    'LPORDDIR = $(topdir)/PORD/lib/' \
    'IPORD    = -I$(topdir)/PORD/include/' \
    'LPORD    = -L$(LPORDDIR) -lpord$(PLAT)' \
    'ORDERINGSF = -Dpord' \
    'ORDERINGSC = $(ORDERINGSF)' \
    'LORDERINGS  = $(LPORD)' \
    'IORDERINGSF =' \
    'IORDERINGSC = $(IPORD)' \
    'INCPAR  = -I$(topdir)/libseq' \
    'LIBPAR  = $(topdir)/libseq/libmpiseq_seq.a' \
    'LIBC    =' \
    'LIBOTHERS = -lpthread' \
    'CDEFS   = -DAdd_' \
    'OPTF    = -O3 -fPIC -fallow-argument-mismatch' \
    'OPTC    = -O3 -fPIC' \
    'OPTL    = -O3' \
    'INCS    = $(INCPAR) $(IORDERINGSC)' \
    'LIBS    = $(LIBPAR)' \
    'LIBSEQNEEDED = libseqneeded' \
    > Makefile.inc

echo "[INFO] Building MUMPS ${MUMPS_VERSION} (sequential, double precision, ${JOBS} jobs)..."
# Build prerequisites (libseq stub + PORD), then src only — skips examples
# which would require BLAS at link time and aren't needed for the libraries.
make -j"$JOBS" prerequisites
make -j"$JOBS" -C src d

# Install libraries and headers
mkdir -p "${OUTPUT_DIR}/lib" "${OUTPUT_DIR}/include"
cp lib/libdmumps_seq.a lib/libmumps_common_seq.a lib/libpord_seq.a "${OUTPUT_DIR}/lib/"
cp libseq/libmpiseq_seq.a "${OUTPUT_DIR}/lib/"
# MUMPS headers + stub MPI headers (mpi.h required by mumps_c.h) + PORD headers
cp include/*.h          "${OUTPUT_DIR}/include/"
cp libseq/*.h           "${OUTPUT_DIR}/include/"
cp PORD/include/*.h     "${OUTPUT_DIR}/include/"

echo "[OK] MUMPS ${MUMPS_VERSION} installed to ${OUTPUT_DIR}"
