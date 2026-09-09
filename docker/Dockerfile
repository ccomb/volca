# VoLCA Docker image: fully-static musl binary on Alpine.
#
# The binary is statically linked against musl libc, so it runs on any Linux
# distro without a runtime libc dependency. Used both as a server image and
# as a source of a portable standalone binary (extract via
# `./docker/docker-build.sh --extract`).
#
# Build:
#   ./docker/docker-build.sh                 # build image (tag volca)
#   ./docker/docker-build.sh --extract       # build + copy binary to ./dist/volca
#
# Verify a built/extracted binary:
#   file dist/volca       # → "statically linked", "ELF 64-bit"
#   ldd dist/volca        # → "not a dynamic executable"

# ALPINE_VERSION is the single source of truth — set in versions.env and
# passed by docker/docker-build.sh. No default here on purpose: a stale
# fallback is exactly how OPENBLAS_VERSION drifted out of sync before.
ARG ALPINE_VERSION
FROM alpine:${ALPINE_VERSION} AS haskell-builder

# musl toolchain + everything MUMPS / GHC need to build statically.
# `*-static` packages ship the .a variants required by `executable-static`.
RUN apk add --no-cache \
    bash \
    binutils \
    curl \
    file \
    git \
    tar \
    xz \
    perl \
    python3 \
    make \
    cmake \
    pkgconfig \
    linux-headers \
    musl-dev \
    libc-dev \
    gcc \
    g++ \
    gfortran \
    gmp-dev \
    gmp-static \
    libffi-dev \
    ncurses-dev \
    ncurses-static \
    zlib-dev \
    zlib-static \
    zstd-dev \
    zstd-static \
    xz-dev \
    xz-static \
    upx

# Copy build scripts and version pins
COPY versions.env /tmp/
COPY build-mumps.sh /tmp/

# Build OpenBLAS from source. Alpine's `lapack-dev` / `blas-dev` ship .so
# only, so a static link can't resolve `-llapack` / `-lblas`. OpenBLAS
# bundles both BLAS and LAPACK into a single libopenblas.a — one source
# build replaces both.
#
# Knobs:
#   NO_SHARED=1            archive-only, no .so
#   USE_THREAD=1,
#   USE_OPENMP=0           pthread parallelism (matches link flags)
#   DYNAMIC_ARCH=1         ship kernel sets for ~12 microarchitectures
#                          (Nehalem, Sandybridge, Haswell, Skylake-X,
#                          Zen, Zen3, …); pick at runtime. For LCA
#                          workloads MUMPS factorization is heavy on
#                          dense BLAS3 (dgemm, dtrsm) on frontal
#                          matrices — hardware-tuned kernels matter:
#                          AVX-512 / Zen-tuned routines run 30–80 %
#                          faster than the AVX2 baseline. The ~13 MB
#                          binary cost compresses well under UPX.
#   TARGET=GENERIC         build-host fallback kernel; runtime still
#                          dispatches via DYNAMIC_ARCH.
#
# We deliberately do NOT pass NO_LAPACKE=1 / NO_CBLAS=1: they only shrink
# the intermediate libopenblas.a, not the final exe (the Haskell →
# MUMPS → Fortran path never resolves cblas_/LAPACKE_ symbols, so
# archive resolution already drops those objects at link time). Adding
# them also breaks OpenBLAS's bundled tests (which reference cblas_*),
# making the default `make` target fail.
RUN . /tmp/versions.env \
    && curl -fsSL "https://github.com/OpenMathLib/OpenBLAS/releases/download/v${OPENBLAS_VERSION}/OpenBLAS-${OPENBLAS_VERSION}.tar.gz" \
        | tar -xz -C /tmp \
    && cd "/tmp/OpenBLAS-${OPENBLAS_VERSION}" \
    && make -j"$(nproc)" \
        NO_SHARED=1 \
        USE_THREAD=1 USE_OPENMP=0 \
        DYNAMIC_ARCH=1 \
        TARGET=GENERIC \
    && make PREFIX=/build/openblas NO_SHARED=1 install \
    && cd / && rm -rf "/tmp/OpenBLAS-${OPENBLAS_VERSION}"

# Build MUMPS from source against musl, linking BLAS/LAPACK from our
# locally-built OpenBLAS. `-fPIC` is OK in static archives; what we need
# to avoid is OS-installed glibc archives, which we don't have here at all.
RUN . /tmp/versions.env && \
    MUMPS_VERSION="$MUMPS_VERSION" \
    OUTPUT_DIR="/build/mumps" \
    BUILD_DIR="/tmp/mumps-build" \
    BLAS_LIBS="-L/build/openblas/lib -lopenblas" \
    /tmp/build-mumps.sh && \
    rm -rf /tmp/mumps-build

# Install ghcup; on Alpine it auto-selects the musl-targeted GHC bindist.
ENV GHCUP_INSTALL_BASE_PREFIX=/opt
ENV PATH="/opt/.ghcup/bin:$PATH"
RUN . /tmp/versions.env && \
    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_GHC_VERSION=${GHC_VERSION} \
    BOOTSTRAP_HASKELL_CABAL_VERSION=latest \
    BOOTSTRAP_HASKELL_INSTALL_NO_STACK=1 \
    sh

WORKDIR /build

# Copy ONLY dependency specification files first (for layer caching)
COPY volca.cabal /build/volca/
COPY mumps-hs/ /build/mumps-hs/
COPY gen-cabal-config.sh /build/volca/

# Set up cabal.project with mumps-hs as local package, LINK_MODE=musl.
# This project is written here rather than copied, so it inherits nothing
# from the repository's own cabal.project: the hold on
# insert-ordered-containers has to be repeated, and allow-newer above
# would let the breaking version in otherwise.
# 2 because this image is normally the published one. A caller building an
# image from source to try something out can pass 1 and get the compile back
# in a fraction of the time; see gen-cabal-config.sh for what each level is for.
ARG VOLCA_OPT_LEVEL=2

RUN echo "packages: ./volca ./mumps-hs" > /build/cabal.project \
    && echo "allow-newer: true" >> /build/cabal.project \
    && echo "constraints: insert-ordered-containers < 0.3" >> /build/cabal.project \
    && MUMPS_LIB_DIR="/build/mumps/lib" \
    MUMPS_INCLUDE_DIR="/build/mumps/include" \
    OPENBLAS_LIB_DIR="/build/openblas/lib" \
    LINK_MODE="musl" \
    OUTPUT_DIR="/build" \
    VOLCA_OPT_LEVEL="$VOLCA_OPT_LEVEL" \
    /build/volca/gen-cabal-config.sh

# Update cabal and build ONLY dependencies (cached layer)
RUN cabal update \
    && cd /build && cabal build --only-dependencies volca

# NOW copy the rest of the source
COPY . /build/volca

# Generate Version.hs (.git excluded from Docker context, so pass via build args)
ARG GIT_HASH=unknown
ARG GIT_TAG=""
RUN cd /build/volca && GIT_HASH="$GIT_HASH" GIT_TAG="$GIT_TAG" ./gen-version.sh

# Build volca (only recompiles app code, deps already cached)
RUN cd /build && cabal build volca

# Strip, UPX-compress, and verify the binary is fully static. Use `file`:
# glibc and musl ldd disagree on the wording for static binaries (glibc says
# "not a dynamic executable", musl says "Not a valid dynamic program" with
# non-zero exit), but ELF "statically linked" from `file` is unambiguous.
# UPX (default level, NRV2B codec) gives ~3–4× compression with negligible
# startup cost; --best/--lzma would shrink further but pay hundreds of ms
# decompression on every invocation, which the volca CLI cannot absorb.
RUN mkdir -p /build/output \
    && cp $(cd /build && cabal list-bin exe:volca) /build/output/volca \
    && strip /build/output/volca \
    && file /build/output/volca | grep -q "statically linked" \
        || { echo "ERROR: binary is not statically linked"; exit 1; } \
    && echo "Size before UPX: $(du -h /build/output/volca | cut -f1)" \
    && upx /build/output/volca \
    && echo "Size after UPX:  $(du -h /build/output/volca | cut -f1)" \
    # Guard the musl pthread-stack fix from PR #60/#61. The link-time
    # -Wl,-z,stack-size=8388608 (see gen-cabal-config.sh, LINK_MODE=musl)
    # bakes an 8 MiB PT_GNU_STACK header into the ELF; musl reads it at
    # startup as __default_stacksize. If a future linker-flag refactor
    # drops it — or UPX strips it — every pthread falls back to musl's
    # hardcoded 128 KB and OpenBLAS DYNAMIC_ARCH Fortran workers SIGSEGV
    # on the first BLAS3 call inside MUMPS factorization (exit 139).
    # Fail the build loudly here rather than rediscover it from a
    # production crash log a third time.
    && { STACK_MEMSZ=$(readelf -l /build/output/volca | grep -A1 GNU_STACK | tail -n1 | awk '{print $2}'); \
         echo "$STACK_MEMSZ" | grep -qE '^0x0*800000$' \
         || { echo "ERROR: PT_GNU_STACK MemSiz=$STACK_MEMSZ on shipped binary, expected 8 MiB (0x800000) — musl default pthread stack would fall back to 128 KB; OpenBLAS workers will SIGSEGV in MUMPS factorization. See PR #60/#61."; exit 1; }; } \
    && file /build/output/volca

# Stage 2: small runtime image. The volca binary is fully static (no libc
# needed) so `scratch` would work, but Alpine gives us /etc/passwd, a shell
# for debugging, and the few utilities volca shells out to (7z for SimaPro
# CSV archives) at negligible size cost.
ARG ALPINE_VERSION
FROM alpine:${ALPINE_VERSION}
ARG GIT_HASH=unknown
LABEL org.opencontainers.image.revision="${GIT_HASH}"

# musl-locales / musl-locales-lang give us a working en_US.UTF-8 (musl's
# native locale support is minimal; this package fills in LC_MESSAGES /
# LC_TIME translations enough for downstream tools that probe LANG).
RUN apk add --no-cache \
    ca-certificates \
    tzdata \
    7zip \
    musl-locales \
    musl-locales-lang

ENV LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8

COPY --from=haskell-builder /build/output/volca /usr/local/bin/volca

WORKDIR /app

# /data/ is the engine's writing root (uploads, editable copies, journals)
# and the home of the shipped method below. The reference tables themselves
# are built into the binary, so a bind mount over /data/ hides nothing the
# engine needs. The copy happens BEFORE the volume declaration so a fresh
# named volume starts with the method (Docker populates an empty named
# volume with whatever the image holds at the volume path). A configuration
# that names a `data/...` file of its own still resolves it here, through
# VOLCA_DATA_DIR (Config.redirectIntoDataDir).
COPY data/ /data/

ENV VOLCA_DATA_DIR=/data
VOLUME /data

RUN cat > /app/volca.toml <<'EOF'
# The reference tables (flow synonyms, compartment mapping, units, energy
# densities) and the geographies are built into the engine and active; a
# configuration names one only to replace it with a file or to switch it off.
# The chemical synonyms are not built in: this top-level key names the file
# the image ships, and must stay above the first [section] header.
chem-synonyms = "data/chem_synonyms.csv"

[server]
port = 8080
host = "0.0.0.0"

# Method paths are user content that VOLCA_DATA_DIR does not rewrite, so the
# shipped method needs the absolute path; a relative one would resolve under
# /app and the image would start with no method at all.
[[methods]]
name = "plain-indicators"
path = "/data/methods/plain-indicators.csv"
active = true
description = "Raw physical quantities counted through the supply chain (CF=1.0)"
EOF

COPY docker/rts-flags.sh /app/rts-flags.sh
COPY docker/docker-entrypoint.sh /app/docker-entrypoint.sh

EXPOSE 8080

ENTRYPOINT ["/app/docker-entrypoint.sh"]
CMD ["--config", "/app/volca.toml", "server"]
