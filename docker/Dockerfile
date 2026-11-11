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

# ALPINE_VERSION is the single source of truth – set in versions.env and
# passed by docker/docker-build.sh. No default here on purpose: a stale
# fallback is how a pin drifts out of sync.
ARG ALPINE_VERSION
FROM alpine:${ALPINE_VERSION} AS haskell-builder

# musl toolchain + everything GHC needs to build statically.
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

# Copy the version pins
COPY versions.env /tmp/

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
COPY gen-cabal-config.sh /build/volca/

# Set up cabal.project with LINK_MODE=musl.
# This project is written here rather than copied, so it inherits nothing
# from the repository's own cabal.project: the hold on
# insert-ordered-containers has to be repeated, and allow-newer above
# would let the breaking version in otherwise.
RUN echo "packages: ./volca" > /build/cabal.project \
    && echo "allow-newer: true" >> /build/cabal.project \
    && echo "constraints: insert-ordered-containers < 0.3" >> /build/cabal.project \
    && LINK_MODE="musl" \
    OUTPUT_DIR="/build" \
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

# The level volca's own code is compiled at. 2, because this image is normally
# the published one; a caller building from source to try something out passes
# 1 and gets the compile back in a fraction of the time. See
# gen-cabal-config.sh for what each level means.
#
# Declared here rather than beside the first call, which is why that call is
# regenerating the same file: docker adds an ARG to the cache key of every
# layer from its first use onwards, so naming the level any earlier would
# rebuild the whole musl dependency tree each time it changed. Dependencies
# compile at the global optimization: 2 whatever this is, so they have no
# business depending on it.
ARG VOLCA_OPT_LEVEL=2
RUN LINK_MODE="musl" \
    OUTPUT_DIR="/build" \
    VOLCA_OPT_LEVEL="$VOLCA_OPT_LEVEL" \
    /build/volca/gen-cabal-config.sh

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
    # The link asks for 8 MiB of stack per thread (see gen-cabal-config.sh,
    # LINK_MODE=musl), which musl reads from the PT_GNU_STACK header. A
    # linker flag dropped or a header UPX strips would leave every thread
    # on musl's 128 KB without a word, so read it back on the shipped binary.
    && { STACK_MEMSZ=$(readelf -l /build/output/volca | grep -A1 GNU_STACK | tail -n1 | awk '{print $2}'); \
         echo "$STACK_MEMSZ" | grep -qE '^0x0*800000$' \
         || { echo "ERROR: PT_GNU_STACK MemSiz=$STACK_MEMSZ on shipped binary, expected 8 MiB (0x800000) – threads would fall back to musl's 128 KB of stack."; exit 1; }; } \
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
# The licences of the libraries linked into the binary ask for their notices
# and texts to travel with it.
COPY LICENSE NOTICE THIRD_PARTY_LICENSES.md /usr/share/licenses/volca/
COPY licenses/ /usr/share/licenses/volca/licenses/

WORKDIR /app

# /data/ is the engine's writing root (uploads, editable copies, journals)
# and the home of the chemical synonyms file below. The reference tables and
# the plain-indicators method are built into the binary, so a bind mount over
# /data/ hides nothing the engine needs to score. The copy happens BEFORE the
# volume declaration so a fresh named volume starts with that file (Docker
# populates an empty named volume with whatever the image holds at the
# volume path). A configuration that names a `data/...` file of its own
# still resolves it here, through VOLCA_DATA_DIR (Config.redirectIntoDataDir).
COPY data/ /data/

ENV VOLCA_DATA_DIR=/data
VOLUME /data

RUN cat > /app/volca.toml <<'EOF'
# The reference tables (flow synonyms, compartment mapping, units, energy
# densities), the geographies and the plain-indicators method are built into
# the engine and active; a configuration names one only to replace it with a
# file or to switch it off.
# The chemical synonyms are not built in: this top-level key names the file
# the image ships, and must stay above the first [section] header.
chem-synonyms = "data/chem_synonyms.csv"

[server]
port = 8080
host = "0.0.0.0"
EOF

COPY docker/rts-flags.sh /app/rts-flags.sh
COPY docker/docker-entrypoint.sh /app/docker-entrypoint.sh

EXPOSE 8080

ENTRYPOINT ["/app/docker-entrypoint.sh"]
CMD ["--config", "/app/volca.toml", "server"]
