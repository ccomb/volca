# Stage 1: Build Haskell application
# Build from volca directory: docker build -t volca .
FROM debian:bookworm AS haskell-builder

# Install system dependencies
RUN apt-get update && apt-get install -y \
    curl \
    libgmp-dev \
    zlib1g-dev \
    libzstd-dev \
    pkg-config \
    nodejs \
    npm \
    git \
    build-essential \
    gfortran \
    libnuma-dev \
    libncurses-dev \
    unzip \
    libblas-dev \
    liblapack-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy build scripts and version pins
COPY versions.env /tmp/
COPY build-mumps.sh /tmp/

# Build MUMPS from source (cached until MUMPS_VERSION or build-mumps.sh changes)
RUN . /tmp/versions.env && \
    MUMPS_VERSION="$MUMPS_VERSION" \
    OUTPUT_DIR="/build/mumps" \
    BUILD_DIR="/tmp/mumps-build" \
    /tmp/build-mumps.sh && \
    rm -rf /tmp/mumps-build

# Install ghcup and GHC (version from versions.env)
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

# Set up cabal.project with mumps-hs as local package
# Static MUMPS: link .a libs into the binary so the runtime image needs no MUMPS package
RUN echo "packages: ./volca ./mumps-hs" > /build/cabal.project \
    && echo "allow-newer: true" >> /build/cabal.project \
    && MUMPS_LIB_DIR="/build/mumps/lib" \
    MUMPS_INCLUDE_DIR="/build/mumps/include" \
    LINK_MODE="static" \
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

# Build volca (only recompiles app code, deps already cached)
RUN cd /build && cabal build volca

# Build Elm frontend (uses local npm packages)
RUN cd /build/volca/web && npm install && ./build.sh

# Find and copy the executable
RUN mkdir -p /build/output \
    && cp $(cd /build && cabal list-bin exe:volca) /build/output/volca

# Stage 2: Runtime image
FROM debian:bookworm-slim
ARG GIT_HASH=unknown
LABEL org.opencontainers.image.revision="${GIT_HASH}"

# MUMPS is statically linked into the binary; only its runtime BLAS/Fortran deps are needed
RUN apt-get update && apt-get install -y \
    libgfortran5 \
    libgomp1 \
    libgmp10 \
    zlib1g \
    libzstd1 \
    libblas3 \
    liblapack3 \
    ca-certificates \
    locales \
    7zip \
    && rm -rf /var/lib/apt/lists/* \
    && sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen \
    && locale-gen

ENV LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8

# Copy application
COPY --from=haskell-builder /build/output/volca /usr/local/bin/volca
COPY --from=haskell-builder /build/volca/web/dist /app/web/dist

WORKDIR /app

# User data directory (uploads, cache) - mount as volume for persistence
ENV VOLCA_DATA_DIR=/data
VOLUME /data

# Copy runtime config and data (staged by docker-build.sh)
COPY .docker-bundle/volca.toml /app/volca.toml
COPY .docker-bundle/data/ /app/data/

COPY rts-flags.sh /app/rts-flags.sh
COPY docker-entrypoint.sh /app/docker-entrypoint.sh

EXPOSE 8080

ENTRYPOINT ["/app/docker-entrypoint.sh"]
CMD ["--config", "/app/volca.toml", "server"]
