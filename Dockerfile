# Stage 1: Build PETSc from source (static libraries)
# Configuration is centralized in petsc.env and versions.env
FROM debian:bookworm-slim AS petsc-builder

RUN apt-get update && apt-get install -y \
    build-essential \
    gfortran \
    python3 \
    wget \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt

# Copy build scripts and configuration
COPY versions.env petsc.env build-petsc.sh /tmp/

# Download and build PETSc using shared script
ENV PETSC_DIR=/opt/petsc
ENV PETSC_ARCH=arch-linux-c-opt
RUN cd /opt && set -a && . /tmp/versions.env && . /tmp/petsc.env && set +a && \
    PETSC_DIR=/opt/petsc \
    PETSC_PLATFORM_OPTS="$PETSC_CONFIGURE_DOCKER" \
    /tmp/build-petsc.sh

# Stage 2: Build Haskell application
# Build from volca directory: docker build -t volca .
FROM debian:bookworm AS haskell-builder

# Install system dependencies (no ghc/cabal - we use ghcup)
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
    && rm -rf /var/lib/apt/lists/*

# Copy versions.env for GHC version
COPY versions.env /tmp/

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

# Install c2hs via cabal
RUN cabal update && cabal install c2hs --install-method=copy --installdir=/usr/local/bin

# Copy PETSc from builder (no SLEPc)
COPY --from=petsc-builder /opt/petsc /opt/petsc

ENV PETSC_DIR=/opt/petsc
ENV PETSC_ARCH=arch-linux-c-opt
ENV LD_LIBRARY_PATH=/opt/petsc/${PETSC_ARCH}/lib

WORKDIR /build

# Clone petsc-hs from GitHub (same as build.sh does)
# ADD fetches the branch ref from GitHub API - cache is invalidated when commit SHA changes
ADD https://api.github.com/repos/ccomb/petsc-hs/git/refs/heads/master /tmp/petsc-hs-version
RUN git clone https://github.com/ccomb/petsc-hs.git /build/petsc-hs

# Generate and process c2hs files for petsc-hs
# 1. Generate TypesC2HsGen.chs from GenerateC2Hs.hs (outputs to stdout)
# 2. Run c2hs to generate TypesC2HsGen.hs from the .chs file
RUN cd /build/petsc-hs/src/Numerical/PETSc/Internal/C2HsGen && \
    runhaskell GenerateC2Hs.hs > TypesC2HsGen.chs && \
    c2hs -C -I/opt/petsc/include \
         -C -I/opt/petsc/${PETSC_ARCH}/include \
         TypesC2HsGen.chs

# Copy ONLY dependency specification files first (for layer caching)
COPY volca.cabal /build/volca/
COPY gen-cabal-config.sh /build/volca/

# Set up cabal.project with petsc-hs as local package
# Docker uses dynamic linking (PETSc/MPI libs copied to runtime image)
RUN echo "packages: ./volca ./petsc-hs" > /build/cabal.project \
    && echo "allow-newer: true" >> /build/cabal.project \
    && PETSC_LIB_DIR="/opt/petsc/${PETSC_ARCH}/lib" \
    PETSC_INCLUDE_DIR="/opt/petsc/include" \
    PETSC_ARCH_INCLUDE_DIR="/opt/petsc/${PETSC_ARCH}/include" \
    LINK_MODE="dynamic" \
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

# Stage 3: Runtime image (slim — no PETSc libs needed, statically linked)
FROM debian:bookworm-slim
ARG GIT_HASH=unknown
LABEL org.opencontainers.image.revision="${GIT_HASH}"

RUN apt-get update && apt-get install -y \
    libgfortran5 \
    libgomp1 \
    libgmp10 \
    zlib1g \
    libzstd1 \
    ca-certificates \
    locales \
    7zip \
    && rm -rf /var/lib/apt/lists/* \
    && sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen \
    && locale-gen

ENV LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8

# Copy PETSc runtime libraries (shared linking)
COPY --from=petsc-builder /opt/petsc/arch-linux-c-opt/lib /opt/petsc/lib

# Copy application
COPY --from=haskell-builder /build/output/volca /usr/local/bin/volca
COPY --from=haskell-builder /build/volca/web/dist /app/web/dist

ENV LD_LIBRARY_PATH=/opt/petsc/lib

# PETSC_OPTIONS: MUMPS direct solver settings
# -malloc_hbw false is Docker-specific (no high-bandwidth memory)
ENV PETSC_OPTIONS="-pc_type lu -pc_factor_mat_solver_type mumps -mat_mumps_icntl_14 80 -mat_mumps_icntl_24 1 -malloc_hbw false -no_signal_handler"

WORKDIR /app

# User data directory (uploads, cache) - mount as volume for persistence
ENV VOLCA_DATA_DIR=/data
VOLUME /data

# Copy runtime config (edit volca.docker.toml to add/remove databases)
COPY volca.docker.toml /app/volca.toml
COPY data/ /app/data/

COPY rts-flags.sh /app/rts-flags.sh
COPY docker-entrypoint.sh /app/docker-entrypoint.sh

EXPOSE 8080

ENTRYPOINT ["/app/docker-entrypoint.sh"]
CMD ["--config", "/app/volca.toml", "server"]
