# Building and testing VoLCA

Most people only need a released binary: see the installation section of the
[README](../README.md). This page is for building the engine from source.

## Linux

Install dependencies, then build:

```bash
# Debian/Ubuntu
sudo apt install build-essential python3 curl zlib1g-dev upx-ucl

# Fedora
sudo dnf install gcc gcc-c++ make python3 curl zlib-devel

# Arch Linux
sudo pacman -S base-devel python curl zlib
```

Install the [Haskell toolchain via GHCup](https://www.haskell.org/ghcup/), then:

```bash
./build.sh              # Build
./build.sh --test       # Build and run tests
```

`build.sh` also accepts:

- `--clean` / `--all` – discard `dist-newstyle/` before building
- `--coverage` – run tests with coverage and emit an HTML report
- `--static` – produce a statically-linked binary (Linux only)
- `--no-optimize` – skip `strip` and UPX, leaving the binary as linked.
  The macOS test run of CI uses it, then strips and signs with
  `--optimize-only`.

A build that ships splits every object into one ELF section per top-level
symbol so the executable can be pruned to size, and that link is the one the
default `ld.bfd` is slowest at: about 22 seconds, where every alternative
takes one or two. Below `-O2` the split is off, since nothing prunes a test
suite and the sections would be pure cost, and the link is a few seconds
whatever the linker. Changing linker changes the link alone, so it costs
nothing on an already compiled tree:

```bash
cabal build exe:volca --ghc-options=-optl-fuse-ld=lld
```

`-fuse-ld=` looks for a plain `ld.lld`, `ld.mold` or `ld.gold` on the
`PATH`, and will not find a versioned `ld.lld-21`.

## macOS (Apple Silicon)

Tested on macOS 13 Ventura and later, arm64 only. The build pins
`MACOSX_DEPLOYMENT_TARGET=13.0` so binaries produced on a recent macOS still
load on Ventura.

```bash
# Xcode Command Line Tools (provides clang, ld64, the macOS SDK)
xcode-select --install

# Homebrew
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
eval "$(/opt/homebrew/bin/brew shellenv)"

brew install ghcup python3 curl node rust elm upx

# Haskell toolchain
ghcup install ghc 9.12.4 --set
ghcup install cabal latest
source ~/.ghcup/env

./build.sh              # Build
./build.sh --test       # Build and run tests
```

## Windows (MSYS2)

1. Install [MSYS2](https://www.msys2.org/) and open the "MSYS2 UCRT64" terminal
2. Install dependencies:
   ```bash
   pacman -S \
     mingw-w64-ucrt-x86_64-gcc \
     mingw-w64-ucrt-x86_64-cmake \
     mingw-w64-ucrt-x86_64-make \
     mingw-w64-ucrt-x86_64-python \
     mingw-w64-ucrt-x86_64-upx \
     make python git curl tar
   ```
3. Install [GHCup](https://www.haskell.org/ghcup/) for the compiler toolchain
4. Run:
   ```bash
   ./build.sh            # Same script as Linux/macOS
   ```

## Docker

```bash
docker build -f docker/Dockerfile -t volca .
docker run -p 8080:8080 -v /path/to/data:/data volca
```

## Testing

```bash
./build.sh --test

# One spec group, in a tree that script has already built
cabal test lca-tests --test-options="--match /Inventory/"
```

`./build.sh --test` is the way in: it generates `src/Version.hs`, compiles at
`-O0`, builds the `volca` executable and names it to the specs that start a
server. A bare `cabal test` from a fresh clone does none of those: it stops on
the missing `src/Version.hs`, and once that is generated the specs that want the
executable stop too. `AGENTS.md` says the rest.

Tests cover matrix construction (sign convention), inventory calculation (golden values), parsers (EcoSpold1/2, ILCD, SimaPro, Brightway Excel, classification fields), and matrix export format compliance.
