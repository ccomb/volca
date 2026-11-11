# Third-party licenses

VoLCA is licensed under the Apache License, Version 2.0 (see [LICENSE](LICENSE)).
This document inventories the third-party software bundled with, or linked into,
a VoLCA distribution, and travels in every downloaded archive. See also
[NOTICE](NOTICE) for the abridged summary. Component versions are pinned in
`versions.env`.

## GCC runtime – GPLv3 with the GCC Runtime Library Exception

libgcc, the compiler's support routines and its stack unwinder, linked into
the binary on Linux and Windows. The Exception covers this binary: it is
compiled by GHC, clang and the Rust compiler, which are not works based on
GCC - an Eligible Compilation Process as the Exception defines it. Nothing
further is owed.
Texts: <https://gcc.gnu.org/onlinedocs/gcc/Copying.html> and
<https://www.gnu.org/licenses/gcc-exception-3.1.html>.

## MinGW-w64 runtime linked into the binary, Windows only

The Windows binary is linked with the MSYS2 ucrt64 toolchain and loads no DLL
of it. The zip's `licenses/` directory is MSYS2's own `share/licenses` copied
whole, so it holds the text for what that toolchain linked in and for the rest
of what is installed alongside it. Copied rather than curated: the terms are
certainly in there, and no list here can go stale.

## Sparse solver – Rust crates

The sparse solver in `solver/` is written in Rust and linked statically into
the binary, with Rust's standard library (MIT OR Apache-2.0,
<https://github.com/rust-lang/rust>) and the crates below, each found at
`https://crates.io/crates/<name>`. The table holds the crates whose code cargo
puts in the solver's archive on one platform or another. Those that only run
while compiling (procedural macros and what they are built from) leave no code
of their own and are out of scope. `scripts/check-licenses-table.sh` fails the
build when this table and what cargo reports disagree, on a name or on a
license.

| Crate | License |
|---|---|
| `bitflags` | MIT OR Apache-2.0 |
| `bytemuck` | Zlib OR Apache-2.0 OR MIT |
| `byteorder` | Unlicense OR MIT |
| `cfg-if` | MIT OR Apache-2.0 |
| `dyn-stack` | MIT |
| `equator` | MIT |
| `faer` | MIT |
| `faer-traits` | MIT |
| `gemm` | MIT |
| `gemm-c32` | MIT |
| `gemm-c64` | MIT |
| `gemm-common` | MIT |
| `gemm-f32` | MIT |
| `gemm-f64` | MIT |
| `generativity` | MIT OR Apache-2.0 |
| `hermit-abi` | MIT OR Apache-2.0 |
| `libc` | MIT OR Apache-2.0 |
| `libm` | MIT |
| `nano-gemm` | MIT |
| `nano-gemm-c32` | MIT |
| `nano-gemm-c64` | MIT |
| `nano-gemm-core` | MIT |
| `nano-gemm-f32` | MIT |
| `nano-gemm-f64` | MIT |
| `num-complex` | MIT OR Apache-2.0 |
| `num-traits` | MIT OR Apache-2.0 |
| `num_cpus` | MIT OR Apache-2.0 |
| `once_cell` | MIT OR Apache-2.0 |
| `private-gemm-x86` | MIT |
| `pulp` | MIT |
| `pulp-wasm-simd-flag` | MIT |
| `qd` | MIT |
| `raw-cpuid` | MIT |
| `reborrow` | MIT |
| `same-file` | Unlicense/MIT |
| `sysctl` | MIT |
| `thiserror` | MIT OR Apache-2.0 |
| `walkdir` | Unlicense/MIT |
| `winapi-util` | Unlicense OR MIT |
| `windows-link` | MIT OR Apache-2.0 |
| `windows-sys` | MIT OR Apache-2.0 |

`faer` is MIT as a whole and carries algorithms ported from other projects,
which stay under the terms they came with:

- its minimum-degree ordering, which the solver calls, is ported from AMD –
  BSD 3-Clause, notice at the end of this document;
- a column ordering and several dense decompositions (BSD 3-Clause) and a
  singular value decomposition (MPL-2.0), none of which the solver calls and
  none of which is compiled into the binary.

## Haskell direct dependencies

The following table covers the libraries the shipped binary links: the direct
`build-depends` of `volca.cabal`'s `library` and `executable` stanzas. Test and
benchmark dependencies are out of scope, and so are transitive ones, whose
licenses are uniformly BSD-style or MIT and can be re-derived from a fresh
`cabal-plan license-report` run. `scripts/check-licenses-table.sh` fails the
build when this table and `volca.cabal` disagree.

| Package | License | Source |
|---|---|---|
| aeson | BSD-3-Clause | <https://hackage.haskell.org/package/aeson> |
| aeson-pretty | BSD-3-Clause | <https://hackage.haskell.org/package/aeson-pretty> |
| async | BSD-3-Clause | <https://hackage.haskell.org/package/async> |
| attoparsec | BSD-3-Clause | <https://hackage.haskell.org/package/attoparsec> |
| base | BSD-3-Clause (with GHC exceptions) | bundled with GHC |
| base64-bytestring | BSD-3-Clause | <https://hackage.haskell.org/package/base64-bytestring> |
| bytestring | BSD-3-Clause | bundled with GHC |
| cassava | BSD-3-Clause | <https://hackage.haskell.org/package/cassava> |
| containers | BSD-3-Clause | bundled with GHC |
| crypton | BSD-3-Clause | <https://hackage.haskell.org/package/crypton> |
| deepseq | BSD-3-Clause | bundled with GHC |
| directory | BSD-3-Clause | bundled with GHC |
| exceptions | BSD-3-Clause | bundled with GHC |
| filepath | BSD-3-Clause | bundled with GHC |
| haskeline | BSD-3-Clause | <https://hackage.haskell.org/package/haskeline> |
| http-client | MIT | <https://hackage.haskell.org/package/http-client> |
| http-client-tls | MIT | <https://hackage.haskell.org/package/http-client-tls> |
| http-media | MIT | <https://hackage.haskell.org/package/http-media> |
| http-types | BSD-3-Clause | <https://hackage.haskell.org/package/http-types> |
| insert-ordered-containers | BSD-3-Clause | <https://hackage.haskell.org/package/insert-ordered-containers> |
| lens | BSD-2-Clause | <https://hackage.haskell.org/package/lens> |
| megaparsec | BSD-2-Clause | <https://hackage.haskell.org/package/megaparsec> |
| mtl | BSD-3-Clause | bundled with GHC |
| network-uri | BSD-3-Clause | <https://hackage.haskell.org/package/network-uri> |
| openapi3 | BSD-3-Clause | <https://hackage.haskell.org/package/openapi3> |
| optparse-applicative | BSD-3-Clause | <https://hackage.haskell.org/package/optparse-applicative> |
| process | BSD-3-Clause | bundled with GHC |
| random | BSD-3-Clause | bundled with GHC |
| scientific | BSD-3-Clause | <https://hackage.haskell.org/package/scientific> |
| servant | BSD-3-Clause | <https://hackage.haskell.org/package/servant> |
| servant-openapi3 | BSD-3-Clause | <https://hackage.haskell.org/package/servant-openapi3> |
| servant-server | BSD-3-Clause | <https://hackage.haskell.org/package/servant-server> |
| stm | BSD-3-Clause | bundled with GHC |
| store | MIT | <https://hackage.haskell.org/package/store> |
| text | BSD-2-Clause | bundled with GHC |
| time | BSD-3-Clause | bundled with GHC |
| toml-reader | BSD-3-Clause | <https://hackage.haskell.org/package/toml-reader> |
| transformers | BSD-3-Clause | bundled with GHC |
| unicode-transforms | BSD-3-Clause | <https://hackage.haskell.org/package/unicode-transforms> |
| uuid | BSD-3-Clause | <https://hackage.haskell.org/package/uuid> |
| vector | BSD-3-Clause | <https://hackage.haskell.org/package/vector> |
| wai | MIT | <https://hackage.haskell.org/package/wai> |
| wai-app-static | MIT | <https://hackage.haskell.org/package/wai-app-static> |
| wai-extra | MIT | <https://hackage.haskell.org/package/wai-extra> |
| warp | MIT | <https://hackage.haskell.org/package/warp> |
| xeno | BSD-3-Clause | <https://hackage.haskell.org/package/xeno> |
| zip-archive | BSD-3-Clause | <https://hackage.haskell.org/package/zip-archive> |
| zstd | BSD-3-Clause | <https://hackage.haskell.org/package/zstd> |

## Regenerating this list

After `cabal build all`, the canonical license for every direct dep is in the
matching `~/.cabal/store/ghc-<ver>/package.db/<name>-<version>-<hash>.conf`
file (`grep ^license:`). The list above was last reconciled by reading those
conf files for every entry of the `depends` sets of `volca:lib:volca` and
`volca:exe:volca` in `dist-newstyle/cache/plan.json`. Those two components are
the scope of the table, so reading the library alone would skip the packages
only the executable links, `warp`, `wai-app-static` and `wai-extra`. Re-run
that reconciliation at release time and update any drifted entries.

`scripts/check-licenses-table.sh` only compares the names, which is what
changes when a dependency comes or goes. A package that relicenses keeps its
name, so the reconciliation above is still the only thing that catches it.

The crates table needs no such reconciliation: the script compares its
licenses too, and prints the rows to add, remove or correct.

## AMD license

AMD, Copyright (c), 1996-2022, Timothy A. Davis, Patrick R. Amestoy, and
Iain S. Duff. All Rights Reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the organizations to which the authors are
      affiliated, nor the names of its contributors may be used to endorse
      or promote products derived from this software without specific prior
      written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
DAMAGE.
