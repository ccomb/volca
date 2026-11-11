# Third-party licenses

VoLCA is licensed under the Apache License, Version 2.0 (see [LICENSE](LICENSE)).
This document inventories the third-party software bundled with, or linked into,
a VoLCA distribution, and travels in every downloaded archive. See also
[NOTICE](NOTICE) for the abridged summary.

## Native libraries

The binary also carries C libraries, which differ from one platform to the
next. Their license texts are in the [`licenses/`](licenses) folder, which
travels with the binary.

| Library | Linked into | License | Text |
|---|---|---|---|
| GMP (GNU MP) | Linux, macOS, Windows | LGPL-3.0-or-later | [GMP.txt](licenses/GMP.txt), [LGPL-3.0.txt](licenses/LGPL-3.0.txt), [GPL-3.0.txt](licenses/GPL-3.0.txt) |
| Zstandard | Linux, macOS, Windows | BSD-3-Clause | [zstd.txt](licenses/zstd.txt) |
| zlib | Linux, Windows | Zlib | [zlib.txt](licenses/zlib.txt) |
| musl | Linux | MIT | [musl.txt](licenses/musl.txt) |
| libffi | Linux | MIT | [libffi.txt](licenses/libffi.txt) |
| ncurses | Linux | X11 | [ncurses.txt](licenses/ncurses.txt) |
| MinGW-w64 runtime | Windows | ZPL-2.1 and others | [mingw-w64-runtime.txt](licenses/mingw-w64-runtime.txt) |

GMP is offered under either the LGPL or the GPL; VoLCA takes it under the
LGPL, version 3 or later. Its source code is at <https://gmplib.org/>, and
VoLCA's own source code and build script are public, so the binary can be
rebuilt against a modified GMP.

On macOS, zlib, libffi and ncurses are the system's own libraries, loaded
from the system and not distributed with the binary. The compiler's runtime
code linked on Linux, libgcc, is under the GCC Runtime Library Exception,
which sets no condition on the binary.

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
| primitive | BSD-3-Clause | <https://hackage.haskell.org/package/primitive> |
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
