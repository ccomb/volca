# Third-party licenses

VoLCA is licensed under the Apache License, Version 2.0 (see [LICENSE](LICENSE)).
This document inventories the third-party software bundled with, or linked into,
a VoLCA distribution. See also [NOTICE](NOTICE) for the abridged summary.

## MUMPS 5.8.1 - CeCILL-C

The MUMPS sequential sparse direct solver is statically linked into the VoLCA
binary via the `mumps-hs` FFI bindings.

- **License**: CeCILL-C (LGPL-compatible French free-software license).
  Full text: <http://www.cecill.info/licences/Licence_CeCILL-C_V1-en.txt>
- **Upstream**: <https://mumps-solver.org/>
- **Copyright**: 1991–2024 CERFACS, CNRS, ENS Lyon, INP Toulouse, Inria,
  Mumps Technologies, University of Bordeaux.
- **Source**: fetchable from the upstream URL above. CeCILL-C §5.3.1 entitles
  you to request the unmodified source at no more than the cost of transfer.

The MUMPS distribution embeds two further components covered by their own
licenses:

### PORD - public domain

Ordering library by Jürgen Schulze, included in `MUMPS_5.8.1/PORD/`. Released
into the public domain.

### AMD variants and `*_TRUNCATED_RRQR` - BSD 3-Clause

Approximate Minimum Degree ordering and rank-revealing QR variants distributed
with MUMPS. BSD 3-Clause license; copyright held by the respective authors as
declared in the source headers.

## BLAS / LAPACK - BSD 3-Clause

Linked at runtime as system shared libraries; not redistributed by VoLCA.

- **License**: BSD 3-Clause. Full text: <https://www.netlib.org/lapack/LICENSE.txt>
- **Upstream**: <https://www.netlib.org/lapack/>

## mumps-hs - Apache-2.0

Haskell FFI bindings to MUMPS, maintained inside this repository
(`mumps-hs/`). Apache-2.0; copyright Christophe Combelles. See
`mumps-hs/LICENSE`.

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
| deepseq | BSD-3-Clause | bundled with GHC |
| directory | BSD-3-Clause | bundled with GHC |
| exceptions | BSD-3-Clause | bundled with GHC |
| filepath | BSD-3-Clause | bundled with GHC |
| haskeline | BSD-3-Clause | <https://hackage.haskell.org/package/haskeline> |
| http-client | MIT | <https://hackage.haskell.org/package/http-client> |
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
