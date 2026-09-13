{-# LANGUAGE OverloadedStrings #-}

{- | Extract synonym pairs from loaded databases and methods.

Produces CSV-compatible pairs that can be written to disk and loaded
by the existing SynonymDB pipeline.
-}
module SynonymDB.Extract (
    extractFromEcoSpold2,
    extractFromILCDFlows,
    synonymPairsToCSV,
) where

import Control.Monad (mfilter)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.UUID (UUID)

import Method.FlowResolver (ILCDFlowInfo (..))
import SynonymDB (normalizeName)
import Types (BioFlowDB, BiosphereFlow (..))

{- | Extract synonym pairs from EcoSpold2 biosphere flows.
For each flow with English synonyms, generates (flowName, synonym) pairs.
-}
extractFromEcoSpold2 :: BioFlowDB -> [(Text, Text)]
extractFromEcoSpold2 bioFlowDB =
    S.toList $
        S.fromList
            [ (bfName f, syn)
            | f <- M.elems bioFlowDB
            , syns <- maybe [] S.toList (M.lookup "en" (bfSynonyms f))
            , let syn = T.strip syns
            , not (T.null syn)
            , syn /= bfName f
            ]

{- | Extract synonym pairs from ILCD flow definitions: the direct
@<common:synonyms>@ of each flow (baseName ↔ each synonym).

CAS-based equivalence is deliberately NOT emitted as synonym pairs. Flows
sharing a CAS are already matched by the CAS cascade (@mtCasCF@) at lookup time,
so chaining same-CAS names here adds no new match - it only injects transitive
bridges that fuse unrelated substances into oversized closure classes (one
shared or blank CAS can connect hundreds of names through a single chain).
-}
extractFromILCDFlows :: M.Map UUID ILCDFlowInfo -> [(Text, Text)]
extractFromILCDFlows flowInfo =
    S.toList $
        S.fromList
            [ (base, syn)
            | info <- M.elems flowInfo
            , let base = ilcdBaseName info
            , syn <- ilcdSynonyms info
            , syn /= base
            , not (ambiguous base || ambiguous syn)
            ]
  where
    -- Test a name with the SAME normalization the closure keys by
    -- ('SynonymDB.buildFromPairs' → 'normalizeName'): the name the closure would
    -- fuse into one node is exactly the name whose ambiguity we check. A weaker
    -- local normalizer (lower+strip only) leaves punctuation/whitespace/unit
    -- variants under distinct keys, so a junk hub slips past the filter.
    ambiguous nm = normalizeName nm `S.member` casAmbiguous
    -- A name is an ambiguous bridge when the flows carrying it span more than one
    -- substance identity. Identity is the flow's CAS, or 'Nothing' when it has
    -- none: an un-annotated flow cannot be proven to be the same substance, so a
    -- name shared by a CAS-less flow and a CAS-bearing one still counts as a
    -- bridge. Dropping it stops the transitive closure from fusing unrelated
    -- substances into one junk hub (which then leaks a characterization factor
    -- across them); same-CAS flows lose nothing, as the CAS cascade ('mtCasCF')
    -- already matches them at lookup time. Both endpoints of every pair are
    -- checked, so an ambiguous baseName is dropped too, not just a synonym.
    identityOfName :: M.Map Text (S.Set (Maybe Text))
    identityOfName =
        M.fromListWith
            S.union
            [ (normalizeName nm, S.singleton (mfilter (not . T.null) (ilcdCAS info)))
            | info <- M.elems flowInfo
            , nm <- ilcdBaseName info : ilcdSynonyms info
            ]
    casAmbiguous = M.keysSet (M.filter ((> 1) . S.size) identityOfName)

-- | Render synonym pairs as CSV with header.
synonymPairsToCSV :: [(Text, Text)] -> BL.ByteString
synonymPairsToCSV pairs =
    BLC.unlines ("name1,name2" : map renderPair pairs)
  where
    renderPair (a, b) = BLC.fromStrict (T.encodeUtf8 (csvField a <> "," <> csvField b))
    csvField t
        | T.any (\c -> c == ',' || c == '"' || c == '\n') t =
            "\"" <> T.replace "\"" "\"\"" t <> "\""
        | otherwise = t
