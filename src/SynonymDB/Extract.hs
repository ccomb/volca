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

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.UUID (UUID)

import Method.FlowResolver (ILCDFlowInfo (..))
import Types (Flow (..), FlowDB)

{- | Extract synonym pairs from EcoSpold2 biosphere flows.
For each flow with English synonyms, generates (flowName, synonym) pairs.
-}
extractFromEcoSpold2 :: FlowDB -> S.Set UUID -> [(Text, Text)]
extractFromEcoSpold2 flowDB bioUUIDs =
    S.toList $
        S.fromList
            [ (flowName f, syn)
            | f <- M.elems flowDB
            , S.member (flowId f) bioUUIDs
            , syns <- maybe [] S.toList (M.lookup "en" (flowSynonyms f))
            , let syn = T.strip syns
            , not (T.null syn)
            , syn /= flowName f
            ]

{- | Extract synonym pairs from ILCD flow definitions.
Two sources:
1. Direct: baseName ↔ each synonym from <common:synonyms>
2. CAS grouping: flows sharing a CAS are chained as synonyms
-}
extractFromILCDFlows :: M.Map UUID ILCDFlowInfo -> [(Text, Text)]
extractFromILCDFlows flowInfo =
    S.toList (S.union directSet casSet)
  where
    infos = M.elems flowInfo

    -- 1. Direct synonyms from <common:synonyms>
    directSet =
        S.fromList
            [ (ilcdBaseName info, syn)
            | info <- infos
            , syn <- ilcdSynonyms info
            , syn /= ilcdBaseName info
            ]

    -- 2. CAS grouping: chain distinct baseNames sharing the same CAS
    -- Use Set to collect unique names per CAS, then chain (not all-pairs)
    casSet =
        S.fromList
            [ (n1, n2)
            | names <- M.elems casByName
            , let unique = S.toAscList names
            , (n1, n2) <- zip unique (drop 1 unique)
            ]
    casByName =
        M.fromListWith
            S.union
            [ (cas, S.singleton (ilcdBaseName info))
            | info <- infos
            , Just cas <- [ilcdCAS info]
            , not (T.null cas)
            ]

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
