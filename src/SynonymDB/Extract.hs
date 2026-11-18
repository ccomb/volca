{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Extract synonym pairs from loaded databases and methods.

Produces CSV-compatible pairs that can be written to disk and loaded
by the existing SynonymDB pipeline.
-}
module SynonymDB.Extract (
    extractFromEcoSpold2,
    extractFromILCDFlows,
    synonymPairsToCSV,
    SynonymCandidate (..),
    synonymCandidate,
) where

import Control.Monad (mfilter)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Store (Store)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.UUID (UUID)
import GHC.Generics (Generic)

import Method.FlowResolver (ILCDFlowInfo (..))
import SynonymDB (excludeJunkSynonyms, excludeOverFrequentSynonyms, normalizeName, oversizedClasses)
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
so chaining same-CAS names here adds no new match – it only injects transitive
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

{- | The synonym pairs drawn from one source, as the opt-in set they are offered
as: the file to write, and what the filters set aside or found suspect, for
the log. Held whole so that a start which finds it already made does none of
the work, which takes seconds on a large method.
-}
data SynonymCandidate = SynonymCandidate
    { scCsv :: !BS.ByteString
    -- ^ the pairs kept, as the file's content
    , scPairCount :: !Int
    , scJunkTokens :: ![Text]
    -- ^ placeholder and non-substance tokens, dropped
    , scOverFrequent :: ![(Text, Int)]
    -- ^ class labels and stop-words, dropped, with the number of flows carrying each
    , scFusedClasses :: ![(Int, [Text])]
    {- ^ the size and first names of each class the closure of the kept pairs
    fuses beyond a hundred names. The set is closed only for this audit: such
    a class means an ambiguous bridge fused unrelated substances, which the
    curator must see before ever activating the source.
    -}
    }
    deriving (Eq, Show, Generic, Store)

-- | Nothing for a source that yields no pair: it gets no candidate at all.
synonymCandidate :: [(Text, Text)] -> Maybe SynonymCandidate
synonymCandidate [] = Nothing
synonymCandidate pairs =
    Just
        SynonymCandidate
            { scCsv = BL.toStrict (synonymPairsToCSV keptPairs)
            , scPairCount = length keptPairs
            , scJunkTokens = junkTokens
            , scOverFrequent = excludedSyns
            , scFusedClasses = [(length cls, take 5 cls) | cls <- oversizedClasses 100 keptPairs]
            }
  where
    (nonJunkPairs, junkTokens) = excludeJunkSynonyms pairs
    (keptPairs, excludedSyns) = excludeOverFrequentSynonyms maxSynonymFlowFrequency nonJunkPairs

{- | A synonym carried by more distinct flows than this is a classification label
or stop-word (e.g. @"organic"@), not a true synonym: 'excludeOverFrequentSynonyms'
drops it. The bound sits in the gap between the class-label hubs (≥187 flows in
EF 3.1) and the first genuine flow name used as a synonym (~17 flows).
-}
maxSynonymFlowFrequency :: Int
maxSynonymFlowFrequency = 25
