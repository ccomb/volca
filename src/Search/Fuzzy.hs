-- | Typo- and stem-tolerant query expansion for BM25.
--
-- Pipeline per query token:
--   1. If the token is already in the BM25 vocabulary, pass through at weight 1.0.
--   2. Otherwise try edit-distance candidates (weight 0.5) — handles typos.
--   3. If edit distance finds nothing, try prefix-coverage (weight 0.7) —
--      handles stems like "electr" → "electricity".
--   4. Still nothing: the token is dropped.
--
-- The trigram index on BM25Index acts as a cheap prefilter so we only compute
-- edit distance against plausible candidates, not the whole vocabulary.
module Search.Fuzzy
    ( expandTokens
    , expandTokensGrouped
    ) where

import Data.List (sortBy)
import qualified Data.Map.Strict as M
import Data.Ord (Down(..), comparing)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Control.Monad.ST (runST)
import Control.Monad (forM_)

import Search.BM25.Types (BM25Index(..))

-- Tunable constants (not wired to config — see plan thresholds section).
fuzzyWeight, prefixWeight :: Double
fuzzyWeight  = 0.5
prefixWeight = 0.7

minSharedTrigrams, topN, minPrefixLen :: Int
minSharedTrigrams = 2
topN              = 3
minPrefixLen      = 4

minJaccard, minCoverage :: Double
minJaccard  = 0.3
minCoverage = 0.9

-- | Maximum Levenshtein distance tolerated for a query token of this length.
maxEditK :: Int -> Int
maxEditK n = if n < 6 then 1 else 2

-- | Upper bound on candidate length for prefix matching, relative to query length.
maxPrefixRatio :: Int
maxPrefixRatio = 3

-- | Turn normalized query tokens into weighted BM25 query terms. Tokens already
-- in vocabulary pass through at weight 1.0; typos resolve to edit-distance
-- matches at 0.5; stems resolve to prefix-coverage matches at 0.7; otherwise
-- dropped.
expandTokens :: BM25Index -> [Text] -> [(Text, Double)]
expandTokens idx = concat . expandTokensGrouped idx

-- | Like 'expandTokens' but preserves the mapping back to original query
-- tokens: one list per input token, possibly empty. Used when the caller
-- needs AND semantics across query tokens (each group = one AND conjunct).
expandTokensGrouped :: BM25Index -> [Text] -> [[(Text, Double)]]
expandTokensGrouped idx = map expand
  where
    expand t
      | t `M.member` bm25Postings idx = [(t, 1.0)]
      | otherwise =
          case editCandidates idx t of
              edits@(_:_) -> [(c, fuzzyWeight)  | c <- take topN edits]
              []          -> [(c, prefixWeight) | c <- take topN (prefixCandidates idx t)]

-- | Candidate tokens within the permitted edit distance of the query.
-- Uses the trigram index as a prefilter: only tokens sharing at least
-- `minSharedTrigrams` trigrams with the query are considered.
-- Results sorted by (jaccard desc, editDistance asc).
editCandidates :: BM25Index -> Text -> [Text]
editCandidates idx q
    | S.null qTrigs = []
    | otherwise     = map (\(c, _, _) -> c) (sortBy (comparing sortKey) scored)
  where
    qTrigs  = trigramsOf q
    k       = maxEditK (T.length q)
    sortKey (_, ja, d) = (Down ja, d)
    scored  =
        [ (c, ja, d)
        | c <- candidateBucket idx qTrigs
        , let ja = jaccard qTrigs (trigramsOf c)
        , ja >= minJaccard
        , let d = editDistance q c
        , d <= k
        ]

-- | Candidate tokens that have the query's trigrams as a near-subset
-- (prefix/stem matches). Length guard prevents short queries from matching
-- arbitrarily long tokens.
-- Results sorted by (coverage desc, candidate length asc).
prefixCandidates :: BM25Index -> Text -> [Text]
prefixCandidates idx q
    | T.length q < minPrefixLen         = []
    | S.size qTrigs < minSharedTrigrams = []
    | otherwise                         = map fst (sortBy (comparing sortKey) scored)
  where
    qTrigs  = trigramsOf q
    qLen    = T.length q
    sortKey (c, cov) = (Down cov, T.length c)
    scored  =
        [ (c, cov)
        | c <- candidateBucket idx qTrigs
        , T.length c <= maxPrefixRatio * qLen
        , let cov = coverage qTrigs (trigramsOf c)
        , cov >= minCoverage
        ]

-- | Vocabulary tokens that share at least `minSharedTrigrams` trigrams with
-- the query's trigrams. Aggregation uses an unboxed counter vector for speed.
candidateBucket :: BM25Index -> S.Set Text -> [Text]
candidateBucket idx qTrigs =
    let vocab  = bm25Vocabulary idx
        vocabN = V.length vocab
        counts = runST $ do
            acc <- VUM.replicate vocabN (0 :: Int)
            forM_ (S.toList qTrigs) $ \tg ->
                case M.lookup tg (bm25Trigrams idx) of
                    Nothing -> pure ()
                    Just tids ->
                        VU.forM_ tids $ \tid ->
                            VUM.modify acc (+ 1) (fromIntegral tid)
            VU.freeze acc
    in [ vocab V.! i
       | i <- [0 .. vocabN - 1]
       , counts VU.! i >= minSharedTrigrams
       ]

-- | Set of 3-char substrings of a token. Empty for tokens < 3 chars.
trigramsOf :: Text -> S.Set Text
trigramsOf t
    | T.length t < 3 = S.empty
    | otherwise      = S.fromList (go t)
  where
    go s
      | T.length s < 3 = []
      | otherwise      = T.take 3 s : go (T.drop 1 s)

jaccard :: S.Set Text -> S.Set Text -> Double
jaccard a b
    | S.null a && S.null b = 0
    | otherwise =
        let i = S.size (S.intersection a b)
            u = S.size (S.union a b)
        in fromIntegral i / fromIntegral u

-- | One-way trigram coverage: |Q ∩ C| / |Q|. Returns 0 if Q is empty.
coverage :: S.Set Text -> S.Set Text -> Double
coverage q c
    | S.null q  = 0
    | otherwise = fromIntegral (S.size (S.intersection q c)) / fromIntegral (S.size q)

-- | Levenshtein edit distance. Standard DP over an unboxed row buffer;
-- O(|a| * |b|) time, O(min |a| |b|) space.
editDistance :: Text -> Text -> Int
editDistance a b
    | T.length a < T.length b = editDistance b a
    | T.null b = T.length a
    | otherwise = runST $ do
        let m = T.length a
            n = T.length b
            aChars = T.unpack a
            bChars = T.unpack b
        prev <- VU.thaw (VU.generate (n + 1) id)
        curr <- VUM.replicate (n + 1) 0
        forM_ (zip [1 .. m] aChars) $ \(i, ca) -> do
            VUM.write curr 0 i
            forM_ (zip [1 .. n] bChars) $ \(j, cb) -> do
                up   <- VUM.read prev j
                left <- VUM.read curr (j - 1)
                diag <- VUM.read prev (j - 1)
                let subCost = if ca == cb then 0 else 1
                    dist = minimum [up + 1, left + 1, diag + subCost]
                VUM.write curr j dist
            -- swap rows by copying curr → prev
            forM_ [0 .. n] $ \j -> do
                v <- VUM.read curr j
                VUM.write prev j v
        VUM.read prev n

