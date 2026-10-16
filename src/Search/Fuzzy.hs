{- | Typo- and stem-tolerant query expansion for BM25.

Pipeline per query token:
  1. If the token is already in the BM25 vocabulary, include it at weight 1.0.
  2. Edit-distance candidates (weight 0.5, then 0.45 and 0.4 for the next
     closest) – typos and orthographic variants
     (e.g. trellis ↔ treillis), two swapped letters counting as one typo.
  3. Prefix-coverage candidates (weight 0.7) – stems like "elec" →
     "electricity". Run alongside edit-distance, not as a fallback: a short
     query like "elec" can have unrelated edit-distance hits ("alec",
     "elect") that would otherwise mask its real intent (the stem).
  4. Edit and prefix lists are merged; if a token appears in both, the
     higher weight wins (prefix > edit).
  5. If nothing matches at any stage, the token is dropped.

The trigram index on BM25Index acts as a cheap prefilter for prefix matches.
Edit-distance candidates are not drawn from it: one changed letter breaks up
to three trigrams, so a five-letter typo ("wheet") shares one with the word
meant ("wheat") and a swap ("sotf") none.
-}
module Search.Fuzzy (
    expandTokens,
    expandTokensGrouped,
) where

import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Ord (Down (..))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import Search.BM25.Types (BM25Index (..))

-- Tunable constants (not wired to config – see plan thresholds section).
fuzzyWeight, fuzzyStep, prefixWeight :: Double
fuzzyWeight = 0.5
fuzzyStep = 0.05
prefixWeight = 0.7

minSharedTrigrams, topN, prefixTopN, minPrefixLen, minEditLen, minVariantLen :: Int
minSharedTrigrams = 2
topN = 3
-- Stem expansions are inherently a long tail (a 4-5 char prefix typically
-- has 5–30 covering tokens in a real LCA corpus). Keep more than the
-- edit-distance topN so longer derived forms ("electricity") survive when
-- many shorter siblings ("elect", "electa", "electro") share the prefix.
prefixTopN = 10
minPrefixLen = 4
-- Below four letters a word has too many neighbours one letter away ("tea",
-- "pea", "sea") for any of them to be the one meant.
minEditLen = 4
-- A word the vocabulary holds and shorter than this is taken as typed: "milk"
-- is milk, not silk. Longer ones also reach their spelling variants one
-- letter away (trellis, treillis), but not two: "potato" is not potash.
minVariantLen = 6

minCoverage :: Double
minCoverage = 0.9

-- | Maximum edit distance tolerated for a query token of this length.
maxEditK :: Int -> Int
maxEditK n = if n < 6 then 1 else 2

-- | Upper bound on candidate length for prefix matching, relative to query length.
maxPrefixRatio :: Int
maxPrefixRatio = 3

{- | Turn normalized query tokens into weighted BM25 query terms. Tokens already
in vocabulary pass through at weight 1.0; typos resolve to edit-distance
matches at 0.5 down to 0.4; stems resolve to prefix-coverage matches at 0.7; otherwise
dropped.
-}
expandTokens :: BM25Index -> [Text] -> [(Text, Double)]
expandTokens idx = concat . expandTokensGrouped idx

{- | Like 'expandTokens' but preserves the mapping back to original query
tokens: one list per input token, possibly empty. Used when the caller
needs to know which word typed a term stands for: an AND across words
(each group one conjunct), or a ranking by how much of each word a name
holds.
-}
expandTokensGrouped :: BM25Index -> [Text] -> [[(Text, Double)]]
expandTokensGrouped idx = map expand
  where
    expand t =
        let inVocab = t `M.member` bm25Postings idx
            exact = [(t, 1.0) | inVocab]
            len = T.length t
            edits
                | len < minEditLen || (inVocab && len < minVariantLen) = []
                -- Weights fall in the candidates' order, so the names holding the
                -- closest, most used one come first: "braed" lists bread before
                -- brandy, which equal weights ranked first for its rarer word.
                | otherwise = zip (take topN (filter (/= t) (editCandidates idx k t))) [fuzzyWeight, fuzzyWeight - fuzzyStep ..]
            k = if inVocab then 1 else maxEditK len
            prefixes = [(c, prefixWeight) | c <- take prefixTopN (prefixCandidates idx t), c /= t]
            -- A candidate can satisfy both edit-distance and prefix-coverage
            -- (e.g. "electricit" → "electricity"). Keep the higher weight.
            variants = M.toList (M.fromListWith max (edits ++ prefixes))
         in exact ++ variants

{- | Candidate tokens at most @k@ edits from the query, the
closest first and, among equally close ones, the most widely used: of
"sheet", "wheel" and "wheat" for "wheet", a food database means the one
its names hold most. Every vocabulary token whose length allows it is
compared, since trigrams miss short typos (see the module header).
-}
editCandidates :: BM25Index -> Int -> Text -> [Text]
editCandidates idx k q = map fst (sortOn sortKey scored)
  where
    n = T.length q
    sortKey (c, d) = (d, Down (documentFrequency idx c))
    scored =
        [ (c, d)
        | c <- V.toList (bm25Vocabulary idx)
        , abs (T.length c - n) <= k
        , let d = editDistance q c
        , d <= k
        ]

-- | How many documents hold a vocabulary token.
documentFrequency :: BM25Index -> Text -> Int
documentFrequency idx t = maybe 0 VU.length (M.lookup t (bm25Postings idx))

{- | Candidate tokens that have the query's trigrams as a near-subset
(prefix/stem matches). Length guard prevents short queries from matching
arbitrarily long tokens.
Results sorted by (coverage desc, candidate length asc).
-}
prefixCandidates :: BM25Index -> Text -> [Text]
prefixCandidates idx q
    | T.length q < minPrefixLen = []
    | S.size qTrigs < minSharedTrigrams = []
    | otherwise = map fst (sortOn sortKey scored)
  where
    qTrigs = trigramsOf q
    qLen = T.length q
    sortKey (c, cov) = (Down cov, T.length c)
    scored =
        [ (c, cov)
        | c <- candidateBucket idx qTrigs
        , T.length c <= maxPrefixRatio * qLen
        , let cov = coverage qTrigs (trigramsOf c)
        , cov >= minCoverage
        ]

{- | Vocabulary tokens that share at least `minSharedTrigrams` trigrams with
the query's trigrams. Aggregation uses an unboxed counter vector for speed.
-}
candidateBucket :: BM25Index -> S.Set Text -> [Text]
candidateBucket idx qTrigs =
    let vocab = bm25Vocabulary idx
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
    | otherwise = S.fromList (go t)
  where
    go s
        | T.length s < 3 = []
        | otherwise = T.take 3 s : go (T.drop 1 s)

-- | One-way trigram coverage: |Q ∩ C| / |Q|. Returns 0 if Q is empty.
coverage :: S.Set Text -> S.Set Text -> Double
coverage q c
    | S.null q = 0
    | otherwise = fromIntegral (S.size (S.intersection q c)) / fromIntegral (S.size q)

{- | Edit distance where a letter inserted, deleted, replaced, or swapped with
its neighbour costs one (optimal string alignment): "sotf" is one typo from
"soft", where plain Levenshtein counts two.
-}
editDistance :: Text -> Text -> Int
editDistance a b = VU.last (snd (foldl' step (VU.empty, VU.enumFromN 0 (n + 1)) (zip [1 ..] (T.unpack a))))
  where
    av = VU.fromList (T.unpack a)
    bv = VU.fromList (T.unpack b)
    n = VU.length bv
    -- One row per letter of @a@; the row two above is what a swap reads.
    step (above2, above) (i, ca) = (above, VU.constructN (n + 1) cell)
      where
        cell done
            | j == 0 = i
            | otherwise = minimum (above VU.! j + 1 : done VU.! (j - 1) + 1 : above VU.! (j - 1) + cost : swapped)
          where
            j = VU.length done
            cb = bv VU.! (j - 1)
            cost = if ca == cb then 0 else 1
            swapped = [above2 VU.! (j - 2) + 1 | i > 1, j > 1, ca == bv VU.! (j - 2), av VU.! (i - 2) == cb]
