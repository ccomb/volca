{-# LANGUAGE BangPatterns #-}

-- | Pure BM25 ranker for activity search.
--
-- The index is built once at database load time and queried per-request.
-- Documents are identified by ProcessId-equivalent Ints (0..N-1), matching
-- the activity vector layout the rest of the engine uses.
module Search.BM25
    ( BM25Index
    , buildIndex
    , addBM25Index
    , score
    ) where

import Data.Foldable (foldl')
import Data.Int (Int32)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Control.Monad.ST (runST)
import Control.Monad (forM_)

import Search.Normalize (tokenize)
import Search.BM25.Types (BM25Index(..))
import Types (Activity, Database(..), Flow, activityName, exchanges, exchangeIsReference, exchangeIsInput, exchangeFlowId, flowName)
import Data.UUID (UUID)

-- | Populate the BM25 index field on a Database. Called after
-- initializeRuntimeFields during database load. Idempotent.
addBM25Index :: Database -> Database
addBM25Index db = db { dbBM25Index = Just (buildIndex (dbActivities db) (dbFlows db)) }

-- BM25 hyperparameters. Defaults from the canonical Okapi BM25 paper.
k1 :: Double
k1 = 1.2

b :: Double
b = 0.75

-- | Build the BM25 index for a vector of activities.
-- Document text per activity = activity name + reference product name.
-- Location is intentionally excluded: it's a structured filter (geoParam),
-- not a ranking signal.
buildIndex :: V.Vector Activity -> Map UUID Flow -> BM25Index
buildIndex activities flowDb =
    let n = V.length activities
        tokensByDoc :: V.Vector [Text]
        tokensByDoc = V.map (documentTokens flowDb) activities

        docLengths :: VU.Vector Int
        docLengths = VU.generate n (\i -> length (tokensByDoc V.! i))

        totalLen = VU.sum docLengths
        avgDL = if n == 0 then 0 else fromIntegral totalLen / fromIntegral n

        -- Accumulate (docId, tf) entries per term, keeping the list sorted by docId
        -- by construction (we walk docs in order).
        postingsMap :: Map Text [(Int, Int)]
        postingsMap = V.ifoldl' addDoc M.empty tokensByDoc
          where
            addDoc !acc i toks =
                let tfs = termFreqs toks
                in M.foldlWithKey'
                    (\m t f -> M.insertWith prepend t [(i, f)] m)
                    acc
                    tfs
            -- new entry comes first; final toVector reverses to ascending docId order
            prepend new old = new ++ old

        toVector = VU.fromList . reverse
        postings = M.map toVector postingsMap

        -- Vocabulary: ordered vector of distinct tokens. Token IDs = positions.
        vocabulary :: V.Vector Text
        vocabulary = V.fromList (M.keys postings)

        -- Trigram inverted index: 3-char substring → sorted vector of token IDs.
        trigramPairs :: [(Text, Int32)]
        trigramPairs =
            [ (tg, tid)
            | (tid, tok) <- zip [0 ..] (V.toList vocabulary)
            , tg <- tokenTrigrams tok
            ]

        trigramMap :: Map Text [Int32]
        trigramMap = foldl' (\m (tg, tid) -> M.insertWith (++) tg [tid] m) M.empty trigramPairs

        trigrams :: Map Text (VU.Vector Int32)
        trigrams = M.map (VU.fromList . reverse) trigramMap
    in BM25Index
        { bm25Postings   = postings
        , bm25DocLengths = docLengths
        , bm25AvgDL      = avgDL
        , bm25DocCount   = n
        , bm25Vocabulary = vocabulary
        , bm25Trigrams   = trigrams
        }

-- | Extract the set of 3-char substrings of a token. Empty for tokens < 3 chars.
tokenTrigrams :: Text -> [Text]
tokenTrigrams t
    | T.length t < 3 = []
    | otherwise =
        let go s
              | T.length s < 3 = []
              | otherwise      = T.take 3 s : go (T.drop 1 s)
            raw = go t
        in M.keys (M.fromList [(tg, ()) | tg <- raw])  -- dedupe

-- | Extract searchable tokens for one activity: name + reference product name(s).
documentTokens :: Map UUID Flow -> Activity -> [Text]
documentTokens flowDb a =
    tokenize (activityName a)
    ++ concatMap productTokens (exchanges a)
  where
    productTokens ex
        | exchangeIsReference ex
        , not (exchangeIsInput ex)
        , Just flow <- M.lookup (exchangeFlowId ex) flowDb
        = tokenize (flowName flow)
        | otherwise = []

-- | Count term frequencies in a document.
termFreqs :: [Text] -> Map Text Int
termFreqs = foldl' (\m t -> M.insertWith (+) t 1 m) M.empty

-- | Score all documents against a weighted list of query terms. Each term's
-- IDF contribution is multiplied by the supplied weight. Terms appearing more
-- than once keep the maximum weight (exact > fuzzy).
-- Returns a dense vector of scores indexed by docId. Documents not matching
-- any query term score 0.
score :: BM25Index -> [(Text, Double)] -> VU.Vector Double
score idx weightedTerms = runST $ do
    acc <- VUM.replicate (bm25DocCount idx) 0.0
    let termWeights = M.fromListWith max weightedTerms
        nDocs = fromIntegral (bm25DocCount idx) :: Double
        avgdl = bm25AvgDL idx
        dlFor i = fromIntegral (bm25DocLengths idx VU.! i) :: Double
    forM_ (M.toList termWeights) $ \(t, w) ->
        case M.lookup t (bm25Postings idx) of
            Nothing -> pure ()
            Just postings -> do
                let df = fromIntegral (VU.length postings) :: Double
                    idf = w * log ((nDocs - df + 0.5) / (df + 0.5) + 1)
                VU.forM_ postings $ \(docId, tf) -> do
                    let tfd = fromIntegral tf :: Double
                        dl = dlFor docId
                        denom = tfd + k1 * (1 - b + b * dl / avgdl)
                        contrib = idf * tfd * (k1 + 1) / denom
                    VUM.modify acc (+ contrib) docId
    VU.freeze acc
