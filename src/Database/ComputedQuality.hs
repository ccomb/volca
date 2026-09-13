{-# LANGUAGE OverloadedStrings #-}

{- | Computed dataset checks, for the people who build or repair databases.

Where 'Database.Quality' scans what a database stores, these checks judge
what it computes: category scores far off the database norm (a mg-read-as-kg
slip surfaces as a ×1000 outlier), entries whose every score is zero, and
negative category scores. They need a loaded database and a method
collection, so they live in their own report rather than compromising the
structural report's staged/loaded identity.

The scoring itself happens at the effectful edge; this module only judges
the numbers it is handed.
-}
module Database.ComputedQuality (
    CategoryScore (..),
    ScoredEntry (..),
    ComputedQualityReport (..),
    computedQualityReport,
    outlierSigma,
    minGroupSize,
) where

import Data.List (sort, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (showFFloat, showGFloat)

import Database.Quality (QualityCheck (..), QualityOffender (..))
import Types (Severity (..))

-- | One impact-category score of one entry, with the unit it is expressed in.
data CategoryScore = CategoryScore
    { csCategory :: !Text
    , csUnit :: !Text
    , csScore :: !Double
    }
    deriving (Show, Eq)

-- | One scored (activity, product) entry, as the effectful edge hands it over.
data ScoredEntry = ScoredEntry
    { seProcessId :: !Text
    , seActivityName :: !Text
    , seLocation :: !Text
    , seProductName :: !(Maybe Text)
    , seRefUnit :: !Text
    {- ^ Unit of the reference product. Scores are per reference unit, so only
    entries sharing it are comparable - a kg of wheat and a kWh of power
    legitimately live orders of magnitude apart.
    -}
    , seScores :: ![CategoryScore]
    }
    deriving (Show, Eq)

{- | Computed counterpart of 'Database.Quality.QualityReport': one named field
per check, same offender shape, so consumers treat both reports alike.
-}
data ComputedQualityReport = ComputedQualityReport
    { cqDbName :: !Text
    , cqCollection :: !Text
    -- ^ The method collection the scores were computed against
    , cqProcessCount :: !Int
    , cqScoreOutliers :: !QualityCheck
    , cqZeroScores :: !QualityCheck
    , cqNegativeScores :: !QualityCheck
    }
    deriving (Show, Eq)

{- | How far (in robust sigmas) a log-score may sit from its group's median
before it is flagged. 3.5 is the classic conservative cut; a mg-as-kg slip
lands three orders of magnitude out and clears it by a wide margin.
-}
outlierSigma :: Double
outlierSigma = 3.5

{- | Groups smaller than this carry too little evidence for a robust norm, so
they produce no outlier findings at all.
-}
minGroupSize :: Int
minGroupSize = 20

-- | Run every computed check over the scored catalogue.
computedQualityReport :: Text -> Text -> [ScoredEntry] -> ComputedQualityReport
computedQualityReport dbName collection entries =
    ComputedQualityReport
        { cqDbName = dbName
        , cqCollection = collection
        , cqProcessCount = length entries
        , -- Every outlier shares one severity, so the generic worst-first order
          -- would fall back to names; the maintainer wants the wildest
          -- deviation on top - that is where the unit slip sits.
          cqScoreOutliers = QualityCheck True (map fst (sortOn (Down . snd) outlierOffenders))
        , cqZeroScores = QualityCheck True (worstFirst zeroOffenders)
        , cqNegativeScores = QualityCheck True (worstFirst negativeOffenders)
        }
  where
    worstFirst = sortOn (\o -> (qoSeverity o, qoActivityName o))
    offender sev e = QualityOffender sev (seProcessId e) (seActivityName e) (seLocation e) (seProductName e)

    -- Scores are compared on a log scale within one (category, reference
    -- unit) group: multiplicative unit slips are exactly what we hunt, and
    -- honest scores span orders of magnitude anyway. Zero and negative
    -- scores have no logarithm and their own checks below.
    groups =
        M.fromListWith
            (<>)
            [ ((csCategory s, seRefUnit e), [logBase 10 (csScore s)])
            | e <- entries
            , s <- seScores e
            , csScore s > 0
            ]
    norms = M.mapMaybe groupNorm groups
    groupNorm ls
        | length ls < minGroupSize = Nothing
        | otherwise = do
            m <- median ls
            mad <- median (map (abs . subtract m) ls)
            -- A zero MAD means half the group is identical - a degenerate
            -- norm that would flag any honest variation, so judge nothing.
            if mad > 0 then Just (m, 1.4826 * mad) else Nothing

    -- Each element carries the entry's worst absolute deviation, the sort key
    -- above.
    outlierOffenders = mapMaybe entryOutlier entries
    entryOutlier e = case sortOn (Down . abs . snd) (mapMaybe deviation (seScores e)) of
        [] -> Nothing
        (s, d) : rest ->
            let detail =
                    T.pack (show (1 + length rest))
                        <> " category score(s) far off the database norm for "
                        <> seRefUnit e
                        <> "-referenced entries (worst: "
                        <> csCategory s
                        <> " at "
                        <> fmtScore (csScore s)
                        <> " "
                        <> csUnit s
                        <> ", "
                        <> fmtSigma (abs d)
                        <> " robust sigmas "
                        <> (if d > 0 then "above" else "below")
                        <> " the median)"
             in Just (offender WarningSev e detail, abs d)
      where
        deviation s = do
            (m, sigma) <- M.lookup (csCategory s, seRefUnit e) norms
            if csScore s > 0
                then
                    let d = (logBase 10 (csScore s) - m) / sigma
                     in if abs d > outlierSigma then Just (s, d) else Nothing
                else Nothing

    -- Every category at exactly zero: the inventory solved to nothing, or
    -- nothing in it is characterized. Either way the entry contributes
    -- nothing to any assessment, which its maker should know.
    zeroOffenders =
        [ offender WarningSev e "every category score is zero - the inventory is empty or nothing in it is characterized"
        | e <- entries
        , not (null (seScores e))
        , all ((== 0) . csScore) (seScores e)
        ]

    -- Negative scores are legitimate where avoided-production credits or
    -- waste treatment dominate - hence Info, a place to look, not a verdict.
    negativeOffenders =
        [ offender InfoSev e $
            T.pack (show (length negs))
                <> " category score(s) below zero (worst: "
                <> csCategory s
                <> " at "
                <> fmtScore (csScore s)
                <> " "
                <> csUnit s
                <> ") - expected for avoided-production credits and waste treatment"
        | e <- entries
        , let negs = filter ((< 0) . csScore) (seScores e)
        , s : _ <- [sortOn csScore negs]
        ]

-- | Total median: 'Nothing' on an empty list rather than a crash.
median :: [Double] -> Maybe Double
median xs = case (drop ((n - 1) `div` 2) sorted, drop (n `div` 2) sorted) of
    (lo : _, hi : _) -> Just ((lo + hi) / 2)
    (_, _) -> Nothing
  where
    sorted = sort xs
    n = length xs

fmtScore :: Double -> Text
fmtScore x = T.pack (showGFloat (Just 3) x "")

fmtSigma :: Double -> Text
fmtSigma x = T.pack (showFFloat (Just 1) x "")
