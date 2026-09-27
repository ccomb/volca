{-# LANGUAGE OverloadedStrings #-}

{- | Which activities and which flows made one score of a scoring set.

A score that is a weighted sum of its indicators splits the way they do: the
part of an activity, or of a flow, is the weighted sum of its parts in each
indicator. The weights come from the set's own formulas ('scoreWeights'), and
the parts from the same paths that score each indicator, so the parts of the
score add up to it.

Every surface that breaks a single score down comes through here, so a REST
answer and a tool answer cannot disagree on the number.
-}
module Impact.Score (
    ScoreRef (..),
    ResolvedScore,
    ScoreRefusal (..),
    Source (..),
    refusalMessage,
    resolveScore,
    scoreTitle,
    scoreUnit,
    activityParts,
    flowParts,
) where

import Control.Monad (forM, mfilter, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), except, runExceptT, withExceptT)
import Data.Bifunctor (first)
import Data.List (find)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)

import Database.Manager (CollectionName, DatabaseManager, mapMethodToTablesCached)
import qualified Impact
import Matrix (Inventory)
import Method.Mapping (FlowContribution (..), MethodTables)
import Method.Types (Method (..), ScoringSet (..), scoreWeights)
import qualified SharedSolver
import Types (BiosphereFlow (..), Database, ProcessId)

-- | One score of a scoring set: two names side by side, kept apart so they cannot be swapped.
data ScoreRef = ScoreRef
    { srSet :: Text
    , srScore :: Text
    }

{- | A score the collection holds, with the method behind each indicator it
weighs. Built by 'resolveScore' only, so the parts are never asked of a score
that names nothing.
-}
data ResolvedScore = ResolvedScore
    { rsSet :: ScoringSet
    , rsScore :: Text
    , rsIndicators :: [Method]
    }

-- | Why a single score has no breakdown.
data ScoreRefusal
    = -- | The set, or the score within it, names nothing.
      UnknownScore Text
    | -- | The score names something, but does not split into its indicators' parts.
      Unsplittable Text
    | -- | An indicator could not be scored.
      ScoringFailed Text

-- | What the refusal says, for a surface that has one way to say no.
refusalMessage :: ScoreRefusal -> Text
refusalMessage (UnknownScore msg) = msg
refusalMessage (Unsplittable msg) = msg
refusalMessage (ScoringFailed msg) = msg

{- | Find the score and the method behind each of its indicators. An indicator
the collection does not hold, or holds twice, is refused: scored as zero it
would quietly shrink the score.
-}
resolveScore :: [Method] -> [ScoringSet] -> ScoreRef -> Either ScoreRefusal ResolvedScore
resolveScore methods sets ref = do
    ss <- maybe (Left (UnknownScore noSuchSet)) Right (find ((== srSet ref) . ssName) sets)
    unless (M.member (srScore ref) (ssScores ss)) $ Left (UnknownScore (noSuchScore ss))
    indicators <- first Unsplittable (traverse indicatorMethod (S.toList (S.fromList (M.elems (ssVariables ss)))))
    pure ResolvedScore{rsSet = ss, rsScore = srScore ref, rsIndicators = indicators}
  where
    noSuchSet :: Text
    noSuchSet =
        "No scoring set named '"
            <> srSet ref
            <> "' in this collection. Its scoring sets: "
            <> T.intercalate ", " (map ssName sets)

    noSuchScore :: ScoringSet -> Text
    noSuchScore ss =
        "Scoring set '"
            <> ssName ss
            <> "' has no score named '"
            <> srScore ref
            <> "'. Its scores: "
            <> T.intercalate ", " (M.keys (ssScores ss))

    indicatorMethod :: Text -> Either Text Method
    indicatorMethod name = case filter ((== name) . methodName) methods of
        [m] -> Right m
        [] -> Left ("Scoring set '" <> srSet ref <> "' weighs '" <> name <> "', which this collection has no method for.")
        _ -> Left ("Scoring set '" <> srSet ref <> "' weighs '" <> name <> "', which names several methods of this collection.")

-- | The name the score is shown under.
scoreTitle :: ResolvedScore -> Text
scoreTitle rs = ssName (rsSet rs) <> " " <> rsScore rs

scoreUnit :: ResolvedScore -> Text
scoreUnit = ssUnit . rsSet

-- | Where the indicators of a score are read: the root database and its collection.
data Source = Source
    { srcManager :: DatabaseManager
    , srcDbName :: Text
    , srcDatabase :: Database
    , srcCollection :: CollectionName
    }

{- | Every activity's part of the score. The parts are the terms of the score,
so their sum is it.
-}
activityParts :: Source -> ResolvedScore -> SharedSolver.CrossDBSolution -> IO (Either ScoreRefusal (M.Map (Text, ProcessId) Double))
activityParts src rs sol = runExceptT $ do
    perIndicator <- forM (rsIndicators rs) $ \method -> do
        tables <- liftIO $ tablesOf src method
        parts <- withExceptT ScoringFailed (ExceptT (Impact.processContributionsOf (srcManager src) (srcCollection src) method tables sol))
        pure (methodName method, parts)
    weights <- except (weightsOf rs (M.fromList [(name, sum (M.elems parts)) | (name, parts) <- perIndicator]))
    pure (weighParts weights perIndicator)

{- | The score, and every flow's part of it. A flow's factor is read back as
its part over its quantity: each indicator may apply its own factor to the flow
in a unit of its own (per MJ where the flow is in kg), so the factors
themselves do not add up.
-}
flowParts :: Source -> ResolvedScore -> SharedSolver.CrossDBSolution -> IO (Either ScoreRefusal (Double, [FlowContribution]))
flowParts src rs sol = runExceptT $ do
    perIndicator <- forM (rsIndicators rs) $ \method -> do
        tables <- liftIO $ tablesOf src method
        score <- withExceptT ScoringFailed (ExceptT (Impact.scoreSolution (srcManager src) (srcCollection src) method tables sol))
        (parts, unknown) <- withExceptT ScoringFailed (ExceptT (Impact.contributionsOf (srcManager src) (srcCollection src) method tables sol))
        liftIO $ Impact.warnUnknownFlowIds ("single score " <> methodName method) unknown
        pure (methodName method, score, parts)
    let scores = M.fromList [(name, score) | (name, score, _) <- perIndicator]
    weights <- except (weightsOf rs scores)
    let flows = M.fromList [(bfId (fcFlow fc), fcFlow fc) | (_, _, parts) <- perIndicator, fc <- parts]
        weighed = weighParts weights [(name, M.fromListWith (+) [(bfId (fcFlow fc), fcContribution fc) | fc <- parts]) | (name, _, parts) <- perIndicator]
    pure
        ( sum (M.intersectionWith (*) weights scores)
        , M.elems (M.mapWithKey (flowPart (SharedSolver.csInventory sol)) (M.intersectionWith (,) flows weighed))
        )
  where
    -- A flow whose quantity nets to zero (emitted in one place, avoided in
    -- another) keeps its part and reads a factor of zero, as an indicator's
    -- own rows do ('Method.Mapping.sharesToContributions').
    flowPart :: Inventory -> UUID -> (BiosphereFlow, Double) -> FlowContribution
    flowPart inventory fid (f, c) =
        FlowContribution
            { fcFlow = f
            , fcFactor = maybe 0 (c /) (mfilter (/= 0) (M.lookup fid inventory))
            , fcContribution = c
            }

-- | A method's tables against the root database.
tablesOf :: Source -> Method -> IO MethodTables
tablesOf src = mapMethodToTablesCached (srcManager src) (srcDbName src) (srcCollection src) (srcDatabase src)

-- | The weights of the score, or why it has none.
weightsOf :: ResolvedScore -> M.Map Text Double -> Either ScoreRefusal (M.Map Text Double)
weightsOf rs = first Unsplittable . scoreWeights (rsSet rs) (rsScore rs)

{- | Each indicator's parts, weighed into the score's: a part of the score is
the weighted sum of that part in every indicator.
-}
weighParts :: (Ord k) => M.Map Text Double -> [(Text, M.Map k Double)] -> M.Map k Double
weighParts weights perIndicator =
    M.unionsWith (+) [M.map (* w) parts | (name, parts) <- perIndicator, Just w <- [M.lookup name weights]]
