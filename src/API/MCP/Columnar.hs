{-# LANGUAGE OverloadedStrings #-}

{- | Columnar JSON projection used by the @score_activities@ MCP tool.

The shape hoists batch-constant metadata (scoring set name, unit,
functional unit when the whole batch shares one) to the top level and
packs each activity as a flat array of scalars indexed by 'columns'.
Trades a few bytes of header for the N×M repetition of JSON keys the
previous row-shaped payload paid for - typically ~6× smaller for a
batch of 24+ activities.

Lives in its own module rather than "API.MCP.Enrich" because it works
on typed records ('BatchImpactsResponse', 'ScoringSet') rather than raw
'Value's, and so it is also straightforward to unit-test independently
of the live MCP server.
-}
module API.MCP.Columnar (
    -- * Scoring set selection
    resolveSingleScoringSet,

    -- * Columnar projection
    toColumnarBatch,

    -- * Internals exported for tests
    dominantIndicatorCell,
) where

import API.MCP.Enrich (scoreActivityWebUrl)
import API.Types (
    BatchImpactsEntry (..),
    BatchImpactsResponse (..),
    LCIABatchResult (..),
    LCIAResult (..),
    ScoringIndicator (..),
 )
import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (isJust, mapMaybe, maybeToList)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Method.Types (ScoringSet (..))

-- ---------------------------------------------------------------------------
-- Scoring set selection
-- ---------------------------------------------------------------------------

{- | Pick the single 'ScoringSet' the columnar 'score_activities' response
will project against. The columnar shape covers one set per call (one
unit, one list of indicator columns), so:

  * Caller passes one name in 'scoring_sets' → that one (validated).
  * Caller passes none, exactly one set is configured → that one.
  * Caller passes none, several sets are configured → error listing the
    options. We don't pick arbitrarily to avoid silent ambiguity.
  * Caller passes several names → error: only one is supported.
  * No sets configured at all → error.
-}
resolveSingleScoringSet :: [Text] -> [ScoringSet] -> Either Text ScoringSet
resolveSingleScoringSet wantedNames configured = case wantedNames of
    [] -> case configured of
        [] -> Left "No scoring sets configured on this collection."
        [s] -> Right s
        ss ->
            Left $
                "Multiple scoring sets are configured ("
                    <> T.intercalate ", " (map ssName ss)
                    <> "); pass scoring_sets: [\"<one>\"] to pick one. score_activities returns a columnar shape that covers a single scoring set per call."
    [w] -> case L.find (\s -> ssName s == w) configured of
        Just s -> Right s
        Nothing ->
            Left $
                "Unknown scoring set: "
                    <> w
                    <> ". Configured on this collection: "
                    <> T.intercalate ", " (map ssName configured)
    ws ->
        Left $
            "score_activities accepts at most one scoring set in 'scoring_sets'; got ["
                <> T.intercalate ", " ws
                <> "]. The columnar response shape covers a single scoring set per call."

-- ---------------------------------------------------------------------------
-- Columnar projection
-- ---------------------------------------------------------------------------

{- | Project a 'BatchImpactsResponse' against one chosen 'ScoringSet' into
the columnar JSON shape:

@
{ "scoring_set":     "PEF"
, "scoring_unit":    "µPts PEF"
, "functional_unit": "1.00 cubic meter of ..."   -- only when all rows agree
, "columns": ["activity_name", "process_id", "web_url", "total", "acd", ...]
, "rows":    [[...], [...], ...]
, "not_found": [...]
, "invalid":   [...]
}
@

Functional unit handling avoids the silent-misrepresentation trap of an
unconditional hoist: rows of a batch may carry different reference
products (e.g. 1 kg of milk vs 1 kg of steak), so the FU is only lifted
to the top level when every row in the batch agrees on it. When the
batch is heterogeneous, the top-level field is dropped and a
@functional_unit@ column is appended to each row instead. The columns
header reflects which shape was emitted.

A row's @total@ cell is the @total@ score from @ssScores@ when present;
otherwise null. An indicator cell is the @siValue@ of the matching entry
in @lbrScoringIndicators[setName]@; missing keys land as null.

With @summaryOnly = True@, the per-indicator columns collapse to a
single @dominant_indicator@ column carrying an object
@{key, share_pct}@ - the variable with the largest absolute share of
the total. Useful for ranking large batches before drilling into one
PID with @score_activity@.

When the base URL is 'Nothing' (backend-only deployment, no Elm SPA
bundled), the @web_url@ column is dropped from both the header and
every row - emitting a column of dead links would be a silent lie.
-}
toColumnarBatch :: Bool -> Maybe Text -> Text -> Text -> ScoringSet -> BatchImpactsResponse -> Value
toColumnarBatch summaryOnly mBaseUrl dbName coll ss bir =
    object $
        [ "scoring_set" .= ssName ss
        , "scoring_unit" .= scoringUnit
        , "columns" .= columns
        , "rows" .= map entryRow (birResults bir)
        , "not_found" .= birNotFound bir
        , "invalid" .= birInvalid bir
        ]
            ++ topLevelFU
  where
    setName = ssName ss
    rowFUOf :: BatchImpactsEntry -> Maybe Text
    rowFUOf e = case lbrResults (bieImpacts e) of
        r : _ -> Just (lrFunctionalUnit r)
        [] -> Nothing
    uniqueFUs = L.nub (mapMaybe rowFUOf (birResults bir))
    -- When every resolved row shares a single FU, hoist it to top level
    -- and drop the per-row column. Otherwise emit the FU per row to keep
    -- the relabelling honest.
    isHeterogeneous = length uniqueFUs > 1
    topLevelFU = case uniqueFUs of
        [fu] -> ["functional_unit" .= fu]
        _ -> []
    indicatorKeys :: [Text]
    indicatorKeys = M.keys (M.union (ssVariables ss) (ssComputed ss))
    webUrlCol = ["web_url" | isJust mBaseUrl]
    fixedColumns :: [Text]
    fixedColumns
        | isHeterogeneous = ["activity_name", "process_id"] ++ webUrlCol ++ ["functional_unit", "total"]
        | otherwise = ["activity_name", "process_id"] ++ webUrlCol ++ ["total"]
    columns :: [Text]
    columns
        | summaryOnly = fixedColumns ++ ["dominant_indicator"]
        | otherwise = fixedColumns ++ indicatorKeys
    scoringUnit = case birResults bir of
        e : _ -> M.findWithDefault (ssUnit ss) setName (lbrScoringUnits (bieImpacts e))
        [] -> ssUnit ss
    entryRow :: BatchImpactsEntry -> Value
    entryRow e = toJSON cells
      where
        urlCells = toJSON <$> maybeToList (scoreActivityWebUrl mBaseUrl dbName (bieProcessId e) coll)
        lbr = bieImpacts e
        scoreMap = M.findWithDefault M.empty setName (lbrScoringResults lbr)
        indMap = M.findWithDefault M.empty setName (lbrScoringIndicators lbr)
        totalRaw = M.lookup "total" scoreMap
        total = maybe Null toJSON totalRaw
        fuCells
            | isHeterogeneous = [maybe Null toJSON (rowFUOf e)]
            | otherwise = []
        tailCells
            | summaryOnly = [dominantIndicatorCell totalRaw indMap]
            | otherwise = map indVal indicatorKeys
        indVal k = maybe Null (toJSON . siValue) (M.lookup k indMap)
        cells =
            [ toJSON (bieActivityName e)
            , toJSON (bieProcessId e)
            ]
                ++ urlCells
                ++ fuCells
                ++ [total]
                ++ tailCells

{- | Format the dominant indicator of a row as a @{key, label, share_pct}@
object - @label@ is the indicator's display name ('siCategory'), so clients
never have to show the raw variable key. Returns 'Null' when the row has no
total, the total is zero (share is undefined), or the indicator map is empty.
-}
dominantIndicatorCell :: Maybe Double -> M.Map Text ScoringIndicator -> Value
dominantIndicatorCell mTotal indMap
    | Just t <- mTotal
    , t /= 0
    , not (M.null indMap) =
        let (k, ind) =
                L.maximumBy
                    (comparing (abs . siValue . snd))
                    (M.toList indMap)
            share = abs (siValue ind) / abs t * 100
         in object ["key" .= k, "label" .= siCategory ind, "share_pct" .= share]
    | otherwise = Null
