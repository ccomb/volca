{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Pure JSON munging used by the @score_activity@ MCP tool and by
single-activity handlers that need a @web_url@ deep link, a slimmer
panel, a scoring-set filter, or a market-activity hint.

The columnar batch projection for @score_activities@ lives in
"API.MCP" - it works on typed records, not raw 'Value's, so it does not
fit this module's "pure JSON munging" remit.

Every traversal goes through 'overObject' / 'overArray', the single
place that has to enumerate every constructor of Aeson's 'Value'.
Callers stay free of wildcard patterns on the sum.
-}
module API.MCP.Enrich (
    -- * URL helpers
    encodeSegment,
    scoreActivityWebUrl,

    -- * web_url enrichment
    addWebUrl,
    addWebUrlMaybe,
    webUrlField,

    -- * payload slimming
    slimLCIAPanel,
    summarizeLCIAPanel,

    -- * scoring_sets filter
    filterScoringSets,

    -- * market-activity hint
    isMarketActivityName,
    attachMarketHintByName,

    -- * Value combinators (exported for tests)
    overObject,
    overArray,
) where

import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.Key as Key
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Pair)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Network.URI (escapeURIString, isUnreserved)

-- ---------------------------------------------------------------------------
-- Value combinators - single point of exhaustive Aeson pattern matching
-- ---------------------------------------------------------------------------

{- | Apply 'f' to the contents of a JSON Object; pass any other 'Value'
shape through unchanged. One exhaustive match lives here so individual
callers do not sprinkle wildcards.
-}
overObject :: (KeyMap Value -> KeyMap Value) -> Value -> Value
overObject f = \case
    Object km -> Object (f km)
    Array a -> Array a
    String s -> String s
    Number n -> Number n
    Bool b -> Bool b
    Null -> Null

-- | Apply 'f' to each element of a JSON Array; pass any other shape through.
overArray :: (Value -> Value) -> Value -> Value
overArray f = \case
    Array v -> Array (V.map f v)
    Object km -> Object km
    String s -> String s
    Number n -> Number n
    Bool b -> Bool b
    Null -> Null

-- ---------------------------------------------------------------------------
-- URL helpers
-- ---------------------------------------------------------------------------

-- | Percent-encode a 'Text' value for use as a URL path segment.
encodeSegment :: Text -> Text
encodeSegment = T.pack . escapeURIString isUnreserved . T.unpack

{- | Activity-level impacts page URL (the LCIA batch view in the web UI).
Every dynamic segment is percent-encoded so a 'dbName' \/ 'processId' \/
'collection' that contains @/@ or @?@ does not silently fracture the
URL.

The 'Maybe' on 'baseUrl' threads frontend availability through callers:
'Nothing' means the SPA is not bundled (backend-only image), and the
URL would point at a 404. We propagate the absence rather than emit a
dead link.
-}
scoreActivityWebUrl :: Maybe Text -> Text -> Text -> Text -> Maybe Text
scoreActivityWebUrl mBaseUrl dbName pidText coll = do
    base <- mBaseUrl
    pure $
        base
            <> "/db/"
            <> encodeSegment dbName
            <> "/activity/"
            <> encodeSegment pidText
            <> "/impacts/"
            <> encodeSegment coll

-- ---------------------------------------------------------------------------
-- web_url enrichment
-- ---------------------------------------------------------------------------

webUrlKey, resultsKey, functionalUnitKey, hintKey :: Key.Key
webUrlKey = fromText "web_url"
resultsKey = fromText "results"
functionalUnitKey = fromText "functionalUnit"
hintKey = fromText "hint"

-- | Add a 'web_url' field to a JSON object at the top level.
addWebUrl :: Text -> Value -> Value
addWebUrl url = overObject (KM.insert webUrlKey (String url))

{- | Like 'addWebUrl', but a no-op when the URL is 'Nothing'. Lets callers
thread frontend availability through 'Maybe Text' without case-splitting
at every emission site.
-}
addWebUrlMaybe :: Maybe Text -> Value -> Value
addWebUrlMaybe = maybe id addWebUrl

{- | Inline-object companion to 'addWebUrlMaybe' for handlers that build
their response via @object (fields ++ extras)@.

Yields @["web_url" .= base <> path]@ when a base URL is configured and
@[]@ otherwise - keeping the "drop the field when no frontend" invariant
in one place rather than fanning out a 'case' at every emission site.
-}
webUrlField :: Maybe Text -> Text -> [Pair]
webUrlField mBase path = foldMap (\b -> ["web_url" .= (b <> path)]) mBase

{- | Reduce a serialized 'LCIABatchResult' to its aggregates for bulk
transport: hoist @functionalUnit@ to the top level (same value on
every entry anyway) and drop the per-method @results@ array entirely.
What stays: @singleScore@, @scoringResults@, @scoringIndicators@,
@scoringUnits@, @availableNWsets@, @normWeightSetName@.

A ranking caller has everything it needs in those aggregates. A caller
that wants per-method drill-down on a specific activity calls
@score_activity@ on that one process_id.
-}
summarizeLCIAPanel :: Value -> Value
summarizeLCIAPanel = overObject (KM.delete resultsKey . hoistFunctionalUnit)

-- ---------------------------------------------------------------------------
-- payload slimming
-- ---------------------------------------------------------------------------

{- | Slim down a serialized 'LCIABatchResult' for MCP transport. Two
edits, both purely about wire weight (no information lost):

  * Hoist @functionalUnit@ from @results[0]@ to the top level. Every
    entry in @results@ shares the same value (one panel = one activity
    = one functional unit), so repeating it 27 times is pure bloat.
    The lifted copy lives next to @results@ and the per-entry copies
    are removed.
  * Drop @web_url@ from every entry. The panel-level @web_url@ added
    by 'addWebUrl' already lands on the page that lists every method;
    a deep link per method would be redundant.

Defensive on the shape: missing @results@, empty @results@, or entries
without a @functionalUnit@ are all passed through cleanly.
-}
slimLCIAPanel :: Value -> Value
slimLCIAPanel = overObject (stripWebUrlFromEntries . hoistFunctionalUnit)
  where
    stripWebUrlFromEntries = adjustKey resultsKey (overArray (overObject (KM.delete webUrlKey)))

{- | Copy @results[0].functionalUnit@ to the top level of the panel and
remove it from every entry. The value is constant across @results@ by
construction (one panel = one activity = one functional unit), so the
lifted copy carries the same information at a fraction of the bytes.

A missing @results@, an empty @results@, or a first entry without a
@functionalUnit@ all pass through cleanly - nothing is invented.
-}
hoistFunctionalUnit :: KeyMap Value -> KeyMap Value
hoistFunctionalUnit km = case firstFunctionalUnit km of
    Just fu -> KM.insert functionalUnitKey fu (stripFromEntries km)
    Nothing -> km
  where
    stripFromEntries = adjustKey resultsKey (overArray (overObject (KM.delete functionalUnitKey)))

firstFunctionalUnit :: KeyMap Value -> Maybe Value
firstFunctionalUnit km = case KM.lookup resultsKey km of
    Just (Array rs) -> case V.toList rs of
        Object e : _ -> KM.lookup functionalUnitKey e
        _ -> Nothing
    _ -> Nothing

-- ---------------------------------------------------------------------------
-- scoring_sets filter
-- ---------------------------------------------------------------------------

{- | Restrict the scoring-related maps on a serialized 'LCIABatchResult'
to the scoring-set names in 'requested'.

'configured' is the authoritative list of scoring-set names defined on
the method collection (e.g. @map ssName . mcScoringSets@). Validation
runs against it - not against the keys actually present in
@scoringResults@ - so:

  * A configured scoring set whose evaluation produced no scores (and
    is therefore absent from @scoringResults@) is still accepted by
    the filter, instead of being incorrectly reported as unknown.
  * The "unknown name" error message lists the same set of legal
    values regardless of evaluation outcomes.

An empty 'requested' is a no-op; an unknown name is a hard error.
-}
filterScoringSets :: [Text] -> [Text] -> Value -> Either Text Value
filterScoringSets _ [] v = Right v
filterScoringSets configured requested v = do
    validateRequested configured requested
    Right (restrictScoringSets requested v)

{- | Check 'requested' against 'configured', failing with a message that
lists the missing names and the legal options.
-}
validateRequested :: [Text] -> [Text] -> Either Text ()
validateRequested configured requested =
    case [r | r <- requested, r `notElem` configured] of
        [] -> Right ()
        missing ->
            Left
                ( "Unknown scoring set(s): "
                    <> T.intercalate ", " missing
                    <> ". Configured on this collection: "
                    <> T.intercalate ", " configured
                )

{- | Filter the three scoring maps to keys in 'requested'. Validation has
already run in the caller.
-}
restrictScoringSets :: [Text] -> Value -> Value
restrictScoringSets requested =
    overObject (restrictAt scoringIndicatorsKey . restrictAt scoringUnitsKey . restrictAt scoringResultsKey)
  where
    scoringResultsKey = fromText "scoringResults"
    scoringUnitsKey = fromText "scoringUnits"
    scoringIndicatorsKey = fromText "scoringIndicators"
    keep k _ = Key.toText k `elem` requested
    restrict = overObject (KM.filterWithKey keep)
    restrictAt k = adjustKey k restrict

{- | Update one key of a 'KeyMap' via a pure transformation; if the key
is absent, the map is returned unchanged. (Aeson's 'KeyMap' has no
@adjust@ of its own.)
-}
adjustKey :: Key.Key -> (Value -> Value) -> KeyMap Value -> KeyMap Value
adjustKey k f km =
    case KM.lookup k km of
        Just v -> KM.insert k (f v) km
        Nothing -> km

-- ---------------------------------------------------------------------------
-- market-activity hint
-- ---------------------------------------------------------------------------

{- | Case-insensitive test for the @"market for "@ naming convention used
across ecoinvent and SimaPro-imported databases. A market activity is an
aggregated supplier mix, not a source ICV - a caller asking for raw
inventory probably wants the upstream producers instead.
-}
isMarketActivityName :: Text -> Bool
isMarketActivityName name = "market for " `T.isPrefixOf` T.toLower (T.stripStart name)

-- | The hint payload attached to responses for market activities.
marketHintObject :: Value
marketHintObject =
    object
        [ "kind" .= ("market_activity" :: Text)
        , "message"
            .= ( "This is a 'market for ...' activity - an aggregated supplier mix, \
                 \not a source ICV. Call get_activity to inspect its technosphere \
                 \inputs (the actual producers)." ::
                    Text
               )
        ]

{- | When 'name' looks like a market activity, attach a @hint@ field at
the top level of the JSON object. Non-objects and non-markets pass
through unchanged.
-}
attachMarketHintByName :: Text -> Value -> Value
attachMarketHintByName name
    | isMarketActivityName name = overObject (KM.insert hintKey marketHintObject)
    | otherwise = id
