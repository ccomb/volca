{-# LANGUAGE OverloadedStrings #-}

{- | The file that says how a child is divided from its parent: a property
for every block of several products, and exceptions giving some of them
another, each with its reason.

@
name = "Agribalyse 4.0, economic allocation"
version = "1.0.0"
description = "Price everywhere, dry matter for cheese making."

[allocation]
default = "price"

[[allocation.exceptions]]
key = "dry mass"
reason = "Dry matter follows the milk, not the price."
classification = { system = "Category", value = "Agricultural\\\\Food\\\\Cheese production" }
@

The file is read strictly: a key it does not know is refused, since a rule
written for a later release and skipped here would be a child its author did
not ask for.
-}
module Database.SystemModel (
    parseSystemModel,
    resolveModel,
    Resolution (..),
    parentProcesses,
    takenProcesses,
    severalProducts,
) where

import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import TOML (Decoder, TOMLError, Table, Value, decode, decodeWith, getArrayOf, getField, getFieldOpt, getFieldWith, renderTOMLError, tomlDecoder)

import Config (KeySpec (..), unknownKeys)
import Types (
    Activity (..),
    AllocationProperty,
    ClassificationFilter (..),
    ClassificationMatch (..),
    Database,
    ModelException (..),
    Reason (..),
    Selector (..),
    SimpleDatabase (..),
    SystemModel (..),
    classificationMatches,
    exchangeIsProductOutput,
    propertyName,
    reasonText,
    toSimpleDatabase,
 )
import qualified Types

{- | A system model from the text of its file, or every reason it is not one.
The text is kept as written.
-}
parseSystemModel :: Text -> Either Text SystemModel
parseSystemModel text = do
    table <- either (Left . renderTOMLError) Right (decode text :: Either TOMLError Table)
    case unknownKeys modelKeys table of
        [] -> pure ()
        unknown -> Left ("the system model names keys nothing reads: " <> T.intercalate ", " unknown)
    model <- either (Left . renderTOMLError) Right (decodeWith (modelDecoder text) text)
    case [exReason ex | ex <- smExceptions model, exKey ex == smDefault model] of
        [] -> pure model
        Reason why : _ -> Left ("an exception divides under the default key, so it changes nothing: " <> why)

modelKeys :: KeySpec
modelKeys =
    keys
        [ plain "name"
        , plain "version"
        , plain "description"
        ,
            ( "allocation"
            , keys
                [ plain "default"
                ,
                    ( "exceptions"
                    , keys [plain "key", plain "reason", plain "processes", ("classification", keys [plain "system", plain "value"])]
                    )
                ]
            )
        ]
  where
    keys = Accepts . M.fromList
    plain k = (k, Accepts M.empty)

modelDecoder :: Text -> Decoder SystemModel
modelDecoder text = do
    name <- getField "name"
    version <- getField "version"
    description <- getFieldOpt "description"
    (deflt, exceptions) <- getFieldWith allocation "allocation"
    pure
        SystemModel
            { smName = name
            , smVersion = version
            , smDescription = description
            , smDefault = deflt
            , smExceptions = exceptions
            , smText = text
            }
  where
    allocation :: Decoder (AllocationProperty, [ModelException])
    allocation = (,) <$> getFieldWith property "default" <*> (fromMaybe [] <$> optionalWith (getArrayOf exception) "exceptions")

    exception :: Decoder ModelException
    exception = do
        key <- getFieldWith property "key"
        why <- T.strip <$> getField "reason"
        selector <- (,) <$> optionalWith classification "classification" <*> getFieldOpt "processes"
        reason <- if T.null why then fail "an exception needs its reason" else pure (Reason why)
        selects <- case selector of
            (Just c, Nothing) -> pure (ByClassification c)
            (Nothing, Just ids) -> ByProcesses <$> processIds ids
            (Nothing, Nothing) -> fail ("the exception \"" <> T.unpack why <> "\" takes no process: write classification or processes")
            (Just _, Just _) -> fail ("the exception \"" <> T.unpack why <> "\" writes both classification and processes: write one")
        pure ModelException{exKey = key, exReason = reason, exSelects = selects}

    -- toml-reader's getFieldOptWith reads a field missing inside the table as
    -- the table itself missing, which dropped an exception written without
    -- its reason: once the table is there, every field it needs is required.
    optionalWith :: Decoder a -> Text -> Decoder (Maybe a)
    optionalWith decoder key = do
        present <- getFieldOpt key :: Decoder (Maybe Value)
        traverse (const (getFieldWith decoder key)) present

    classification :: Decoder ClassificationFilter
    classification = do
        system <- getField "system"
        value <- getField "value"
        pure ClassificationFilter{clfSystem = system, clfValue = value, clfMatch = MatchExact}

    processIds :: [Text] -> Decoder (NonEmpty UUID)
    processIds raw = case partitionEithers [maybe (Left t) Right (UUID.fromText (T.strip t)) | t <- raw] of
        ([], u : us) -> pure (u :| us)
        ([], []) -> fail "processes is empty"
        (bad, _) -> fail ("not process identifiers: " <> T.unpack (T.intercalate ", " bad))

    property :: Decoder AllocationProperty
    property = do
        raw <- tomlDecoder :: Decoder Text
        case Types.parseAllocationKey raw of
            Right (Types.ByProperty prop) -> pure prop
            Right _ -> fail ("a system model divides by price, dry mass or wet mass, not by " <> T.unpack raw)
            Left err -> fail (T.unpack err)

{- | What a model does to one parent: the property each block of several
products is divided under, keyed by the block's activity, and the blocks each
exception took.
-}
data Resolution = Resolution
    { rsKeys :: !(M.Map UUID AllocationProperty)
    , rsTaken :: ![(ModelException, [UUID])]
    }

{- | The model against the parent's blocks, or every way it does not fit:
a process it names that the parent lacks or that makes one product, an
exception that takes no block, a block two exceptions take.

The parent is given as its processes keyed by activity.
-}
resolveModel :: SystemModel -> M.Map UUID Activity -> Either (NonEmpty Text) Resolution
resolveModel model allActs = case NE.nonEmpty (concatMap problemsOf taken <> overlaps) of
    Just problems -> Left problems
    Nothing ->
        Right
            Resolution
                { rsKeys = M.union (M.fromList [(u, exKey ex) | (ex, us) <- taken, u <- us]) (smDefault model <$ blocks)
                , rsTaken = taken
                }
  where
    blocks :: M.Map UUID Activity
    blocks = M.filter severalProducts allActs

    taken :: [(ModelException, [UUID])]
    taken = [(ex, takenBy (exSelects ex)) | ex <- smExceptions model]

    takenBy :: Selector -> [UUID]
    takenBy selector = case selector of
        ByClassification f -> M.keys (M.filter (classificationMatches f . activityClassification) blocks)
        ByProcesses ids -> filter (`M.member` blocks) (S.toList (S.fromList (NE.toList ids)))

    problemsOf :: (ModelException, [UUID]) -> [Text]
    problemsOf (ex, us) =
        [quoted ex <> " names " <> UUID.toText u <> ", which the parent " <> lacking u | ByProcesses ids <- [exSelects ex], u <- NE.toList ids, not (M.member u blocks)]
            <> [quoted ex <> " takes no process of several products" | null us]

    lacking :: UUID -> Text
    lacking u = maybe "does not hold" (const "holds with one product only") (M.lookup u allActs)

    overlaps :: [Text]
    overlaps =
        [ activityName (blocks M.! u) <> " is taken by two exceptions: " <> T.intercalate " and " (map quoted exs)
        | (u, exs) <- M.toList (M.fromListWith (flip (<>)) [(u, [ex]) | (ex, us) <- taken, u <- S.toList (S.fromList us)])
        , length exs > 1
        ]

    quoted :: ModelException -> Text
    quoted ex = "the " <> propertyName (exKey ex) <> " exception \"" <> reasonText (exReason ex) <> "\""

-- | A block: the only kind of process a model divides.
severalProducts :: Activity -> Bool
severalProducts act = length (filter exchangeIsProductOutput (exchanges act)) > 1

-- | A parent's processes keyed by activity, as 'resolveModel' reads them.
parentProcesses :: Database -> M.Map UUID Activity
parentProcesses parent = M.fromList [(activityId, act) | ((activityId, _), act) <- M.toList (sdbActivities (toSimpleDatabase parent))]

{- | Each exception of a model with the parent's processes it takes, by
activity and name: what a reader checks a rule against its reason with.
-}
takenProcesses :: SystemModel -> Database -> Either (NonEmpty Text) [(ModelException, [(UUID, Text)])]
takenProcesses model parent = named <$> resolveModel model processes
  where
    processes :: M.Map UUID Activity
    processes = parentProcesses parent

    named :: Resolution -> [(ModelException, [(UUID, Text)])]
    named resolution = [(ex, [(u, activityName act) | u <- us, Just act <- [M.lookup u processes]]) | (ex, us) <- rsTaken resolution]
