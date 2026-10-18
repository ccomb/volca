{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Database.Scenarios
Description : Named sets of parameter values, saved on a database

A scenario is a name and the parameter values that make it: "French mix,
30-year life" is the mix parameters and the lifetime set to those values. A
client applies one by sending its values with a scoring request, so the
engine never scores a scenario by name.

A database's source may ship scenarios of its own, read with it at every
load; those are listed beside the saved ones and are neither replaced nor
dropped. A scenario saved under such a name before the source shipped it is
the one listed, and dropping it shows the shipped one again.

Saved scenarios live in their own journal, @scenarios/journal.jsonl@ in the
database's upload directory, apart from the database's journal: saving one
changes nothing in the database, so it must not look like an edit of it to
the cache or to a replay. Nothing is kept in memory: every listing, save and
drop reads the file, which holds a handful of lines.
-}
module Database.Scenarios (
    ScenarioOp (..),
    ScenarioName (..),
    scenariosOf,
    scenarioProblem,
    listScenarios,
    savedForExport,
    saveScenario,
    dropScenario,
) where

import Control.Concurrent.MVar (withMVar)
import Control.Monad (void)
import Data.Aeson (Value, object, withObject, (.:), (.:?), (.=))
import Data.Aeson.Types (Parser)
import Data.Bifunctor (first)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID

import API.Types (ParameterValue (..), ScenarioAPI (..))
import Data.JournalFile (Entry (..), JournalVocabulary (..), appendEntry, readEntries)
import Database.Edit (WriteRefusal (..))
import Database.Manager (DatabaseManager (..), LoadedDatabase (..), editHome, getDatabase, scenariosHome)
import OlcaSchema.Parser (shippedScenarios)
import Parametric (Setting (..), describeParameterRefusal, instantiate)
import Service (serviceMessage, settingsOf)
import Types (Database (..), ProcessRef (..), ScenarioValue (..), processIdToRef, processIdToText)

-- | The name a scenario is saved under, apart from the name of its database.
newtype ScenarioName = ScenarioName Text
    deriving newtype (IsString)

-- | What a line of the scenarios journal says.
data ScenarioOp
    = -- | Saved under this name, replacing a scenario of the same name.
      Saved Text [ParameterValue]
    | Dropped Text

instance JournalVocabulary ScenarioOp where
    vocabularyVersion _ = 1
    opFields = \case
        Saved name values -> ["op" .= ("save" :: Text), "name" .= name, "parameters" .= map valueJSON values]
        Dropped name -> ["op" .= ("drop" :: Text), "name" .= name]
    parseOp o =
        o .: "op" >>= \case
            ("save" :: Text) -> Saved <$> o .: "name" <*> (o .: "parameters" >>= traverse parseValue)
            "drop" -> Dropped <$> o .: "name"
            other -> fail ("unknown scenario operation: " <> T.unpack other)

-- Written out here rather than through the wire type, so a rename on the wire leaves the file readable.
valueJSON :: ParameterValue -> Value
valueJSON ParameterValue{pvProcess = process, pvName = name, pvValue = value} = object ["process" .= process, "name" .= name, "value" .= value]

parseValue :: Value -> Parser ParameterValue
parseValue = withObject "parameter value" $ \v -> ParameterValue <$> v .:? "process" <*> v .: "name" <*> v .: "value"

-- | The scenarios a journal leaves, by name.
scenariosOf :: [Entry ScenarioOp] -> M.Map Text [ParameterValue]
scenariosOf = foldl' step M.empty . map jeOp
  where
    step :: M.Map Text [ParameterValue] -> ScenarioOp -> M.Map Text [ParameterValue]
    step saved = \case
        Saved name values -> M.insert name values saved
        Dropped name -> M.delete name saved

-- | Why these values cannot be set on the database, as a scoring request would be refused for them.
scenarioProblem :: Database -> [ParameterValue] -> Maybe Text
scenarioProblem db values = either Just (const Nothing) $ do
    settings <- first serviceMessage (settingsOf db values)
    void (first describeParameterRefusal (instantiate db settings))

{- | The scenarios of a loaded database: those its source ships, then those
saved on it. A database read from the configuration has no saved one: it
cannot hold any.
-}
listScenarios :: DatabaseManager -> Text -> IO (Either WriteRefusal [ScenarioAPI])
listScenarios manager dbName =
    getDatabase manager dbName >>= \case
        Nothing -> pure (Left (NotLoaded dbName))
        Just loaded ->
            editHome (ldConfig loaded) >>= \case
                Nothing -> pure (Right (scenarioListing (ldDatabase loaded) M.empty))
                Just home -> do
                    saved <- readEntries (scenariosHome home)
                    pure $ case saved of
                        Left err -> Left (WriteFailed err)
                        Right entries -> Right (scenarioListing (ldDatabase loaded) (scenariosOf entries))

{- | The saved scenarios of a listing as an export writes them, each value's
process named by its activity, and why each one left out is: one with a
problem would not apply to the database read back either.
-}
savedForExport :: Database -> [ScenarioAPI] -> (M.Map Text [ScenarioValue], [Text])
savedForExport db scenarios =
    ( M.fromList [(saName s, values) | (s, Right values) <- resolved]
    , [saName s <> ": " <> why | (s, Left why) <- resolved]
    )
  where
    resolved :: [(ScenarioAPI, Either Text [ScenarioValue])]
    resolved = [(s, resolve s) | s <- scenarios, not (saShipped s)]

    resolve :: ScenarioAPI -> Either Text [ScenarioValue]
    resolve s = do
        maybe (Right ()) Left (saProblem s)
        settings <- first serviceMessage (settingsOf db (saParameters s))
        traverse value settings

    value :: Setting -> Either Text ScenarioValue
    value Setting{settingProcess = pid, settingName = name, settingValue = v} = do
        process <- traverse (maybe (Left "its process is not in the database") (Right . prActivity) . processIdToRef db) pid
        Right ScenarioValue{svProcess = process, svName = name, svValue = v}

-- | The scenarios the database ships, by name.
shipped :: Database -> M.Map Text [ScenarioValue]
shipped = fst . shippedScenarios . dbProductSystems

-- | The shipped scenarios no saved one replaces, then the saved ones, each with its problem.
scenarioListing :: Database -> M.Map Text [ParameterValue] -> [ScenarioAPI]
scenarioListing db saved =
    [ ScenarioAPI{saName = name, saParameters = values, saProblem = scenarioProblem db values, saShipped = True}
    | (name, values) <- M.toList (M.map (map (shippedValue db)) (shipped db))
    , M.notMember name saved
    ]
        <> [ScenarioAPI{saName = name, saParameters = values, saProblem = scenarioProblem db values, saShipped = False} | (name, values) <- M.toList saved]

{- | A shipped value as a client sends it back: its process named by the first
product of its block, where a value lands on every product. An activity the
database does not hold is named by its bare identifier, which the problem of
the scenario then reports as not found.
-}
shippedValue :: Database -> ScenarioValue -> ParameterValue
shippedValue db ScenarioValue{svProcess = process, svName = name, svValue = value} =
    ParameterValue{pvProcess = processText <$> process, pvName = name, pvValue = value}
  where
    processText :: UUID.UUID -> Text
    processText activity = maybe (UUID.toText activity) (processIdToText db . NE.head) (M.lookup activity (dbActivityUUIDIndex db))

{- | Save values under a name, replacing what that name held. Refused when a
scoring request would refuse the values, under the name of a scenario the
database ships that no saved one holds, and on a database read from the
configuration, where a scenario would be lost at the next restart.
-}
saveScenario :: DatabaseManager -> Text -> ScenarioName -> [ParameterValue] -> IO (Either WriteRefusal ())
saveScenario manager dbName (ScenarioName name) values
    | T.null (T.strip name) = pure (Left (Malformed ["A scenario needs a name."]))
    | T.strip name /= name = pure (Left (Malformed ["The scenario name '" <> name <> "' starts or ends with a space."]))
    | null values = pure (Left (Malformed ["The scenario '" <> name <> "' sets no parameter: the defaults need no scenario."]))
    | otherwise = withScenarios manager dbName $ \db home saved ->
        -- One saved before the database shipped a scenario of its name is the one listed, so it stays replaceable.
        if M.member name (shipped db) && M.notMember name saved
            then pure (Left (Malformed ["The scenario '" <> name <> "' ships with the database: save under another name."]))
            else case scenarioProblem db values of
                Just problem -> pure (Left (Malformed [problem]))
                Nothing -> first WriteFailed <$> appendEntry home (Saved name values)

-- | Drop a saved scenario. Refused for a name that holds none, and for one the database ships.
dropScenario :: DatabaseManager -> Text -> ScenarioName -> IO (Either WriteRefusal ())
dropScenario manager dbName (ScenarioName name) = withScenarios manager dbName $ \db home saved ->
    if M.member name saved
        then first WriteFailed <$> appendEntry home (Dropped name)
        else
            pure . Left $
                if M.member name (shipped db)
                    then Malformed ["The scenario '" <> name <> "' ships with the database, and comes back at the next load."]
                    else NotPresent ["scenario '" <> name <> "'"]

-- | Run a change of the scenarios of an uploaded database, one at a time, with the scenarios it holds.
withScenarios :: DatabaseManager -> Text -> (Database -> FilePath -> M.Map Text [ParameterValue] -> IO (Either WriteRefusal ())) -> IO (Either WriteRefusal ())
withScenarios manager dbName change = withMVar (dmScenarioLock manager) $ \() ->
    getDatabase manager dbName >>= \case
        Nothing -> pure (Left (NotLoaded dbName))
        Just loaded ->
            editHome (ldConfig loaded) >>= \case
                Nothing -> pure (Left (NotWritable dbName))
                Just home ->
                    readEntries (scenariosHome home) >>= \case
                        Left err -> pure (Left (WriteFailed err))
                        Right entries -> change (ldDatabase loaded) (scenariosHome home) (scenariosOf entries)
