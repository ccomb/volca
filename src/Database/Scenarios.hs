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

Saved scenarios live in their own journal, @scenarios/journal.jsonl@ in the
database's upload directory, apart from the database's journal: saving one
changes nothing in the database, so it must not look like an edit of it to
the cache or to a replay. Nothing is kept in memory: every listing, save and
drop reads the file, which holds a handful of lines.
-}
module Database.Scenarios (
    ScenarioOp (..),
    scenariosOf,
    scenarioProblem,
    listScenarios,
    saveScenario,
    dropScenario,
) where

import Control.Concurrent.MVar (withMVar)
import Control.Monad (void)
import Data.Aeson (Value, object, withObject, (.:), (.:?), (.=))
import Data.Aeson.Types (Parser)
import Data.Bifunctor (first)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T

import API.Types (ParameterValue (..), ScenarioAPI (..))
import Data.JournalFile (Entry (..), JournalVocabulary (..), appendEntry, readEntries)
import Database.Edit (WriteRefusal (..))
import Database.Manager (DatabaseManager (..), LoadedDatabase (..), editHome, getDatabase, scenariosHome)
import Parametric (describeParameterRefusal, instantiate)
import Service (serviceMessage, settingsOf)
import Types (Database)

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

{- | The scenarios saved on a loaded database. A database read from the
configuration has none: it cannot hold any.
-}
listScenarios :: DatabaseManager -> Text -> IO (Either WriteRefusal [ScenarioAPI])
listScenarios manager dbName =
    getDatabase manager dbName >>= \case
        Nothing -> pure (Left (NotLoaded dbName))
        Just loaded ->
            editHome (ldConfig loaded) >>= \case
                Nothing -> pure (Right [])
                Just home -> do
                    saved <- readEntries (scenariosHome home)
                    pure $ case saved of
                        Left err -> Left (WriteFailed err)
                        Right entries -> Right [ScenarioAPI name values (scenarioProblem (ldDatabase loaded) values) | (name, values) <- M.toList (scenariosOf entries)]

{- | Save values under a name, replacing what that name held. Refused when a
scoring request would refuse the values, and on a database read from the
configuration, where a scenario would be lost at the next restart.
-}
saveScenario :: DatabaseManager -> Text -> Text -> [ParameterValue] -> IO (Either WriteRefusal ())
saveScenario manager dbName name values
    | T.null (T.strip name) = pure (Left (Malformed ["A scenario needs a name."]))
    | null values = pure (Left (Malformed ["The scenario '" <> name <> "' sets no parameter: the defaults need no scenario."]))
    | otherwise = withScenarios manager dbName $ \db home _ ->
        case scenarioProblem db values of
            Just problem -> pure (Left (Malformed [problem]))
            Nothing -> first WriteFailed <$> appendEntry home (Saved name values)

-- | Drop a saved scenario. Refused for a name that holds none.
dropScenario :: DatabaseManager -> Text -> Text -> IO (Either WriteRefusal ())
dropScenario manager dbName name = withScenarios manager dbName $ \_ home saved ->
    if M.member name saved
        then first WriteFailed <$> appendEntry home (Dropped name)
        else pure (Left (NotPresent ["scenario '" <> name <> "'"]))

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
