{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Method.Edit
Description : Copying a method collection, and changing a copy through its journal

A collection the configuration declares, or one the engine carries, is never
changed in place: it is copied, and the copy lives under @uploads/methods/@
with a journal of its own. The copy reads its source's files, so it holds
nothing but that journal and the @meta.toml@ that points at them.
-}
module Method.Edit (
    MethodEditRefusal (..),
    refusalText,
    copyMethodCollection,
) where

import Control.Concurrent.MVar (withMVar)
import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import Control.Exception (SomeException, try)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Data.Bifunctor (first)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, makeAbsolute, removeDirectoryRecursive)
import System.FilePath ((</>))

import Builtin (builtinMethodName)
import Config (MethodConfig (..), MethodOrigin (..))
import Data.JournalFile (appendEntry, journalPath)
import Database.Manager (
    DatabaseManager (..),
    addMethodCollection,
    configToScoringSet,
    loadMethodCollection,
    loadMethodCollectionFromConfig,
 )
import Database.Upload (DatabaseFormat (UnknownFormat), slugify)
import qualified Database.UploadedDatabase as UploadedDB
import Method.EditPlan (seedLines)
import Method.Journal (LineKind (..), MethodLine (..), MethodOp)
import Progress (ProgressLevel (..), reportProgress)
import Types (AllocationKey (..))

-- | Why a change to a method collection was not made.
data MethodEditRefusal
    = CollectionNotFound Text
    | CollectionNotLoaded Text
    | NotEditable Text
    | NameTaken Text
    | EditRefused Text
    deriving (Eq, Show)

refusalText :: MethodEditRefusal -> Text
refusalText = \case
    CollectionNotFound n -> "Method collection not found: " <> n
    CollectionNotLoaded n -> "Load " <> n <> " first: a change applies to the collection in use"
    NotEditable n -> n <> " is a collection the configuration declares, or one built into the engine. Copy it, and change the copy."
    NameTaken n -> "A method collection is already named " <> n
    EditRefused t -> t

{- | Copy a collection under a new name, and load the copy. Gives the name the
copy is known by, the slug of the one asked for.
-}
copyMethodCollection :: DatabaseManager -> Text -> Text -> IO (Either MethodEditRefusal Text)
copyMethodCollection manager srcName newName = withMVar (dmMethodEditLock manager) $ \() -> runExceptT $ do
    let slug = slugify newName
    when (T.null slug) $ throwE (EditRefused ("a copy needs a name with letters or digits: " <> newName))
    available <- liftIO (readTVarIO (dmAvailableMethods manager))
    source <- maybe (throwE (CollectionNotFound srcName)) pure (M.lookup srcName available)
    home <- liftIO ((</> T.unpack slug) <$> UploadedDB.getMethodUploadsDir)
    taken <- liftIO (doesDirectoryExist home)
    when (M.member slug available || taken) $ throwE (NameTaken slug)
    seed <- ExceptT (first EditRefused <$> journalSeed source)
    ExceptT (first EditRefused <$> recordMethodCopy home slug source seed)
    liftIO (addMethodCollection manager (copyConfig slug home source))
    loaded <- liftIO (loadMethodCollection manager slug)
    either (\err -> liftIO (discardCopy manager slug home) >> throwE (EditRefused err)) pure loaded
    pure slug

-- | Where a copy's journal starts.
data JournalSeed
    = -- | The journal of a source under @uploads/methods/@, taken as it is.
      CopiedJournal FilePath
    | -- | What the configuration adds to the files of a source it declares.
      SeedLines [MethodOp]

{- | A source already loaded has its configuration applied, hence the second
read of its files: it is what gives the number each patch touches on them,
which its line records.
-}
journalSeed :: MethodConfig -> IO (Either Text JournalSeed)
journalSeed source = case mcHome source of
    Just home -> pure (Right (CopiedJournal (journalPath home)))
    Nothing -> fmap (SeedLines . fromConfig . fst) <$> loadMethodCollectionFromConfig source
  where
    fromConfig =
        seedLines
            (map configToScoringSet (mcScoringSets source))
            (mcPatches source)
            (mcGlobalMethods source)

{- | Write a copy's home: its journal, then its @meta.toml@. A directory without
@meta.toml@ is not discovered, so a copy cut short leaves nothing a restart
would take for one.
-}
recordMethodCopy :: FilePath -> Text -> MethodConfig -> JournalSeed -> IO (Either Text ())
recordMethodCopy home slug source seed = do
    written <- try $ do
        createDirectoryIfMissing True home
        journaled <- case seed of
            CopiedJournal from -> do
                exists <- doesFileExist from
                Right () <$ when exists (copyFile from (journalPath home))
            SeedLines ops -> sequence_ <$> mapM (\op -> appendEntry home (MethodLine op TakenFromConfiguration)) ops
        dataPath <- traverse makeAbsolute (filePath (mcOrigin source))
        UploadedDB.writeUploadMeta
            home
            UploadedDB.UploadMeta
                { UploadedDB.umVersion = UploadedDB.metaVersion
                , UploadedDB.umDisplayName = slug
                , UploadedDB.umDescription = mcDescription source
                , UploadedDB.umFormat = UnknownFormat
                , UploadedDB.umDataPath = fromMaybe "" dataPath
                , UploadedDB.umDepends = []
                , UploadedDB.umSource = Just (mcName source)
                , UploadedDB.umAllocation = Declared
                , UploadedDB.umBuiltIn = builtinOf (mcOrigin source)
                }
        pure journaled
    pure $ case written of
        Right result -> result
        Left (err :: SomeException) -> Left ("could not record the copy " <> slug <> ": " <> T.pack (show err))
  where
    filePath :: MethodOrigin -> Maybe FilePath
    filePath = \case
        MethodFromFile path -> Just path
        MethodBuiltIn _ -> Nothing
    builtinOf :: MethodOrigin -> Maybe Text
    builtinOf = \case
        MethodFromFile _ -> Nothing
        MethodBuiltIn b -> Just (builtinMethodName b)

-- | The copy as the registry lists it: its source's files, nothing configured.
copyConfig :: Text -> FilePath -> MethodConfig -> MethodConfig
copyConfig slug home source =
    source
        { mcName = slug
        , mcActive = False
        , mcHome = Just home
        , mcSource = Just (mcName source)
        , mcScoringSets = []
        , mcGlobalMethods = []
        , mcPatches = []
        }

-- | A copy that does not load is not kept.
discardCopy :: DatabaseManager -> Text -> FilePath -> IO ()
discardCopy manager slug home = do
    atomically (modifyTVar' (dmAvailableMethods manager) (M.delete slug))
    removed <- try (removeDirectoryRecursive home)
    either
        (\(err :: SomeException) -> reportProgress Warning ("could not remove the copy " <> home <> " that did not load: " <> show err))
        pure
        removed
