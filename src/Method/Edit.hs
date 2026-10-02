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
    EditOutcome (..),
    editMethodFactors,
    undoMethodEdit,
    HistoryLine (..),
    methodHistory,
) where

import Control.Concurrent.MVar (withMVar)
import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import Control.Exception (SomeException, try)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), except, runExceptT, throwE)
import Data.Bifunctor (first)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, makeAbsolute, removeDirectoryRecursive)
import System.FilePath ((</>))

import Builtin (builtinMethodName)
import Config (MethodConfig (..), MethodOrigin (..))
import Data.JournalFile (Entry (..), appendEntry, journalPath, readEntries)
import Database.Manager (
    CollectionName (..),
    DatabaseManager (..),
    addMethodCollection,
    clearMethodCachesFor,
    configToScoringSet,
    getMethodCollection,
    loadMethodCollection,
    loadMethodCollectionFromConfig,
 )
import Database.Upload (DatabaseFormat (UnknownFormat), slugify)
import qualified Database.UploadedDatabase as UploadedDB
import Method.EditPlan (EditEffect, FactorEdit, Undo (..), inEffect, inverseOf, planEdit, restoreOf, seedLines, undoEffect, undoTarget)
import Method.Journal (LineKind (..), MethodLine (..), MethodOp (..), applyMethodOp, replayMethodJournal)
import Method.Types (MethodCollection)
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

-- | What a change did, and the journal line that records it.
data EditOutcome = EditOutcome
    { eoLine :: Int
    , eoEffect :: EditEffect
    }
    deriving (Eq, Show)

{- | Change a loaded collection of one's own, and record the change where a
later load finds it again.

The order is what makes an acknowledged change durable, as for a database:
plan the change against the collection in use (every refusal the caller can
act on comes from there), append its line (the commit point), then swap the
result in and drop what was built from the old one. A crash before the append
leaves nothing; after it, the next load replays the line, which is the answer
the caller was given.
-}
editMethodFactors :: DatabaseManager -> Text -> FactorEdit -> IO (Either MethodEditRefusal EditOutcome)
editMethodFactors manager name edit = withMVar (dmMethodEditLock manager) $ \() -> runExceptT $ do
    (home, collection) <- ExceptT (editable manager name)
    entries <- ExceptT (first EditRefused <$> readEntries home)
    (op, effect) <- except (first EditRefused (planEdit collection edit))
    changed <- except (first EditRefused (applyMethodOp collection op))
    line <- ExceptT (commitLine manager name home entries (MethodLine op Change) changed)
    pure (EditOutcome line effect)

-- | The home and the collection in use of a collection one may change.
editable :: DatabaseManager -> Text -> IO (Either MethodEditRefusal (FilePath, MethodCollection))
editable manager name = do
    available <- readTVarIO (dmAvailableMethods manager)
    loaded <- getMethodCollection manager name
    pure $ case (mcHome <$> M.lookup name available, loaded) of
        (Nothing, _) -> Left (CollectionNotFound name)
        (Just Nothing, _) -> Left (NotEditable name)
        (Just (Just _), Nothing) -> Left (CollectionNotLoaded name)
        (Just (Just home), Just collection) -> Right (home, collection)

{- | Append a line after the ones already read, then install what it
produced, and give the line's number. The swap adjusts, never inserts: a
collection unloaded while the line was written stays unloaded, and its next
load replays the line. The number is counted from the lines read under the
lock: 'appendEntry' drops a torn tail exactly as 'readEntries' does, so the
two agree.
-}
commitLine :: DatabaseManager -> Text -> FilePath -> [Entry MethodLine] -> MethodLine -> MethodCollection -> IO (Either MethodEditRefusal Int)
commitLine manager name home before line changed =
    appendEntry home line >>= \case
        Left err -> pure (Left (EditRefused err))
        Right () -> do
            atomically $ modifyTVar' (dmLoadedMethods manager) (M.adjust (const changed) name)
            clearMethodCachesFor manager (CollectionName name)
            pure (Right (length before + 1))

{- | Undo a line by writing its inverse. The journal never loses a line, so an
undo is a change like any other and can itself be undone, by naming its line.
-}
undoMethodEdit :: DatabaseManager -> Text -> Maybe Int -> IO (Either MethodEditRefusal EditOutcome)
undoMethodEdit manager name requested = withMVar (dmMethodEditLock manager) $ \() -> runExceptT $ do
    (home, collection) <- ExceptT (editable manager name)
    entries <- ExceptT (first EditRefused <$> readEntries home)
    target <- except (first EditRefused (undoTarget (map jeOp entries) requested))
    undone <- except (first EditRefused (lineAt target entries))
    inverse <-
        except (first EditRefused (inverseOf collection undone)) >>= \case
            UndoWith op -> pure op
            UndoSelector patch -> RestoreFactors . restoreOf patch <$> ExceptT (stateBefore manager name target entries)
    changed <- except (first EditRefused (applyMethodOp collection inverse))
    line <- ExceptT (commitLine manager name home entries (MethodLine inverse (Undoing target)) changed)
    pure (EditOutcome line (undoEffect inverse))

-- | What line @k@ of a journal did; 'undoTarget' has already said it exists.
lineAt :: Int -> [Entry MethodLine] -> Either Text MethodOp
lineAt k entries = case drop (k - 1) entries of
    e : _ | k >= 1 -> Right (mlOp (jeOp e))
    _ -> Left ("the journal has no line " <> T.pack (show k))

{- | The collection just before a line: its files read again and the lines
before it replayed. Only a selector's undo asks for it, since its inverse is
the values it replaced. ponytail: reads the files at each such undo; keep the
parsed source in memory if that wait shows.
-}
stateBefore :: DatabaseManager -> Text -> Int -> [Entry MethodLine] -> IO (Either MethodEditRefusal MethodCollection)
stateBefore manager name target entries = do
    available <- readTVarIO (dmAvailableMethods manager)
    case M.lookup name available of
        Nothing -> pure (Left (CollectionNotFound name))
        Just mc -> do
            parsed <- loadMethodCollectionFromConfig mc
            pure (first EditRefused (parsed >>= \(c, _) -> replayMethodJournal c (take (target - 1) entries)))

-- | One line of a collection's journal, as its history shows it.
data HistoryLine = HistoryLine
    { hlLine :: Int
    , hlAt :: Text
    , hlOp :: MethodOp
    , hlKind :: LineKind
    , hlInEffect :: Bool
    }
    deriving (Eq, Show)

{- | A collection's journal, line by line. A collection the configuration
declares has none, which is an empty history rather than a refusal: the
history reads the same for every collection.
-}
methodHistory :: DatabaseManager -> Text -> IO (Either MethodEditRefusal [HistoryLine])
methodHistory manager name = do
    available <- readTVarIO (dmAvailableMethods manager)
    case mcHome <$> M.lookup name available of
        Nothing -> pure (Left (CollectionNotFound name))
        Just Nothing -> pure (Right [])
        Just (Just home) -> fmap describe . first EditRefused <$> readEntries home
  where
    describe :: [Entry MethodLine] -> [HistoryLine]
    describe entries =
        [ HistoryLine i (jeAt e) (mlOp (jeOp e)) (mlKind (jeOp e)) effective
        | (i, e, effective) <- zip3 [1 ..] entries (inEffect (map jeOp entries))
        ]
