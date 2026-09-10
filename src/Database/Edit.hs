{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Database.Edit
Description : In-memory database editing primitives (copy, delete-by-selection)

Shared home for operations that produce a *new* loaded database from an
existing one without touching disk.

COPY is construction, not mutation. 'Database' is a pure, immutable value
(see "Types"), so duplicating it is just sharing the same persistent
structure under a second registry key: nothing observable can alias because
nothing is mutable. The only fresh allocation is the solver (it holds an
'MVar' factorization cache keyed by name); reusing the source solver would
let the two registry entries share a factorization cache, so the copy gets
its own.

What an edit does to the activity set itself is pure and lives in
"Database.Rebuild"; this module is the effectful half around it, which
database, under what reservation, written where, swapped into the registry
how.

Memory cost (copy): the copy keeps the source 'Database' alive for as long as
it is loaded. Structural sharing means we don't re-allocate the activity/flow
vectors, but a large database that would otherwise be unloaded stays resident
while any copy of it is loaded.
-}
module Database.Edit (
    copyDatabase,
    deriveDatabase,
    resolveDeleteSelection,
    DeleteSelection (..),
    DeleteRequest (..),
    DeleteOutcome (..),
    deleteActivitiesInDB,
    MutationOutcome (..),
    mutateUploadedDatabase,
    WriteVerb (..),
    WriteRefusal (..),
    WriteReport (..),
    writeActivities,
    EditReport (..),
    editExchanges,
    refusalMessage,
) where

import Control.Concurrent.STM (STM, atomically, modifyTVar', readTVar, readTVarIO)
import Control.Exception (SomeException, try)
import Control.Monad (when)
import qualified Data.IntSet as IS
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, makeAbsolute)
import System.FilePath ((</>))

import Config (DatabaseConfig (..))
import Database (Geographies)
import Database.Author (
    AuthorContext (..),
    AuthoredActivity,
    EditedActivity (..),
    ExchangeEdit (..),
    ResolvedInsert (..),
    applyExchangeEdits,
    validateAuthored,
 )
import Database.CrossLinking (buildIndexedDatabaseFromDB)
import Database.Journal (
    JournalOp (..),
    appendEvent,
    applyOp,
    journalPath,
    journalStamp,
    writeAppliedStamp,
 )
import qualified Database.Loader as Loader
import Database.Manager (
    DatabaseManager (..),
    DepLoadResult,
    LoadedDatabase (..),
    clearMethodMappingCacheForDb,
    editHome,
    getDatabase,
    getMergedSynonymDB,
    getMergedUnitConfig,
    loadDatabase,
    managerGeographies,
    publishLoaded,
    relinkDatabase,
    removeDatabase,
    solverFor,
    unloadDatabase,
    withReservedName,
 )
import Database.Rebuild (processKey, renderKey, resolveProcess)
import Database.Upload (DatabaseFormat (..), slugify)
import qualified Database.UploadedDatabase as UploadedDB
import Matrix (clearCachedSolver)
import Progress (ProgressLevel (..), reportProgress)
import qualified Search.BM25 as BM25
import Service (ActivityFilterCore (..), SearchFilter (..), activityMatches)
import Types (
    AllocationKey,
    Database (..),
    ProcessId,
    allocationKeyText,
    findProcessIdByActivityUUID,
    getActivity,
    initializeRuntimeFields,
    parseProcessRef,
    processRefText,
 )

{- | Copy a loaded database into the runtime registry under the slugified
@newName@.

Looks up the loaded source, builds an independent 'LoadedDatabase' (renamed
config + fresh solver: 'Database' is immutable, so the value itself is shared
safely) and inserts it into the loaded / available / indexed maps.

@newName@ is slugified to the same charset as uploaded databases: the copy is
registered as uploaded (see 'renameConfig'), and uploaded databases are later
deleted by name via 'removeDirectoryRecursive', so an unsanitised name (e.g.
@"../x"@ or @""@) would let the eventual delete escape the uploads directory.

Fails (Left) when the source is not loaded, when @newName@ slugifies to empty,
or when the name already designates a loaded, configured, or in-flight
database: a copy must never silently overwrite an existing entry. The name is
reserved atomically (in 'dmStagingDbs') across the slow solver build, so two
concurrent copies of the same name cannot both pass the existence check.
-}
copyDatabase :: DatabaseManager -> Text -> Text -> IO (Either Text ())
copyDatabase manager srcName newName = do
    let slug = slugify newName
    if T.null slug
        then pure $ Left $ "Invalid copy name (no usable characters): " <> newName
        else
            getDatabase manager srcName >>= \case
                Nothing -> pure $ Left $ "Database not loaded: " <> srcName
                Just src ->
                    withReservedName manager slug (nameIsFree manager slug) $ \() ->
                        registerCopy manager slug src

{- | Whether a name designates nothing yet: not loaded, not configured, and
not being written under by somebody else.

Read inside the transaction that claims it, which is what leaves no window
between finding a name free and taking it.
-}
nameIsFree :: DatabaseManager -> Text -> STM (Either Text ())
nameIsFree manager slug = do
    loadedDbs <- readTVar (dmLoadedDbs manager)
    availableDbs <- readTVar (dmAvailableDbs manager)
    stagingDbs <- readTVar (dmStagingDbs manager)
    pure $
        if M.member slug loadedDbs || M.member slug availableDbs || S.member slug stagingDbs
            then Left ("Database already exists: " <> slug)
            else Right ()

{- | Read a source's own files again under another allocation key, and
register the result under @newName@.

Not a copy. A copy shares the source's value as it stands, and the key is what
produced that value: it decides the inventory of every process the load
produces, so a re-keyed database has to be read from the files again. That is
a full load -- tens of seconds where a copy is instantaneous -- which is why
this hands back what a load hands back rather than what a copy does.

The home it writes is a copy's home: no data of its own, 'umDataPath' pointing
at the source's files and 'umSource' naming whose they are, plus the key,
without which a restart would rebuild the database as @declared@ under a name
promising otherwise. The source's journal is deliberately left behind: an edit
is recorded against a process the source's key produced, and under another key
that process need not exist. The load says so when there is one.

Asking for the key the source already reads under is refused before the load:
that result is the source, and it is the one duplicate no load can detect,
since a key dividing the same blocks a second time divides them just as well.
-}
deriveDatabase ::
    DatabaseManager ->
    Text ->
    Text ->
    AllocationKey ->
    IO (Either Text (LoadedDatabase, [DepLoadResult]))
deriveDatabase manager srcName newName key = do
    availableDbs <- readTVarIO (dmAvailableDbs manager)
    case M.lookup srcName availableDbs of
        Nothing -> pure (Left ("Database not found: " <> srcName))
        Just srcConfig
            | T.null slug -> pure (Left ("Invalid name (no usable characters): " <> newName))
            | key == dcAllocation srcConfig ->
                pure . Left $
                    srcName
                        <> " is already read under "
                        <> allocationKeyText key
                        <> ": the result would be that database under another name"
            | otherwise ->
                withReservedName manager slug (nameIsFree manager slug) $ \() ->
                    loadDerived manager slug srcConfig key
  where
    slug :: Text
    slug = slugify newName

{- | Give the derived database a home, register it, load it, and keep it only
if the key actually divided something.

A key that divided no block leaves a database identical to its source, under a
name saying its shares were recomputed, and costing a second database in
memory and a second matrix cache on disk. There is no way to know that without
loading -- what a property divides depends on what each block states -- so the
load happens and is then thrown away, home and cache with it.
-}
loadDerived ::
    DatabaseManager ->
    Text ->
    DatabaseConfig ->
    AllocationKey ->
    IO (Either Text (LoadedDatabase, [DepLoadResult]))
loadDerived manager slug srcConfig key =
    recordDerived slug srcConfig key >>= \case
        Left err -> pure (Left err)
        Right derivedConfig -> do
            atomically $ modifyTVar' (dmAvailableDbs manager) (M.insert slug derivedConfig)
            loadDatabase manager slug >>= \case
                Left err -> Left err <$ discardDerived manager slug
                Right (loaded, deps) -> case dividedRefusal key (dcName srcConfig) (ldDatabase loaded) of
                    Nothing -> pure (Right (loaded, deps))
                    Just refusal -> Left refusal <$ discardDerived manager slug

{- | Why the key divided nothing worth keeping, when it did.

The coproducts a key divided share an activity, so a block it divided is
several processes under one activity and a block it could not is the single
process it came in as. Counting the first is the only measure of what a key did
to a database, and both numbers are named: @0 of 4087@ says the source carries
blocks and none of them could be weighed, where @0 of 0@ would say it carries
none at all.
-}
dividedRefusal :: AllocationKey -> Text -> Database -> Maybe Text
dividedRefusal key srcName db
    | divided > 0 = Nothing
    | otherwise =
        Just $
            allocationKeyText key
                <> " divided 0 of the "
                <> T.pack (show (M.size blocks))
                <> " blocks of "
                <> srcName
                <> ": the result would be that database under another name"
  where
    blocks :: M.Map UUID.UUID (NonEmpty ProcessId)
    blocks = dbActivityUUIDIndex db

    divided :: Int
    divided = length (filter ((> 1) . NE.length) (M.elems blocks))

{- | Undo a derivation that will not be kept: unload it, then take its home
and its cache with 'removeDatabase', which refuses a loaded database.

The unload is skipped when the load is what failed, since nothing was
registered to unload and the refusal it answers would be a second warning
saying so beside the real one.

What is left behind if a step fails is a registry entry or a directory nobody
asked for, so both are reported: the caller is already on its way to returning
the refusal that brought us here.
-}
discardDerived :: DatabaseManager -> Text -> IO ()
discardDerived manager slug = do
    loaded <- M.member slug <$> readTVarIO (dmLoadedDbs manager)
    when loaded $ unloadDatabase manager slug >>= warnLeftover "unload"
    removeDatabase manager slug >>= warnLeftover "delete"
  where
    warnLeftover :: Text -> Either Text () -> IO ()
    warnLeftover step =
        either
            (\err -> reportProgress Warning . T.unpack $ "could not " <> step <> " " <> slug <> ": " <> err)
            pure

{- | Write the derived database's home and return the config it loads from.

The path is the source's own, made absolute for the same reason a copy's is:
it is read back from a home of its own, where a path relative to the working
directory would mean somewhere else.
-}
recordDerived :: Text -> DatabaseConfig -> AllocationKey -> IO (Either Text DatabaseConfig)
recordDerived slug srcConfig key = do
    written <- try $ do
        uploadsDir <- UploadedDB.getDatabaseUploadsDir
        let home = uploadsDir </> T.unpack slug
        createDirectoryIfMissing True home
        sourcePath <- makeAbsolute (dcPath srcConfig)
        hasJournal <- doesFileExist (journalPath (uploadsDir </> T.unpack (dcName srcConfig)))
        when hasJournal . reportProgress Warning . T.unpack $
            "the edits recorded on "
                <> dcName srcConfig
                <> " stay behind: each names a process its key produced, and "
                <> slug
                <> " reads the same files under another one"
        UploadedDB.writeUploadMeta
            home
            UploadedDB.UploadMeta
                { UploadedDB.umVersion = UploadedDB.metaVersion
                , UploadedDB.umDisplayName = slug
                , UploadedDB.umDescription = dcDescription srcConfig
                , UploadedDB.umFormat = fromMaybe UnknownFormat (dcFormat srcConfig)
                , UploadedDB.umDataPath = sourcePath
                , UploadedDB.umDepends = dcDepends srcConfig
                , UploadedDB.umSource = Just (dcName srcConfig)
                , UploadedDB.umAllocation = key
                }
        pure
            srcConfig
                { dcName = slug
                , dcDisplayName = slug
                , dcPath = sourcePath
                , dcLoad = False
                , dcDefault = False
                , dcIsUploaded = True
                , dcDeletable = True
                , dcAllocation = key
                , dcSource = Just (dcName srcConfig)
                }
    pure $ case written of
        Right config -> Right config
        Left (err :: SomeException) ->
            Left $ "could not record the derived database " <> slug <> ": " <> T.pack (show err)

{- | Build the copy's solver/index and insert it under @slug@. Caller holds the
'dmStagingDbs' reservation for @slug@.

The home is written before anything is registered: a copy that cannot be
recorded on disk would work until the restart and then be gone, which is the
kind of quiet loss this ordering exists to refuse. Nothing after the write can
fail into an inconsistent state: the registry swap is a pure STM commit.
-}
registerCopy :: DatabaseManager -> Text -> LoadedDatabase -> IO (Either Text ())
registerCopy manager slug src =
    recordCopy slug src >>= \case
        Left err -> pure (Left err)
        Right () -> do
            let copiedDb = ldDatabase src
                newConfig = renameConfig slug (ldConfig src)
            -- Fresh solver: a distinct name keys a distinct factorization cache.
            solver <- solverFor slug copiedDb
            synonymDB <- getMergedSynonymDB manager
            let copied =
                    LoadedDatabase
                        { ldDatabase = copiedDb
                        , ldSharedSolver = solver
                        , ldConfig = newConfig
                        }
                indexedDb = buildIndexedDatabaseFromDB slug synonymDB copiedDb
            atomically $ do
                publishLoaded manager slug copied indexedDb
                modifyTVar' (dmAvailableDbs manager) (M.insert slug newConfig)
            clearMethodMappingCacheForDb manager slug
            pure (Right ())

{- | Give the copy a home: a directory of its own holding the @meta.toml@ that
describes it and, once it is edited, its journal.

It is written now rather than at the first edit, so a copy survives a restart
from the moment it exists. The home holds no data: @dataPath@ points at the
source's own files, which the copy reads and never writes, and @source@ names
whose they are so a delete can refuse to take files a copy still needs. That
is what makes a copy cost a directory rather than a second database.

The copy forks from the source as it stands, not as it was uploaded: what was
copied is the source's value /after/ its edits, but its files never carry
them, so the copy starts from a snapshot of the source's journal and a load
replays the source's edits before its own. Edits the source makes later
belong to the source alone.
-}
recordCopy :: Text -> LoadedDatabase -> IO (Either Text ())
recordCopy slug src = do
    written <- try $ do
        uploadsDir <- UploadedDB.getDatabaseUploadsDir
        let home = uploadsDir </> T.unpack slug
            config = ldConfig src
            srcJournal = journalPath (uploadsDir </> T.unpack (dcName config))
        createDirectoryIfMissing True home
        -- The source's path may be relative to the working directory, while
        -- the copy's is read back from its own home; 'uploadMetaToConfig'
        -- keeps an absolute path as it is.
        sourcePath <- makeAbsolute (dcPath config)
        hasJournal <- doesFileExist srcJournal
        when hasJournal $ copyFile srcJournal (journalPath home)
        UploadedDB.writeUploadMeta
            home
            UploadedDB.UploadMeta
                { UploadedDB.umVersion = UploadedDB.metaVersion
                , UploadedDB.umDisplayName = slug
                , UploadedDB.umDescription = dcDescription config
                , UploadedDB.umFormat = fromMaybe UnknownFormat (dcFormat config)
                , UploadedDB.umDataPath = sourcePath
                , UploadedDB.umDepends = dbDependsOn (ldDatabase src)
                , UploadedDB.umSource = Just (dcName config)
                , -- A copy holds the source's value as it stands, which the
                  -- source's own key produced.
                  UploadedDB.umAllocation = dcAllocation config
                }
    pure $ case written of
        Right () -> Right ()
        Left (err :: SomeException) ->
            Left $ "could not record the copy " <> slug <> ": " <> T.pack (show err)

{- | Rename a config for the copy: new internal name, derived display name, and
forced deletable/uploaded so the copy can be removed again via the normal
delete path (the source may be a TOML-pinned, non-deletable database).

The source it names is the one 'recordCopy' writes to the copy's home, so a
listing says the same thing before and after the restart that reads it back.
-}
renameConfig :: Text -> DatabaseConfig -> DatabaseConfig
renameConfig newName cfg =
    cfg
        { dcName = newName
        , dcDisplayName = newName
        , dcIsUploaded = True
        , dcDeletable = True
        , dcSource = Just (dcName cfg)
        }

-- ---------------------------------------------------------------------------
-- Writing authored activities
-- ---------------------------------------------------------------------------

{- | Which way a batch is meant to land: added to the collection, or written
over the one process the caller names.
-}
data WriteVerb = CreateActivities | ReplaceActivity Text

{- | Why a write did not happen, in the terms its caller has to answer in.

Each constructor exists because a different response is owed: an HTTP client
needs 404 apart from 409 apart from 400, and someone at a terminal needs to be
told which of "there is no such database", "that one is not yours to write"
and "your file has four problems" happened. A single error string would force
both to guess by reading it.
-}
data WriteRefusal
    = NotLoaded Text
    | NotWritable Text
    | Malformed [Text]
    | AlreadyPresent [Text]
    | NotPresent [Text]
    | WriteFailed Text

-- | What a write produced.
data WriteReport = WriteReport
    { wrWritten :: [Text]
    , wrPersisted :: Bool
    , wrWarnings :: [Text]
    }

{- | Validate a batch of authored activities against a loaded database and, if
nothing is wrong with it, write it.

This is the whole of authoring above the primitives, shared by the HTTP
endpoints and the command line so the two cannot drift on what is allowed.
Refusals are classified rather than stringly-typed, because the two callers
owe their users different answers to the same refusal.

A database the engine reads from its configuration is refused: that is
background data the whole installation shares, and a copy or an upload is
where authoring belongs.
-}
writeActivities ::
    DatabaseManager ->
    Text ->
    WriteVerb ->
    [AuthoredActivity] ->
    IO (Either WriteRefusal WriteReport)
writeActivities _ _ _ [] =
    -- Committing re-serializes the whole database and rebuilds its solver;
    -- an empty batch would pay all of that to write nothing.
    pure (Left (Malformed ["The batch is empty: there is nothing to write."]))
writeActivities manager dbName verb authored =
    getDatabase manager dbName >>= \case
        Nothing -> pure (Left (NotLoaded dbName))
        Just loaded
            | not (dcIsUploaded (ldConfig loaded)) -> pure (Left (NotWritable dbName))
            | otherwise -> do
                ctx <- authorContext manager (ldDatabase loaded)
                case validateAuthored ctx authored of
                    Left errs -> pure (Left (Malformed errs))
                    Right (resolved, warnings) ->
                        case presenceRefusal (ldDatabase loaded) verb resolved of
                            Just refusal -> pure (Left refusal)
                            Nothing -> either (pure . Left) (commit resolved warnings) (operation resolved)
  where
    {- The operation is what gets journalled and what gets applied - one
    description, so the record and the result cannot disagree. Its identities
    are the canonical ones minted here, which is also what a replay compares
    against. -}
    operation resolved = case (verb, zip authored (map (renderKey . riKey) resolved)) of
        -- Refused rather than journalled: a line recording that nothing
        -- happened would be replayed forever for nothing.
        (CreateActivities, []) -> Left (Malformed ["a create writes at least one activity"])
        (CreateActivities, written) -> Right (Created authored (map snd written))
        (ReplaceActivity _, [(activity, key)]) -> Right (Replaced key activity)
        (ReplaceActivity target, _) ->
            Left (Malformed ["a replace writes exactly one activity over " <> target])
    -- Everything above judged a snapshot taken before the reservation;
    -- 'mutateReserved' re-reads the database under it and validates the
    -- operation again against what is actually there, so a batch overtaken by
    -- a concurrent edit is refused rather than written with a supplier link
    -- that no longer resolves. In that rare interleaving the refusal degrades
    -- from a classified status to a 'WriteFailed' message, never to a dangling
    -- link. Identity minting is pure, so the keys cannot differ between the
    -- two runs.
    commit resolved warnings op = do
        outcome <- mutateUploadedDatabase manager dbName op
        pure $ case outcome of
            Left err -> Left (WriteFailed err)
            Right done ->
                Right
                    WriteReport
                        { wrWritten = map (renderKey . riKey) resolved
                        , wrPersisted = moPersisted done
                        , wrWarnings = warnings <> moWarnings done
                        }

-- | What an inventory edit produced, counted the way the request stated it.
data EditReport = EditReport
    { erRemoved :: [Int]
    , erAmountsSet :: [Int]
    , erAdded :: Int
    , erPersisted :: Bool
    , erWarnings :: [Text]
    }

{- | Change the inventory of one activity a loaded database already holds.

The sibling of 'writeActivities', and refused for the same reasons in the same
order, but not a third 'WriteVerb': a write states a whole activity, an edit
states changes to one, and folding them into one verb would mean a payload
that is half ignored either way.

What it exists for is the activity a write cannot reach: one that came in from
a database file, whose identity no description mints. Here the identity is the
caller's target, never re-derived, and everything the edits do not name stays
as it is.
-}
editExchanges ::
    DatabaseManager ->
    Text ->
    Text ->
    [ExchangeEdit] ->
    IO (Either WriteRefusal EditReport)
editExchanges _ _ _ [] =
    -- Committing re-serializes the database and rebuilds its solver; an empty
    -- edit would pay all of that to change nothing.
    pure (Left (Malformed ["There is nothing to change: the edit names no exchange."]))
editExchanges manager dbName target edits =
    getDatabase manager dbName >>= \case
        Nothing -> pure (Left (NotLoaded dbName))
        Just loaded
            | not (dcIsUploaded (ldConfig loaded)) -> pure (Left (NotWritable dbName))
            | otherwise -> case addressed (ldDatabase loaded) of
                Nothing -> pure (Left (NotPresent [target]))
                Just (key, activity) -> do
                    ctx <- authorContext manager (ldDatabase loaded)
                    case applyExchangeEdits ctx edits activity of
                        Left errs -> pure (Left (Malformed errs))
                        Right edited -> commit key (eaMatched edited) (eaWarnings edited)
  where
    -- The identity is recorded canonically even when the caller addressed the
    -- activity by its bare UUID, so the journal keeps naming the same process
    -- if that activity ever gains a second product.
    addressed db = do
        pid <- either (const Nothing) Just (resolveProcess db target)
        key <- either (const Nothing) Just (processKey db pid)
        activity <- getActivity db pid
        pure (renderKey key, activity)
    commit key matched warnings = do
        outcome <- mutateUploadedDatabase manager dbName (Edited key (zip edits matched))
        pure $ case outcome of
            Left err -> Left (WriteFailed err)
            Right done ->
                Right
                    EditReport
                        { erRemoved = [n | (RemoveExchange _, n) <- zip edits matched]
                        , erAmountsSet = [n | (SetAmount _ _, n) <- zip edits matched]
                        , erAdded = length [() | AddExchange _ <- edits]
                        , erPersisted = moPersisted done
                        , erWarnings = warnings <> moWarnings done
                        }

{- | The two refusals only a verb can name: creating over a process that is
already there, and rewriting one that is not. Checked before the mutation so
the caller learns which happened rather than reading it out of a message.

'ReplaceActivity' also checks that the body describes the process the caller
addressed. Identity is minted from the name, location, product and unit, so a
body that mints elsewhere would silently become a second row.
-}
presenceRefusal :: Database -> WriteVerb -> [ResolvedInsert] -> Maybe WriteRefusal
presenceRefusal db verb resolved = case verb of
    CreateActivities -> case filter present keys of
        [] -> Nothing
        clashes -> Just (AlreadyPresent (map renderKey clashes))
    -- The mismatch is checked before presence on purpose: a body that mints
    -- elsewhere describes a process the database may well not have, and
    -- answering "no such activity" would send the author looking for the wrong
    -- mistake.
    ReplaceActivity target -> case filter ((/= canonicalTarget db target) . renderKey) keys of
        elsewhere@(_ : _) ->
            Just . Malformed $
                [ "This activity's identity is "
                    <> renderKey wrong
                    <> ", not "
                    <> target
                    <> ". Identity comes from the name, location, product and unit, so\
                       \ writing this body here would address a different activity."
                | wrong <- elsewhere
                ]
        [] -> case filter (not . present) keys of
            [] -> Nothing
            absent -> Just (NotPresent (map renderKey absent))
  where
    keys = map riKey resolved
    present key = M.member key (dbProcessIdLookup db)

{- | The PUT target in the currency 'renderKey' speaks. A process is addressed
by the canonical @activityUUID_productUUID@ pair, or by the bare activity UUID
the read endpoints also accept, so the handle a caller got from a read works
here too. A target that resolves to nothing is kept as sent: the presence
check owns that refusal.
-}
canonicalTarget :: Database -> Text -> Text
canonicalTarget db target = case parseProcessRef target of
    Just ref -> processRefText ref
    Nothing -> fromMaybe target $ do
        actUUID <- UUID.fromText target
        pid <- findProcessIdByActivityUUID db actUUID
        renderKey <$> dbProcessIdTable db V.!? fromIntegral pid

-- | The loaded databases a database draws suppliers from.
loadedDependencies :: DatabaseManager -> Database -> IO [Database]
loadedDependencies manager db = do
    loadedDbs <- readTVarIO (dmLoadedDbs manager)
    pure [ldDatabase ld | name <- dbDependsOn db, Just ld <- [M.lookup name loadedDbs]]

{- | Plain-language rendering, for callers with nowhere to put a status code.
An HTTP caller maps the constructors to codes instead.
-}
refusalMessage :: WriteRefusal -> Text
refusalMessage = \case
    NotLoaded name -> "Database not loaded: " <> name
    NotWritable name ->
        name
            <> " is a database this engine reads from its configuration, so it is shared\
               \ background data rather than yours to write. Copy it, or upload a database\
               \ of your own, and author there."
    Malformed errs -> T.intercalate "\n" errs
    AlreadyPresent keys -> "Already in this database: " <> T.intercalate ", " keys
    NotPresent keys -> "Not in this database: " <> T.intercalate ", " keys
    WriteFailed err -> err

-- ---------------------------------------------------------------------------
-- Persisting an edit
-- ---------------------------------------------------------------------------

{- | What a mutation did.

'moPersisted' is the honest half: an edit to a configured (TOML) database
lives in memory only and is gone at the next unload, and the caller has to be
able to say so rather than let the user assume otherwise.
-}
data MutationOutcome = MutationOutcome
    { moPersisted :: Bool
    , moWarnings :: [Text]
    }

{- | Apply an edit to a loaded database and record it where a later load will
find it again.

The order is what makes an acknowledged edit durable:

  1. refuse while another loaded database depends on this one (a rebuild
     renumbers processes, and the dependent's cross-database links would
     resolve to the wrong rows or silently drop at solve time);
  2. apply the operation to the loaded value, which is where every refusal
     the author can act on comes from;
  3. append it to the database's journal, the commit point: it is the last
     step that can fail while nothing has been claimed yet;
  4. install the result - fresh solver, registry swap, relink;
  5. save the matrix cache, then stamp it with the journal it now holds.

A crash before step 3 leaves nothing behind. A crash after it leaves an edit
in the journal, which the next load replays: the same answer the caller was
given. A crash between 4 and 5 leaves a cache that predates the journal, and
the missing stamp is what makes the next load read the sources again rather
than trust it.

The database's own files are never rewritten. They are what their author
uploaded, and only some formats could be written back without moving every
process identity in them ("Database.Journal" says why).
-}
mutateUploadedDatabase ::
    DatabaseManager ->
    Text ->
    JournalOp ->
    IO (Either Text MutationOutcome)
mutateUploadedDatabase manager dbName op = do
    -- Two concurrent edits of the same database would each apply to the value
    -- the other started from, and both would land in the journal. Reserve the
    -- name (the same reservation copy and staging use) and refuse the second
    -- rather than queue it: edits are interactive and rare.
    withReservedName manager dbName notAlreadyRunning $ \() ->
        mutateReserved manager dbName op
  where
    notAlreadyRunning = do
        staging <- readTVar (dmStagingDbs manager)
        pure $
            if S.member dbName staging
                then Left ("An edit of " <> dbName <> " is already in progress. Retry when it finishes.")
                else Right ()

-- | The mutation proper. The caller holds the 'dmStagingDbs' reservation.
mutateReserved :: DatabaseManager -> Text -> JournalOp -> IO (Either Text MutationOutcome)
mutateReserved manager dbName op =
    getDatabase manager dbName >>= \case
        Nothing -> pure $ Left $ "Database not loaded: " <> dbName
        Just loaded -> do
            loadedDbs <- readTVarIO (dmLoadedDbs manager)
            ctx <- authorContext manager (ldDatabase loaded)
            case guardDependents dbName loadedDbs *> applyOp ctx op of
                Left err -> pure (Left err)
                Right edited -> do
                    home <- editHome (ldConfig loaded)
                    recorded <- traverse (`appendEvent` op) home
                    case sequence recorded of
                        Left err -> pure (Left err)
                        Right _ -> Right <$> commitMutation manager dbName loaded edited home

{- | Everything an edit is judged against: the database it applies to, the
dependencies its suppliers may live in, and the units its amounts are stated
in. The same context serves a live edit and a replay, which is what keeps the
two from disagreeing about what is valid.
-}
authorContext :: DatabaseManager -> Database -> IO AuthorContext
authorContext manager db = do
    deps <- loadedDependencies manager db
    unitConfig <- getMergedUnitConfig manager
    pure AuthorContext{acDb = db, acDeps = deps, acUnitConfig = unitConfig}

{- | Refuse an edit while another loaded database depends on this one. Shared
with the delete path, which has the same reason: rebuilding renumbers every
process, and a dependent's cross-database links would then resolve elsewhere
or drop silently at solve time.
-}
guardDependents :: Text -> M.Map Text LoadedDatabase -> Either Text ()
guardDependents dbName loadedDbs = case dependents of
    [] -> Right ()
    names ->
        Left $
            "Cannot edit "
                <> dbName
                <> ": still required by "
                <> T.intercalate ", " names
                <> ". Unload dependents first."
  where
    dependents =
        [ name
        | (name, ld) <- M.toList loadedDbs
        , name /= dbName
        , dbName `elem` dbDependsOn (ldDatabase ld)
        ]

{- | Install the edited database: rebuilt runtime indexes, a fresh solver, the
registry swap, and - for a database that has a journal - a matrix cache the
next load can trust.
-}
commitMutation ::
    DatabaseManager ->
    Text ->
    LoadedDatabase ->
    Database ->
    Maybe FilePath ->
    IO MutationOutcome
commitMutation manager dbName loaded edited home = do
    synonymDB <- getMergedSynonymDB manager
    let withRuntime = BM25.addBM25Index (initializeRuntimeFields edited synonymDB)
    -- The solver cached under this name has the pre-edit dimensions; drain and
    -- destroy it before installing the rebuilt one, as the delete path does.
    clearCachedSolver dbName
    solver <- solverFor dbName withRuntime
    let loaded' = loaded{ldDatabase = withRuntime, ldSharedSolver = solver}
        indexedDb = buildIndexedDatabaseFromDB dbName synonymDB withRuntime
    atomically $ publishLoaded manager dbName loaded' indexedDb
    clearMethodMappingCacheForDb manager dbName
    warnings <- case home of
        -- The transient path must not relink: 'relinkDatabase' saves the
        -- matrix cache when links change, which would half-persist an edit
        -- the caller is being told is memory-only. Say what that costs.
        Nothing ->
            pure
                [ "the edit cleared the links to "
                    <> T.intercalate ", " (dbDependsOn edited)
                    <> "; they return at the next load, and cross-database totals undercount until then"
                | not (null (dbDependsOn edited))
                , null (dbCrossDBLinks edited)
                ]
        Just dir -> do
            -- Rebuilding cleared the cross-database links; rebuild them against
            -- the current dependency set before the cache records the result.
            relinked <- relinkDatabase manager dbName
            saved <- getDatabase manager dbName
            mapM_ (Loader.saveCachedDatabaseWithMatrices dbName (dcPath (ldConfig loaded)) . ldDatabase) saved
            journalStamp dir >>= writeAppliedStamp dir
            pure (either (\err -> ["the edit is saved, but relinking failed: " <> err]) (const []) relinked)
    pure
        MutationOutcome
            { moPersisted = isJust home
            , moWarnings = warnings
            }

-- ---------------------------------------------------------------------------
-- Selection resolver
-- ---------------------------------------------------------------------------

{- | A deletion request: the whole set matched by a filter, plus explicit
adjustments. @dsKeep@ rescues individuals the filter matched (checkbox
unticked); @dsExtra@ adds individuals the filter missed (checkbox ticked on a
row outside the current filter). 'ProcessId' lists, never paginated.
-}
data DeleteSelection = DeleteSelection
    { dsFiltered :: [ProcessId]
    -- ^ the full filtered set (pagination ignored)
    , dsKeep :: [ProcessId]
    -- ^ individuals to spare from deletion
    , dsExtra :: [ProcessId]
    -- ^ individuals to add to deletion
    }

{- | Final delete set = (filtered ∪ extra) \\ keep. Deduplicated; @keep@ wins
over both @filtered@ and @extra@ so an unticked checkbox always spares a row.
Order is not significant: the result is consumed as a set by
'deleteActivities'.
-}
resolveDeleteSelection :: DeleteSelection -> [ProcessId]
resolveDeleteSelection sel =
    map fromIntegral . IS.toList $
        IS.difference
            (IS.union (toSet (dsFiltered sel)) (toSet (dsExtra sel)))
            (toSet (dsKeep sel))
  where
    toSet = IS.fromList . map fromIntegral

{- | Resolve a filter to the full set of matching 'ProcessId's, ignoring
pagination.

It is 'Service.activityMatches', the same funnel the listing and its tab
counters go through, with pagination and ordering left off: that is what makes
the UI's "delete the whole filtered set" button remove exactly the rows the
user saw, no more and no fewer. Answering the question a second way here would
mean every tier added to the search had to be remembered twice, and the day one
was not, the button would delete rows the table never showed.

Order is irrelevant: the result is consumed as a set.
-}
filteredProcessIds ::
    Geographies ->
    Database ->
    Maybe Text -> -- name
    Maybe Text -> -- location
    Maybe Text -> -- product
    [(Text, Text, Bool)] -> -- classification (system, value, isExact)
    Bool -> -- exact name match
    [ProcessId]
filteredProcessIds geographies db nameP geoP prodP classFilters exactMatch =
    map fst (activityMatches geographies db (SearchFilter core exactMatch))
  where
    core :: ActivityFilterCore
    core =
        ActivityFilterCore
            { afcName = nameP
            , afcLocation = geoP
            , afcProduct = prodP
            , afcClassifications = classFilters
            , afcLimit = Nothing
            , afcOffset = Nothing
            , afcSort = Nothing
            , afcOrder = Nothing
            }

-- ---------------------------------------------------------------------------
-- Effectful entry point (registry swap)
-- ---------------------------------------------------------------------------

{- | One delete-by-selection request against a loaded database. Two exclusive
selection modes: the filter fields select the whole matching set (@drIds =
Nothing@), or @drIds@ names the set exactly: every filter field must then be
absent, so a request can never silently mean two things. @drKeep@ spares and
@drExtra@ adds explicit process ids in both modes.
-}
data DeleteRequest = DeleteRequest
    { drName :: Maybe Text
    , drLocation :: Maybe Text
    , drProduct :: Maybe Text
    , drClassifications :: [(Text, Text, Bool)]
    , drExactName :: Bool
    , drKeep :: [Text]
    , drExtra :: [Text]
    , drIds :: Maybe [Text]
    }

{- | What a delete-by-selection request did: how many activities went, and
whether the database it edited keeps the change past an unload.
-}
data DeleteOutcome = DeleteOutcome
    { doRemoved :: Int
    , doPersisted :: Bool
    , doWarnings :: [Text]
    }

{- | Delete a selection from a loaded database, in place under the same name.

Resolves the selection ('drIds' verbatim, else the filter's full matching
set), resolves the explicit keep/extra process-id strings, applies the
adjustments, then hands the deletion to 'mutateUploadedDatabase', which
rebuilds, rewrites the sources of a database that owns them, and swaps the
result in. Fails (Left) when the database is not loaded, an ids/keep/extra id
is unknown, 'drIds' is combined with filter fields, the format cannot record
what the edit produced, or the rebuild reports an inconsistency.

A deletion from a database with its own files now outlives the process: the
old behaviour left the sources and the matrix cache holding the pre-delete
set, so a restart quietly resurrected every removed activity. A configured
(TOML) database still edits in memory only, and 'doPersisted' says so.
-}
deleteActivitiesInDB ::
    DatabaseManager ->
    Text -> -- database name
    DeleteRequest ->
    IO (Either Text DeleteOutcome)
deleteActivitiesInDB manager dbName DeleteRequest{drName = nameP, drLocation = geoP, drProduct = prodP, drClassifications = classFilters, drExactName = exactMatch, drKeep = keep, drExtra = extra, drIds = mIds} =
    getDatabase manager dbName >>= \case
        Nothing -> pure $ Left $ "Database not loaded: " <> dbName
        Just loaded -> do
            let db = ldDatabase loaded
                geographies = managerGeographies manager
                -- The two selection modes are exclusive: ids name the set
                -- verbatim, filters compute it. A request carrying both is
                -- ambiguous, so it is refused rather than guessed at: exact
                -- included, since it only modifies name/classification matching.
                hasFilter =
                    any isJust [nameP, geoP, prodP] || not (null classFilters) || exactMatch
                selectionE = case mIds of
                    Just ids
                        | hasFilter -> Left "ids cannot be combined with name/location/product/classification/exact filters"
                        | otherwise -> traverse (resolveProcess db) ids
                    Nothing -> Right (filteredProcessIds geographies db nameP geoP prodP classFilters exactMatch)
            case (,,) <$> traverse (resolveProcess db) keep <*> traverse (resolveProcess db) extra <*> selectionE of
                Left err -> pure $ Left err
                Right (keepPids, extraPids, filtered) -> do
                    let toDelete =
                            resolveDeleteSelection
                                DeleteSelection{dsFiltered = filtered, dsKeep = keepPids, dsExtra = extraPids}
                    case traverse (fmap renderKey . processKey db) toDelete of
                        Left err -> pure (Left err)
                        -- A filter that matched nothing changes nothing: no
                        -- rebuild, and no line in the journal saying an empty
                        -- deletion happened.
                        Right [] -> pure (Right (removed 0 (dcIsUploaded (ldConfig loaded)) []))
                        Right targets -> do
                            outcome <- mutateUploadedDatabase manager dbName (Deleted targets)
                            pure $ flip fmap outcome $ \done ->
                                removed (length targets) (moPersisted done) (moWarnings done)
  where
    removed count persisted warnings =
        DeleteOutcome{doRemoved = count, doPersisted = persisted, doWarnings = warnings}
