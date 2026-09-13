{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Database.Rebuild
Description : How a changed activity set becomes a database again (pure)

Editing a database is two things: deciding what changes, which is effectful
(the registry, the files, the solver - "Database.Edit"), and turning the
changed activity set back into a 'Database', which is not. This module is the
second half, and nothing here touches the manager or the disk.

The split is what lets anything below the effectful editor apply an edit.
Every function is @Database -> Either Text Database@ or smaller, so the same
primitives serve a live mutation and a replay of edits recorded earlier.

The rebuild reuses the exact pure builders that back a freshly-loaded database
('buildInterningTables' / 'buildTechTriples' / 'buildBioTriples' /
'buildProductIndex' / 'buildIndexesWithProcessIds'), so an edited database is
indistinguishable from one that was loaded that way to begin with.
-}
module Database.Rebuild (
    -- * Delete
    deleteActivities,
    deleteActivitiesWith,

    -- * Insert and replace
    insertActivities,
    replaceActivities,

    -- * The rebuild itself
    rebuildFromActivities,

    -- * Identity
    renderKey,
    processKey,
    resolveProcess,
) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector as V

import Database (buildIndexesWithProcessIds, buildProductIndex)
import Database.Author (ResolvedInsert (..))
import Database.MatrixBuild (
    InterningTables (..),
    buildBioTriples,
    buildInterningTables,
    buildSupplierRefUnits,
    buildTechTriples,
    collectBioFlowOrder,
 )
import Types (
    Activity (..),
    BiosphereFlow (..),
    Database (..),
    Exchange (..),
    ProcessId,
    ProcessRef (..),
    TechnosphereFlow (..),
    UUID,
    findProcessId,
    findProcessIdByActivityUUID,
    parseProcessRef,
    processRefText,
 )
import UnitConversion (UnitConfig, defaultUnitConfig)

-- ---------------------------------------------------------------------------
-- Delete
-- ---------------------------------------------------------------------------

{- | Remove a set of activities (by 'ProcessId') and rebuild every dependent
structure. Uses 'defaultUnitConfig'; effectful call sites that carry a merged
unit config (the only place where non-SI conversions matter) should call
'deleteActivitiesWith' instead so a coefficient is never silently dropped on
an unknown-unit conversion.
-}
deleteActivities :: [ProcessId] -> Database -> Either Text Database
deleteActivities = deleteActivitiesWith defaultUnitConfig

{- | 'deleteActivities' with an explicit 'UnitConfig'.

Steps, all pure:

  1. Resolve the delete set to the @(activityUUID, productUUID)@ keys it
     occupies in 'dbProcessIdTable', validating that every requested
     'ProcessId' exists (no silent skip).
  2. Drop those keys; for every surviving activity, UNLINK any technosphere /
     waste exchange whose @(activityLink, flow)@ pointed at a deleted key -
     clear its activity link and clear the stale process link.
  3. Rebuild interning tables, indexes, matrices and the product index from
     the surviving activity map via the shared loader builders.

Fails (Left) when an out-of-range 'ProcessId' is requested, when the result
would be empty, or when matrix construction reports an inconsistency (e.g. an
unknown unit conversion under the given config).
-}
deleteActivitiesWith :: UnitConfig -> [ProcessId] -> Database -> Either Text Database
deleteActivitiesWith unitConfig pids db = do
    deletedKeys <- resolveDeleteKeys db pids
    let survivors = surviving db deletedKeys
        survivingKeys = M.keysSet survivors
        unlinkedMap = M.map (unlinkActivity survivingKeys) survivors
    if M.null unlinkedMap
        then Left "Refusing to delete: the result would have no activities"
        else rebuildFromActivities unitConfig db unlinkedMap

{- | Resolve each requested 'ProcessId' to its @(activityUUID, productUUID)@ key.
Every id must be in range - an out-of-range id is a caller error, surfaced as
'Left' rather than silently ignored. The result is a 'Set' so membership tests
during unlinking are @O(log n)@.
-}
resolveDeleteKeys :: Database -> [ProcessId] -> Either Text (S.Set (UUID, UUID))
resolveDeleteKeys db = fmap S.fromList . traverse (processKey db)

-- | The activity map keyed by @(activityUUID, productUUID)@, minus the deleted keys.
surviving :: Database -> S.Set (UUID, UUID) -> M.Map (UUID, UUID) Activity
surviving db deletedKeys =
    M.fromList
        [ (key, dbActivities db V.! i)
        | i <- [0 .. V.length (dbActivities db) - 1]
        , let key = dbProcessIdTable db V.! i
        , not (S.member key deletedKeys)
        ]

-- | The database's activities, keyed the way a rebuild takes them.
activityMap :: Database -> M.Map (UUID, UUID) Activity
activityMap db = surviving db S.empty

{- | Reset technosphere / waste exchanges on a surviving activity whose producer
link no longer resolves to a surviving @(activityUUID, productUUID)@ key.

The link target is exactly that pair: 'findProducer' resolves
@(activityLink, flowId)@ against the interning table, so a multi-product
activity that keeps at least one product stays a valid target for exchanges
pointing at a *surviving* product, while exchanges pointing at a deleted
product are unlinked. A biosphere exchange has no producer link and is
returned unchanged. An already-orphan link stays orphan.
-}
unlinkActivity :: S.Set (UUID, UUID) -> Activity -> Activity
unlinkActivity survivingKeys act =
    act{exchanges = map unlinkExchange (exchanges act)}
  where
    dangling link flow = not (S.member (link, flow) survivingKeys)
    unlinkExchange ex = case ex of
        BiosphereExchange{} -> ex
        TechnosphereExchange{techActivityLinkId = Just link, techFlowId = flow}
            | dangling link flow -> ex{techActivityLinkId = Nothing}
        TechnosphereExchange{} -> ex
        WasteExchange{waActivityLinkId = Just link, waFlowId = flow}
            | dangling link flow -> ex{waActivityLinkId = Nothing}
        WasteExchange{} -> ex

{- | Rebuild a 'Database' from a surviving activity map, reusing the exact pure
builders that back a freshly-loaded database. Flow / unit tables are carried
over unchanged: deletion removes activities, never the flow or unit
vocabulary, so a relink can still resolve the surviving links. Runtime fields
(synonym, flow-name, BM25 indexes) are reset to their unloaded state; the
effectful caller re-attaches them with the merged synonym DB.
-}
rebuildFromActivities :: UnitConfig -> Database -> M.Map (UUID, UUID) Activity -> Either Text Database
rebuildFromActivities unitConfig db activities =
    let tables = buildInterningTables activities
        supplierRefUnits = buildSupplierRefUnits (dbUnits db) (itActivities tables)
        indexes = buildIndexesWithProcessIds (itActivities tables) (itProcessIdTable tables)
        bioFlowUUIDs = collectBioFlowOrder (itActivities tables)
        bioTriples = buildBioTriples bioFlowUUIDs tables
        productIndex = buildProductIndex (itActivities tables) (itProcessIdTable tables) (dbTechFlows db)
     in case buildTechTriples unitConfig (dbUnits db) tables supplierRefUnits of
            Left err -> Left err
            Right (techTriples, _warnings) ->
                Right
                    db
                        { dbProcessIdTable = itProcessIdTable tables
                        , dbProcessIdLookup = itProcessIdLookup tables
                        , dbActivityUUIDIndex = itActivityUUIDIndex tables
                        , dbProductIndex = productIndex
                        , dbActivities = itActivities tables
                        , dbIndexes = indexes
                        , dbTechnosphereTriples = techTriples
                        , dbBiosphereTriples = bioTriples
                        , dbActivityIndex = V.generate (fromIntegral (itActivityCount tables)) fromIntegral
                        , dbBiosphereOrder = bioFlowUUIDs
                        , dbActivityCount = itActivityCount tables
                        , dbBiosphereCount = fromIntegral (V.length bioFlowUUIDs)
                        , -- Cross-DB links may now reference deleted activities; clear so a
                          -- subsequent relink rebuilds them against the surviving set.
                          dbCrossDBLinks = []
                        , -- Linking stats describe the pre-delete set (input count, completeness,
                          -- missing suppliers); reset to the fresh-load default so the setup/status
                          -- endpoint never reports stale numbers until the next relink.
                          dbLinkingStats = mempty
                        , -- Runtime-only indexes are reset; re-attached by the effectful caller.
                          dbSynonymDB = Nothing
                        , dbFlowsByName = M.empty
                        , dbFlowsByCAS = M.empty
                        , dbProductSearchIndex = M.empty
                        , dbBM25Index = Nothing
                        }

-- ---------------------------------------------------------------------------
-- Insert / replace
-- ---------------------------------------------------------------------------

{- | Whether a batch means to add rows the database does not have, or to
rewrite rows it does. There is deliberately no upsert: a mistyped identity
must fail, not quietly become a second copy of an activity the author thought
they were correcting.
-}
data WriteIntent = Insert | Replace

{- | Add authored activities to a database and rebuild everything that depends
on them.

Every key must be absent - 'Database.Author' mints identity from the activity's
own name, location, product and unit, so a key that already exists means the
author is re-describing something the database already holds and wants
'replaceActivities' instead.
-}
insertActivities :: UnitConfig -> [ResolvedInsert] -> Database -> Either Text Database
insertActivities = applyResolved Insert

{- | Rewrite activities the database already holds, keeping their identity.

Every key must be present. Replacing keeps the old version's flows in the
vocabulary, exactly as deletion does: a flow no activity uses any more is
inert, and dropping it would break a relink that still resolves through it.
-}
replaceActivities :: UnitConfig -> [ResolvedInsert] -> Database -> Either Text Database
replaceActivities = applyResolved Replace

{- | Shared write path. The vocabulary the batch brings (its product flows and
any new biosphere flows) lands in the same step as the activities that
reference it, so no intermediate value exists in which an exchange points at a
flow the database cannot name.

An empty batch returns the database untouched rather than rebuilding: a
rebuild resets cross-database links ('rebuildFromActivities'), so treating "no
activities to write" as a no-op is what keeps a caller's empty request from
silently unlinking the database.
-}
applyResolved :: WriteIntent -> UnitConfig -> [ResolvedInsert] -> Database -> Either Text Database
applyResolved intent unitConfig inserts db
    | null inserts = Right db
    | otherwise = do
        let existing = activityMap db
        checkKeys intent existing (map riKey inserts)
        rebuildFromActivities
            unitConfig
            db
                { dbTechFlows = M.union (indexBy tfId (concatMap riNewTechFlows inserts)) (dbTechFlows db)
                , dbBioFlows = M.union (indexBy bfId (concatMap riNewBioFlows inserts)) (dbBioFlows db)
                }
            (M.union (M.fromList [(riKey i, riActivity i) | i <- inserts]) existing)
  where
    indexBy key xs = M.fromList [(key x, x) | x <- xs]

{- | Refuse a batch whose keys contradict the intent, naming every offending
identity at once so a long batch is not fixed one round-trip at a time.
-}
checkKeys :: WriteIntent -> M.Map (UUID, UUID) Activity -> [(UUID, UUID)] -> Either Text ()
checkKeys intent existing keys = case intent of
    Insert -> refuse "already exists" (filter (`M.member` existing) keys)
    Replace -> refuse "does not exist" (filter (not . (`M.member` existing)) keys)
  where
    refuse _ [] = Right ()
    refuse what offenders =
        Left $
            "Refusing to write: "
                <> T.intercalate ", " (map renderKey offenders)
                <> " "
                <> what
                <> " in this database"

-- | @activityUUID_productUUID@, the identity a process is addressed by.
renderKey :: (UUID, UUID) -> Text
renderKey = processRefText . uncurry ProcessRef

{- | The identity a 'ProcessId' currently stands for. Out of range is a caller
error and says so, rather than resolving to whichever row is at that index
after the next rebuild.
-}
processKey :: Database -> ProcessId -> Either Text (UUID, UUID)
processKey db pid =
    maybe (Left ("ProcessId out of range: " <> T.pack (show pid))) Right $
        dbProcessIdTable db V.!? fromIntegral pid

{- | Resolve a process-id string to its 'ProcessId'. Accepts the canonical
@activityUUID_productUUID@ form and the bare-activity-UUID fallback (when the
activity has a unique reference product), mirroring
'Service.resolveActivityAndProcessId'. The UI, the CLI and the journal all
carry these textual ids, because a 'ProcessId' is a matrix index that
renumbers on every edit. Unresolvable ids fail loudly rather than being
silently dropped.
-}
resolveProcess :: Database -> Text -> Either Text ProcessId
resolveProcess db queryText =
    maybe (Left ("Unknown process id: " <> queryText)) Right $ case parseProcessRef queryText of
        Just ref -> findProcessId db (prActivity ref) (prProduct ref)
        Nothing -> UUID.fromText queryText >>= findProcessIdByActivityUUID db
