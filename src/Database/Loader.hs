{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Database.Loader
Description : High-performance EcoSpold XML loading with matrix caching

This module provides optimized loading of EcoSpold XML files together with a
single cache storing the fully indexed database and pre-computed sparse
matrices. When the cache is absent or invalidated, the loader reparses all
EcoSpold datasets, builds the in-memory structures, and writes the matrix cache
for subsequent runs.

Key performance features:
- Parallel parsing with controlled concurrency (prevents resource exhaustion)
- Automatic cache invalidation based on source file changes
- Memory-efficient chunked processing for large databases
- Hash-based cache filenames for multi-dataset support

Cache performance (Ecoinvent 3.8 with 18K activities):
- Cold start (XML parsing + matrix build): ~45s
- Matrix cache hit: ~0.5s

The cache keeps day-to-day execution fast while preserving reproducibility.
-}
module Database.Loader (
    -- * Main Loading Functions
    loadDatabase,
    loadDatabaseWithLocationAliases,
    loadDatabaseWithCrossDBLinking,

    -- * Cache Operations
    loadCachedDatabaseWithMatrices,
    saveCachedDatabaseWithMatrices,
    loadDatabaseFromCacheFile,
    generateMatrixCacheFilename,

    -- * Cross-Database Linking
    fixActivityLinksWithCrossDB,
    findAllCrossDBLinks,
    CrossDBLinkingStats (..),
    emptyCrossDBLinkingStats,
    crossDBLinksCount,
    unresolvedCount,
    crossDBBySource,
    collectUnlinkedProductNames,

    -- * Database Analysis
    countTotalTechInputs,
    countUnlinkedExchanges,

    -- * Internal Linking
    fixSimaProActivityLinks,

    -- * Reporting
    reportCrossDBLinkingStats,

    -- * Internal (exposed for testing)
    normalizeText,
    mergeFlows,
    generateActivityUUIDFromActivity,
    getReferenceProductUUID,
    UnlinkedSummary (..),
    emptyUnlinkedSummary,
    mergeUnlinkedSummaries,
    buildSupplierIndex,
    buildSupplierIndexByName,
    fixExchangeLinkByName,
) where

import qualified Codec.Compression.Zstd as Zstd
import Control.Concurrent.Async
import Control.DeepSeq (force)
import Control.Exception (SomeException, catch, evaluate)
import Control.Monad
import Data.Bits (xor)
import qualified Data.ByteString as BS
import Data.Char (toLower)
import Data.Either (partitionEithers)
import Data.List (group, sort, sortBy, sortOn, unzip7)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import qualified Data.Set as S
import Data.Store (decodeEx, encode)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Data.Typeable (typeOf, typeRepFingerprint)
import qualified Data.UUID as UUID
import qualified Data.UUID.V5 as UUID5
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word64)
import Database.CrossLinking (
    CrossDBLinkResult (..),
    IndexedDatabase (..),
    LinkWarning (..),
    LinkingContext (..),
    defaultLinkingThreshold,
    extractProductPrefixes,
    findSupplierAcrossDatabases,
    locationHierarchy,
    normalizeUnicode,
 )
import Database.UploadedDatabase (getDataDir)
import EcoSpold.Common (distributeFiles)
import EcoSpold.Parser1 (streamParseActivityAndFlowsFromFile1, streamParseAllDatasetsFromFile1)
import EcoSpold.Parser2 (streamParseActivityAndFlowsFromFile)
import GHC.Conc (getNumCapabilities)
import GHC.Fingerprint (Fingerprint (..))
import qualified ILCD.Parser as ILCD
import Progress
import qualified SimaPro.Parser as SimaPro
import SynonymDB (SynonymDB)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getFileSize, listDirectory, removeFile)
import System.FilePath (takeBaseName, takeExtension, (</>))
import Text.Printf (printf)
import Types
import qualified UnitConversion as UC

-- | Magic bytes to identify VoLCA cache files
cacheMagic :: BS.ByteString
cacheMagic = "VOLCACHE"

{- | Merge two Flow records with the same UUID, combining their synonyms.
When multiple .spold files reference the same biosphere flow, each may carry
different synonyms. M.fromListWith mergeFlows ensures no synonym is lost.
-}
mergeFlows :: Flow -> Flow -> Flow
mergeFlows a b = a{flowSynonyms = M.unionWith S.union (flowSynonyms a) (flowSynonyms b)}

{- |
Schema signature automatically derived from the Database type structure.

This signature changes when:
- Fields are added/removed from Database or nested types
- Type names change
- Type structure changes

The signature is stored inside the cache file and checked on load.
If it doesn't match, the cache is automatically invalidated and rebuilt.
-}
schemaSignature :: Word64
schemaSignature =
    let Fingerprint hi lo = typeRepFingerprint (typeOf (undefined :: Database))
     in hi `xor` lo `xor` 4

{- |
Helper function to parse UUID from Text with deterministic UUID generation fallback.
Uses the same namespace as Parser.hs to ensure consistency.
-}
testDataNamespace :: UUID.UUID
testDataNamespace = UUID5.generateNamed UUID5.namespaceURL (BS.unpack $ T.encodeUtf8 "acvengine.test")

parseUUID :: T.Text -> UUID.UUID
parseUUID txt = fromMaybe (UUID5.generateNamed testDataNamespace (BS.unpack $ T.encodeUtf8 txt)) (UUID.fromText txt)

-- | Namespace for EcoSpold1 UUID generation
ecospold1Namespace :: UUID.UUID
ecospold1Namespace = UUID5.generateNamed UUID5.namespaceURL (BS.unpack $ T.encodeUtf8 "ecospold1.ecoinvent.org")

-- | Generate activity UUID from activity name and location (for EcoSpold1)
generateActivityUUIDFromActivity :: Activity -> UUID.UUID
generateActivityUUIDFromActivity act =
    let key = activityName act <> ":" <> activityLocation act
     in UUID5.generateNamed ecospold1Namespace (BS.unpack $ T.encodeUtf8 key)

-- | Get reference product UUID from activity exchanges
getReferenceProductUUID :: Activity -> UUID.UUID
getReferenceProductUUID act =
    case filter exchangeIsReference (exchanges act) of
        (ref : _) -> exchangeFlowId ref
        [] -> UUID.nil -- No reference product found

-- | Type alias for supplier lookup index (with location)
type SupplierIndex = M.Map (T.Text, T.Text) (UUID.UUID, UUID.UUID)

{- | Type alias for name-only supplier lookup (for SimaPro)
Maps normalizedProductName → (activityUUID, productUUID)
-}
type NameOnlyIndex = M.Map T.Text (UUID.UUID, UUID.UUID)

{- | Type alias for name-only supplier lookup with location (for EcoSpold1)
Maps normalizedProductName → (activityUUID, productUUID, location)
Used when exchange has no location attribute to find the activity's actual location
-}
type SupplierByNameWithLocation = M.Map T.Text (UUID.UUID, UUID.UUID, T.Text)

-- | Dataset number → (activityUUID, productUUID) for EcoSpold1 Tier 1 linking
type DatasetNumberIndex = M.Map Int (UUID.UUID, UUID.UUID)

-- | Information about an unlinked technosphere exchange
data UnlinkedExchange = UnlinkedExchange
    { ueFlowName :: !T.Text
    , ueLocation :: !T.Text
    }
    deriving (Eq, Ord, Show)

-- | Summary of unlinked exchanges grouped by consumer activity
data UnlinkedSummary = UnlinkedSummary
    { usActivities :: !(M.Map T.Text [UnlinkedExchange]) -- consumer name → list of unlinked exchanges
    , usTotalLinks :: !Int
    , usFoundLinks :: !Int
    , usMissingLinks :: !Int
    }
    deriving (Show)

-- | Empty unlinked summary
emptyUnlinkedSummary :: UnlinkedSummary
emptyUnlinkedSummary = UnlinkedSummary M.empty 0 0 0

-- | Merge two unlinked summaries
mergeUnlinkedSummaries :: UnlinkedSummary -> UnlinkedSummary -> UnlinkedSummary
mergeUnlinkedSummaries s1 s2 =
    UnlinkedSummary
        { usActivities = M.unionWith (++) (usActivities s1) (usActivities s2)
        , usTotalLinks = usTotalLinks s1 + usTotalLinks s2
        , usFoundLinks = usFoundLinks s1 + usFoundLinks s2
        , usMissingLinks = usMissingLinks s1 + usMissingLinks s2
        }

-- | Report grouped summary of unlinked exchanges
reportUnlinkedSummary :: UnlinkedSummary -> IO ()
reportUnlinkedSummary summary
    | M.null (usActivities summary) = return () -- Nothing to report
    | otherwise = do
        let activities = usActivities summary
            activityCount = M.size activities
            -- Sort activities by number of unlinked exchanges (descending)
            sortedActivities = take 10 $ reverse $ sortOn' (length . snd) $ M.toList activities
            remainingCount = activityCount - length sortedActivities

        reportProgress Warning $
            printf "Unlinked activities: %d activities affected" activityCount

        -- Report top activities with their missing suppliers
        forM_ sortedActivities $ \(actName, unlinkedExchanges) -> do
            let uniqueExchanges = nub unlinkedExchanges -- Remove duplicates
                flowCount = length uniqueExchanges
                topFlows = take 3 uniqueExchanges
                remainingFlows = flowCount - length topFlows
            reportProgress Warning $
                printf "  - %s: %d missing suppliers" (T.unpack actName) flowCount
            forM_ topFlows $ \ue ->
                if T.null (ueLocation ue)
                    then reportProgress Warning $ printf "      * %s" (T.unpack (ueFlowName ue))
                    else reportProgress Warning $ printf "      * %s [%s]" (T.unpack (ueFlowName ue)) (T.unpack (ueLocation ue))
            when (remainingFlows > 0) $
                reportProgress Warning $
                    printf "      ... and %d more" remainingFlows

        when (remainingCount > 0) $
            reportProgress Warning $
                printf "  ... and %d more activities" remainingCount
  where
    sortOn' f = sortBy (\a b -> compare (f a) (f b))
    nub = map head . group . sort

-- | Normalize text for matching: lowercase, strip whitespace, normalize Unicode
normalizeText :: T.Text -> T.Text
normalizeText = T.toLower . T.strip . normalizeUnicode

{- | Build supplier index: (normalizedProductName, location) → (activityUUID, productUUID)
For each activity, we index it by its reference product name + activity location
-}
buildSupplierIndex :: ActivityMap -> FlowDB -> SupplierIndex
buildSupplierIndex activities flowDb =
    M.fromList
        [ ((normalizeText (flowName flow), activityLocation act), (actUUID, prodUUID))
        | ((actUUID, prodUUID), act) <- M.toList activities
        , ex <- exchanges act
        , exchangeIsReference ex
        , Just flow <- [M.lookup (exchangeFlowId ex) flowDb]
        ]

{- | Build name-only supplier index for SimaPro linking
Uses the normalized product name + extracted prefixes (no location required).
Exact names take priority via M.union.
-}
buildSupplierIndexByName :: ActivityMap -> FlowDB -> NameOnlyIndex
buildSupplierIndexByName activities flowDb =
    let entries =
            [ (flowName flow, (actUUID, prodUUID))
            | ((actUUID, prodUUID), act) <- M.toList activities
            , ex <- exchanges act
            , exchangeIsReference ex
            , Just flow <- [M.lookup (exchangeFlowId ex) flowDb]
            ]
        exactIndex = M.fromList [(normalizeText name, val) | (name, val) <- entries]
        prefixIndex =
            M.fromList
                [ (normalizeText p, val)
                | (name, val) <- entries
                , p <- extractProductPrefixes name
                , normalizeText p /= normalizeText name
                ]
     in M.union exactIndex prefixIndex

{- | Build name-only supplier index with location for EcoSpold1 linking
Used when exchange has no location attribute to find the activity's actual location
Maps normalizedProductName → (activityUUID, productUUID, activityLocation)
-}
buildSupplierIndexByNameWithLocation :: ActivityMap -> FlowDB -> SupplierByNameWithLocation
buildSupplierIndexByNameWithLocation activities flowDb =
    M.fromList
        [ (normalizeText (flowName flow), (actUUID, prodUUID, activityLocation act))
        | ((actUUID, prodUUID), act) <- M.toList activities
        , ex <- exchanges act
        , exchangeIsReference ex
        , Just flow <- [M.lookup (exchangeFlowId ex) flowDb]
        ]

{- | Fix EcoSpold1 activity links by resolving supplier references.
Matches input exchanges to suppliers by (flowName, location).
Unlinked exchanges stay unlinked so that cross-DB linking can resolve them.
Location aliases map wrongLocation → correctLocation (e.g., "ENTSO" → "ENTSO-E")
-}
fixEcoSpold1ActivityLinks :: M.Map T.Text T.Text -> DatasetNumberIndex -> M.Map UUID.UUID Int -> SimpleDatabase -> IO SimpleDatabase
fixEcoSpold1ActivityLinks locationAliases dsIndex supplierLinks db = do
    -- Build supplier index
    let supplierIndex = buildSupplierIndex (sdbActivities db) (sdbFlows db)
    -- Build name-only index with location for exchanges missing location attribute
    let nameIndex = buildSupplierIndexByNameWithLocation (sdbActivities db) (sdbFlows db)
    reportProgress Info $
        printf
            "Built supplier index with %d entries for activity linking (%d location aliases, %d name-only entries, %d dataset-number entries)"
            (M.size supplierIndex)
            (M.size locationAliases)
            (M.size nameIndex)
            (M.size dsIndex)

    -- Count and report statistics
    let ctx =
            ExchangeLinkContext
                { elcLocationAliases = locationAliases
                , elcSupplierIndex = supplierIndex
                , elcNameIndex = nameIndex
                , elcDatasetIndex = dsIndex
                , elcSupplierLinks = supplierLinks
                , elcFlowDB = sdbFlows db
                }
        (fixedActivities, summary) = fixAllActivities ctx (sdbActivities db)

    reportProgress Info $
        printf
            "Activity linking: %d/%d resolved (%.1f%%), %d unresolved"
            (usFoundLinks summary)
            (usTotalLinks summary)
            (if usTotalLinks summary > 0 then 100.0 * fromIntegral (usFoundLinks summary) / fromIntegral (usTotalLinks summary) else 0.0 :: Double)
            (usMissingLinks summary)

    -- Report grouped summary of unlinked exchanges
    reportUnlinkedSummary summary

    return $ db{sdbActivities = fixedActivities}

{- | Bundle of lookup tables threaded through EcoSpold1 activity-link resolution.
Previously these six fields were passed as positional parameters through
'fixAllActivities' -> 'fixActivityExchanges' -> 'fixExchangeLink', each call
re-forwarding the same values. The record collapses the cascade to a single
argument and makes the dependencies explicit.
-}
data ExchangeLinkContext = ExchangeLinkContext
    { elcLocationAliases :: !(M.Map T.Text T.Text)
    , elcSupplierIndex :: !SupplierIndex
    , elcNameIndex :: !SupplierByNameWithLocation
    , elcDatasetIndex :: !DatasetNumberIndex
    , elcSupplierLinks :: !(M.Map UUID.UUID Int)
    , elcFlowDB :: !FlowDB
    }

-- | Fix all activities and return statistics with unlinked summary
fixAllActivities :: ExchangeLinkContext -> ActivityMap -> (ActivityMap, UnlinkedSummary)
fixAllActivities ctx activities =
    let results = M.map (fixActivityExchanges ctx) activities
        summaries = map snd $ M.elems results
        combinedSummary = foldr mergeUnlinkedSummaries emptyUnlinkedSummary summaries
        fixedActivities = M.map fst results
     in (fixedActivities, combinedSummary)

-- | Fix activity exchanges and return (fixed activity, UnlinkedSummary)
fixActivityExchanges :: ExchangeLinkContext -> Activity -> (Activity, UnlinkedSummary)
fixActivityExchanges ctx act =
    let (fixedExchanges, summaries) = unzip $ map (fixExchangeLink ctx (activityName act)) (exchanges act)
        combinedSummary = foldr mergeUnlinkedSummaries emptyUnlinkedSummary summaries
     in (act{exchanges = fixedExchanges}, combinedSummary)

{- | Fix a single exchange's activity link by (flowName, location) match.

Unlinked exchanges stay unlinked for cross-DB resolution.
Returns (fixed exchange, UnlinkedSummary)
-}
fixExchangeLink :: ExchangeLinkContext -> T.Text -> Exchange -> (Exchange, UnlinkedSummary)
fixExchangeLink ExchangeLinkContext{..} consumerName ex@(TechnosphereExchange fid amt uid isInp isRef _ procLink loc)
    | isInp =
        let linked actUUID prodUUID = (TechnosphereExchange prodUUID amt uid isInp isRef actUUID procLink loc, UnlinkedSummary M.empty 1 1 0)
            unlinked flow lookupLoc =
                let ue = UnlinkedExchange (flowName flow) lookupLoc
                 in (ex, UnlinkedSummary (M.singleton consumerName [ue]) 1 0 1)
         in case M.lookup fid elcFlowDB of
                Just flow ->
                    -- Tier 1: dataset-number lookup with name validation
                    case M.lookup fid elcSupplierLinks >>= \dsNum -> M.lookup dsNum elcDatasetIndex of
                        Just (actUUID, prodUUID)
                            | Just supplierFlow <- M.lookup prodUUID elcFlowDB
                            , normalizeText (flowName supplierFlow) == normalizeText (flowName flow) ->
                                linked actUUID prodUUID
                        _ ->
                            -- Tier 2: name + location lookup
                            let normalizedLoc = fromMaybe loc (M.lookup loc elcLocationAliases)
                                lookupLoc
                                    | T.null normalizedLoc =
                                        case M.lookup (normalizeText (flowName flow)) elcNameIndex of
                                            Just (_, _, actLoc) -> actLoc
                                            Nothing -> normalizedLoc
                                    | otherwise = normalizedLoc
                                key = (normalizeText (flowName flow), lookupLoc)
                             in case M.lookup key elcSupplierIndex of
                                    Just (actUUID, prodUUID) -> linked actUUID prodUUID
                                    Nothing ->
                                        -- Tier 3: name-only fallback (safe for EcoSpold1 where names include {LOCATION})
                                        case M.lookup (normalizeText (flowName flow)) elcNameIndex of
                                            Just (actUUID, prodUUID, _) -> linked actUUID prodUUID
                                            Nothing -> unlinked flow lookupLoc
                Nothing ->
                    (ex, UnlinkedSummary M.empty 1 0 1)
    | otherwise = (ex, emptyUnlinkedSummary)
fixExchangeLink _ _ ex = (ex, emptyUnlinkedSummary)

{- |
Load all EcoSpold files with optimized parallel processing and deduplication.

This function implements a high-performance loading strategy:
1. **Chunked Processing**: Split files into optimal chunks (500 files/chunk)
2. **Controlled Parallelism**: Limit concurrent file handles (4 max)
3. **Memory Management**: Process chunks sequentially to control memory usage
4. **Deduplication**: Automatic flow and unit deduplication across files

Performance characteristics:
- Memory usage: ~2-4GB peak for Ecoinvent 3.8
- Processing time: ~45s for 18K activities (cold start)
- Parallelism: 4x concurrent file parsing within chunks
- Chunk size: 500 files (optimal for memory vs parallelism trade-off)

Used when no cache exists or caching is disabled.
-}
loadDatabase :: FilePath -> IO (Either T.Text SimpleDatabase)
loadDatabase = loadDatabaseWithLocationAliases M.empty

{- | Load all EcoSpold files with location aliases
Location aliases map wrongLocation → correctLocation (e.g., "ENTSO" → "ENTSO-E")
-}
loadDatabaseWithLocationAliases :: M.Map T.Text T.Text -> FilePath -> IO (Either T.Text SimpleDatabase)
loadDatabaseWithLocationAliases locationAliases path = do
    -- Check if path is a file (SimaPro CSV) or directory (EcoSpold)
    isFile <- doesFileExist path
    isDir <- doesDirectoryExist path

    if isFile
        then case map toLower (takeExtension path) of
            ".csv" -> loadSimaProCSV path
            ".xml" -> loadSingleEcoSpold1File locationAliases path
            _ -> return $ Left $ T.pack $ "Unsupported file type: " ++ path
        else
            if isDir
                then do
                    hasProcesses <- doesDirectoryExist (path </> "processes")
                    if hasProcesses
                        then ILCD.parseILCDDirectory path
                        else loadEcoSpoldDirectory locationAliases path
                else return $ Left $ T.pack $ "Path does not exist: " ++ path

-- | Load SimaPro CSV file
loadSimaProCSV :: FilePath -> IO (Either T.Text SimpleDatabase)
loadSimaProCSV csvPath = do
    (activities, flowDB, unitDB) <- SimaPro.parseSimaProCSV csvPath

    if null activities
        then return $ Left "No activities found in SimaPro CSV file."
        else do
            -- Build ActivityMap with generated ProcessIds
            -- For SimaPro: use the same UUID for both activity and product (like EcoSpold1)
            let procMap =
                    M.fromList
                        [ ((SimaPro.generateActivityUUID act, getReferenceProductUUID act), act)
                        | act <- activities
                        ]

            -- Build initial database
            let simpleDb = SimpleDatabase procMap flowDB unitDB

            -- Fix activity links using supplier lookup (same as EcoSpold1)
            Right <$> fixSimaProActivityLinks simpleDb

{- | Fix SimaPro activity links by resolving supplier references
Uses name-only matching (no location required) for SimaPro technosphere inputs
-}
fixSimaProActivityLinks :: SimpleDatabase -> IO SimpleDatabase
fixSimaProActivityLinks db = do
    let nameIndex = buildSupplierIndexByName (sdbActivities db) (sdbFlows db)
    reportProgress Info $ printf "Built name-only supplier index with %d entries for SimaPro linking" (M.size nameIndex)

    -- Count and report statistics
    let (fixedActivities, summary) = fixAllActivitiesByName nameIndex (sdbFlows db) (sdbActivities db)

    reportProgress Info $
        printf
            "SimaPro activity linking: %d/%d resolved (%.1f%%), %d unresolved"
            (usFoundLinks summary)
            (usTotalLinks summary)
            (if usTotalLinks summary > 0 then 100.0 * fromIntegral (usFoundLinks summary) / fromIntegral (usTotalLinks summary) else 0.0 :: Double)
            (usMissingLinks summary)

    -- Report grouped summary of unlinked exchanges
    reportUnlinkedSummary summary

    return $ db{sdbActivities = fixedActivities}

-- | Fix all activities using name-only matching
fixAllActivitiesByName :: NameOnlyIndex -> FlowDB -> ActivityMap -> (ActivityMap, UnlinkedSummary)
fixAllActivitiesByName idx flowDb activities =
    let results = M.map (\act -> fixActivityExchangesByName idx flowDb act) activities
        summaries = map snd $ M.elems results
        combinedSummary = foldr mergeUnlinkedSummaries emptyUnlinkedSummary summaries
        fixedActivities = M.map fst results
     in (fixedActivities, combinedSummary)

-- | Fix activity exchanges using name-only matching
fixActivityExchangesByName :: NameOnlyIndex -> FlowDB -> Activity -> (Activity, UnlinkedSummary)
fixActivityExchangesByName idx flowDb act =
    let (fixedExchanges, summaries) = unzip $ map (fixExchangeLinkByName idx flowDb (activityName act)) (exchanges act)
        combinedSummary = foldr mergeUnlinkedSummaries emptyUnlinkedSummary summaries
     in (act{exchanges = fixedExchanges}, combinedSummary)

{- | Fix a single exchange's activity link using name-only matching
Returns (fixed exchange, UnlinkedSummary)
-}
fixExchangeLinkByName :: NameOnlyIndex -> FlowDB -> T.Text -> Exchange -> (Exchange, UnlinkedSummary)
fixExchangeLinkByName idx flowDb consumerName ex@(TechnosphereExchange fid amt uid isInp isRef _ procLink loc)
    | isInp || not isRef -- Inputs + co-product outputs (avoided production credits)
        =
        case M.lookup fid flowDb of
            Just flow ->
                let key = normalizeText (flowName flow)
                 in case M.lookup key idx of
                        Just (actUUID, prodUUID) ->
                            -- Found supplier: update both activityLinkId AND flowId to match supplier's reference product
                            (TechnosphereExchange prodUUID amt uid isInp isRef actUUID procLink loc, UnlinkedSummary M.empty 1 1 0)
                        Nothing ->
                            -- Fallback: try splitting compound name at separators
                            let prefixes = extractProductPrefixes (flowName flow)
                                tryPrefix [] = Nothing
                                tryPrefix (p : ps) = case M.lookup (normalizeText p) idx of
                                    Just result -> Just result
                                    Nothing -> tryPrefix ps
                             in case tryPrefix prefixes of
                                    Just (actUUID, prodUUID) ->
                                        (TechnosphereExchange prodUUID amt uid isInp isRef actUUID procLink loc, UnlinkedSummary M.empty 1 1 0)
                                    Nothing ->
                                        -- Supplier not found - collect unlinked exchange info
                                        let unlinked = UnlinkedExchange (flowName flow) loc
                                            unlinkedMap = M.singleton consumerName [unlinked]
                                         in (ex, UnlinkedSummary unlinkedMap 1 0 1)
            Nothing ->
                -- Flow not in database - shouldn't happen but be safe
                (ex, UnlinkedSummary M.empty 1 0 1)
    | otherwise = (ex, emptyUnlinkedSummary) -- Not a linkable exchange (outputs/references)
fixExchangeLinkByName _ _ _ ex = (ex, emptyUnlinkedSummary) -- BiosphereExchange - no linking needed

-- | Load EcoSpold files from directory
loadEcoSpoldDirectory :: M.Map T.Text T.Text -> FilePath -> IO (Either T.Text SimpleDatabase)
loadEcoSpoldDirectory locationAliases dir = do
    reportProgress Info "Scanning directory for EcoSpold files"
    files <- listDirectory dir
    -- Support both EcoSpold2 (.spold) and EcoSpold1 (.XML/.xml) files
    let spold2Files = [dir </> f | f <- files, takeExtension f == ".spold"]
    let spold1Files = [dir </> f | f <- files, map toLower (takeExtension f) == ".xml"]

    -- Determine which format to use based on what's found
    case (spold2Files, spold1Files) of
        ([], []) -> return $ Left $ T.pack $ "No EcoSpold files found in directory: " ++ dir
        ([], [singleXml]) -> do
            -- Single XML file: likely a multi-dataset EcoSpold1 file
            reportProgress Info $ "Found single EcoSpold1 file: " ++ singleXml
            loadSingleEcoSpold1File locationAliases singleXml
        ([], xs) -> do
            reportProgress Info $ "Found " ++ show (length xs) ++ " EcoSpold1 (.XML) files for processing"
            loadWithWorkerParallelism xs True
        (xs, []) -> do
            reportProgress Info $ "Found " ++ show (length xs) ++ " EcoSpold2 (.spold) files for processing"
            loadWithWorkerParallelism xs False
        (xs, _) -> do
            reportProgress Info $ "Found " ++ show (length xs) ++ " EcoSpold2 (.spold) files for processing"
            loadWithWorkerParallelism xs False -- Prefer EcoSpold2 if both present
  where
    -- Worker-based parallelism: divide files among N workers, all process in parallel
    loadWithWorkerParallelism :: [FilePath] -> Bool -> IO (Either T.Text SimpleDatabase)
    loadWithWorkerParallelism allFiles isEcoSpold1 = do
        -- Get actual number of CPU capabilities (respects +RTS -N)
        numWorkers <- getNumCapabilities
        let workers = distributeFiles numWorkers allFiles
        reportProgress Info $
            printf
                "Processing %d files with %d parallel workers (%d files per worker)"
                (length allFiles)
                numWorkers
                (length allFiles `div` numWorkers)

        -- Process all workers in parallel
        startTime <- getCurrentTime
        results <- mapConcurrently (processWorker startTime isEcoSpold1) (zip [1 ..] workers)

        -- Check for errors from any worker
        let errors = [e | Left e <- results]
        case errors of
            (firstErr : _) -> return $ Left firstErr
            [] -> do
                let successResults = [r | Right r <- results]
                let (procMaps, flowMaps, unitMaps, rawFlowCounts, rawUnitCounts, dsIndexes, supplierLinksLists) = unzip7 successResults
                let !finalProcMap = M.unions procMaps
                let !finalFlowMap = M.unionsWith mergeFlows flowMaps
                let !finalUnitMap = M.unions unitMaps
                let !finalDsIndex = M.unions dsIndexes
                let !finalSupplierLinks = M.unions supplierLinksLists

                endTime <- getCurrentTime
                let totalDuration = realToFrac $ diffUTCTime endTime startTime
                let totalFiles = length allFiles
                let avgFilesPerSec = fromIntegral totalFiles / totalDuration
                let totalRawFlows = sum rawFlowCounts
                let totalRawUnits = sum rawUnitCounts
                let flowDeduplication = if totalRawFlows > 0 then 100.0 * (1.0 - fromIntegral (M.size finalFlowMap) / fromIntegral totalRawFlows) else 0.0 :: Double
                let unitDeduplication = if totalRawUnits > 0 then 100.0 * (1.0 - fromIntegral (M.size finalUnitMap) / fromIntegral totalRawUnits) else 0.0 :: Double

                reportProgress Info $ printf "Parsing completed (%s, %.1f files/sec):" (formatDuration totalDuration) avgFilesPerSec
                reportProgress Info $ printf "  Activities: %d processes" (M.size finalProcMap)
                reportProgress Info $
                    printf
                        "  Flows: %d unique (%.1f%% deduplication from %d raw)"
                        (M.size finalFlowMap)
                        flowDeduplication
                        totalRawFlows
                reportProgress Info $
                    printf
                        "  Units: %d unique (%.1f%% deduplication from %d raw)"
                        (M.size finalUnitMap)
                        unitDeduplication
                        totalRawUnits
                reportMemoryUsage "Final parsing memory usage"

                -- For EcoSpold1: fix activity links using supplier lookup table
                let simpleDb = SimpleDatabase finalProcMap finalFlowMap finalUnitMap
                if isEcoSpold1
                    then Right <$> fixEcoSpold1ActivityLinks locationAliases finalDsIndex finalSupplierLinks simpleDb
                    else return $ Right simpleDb

    -- Process one worker's share of files
    processWorker :: UTCTime -> Bool -> (Int, [FilePath]) -> IO (Either T.Text (ActivityMap, FlowDB, UnitDB, Int, Int, DatasetNumberIndex, M.Map UUID.UUID Int))
    processWorker _startTime isEcoSpold1 (workerNum, workerFiles) = do
        workerStartTime <- getCurrentTime
        reportProgress Info $ printf "Worker %d started: processing %d files" workerNum (length workerFiles)

        -- Parse all files for this worker using appropriate parser
        -- Both paths return (Activity, [Flow], [Unit], Int, M.Map UUID Int)
        -- For EcoSpold2: dataset number = 0, supplier links = empty
        let parseFile =
                if isEcoSpold1
                    then streamParseActivityAndFlowsFromFile1
                    else \f -> fmap (fmap (\(a, fs, us) -> (a, fs, us, 0, M.empty))) (streamParseActivityAndFlowsFromFile f)
        workerResults <- mapM parseFile workerFiles
        -- Pair each result with its file path, then partition
        let paired = zipWith (\f r -> fmap (\v -> (f, v)) r) workerFiles workerResults
        let (errs, oks) = partitionEithers paired
        forM_ errs $ \e ->
            reportProgress Warning e
        let (okFiles, okResults) = unzip oks
        let procs = [a | (a, _, _, _, _) <- okResults]
            flowLists = [fs | (_, fs, _, _, _) <- okResults]
            unitLists = [us | (_, _, us, _, _) <- okResults]
            dsNums = [n | (_, _, _, n, _) <- okResults]
            supplierLinksList = [sl | (_, _, _, _, sl) <- okResults]
        let !allFlows = concat flowLists
        let !allUnits = concat unitLists

        -- Build maps for this worker - extract UUID pairs from filenames
        -- For EcoSpold1: generate UUIDs from numeric dataset number
        -- For EcoSpold2: parse UUIDs from filename (activityUUID_productUUID.spold)
        let procEntries = zipWith (buildProcEntry isEcoSpold1) okFiles procs

        -- Check for any filename parsing errors
        case [e | Left e <- procEntries] of
            (firstErr : _) -> return $ Left firstErr
            [] -> do
                let !procMap = M.fromList [e | Right e <- procEntries]
                let !flowMap = M.fromListWith mergeFlows [(flowId f, f) | f <- allFlows]
                let !unitMap = M.fromList [(unitId u, u) | u <- allUnits]
                -- Build dataset number index: dsNum → (actUUID, prodUUID)
                let !dsIndex =
                        M.fromList
                            [(n, key) | (n, Right (key, _)) <- zip dsNums procEntries, n /= 0]
                let !allSupplierLinks = M.unions supplierLinksList

                workerEndTime <- getCurrentTime
                let workerDuration = realToFrac $ diffUTCTime workerEndTime workerStartTime
                let filesPerSec = fromIntegral (length workerFiles) / workerDuration
                let rawFlowCount = length allFlows
                let rawUnitCount = length allUnits
                reportProgress Info $
                    printf
                        "Worker %d completed: %d activities, %d flows (%s, %.1f files/sec)"
                        workerNum
                        (M.size procMap)
                        (M.size flowMap)
                        (formatDuration workerDuration)
                        filesPerSec

                return $ Right (procMap, flowMap, unitMap, rawFlowCount, rawUnitCount, dsIndex, allSupplierLinks)

    -- Build a single process entry, returning Either for error handling
    buildProcEntry :: Bool -> FilePath -> Activity -> Either T.Text ((UUID, UUID), Activity)
    buildProcEntry True _filepath activity =
        -- EcoSpold1: Generate activity UUID from name and location
        let actUUID = generateActivityUUIDFromActivity activity
            prodUUID = getReferenceProductUUID activity
         in Right ((actUUID, prodUUID), activity)
    buildProcEntry False filepath activity =
        -- EcoSpold2: Parse UUIDs from filename
        let filename = T.pack $ takeBaseName filepath
         in case T.splitOn "_" filename of
                [actUUIDText, prodUUIDText] ->
                    let actUUID = parseUUID actUUIDText
                        prodUUID = parseUUID prodUUIDText
                     in Right ((actUUID, prodUUID), activity)
                _ -> Left $ T.pack $ "Invalid filename format (expected activityUUID_productUUID.spold): " ++ filepath

{- | Load a single EcoSpold1 file containing multiple datasets
This handles files where <ecoSpold> contains multiple <dataset> elements
-}
loadSingleEcoSpold1File :: M.Map T.Text T.Text -> FilePath -> IO (Either T.Text SimpleDatabase)
loadSingleEcoSpold1File locationAliases filepath = do
    reportProgress Info "Parsing multi-dataset EcoSpold1 file..."
    results <- streamParseAllDatasetsFromFile1 filepath
    reportProgress Info $ "Parsed " ++ show (length results) ++ " datasets from file"

    -- Build activity map from all parsed activities
    let expanded = map buildProcEntryFromResult results
        !procMap = M.fromList [(key, act) | (key, act) <- expanded]
        !flowMap = M.fromListWith mergeFlows [(flowId f, f) | (_, flows, _, _, _) <- results, f <- flows]
        !unitMap = M.fromList [(unitId u, u) | (_, _, units, _, _) <- results, u <- units]
        -- Build dataset number index: dsNum → (actUUID, prodUUID)
        !dsIndex =
            M.fromList
                [(dsNum, key) | ((_, _, _, dsNum, _), (key, _)) <- zip results expanded, dsNum /= 0]
        -- Merge supplier links from all datasets
        !supplierLinks = M.unions [sl | (_, _, _, _, sl) <- results]
        simpleDb = SimpleDatabase procMap flowMap unitMap

    -- Report statistics
    let totalFlows = sum [length flows | (_, flows, _, _, _) <- results]
    let totalUnits = sum [length units | (_, _, units, _, _) <- results]
    reportProgress Info $ printf "  Activities: %d processes" (M.size procMap)
    reportProgress Info $ printf "  Flows: %d unique (from %d raw)" (M.size flowMap) totalFlows
    reportProgress Info $ printf "  Units: %d unique (from %d raw)" (M.size unitMap) totalUnits

    -- Fix activity links using supplier lookup
    Right <$> fixEcoSpold1ActivityLinks locationAliases dsIndex supplierLinks simpleDb
  where
    buildProcEntryFromResult :: (Activity, [Flow], [Unit], Int, M.Map UUID.UUID Int) -> ((UUID.UUID, UUID.UUID), Activity)
    buildProcEntryFromResult (activity, _, _, _, _) =
        let actUUID = generateActivityUUIDFromActivity activity
            prodUUID = getReferenceProductUUID activity
         in ((actUUID, prodUUID), activity)

{- |
Generate filename for matrix cache.

Matrix caches store pre-computed sparse matrices (technosphere A,
biosphere B) enabling direct LCA solving without matrix construction.

Cache location depends on database source:
- Uploaded databases (path starts with "uploads/"): cache in the upload directory
- Configured databases: cache in "cache/" subdirectory

Cache invalidation is handled by a schema signature stored inside
the cache file, not by the filename.
-}
generateMatrixCacheFilename :: T.Text -> FilePath -> IO FilePath
generateMatrixCacheFilename dbName _dataPath = do
    let cacheFilename = "volca.cache." ++ T.unpack dbName ++ ".bin"
    base <- getDataDir
    let cacheDir = base </> "cache"
    createDirectoryIfMissing True cacheDir
    return $ cacheDir </> cacheFilename

{- |
Validate cache file integrity before attempting to decode.

Checks:
- File size is reasonable (> 1KB to avoid empty/corrupted files)
- File exists and is readable

Returns True if cache file appears valid, False otherwise.
-}
validateCacheFile :: FilePath -> IO Bool
validateCacheFile cacheFile = do
    exists <- doesFileExist cacheFile
    if not exists
        then return False
        else do
            fileSize <- getFileSize cacheFile
            -- Cache file should be at least 1KB for a valid database
            -- Typical size is 100MB-600MB
            if fileSize < 1024
                then do
                    reportCacheOperation $ "Cache file is too small (" ++ show fileSize ++ " bytes), likely corrupted"
                    return False
                else return True

{- |
Load Database with pre-computed matrices from cache (second-tier).

This is the fastest loading method (~0.5s) as it bypasses both
XML parsing and matrix construction. The Database includes:
- All activities, flows, units (from SimpleDatabase)
- Pre-built indexes for fast querying
- Pre-computed sparse matrices (technosphere A, biosphere B)
- Activity and flow UUID mappings for matrix operations

Returns Nothing if no matrix cache exists.
-}
loadCachedDatabaseWithMatrices :: T.Text -> FilePath -> IO (Maybe Database)
loadCachedDatabaseWithMatrices dbName dataDir = do
    cacheFile <- generateMatrixCacheFilename dbName dataDir
    let zstdFile = cacheFile ++ ".zst"
    zstdExists <- doesFileExist zstdFile
    if not zstdExists
        then do
            reportCacheOperation "No matrix cache found"
            return Nothing
        else do
            -- Delegate to the shared reader; a Nothing here means the cache
            -- is corrupted/incompatible and should be rebuilt from source.
            result <- loadCompressedCacheFile zstdFile
            case result of
                Just _ -> return result
                Nothing -> do
                    reportCacheOperation $ "Deleting corrupted cache file: " ++ zstdFile
                    removeFile zstdFile
                    reportCacheOperation "Will rebuild database from source files"
                    return Nothing

{- |
Load Database directly from a specified cache file.

Similar to loadCachedDatabaseWithMatrices but takes an explicit cache file path
instead of generating it from a data directory. Supports both compressed (.bin.zst)
and uncompressed (.bin) formats.

This is useful for deploying just the cache file without the original .spold files.

Returns Nothing if the file cannot be loaded.
-}
loadDatabaseFromCacheFile :: FilePath -> IO (Maybe Database)
loadDatabaseFromCacheFile cacheFile = do
    let ext = takeExtension cacheFile
    let isCompressed = ext == ".zst"

    -- Validate file exists
    fileExists <- doesFileExist cacheFile
    if not fileExists
        then do
            reportError $ "Cache file not found: " ++ cacheFile
            return Nothing
        else do
            if isCompressed
                then loadCompressedCacheFile cacheFile
                else loadUncompressedCacheFile cacheFile

-- | Load compressed (.bin.zst) cache file with header validation
loadCompressedCacheFile :: FilePath -> IO (Maybe Database)
loadCompressedCacheFile zstdFile = do
    reportCacheInfo zstdFile
    catch
        ( withProgressTiming Cache "Matrix cache load with zstd decompression" $ do
            contents <- BS.readFile zstdFile
            -- Check minimum size for header (16 bytes)
            if BS.length contents < 16
                then do
                    reportCacheOperation "Cache file too small (missing header)"
                    return Nothing
                else do
                    let (header, compressed) = BS.splitAt 16 contents
                        (magic, sigBytes) = BS.splitAt 8 header
                    -- Check magic bytes
                    if magic /= cacheMagic
                        then do
                            reportCacheOperation "Invalid cache file (wrong magic bytes)"
                            return Nothing
                        else do
                            -- Check schema signature
                            let storedSig = decodeEx sigBytes :: Word64
                            if storedSig /= schemaSignature
                                then do
                                    reportCacheOperation $ "Schema mismatch: cache=" ++ show storedSig ++ " current=" ++ show schemaSignature
                                    reportCacheOperation "Cache will be rebuilt with new schema"
                                    return Nothing
                                else do
                                    -- Decompress and decode the payload
                                    case Zstd.decompress compressed of
                                        Zstd.Skip -> do
                                            reportError "Zstd decompression failed: Skip"
                                            return Nothing
                                        Zstd.Error err -> do
                                            reportError $ "Zstd decompression failed: " ++ show err
                                            return Nothing
                                        Zstd.Decompress decompressed -> do
                                            let !db = decodeEx decompressed
                                            -- Force full evaluation to prevent lazy thunk buildup
                                            db' <- evaluate (force db)
                                            reportCacheOperation $
                                                "Matrix cache loaded: "
                                                    ++ show (dbActivityCount db')
                                                    ++ " activities, "
                                                    ++ show (VU.length $ dbTechnosphereTriples db')
                                                    ++ " tech entries, "
                                                    ++ show (VU.length $ dbBiosphereTriples db')
                                                    ++ " bio entries (decompressed)"
                                            return (Just db')
        )
        ( \(e :: SomeException) -> do
            reportError $ "Compressed cache load failed: " ++ show e
            reportCacheOperation "The compressed cache file is corrupted or incompatible"
            return Nothing
        )

-- | Load uncompressed (.bin) cache file
loadUncompressedCacheFile :: FilePath -> IO (Maybe Database)
loadUncompressedCacheFile cacheFile = do
    -- Validate cache file before attempting to decode
    isValid <- validateCacheFile cacheFile
    if not isValid
        then do
            reportCacheOperation "Cache file validation failed"
            return Nothing
        else do
            reportCacheInfo cacheFile
            catch
                ( withProgressTiming Cache "Matrix cache load" $ do
                    !db <- BS.readFile cacheFile >>= \bs -> evaluate (force (decodeEx bs))
                    reportCacheOperation $
                        "Matrix cache loaded: "
                            ++ show (dbActivityCount db)
                            ++ " activities, "
                            ++ show (VU.length $ dbTechnosphereTriples db)
                            ++ " tech entries, "
                            ++ show (VU.length $ dbBiosphereTriples db)
                            ++ " bio entries"
                    return (Just db)
                )
                ( \(e :: SomeException) -> do
                    reportError $ "Cache load failed: " ++ show e
                    reportCacheOperation "The cache file is corrupted or incompatible with the current version"
                    return Nothing
                )

{- |
Save Database with pre-computed matrices to cache.

Serializes the complete Database including sparse matrices to enable
ultra-fast startup (~0.5s load time). The cache file includes:
- 8 bytes magic ("VOLCACHE")
- 8 bytes schema signature (auto-generated from type structure)
- Zstd compressed Database binary

Should be called after matrix construction is complete.
-}
saveCachedDatabaseWithMatrices :: T.Text -> FilePath -> Database -> IO ()
saveCachedDatabaseWithMatrices dbName dataDir db = do
    cacheFile <- generateMatrixCacheFilename dbName dataDir
    let zstdFile = cacheFile ++ ".zst"
    reportCacheOperation $ "Saving Database with matrices to compressed cache: " ++ zstdFile
    withProgressTiming Cache "Matrix cache save with zstd compression" $ do
        -- Serialize to ByteString (store returns strict ByteString)
        let serialized = encode db
        -- Compress with zstd (level 1 = fast compression, ~5% larger than level 3)
        let compressed = Zstd.compress 1 serialized
        -- Build header: magic (8 bytes) + schema signature (8 bytes)
        let signatureBytes = encode schemaSignature
        let header = cacheMagic <> signatureBytes
        -- Write header + compressed data
        BS.writeFile zstdFile (header <> compressed)
        reportCacheOperation $
            "Matrix cache saved ("
                ++ show (dbActivityCount db)
                ++ " activities, "
                ++ show (VU.length $ dbTechnosphereTriples db)
                ++ " tech entries, "
                ++ show (VU.length $ dbBiosphereTriples db)
                ++ " bio entries, compressed)"

--------------------------------------------------------------------------------
-- Cross-Database Linking
--------------------------------------------------------------------------------

{- | CrossDBLinkingStats, emptyCrossDBLinkingStats, mergeCrossDBStats,
  crossDBLinksCount, unresolvedCount, crossDBBySource
  are now defined in Types and re-exported from this module.
-}

{- | Load EcoSpold files with cross-database linking support.

This function loads EcoSpold files and then attempts to resolve unlinked
technosphere exchanges by searching across other already-loaded databases.

The loading sequence:
1. Parse XML files into SimpleDatabase
2. Build supplier index for THIS database
3. Attempt linking within THIS database (standard behavior)
4. For remaining unlinked exchanges, search OTHER databases
5. Report linking summary with cross-DB statistics
-}
loadDatabaseWithCrossDBLinking ::
    -- | Location aliases (wrongLocation → correctLocation)
    M.Map T.Text T.Text ->
    -- | Pre-built indexes from other databases
    [IndexedDatabase] ->
    -- | Synonym database for name matching
    SynonymDB ->
    -- | Unit configuration for compatibility checking
    UC.UnitConfig ->
    -- | Location hierarchy (empty = use built-in)
    M.Map T.Text [T.Text] ->
    -- | Path to load from
    FilePath ->
    IO (Either T.Text (SimpleDatabase, CrossDBLinkingStats))
loadDatabaseWithCrossDBLinking locationAliases otherIndexes synonymDB unitConfig locationHier path = do
    result <- loadDatabaseWithLocationAliases locationAliases path
    case result of
        Left err -> return $ Left err
        Right simpleDb -> do
            -- Detect unknown units from the database's unit definitions
            let !unknownUnits =
                    S.fromList
                        [ unitName u
                        | u <- M.elems (sdbUnits simpleDb)
                        , not (UC.isKnownUnit unitConfig (unitName u))
                        , not (T.null (unitName u))
                        ]
            unless (S.null unknownUnits) $
                reportProgress Warning $
                    printf
                        "%d unknown unit(s): %s — add to the [[units]] CSV file"
                        (S.size unknownUnits)
                        (T.unpack $ T.intercalate ", " $ map (\u -> "\"" <> u <> "\"") $ S.toList unknownUnits)

            -- If there are other databases to search, perform cross-DB linking
            let !totalInputs = countTotalTechInputs simpleDb
            if null otherIndexes
                then do
                    -- No cross-DB linking needed
                    let !stats = emptyCrossDBLinkingStats{cdlUnknownUnits = unknownUnits, cdlTotalInputs = totalInputs}
                    reportCrossDBLinkingStats (M.size (sdbActivities simpleDb)) stats
                    return $ Right (simpleDb, stats)
                else do
                    -- Perform cross-database linking using pre-built indexes
                    (linkedDb, stats) <-
                        fixActivityLinksWithCrossDB
                            otherIndexes
                            synonymDB
                            unitConfig
                            locationHier
                            simpleDb
                    return $ Right (linkedDb, stats{cdlUnknownUnits = unknownUnits})

{- | Fix activity links using cross-database lookup.

For each unlinked technosphere input (where activityLinkId is nil),
search across other loaded databases to find a matching supplier.

Matching criteria:
- Product name must match (exact, synonym, or fuzzy)
- Units must be compatible
- Location scoring with hierarchy fallback

Cross-database links are stored in CrossDBLinkingStats.cdlLinks for use
in chained inventory solving. The exchanges are NOT modified - they
remain "unlinked" from the perspective of the internal matrix, but the
CrossDBLinks provide the information needed to resolve them at solve time.
-}
fixActivityLinksWithCrossDB ::
    -- | Pre-built indexes from other databases
    [IndexedDatabase] ->
    -- | Synonym database
    SynonymDB ->
    -- | Unit configuration
    UC.UnitConfig ->
    -- | Location hierarchy (code → parent codes)
    M.Map T.Text [T.Text] ->
    -- | Database to fix
    SimpleDatabase ->
    IO (SimpleDatabase, CrossDBLinkingStats)
fixActivityLinksWithCrossDB indexedDbs synonymDB unitConfig locationHier db = do
    -- Count unlinked exchanges before
    let unlinkedBefore = countUnlinkedExchanges db
        !totalInputs = countTotalTechInputs db

    -- If no unlinked exchanges, skip
    if unlinkedBefore == 0
        then do
            reportProgress Info "No unlinked exchanges to resolve via cross-DB linking"
            return (db, emptyCrossDBLinkingStats{cdlTotalInputs = totalInputs})
        else do
            reportProgress Info $
                printf
                    "Cross-database linking: %d unlinked exchanges, searching %d database(s)..."
                    unlinkedBefore
                    (length indexedDbs)

            -- Report index stats
            forM_ indexedDbs $ \idb ->
                reportProgress Info $
                    printf
                        "  - %s: %d products indexed"
                        (T.unpack (idbName idb))
                        (M.size (idbByProductName idb))

            -- Build the linking context with pre-built indexes
            let linkingCtx =
                    LinkingContext
                        { lcIndexedDatabases = indexedDbs
                        , lcSynonymDB = synonymDB
                        , lcUnitConfig = unitConfig
                        , lcThreshold = defaultLinkingThreshold
                        , lcLocationHierarchy = if M.null locationHier then locationHierarchy else locationHier
                        }

            -- Process all activities to find cross-DB links
            reportProgress Info "Finding cross-database suppliers..."
            let stats =
                    findAllCrossDBLinks
                        linkingCtx
                        (sdbFlows db)
                        (sdbUnits db)
                        (sdbActivities db)

            -- Report statistics
            let !stats' = stats{cdlTotalInputs = totalInputs}
            reportCrossDBLinkingStats (M.size (sdbActivities db)) stats'

            -- Return the original database unchanged, along with the cross-DB links
            -- The links will be stored in the Database.dbCrossDBLinks field later
            return (db, stats')

-- | Collect unlinked product names from a database (for databases without cross-DB linking)
collectUnlinkedProductNames :: SimpleDatabase -> M.Map T.Text Int
collectUnlinkedProductNames db =
    M.fromListWith
        (+)
        [ (flowName flow, 1)
        | act <- M.elems (sdbActivities db)
        , TechnosphereExchange fid _ _ True _ linkId _ _ <- exchanges act
        , linkId == UUID.nil
        , Just flow <- [M.lookup fid (sdbFlows db)]
        ]

-- | Count unlinked technosphere exchanges in a database
countUnlinkedExchanges :: SimpleDatabase -> Int
countUnlinkedExchanges db =
    sum
        [ 1
        | act <- M.elems (sdbActivities db)
        , ex <- exchanges act
        , isUnlinkedTechInput ex
        ]
  where
    isUnlinkedTechInput :: Exchange -> Bool
    isUnlinkedTechInput (TechnosphereExchange _ _ _ isInp _ linkId _ _) =
        isInp && linkId == UUID.nil
    isUnlinkedTechInput _ = False

-- | Count total technosphere input exchanges in a database
countTotalTechInputs :: SimpleDatabase -> Int
countTotalTechInputs db =
    sum
        [ 1
        | act <- M.elems (sdbActivities db)
        , ex <- exchanges act
        , isTechInput ex
        ]
  where
    isTechInput :: Exchange -> Bool
    isTechInput (TechnosphereExchange _ _ _ isInp _ _ _ _) = isInp
    isTechInput _ = False

{- | Find all cross-database links without modifying activities
Returns statistics including the CrossDBLinks for chained solving
-}
findAllCrossDBLinks ::
    LinkingContext ->
    FlowDB ->
    UnitDB ->
    ActivityMap ->
    CrossDBLinkingStats
findAllCrossDBLinks ctx flowDb unitDb activities =
    let results = M.mapWithKey (findActivityCrossDBLinks ctx flowDb unitDb) activities
     in foldr mergeCrossDBStats emptyCrossDBLinkingStats (M.elems results)

-- | Find cross-database links for one activity's exchanges
findActivityCrossDBLinks ::
    LinkingContext ->
    FlowDB ->
    UnitDB ->
    -- | Consumer activity key (actUUID, prodUUID)
    (UUID.UUID, UUID.UUID) ->
    Activity ->
    CrossDBLinkingStats
findActivityCrossDBLinks ctx flowDb unitDb (consumerActUUID, consumerProdUUID) act =
    let stats = map (findExchangeCrossDBLink ctx flowDb unitDb consumerActUUID consumerProdUUID) (exchanges act)
     in foldr mergeCrossDBStats emptyCrossDBLinkingStats stats

{- | Find cross-database link for a single exchange

Only attempts cross-DB linking if:
1. Exchange is a technosphere input
2. Exchange is currently unlinked (activityLinkId is nil)

When a link is found, a CrossDBLink is created and returned in stats.
The exchange itself is NOT modified.
-}
findExchangeCrossDBLink ::
    LinkingContext ->
    FlowDB ->
    UnitDB ->
    -- | Consumer activity UUID
    UUID.UUID ->
    -- | Consumer product UUID
    UUID.UUID ->
    Exchange ->
    CrossDBLinkingStats
findExchangeCrossDBLink ctx flowDb unitDb consumerActUUID consumerProdUUID (TechnosphereExchange fid amt _uid isInp _isRef linkId _procLink loc)
    | isInp && linkId == UUID.nil -- Unlinked technosphere input
        =
        case M.lookup fid flowDb of
            Nothing ->
                -- Flow not found in FlowDB - can't name it
                emptyCrossDBLinkingStats
            Just flow ->
                -- Get unit name from UnitDB
                let flowUnitName = case M.lookup (flowUnitId flow) unitDb of
                        Just u -> unitName u
                        Nothing -> "" -- Unknown unit
                 in case findSupplierAcrossDatabases ctx (flowName flow) loc flowUnitName of
                        CrossDBLinked supplierActUUID supplierProdUUID dbName _score prodName supplierLoc warnings ->
                            -- Successfully found cross-DB supplier
                            let !crossLink =
                                    CrossDBLink
                                        { cdlConsumerActUUID = consumerActUUID
                                        , cdlConsumerProdUUID = consumerProdUUID
                                        , cdlSupplierActUUID = supplierActUUID
                                        , cdlSupplierProdUUID = supplierProdUUID
                                        , cdlCoefficient = amt
                                        , cdlExchangeUnit = flowUnitName
                                        , cdlFlowName = prodName
                                        , cdlLocation = supplierLoc
                                        , cdlSourceDatabase = dbName
                                        }
                                fallbacks = [(prodName, req, act) | UpperLocationUsed req act <- warnings]
                             in CrossDBLinkingStats [crossLink] M.empty S.empty fallbacks 0
                        CrossDBNotLinked blocker ->
                            -- Record as unresolved with the blocker reason
                            CrossDBLinkingStats [] (M.singleton (flowName flow) (1, blocker)) S.empty [] 0
    | otherwise =
        emptyCrossDBLinkingStats
findExchangeCrossDBLink _ _ _ _ _ _ = emptyCrossDBLinkingStats

-- | Report cross-database linking statistics
reportCrossDBLinkingStats :: Int -> CrossDBLinkingStats -> IO ()
reportCrossDBLinkingStats nActivities stats = do
    let !nInputs = cdlTotalInputs stats
        !nCrossDB = crossDBLinksCount stats
        !nUnresolved = unresolvedCount stats
        !nInternal = max 0 (nInputs - nCrossDB - nUnresolved)
        !nResolved = nInternal + nCrossDB

    -- Summary line (skip "0/0" for databases without technosphere input tracking)
    if nInputs > 0
        then do
            let !completeness = 100.0 * fromIntegral nResolved / fromIntegral nInputs :: Double
            reportProgress Info $
                printf
                    "Supply chain: %.1f%% complete (%d/%d inputs resolved), %d activities"
                    completeness
                    nResolved
                    nInputs
                    nActivities
            reportProgress Info $
                printf "  Internal: %d, Cross-DB: %d, Unresolved: %d" nInternal nCrossDB nUnresolved
        else
            reportProgress Info $
                printf "Supply chain: %d activities (no technosphere inputs)" nActivities

    -- Per-database breakdown
    forM_ (M.toList (crossDBBySource stats)) $ \(srcDb, count) ->
        reportProgress Info $
            printf "  - %s: %d links" (T.unpack srcDb) count

    -- Missing suppliers
    let !missing = sortOn (\(_, (cnt, _)) -> Down cnt) $ M.toList (cdlUnresolvedProducts stats)
    when (not (null missing)) $ do
        reportProgress Warning $
            printf "Missing suppliers: %d products unresolved" (length missing)
        forM_ (take 20 missing) $ \(name, (cnt, blocker)) ->
            reportProgress Warning $
                printf "  - %s (%d activities) — %s" (T.unpack name) cnt (showBlocker blocker)
        when (length missing > 20) $
            reportProgress Warning $
                printf "  ... and %d more" (length missing - 20)

    -- Unknown units
    let !unknowns = S.toList (cdlUnknownUnits stats)
    when (not (null unknowns)) $
        reportProgress Warning $
            printf "Unknown units: %s" (T.unpack $ T.intercalate ", " unknowns)

    -- Location fallbacks (deduplicated)
    let !uniqueFallbacks = deduplicateFallbacks (cdlLocationFallbacks stats)
        !nFallbacks = length uniqueFallbacks
    when (nFallbacks > 0) $ do
        reportProgress Info $
            printf "Location fallbacks: %d unique products matched with different location" nFallbacks
        forM_ uniqueFallbacks $ \(prod, requested, actual) ->
            reportProgress Info $
                printf "  - %s: %s → %s" (T.unpack prod) (T.unpack requested) (T.unpack actual)

showBlocker :: LinkBlocker -> String
showBlocker NoNameMatch = "Not found"
showBlocker (UnitIncompatible q s) = printf "Unit: %s vs %s" (T.unpack q) (T.unpack s)
showBlocker (LocationUnavailable loc) = printf "Location: %s" (T.unpack loc)
