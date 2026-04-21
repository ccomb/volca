{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Synonym Database

Maps flow names to synonym group IDs for flow matching
across different nomenclatures (ILCD, ecoinvent, SimaPro).

Loaded at runtime from CSV files (pairs of synonym names).
-}
module SynonymDB (
    -- * Building
    buildFromCSV,
    buildFromPairs,
    loadFromCSVFileWithCache,

    -- * Lookup
    lookupSynonymGroup,
    getSynonyms,
    normalizeName,
    mergeSynonymDBs,
    synonymCount,

    -- * Re-exports
    SynonymDB (..),
    emptySynonymDB,
) where

import qualified Codec.Compression.Zstd as Zstd
import Control.DeepSeq (force)
import Control.Exception (SomeException, catch, evaluate)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Csv (HasHeader (..), decode)
import qualified Data.Map.Strict as M
import Data.Store (decodeEx, encode)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Directory (doesFileExist, getModificationTime)

import SynonymDB.Types (SynonymDB (..), emptySynonymDB)

{- | Build a SynonymDB from CSV content (two columns: name1, name2).
Each row declares two names as direct synonyms. No transitive closure:
if A↔B and B↔C, looking up A returns {A,B} but NOT C.
This prevents chain pollution (e.g., sulfate→gypsum→calcium→calcite→carbonate→lithium carbonate).
-}
buildFromCSV :: BL.ByteString -> Either String SynonymDB
buildFromCSV csvData =
    case decode HasHeader csvData of
        Left err -> Left $ "CSV parse error: " <> err
        Right rows -> Right $ buildFromPairs (V.toList (rows :: V.Vector (Text, Text)))

{- | Build SynonymDB from direct pairs (no transitive closure).
Each pair (A, B) creates a group containing both A and B.
If A appears in multiple pairs, all its direct partners are in one group.
But partners' partners are NOT included (no transitivity).
-}
buildFromPairs :: [(Text, Text)] -> SynonymDB
buildFromPairs pairs =
    let
        -- Collect direct neighbors for each name (star topology, not transitive)
        directNeighbors = foldl addPair M.empty pairs
        -- Filter: reject names with too many direct synonyms (overly generic terms)
        maxDirectSynonyms = 50
        validNeighbors = M.filter ((<= maxDirectSynonyms) . length) directNeighbors
        -- Build groups: each name's group = itself + its direct neighbors
        allNames = M.keys validNeighbors
        numberedGroups = zip [0 ..] [name : M.findWithDefault [] name validNeighbors | name <- allNames]
        nameToId = M.fromList [(name, gid) | (gid, members) <- numberedGroups, name <- members]
        idToNames = M.fromList [(gid, members) | (gid, members) <- numberedGroups]
     in
        SynonymDB nameToId idToNames
  where
    addPair :: M.Map Text [Text] -> (Text, Text) -> M.Map Text [Text]
    addPair acc (raw1, raw2) =
        let n1 = normalizeName raw1
            n2 = normalizeName raw2
            o1 = T.strip raw1
            o2 = T.strip raw2
         in if T.null n1 || T.null n2 || n1 == n2
                then acc
                else M.insertWith (++) n1 [o2] $ M.insertWith (++) n2 [o1] acc

{- | Load a SynonymDB from a CSV file, using a binary cache for speed.
  On first load: parse CSV → build SynonymDB → save .cache.zst
  On subsequent loads: load .cache.zst directly (if newer than CSV)
-}
loadFromCSVFileWithCache :: FilePath -> IO (Either String SynonymDB)
loadFromCSVFileWithCache csvPath = do
    let cachePath = csvPath ++ ".cache.zst"
    cached <- loadCache cachePath csvPath
    case cached of
        Just db -> return (Right db)
        Nothing -> do
            csvData <- BL.readFile csvPath
            case buildFromCSV csvData of
                Left err -> return (Left err)
                Right db -> do
                    saveCache cachePath db
                    return (Right db)
  where
    loadCache cachePath srcPath = do
        exists <- doesFileExist cachePath
        if not exists
            then return Nothing
            else
                catch
                    ( do
                        cacheTime <- getModificationTime cachePath
                        srcTime <- getModificationTime srcPath
                        if cacheTime < srcTime
                            then return Nothing
                            else do
                                compressed <- BS.readFile cachePath
                                case Zstd.decompress compressed of
                                    Zstd.Decompress raw -> do
                                        let !db = decodeEx raw
                                        result <- evaluate (force db)
                                        return (Just result)
                                    _ -> return Nothing
                    )
                    (\(_ :: SomeException) -> return Nothing)
    saveCache cachePath db =
        catch
            (BS.writeFile cachePath (Zstd.compress 1 (encode db)))
            (\(_ :: SomeException) -> return ())

-- | Merge multiple SynonymDBs into one (later entries take priority on ID conflicts).
mergeSynonymDBs :: [SynonymDB] -> SynonymDB
mergeSynonymDBs [] = emptySynonymDB
mergeSynonymDBs [db] = db
mergeSynonymDBs dbs =
    let
        -- Collect all groups from all DBs, re-number them
        allGroups = concatMap (\db -> M.elems (synIdToNames db)) dbs
        numberedGroups = zip [0 ..] allGroups
        nameToId = M.fromList [(normalizeName name, gid) | (gid, names) <- numberedGroups, name <- names]
        idToNames = M.fromList numberedGroups
     in
        SynonymDB nameToId idToNames

-- | Number of synonym names in the database.
synonymCount :: SynonymDB -> Int
synonymCount = M.size . synNameToId

{- | Normalize a name for lookup in the synonym database

Normalization rules:
- Lowercase
- Strip leading/trailing whitespace
- Collapse multiple spaces to single space
- Strip ", in ground" suffix (ecoinvent resource naming)
- Strip "/kg" suffix (SimaPro unit convention)
- Remove punctuation: commas, parentheses, quotes
-}
normalizeName :: Text -> Text
normalizeName name =
    let
        -- Lowercase and strip
        t1 = T.strip $ T.toLower name
        -- Collapse whitespace
        t2 = T.unwords $ T.words t1
        -- Strip ", in ground" suffix
        t3 = stripSuffix ", in ground" $ stripSuffix " in ground" t2
        -- Strip "/kg" suffix
        t4 = stripSuffix "/kg" t3
        -- Remove punctuation
        t5 = T.filter (`notElem` (",()'\"" :: String)) t4
        -- Collapse whitespace again (from removed punctuation)
        t6 = T.unwords $ T.words t5
     in
        t6
  where
    stripSuffix :: Text -> Text -> Text
    stripSuffix suffix txt =
        if suffix `T.isSuffixOf` txt
            then T.dropEnd (T.length suffix) txt
            else txt

-- | Look up the synonym group ID for a flow name
lookupSynonymGroup :: SynonymDB -> Text -> Maybe Int
lookupSynonymGroup db name =
    M.lookup (normalizeName name) (synNameToId db)

-- | Get all synonyms for a group ID
getSynonyms :: SynonymDB -> Int -> Maybe [Text]
getSynonyms db gid = M.lookup gid (synIdToNames db)
