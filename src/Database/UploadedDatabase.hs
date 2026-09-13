{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.UploadedDatabase (
    -- * Types
    UploadMeta (..),
    DatabaseFormat (..),
    metaVersion,

    -- * Meta file operations
    readUploadMeta,
    writeUploadMeta,
    parseMetaToml,
    formatMetaToml,
    parseFormat,

    -- * Discovery
    discoverUploadedDatabases,
    discoverUploadedMethods,
    getDatabaseUploadsDir,
    getMethodUploadsDir,

    -- * Data directory
    getDataDir,
    isUploadedPath,
) where

import Control.Exception (SomeException, try)
import Control.Monad (filterM, forM)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath (splitDirectories, (</>))
import Text.Read (readMaybe)

-- Re-export DatabaseFormat from Database.Upload (single definition)
import Database.Upload (DatabaseFormat (..))
import Progress (ProgressLevel (..), reportProgress)
import Types (AllocationKey, allocationKeyText, parseAllocationKey)

-- | Metadata for an uploaded database
data UploadMeta = UploadMeta
    { umVersion :: !Int -- Meta format version (for future compatibility)
    , umDisplayName :: !Text -- Human-readable name
    , umDescription :: !(Maybe Text) -- Optional description
    , umFormat :: !DatabaseFormat -- Detected database format
    , umDataPath :: !FilePath -- Relative path to data within upload dir
    , umDepends :: ![Text]
    {- ^ Names of the databases this one draws suppliers from. The only durable
    record of the pin: it otherwise lives in the staging registry and inside
    the binary matrix cache, so a restart between staging and finalizing used
    to lose it silently. A file written before this field existed reads back
    with no dependencies, which is what it meant.
    -}
    , umSource :: !(Maybe Text)
    {- ^ The database this one is a copy of, when it is one. A copy has no
    files: 'umDataPath' points at the source's, and this says whose they are,
    which is what lets a delete refuse to take the files a copy still reads.
    A file written before this field existed is not a copy, which is what it
    meant.
    -}
    , umAllocation :: !AllocationKey
    {- ^ The key this database's multi-output blocks were divided under. The
    only durable record of it: a re-keyed database owns no files of its own,
    so nothing else on disk says the shares it loads are not the ones its
    source declares. A file written before this field existed reads back as
    'Declared', which is what it meant.
    -}
    }
    deriving (Show, Eq, Generic)

{- | The @meta.toml@ shape this engine writes, stamped by every writer.
Version 3 added @source@, which is what tells a copy from an upload; version 4
added @allocation@, without which a re-keyed database came back declared after
a restart. The parser reads every version, taking absent fields to mean what
their absence meant when they did not exist.
-}
metaVersion :: Int
metaVersion = 4

-- | Name of the metadata file in each upload directory
metaFileName :: FilePath
metaFileName = "meta.toml"

{- | Get the base data directory (uploads, cache, etc.)
Uses VOLCA_DATA_DIR env var, falls back to current directory
-}
getDataDir :: IO FilePath
getDataDir = do
    mdir <- lookupEnv "VOLCA_DATA_DIR"
    case mdir of
        Just d -> return d
        Nothing -> return "."

-- | Get the database uploads directory
getDatabaseUploadsDir :: IO FilePath
getDatabaseUploadsDir = do
    base <- getDataDir
    let dir = base </> "uploads" </> "databases"
    createDirectoryIfMissing True dir
    return dir

-- | Get the method uploads directory
getMethodUploadsDir :: IO FilePath
getMethodUploadsDir = do
    base <- getDataDir
    let dir = base </> "uploads" </> "methods"
    createDirectoryIfMissing True dir
    return dir

-- | Check if a path belongs to the uploads directory
isUploadedPath :: FilePath -> Bool
isUploadedPath path =
    "uploads" `elem` splitDirectories path

{- | Read meta.toml from an upload directory
Returns Nothing if file doesn't exist or can't be parsed
-}
readUploadMeta :: FilePath -> IO (Maybe UploadMeta)
readUploadMeta uploadDir = do
    let metaPath = uploadDir </> metaFileName
    exists <- doesFileExist metaPath
    if not exists
        then return Nothing
        else do
            result <- try $ TIO.readFile metaPath
            case result of
                Left (_ :: SomeException) -> return Nothing
                Right content -> return $ parseMetaToml content

-- | Write meta.toml to an upload directory
writeUploadMeta :: FilePath -> UploadMeta -> IO ()
writeUploadMeta uploadDir meta = do
    let metaPath = uploadDir </> metaFileName
    TIO.writeFile metaPath (formatMetaToml meta)

{- | Parse meta.toml content
Simple key=value parser (not a full TOML parser)
-}
parseMetaToml :: Text -> Maybe UploadMeta
parseMetaToml content = do
    let lines' = map T.strip $ T.lines content
        kvPairs =
            [ (T.strip k, v)
            | line <- lines'
            , not (T.null line)
            , not (T.isPrefixOf "#" line)
            , let (k, rest) = T.breakOn "=" line
            , not (T.null rest)
            , let v = T.strip $ T.drop 1 rest
            ]
        getValue key = lookup key kvPairs
        unquote = unescapeToml . T.dropAround (== '"')

    version <- getValue "version" >>= readMaybe . T.unpack
    displayName <- unquote <$> getValue "displayName"
    let description = unquote <$> getValue "description"
    format <- getValue "format" >>= parseFormat . unquote
    dataPath <- T.unpack . unquote <$> getValue "dataPath"

    -- A key nobody wrote is the declared one, which is what every file
    -- written before this field existed means. A key nobody can read stops
    -- the whole file: dividing a database on a guess would restate its
    -- inventory in silence, where a database the scan refuses is named in
    -- the log and still on disk for a binary that knows the key.
    allocation <-
        either (const Nothing) Just $
            parseAllocationKey (maybe "declared" unquote (getValue "allocation"))

    return
        UploadMeta
            { umVersion = version
            , umDisplayName = displayName
            , umDescription = description
            , umFormat = format
            , umDataPath = dataPath
            , umDepends = maybe [] parseStringList (getValue "depends")
            , umSource = unquote <$> getValue "source"
            , umAllocation = allocation
            }

{- | Undo the escaping 'formatMetaToml' writes, so a value survives the round
trip it is written for.

A Windows path is why this is not academic: its separators are backslashes,
which the writer doubles as TOML requires, and a reader that took the value
verbatim handed back a path with every separator twice over.
-}
unescapeToml :: Text -> Text
unescapeToml = T.pack . unescape . T.unpack
  where
    unescape ('\\' : '"' : rest) = '"' : unescape rest
    unescape ('\\' : '\\' : rest) = '\\' : unescape rest
    unescape ('\\' : 'n' : rest) = '\n' : unescape rest
    unescape (c : rest) = c : unescape rest
    unescape [] = []

{- | Read a TOML inline array of strings, @["a", "b"]@. Entries that are not
quoted strings are skipped rather than failing the whole file: the key is
additive metadata, and a malformed dependency list must not make an otherwise
good database undiscoverable. The writer's escapes (@\"@, @\\@) are not
decoded, which is fine for database names: they are slugs and cannot contain
either character.
-}
parseStringList :: Text -> [Text]
parseStringList raw =
    [ T.dropAround (== '"') item
    | item <- map T.strip (T.splitOn "," (T.dropAround (`elem` ("[]" :: String)) (T.strip raw)))
    , not (T.null item)
    , T.isPrefixOf "\"" item
    ]

{- | Parse a format string to a DatabaseFormat.
Inverse of 'formatMetaToml''s writer below – every slug it can write is read back
here. An unrecognized slug reads as 'UnknownFormat' rather than dropping the whole
collection, since the format is re-detected from the files anyway.
-}
parseFormat :: Text -> Maybe DatabaseFormat
parseFormat "ecospold2" = Just EcoSpold2
parseFormat "ecospold1" = Just EcoSpold1
parseFormat "simapro" = Just SimaProCSV
parseFormat "ilcd" = Just ILCDProcess
parseFormat "openlca-jsonld" = Just OpenLcaJsonLd
parseFormat "brightway-excel" = Just BrightwayExcel
parseFormat _ = Just UnknownFormat

-- | Format meta.toml content
formatMetaToml :: UploadMeta -> Text
formatMetaToml UploadMeta{..} =
    T.unlines $
        [ "version = " <> T.pack (show umVersion)
        , "displayName = " <> quote umDisplayName
        ]
            ++ maybe [] (\d -> ["description = " <> quote d]) umDescription
            ++ [ "format = " <> quote (formatToText umFormat)
               , "dataPath = " <> quote (T.pack umDataPath)
               , "depends = [" <> T.intercalate ", " (map quote umDepends) <> "]"
               , "allocation = " <> quote (allocationKeyText umAllocation)
               ]
            ++ maybe [] (\s -> ["source = " <> quote s]) umSource
  where
    quote t = "\"" <> escapeToml t <> "\""

    escapeToml = T.concatMap escape
      where
        escape '"' = "\\\""
        escape '\\' = "\\\\"
        escape '\n' = "\\n"
        escape c = T.singleton c

    formatToText EcoSpold2 = "ecospold2"
    formatToText EcoSpold1 = "ecospold1"
    formatToText SimaProCSV = "simapro"
    formatToText ILCDProcess = "ilcd"
    formatToText OpenLcaJsonLd = "openlca-jsonld"
    formatToText BrightwayExcel = "brightway-excel"
    formatToText UnknownFormat = "unknown"

-- | Scan a directory for subdirectories with meta.toml
scanUploadsIn :: FilePath -> IO [(Text, FilePath, UploadMeta)]
scanUploadsIn dir = do
    exists <- doesDirectoryExist dir
    if not exists
        then return []
        else do
            entries <- listDirectory dir
            let fullPaths = [(T.pack entry, dir </> entry) | entry <- entries]
            dirsOnly <- filterM (doesDirectoryExist . snd) fullPaths
            results <- forM dirsOnly $ \(slug, dirPath) -> do
                maybeMeta <- readUploadMeta dirPath
                case maybeMeta of
                    Just meta -> pure (Just (slug, dirPath, meta))
                    Nothing ->
                        Nothing
                            <$ reportProgress Warning (dirPath <> ": meta.toml could not be read, the database is left out")
            return (catMaybes results)

{- | Discover all uploaded databases by scanning the uploads directory
Scans ./uploads/databases/ first, then legacy ./uploads/ for backward compat
-}
discoverUploadedDatabases :: IO [(Text, FilePath, UploadMeta)]
discoverUploadedDatabases = do
    dbDir <- getDatabaseUploadsDir
    newResults <- scanUploadsIn dbDir
    if not (null newResults)
        then return newResults
        else do
            -- Legacy fallback: scan ./uploads/ directly
            base <- getDataDir
            let legacyDir = base </> "uploads"
            scanUploadsIn legacyDir

-- | Discover all uploaded methods by scanning the methods upload directory
discoverUploadedMethods :: IO [(Text, FilePath, UploadMeta)]
discoverUploadedMethods = do
    methodDir <- getMethodUploadsDir
    scanUploadsIn methodDir
