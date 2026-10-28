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
    readAdded,
    writeUploadMeta,
    parseMetaToml,
    parseMetaTomlWith,
    formatMetaToml,
    parseFormat,

    -- * Discovery
    discoverUploadedDatabases,
    discoverUploadedMethods,
    getDatabaseUploadsDir,
    getMethodUploadsDir,
    getRefDataUploadsDir,

    -- * Data directory
    getDataDir,
    isUploadedPath,
) where

import Control.Exception (SomeException, try)
import Control.Monad (filterM, forM)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath (splitDirectories, (</>))
import Text.Read (readMaybe)

-- Re-export DatabaseFormat from Database.Upload (single definition)

import Database.SystemModel (parseSystemModel)
import Database.Upload (DatabaseFormat (..))
import Progress (ProgressLevel (..), reportProgress)
import Types (AddedAt, Licence, LicenceKeys (..), Reading (..), Release (..), Requirement (..), SystemModel (..), addedText, allocationKeyText, fileReadingKey, licenceFromKeys, licenceKeys, modelOf, parseAdded, parseFileReading)

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
    , umBuiltIn :: !(Maybe Text)
    {- ^ The collection built into the engine a method copy reads, in place of
    a path: such a copy has no file to point at. A file written before this
    field existed reads none, which is what it meant.
    -}
    , umReading :: !Reading
    {- ^ How this database's multi-output blocks are divided: by its files,
    or, for a child, by the system model kept beside this file. The only
    durable record of it: a child owns no files of its own, so nothing else
    on disk says the shares it loads are not the ones its source declares. A
    file written before @allocation@ existed reads back as declared, which is
    what it meant; one naming a property is refused, since that reading no
    longer exists.
    -}
    , umLicence :: !Licence
    {- ^ The licence as its owner set it; a copy records its source's when it
    is made. A file written before this field existed has none, which is what
    it meant.
    -}
    , umRelease :: !(Maybe Release)
    {- ^ Which published database this is, as its owner declared. A file
    written before this field existed declares none, which is what it meant.
    -}
    , umRequires :: ![Requirement]
    {- ^ The releases a package said this database was built on, each with the
    database its reader accepted in its place, if any. A file written before
    this field existed requires none, which is what it meant.
    -}
    , umPackageMethod :: !(Maybe Text)
    {- ^ For a method collection read from an openLCA package, the method of it
    the collection holds; a copy keeps its source's. A file written before this
    field existed names none, which is what it meant.
    -}
    , umAdded :: !(Maybe AddedAt)
    {- ^ When the engine made it, set once by whatever creates it and kept by
    every rewrite. A file written before this field existed has none, which is
    what it meant, and so has one whose date nobody can read: the date is only
    shown, and a database is not refused over it.
    -}
    }
    deriving (Show, Eq, Generic)

{- | The @meta.toml@ shape this engine writes, stamped by every writer.
Version 3 added @source@, which is what tells a copy from an upload; version 4
added @allocation@, without which a re-keyed database came back declared after
a restart; version 5 added the licence (@licence@ for a standard one; @licence_text@,
@refuses@ and @attribution@ for an own one; a @downloads@ key, the switch
they replaced before any release, stops the file); version 6 added
@builtin@, the built-in collection a method copy reads; version 7 added the
release (@release_name@, @release_version@, @release_system_model@); version 8
added one @[[requires]]@ block per required release (@name@, @version@,
@system_model@, @substitute@), written after every other key since TOML puts
the keys that follow a header inside it. The parser reads every
version, taking absent fields to mean what their absence meant when they did
not exist. Version 9 writes @system_model_file@ in place of @allocation@
for a child, the model beside it, and no longer reads a property under
@allocation@. Version 10 added @added@, when the engine made it, to the second
and in UTC.
-}
metaVersion :: Int
metaVersion = 10

-- | The file a child's system model is kept in, beside its @meta.toml@.
systemModelFileName :: FilePath
systemModelFileName = "system-model.toml"

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

{- | Where one kind of uploaded reference data lives (@flow-synonyms@, @units@, ...).
Under the data directory like databases and methods, so that a container whose
working directory is not its data volume still finds, and keeps, what it wrote.
Not created: the callers that write do it.
-}
getRefDataUploadsDir :: FilePath -> IO FilePath
getRefDataUploadsDir subdir = do
    base <- getDataDir
    return (base </> "uploads" </> subdir)

-- | Check if a path belongs to the uploads directory
isUploadedPath :: FilePath -> Bool
isUploadedPath path =
    "uploads" `elem` splitDirectories path

{- | When the @meta.toml@ of a directory says it was made, whatever else the
file holds: a reference table writes a shorter file of its own, which
'readUploadMeta' would refuse for want of a format. Nothing when there is no
file, it cannot be read, or it names no moment.
-}
readAdded :: FilePath -> IO (Maybe AddedAt)
readAdded dir = do
    let path = dir </> metaFileName
    exists <- doesFileExist path
    if not exists
        then pure Nothing
        else do
            result <- try (TIO.readFile path)
            pure $ case result of
                Left (_ :: SomeException) -> Nothing
                Right content -> parseAdded . unquote =<< lookup "added" (fst (requiresBlocks content))

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
                Right content -> do
                    model <- readModel
                    let meta = parseMetaTomlWith model content
                    -- Say why, where the scan would only say it could not read the file.
                    case (meta, readingRefusal model content) of
                        (Nothing, Just why) -> reportProgress Warning (uploadDir <> ": " <> T.unpack why)
                        _ -> pure ()
                    return meta
  where
    readModel :: IO (Maybe Text)
    readModel = do
        let path = uploadDir </> systemModelFileName
        present <- doesFileExist path
        if present then Just <$> TIO.readFile path else pure Nothing

{- | Write meta.toml to an upload directory, and a child's system model
beside it, as its author wrote it.
-}
writeUploadMeta :: FilePath -> UploadMeta -> IO ()
writeUploadMeta uploadDir meta = do
    let metaPath = uploadDir </> metaFileName
    mapM_ (TIO.writeFile (uploadDir </> systemModelFileName) . smText) (modelOf (umReading meta))
    TIO.writeFile metaPath (formatMetaToml meta)

-- | 'parseMetaTomlWith' for a database that keeps no system model.
parseMetaToml :: Text -> Maybe UploadMeta
parseMetaToml = parseMetaTomlWith Nothing

{- | Why the reading a meta.toml names cannot be read, when that is what stops
it: a property no longer read from the files, a model missing or not parsing.
-}
readingRefusal :: Maybe Text -> Text -> Maybe Text
readingRefusal model content = either Just (const Nothing) (readingOf model (fst (requiresBlocks content)))

-- | The reading a meta.toml's keys name, with the system model kept beside it.
readingOf :: Maybe Text -> [(Text, Text)] -> Either Text Reading
readingOf model pairs = case lookup "system_model_file" pairs of
    Just _ -> maybe (Left ("its system model, " <> T.pack systemModelFileName <> ", is missing")) (fmap ByModel . parseSystemModel) model
    Nothing -> FromFiles <$> parseFileReading (maybe "declared" unquote (lookup "allocation" pairs))

{- | Parse meta.toml content, given the system model kept beside it if any.
Simple key=value parser (not a full TOML parser)
-}
parseMetaTomlWith :: Maybe Text -> Text -> Maybe UploadMeta
parseMetaTomlWith model content = do
    let (top, blocks) = requiresBlocks content
        getValue key = lookup key top
        -- A list or a flag nobody can read stops the file, as a bad key does.
        licenceKeysOf =
            LicenceKeys (unquote <$> getValue "licence") (unquote <$> getValue "licence_text")
                <$> traverse parseStrictStringList (getValue "refuses")
                <*> traverse parseBool (getValue "attribution")
                <*> traverse parseBool (getValue "accept")

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
    reading <- either (const Nothing) Just (readingOf model top)

    -- Same rule: a refusal nobody can read must not come back as granted.
    -- The downloads switch these keys replaced was never released, and stops
    -- the file rather than be read as no licence at all.
    licence <- case getValue "downloads" of
        Just _ -> Nothing
        Nothing -> either (const Nothing) Just . licenceFromKeys =<< licenceKeysOf

    -- A release missing its name or its version is no release: read as one,
    -- it would match whichever database shares the half that is there.
    release <- case (getValue "release_name", getValue "release_version") of
        (Nothing, Nothing) -> Just Nothing
        (Just name, Just ver) -> Just (Just Release{releaseName = unquote name, releaseVersion = unquote ver, releaseSystemModel = unquote <$> getValue "release_system_model"})
        _ -> Nothing

    -- Same rule again: a requirement read without its version would accept
    -- every release of that name.
    requires <- traverse requirementOf blocks

    return
        UploadMeta
            { umVersion = version
            , umDisplayName = displayName
            , umDescription = description
            , umFormat = format
            , umDataPath = dataPath
            , umDepends = maybe [] parseStringList (getValue "depends")
            , umSource = unquote <$> getValue "source"
            , umBuiltIn = unquote <$> getValue "builtin"
            , umReading = reading
            , umLicence = licence
            , umRelease = release
            , umRequires = requires
            , umPackageMethod = unquote <$> getValue "package_method"
            , umAdded = parseAdded . unquote =<< getValue "added"
            }

{- | The key-value pairs before the first @[[requires]]@ header, and those of
each block after one. A line-based reading, enough for the file the writer
below produces.
-}
requiresBlocks :: Text -> ([(Text, Text)], [[(Text, Text)]])
requiresBlocks content = (keyValues top, blocks rest)
  where
    (top, rest) = break (== requiresHeader) (filter meaningful (map T.strip (T.lines content)))

    blocks :: [Text] -> [[(Text, Text)]]
    blocks [] = []
    blocks (_ : ls) = let (block, more) = break (== requiresHeader) ls in keyValues block : blocks more

    meaningful :: Text -> Bool
    meaningful line = not (T.null line || T.isPrefixOf "#" line)

    keyValues :: [Text] -> [(Text, Text)]
    keyValues ls = [(T.strip k, T.strip (T.drop 1 v)) | (k, v) <- map (T.breakOn "=") ls, not (T.null v)]

requiresHeader :: Text
requiresHeader = "[[requires]]"

-- | A requirement needs its release's name and version; one missing either is no requirement.
requirementOf :: [(Text, Text)] -> Maybe Requirement
requirementOf keys = do
    name <- unquote <$> lookup "name" keys
    version <- unquote <$> lookup "version" keys
    pure
        Requirement
            { reqRelease = Release{releaseName = name, releaseVersion = version, releaseSystemModel = unquote <$> lookup "system_model" keys}
            , reqSubstitute = unquote <$> lookup "substitute" keys
            }

-- | One quote off each end: a value of its own may end in an escaped one.
unquote :: Text -> Text
unquote = unescapeToml . unwrap '"'

-- | The value between one delimiter at each end, or the value itself when it has none.
unwrap :: Char -> Text -> Text
unwrap c t = fromMaybe t (T.stripPrefix (T.singleton c) t >>= T.stripSuffix (T.singleton c))

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

-- | 'parseStringList' for a key where a dropped entry would change the meaning: any entry that is not a quoted string refuses the whole list.
parseStrictStringList :: Text -> Maybe [Text]
parseStrictStringList raw = case T.strip raw of
    inner | T.isPrefixOf "[" inner && T.isSuffixOf "]" inner -> traverse entry (filter (not . T.null) (map T.strip (T.splitOn "," (T.drop 1 (T.dropEnd 1 inner)))))
    _ -> Nothing
  where
    entry :: Text -> Maybe Text
    entry item
        | T.length item >= 2 && T.isPrefixOf "\"" item && T.isSuffixOf "\"" item = Just (T.drop 1 (T.dropEnd 1 item))
        | otherwise = Nothing

parseBool :: Text -> Maybe Bool
parseBool "true" = Just True
parseBool "false" = Just False
parseBool _ = Nothing

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
parseFormat "openlca-jsonld" = Just OpenLcaImpactCategory
parseFormat "openlca-package" = Just OpenLcaPackage
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
               , readingLine umReading
               ]
            ++ maybe [] (\s -> ["source = " <> quote s]) umSource
            ++ maybe [] (\b -> ["builtin = " <> quote b]) umBuiltIn
            ++ maybe [] (\m -> ["package_method = " <> quote m]) umPackageMethod
            ++ maybe [] (\added -> ["added = " <> quote (addedText added)]) umAdded
            ++ licenceLines (licenceKeys umLicence)
            ++ maybe [] releaseLines umRelease
            ++ concatMap requirementLines umRequires
  where
    readingLine :: Reading -> Text
    readingLine reading = case reading of
        FromFiles fr -> "allocation = " <> quote (allocationKeyText (fileReadingKey fr))
        ByModel _ -> "system_model_file = " <> quote (T.pack systemModelFileName)

    requirementLines :: Requirement -> [Text]
    requirementLines Requirement{reqRelease = Release{..}, ..} =
        ["", requiresHeader, "name = " <> quote releaseName, "version = " <> quote releaseVersion]
            ++ maybe [] (\m -> ["system_model = " <> quote m]) releaseSystemModel
            ++ maybe [] (\d -> ["substitute = " <> quote d]) reqSubstitute

    releaseLines :: Release -> [Text]
    releaseLines Release{..} =
        ["release_name = " <> quote releaseName, "release_version = " <> quote releaseVersion]
            ++ maybe [] (\m -> ["release_system_model = " <> quote m]) releaseSystemModel

    quote t = "\"" <> escapeToml t <> "\""

    licenceLines :: LicenceKeys -> [Text]
    licenceLines LicenceKeys{..} =
        maybe [] (\l -> ["licence = " <> quote l]) lkId
            ++ maybe [] (\t -> ["licence_text = " <> quote t]) lkText
            ++ maybe [] (\ps -> ["refuses = [" <> T.intercalate ", " (map quote ps) <> "]"]) lkRefuses
            ++ maybe [] (\a -> ["attribution = " <> if a then "true" else "false"]) lkAttribution
            ++ maybe [] (\a -> ["accept = " <> if a then "true" else "false"]) lkAccept

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
    formatToText OpenLcaImpactCategory = "openlca-jsonld"
    formatToText OpenLcaPackage = "openlca-package"
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
