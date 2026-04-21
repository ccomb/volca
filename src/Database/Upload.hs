{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Database.Upload (
    -- * Types
    UploadData (..),
    UploadResult (..),
    DatabaseFormat (..),
    ProgressEvent (..),

    -- * Upload handling
    handleUpload,
    extractArchiveFile,
    detectDatabaseFormat,
    findDataDirectory,
    findAllDataDirectories,
    countDataFilesIn,
    anyDataFilesIn,

    -- * Method directory detection
    findMethodDirectory,
    findAllMethodDirectories,
    countMethodFilesIn,
    anyMethodFilesIn,
    slugify,
) where

import Control.Exception (SomeException, try)
import Control.Monad (filterM, forM)
import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum, toLower)
import Data.List (isPrefixOf, isSuffixOf, sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, removeDirectoryRecursive, removeFile)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeExtension, (</>))
import System.Info (os)
import System.Process (readProcessWithExitCode)

-- | Detected database format
data DatabaseFormat
    = SimaProCSV -- SimaPro CSV export
    | EcoSpold1 -- EcoSpold v1 XML format
    | EcoSpold2 -- EcoSpold v2 XML format
    | ILCDProcess -- ILCD process dataset format
    | UnknownFormat -- Could not detect format
    deriving (Show, Eq, Generic)

instance ToJSON DatabaseFormat where
    toJSON EcoSpold2 = A.String "EcoSpold 2"
    toJSON EcoSpold1 = A.String "EcoSpold 1"
    toJSON SimaProCSV = A.String "SimaPro CSV"
    toJSON ILCDProcess = A.String "ILCD"
    toJSON UnknownFormat = A.String ""

instance FromJSON DatabaseFormat where
    parseJSON = withText "DatabaseFormat" $ \case
        "EcoSpold 2" -> pure EcoSpold2
        "EcoSpold 1" -> pure EcoSpold1
        "SimaPro CSV" -> pure SimaProCSV
        "ILCD" -> pure ILCDProcess
        _ -> pure UnknownFormat

-- | Progress event for upload/loading operations
data ProgressEvent = ProgressEvent
    { pePhase :: !Text -- Current phase (extracting, parsing, etc.)
    , pePercent :: !Int -- Progress percentage (0-100)
    , peMessage :: !Text -- Human-readable message
    }
    deriving (Show, Eq, Generic)

-- | Data for database upload
data UploadData = UploadData
    { udName :: !Text -- User-provided name
    , udDescription :: !(Maybe Text) -- Optional description
    , udZipData :: !BL.ByteString -- ZIP file content
    }
    deriving (Show, Generic)

-- | Result of successful upload
data UploadResult = UploadResult
    { urSlug :: !Text -- Generated slug (URL-safe identifier)
    , urPath :: !FilePath -- Path to extracted data
    , urFormat :: !DatabaseFormat -- Detected format
    , urFileCount :: !Int -- Number of data files
    }
    deriving (Show, Eq, Generic)

{- | Handle a database upload
Extracts ZIP archive to uploadsDir/{slug}/ and detects format
-}
handleUpload :: FilePath -> UploadData -> (ProgressEvent -> IO ()) -> IO (Either Text UploadResult)
handleUpload uploadsDir UploadData{..} reportProgress = do
    let slug = slugify udName

    -- Check for valid slug
    if T.null slug
        then return $ Left "Invalid database name - cannot generate slug"
        else do
            let targetDir = uploadsDir </> T.unpack slug

            -- Check if directory already exists
            exists <- doesDirectoryExist targetDir
            if exists
                then return $ Left $ "Database already exists: " <> slug
                else do
                    -- Create target directory
                    createDirectoryIfMissing True targetDir

                    reportProgress $ ProgressEvent "extracting" 10 "Extracting archive..."

                    result <- try $ extractUpload udZipData targetDir
                    case result of
                        Left (e :: SomeException) -> do
                            _ <- try @SomeException $ removeDirectoryRecursive targetDir
                            return $ Left $ "Failed to extract archive: " <> T.pack (show e)
                        Right (Left err) -> do
                            _ <- try @SomeException $ removeDirectoryRecursive targetDir
                            return $ Left err
                        Right (Right ()) -> do
                            -- Find the actual data directory (may be nested)
                            reportProgress $ ProgressEvent "locating" 70 "Locating data files..."
                            dataDir <- findDataDirectory targetDir

                            -- Detect format
                            reportProgress $ ProgressEvent "detecting" 80 "Detecting database format..."
                            format <- detectDatabaseFormat dataDir

                            -- Count data files
                            reportProgress $ ProgressEvent "counting" 90 "Counting data files..."
                            fileCount <- countDataFiles dataDir format

                            reportProgress $ ProgressEvent "complete" 100 "Upload complete!"

                            return $
                                Right
                                    UploadResult
                                        { urSlug = slug
                                        , urPath = dataDir
                                        , urFormat = format
                                        , urFileCount = fileCount
                                        }

-- | Supported archive format (detected by magic bytes)
data ArchiveFormat
    = ArchiveZip -- ZIP: 50 4B (PK)
    | Archive7z -- 7z:  37 7A BC AF 27 1C
    | ArchiveGzip -- gzip (tar.gz): 1F 8B
    | ArchiveXz -- XZ (tar.xz): FD 37 7A 58 5A 00
    | ArchivePlainXML -- Plain XML file (EcoSpold1)
    | ArchivePlainCSV
    | ArchiveUnknown
    deriving (Show, Eq)

-- | Detect archive format from magic bytes
detectArchiveFormat :: BL.ByteString -> ArchiveFormat
detectArchiveFormat content
    | BL.null content = ArchiveUnknown
    | matchesMagic [0x50, 0x4B] = ArchiveZip
    | matchesMagic [0x37, 0x7A, 0xBC, 0xAF, 0x27, 0x1C] = Archive7z
    | matchesMagic [0x1F, 0x8B] = ArchiveGzip
    | matchesMagic [0xFD, 0x37, 0x7A, 0x58, 0x5A, 0x00] = ArchiveXz
    | isXmlFile = ArchivePlainXML -- Check XML before plain text
    | isPlainText = ArchivePlainCSV
    | otherwise = ArchiveUnknown
  where
    bytes = BL.unpack (BL.take 10 content)
    matchesMagic magic = take (length magic) bytes == magic
    -- Check for XML declaration "<?xml" (0x3C 0x3F 0x78 0x6D 0x6C)
    -- or "<ecoSpold" (0x3C 0x65 0x63 0x6F)
    -- or UTF-8 BOM (0xEF 0xBB 0xBF) followed by "<?xml"
    isXmlFile =
        matchesMagic [0x3C, 0x3F, 0x78, 0x6D, 0x6C]
            || matchesMagic [0x3C, 0x65, 0x63, 0x6F]
            || matchesMagic [0xEF, 0xBB, 0xBF, 0x3C, 0x3F]
    -- Check if first byte is printable ASCII (plain text / CSV)
    isPlainText = let b = BL.head content in b == 0x7B || (b >= 0x20 && b < 0x7F)

{- | Extract an archive file on disk to a target directory.
Reuses extractUpload (reads file, detects format, extracts).
-}
extractArchiveFile :: FilePath -> FilePath -> IO (Either Text ())
extractArchiveFile archivePath targetDir = do
    content <- BL.readFile archivePath
    extractUpload content targetDir

{- | Extract upload data to target directory.
Supports ZIP, 7z, tar.gz, tar.xz archives (auto-detected), plain XML, and CSV files.
-}
extractUpload :: BL.ByteString -> FilePath -> IO (Either Text ())
extractUpload content targetDir = do
    let format = detectArchiveFormat content
    case format of
        ArchivePlainXML -> do
            -- Plain XML: write directly (e.g., EcoSpold1 multi-dataset file)
            BL.writeFile (targetDir </> "data.xml") content
            return $ Right ()
        ArchivePlainCSV -> do
            -- Plain CSV: write directly
            BL.writeFile (targetDir </> "data.csv") content
            return $ Right ()
        ArchiveUnknown ->
            return $ Left "Unsupported file format. Please upload a ZIP, 7z, tar.gz, tar.xz archive, XML, or CSV file."
        Archive7z -> extract7z content targetDir
        ArchiveZip -> extractZip content targetDir
        ArchiveGzip -> extractArchive ArchiveGzip content targetDir
        ArchiveXz -> extractArchive ArchiveXz content targetDir

{- | Extract 7z archive using external 7z command
The Haskell libarchive package doesn't properly support 7z extraction
-}
extract7z :: BL.ByteString -> FilePath -> IO (Either Text ())
extract7z archiveData targetDir = do
    -- Write archive to a temp file
    let tempArchive = targetDir </> ".temp.7z"
    BL.writeFile tempArchive archiveData

    -- Get list of 7z commands to try (platform-specific)
    -- Checks for bundled binary first, then system paths
    commands <- get7zCommands

    -- Try each command until one works
    result <- tryCommands commands tempArchive targetDir

    -- Clean up temp file
    _ <- try @SomeException $ removeFile tempArchive

    case result of
        Left err -> return $ Left err
        Right () -> do
            -- Verify files were extracted
            files <- listDirectory targetDir
            -- Filter out any remaining temp files
            let actualFiles = filter (not . isPrefixOf ".temp") files
            if null actualFiles
                then return $ Left "7z extraction produced no files. The archive may be empty or corrupted."
                else return $ Right ()
  where
    -- Get platform-specific 7z command candidates
    -- Checks for bundled binary in VOLCA_DATA_DIR first, then system paths
    get7zCommands :: IO [FilePath]
    get7zCommands = do
        mDataDir <- lookupEnv "VOLCA_DATA_DIR"
        let bundledPaths = case mDataDir of
                Just d ->
                    if isWindows
                        then [d </> "7z.exe", d </> "7z"]
                        else [d </> "7zz", d </> "7z"]
                Nothing -> []
        return $ bundledPaths ++ systemPaths
      where
        isWindows = os == "mingw32" || os == "windows"

        systemPaths
            | isWindows = windowsCommands
            | otherwise = unixCommands

        -- Windows: try common installation paths
        windowsCommands =
            [ "7z" -- If in PATH
            , "7z.exe"
            , "C:\\Program Files\\7-Zip\\7z.exe"
            , "C:\\Program Files (x86)\\7-Zip\\7z.exe"
            ]

        -- Unix/Linux/macOS: try various package names
        unixCommands =
            [ "7zz" -- 7-Zip standalone (newer)
            , "7z" -- p7zip-full
            , "7zr" -- p7zip (reduced, but supports 7z)
            , "7za" -- p7zip standalone
            ]

    tryCommands :: [FilePath] -> FilePath -> FilePath -> IO (Either Text ())
    tryCommands [] _ _ =
        return $
            Left $
                T.concat
                    [ "7z extraction failed. Please install 7-Zip:\n"
                    , if os == "mingw32" || os == "windows"
                        then "  Windows: Download from https://7-zip.org/"
                        else "  Linux: apt install 7zip (or p7zip-full)\n  macOS: brew install p7zip"
                    ]
    tryCommands (cmd : rest) archive target = do
        result <- tryExtractWith cmd archive target
        case result of
            Right () -> return $ Right ()
            Left _ -> tryCommands rest archive target

    tryExtractWith :: FilePath -> FilePath -> FilePath -> IO (Either Text ())
    tryExtractWith cmd archive target = do
        result <-
            try @SomeException $
                readProcessWithExitCode cmd ["x", "-y", "-o" ++ target, archive] ""
        case result of
            Left _ -> return $ Left "command not found"
            Right (ExitSuccess, _, _) -> return $ Right ()
            Right (ExitFailure _, _, stderr) -> return $ Left (T.pack stderr)

{- | Extract ZIP archive using external commands
The Haskell libarchive package produces null-byte files for ZIP extraction
-}
extractZip :: BL.ByteString -> FilePath -> IO (Either Text ())
extractZip archiveData targetDir = do
    let tempArchive = targetDir </> ".temp.zip"
    BL.writeFile tempArchive archiveData

    commands <- getUnzipCommands
    result <- tryZipCommands commands tempArchive targetDir

    _ <- try @SomeException $ removeFile tempArchive

    case result of
        Left err -> return $ Left err
        Right () -> do
            files <- listDirectory targetDir
            let actualFiles = filter (not . isPrefixOf ".temp") files
            if null actualFiles
                then return $ Left "ZIP extraction produced no files."
                else return $ Right ()
  where
    getUnzipCommands :: IO [(FilePath, [String] -> [String])]
    getUnzipCommands = do
        mDataDir <- lookupEnv "VOLCA_DATA_DIR"
        let bundled7z = case mDataDir of
                Just d ->
                    if isWindows
                        then [(d </> "7z.exe", args7z), (d </> "7z", args7z)]
                        else [(d </> "7zz", args7z), (d </> "7z", args7z)]
                Nothing -> []
        return $
            if isWindows
                then [("tar", argsTar)] ++ bundled7z ++ [("7z", args7z), ("7z.exe", args7z)]
                else [("unzip", argsUnzip)] ++ bundled7z ++ [("7zz", args7z), ("7z", args7z)]

    isWindows = os == "mingw32" || os == "windows"

    argsUnzip [archive, target] = ["-o", "-q", archive, "-d", target]
    argsUnzip _ = []
    argsTar [archive, target] = ["-xf", archive, "-C", target]
    argsTar _ = []
    args7z [archive, target] = ["x", "-y", "-o" ++ target, archive]
    args7z _ = []

    tryZipCommands :: [(FilePath, [String] -> [String])] -> FilePath -> FilePath -> IO (Either Text ())
    tryZipCommands [] _ _ =
        return $
            Left $
                T.concat
                    [ "ZIP extraction failed. Please install an extraction tool:\n"
                    , if isWindows
                        then "  Windows: tar should be built-in, or install 7-Zip from https://7-zip.org/"
                        else "  Linux: apt install unzip\n  macOS: brew install unzip (usually pre-installed)"
                    ]
    tryZipCommands ((cmd, mkArgs) : rest) archive target = do
        let args = mkArgs [archive, target]
        result <- try @SomeException $ readProcessWithExitCode cmd args ""
        case result of
            Left _ -> tryZipCommands rest archive target
            Right (ExitSuccess, _, _) -> return $ Right ()
            Right (ExitFailure _, _, _) -> tryZipCommands rest archive target

-- | Extract tar.gz/tar.xz archives using system tar command
extractArchive :: ArchiveFormat -> BL.ByteString -> FilePath -> IO (Either Text ())
extractArchive format archiveData targetDir = do
    let ext = case format of
            ArchiveGzip -> ".tar.gz"
            ArchiveXz -> ".tar.xz"
            ArchiveZip -> ".tar"
            Archive7z -> ".tar"
            ArchivePlainXML -> ".tar"
            ArchivePlainCSV -> ".tar"
            ArchiveUnknown -> ".tar"
    let tempArchive = targetDir </> ".temp" <> ext
    BL.writeFile tempArchive archiveData
    result <-
        try @SomeException $
            readProcessWithExitCode "tar" ["-xf", tempArchive, "-C", targetDir] ""
    _ <- try @SomeException $ removeFile tempArchive
    case result of
        Left e ->
            return $ Left $ "Failed to extract " <> formatName <> " archive: " <> T.pack (show e)
        Right (ExitSuccess, _, _) -> do
            files <- listDirectory targetDir
            let actualFiles = filter (not . isPrefixOf ".temp") files
            if null actualFiles
                then return $ Left $ formatName <> " extraction produced no files."
                else return $ Right ()
        Right (ExitFailure _, _, stderr) ->
            return $ Left $ formatName <> " extraction failed: " <> T.pack stderr
  where
    formatName = case format of
        ArchiveGzip -> "tar.gz"
        ArchiveXz -> "tar.xz"
        ArchiveZip -> "archive"
        Archive7z -> "archive"
        ArchivePlainXML -> "archive"
        ArchivePlainCSV -> "archive"
        ArchiveUnknown -> "archive"

{- | Find the actual data directory (picks the candidate with the most data files)
Archives often contain multiple folders (e.g., "datasets", "MasterData")
For ILCD: detects directories with a processes/ subdirectory first.
-}
findDataDirectory :: FilePath -> IO FilePath
findDataDirectory dir = do
    -- Check for ILCD format first (recursively find dirs with processes/ subdir)
    ilcdDir <- findILCDRoot dir
    case ilcdDir of
        Just d -> return d
        Nothing -> do
            candidates <- findAllDataDirectories dir
            case candidates of
                [] -> return dir
                [one] -> return one
                many -> pickByFileCount many
  where
    pickByFileCount dirs = do
        counts <- mapM (\d -> (,) d <$> countDataFilesIn d) dirs
        let sorted = sortOn (Down . snd) counts
        return $ fst (head sorted)

    -- Recursively find the first directory containing a processes/ subdirectory
    findILCDRoot d = do
        hasProcesses <- doesDirectoryExist (d </> "processes")
        if hasProcesses
            then return (Just d)
            else do
                entries <- listDirectory d
                subdirs <- filterM doesDirectoryExist (map (d </>) entries)
                findFirst subdirs

    findFirst [] = return Nothing
    findFirst (d : ds) = do
        r <- findILCDRoot d
        case r of
            Just _ -> return r
            Nothing -> findFirst ds

-- | Find all directories containing recognized data files under a root.
findAllDataDirectories :: FilePath -> IO [FilePath]
findAllDataDirectories root = go root
  where
    go dir = do
        result <- try @SomeException $ do
            hasData <- anyDataFilesIn dir
            entries <- listDirectory dir
            subdirs <- filterM doesDirectoryExist (map (dir </>) entries)
            childResults <- concat <$> mapM go subdirs
            if hasData
                then return (dir : childResults)
                else return childResults
        case result of
            Right paths -> return paths
            Left _ -> return [] -- skip inaccessible directories

-- | Count files with data extensions (.spold, .xml, .csv) in a single directory (non-recursive).
countDataFilesIn :: FilePath -> IO Int
countDataFilesIn dir = do
    fs <- listDirectory dir
    let exts = map (map toLower . takeExtension) fs
    return $ length $ filter isDataExt exts
  where
    isDataExt e = e == ".spold" || e == ".xml" || e == ".csv"

{- | Check if a directory contains any recognized data files directly.
Uses content-aware checks for XML and CSV to avoid false positives
(e.g. ILCD XML or non-SimaPro CSV files in sibling directories).
-}
anyDataFilesIn :: FilePath -> IO Bool
anyDataFilesIn d = do
    fs <- listDirectory d
    -- ILCD detection: directory contains a processes/ subdirectory
    hasProcessesDir <- doesDirectoryExist (d </> "processes")
    if hasProcessesDir
        then return True
        else do
            let fullPaths = map (d </>) fs
                extensions = map (map toLower . takeExtension) fs
            if any (== ".spold") extensions
                then return True
                else do
                    let xmlFiles = [p | p <- fullPaths, map toLower (takeExtension p) == ".xml"]
                    isEcoSpold <- if null xmlFiles then return False else checkForEcoSpold1 xmlFiles
                    if isEcoSpold
                        then return True
                        else do
                            let csvFiles = [p | p <- fullPaths, map toLower (takeExtension p) == ".csv"]
                            if null csvFiles then return False else checkForSimaProCSV csvFiles

{- | Find the best directory containing ILCD method XML files.
Picks the candidate with the most method files (same pattern as findDataDirectory).
-}
findMethodDirectory :: FilePath -> IO FilePath
findMethodDirectory dir = do
    candidates <- findAllMethodDirectories dir
    case candidates of
        [] -> return dir
        [one] -> return one
        many -> do
            counts <- mapM (\d -> (,) d <$> countMethodFilesIn d) many
            return $ fst $ head $ sortOn (Down . snd) counts

-- | Find all directories containing ILCD method XML files under a root.
findAllMethodDirectories :: FilePath -> IO [FilePath]
findAllMethodDirectories root = go root
  where
    go dir = do
        hasMethod <- anyMethodFilesIn dir
        entries <- listDirectory dir
        subdirs <- filterM doesDirectoryExist (map (dir </>) entries)
        childResults <- concat <$> mapM go subdirs
        if hasMethod then return (dir : childResults) else return childResults

-- | Count method files (ILCD XML or CSV) in a single directory (non-recursive).
countMethodFilesIn :: FilePath -> IO Int
countMethodFilesIn dir = do
    fs <- listDirectory dir
    let xmlFiles = [dir </> f | f <- fs, map toLower (takeExtension f) == ".xml"]
        csvCount = length [f | f <- fs, map toLower (takeExtension f) == ".csv"]
    xmlCount <- sum <$> mapM (\f -> do b <- isMethodXml f; return $ if b then 1 else 0) xmlFiles
    return $ xmlCount + csvCount

{- | Check if a directory contains method files (ILCD XML or CSV) directly.
Content-aware for XML: reads the first few bytes to check for
LCIAMethodDataSet marker, avoiding false positives on contacts/, flows/, etc.
-}
anyMethodFilesIn :: FilePath -> IO Bool
anyMethodFilesIn d = do
    fs <- listDirectory d
    let csvFiles = [f | f <- fs, map toLower (takeExtension f) == ".csv"]
        xmlFiles = [d </> f | f <- fs, map toLower (takeExtension f) == ".xml"]
    if not (null csvFiles)
        then return True
        else case xmlFiles of
            [] -> return False
            (f : _) -> isMethodXml f

-- | Check if an XML file is an ILCD LCIA method dataset
isMethodXml :: FilePath -> IO Bool
isMethodXml f = do
    result <- try $ BS.readFile f
    case result of
        Left (_ :: SomeException) -> return False
        Right content ->
            let header = BS.take 2048 content
             in return $
                    BS.isInfixOf "LCIAMethodDataSet" header
                        || BS.isInfixOf "lciamethods" header

-- | Count data files based on format
countDataFiles :: FilePath -> DatabaseFormat -> IO Int
countDataFiles d format = do
    fs <- listDirectoryRecursive d
    return $ length $ filter (isDataFile format) fs
  where
    isDataFile SimaProCSV f = ".csv" `isSuffixOf` map toLower f
    isDataFile EcoSpold1 f = ".xml" `isSuffixOf` map toLower f
    isDataFile EcoSpold2 f = ".spold" `isSuffixOf` map toLower f
    isDataFile ILCDProcess f = ".xml" `isSuffixOf` map toLower f
    isDataFile UnknownFormat _ = True

-- | Recursively list all files in a directory
listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive d = do
    entries <- listDirectory d
    results <- forM entries $ \entry -> do
        let path = d </> entry
        isDir <- doesDirectoryExist path
        if isDir
            then listDirectoryRecursive path
            else return [path]
    return $ concat results

-- | Detect database format from extracted files or a single file
detectDatabaseFormat :: FilePath -> IO DatabaseFormat
detectDatabaseFormat path = do
    isFile <- doesFileExist path
    isDir <- doesDirectoryExist path

    if isFile
        then do
            let ext = map toLower (takeExtension path)
            case ext of
                ".spold" -> return EcoSpold2
                ".xml" -> do
                    isEcoSpold1 <- checkForEcoSpold1 [path]
                    return $ if isEcoSpold1 then EcoSpold1 else UnknownFormat
                ".csv" -> do
                    isSimaPro <- checkForSimaProCSV [path]
                    return $ if isSimaPro then SimaProCSV else UnknownFormat
                _ -> return UnknownFormat
        else
            if isDir
                then do
                    -- Check for ILCD format: has a processes/ subdirectory
                    hasProcesses <- doesDirectoryExist (path </> "processes")
                    if hasProcesses
                        then return ILCDProcess
                        else do
                            fs <- listDirectoryRecursive path
                            let extensions = map (map toLower . takeExtension) fs
                            let hasSpold = any (== ".spold") extensions
                                hasXml = any (== ".xml") extensions
                                hasCsv = any (== ".csv") extensions
                            if hasSpold
                                then return EcoSpold2
                                else
                                    if hasXml
                                        then do
                                            isEcoSpold1 <- checkForEcoSpold1 fs
                                            return $ if isEcoSpold1 then EcoSpold1 else UnknownFormat
                                        else
                                            if hasCsv
                                                then do
                                                    isSimaPro <- checkForSimaProCSV fs
                                                    return $ if isSimaPro then SimaProCSV else UnknownFormat
                                                else return UnknownFormat
                else return UnknownFormat

-- | Check if XML files are EcoSpold1 format
checkForEcoSpold1 :: [FilePath] -> IO Bool
checkForEcoSpold1 fs = do
    let xmlFiles = filter (\f -> ".xml" `isSuffixOf` map toLower f) fs
    case xmlFiles of
        [] -> return False
        (f : _) -> do
            result <- try $ TIO.readFile f
            case result of
                Left (_ :: SomeException) -> return False
                Right c ->
                    return $ T.isInfixOf "EcoSpold01" c || T.isInfixOf "ecoSpold" c

{- | Check if CSV files are SimaPro format
Uses ByteString to avoid UTF-8 encoding issues (SimaPro files often use Latin1)
-}
checkForSimaProCSV :: [FilePath] -> IO Bool
checkForSimaProCSV fs = do
    let csvFiles = filter (\f -> ".csv" `isSuffixOf` map toLower f) fs
    case csvFiles of
        [] -> return False
        (f : _) -> do
            result <- try $ BS.readFile f
            case result of
                Left (_ :: SomeException) -> return False
                Right c ->
                    return $ BS.isPrefixOf "{" c

-- | Convert name to URL-safe slug
slugify :: Text -> Text
slugify = T.intercalate "-" . filter (not . T.null) . T.split (not . isValidSlugChar) . T.toLower
  where
    isValidSlugChar c = isAlphaNum c || c == '-' || c == '_'
