{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Synonym Database Compiler

Two modes:
1. JSON mode: compile existing JSON (from Python build_synonyms_db.py)
2. Sources mode: build from ILCD flow XMLs + pair CSVs directly

Usage:
  # Mode 1: JSON input
  cabal run synonyms-compiler -- --input synonyms_db.json --output synonyms.bin.zst

  # Mode 2: Build from sources
  cabal run synonyms-compiler -- --from-sources synonyms.toml --output synonyms.bin.zst
-}
module Main where

import qualified Codec.Compression.Zstd as Zstd
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BSL
import Data.Char (toLower)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import qualified Data.Set as S
import qualified Data.Store as Store
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import Options.Applicative
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

import EcoSpold.Parser2 (normalizeCAS)
import SynonymDB (normalizeName)
import SynonymDB.Types (SynonymDB (..))

-- | Shared output options
data OutputOpts = OutputOpts
    { optOutput :: !FilePath
    , optLevel :: !Int
    }
    deriving (Show)

-- | Command line options
data Options
    = JSONMode !FilePath !OutputOpts -- input path
    | SourcesMode !FilePath !OutputOpts -- config path
    deriving (Show)

outputOpts :: Parser OutputOpts
outputOpts =
    OutputOpts
        <$> strOption (long "output" <> short 'o' <> metavar "FILE" <> help "Output binary file")
        <*> option
            auto
            ( long "level"
                <> short 'l'
                <> metavar "LEVEL"
                <> value 3
                <> help "Zstd compression level 1-22 (default: 3)"
            )

optParser :: Parser Options
optParser =
    subparser
        ( command "json" (info jsonParser (progDesc "Compile from JSON (legacy mode)"))
            <> command "sources" (info sourcesParser (progDesc "Build from ILCD flows + pair CSVs"))
        )
        <|> jsonParser -- default to JSON mode for backward compatibility

jsonParser :: Parser Options
jsonParser =
    JSONMode
        <$> strOption
            ( long "input"
                <> short 'i'
                <> metavar "FILE"
                <> help "Input JSON file (from build_synonyms_db.py)"
            )
        <*> outputOpts

sourcesParser :: Parser Options
sourcesParser =
    SourcesMode
        <$> strOption
            ( long "from-sources"
                <> short 's'
                <> metavar "FILE"
                <> help "TOML config listing synonym sources"
            )
        <*> outputOpts

--------------------------------------------------------------------------------
-- JSON Mode (legacy)
--------------------------------------------------------------------------------

-- | Parse the JSON structure into SynonymDB
parseJSONToSynonymDB :: A.Value -> Either String SynonymDB
parseJSONToSynonymDB (A.Object obj) = do
    nameToIdValue <-
        maybe
            (Left "Missing 'name_to_id' field")
            Right
            (KM.lookup "name_to_id" obj)
    nameToId <- parseNameToId nameToIdValue
    idToSynValue <-
        maybe
            (Left "Missing 'id_to_synonyms' field")
            Right
            (KM.lookup "id_to_synonyms" obj)
    idToNames <- parseIdToSynonyms idToSynValue
    Right $ SynonymDB{synNameToId = nameToId, synIdToNames = idToNames}
parseJSONToSynonymDB _ = Left "Expected JSON object at top level"

parseNameToId :: A.Value -> Either String (M.Map T.Text Int)
parseNameToId (A.Object obj) =
    let pairs = [(AK.toText k, v) | (k, v) <- KM.toList obj]
     in M.fromList <$> mapM parsePair pairs
  where
    parsePair (name, A.Number n) = Right (name, round n)
    parsePair (name, _) = Left $ "Expected number for key: " ++ T.unpack name
parseNameToId _ = Left "Expected object for name_to_id"

parseIdToSynonyms :: A.Value -> Either String (M.Map Int [T.Text])
parseIdToSynonyms (A.Object obj) =
    let pairs = [(AK.toText k, v) | (k, v) <- KM.toList obj]
     in M.fromList <$> mapM parsePair pairs
  where
    parsePair (idText, A.Array arr) = do
        let !gid = read (T.unpack idText) :: Int
        names <- mapM parseText (V.toList arr)
        Right (gid, names)
    parsePair (idText, _) = Left $ "Expected array for group: " ++ T.unpack idText
    parseText (A.String t) = Right t
    parseText _ = Left "Expected string in synonyms array"
parseIdToSynonyms _ = Left "Expected object for id_to_synonyms"

runJSONMode :: FilePath -> FilePath -> Int -> IO ()
runJSONMode input output level = do
    hPutStrLn stderr $ "Reading JSON from: " ++ input
    jsonBytes <- BSL.readFile input
    jsonValue <- case A.decode jsonBytes of
        Nothing -> fail "Failed to parse JSON"
        Just v -> return v
    db <- case parseJSONToSynonymDB jsonValue of
        Left err -> fail $ "Failed to convert JSON: " ++ err
        Right d -> return d
    writeDB output level db

--------------------------------------------------------------------------------
-- Sources Mode (new)
--------------------------------------------------------------------------------

-- | A synonym pair: two names that should be in the same group
type SynonymPair = (T.Text, T.Text)

-- | CAS-based group: CAS → set of names
type CASGroups = M.Map T.Text (S.Set T.Text)

{- | Build synonym DB from sources listed in a simple config file.
Config format (one source per line):
  ilcd-flows:<path>        # ILCD flows/ directory
  pairs-csv:<path>         # 2-column CSV: name1,name2
-}
runSourcesMode :: FilePath -> FilePath -> Int -> IO ()
runSourcesMode configPath output level = do
    hPutStrLn stderr $ "Reading sources config: " ++ configPath
    configLines <- T.lines <$> TIO.readFile configPath
    let sources = mapMaybe parseSourceLine configLines

    -- Phase 1: collect CAS groups from ILCD flow XMLs
    casGroups <-
        foldl' (M.unionWith S.union) M.empty
            <$> sequence [loadILCDFlowsCAS dir | ILCDFlowSource dir <- sources]

    -- Phase 2: collect name pairs from CSV files
    namePairs <-
        concat
            <$> sequence [loadPairsCSV path | PairsCSVSource path <- sources]

    -- Phase 3: build synonym groups
    let db = buildSynonymDB casGroups namePairs

    hPutStrLn stderr $
        printf
            "Built: %d names, %d groups"
            (M.size (synNameToId db))
            (M.size (synIdToNames db))

    -- Validate: no group > 100 names
    let bigGroups =
            [ (gid, length names)
            | (gid, names) <- M.toList (synIdToNames db)
            , length names > 100
            ]
    mapM_
        ( \(gid, sz) ->
            hPutStrLn stderr $
                printf "WARNING: Group %d has %d names (capped at 100)" gid sz
        )
        bigGroups

    writeDB output level db

-- | Source types
data Source
    = ILCDFlowSource !FilePath
    | PairsCSVSource !FilePath
    deriving (Show)

-- | Parse a source line from the config file
parseSourceLine :: T.Text -> Maybe Source
parseSourceLine line
    | T.null stripped || "#" `T.isPrefixOf` stripped = Nothing
    | Just path <- T.stripPrefix "ilcd-flows:" stripped = Just $ ILCDFlowSource (T.unpack $ T.strip path)
    | Just path <- T.stripPrefix "pairs-csv:" stripped = Just $ PairsCSVSource (T.unpack $ T.strip path)
    | otherwise = Nothing
  where
    stripped = T.strip line

-- | Load ILCD flow XMLs and group names by CAS number
loadILCDFlowsCAS :: FilePath -> IO CASGroups
loadILCDFlowsCAS dir = do
    exists <- doesDirectoryExist dir
    if not exists
        then do
            hPutStrLn stderr $ "  WARNING: ILCD flows directory not found: " ++ dir
            return M.empty
        else do
            files <- listDirectory dir
            let xmlFiles = [dir </> f | f <- files, map toLower (takeExtension f) == ".xml"]
            hPutStrLn stderr $ "  Loading " ++ show (length xmlFiles) ++ " ILCD flow XMLs from " ++ dir
            results <- mapM parseILCDFlowForSynonyms xmlFiles
            let groups =
                    foldl'
                        ( \acc (cas, name) ->
                            M.insertWith S.union cas (S.singleton (normalizeName name)) acc
                        )
                        M.empty
                        (catMaybes results)
            hPutStrLn stderr $ "  Found " ++ show (M.size groups) ++ " CAS groups"
            return groups

-- | Parse a single ILCD flow XML for CAS + baseName
parseILCDFlowForSynonyms :: FilePath -> IO (Maybe (T.Text, T.Text))
parseILCDFlowForSynonyms path = do
    bytes <- BS.readFile path
    return $ extractCASAndName bytes
  where
    -- Simple extraction: find <CASNumber> and <baseName> elements
    extractCASAndName :: BS.ByteString -> Maybe (T.Text, T.Text)
    extractCASAndName bs =
        let text = TE.decodeUtf8 bs
            cas = extractElement "CASNumber" text
            name = extractElement "baseName" text
         in case (cas, name) of
                (Just c, Just n) | not (T.null c) -> Just (normalizeCAS c, n)
                _ -> Nothing

    extractElement :: T.Text -> T.Text -> Maybe T.Text
    extractElement tag text =
        let open = "<" <> tag
            close = "</" <> tag <> ">"
         in case T.breakOn open text of
                (_, rest) | T.null rest -> Nothing
                (_, rest) ->
                    let afterOpen = T.drop 1 $ T.dropWhile (/= '>') rest
                        value = T.takeWhile (/= '<') afterOpen
                     in if close `T.isInfixOf` rest
                            then Just (T.strip value)
                            else Nothing

-- | Load pairs from a 2-column CSV (name1,name2)
loadPairsCSV :: FilePath -> IO [SynonymPair]
loadPairsCSV path = do
    exists <- doesFileExist path
    if not exists
        then do
            hPutStrLn stderr $ "  WARNING: Pairs CSV not found: " ++ path
            return []
        else do
            content <- BS.readFile path
            let rows = BC.lines content
                -- Skip header
                dataRows = drop 1 rows
                pairs = mapMaybe parsePairRow dataRows
            hPutStrLn stderr $ "  Loaded " ++ show (length pairs) ++ " pairs from " ++ path
            return pairs
  where
    parsePairRow row =
        let fields = BC.split ',' row
         in case fields of
                (a : b : _) ->
                    let name1 = T.strip $ TE.decodeUtf8 a
                        name2 = T.strip $ TE.decodeUtf8 b
                     in if T.null name1 || T.null name2
                            then Nothing
                            else Just (normalizeName name1, normalizeName name2)
                _ -> Nothing

--------------------------------------------------------------------------------
-- Synonym Group Builder
--------------------------------------------------------------------------------

{- | Build SynonymDB from CAS groups and name pairs.
Uses Union-Find with size cap (max 100 names per group).
-}
buildSynonymDB :: CASGroups -> [SynonymPair] -> SynonymDB
buildSynonymDB casGroups namePairs =
    let
        -- Step 1: Create initial groups from CAS
        casGroupsList = [(S.toList names) | names <- M.elems casGroups, S.size names >= 2]
        -- Step 2: Merge name pairs
        allGroups = mergeNamePairs namePairs casGroupsList
        -- Step 3: Build maps, enforcing size cap
        (nameToId, idToNames) = buildMaps allGroups
     in
        SynonymDB{synNameToId = nameToId, synIdToNames = idToNames}

-- | Merge name pairs into existing groups using simple Union-Find on Map
mergeNamePairs :: [SynonymPair] -> [[T.Text]] -> [[T.Text]]
mergeNamePairs pairs initialGroups =
    let
        -- Build name → group-id map from initial groups
        initialMap =
            M.fromList
                [(name, gid) | (gid, names) <- zip [0 ..] initialGroups, name <- names]
        initialGroupMap = M.fromList (zip [0 ..] (map S.fromList initialGroups))
        nextId = length initialGroups

        -- Process each pair
        (finalNameMap, finalGroupMap, _) = foldl' mergePair (initialMap, initialGroupMap, nextId) pairs
     in
        map S.toList (M.elems finalGroupMap)
  where
    mergePair (nameMap, groupMap, nid) (name1, name2) =
        case (M.lookup name1 nameMap, M.lookup name2 nameMap) of
            -- Both in same group: no-op
            (Just g1, Just g2) | g1 == g2 -> (nameMap, groupMap, nid)
            -- Both in different groups: merge smaller into larger (with cap)
            (Just g1, Just g2) ->
                let s1 = maybe S.empty id (M.lookup g1 groupMap)
                    s2 = maybe S.empty id (M.lookup g2 groupMap)
                    merged = S.union s1 s2
                 in if S.size merged > 100
                        then (nameMap, groupMap, nid) -- skip: would exceed cap
                        else
                            let newNameMap = foldl' (\m n -> M.insert n g1 m) nameMap (S.toList s2)
                                newGroupMap = M.insert g1 merged (M.delete g2 groupMap)
                             in (newNameMap, newGroupMap, nid)
            -- Only one exists: add the other to that group
            (Just g, Nothing) ->
                let s = M.findWithDefault S.empty g groupMap
                 in if S.size s >= 100
                        then (nameMap, groupMap, nid)
                        else (M.insert name2 g nameMap, M.insert g (S.insert name2 s) groupMap, nid)
            (Nothing, Just g) ->
                let s = M.findWithDefault S.empty g groupMap
                 in if S.size s >= 100
                        then (nameMap, groupMap, nid)
                        else (M.insert name1 g nameMap, M.insert g (S.insert name1 s) groupMap, nid)
            -- Neither exists: create new group
            (Nothing, Nothing) ->
                let newGroup = S.fromList [name1, name2]
                 in ( M.insert name1 nid (M.insert name2 nid nameMap)
                    , M.insert nid newGroup groupMap
                    , nid + 1
                    )

-- | Build final maps with sequential IDs
buildMaps :: [[T.Text]] -> (M.Map T.Text Int, M.Map Int [T.Text])
buildMaps groups =
    let numbered = zip [0 ..] [g | g <- groups, length g >= 2]
        nameToId = M.fromList [(name, gid) | (gid, names) <- numbered, name <- names]
        idToNames = M.fromList [(gid, names) | (gid, names) <- numbered]
     in (nameToId, idToNames)

-- | Write SynonymDB to compressed binary file
writeDB :: FilePath -> Int -> SynonymDB -> IO ()
writeDB output level db = do
    !db' <- evaluate (force db)
    let nameCount = M.size (synNameToId db')
        groupCount = M.size (synIdToNames db')
    hPutStrLn stderr $ printf "Final: %d names, %d synonym groups" nameCount groupCount

    let binary = Store.encode db'
        binarySize = BS.length binary
    hPutStrLn stderr $
        printf
            "Binary size: %d bytes (%.2f MB)"
            binarySize
            (fromIntegral binarySize / (1024 * 1024) :: Double)

    let compressed = Zstd.compress level binary
        compressedSize = BS.length compressed
        ratio = (fromIntegral compressedSize / fromIntegral binarySize) * 100 :: Double
    hPutStrLn stderr $
        printf
            "Compressed size: %d bytes (%.2f MB, %.1f%% of original)"
            compressedSize
            (fromIntegral compressedSize / (1024 * 1024) :: Double)
            ratio

    BS.writeFile output compressed
    hPutStrLn stderr $ "Written to: " ++ output
    hPutStrLn stderr "Done!"

main :: IO ()
main = do
    opts <-
        execParser $
            info
                (optParser <**> helper)
                ( fullDesc
                    <> progDesc "Compile synonym database to compressed binary"
                    <> header "synonyms-compiler - build synonyms DB for embedding"
                )
    case opts of
        JSONMode input (OutputOpts output level) -> runJSONMode input output level
        SourcesMode config (OutputOpts output level) -> runSourcesMode config output level
