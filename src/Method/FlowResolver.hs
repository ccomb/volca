{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Parse ILCD flow XML files to extract baseName, compartment, and CAS.

ILCD method packages contain a @flows/@ directory with one XML per flow.
Each XML has: UUID, baseName, elementaryFlowCategorization (compartment),
and optionally a CASNumber. This module builds a lookup map from UUID
to enrichment data, used to annotate MethodCFs during method parsing.

Parsed flow data is cached to disk (zstd-compressed) to avoid re-parsing
94K+ XML files on every startup (~30s → <1s).
-}
module Method.FlowResolver (
    ILCDFlowInfo (..),
    resolveFlowDirectory,
    parseFlowDirectory,
    parseFlowXML,

    -- * Pure helpers (exported for testing)
    parseCompartment,
) where

import qualified Codec.Compression.Zstd as Zstd
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (mapConcurrently)
import Control.DeepSeq (NFData, force)
import Control.Exception (SomeException, catch, evaluate)
import qualified Data.ByteString as BS
import Data.Char (toLower)
import qualified Data.Map.Strict as M
import Data.Store (Store, decodeEx, encode)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import GHC.Generics (Generic)
import System.Directory (doesDirectoryExist, doesFileExist, getModificationTime, listDirectory, removeFile)
import System.FilePath (takeExtension, (</>))
import qualified Xeno.SAX as X

import EcoSpold.Common (bsToText, distributeFiles, isElement)
import EcoSpold.Parser2 (normalizeCAS)
import Method.Types (Compartment (..))
import Progress (ProgressLevel (..), reportProgress)

-- | Enrichment data extracted from an ILCD flow XML
data ILCDFlowInfo = ILCDFlowInfo
    { ilcdBaseName :: !Text
    , ilcdCompartment :: !(Maybe Compartment)
    , ilcdCAS :: !(Maybe Text) -- normalized CAS
    , ilcdSynonyms :: ![Text] -- from <common:synonyms xml:lang="en">, semicolon-split
    , ilcdFlowType :: !Text -- "Elementary flow" / "Product flow" / "Waste flow"
    , ilcdFlowPropertyRef :: !(Maybe UUID) -- reference flow property UUID (for unit resolution)
    }
    deriving (Show, Generic, NFData, Store)

{- | Given a method directory (containing lciamethods/), find the sibling flows/ directory.
ILCD packages have structure: ILCD/{lciamethods/, flows/, ...}
-}
resolveFlowDirectory :: FilePath -> IO (Maybe FilePath)
resolveFlowDirectory methodDir = do
    -- methodDir is the directory containing method XMLs (e.g., .../ILCD/lciamethods)
    -- flows/ is a sibling: .../ILCD/flows
    let parent = takeParentDir methodDir
        flowsDir = parent </> "flows"
    exists <- doesDirectoryExist flowsDir
    return $ if exists then Just flowsDir else Nothing
  where
    -- Go up one directory level
    takeParentDir = reverse . dropWhile (/= '/') . dropWhile (== '/') . reverse

{- | Parse all flow XMLs in a directory, returning UUID → ILCDFlowInfo map.
  Uses a zstd-compressed cache to skip re-parsing on subsequent startups.
  Falls back to worker-based parallel parsing on cache miss.
-}
parseFlowDirectory :: FilePath -> IO (M.Map UUID ILCDFlowInfo)
parseFlowDirectory dir = do
    let cacheFile = flowCacheFile dir
    cached <- loadFlowCache cacheFile dir
    case cached of
        Just info -> return info
        Nothing -> do
            info <- parseFlowDirectoryFresh dir
            saveFlowCache cacheFile info
            return info

-- | Parse all flow XMLs from scratch using worker-based parallelism
parseFlowDirectoryFresh :: FilePath -> IO (M.Map UUID ILCDFlowInfo)
parseFlowDirectoryFresh dir = do
    files <- listDirectory dir
    let xmlFiles = [dir </> f | f <- files, map toLower (takeExtension f) == ".xml"]
    numWorkers <- getNumCapabilities
    let workers = distributeFiles numWorkers xmlFiles
    workerResults <- mapConcurrently parseWorker workers
    return $! M.unions workerResults
  where
    parseWorker paths = do
        results <- mapM parseOneFile paths
        return $! M.fromList [(uuid, info) | Just (uuid, info) <- results]
    parseOneFile path = do
        bytes <- BS.readFile path
        return $ parseFlowXML bytes

-- | Cache file path for a flows directory
flowCacheFile :: FilePath -> FilePath
flowCacheFile dir = dir </> ".volca.flows.cache.zst"

-- | Load flow info from cache if valid (file exists and is newer than directory)
loadFlowCache :: FilePath -> FilePath -> IO (Maybe (M.Map UUID ILCDFlowInfo))
loadFlowCache cacheFile dir = do
    exists <- doesFileExist cacheFile
    if not exists
        then return Nothing
        else
            catch
                ( do
                    cacheTime <- getModificationTime cacheFile
                    dirTime <- getModificationTime dir
                    if cacheTime < dirTime
                        then do
                            reportProgress Info "[FLOWS-CACHE] Stale, reparsing"
                            removeFile cacheFile
                            return Nothing
                        else do
                            compressed <- BS.readFile cacheFile
                            case Zstd.decompress compressed of
                                Zstd.Decompress raw -> do
                                    let !info = decodeEx raw
                                    result <- evaluate (force info)
                                    reportProgress Info $ "[FLOWS-CACHE] Loaded " <> show (M.size result) <> " flow definitions from cache"
                                    return (Just result)
                                _ -> do
                                    reportProgress Warning "[FLOWS-CACHE] Decompression failed, reparsing"
                                    removeFile cacheFile
                                    return Nothing
                )
                (\(_ :: SomeException) -> return Nothing)

-- | Save parsed flow info to cache
saveFlowCache :: FilePath -> M.Map UUID ILCDFlowInfo -> IO ()
saveFlowCache cacheFile info =
    catch
        ( do
            let serialized = encode info
                compressed = Zstd.compress 1 serialized
            BS.writeFile cacheFile compressed
            reportProgress Info $ "[FLOWS-CACHE] Saved " <> show (M.size info) <> " flow definitions to cache"
        )
        (\(_ :: SomeException) -> return ())

-- | SAX parse state for flow XML
data FlowParseState = FlowParseState
    { fpsUUID :: !Text
    , fpsBaseName :: !Text
    , fpsCAS :: !Text
    , fpsCategories :: ![Text] -- category level="0", "1", "2" in order
    , fpsPath :: ![BS.ByteString]
    , fpsTextAccum :: ![BS.ByteString]
    , fpsInBaseName :: !Bool
    , fpsSynonyms :: ![Text] -- accumulated synonym names
    , fpsInSynonyms :: !Bool -- inside <common:synonyms>
    , fpsLangIsEn :: !Bool -- current synonyms element has xml:lang="en"
    , fpsFlowType :: !Text -- <typeOfDataSet> text
    , fpsRefFlowPropIdx :: !Int -- referenceToReferenceFlowProperty index
    , fpsCurrentFPIdx :: !Int -- current flowProperty dataSetInternalID
    , fpsFlowPropRef :: !(Maybe UUID) -- resolved flow property UUID
    , fpsInFlowProperty :: !Bool -- inside <flowProperty>
    }

initialFlowState :: FlowParseState
initialFlowState = FlowParseState "" "" "" [] [] [] False [] False False "" 0 (-1) Nothing False

-- | Parse a single ILCD flow XML from bytes
parseFlowXML :: BS.ByteString -> Maybe (UUID, ILCDFlowInfo)
parseFlowXML bytes =
    case X.fold openTag attr endOpen txt closeTag cdata initialFlowState bytes of
        Left _ -> Nothing
        Right s -> buildFlowInfo s
  where
    openTag s tag =
        let !inSyn = isElement tag "synonyms" || fpsInSynonyms s
            !inBase = isElement tag "baseName" || fpsInBaseName s
            !inFP = isElement tag "flowProperty" || fpsInFlowProperty s
         in s
                { fpsPath = tag : fpsPath s
                , fpsTextAccum = []
                , fpsInBaseName = inBase
                , fpsInSynonyms = inSyn
                , fpsLangIsEn = if isElement tag "synonyms" then False else fpsLangIsEn s
                , fpsInFlowProperty = inFP
                }

    attr s name value
        | fpsInSynonyms s && isElement name "lang" && bsToText value == "en" =
            s{fpsLangIsEn = True}
        | isElement name "dataSetInternalID" && fpsInFlowProperty s =
            s
                { fpsCurrentFPIdx = case TR.decimal (bsToText value) of
                    Right (n, _) -> n
                    Left _ -> -1
                }
        | isElement name "refObjectId"
            && fpsInFlowProperty s
            && fpsCurrentFPIdx s == fpsRefFlowPropIdx s =
            s{fpsFlowPropRef = UUID.fromText (bsToText value)}
        | isElement name "level" = s
        | otherwise = s

    endOpen s _ = s

    txt s content =
        let trimmed = BS.dropWhile (== 32) $ BS.dropWhileEnd (== 32) content
         in if BS.null trimmed
                then s
                else s{fpsTextAccum = trimmed : fpsTextAccum s}

    closeTag s tag
        | isElement tag "UUID" && T.null (fpsUUID s) =
            s{fpsPath = dropPath s, fpsUUID = accum s, fpsTextAccum = []}
        | isElement tag "baseName" =
            s
                { fpsPath = dropPath s
                , fpsBaseName = accum s
                , fpsTextAccum = []
                , fpsInBaseName = False
                }
        | isElement tag "CASNumber" =
            s{fpsPath = dropPath s, fpsCAS = accum s, fpsTextAccum = []}
        | isElement tag "typeOfDataSet" && T.null (fpsFlowType s) =
            s{fpsPath = dropPath s, fpsFlowType = accum s, fpsTextAccum = []}
        | isElement tag "referenceToReferenceFlowProperty" =
            s
                { fpsPath = dropPath s
                , fpsRefFlowPropIdx = case TR.decimal (accum s) of
                    Right (n, _) -> n
                    Left _ -> 0
                , fpsTextAccum = []
                }
        | isElement tag "flowProperty" =
            s{fpsPath = dropPath s, fpsTextAccum = [], fpsInFlowProperty = False}
        | isElement tag "synonyms" =
            let !syns =
                    if fpsLangIsEn s
                        then filter (not . T.null) $ map T.strip $ T.splitOn ";" (accum s)
                        else []
             in s
                    { fpsPath = dropPath s
                    , fpsTextAccum = []
                    , fpsInSynonyms = False
                    , fpsLangIsEn = False
                    , fpsSynonyms = fpsSynonyms s ++ syns
                    }
        | isElement tag "category" =
            s
                { fpsPath = dropPath s
                , fpsCategories = fpsCategories s ++ [accum s]
                , fpsTextAccum = []
                }
        | otherwise =
            s{fpsPath = dropPath s, fpsTextAccum = []}

    cdata = txt

    dropPath s = case fpsPath s of
        (_ : rest) -> rest
        [] -> []

    accum s = T.strip $ T.concat $ reverse $ map bsToText (fpsTextAccum s)

    buildFlowInfo s = do
        uuid <- UUID.fromText (fpsUUID s)
        let baseName = fpsBaseName s
        if T.null baseName
            then Nothing
            else
                Just
                    ( uuid
                    , ILCDFlowInfo
                        { ilcdBaseName = baseName
                        , ilcdCompartment = parseCompartment (fpsCategories s)
                        , ilcdCAS =
                            if T.null (fpsCAS s)
                                then Nothing
                                else Just (normalizeCAS (fpsCAS s))
                        , ilcdSynonyms = fpsSynonyms s
                        , ilcdFlowType = fpsFlowType s
                        , ilcdFlowPropertyRef = fpsFlowPropRef s
                        }
                    )

{- | Parse compartment from ILCD category levels.
Level 0: "Emissions" or "Resources"
Level 1: "Emissions to air" → medium = "air"
Level 2: "Emissions to air, indoor" → subcompartment = "indoor"
-}
parseCompartment :: [Text] -> Maybe Compartment
parseCompartment [] = Nothing
parseCompartment cats =
    let
        -- Level 1 typically contains medium: "Emissions to air", "Emissions to water", etc.
        medium = case drop 1 cats of
            (lvl1 : _) -> extractMedium lvl1
            [] -> extractMedium (head cats) -- cats is non-empty ([] case handled above)
            -- Level 2 is the subcompartment
        subcomp = case drop 2 cats of
            (lvl2 : _) -> extractSubcompartment lvl2
            [] -> ""
     in
        if T.null medium
            then Nothing
            else Just (Compartment medium subcomp "")
  where
    -- Extract medium from "Emissions to air" → "air", "Resources from ground" → "natural resource"
    extractMedium txt
        | "Emissions to air" `T.isPrefixOf` txt = "air"
        | "Emissions to water" `T.isPrefixOf` txt = "water"
        | "Emissions to soil" `T.isPrefixOf` txt = "soil"
        | "Resources" `T.isPrefixOf` txt = "natural resource"
        | "Emissions to fresh water" `T.isPrefixOf` txt = "water"
        | "Emissions to sea water" `T.isPrefixOf` txt = "water"
        | otherwise = T.toLower $ T.strip txt

    -- Extract subcompartment: take the last part after comma or "to X,"
    extractSubcompartment txt
        | "Emissions to air, " `T.isPrefixOf` txt =
            T.strip $ T.drop (T.length "Emissions to air, ") txt
        | "Emissions to water, " `T.isPrefixOf` txt =
            T.strip $ T.drop (T.length "Emissions to water, ") txt
        | "Emissions to soil, " `T.isPrefixOf` txt =
            T.strip $ T.drop (T.length "Emissions to soil, ") txt
        | "Emissions to " `T.isPrefixOf` txt =
            T.strip $ T.drop (T.length "Emissions to ") txt
        | "Resources " `T.isPrefixOf` txt =
            T.strip $ T.drop (T.length "Resources ") txt
        | otherwise = T.toLower $ T.strip txt
