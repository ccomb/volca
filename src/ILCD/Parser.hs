{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Parse ILCD process datasets into VoLCA's SimpleDatabase.

ILCD packages have structure: ILCD/{processes/, flows/, flowproperties/, unitgroups/}
This module parses all four to build a complete activity database.
-}
module ILCD.Parser (
    parseILCDDirectory,
    parseProcessXML,
    ILCDProcessRaw (..),
    ILCDExchangeRaw (..),
    buildSupplierIndex,
    fixActivityExchanges,
) where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (mapConcurrently)
import qualified Data.ByteString as BS
import Data.Char (toLower)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.UUID as UUID
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))
import Text.Printf (printf)
import qualified Xeno.SAX as X

import EcoSpold.Common (bsToText, distributeFiles, isElement)
import Method.FlowResolver (ILCDFlowInfo (..), parseFlowDirectory)
import Method.Types (Compartment (..))
import Progress (ProgressLevel (..), reportProgress)
import Types

-- | Raw parsed ILCD process (before conversion to VoLCA types)
data ILCDProcessRaw = ILCDProcessRaw
    { iprUUID :: !UUID
    , iprName :: !Text
    , iprLocation :: !Text
    , iprRefFlowIdx :: !Int -- dataSetInternalID of reference exchange
    , iprExchanges :: ![ILCDExchangeRaw]
    , iprClassifications :: !(M.Map Text Text)
    }

data ILCDExchangeRaw = ILCDExchangeRaw
    { ierInternalId :: !Int
    , ierFlowRef :: !UUID
    , ierDirection :: !Text -- "Input" / "Output"
    , ierAmount :: !Double
    , ierLocation :: !Text
    }

{- | Parse an ILCD directory into a SimpleDatabase.
Expects subdirectories: processes/, flows/, flowproperties/, unitgroups/
-}
parseILCDDirectory :: FilePath -> IO (Either Text SimpleDatabase)
parseILCDDirectory dir = do
    reportProgress Info $ "Loading ILCD database from: " ++ dir

    -- Step 1: Parse unit groups and flow properties (small, sequential)
    unitGroupMap <- parseUnitGroups (dir </> "unitgroups")
    flowPropMap <- parseFlowProperties (dir </> "flowproperties")
    reportProgress Info $ printf "Parsed %d unit groups, %d flow properties" (M.size unitGroupMap) (M.size flowPropMap)

    -- Step 2: Parse flows (reuse FlowResolver, parallel + cached)
    flowInfoMap <- parseFlowDirectory (dir </> "flows")
    reportProgress Info $ printf "Parsed %d flows" (M.size flowInfoMap)

    -- Step 3: Build FlowDB and UnitDB from parsed data
    let (flowDB, unitDB) = buildFlowAndUnitDB flowInfoMap flowPropMap unitGroupMap

    -- Step 4: Parse process XMLs in parallel
    processFiles <- listXMLFiles (dir </> "processes")
    reportProgress Info $ printf "Parsing %d ILCD process files..." (length processFiles)
    rawProcesses <- parseProcessFilesParallel processFiles

    reportProgress Info $ printf "Parsed %d processes, building activity map..." (length rawProcesses)

    -- Step 5: Build ActivityMap
    let activityMap = buildActivityMap flowInfoMap flowDB unitDB rawProcesses

    -- Step 6: Fix supplier links (name-based, like SimaPro)
    let simpleDb = SimpleDatabase activityMap flowDB unitDB
    fixedDb <- fixILCDActivityLinks simpleDb
    reportProgress Info $
        printf
            "ILCD database loaded: %d activities, %d flows, %d units"
            (M.size $ sdbActivities fixedDb)
            (M.size $ sdbFlows fixedDb)
            (M.size $ sdbUnits fixedDb)
    return $ Right fixedDb

-- | List XML files in a directory
listXMLFiles :: FilePath -> IO [FilePath]
listXMLFiles d = do
    fs <- listDirectory d
    return [d </> f | f <- fs, map toLower (takeExtension f) == ".xml"]

--------------------------------------------------------------------------------
-- Unit Groups: unitGroupUUID → (refUnitName, refUnitInternalId)
--------------------------------------------------------------------------------

parseUnitGroups :: FilePath -> IO (M.Map UUID (Text, Int))
parseUnitGroups dir = do
    files <- listXMLFiles dir
    results <- mapM (\f -> parseUnitGroupXML <$> BS.readFile f) files
    return $ M.fromList [r | Just r <- results]

data UGState = UGState
    { ugUUID :: !Text
    , ugRefIdx :: !Int
    , ugUnits :: ![(Int, Text)]
    , ugCurId :: !Int
    , ugCurName :: !Text
    , ugInUnit :: !Bool
    , ugTextAccum :: ![BS.ByteString]
    }

parseUnitGroupXML :: BS.ByteString -> Maybe (UUID, (Text, Int))
parseUnitGroupXML bytes =
    case X.fold openTag attr endOpen txt closeTag cdata (UGState "" 0 [] (-1) "" False []) bytes of
        Left _ -> Nothing
        Right s -> do
            uuid <- UUID.fromText (ugUUID s)
            case lookup (ugRefIdx s) (ugUnits s) of
                Just name -> Just (uuid, (name, ugRefIdx s))
                Nothing -> Nothing
  where
    openTag s tag
        | isElement tag "unit" = s{ugInUnit = True, ugTextAccum = [], ugCurId = -1, ugCurName = ""}
        | otherwise = s{ugTextAccum = []}

    attr s name value
        | isElement name "dataSetInternalID" && ugInUnit s =
            case TR.decimal (bsToText value) of Right (n, _) -> s{ugCurId = n}; Left _ -> s
        | otherwise = s

    endOpen s _ = s

    txt s content =
        let trimmed = BS.dropWhile (== 32) $ BS.dropWhileEnd (== 32) content
         in if BS.null trimmed then s else s{ugTextAccum = trimmed : ugTextAccum s}

    closeTag s tag
        | isElement tag "UUID" && T.null (ugUUID s) =
            s{ugUUID = accum s, ugTextAccum = []}
        | isElement tag "referenceToReferenceUnit" =
            case TR.decimal (accum s) of
                Right (n, _) -> s{ugRefIdx = n, ugTextAccum = []}
                Left _ -> s{ugTextAccum = []}
        | isElement tag "name" && ugInUnit s =
            s{ugCurName = accum s, ugTextAccum = []}
        | isElement tag "unit" =
            s
                { ugInUnit = False
                , ugTextAccum = []
                , ugUnits = (ugCurId s, ugCurName s) : ugUnits s
                }
        | otherwise = s{ugTextAccum = []}

    cdata = txt
    accum s = T.strip $ T.concat $ reverse $ map bsToText (ugTextAccum s)

--------------------------------------------------------------------------------
-- Flow Properties: flowPropertyUUID → unitGroupUUID
--------------------------------------------------------------------------------

parseFlowProperties :: FilePath -> IO (M.Map UUID UUID)
parseFlowProperties dir = do
    files <- listXMLFiles dir
    results <- mapM (\f -> parseFlowPropertyXML <$> BS.readFile f) files
    return $ M.fromList [r | Just r <- results]

data FPState = FPState
    { fpUUID :: !Text
    , fpUnitGroupRef :: !Text
    , fpInQuantRef :: !Bool
    , fpTextAccum :: ![BS.ByteString]
    }

parseFlowPropertyXML :: BS.ByteString -> Maybe (UUID, UUID)
parseFlowPropertyXML bytes =
    case X.fold openTag attr endOpen txt closeTag cdata (FPState "" "" False []) bytes of
        Left _ -> Nothing
        Right s -> (,) <$> UUID.fromText (fpUUID s) <*> UUID.fromText (fpUnitGroupRef s)
  where
    openTag s tag
        | isElement tag "quantitativeReference" = s{fpInQuantRef = True, fpTextAccum = []}
        | otherwise = s{fpTextAccum = []}

    attr s name value
        | fpInQuantRef s && isElement name "refObjectId" && T.null (fpUnitGroupRef s) =
            s{fpUnitGroupRef = bsToText value}
        | otherwise = s

    endOpen s _ = s

    txt s content =
        let trimmed = BS.dropWhile (== 32) $ BS.dropWhileEnd (== 32) content
         in if BS.null trimmed then s else s{fpTextAccum = trimmed : fpTextAccum s}

    closeTag s tag
        | isElement tag "UUID" && T.null (fpUUID s) =
            s{fpUUID = accum s, fpTextAccum = []}
        | isElement tag "quantitativeReference" =
            s{fpInQuantRef = False, fpTextAccum = []}
        | otherwise = s{fpTextAccum = []}

    cdata = txt
    accum s = T.strip $ T.concat $ reverse $ map bsToText (fpTextAccum s)

--------------------------------------------------------------------------------
-- Build FlowDB and UnitDB from ILCD data
--------------------------------------------------------------------------------

buildFlowAndUnitDB ::
    M.Map UUID ILCDFlowInfo ->
    M.Map UUID UUID -> -- flowProperty UUID → unitGroup UUID
    M.Map UUID (Text, Int) -> -- unitGroup UUID → (refUnitName, refIdx)
    (FlowDB, UnitDB)
buildFlowAndUnitDB flowInfoMap fpMap ugMap = (flows, allUnits)
  where
    flows = M.mapWithKey mkFlow flowInfoMap

    -- Collect all unique units (keyed by unitGroup UUID)
    allUnits =
        M.fromList
            [ (ugId, Unit ugId uName uName "")
            | (ugId, (uName, _)) <- M.toList ugMap
            ]

    mkFlow uuid info =
        let ftype = if ilcdFlowType info == "Elementary flow" then Biosphere else Technosphere
         in Flow
                { flowId = uuid
                , flowName = ilcdBaseName info
                , flowCategory = maybe "" (\(Compartment m _ _) -> m) (ilcdCompartment info)
                , flowSubcompartment =
                    ilcdCompartment info >>= \(Compartment _ sc _) ->
                        if T.null sc then Nothing else Just sc
                , flowUnitId = resolveUnit info
                , flowType = ftype
                , flowSynonyms = M.empty
                , flowCAS = ilcdCAS info
                , flowSubstanceId = Nothing
                }

    resolveUnit info =
        maybe UUID.nil id $
            ilcdFlowPropertyRef info >>= (`M.lookup` fpMap) >>= \ugId ->
                ugId <$ M.lookup ugId ugMap -- use unitGroup UUID as unit key

--------------------------------------------------------------------------------
-- Process XML Parsing
--------------------------------------------------------------------------------

data ProcState = ProcState
    { psUUID :: !Text
    , psBaseName :: !Text
    , psLocation :: !Text
    , psRefFlowIdx :: !Int
    , psExchanges :: ![ILCDExchangeRaw]
    , psInExchange :: !Bool
    , psExInternalId :: !Int
    , psExFlowRef :: !Text
    , psExDirection :: !Text
    , psExAmount :: !Double
    , psExLocation :: !Text
    , psTextAccum :: ![BS.ByteString]
    , psInName :: !Bool
    , psClassifications :: !(M.Map Text Text)
    , psPendingClassName :: !Text
    , psInClass :: !Bool
    }

parseProcessXML :: BS.ByteString -> Maybe ILCDProcessRaw
parseProcessXML bytes =
    case X.fold
        openTag
        attr
        endOpen
        txt
        closeTag
        cdata
        (ProcState "" "" "" 0 [] False (-1) "" "" 0 "" [] False M.empty "" False)
        bytes of
        Left _ -> Nothing
        Right s -> buildProcess s
  where
    openTag s tag
        | isElement tag "exchange" =
            s
                { psInExchange = True
                , psTextAccum = []
                , psExInternalId = -1
                , psExFlowRef = ""
                , psExDirection = ""
                , psExAmount = 0
                , psExLocation = ""
                }
        | isElement tag "name" && not (psInExchange s) =
            s{psInName = True, psTextAccum = []}
        | isElement tag "class" && not (psInExchange s) =
            s{psInClass = True, psTextAccum = []}
        | otherwise = s{psTextAccum = []}

    attr s name value
        | isElement name "dataSetInternalID" && psInExchange s =
            case TR.decimal (bsToText value) of
                Right (n, _) -> s{psExInternalId = n}
                Left _ -> s
        | isElement name "refObjectId" && psInExchange s && T.null (psExFlowRef s) =
            s{psExFlowRef = bsToText value}
        | isElement name "location" && not (psInExchange s) && T.null (psLocation s) =
            s{psLocation = bsToText value}
        | isElement name "name" && not (psInExchange s) && not (psInName s) =
            s{psPendingClassName = bsToText value}
        | otherwise = s

    endOpen s _ = s

    txt s content =
        let trimmed = BS.dropWhile (== 32) $ BS.dropWhileEnd (== 32) content
         in if BS.null trimmed then s else s{psTextAccum = trimmed : psTextAccum s}

    closeTag s tag
        | isElement tag "UUID" && T.null (psUUID s) =
            s{psUUID = accum s, psTextAccum = []}
        | isElement tag "baseName" && psInName s =
            s{psBaseName = accum s, psTextAccum = []}
        | isElement tag "name" && not (psInExchange s) =
            s{psInName = False, psTextAccum = []}
        | isElement tag "referenceToReferenceFlow" =
            case TR.decimal (accum s) of
                Right (n, _) -> s{psRefFlowIdx = n, psTextAccum = []}
                Left _ -> s{psTextAccum = []}
        | isElement tag "exchangeDirection" && psInExchange s =
            s{psExDirection = accum s, psTextAccum = []}
        | isElement tag "resultingAmount" && psInExchange s =
            s{psExAmount = parseDouble (accum s), psTextAccum = []}
        | isElement tag "meanAmount" && psInExchange s && psExAmount s == 0 =
            s{psExAmount = parseDouble (accum s), psTextAccum = []}
        | isElement tag "location" && psInExchange s =
            s{psExLocation = accum s, psTextAccum = []}
        | isElement tag "class" && psInClass s =
            let classVal = accum s
                key = psPendingClassName s
                existing = M.findWithDefault "" key (psClassifications s)
                val = if T.null existing then classVal else existing <> "/" <> classVal
             in s
                    { psClassifications =
                        if T.null classVal
                            then psClassifications s
                            else M.insert key val (psClassifications s)
                    , psInClass = False
                    , psTextAccum = []
                    }
        | isElement tag "classification" =
            s{psPendingClassName = "", psTextAccum = []}
        | isElement tag "exchange" =
            let ex =
                    ILCDExchangeRaw
                        { ierInternalId = psExInternalId s
                        , ierFlowRef = maybe UUID.nil id (UUID.fromText (psExFlowRef s))
                        , ierDirection = psExDirection s
                        , ierAmount = psExAmount s
                        , ierLocation = psExLocation s
                        }
             in s{psInExchange = False, psExchanges = ex : psExchanges s, psTextAccum = []}
        | otherwise = s{psTextAccum = []}

    cdata = txt
    accum s = T.strip $ T.concat $ reverse $ map bsToText (psTextAccum s)

    parseDouble t = case TR.double t of Right (v, _) -> v; Left _ -> 0

    buildProcess s = do
        uuid <- UUID.fromText (psUUID s)
        if T.null (psBaseName s)
            then Nothing
            else
                Just
                    ILCDProcessRaw
                        { iprUUID = uuid
                        , iprName = psBaseName s
                        , iprLocation = psLocation s
                        , iprRefFlowIdx = psRefFlowIdx s
                        , iprExchanges = reverse (psExchanges s)
                        , iprClassifications = psClassifications s
                        }

-- | Parse process files in parallel using worker pattern
parseProcessFilesParallel :: [FilePath] -> IO [ILCDProcessRaw]
parseProcessFilesParallel files = do
    numWorkers <- getNumCapabilities
    let workers = distributeFiles numWorkers files
    workerResults <- mapConcurrently parseWorker workers
    return $ concat workerResults
  where
    parseWorker paths = do
        results <- mapM (\f -> parseProcessXML <$> BS.readFile f) paths
        return [r | Just r <- results]

--------------------------------------------------------------------------------
-- Build ActivityMap from raw processes
--------------------------------------------------------------------------------

buildActivityMap ::
    M.Map UUID ILCDFlowInfo ->
    FlowDB ->
    UnitDB ->
    [ILCDProcessRaw] ->
    ActivityMap
buildActivityMap flowInfoMap flowDB unitDB procs =
    M.fromList
        [ ((iprUUID p, refFlowUUID), activity)
        | p <- procs
        , let refEx = findRefExchange p
        , let refFlowUUID = maybe (iprUUID p) ierFlowRef refEx
        , let activity = buildActivity flowInfoMap flowDB unitDB p
        ]

findRefExchange :: ILCDProcessRaw -> Maybe ILCDExchangeRaw
findRefExchange p =
    case filter (\e -> ierInternalId e == iprRefFlowIdx p) (iprExchanges p) of
        (e : _) -> Just e
        [] -> Nothing

buildActivity :: M.Map UUID ILCDFlowInfo -> FlowDB -> UnitDB -> ILCDProcessRaw -> Activity
buildActivity flowInfoMap flowDB unitDB p =
    Activity
        { activityName = iprName p
        , activityDescription = []
        , activitySynonyms = M.empty
        , activityClassification = iprClassifications p
        , activityLocation = iprLocation p
        , activityUnit = refUnit
        , exchanges = map (mkExchange (iprRefFlowIdx p)) (iprExchanges p)
        , activityParams = M.empty
        , activityParamExprs = M.empty
        }
  where
    refUnit = case findRefExchange p of
        Just re -> case M.lookup (ierFlowRef re) flowDB of
            Just flow -> case M.lookup (flowUnitId flow) unitDB of
                Just u -> unitName u
                Nothing -> "kg"
            Nothing -> "kg"
        Nothing -> "kg"

    mkExchange refIdx raw =
        let flowUUID = ierFlowRef raw
            isInput = ierDirection raw == "Input"
            isRef = ierInternalId raw == refIdx
            isElementary =
                maybe
                    False
                    (\fi -> ilcdFlowType fi == "Elementary flow")
                    (M.lookup flowUUID flowInfoMap)
            fUnitId = maybe UUID.nil flowUnitId (M.lookup flowUUID flowDB)
         in if isElementary
                then BiosphereExchange flowUUID (ierAmount raw) fUnitId isInput (ierLocation raw)
                else
                    TechnosphereExchange
                        flowUUID
                        (ierAmount raw)
                        fUnitId
                        isInput
                        isRef
                        UUID.nil
                        Nothing
                        (ierLocation raw)

--------------------------------------------------------------------------------
-- Fix activity links (supplier resolution by name)
--------------------------------------------------------------------------------

-- | Flow UUID → (activityUUID, productUUID) for reference exchanges
type SupplierIndex = M.Map UUID (UUID, UUID)

fixILCDActivityLinks :: SimpleDatabase -> IO SimpleDatabase
fixILCDActivityLinks db = do
    let idx = buildSupplierIndex (sdbActivities db)
    reportProgress Info $ printf "Built supplier index with %d entries for ILCD linking" (M.size idx)
    return db{sdbActivities = M.map (fixActivityExchanges idx) (sdbActivities db)}

{- | Build a flow-UUID-keyed index of (activityUUID, productUUID) from reference exchanges.
UUID-based: no name collisions, no indirection through flowDB.
-}
buildSupplierIndex :: ActivityMap -> SupplierIndex
buildSupplierIndex activities =
    M.fromList
        [ (exchangeFlowId ex, (actUUID, prodUUID))
        | ((actUUID, prodUUID), act) <- M.toList activities
        , ex <- exchanges act
        , exchangeIsReference ex
        ]

fixActivityExchanges :: SupplierIndex -> Activity -> Activity
fixActivityExchanges idx act =
    act{exchanges = map fixEx (exchanges act)}
  where
    fixEx ex@(TechnosphereExchange fid amt uid isInp _ _ procLink loc)
        | isInp = case M.lookup fid idx of
            Just (actUUID, prodUUID) ->
                TechnosphereExchange prodUUID amt uid isInp False actUUID procLink loc
            Nothing -> ex
    fixEx ex = ex
