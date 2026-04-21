{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Matrix.Export (
    -- * Universal matrix format
    exportUniversalMatrixFormat,

    -- * Debug export
    exportMatrixDebugCSVs,
    extractMatrixDebugInfo,
    MatrixDebugInfo (..),

    -- * Utilities
    escapeCsvField,
) where

import Matrix (applySparseMatrix, buildDemandVectorFromIndex, solveSparseLinearSystem, toList)
import Progress (ProgressLevel (..), reportProgress)
import Types

import Data.Int (Int32)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

{- | Placeholder for the 5 uncertainty columns (type, variance, min, mostLikely, max).
VoLCA does not model uncertainty; these are left empty for format compatibility
with tools expecting the Ecoinvent universal matrix exchange format.
-}
emptyCsvUncertainty :: Text
emptyCsvUncertainty = ";;;;;"

-- | Matrix debug information container
data MatrixDebugInfo = MatrixDebugInfo
    { mdActivities :: ActivityDB
    , mdFlows :: M.Map UUID Flow
    , mdTechTriples :: U.Vector SparseTriple
    , mdBioTriples :: U.Vector SparseTriple
    , mdActivityIndex :: V.Vector Int32
    , mdBioFlowUUIDs :: V.Vector UUID
    , mdTargetUUID :: UUID
    , mdTargetProcessId :: ProcessId
    , mdDatabase :: Database
    , mdSupplyVector :: [Double]
    , mdDemandVector :: [Double]
    , mdInventoryVector :: [Double]
    }

-- | Extract matrix debug information from Database
extractMatrixDebugInfo :: Database -> UUID -> Maybe Text -> IO MatrixDebugInfo
extractMatrixDebugInfo database targetUUID flowFilter = do
    let activities = dbActivities database
        flows = dbFlows database
        techTriples = dbTechnosphereTriples database
        bioTriples = dbBiosphereTriples database
        activityIndexVec = dbActivityIndex database
        bioFlowUUIDs = dbBiosphereFlows database
        activityCount = dbActivityCount database
        bioFlowCount = dbBiosphereCount database

        targetProcessId = case findProcessIdByActivityUUID database targetUUID of
            Just pid -> pid
            Nothing -> error $ "Activity " <> show targetUUID <> " has no ProcessId in database for debug-matrices"
        demandVec = buildDemandVectorFromIndex activityIndexVec targetProcessId
        demandList = toList demandVec

        techTriplesInt = [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList techTriples]
        activityCountInt = fromIntegral activityCount

    supplyVec <- solveSparseLinearSystem techTriplesInt activityCountInt demandVec
    let supplyList = toList supplyVec

        bioTriplesInt = [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList bioTriples]
        bioFlowCountInt = fromIntegral bioFlowCount
        inventoryVec = applySparseMatrix bioTriplesInt bioFlowCountInt supplyVec
        inventoryList = toList inventoryVec

        filteredBioTriples = case flowFilter of
            Nothing -> bioTriples
            Just filterText ->
                let matchingFlowIndices =
                        [ idx | (uuid, idx) <- zip (V.toList bioFlowUUIDs) ([0 ..] :: [Int]), Just flow <- [M.lookup uuid flows], T.toLower filterText `T.isInfixOf` T.toLower (flowName flow)
                        ]
                    matchingFlowIndicesInt32 = map fromIntegral matchingFlowIndices :: [Int32]
                 in U.filter (\(SparseTriple row _ _) -> row `elem` matchingFlowIndicesInt32) bioTriples
    return
        MatrixDebugInfo
            { mdActivities = activities
            , mdFlows = flows
            , mdTechTriples = techTriples
            , mdBioTriples = filteredBioTriples
            , mdActivityIndex = activityIndexVec
            , mdBioFlowUUIDs = bioFlowUUIDs
            , mdTargetUUID = targetUUID
            , mdTargetProcessId = targetProcessId
            , mdDatabase = database
            , mdSupplyVector = supplyList
            , mdDemandVector = demandList
            , mdInventoryVector = inventoryList
            }

-- | Export matrix debug CSVs
exportMatrixDebugCSVs :: FilePath -> MatrixDebugInfo -> IO ()
exportMatrixDebugCSVs basePath debugInfo = do
    let supplyChainPath = basePath ++ "_supply_chain.csv"
        biosphereMatrixPath = basePath ++ "_biosphere_matrix.csv"
    exportSupplyChainData supplyChainPath debugInfo
    exportBiosphereMatrixData biosphereMatrixPath debugInfo
    reportProgress Info $ "Debug CSVs written to: " ++ supplyChainPath ++ " and " ++ biosphereMatrixPath

-- | Export supply chain data showing which activities contribute to target
exportSupplyChainData :: FilePath -> MatrixDebugInfo -> IO ()
exportSupplyChainData filePath debugInfo = do
    let database = mdDatabase debugInfo
        activities = mdActivities debugInfo
        activityIndexVec = mdActivityIndex debugInfo
        supplyVector = mdSupplyVector debugInfo

        supplyChainRows =
            [ csvRow processId activity idx supply
            | processId <- [toEnum 0 .. toEnum (V.length activities - 1)]
            , let activity = activities V.! fromEnum processId
            , let idx = fromIntegral (activityIndexVec V.! fromEnum processId) :: Int
            , let supply = if idx < length supplyVector then supplyVector !! idx else 0.0
            ]

        csvRow processId activity idx supply =
            [ T.unpack (processIdToText database processId)
            , T.unpack (activityName activity)
            , T.unpack (activityLocation activity)
            , show supply
            , show idx
            ]

        csvHeader = ["activity_id", "activity_name", "location", "supply_amount", "col_idx"]
        allRows = csvHeader : supplyChainRows
        csvContent = L.intercalate "\n" (map (L.intercalate ",") allRows)

    writeFile filePath csvContent
    reportProgress Info $ "Supply chain: " ++ show (length supplyChainRows) ++ " activities → " ++ filePath

-- | Export biosphere matrix contributions
exportBiosphereMatrixData :: FilePath -> MatrixDebugInfo -> IO ()
exportBiosphereMatrixData filePath debugInfo = do
    let database = mdDatabase debugInfo
        flows = mdFlows debugInfo
        activities = mdActivities debugInfo
        bioTriples = mdBioTriples debugInfo
        bioFlowUUIDs = mdBioFlowUUIDs debugInfo
        activityIndex = mdActivityIndex debugInfo
        supplyVector = mdSupplyVector debugInfo

        matrixRows =
            [ csvRow bioTriple
            | bioTriple@(SparseTriple _row _col value) <- U.toList bioTriples
            , abs value > 1e-20
            ]

        csvRow (SparseTriple row col value) =
            let rowInt = fromIntegral row :: Int
                colInt = fromIntegral col :: Int
             in [ maybe "unknown" show (getFlowUUID rowInt)
                , maybe "unknown" (T.unpack . flowName) (getFlow rowInt)
                , maybe "unknown" T.unpack (getFlowUnit rowInt)
                , maybe "unknown" (T.unpack . processIdToText database) (getActivityProcessId colInt)
                , maybe "unknown" (T.unpack . activityName) (lookupActivity colInt)
                , show value
                , show (realContribution colInt)
                ]
          where
            getFlowUUID :: Int -> Maybe UUID
            getFlowUUID rowIdx =
                if rowIdx < V.length bioFlowUUIDs
                    then Just (bioFlowUUIDs V.! rowIdx)
                    else Nothing
            getFlow :: Int -> Maybe Flow
            getFlow rowIdx = getFlowUUID rowIdx >>= flip M.lookup flows
            getFlowUnit :: Int -> Maybe Text
            getFlowUnit rowIdx = fmap (getUnitNameForFlow (dbUnits database)) (getFlow rowIdx)
            getActivityProcessId :: Int -> Maybe ProcessId
            getActivityProcessId colIdx =
                L.find
                    (\pid -> fromIntegral (activityIndex V.! fromEnum pid) == colIdx)
                    [toEnum 0 .. toEnum (V.length activities - 1)]
            lookupActivity :: Int -> Maybe Activity
            lookupActivity colIdx = do
                processId <- getActivityProcessId colIdx
                if fromEnum processId < V.length activities
                    then Just (activities V.! fromEnum processId)
                    else Nothing
            realContribution :: Int -> Double
            realContribution colIdx =
                if colIdx < length supplyVector
                    then value * (supplyVector !! colIdx)
                    else 0.0

        csvHeader = ["flow_id", "flow_name", "unit", "activity_id", "activity_name", "matrix_value", "contribution"]
        allRows = csvHeader : matrixRows
        csvContent = L.intercalate "\n" (map (L.intercalate ",") allRows)

    writeFile filePath csvContent
    reportProgress Info $ "Biosphere matrix: " ++ show (length matrixRows) ++ " entries → " ++ filePath

-- | Export matrices in universal matrix format (Ecoinvent-compatible)
exportUniversalMatrixFormat :: FilePath -> Database -> IO ()
exportUniversalMatrixFormat outputDir db = do
    reportProgress Info $ "Exporting matrices to universal format in: " ++ outputDir
    exportIEIndex (outputDir ++ "/ie_index.csv") db
    exportEEIndex (outputDir ++ "/ee_index.csv") db
    exportAMatrix (outputDir ++ "/A_public.csv") db
    exportBMatrix (outputDir ++ "/B_public.csv") db
    reportProgress Info "Universal matrix export completed"

-- | Escape text for CSV output (semicolon delimiter)
escapeCsvField :: Text -> Text
escapeCsvField text
    | T.any (\c -> c == ';' || c == '"' || c == '\n' || c == '\r') text =
        "\"" <> T.replace "\"" "\"\"" text <> "\""
    | otherwise = text

-- | Export ie_index.csv (Intermediate Exchanges)
exportIEIndex :: FilePath -> Database -> IO ()
exportIEIndex filePath db = do
    let activities = dbActivities db
        processIdTable = dbProcessIdTable db
        flows = dbFlows db

        rows =
            V.toList $
                V.imap
                    ( \idx (_actUuid, prodUuid) ->
                        let activity = activities V.! idx
                            refProduct = case [ex | ex <- exchanges activity, exchangeIsReference ex] of
                                (ex : _) -> case M.lookup (exchangeFlowId ex) flows of
                                    Just flow -> flowName flow
                                    Nothing -> T.pack (show prodUuid)
                                [] -> T.pack (show prodUuid)
                            unit = activityUnit activity
                         in escapeCsvField (activityName activity)
                                <> ";"
                                <> escapeCsvField (activityLocation activity)
                                <> ";"
                                <> escapeCsvField refProduct
                                <> ";"
                                <> escapeCsvField unit
                                <> ";"
                                <> T.pack (show idx)
                    )
                    processIdTable

        header = "activityName;geography;product;unitName;index"
        content = T.unlines (header : rows)

    TIO.writeFile filePath content
    reportProgress Info $ "ie_index: " ++ show (length rows) ++ " activities → " ++ filePath

-- | Export ee_index.csv (Elementary Exchanges)
exportEEIndex :: FilePath -> Database -> IO ()
exportEEIndex filePath db = do
    let bioFlowUUIDs = dbBiosphereFlows db
        flows = dbFlows db

        rows =
            zipWith
                ( \flowUuid idx ->
                    case M.lookup flowUuid flows of
                        Just flow ->
                            let category = flowCategory flow
                                (compartment, subcompartment) = case T.splitOn "/" category of
                                    [] -> ("unspecified", "")
                                    [c] -> (T.strip c, "")
                                    (c : s : _) -> (T.strip c, T.strip s)
                                unit = getUnitNameForFlow (dbUnits db) flow
                             in escapeCsvField (flowName flow)
                                    <> ";"
                                    <> escapeCsvField compartment
                                    <> ";"
                                    <> escapeCsvField subcompartment
                                    <> ";"
                                    <> escapeCsvField unit
                                    <> ";"
                                    <> T.pack (show idx)
                        Nothing ->
                            escapeCsvField (T.pack (show flowUuid)) <> ";unknown;;;" <> T.pack (show idx)
                )
                (V.toList bioFlowUUIDs)
                ([0 ..] :: [Int])

        header = "name;compartment;subcompartment;unitName;index"
        content = T.unlines (header : rows)

    TIO.writeFile filePath content
    reportProgress Info $ "ee_index: " ++ show (length rows) ++ " biosphere flows → " ++ filePath

-- | Export A_public.csv (Technosphere Matrix)
exportAMatrix :: FilePath -> Database -> IO ()
exportAMatrix filePath db = do
    let techTriples = dbTechnosphereTriples db
        activityCount = dbActivityCount db
        activities = dbActivities db

        -- Waste activities have their reference product stored as outputGroup=0 with a
        -- NEGATIVE amount (e.g., hard coal ash = -1.0 kg). These use -1.0 on the diagonal
        -- in the (I-A) convention. Production activities use 1.0.
        diagValue i =
            let act = activities V.! i
             in if any (\ex -> exchangeIsReference ex && exchangeAmount ex < 0) (exchanges act)
                    then "-1.0"
                    else "1.0"

        diagonalEntries =
            [ T.pack (show i ++ ";" ++ show i ++ ";" ++ diagValue i) <> emptyCsvUncertainty
            | i <- [0 .. fromIntegral activityCount - 1 :: Int]
            ]

        offDiagonalRows =
            U.foldr
                ( \(SparseTriple row col value) acc ->
                    let rowStr =
                            T.pack (show row ++ ";" ++ show col ++ ";" ++ show (-value))
                                <> emptyCsvUncertainty
                     in rowStr : acc
                )
                []
                techTriples

        header = "row;column;coefficient;uncertainty type;varianceWithPedigreeUncertainty;minValue;mostLikelyValue;maxValue"
        allRows = diagonalEntries ++ offDiagonalRows
        content = T.unlines (header : allRows)

    TIO.writeFile filePath content
    reportProgress Info $
        "A_public: "
            ++ show (length diagonalEntries)
            ++ " diagonal + "
            ++ show (U.length techTriples)
            ++ " off-diagonal entries → "
            ++ filePath

-- | Export B_public.csv (Biosphere Matrix)
exportBMatrix :: FilePath -> Database -> IO ()
exportBMatrix filePath db = do
    let bioTriples = dbBiosphereTriples db

        rows =
            U.foldr
                ( \(SparseTriple row col value) acc ->
                    let rowStr =
                            T.pack (show row ++ ";" ++ show col ++ ";" ++ show value)
                                <> emptyCsvUncertainty
                     in rowStr : acc
                )
                []
                bioTriples

        header = "row;column;coefficient;uncertainty type;varianceWithPedigreeUncertainty;minValue;mostLikelyValue;maxValue"
        content = T.unlines (header : rows)

    TIO.writeFile filePath content
    reportProgress Info $ "B_public: " ++ show (U.length bioTriples) ++ " entries → " ++ filePath
