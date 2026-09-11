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

import Database (filterByName, flowSearchFields)
import Matrix (Demand (..), Vector, applySparseMatrix, buildDemandVectorFromIndex, solveSparseLinearSystem, toList)
import Progress (ProgressLevel (..), reportProgress)
import Types

import Data.Int (Int32)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
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
    , mdBioFlows :: BioFlowDB -- B-matrix rows are biosphere flows
    , mdTechTriples :: U.Vector SparseTriple
    , mdBioTriples :: U.Vector SparseTriple
    , mdActivityIndex :: V.Vector Int32
    , mdBioFlowUUIDs :: V.Vector UUID
    , mdTargetProcessId :: ProcessId
    , mdDatabase :: Database
    , mdSupplyVector :: U.Vector Double
    , mdDemandVector :: [Double]
    , mdInventoryVector :: [Double]
    }

{- | Extract matrix debug information from Database. The target arrives as the
row it is, which is the row the caller resolved: an activity UUID would have to
be resolved again here, and would answer with an arbitrary product of an
activity written as several coproduct rows.
-}
extractMatrixDebugInfo :: Database -> ProcessId -> Maybe Text -> IO (Either Text MatrixDebugInfo)
extractMatrixDebugInfo database targetProcessId flowFilter =
    either
        (pure . Left)
        (fmap Right . withDemand . unDemand)
        (buildDemandVectorFromIndex (dbActivityIndex database) targetProcessId)
  where
    withDemand :: Vector -> IO MatrixDebugInfo
    withDemand demandVec = do
        let activities = dbActivities database
            bioFlows = dbBioFlows database
            techTriples = dbTechnosphereTriples database
            bioTriples = dbBiosphereTriples database
            activityIndexVec = dbActivityIndex database
            bioFlowUUIDs = dbBiosphereOrder database
            activityCount = dbActivityCount database
            bioFlowCount = dbBiosphereCount database

            demandList = toList demandVec

            techTriplesInt = [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList techTriples]
            activityCountInt = fromIntegral activityCount

        supplyVec <- solveSparseLinearSystem techTriplesInt activityCountInt demandVec
        let bioTriplesInt = [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList bioTriples]
            bioFlowCountInt = fromIntegral bioFlowCount
            inventoryVec = applySparseMatrix bioTriplesInt bioFlowCountInt supplyVec
            inventoryList = toList inventoryVec

            filteredBioTriples = case flowFilter of
                Nothing -> bioTriples
                Just filterText ->
                    -- The filter read the way a flow search reads a query: the
                    -- words in any order, punctuation of the name left to the name.
                    let candidates =
                            [ (idx, flow)
                            | (uuid, idx) <- zip (V.toList bioFlowUUIDs) ([0 ..] :: [Int])
                            , Just flow <- [M.lookup uuid bioFlows]
                            ]
                        matchingFlowIndices =
                            map fst (filterByName filterText (flowSearchFields . BioKind . snd) candidates)
                        matchingFlowIndicesInt32 = map fromIntegral matchingFlowIndices :: [Int32]
                     in U.filter (\(SparseTriple row _ _) -> row `elem` matchingFlowIndicesInt32) bioTriples
        return
            MatrixDebugInfo
                { mdActivities = activities
                , mdBioFlows = bioFlows
                , mdTechTriples = techTriples
                , mdBioTriples = filteredBioTriples
                , mdActivityIndex = activityIndexVec
                , mdBioFlowUUIDs = bioFlowUUIDs
                , mdTargetProcessId = targetProcessId
                , mdDatabase = database
                , mdSupplyVector = supplyVec
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
            , let supply = fromMaybe 0.0 (supplyVector U.!? idx)
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
        flows = mdBioFlows debugInfo
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
                , maybe "unknown" (T.unpack . bfName) (getFlow rowInt)
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
            getFlow :: Int -> Maybe BiosphereFlow
            getFlow rowIdx = getFlowUUID rowIdx >>= flip M.lookup flows
            getFlowUnit :: Int -> Maybe Text
            getFlowUnit rowIdx = fmap (getUnitNameForBioFlow (dbUnits database)) (getFlow rowIdx)
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
            realContribution colIdx = maybe 0.0 (value *) (supplyVector U.!? colIdx)

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
        techFlows = dbTechFlows db

        rows =
            V.toList $
                V.imap
                    ( \idx (_actUuid, prodUuid) ->
                        let activity = activities V.! idx
                            refProduct = case [ex | ex <- exchanges activity, exchangeIsReference ex] of
                                (ex : _) -> case M.lookup (exchangeFlowId ex) techFlows of
                                    Just flow -> tfName flow
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
    let bioFlowUUIDs = dbBiosphereOrder db
        bioFlows = dbBioFlows db

        rows =
            zipWith
                ( \flowUuid idx ->
                    case M.lookup flowUuid bioFlows of
                        Just flow ->
                            let compartment = bfCompartmentName flow
                                subcompartment = fromMaybe "" (bfCompartmentSub flow)
                                unit = getUnitNameForBioFlow (dbUnits db) flow
                             in escapeCsvField (bfName flow)
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

{- | The sign the published convention gives each activity's whole column.

A waste treatment records the waste it treats as its reference product, with a negative
amount, and our triples are normalised by that reference, so its column comes out with
every sign reversed against the universal matrix format, where a coefficient is simply
positive when the activity produces and negative when it consumes. One sign per column
puts it back, and the diagonal takes the same sign as the rest of the column: signing the
diagonal alone publishes an activity that consumes waste on the diagonal and produces its
own inputs everywhere else, which no solver can read.
-}
referenceSigns :: Database -> U.Vector Double
referenceSigns db = U.generate (fromIntegral (dbActivityCount db)) columnSign
  where
    -- The very quantity the triples were divided by, so the export undoes the
    -- normalisation it is undoing rather than a rule that resembles it: a
    -- treatment whose reference is a negative *input* normalises on its
    -- absolute value and is not flipped at all, and reading the raw amount
    -- would reverse its whole column for nothing.
    columnSign :: Int -> Double
    columnSign i = signum (activityNormFactor (dbActivities db V.! i) (dbProcessIdTable db V.! i))

-- | Export A_public.csv (Technosphere Matrix)
exportAMatrix :: FilePath -> Database -> IO ()
exportAMatrix filePath db = do
    let techTriples = dbTechnosphereTriples db
        activityCount = dbActivityCount db
        signs = referenceSigns db

        diagonalEntries =
            [ T.pack (show i ++ ";" ++ show i ++ ";" ++ show (signs U.! i)) <> emptyCsvUncertainty
            | i <- [0 .. fromIntegral activityCount - 1 :: Int]
            ]

        offDiagonalRows =
            U.foldr
                ( \(SparseTriple row col value) acc ->
                    let coefficient = negate value * (signs U.! fromIntegral col)
                        rowStr =
                            T.pack (show row ++ ";" ++ show col ++ ";" ++ show coefficient)
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
        signs = referenceSigns db

        rows =
            U.foldr
                ( \(SparseTriple row col value) acc ->
                    let coefficient = value * (signs U.! fromIntegral col)
                        rowStr =
                            T.pack (show row ++ ";" ++ show col ++ ";" ++ show coefficient)
                                <> emptyCsvUncertainty
                     in rowStr : acc
                )
                []
                bioTriples

        header = "row;column;coefficient;uncertainty type;varianceWithPedigreeUncertainty;minValue;mostLikelyValue;maxValue"
        content = T.unlines (header : rows)

    TIO.writeFile filePath content
    reportProgress Info $ "B_public: " ++ show (U.length bioTriples) ++ " entries → " ++ filePath
