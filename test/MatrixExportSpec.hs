{-# LANGUAGE OverloadedStrings #-}

module MatrixExportSpec (spec) where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.UUID as UUID
import qualified Data.Vector.Unboxed as U
import Database (buildDatabaseWithMatrices)
import Matrix.Export (
    MatrixDebugInfo (..),
    escapeCsvField,
    exportMatrixDebugCSVs,
    extractMatrixDebugInfo,
 )
import Service (exportUniversalMatrixFormat)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import TestHelpers
import Text.Read (readMaybe)
import Types
import UnitConversion (defaultUnitConfig)

spec :: Spec
spec = do
    describe "Matrix Export Format" $ do
        it "exports A_public.csv in (I-A) format with negative off-diagonal" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            withSystemTempDirectory "acv-test" $ \tmpDir -> do
                -- Export matrices
                exportUniversalMatrixFormat tmpDir db

                -- Read A_public.csv
                let aMatrixPath = tmpDir </> "A_public.csv"
                aMatrixContent <- TIO.readFile aMatrixPath

                -- Check header
                let lines = T.lines aMatrixContent
                length lines `shouldSatisfy` (> 1)

                -- Parse first line as header
                let header = head lines
                T.isInfixOf "row;column;coefficient" header `shouldBe` True

                -- Check diagonal entries (should be 1.0)
                let diagonalLines = filter (T.isInfixOf "0;0;1.0") lines
                length diagonalLines `shouldSatisfy` (>= 1)

                -- For SAMPLE.min3: Expected -0.6 and -0.4 off-diagonal entries.
                let hasNegative = any (T.isInfixOf "-0.") lines
                hasNegative `shouldBe` True

        it "exports B_public.csv with correct signs" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            withSystemTempDirectory "acv-test" $ \tmpDir -> do
                exportUniversalMatrixFormat tmpDir db

                -- Read B_public.csv
                let bMatrixPath = tmpDir </> "B_public.csv"
                bMatrixContent <- TIO.readFile bMatrixPath

                -- Check header
                let lines = T.lines bMatrixContent
                length lines `shouldSatisfy` (> 1)

                -- Biosphere values should be positive for emissions
                -- SAMPLE.min3 has 4.0 kg CO2 and 0.003 kg Zinc
                let hasPositive = any (\l -> T.isInfixOf ";4.0;" l || T.isInfixOf ";0.003;" l) lines
                hasPositive `shouldBe` True

        it "exports ie_index.csv with activity information" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            withSystemTempDirectory "acv-test" $ \tmpDir -> do
                exportUniversalMatrixFormat tmpDir db

                -- Read ie_index.csv
                let indexPath = tmpDir </> "ie_index.csv"
                indexContent <- TIO.readFile indexPath

                -- Check header and 3 activities
                let lines = T.lines indexContent
                length lines `shouldBe` 4 -- header + 3 activities
        it "exports ee_index.csv with biosphere flow information" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            withSystemTempDirectory "acv-test" $ \tmpDir -> do
                exportUniversalMatrixFormat tmpDir db

                -- Read ee_index.csv
                let indexPath = tmpDir </> "ee_index.csv"
                indexContent <- TIO.readFile indexPath

                -- Check header and 2 flows (CO2, Zinc)
                let lines = T.lines indexContent
                length lines `shouldBe` 3 -- header + 2 flows
        it "publishes a waste treatment's column in the format's own sign convention" $ do
            db <- treatmentDatabase
            withSystemTempDirectory "acv-matrix-sign" $ \tmpDir -> do
                exportUniversalMatrixFormat tmpDir db
                columns <- indexByName (tmpDir </> "ie_index.csv")
                technosphere <- cells (tmpDir </> "A_public.csv")
                biosphere <- cells (tmpDir </> "B_public.csv")
                case (lookup "treatment of waste W" columns, lookup "producer of Y" columns) of
                    (Just treatment, Just producer) -> do
                        -- The waste it treats: consumed, so negative, and the rest of the
                        -- column has to read in the convention the diagonal announces.
                        lookup (treatment, treatment) technosphere `shouldBe` Just (-1.0)
                        -- What it consumes to do the treating stays an input, not a product.
                        lookup (producer, treatment) technosphere `shouldBe` Just (-0.5)
                        -- What it emits stays an emission. Treating waste adds burden.
                        lookup (0, treatment) biosphere `shouldBe` Just 2.0
                        -- An ordinary activity is untouched by any of this.
                        lookup (producer, producer) technosphere `shouldBe` Just 1.0
                    _ -> expectationFailure ("ie_index names " <> show (map fst columns))

    describe "Export CSV Format Validation" $ do
        it "uses semicolon as delimiter" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            withSystemTempDirectory "acv-test" $ \tmpDir -> do
                exportUniversalMatrixFormat tmpDir db

                let aMatrixPath = tmpDir </> "A_public.csv"
                aMatrixContent <- TIO.readFile aMatrixPath

                -- All lines should contain semicolons
                let lines = T.lines aMatrixContent
                let allHaveSemicolon = all (T.isInfixOf ";") lines
                allHaveSemicolon `shouldBe` True

        it "exports correct number of columns" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            withSystemTempDirectory "acv-test" $ \tmpDir -> do
                exportUniversalMatrixFormat tmpDir db

                let aMatrixPath = tmpDir </> "A_public.csv"
                aMatrixContent <- TIO.readFile aMatrixPath

                -- Format: row;column;coefficient;uncertainty type;varianceWithPedigreeUncertainty;minValue;mostLikelyValue;maxValue
                -- Should have 8 fields
                let dataLines = tail $ T.lines aMatrixContent -- Skip header
                let firstDataLine = head dataLines
                let fields = T.splitOn ";" firstDataLine
                length fields `shouldBe` 8

    -- -------------------------------------------------------------------
    -- escapeCsvField (pure)
    -- -------------------------------------------------------------------
    describe "escapeCsvField" $ do
        it "passes through plain text unchanged" $
            escapeCsvField "hello world" `shouldBe` "hello world"

        it "quotes text containing a semicolon" $
            escapeCsvField "a;b" `shouldBe` "\"a;b\""

        it "quotes text containing a double quote and escapes it" $
            escapeCsvField "say \"hi\"" `shouldBe` "\"say \"\"hi\"\"\""

        it "quotes text containing a newline" $
            escapeCsvField "line1\nline2" `shouldBe` "\"line1\nline2\""

        it "quotes text containing a carriage return" $
            escapeCsvField "a\rb" `shouldBe` "\"a\rb\""

        it "passes through empty text unchanged" $
            escapeCsvField "" `shouldBe` ""

        it "handles text with all special characters" $
            escapeCsvField "a;b\"c\nd" `shouldBe` "\"a;b\"\"c\nd\""

    -- -------------------------------------------------------------------
    -- extractMatrixDebugInfo + exportMatrixDebugCSVs
    -- -------------------------------------------------------------------
    describe "extractMatrixDebugInfo" $ do
        it "returns supply, demand, and inventory vectors of correct length" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            info <- either (fail . show) pure =<< extractMatrixDebugInfo db (targetRow db) Nothing
            let n = fromIntegral (dbActivityCount db)
            U.length (mdSupplyVector info) `shouldBe` n
            length (mdDemandVector info) `shouldBe` n

        it "demand vector has exactly one non-zero entry" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            info <- either (fail . show) pure =<< extractMatrixDebugInfo db (targetRow db) Nothing
            length (filter (/= 0.0) (mdDemandVector info)) `shouldBe` 1

        it "inventory vector is non-empty (has biosphere contributions)" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            info <- either (fail . show) pure =<< extractMatrixDebugInfo db (targetRow db) Nothing
            any (/= 0.0) (mdInventoryVector info) `shouldBe` True

        it "flow filter restricts biosphere triples" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            infoAll <- either (fail . show) pure =<< extractMatrixDebugInfo db (targetRow db) Nothing
            infoFiltered <- either (fail . show) pure =<< extractMatrixDebugInfo db (targetRow db) (Just "carbon")
            -- Filtered should have ≤ triples than unfiltered
            let nAll = length (mdInventoryVector infoAll)
                nFiltered = length (mdInventoryVector infoFiltered)
            nFiltered `shouldSatisfy` (<= nAll)

    describe "exportMatrixDebugCSVs" $ do
        it "creates supply chain and biosphere CSV files" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            info <- either (fail . show) pure =<< extractMatrixDebugInfo db (targetRow db) Nothing
            withSystemTempDirectory "acv-debug" $ \tmpDir -> do
                let base = tmpDir </> "debug"
                exportMatrixDebugCSVs base info
                supplyContent <- TIO.readFile (base ++ "_supply_chain.csv")
                bioContent <- TIO.readFile (base ++ "_biosphere_matrix.csv")
                T.isInfixOf "activity_id" supplyContent `shouldBe` True
                T.isInfixOf "flow_id" bioContent `shouldBe` True

        it "supply chain CSV has one row per activity" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            info <- either (fail . show) pure =<< extractMatrixDebugInfo db (targetRow db) Nothing
            withSystemTempDirectory "acv-debug" $ \tmpDir -> do
                let base = tmpDir </> "debug"
                exportMatrixDebugCSVs base info
                content <- TIO.readFile (base ++ "_supply_chain.csv")
                -- header + 3 activities (SAMPLE.min3)
                length (lines (T.unpack content)) `shouldBe` 4

        it "names each column's own activity, and scales it by that column's supply" $ do
            -- Both files in full, because both are read by eye and every field of
            -- them is a promise: the two columns have to come back under their own
            -- activity, and the amounts have to keep the last digit the solver
            -- produced. The producer sits at column 0 and the treatment at column
            -- 1, so an export that mistook one column for another would name the
            -- wrong activity here rather than merely lose a row.
            db <- treatmentDatabase
            info <- either (fail . show) pure =<< extractMatrixDebugInfo db (rowOf db treatmentUUID) Nothing
            withSystemTempDirectory "acv-debug-columns" $ \tmpDir -> do
                let base = tmpDir </> "debug"
                    producer = processIdToText db (rowOf db producerUUID)
                    treatment = processIdToText db (rowOf db treatmentUUID)
                    co2 = T.pack (show carbonDioxide)
                exportMatrixDebugCSVs base info
                supplyChain <- TIO.readFile (base ++ "_supply_chain.csv")
                biosphere <- TIO.readFile (base ++ "_biosphere_matrix.csv")
                supplyChain
                    `shouldBe` T.intercalate
                        "\n"
                        [ "activity_id,activity_name,location,supply_amount,col_idx"
                        , producer <> ",producer of Y,GLO,-0.4999999999999999,0"
                        , treatment <> ",treatment of waste W,GLO,1.0,1"
                        ]
                biosphere
                    `shouldBe` T.intercalate
                        "\n"
                        [ "flow_id,flow_name,unit,activity_id,activity_name,matrix_value,contribution"
                        , co2 <> ",carbon dioxide,kg," <> producer <> ",producer of Y,3.0,-1.4999999999999996"
                        , co2 <> ",carbon dioxide,kg," <> treatment <> ",treatment of waste W,-2.0,-2.0"
                        ]

{- | The row SAMPLE.min3's activity X sits at. Row 0 when it is missing, which
only happens if the fixture changes: the assertions below would then fail on a
different activity rather than on a resolution error, which is the shape hspec
reports best.
-}
targetRow :: Database -> ProcessId
targetRow db = rowOf db (read "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa")

-- | The row an activity was interned at. Row 0 when it is missing, as above.
rowOf :: Database -> UUID -> ProcessId
rowOf db activityUUID = fromMaybe 0 (findProcessIdByActivityUUID db activityUUID)

-- --------------------------------------------------------------------------- --
-- A database with one waste treatment in it
-- --------------------------------------------------------------------------- --

{- | Two activities: an ordinary producer of Y, and a treatment of waste W in the
convention EcoSpold 2 uses, where the treated waste is the reference product and its
amount is negative. The treatment consumes half a Y and emits 2 kg of CO2; the producer
emits 3, so both columns of the biosphere matrix carry a row and the debug export has to
name a different activity for each.

Everything the export has to get right about signs is visible in four coefficients, which
is why this is built here rather than loaded from a fixture: a sample file would have to
be read first to know what the answer should be.
-}
treatmentDatabase :: IO Database
treatmentDatabase =
    buildDatabaseWithMatrices
        (BuildInputs defaultUnitConfig mempty Declared)
        SimpleDatabase
            { sdbActivities =
                M.fromList
                    [ ((producerUUID, productY), producerOfY)
                    , ((treatmentUUID, wasteW), treatmentOfW)
                    ]
            , sdbTechFlows =
                M.fromList
                    [ (productY, TechnosphereFlow productY "product Y" kilogram M.empty Nothing Nothing)
                    , (wasteW, TechnosphereFlow wasteW "waste W" kilogram M.empty Nothing Nothing)
                    ]
            , sdbBioFlows =
                M.singleton
                    carbonDioxide
                    ( BiosphereFlow
                        carbonDioxide
                        "carbon dioxide"
                        kilogram
                        M.empty
                        Nothing
                        Nothing
                        (Just (Compartment Air Nothing))
                    )
            , sdbWasteFlows = M.empty
            , sdbUnits = M.singleton kilogram (Unit kilogram "kg" "kg" "")
            }
        >>= either (fail . T.unpack) pure

producerUUID, treatmentUUID, productY, wasteW, carbonDioxide, kilogram :: UUID
producerUUID = testUUID "11111111-1111-1111-1111-111111111111"
treatmentUUID = testUUID "22222222-2222-2222-2222-222222222222"
productY = testUUID "33333333-3333-3333-3333-333333333333"
wasteW = testUUID "44444444-4444-4444-4444-444444444444"
carbonDioxide = testUUID "55555555-5555-5555-5555-555555555555"
kilogram = testUUID "66666666-6666-6666-6666-666666666666"

testUUID :: String -> UUID
testUUID = fromMaybe UUID.nil . UUID.fromString

producerOfY :: Activity
producerOfY = blankActivity "producer of Y" [reference productY 1.0, emits 3.0]

treatmentOfW :: Activity
treatmentOfW =
    blankActivity
        "treatment of waste W"
        [ reference wasteW (-1.0)
        , consumesFrom producerUUID productY 0.5
        , emits 2.0
        ]

-- | Carbon dioxide to air, in kilograms.
emits :: Double -> Exchange
emits amount =
    BiosphereExchange
        { bioFlowId = carbonDioxide
        , bioAmount = amount
        , bioUnitId = kilogram
        , bioDirection = Emission
        , bioLocation = ""
        , bioComment = Nothing
        , bioPedigree = Nothing
        }

blankActivity :: Text -> [Exchange] -> Activity
blankActivity name exs =
    Activity
        { activityName = name
        , activityDescription = []
        , activityDocumentation = []
        , activitySynonyms = M.empty
        , activityClassification = M.empty
        , activityLocation = "GLO"
        , activityLocationSource = LocationDeclared
        , activityUnit = "kg"
        , exchanges = exs
        , activityParams = M.empty
        , activityParamExprs = M.empty
        , activityNativeType = Nothing
        , activityNativeId = Nothing
        , activityFormulaCheck = Nothing
        }

-- | The activity's own product, in the amount its dataset records. Negative for a treatment.
reference :: UUID -> Double -> Exchange
reference flow amount = technosphere flow amount ReferenceProduct Nothing

-- | An input taken from a named producer.
consumesFrom :: UUID -> UUID -> Double -> Exchange
consumesFrom supplier flow amount = technosphere flow amount Input (Just supplier)

technosphere :: UUID -> Double -> TechRole -> Maybe UUID -> Exchange
technosphere flow amount role supplier =
    TechnosphereExchange
        { techFlowId = flow
        , techAmount = amount
        , techUnitId = kilogram
        , techRole = role
        , techActivityLinkId = supplier
        , techSupplierClaim = maybe ClaimByProduct ClaimById supplier
        , techLocation = "GLO"
        , techComment = Nothing
        , techPedigree = Nothing
        , techShare = Nothing
        , techClassification = M.empty
        , techProperties = noProperties
        }

-- | Activity name -> the column it was given, read back out of the index the export wrote.
indexByName :: FilePath -> IO [(Text, Int)]
indexByName path = do
    rows <- drop 1 . T.lines <$> TIO.readFile path
    pure (concatMap entry rows)
  where
    entry :: Text -> [(Text, Int)]
    entry row = case T.splitOn ";" row of
        (name : _geography : _product : _unit : position : _) ->
            [(name, i) | Just i <- [readMaybe (T.unpack position)]]
        _ -> []

-- | (row, column) -> coefficient, from one of the sparse matrix files.
cells :: FilePath -> IO [((Int, Int), Double)]
cells path = do
    rows <- drop 1 . T.lines <$> TIO.readFile path
    pure (concatMap cell rows)
  where
    cell :: Text -> [((Int, Int), Double)]
    cell line = case T.splitOn ";" line of
        (r : c : v : _) ->
            [ ((i, j), x)
            | Just i <- [readMaybe (T.unpack r)]
            , Just j <- [readMaybe (T.unpack c)]
            , Just x <- [readMaybe (T.unpack v)]
            ]
        _ -> []
