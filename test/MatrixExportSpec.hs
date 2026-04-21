{-# LANGUAGE OverloadedStrings #-}

module MatrixExportSpec (spec) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.UUID as UUID
import GoldenData
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
import Types

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

                -- Check off-diagonal entries (should be NEGATIVE for (I-A) format)
                let offDiagonalLines = tail lines -- Skip header
                let offDiagonalEntries = filter (\l -> not (T.isInfixOf ";1.0;" l)) offDiagonalLines

                -- For SAMPLE.min3: Expected -0.6 and -0.4
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
            let targetUUID = read "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa" :: UUID.UUID
            info <- extractMatrixDebugInfo db targetUUID Nothing
            let n = fromIntegral (dbActivityCount db)
            length (mdSupplyVector info) `shouldBe` n
            length (mdDemandVector info) `shouldBe` n

        it "demand vector has exactly one non-zero entry" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let targetUUID = read "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa" :: UUID.UUID
            info <- extractMatrixDebugInfo db targetUUID Nothing
            length (filter (/= 0.0) (mdDemandVector info)) `shouldBe` 1

        it "inventory vector is non-empty (has biosphere contributions)" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let targetUUID = read "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa" :: UUID.UUID
            info <- extractMatrixDebugInfo db targetUUID Nothing
            any (/= 0.0) (mdInventoryVector info) `shouldBe` True

        it "flow filter restricts biosphere triples" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let targetUUID = read "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa" :: UUID.UUID
            infoAll <- extractMatrixDebugInfo db targetUUID Nothing
            infoFiltered <- extractMatrixDebugInfo db targetUUID (Just "carbon")
            -- Filtered should have ≤ triples than unfiltered
            let nAll = length (mdInventoryVector infoAll)
                nFiltered = length (mdInventoryVector infoFiltered)
            nFiltered `shouldSatisfy` (<= nAll)

    describe "exportMatrixDebugCSVs" $ do
        it "creates supply chain and biosphere CSV files" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let targetUUID = read "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa" :: UUID.UUID
            info <- extractMatrixDebugInfo db targetUUID Nothing
            withSystemTempDirectory "acv-debug" $ \tmpDir -> do
                let base = tmpDir </> "debug"
                exportMatrixDebugCSVs base info
                supplyContent <- TIO.readFile (base ++ "_supply_chain.csv")
                bioContent <- TIO.readFile (base ++ "_biosphere_matrix.csv")
                T.isInfixOf "activity_id" supplyContent `shouldBe` True
                T.isInfixOf "flow_id" bioContent `shouldBe` True

        it "supply chain CSV has one row per activity" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let targetUUID = read "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa" :: UUID.UUID
            info <- extractMatrixDebugInfo db targetUUID Nothing
            withSystemTempDirectory "acv-debug" $ \tmpDir -> do
                let base = tmpDir </> "debug"
                exportMatrixDebugCSVs base info
                content <- TIO.readFile (base ++ "_supply_chain.csv")
                -- header + 3 activities (SAMPLE.min3)
                length (lines (T.unpack content)) `shouldBe` 4
