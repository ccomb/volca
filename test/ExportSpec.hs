{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the export dispatcher ('Database.Export.serializeDatabase').

The dispatcher is the single entry point the CLI and HTTP handler use to turn an
in-memory 'Database' into bytes. It must: wire every writable format, fail loudly
(never emit empty bytes) for the formats with no writer, and propagate a writer's
own export-guard 'Left' rather than swallow it.
-}
module ExportSpec (spec) where

import Control.Monad (forM_)
import qualified Data.ByteString.Lazy as BL
import Data.Either (isLeft)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import Test.Hspec

import qualified Database as DB
import Database.Export (MethodExportFormat (..), serializeDatabase, serializeMethodCollection)
import Database.Upload (DatabaseFormat (..))
import qualified Method.Types as MT
import Types
import UnitConversion (defaultUnitConfig)

spec :: Spec
spec = describe "Database.Export dispatcher" $ do
    it "serializes a simple database to every writable format" $ do
        db <- buildFixture (Compartment Air (Just "unspecified"))
        forM_ [SimaProCSV, EcoSpold1, EcoSpold2, ILCDProcess, BrightwayExcel] $ \fmt ->
            case serializeDatabase fmt db of
                Left err -> expectationFailure (show fmt <> ": " <> T.unpack err)
                Right (bytes, _warnings) -> BL.null bytes `shouldBe` False

    it "fails loudly for formats with no writer (never a silent empty file)" $ do
        db <- buildFixture (Compartment Air (Just "unspecified"))
        serializeDatabase OpenLcaJsonLd db `shouldSatisfy` isLeft
        serializeDatabase UnknownFormat db `shouldSatisfy` isLeft

    it "writes an exclusion row only to the format that reads the marker back" $ do
        -- VoLCA's own columnar CSV parses "!…" as an exception and must keep it,
        -- or a round-trip loses it and the category counts what it excepted.
        -- SimaPro has no such notion: the row would land as a characterized flow
        -- named "!Occupation, sea*": the sea counted at 1, the exception inverted.
        let mc = MT.MethodCollection [landOccupied] [] [] []
            landOccupied =
                MT.Method
                    { MT.methodId = UUID.nil
                    , MT.methodName = "Land occupied"
                    , MT.methodDescription = Nothing
                    , MT.methodUnit = "m2a"
                    , MT.methodCategory = "Land occupied"
                    , MT.methodMethodology = Nothing
                    , MT.methodFactors = [mkFactor "Occupation, annual crop", mkFactor "!Occupation, sea*"]
                    }
            mkFactor name =
                MT.MethodCF
                    { MT.mcfFlowRef = UUID.nil
                    , MT.mcfFlowName = name
                    , MT.mcfDirection = MT.Input
                    , MT.mcfValue = 1.0
                    , MT.mcfCompartment = Just (MT.Compartment "natural resource" "" "")
                    , MT.mcfCAS = Nothing
                    , MT.mcfUnit = "m2a"
                    , MT.mcfConsumerLocation = Nothing
                    }
            written fmt = case serializeMethodCollection fmt "plain-indicators" mc of
                Left err -> T.unpack err
                Right (bytes, _warnings) -> T.unpack (TE.decodeUtf8 (BL.toStrict bytes))
        written MethodColumnarCSV `shouldSatisfy` isInfixOf "!Occupation, sea*"
        written MethodSimaProCSV `shouldSatisfy` (not . isInfixOf "!Occupation")
        written MethodSimaProCSV `shouldSatisfy` isInfixOf "Occupation, annual crop"

    it "propagates a writer's export-guard failure" $ do
        -- A "raw" emission compartment has no faithful SimaPro section, so the
        -- SimaPro writer's own guard rejects it; the dispatcher must surface that
        -- Left rather than emit a corrupt file.
        db <- buildFixture (Compartment NaturalResource Nothing)
        serializeDatabase SimaProCSV db `shouldSatisfy` isLeft

{- | One activity: a reference product and a single biosphere emission whose
compartment is @comp@ (air for the all-formats case, "raw" for the guard case).
-}
buildFixture :: Compartment -> IO Database
buildFixture comp = do
    r <-
        DB.buildDatabaseWithMatrices
            (BuildInputs defaultUnitConfig mempty Declared [])
            SimpleDatabase
                { sdbActivities = M.singleton (actU, prodU) act
                , sdbTechFlows = M.singleton prodU (TechnosphereFlow prodU "product" unitU M.empty Nothing Nothing)
                , sdbBioFlows = M.singleton co2U (BiosphereFlow co2U "Carbon dioxide" unitU M.empty Nothing Nothing (Just comp))
                , sdbWasteFlows = M.empty
                , sdbUnits = M.singleton unitU (Unit unitU "kg" "kg" "")
                }
    either (fail . ("buildDatabaseWithMatrices: " <>) . T.unpack) pure r
  where
    actU, prodU, co2U, unitU :: UUID
    actU = read "eeeeeeee-0000-4000-8000-000000000001"
    prodU = read "eeeeeeee-0000-4000-8000-0000000000a1"
    co2U = read "eeeeeeee-0000-4000-8000-0000000000c1"
    unitU = read "22222222-0000-4000-8000-000000000099"
    act =
        Activity
            "maker"
            []
            []
            M.empty
            M.empty
            "GLO"
            LocationDeclared
            "kg"
            [ TechnosphereExchange prodU 1.0 unitU ReferenceProduct Nothing ClaimByProduct "" Nothing Nothing Nothing M.empty noProperties
            , BiosphereExchange co2U 0.5 unitU Emission "" Nothing Nothing
            ]
            M.empty
            M.empty
            Nothing
            Nothing
            Nothing
