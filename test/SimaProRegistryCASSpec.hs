{-# LANGUAGE OverloadedStrings #-}

{- | The trailing substance registry a SimaPro export carries (its
@Raw materials@ / @Airborne emissions@ / … blocks, each row
@name;unit;cas;comment@) backfills the per-flow CAS the inventory rows omit, so
the engine's native CAS bridge fires on a SimaPro database. This pins that the
registry is parsed, merged across the parallel workers, and filled – and that a
trailer @Emissions to soil@ block is read as the registry, not mistaken for the
in-process emission section of the same name.
-}
module SimaProRegistryCASSpec (spec) where

import qualified Data.ByteString as BS
import Data.List (find)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import SimaPro.Parser (parseSimaProCSV)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.Hspec
import Types (Activity, BioFlowDB, BiosphereFlow (..), TechFlowDB, UnitDB, WasteFlowDB)
import UnitConversion (UnitConfig, defaultUnitConfig)

{- | One process emitting four substances, then a trailer substance registry
giving three of them a CAS. Methane and fossil CO2 are listed under the
registry-only @Airborne emissions@ header; Cadmium under @Emissions to soil@,
which also names an in-process section – the trailer copy must win there.
Dinitrogen monoxide is emitted but absent from the registry (negative case).
A later @Waterborne emissions@ block re-binds Methane to a different CAS –
the first binding must win (the file-order rule the fill promises).
-}
registryCSV :: BS.ByteString
registryCSV =
    BS.intercalate
        "\r\n"
        [ "{SimaPro 9.6.0.1}"
        , "{CSV separator: semicolon}"
        , "{Decimal separator: .}"
        , ""
        , "Process"
        , ""
        , "Category type"
        , "material"
        , ""
        , "Process name"
        , "Test process"
        , ""
        , "Type"
        , "Unit process"
        , ""
        , "Geography"
        , "GLO"
        , ""
        , "Products"
        , "Widget;kg;1.0;100;not defined;material;"
        , ""
        , "Emissions to air"
        , "Methane;;kg;2.0;;;;;"
        , "Carbon dioxide, fossil;;kg;3.0;;;;;"
        , "Dinitrogen monoxide;;kg;0.5;;;;;"
        , ""
        , "Emissions to soil"
        , "Cadmium;agricultural;kg;4.0;;;;;"
        , ""
        , "End"
        , ""
        , "Quantities"
        , "Mass;Yes"
        , ""
        , "End"
        , ""
        , "Airborne emissions"
        , "Methane;kg;000074-82-8;Formula: CH4"
        , "Carbon dioxide, fossil;kg;000124-38-9;Formula: CO2"
        , ""
        , "End"
        , ""
        , "Emissions to soil"
        , "Cadmium;kg;007440-43-9;Formula: Cd"
        , ""
        , "End"
        , ""
        , "Waterborne emissions"
        , "Methane;kg;000056-23-5;conflicting duplicate binding"
        , ""
        , "End"
        ]

parseRegistryCSV :: IO BioFlowDB
parseRegistryCSV = withSystemTempFile "registry-cas-test.csv" $ \path handle -> do
    BS.hPut handle registryCSV
    hClose handle
    (_, _, bioFlowDB, _, _) <- parseOrFail defaultUnitConfig path
    pure bioFlowDB

casOf :: Text -> BioFlowDB -> Maybe Text
casOf name db = find ((== name) . bfName) (M.elems db) >>= bfCAS

spec :: Spec
spec = describe "SimaPro trailing substance registry backfills flow CAS" $ do
    it "fills an air emission's CAS from the Airborne emissions registry" $ do
        db <- parseRegistryCSV
        -- 000074-82-8 → normalized (leading zeros stripped from the first group)
        casOf "Methane" db `shouldBe` Just "74-82-8"

    it "fills fossil CO2's CAS (a name the method cannot resolve by origin)" $ do
        db <- parseRegistryCSV
        casOf "Carbon dioxide, fossil" db `shouldBe` Just "124-38-9"

    it "reads a trailer 'Emissions to soil' block as the registry, not the process section" $ do
        db <- parseRegistryCSV
        casOf "Cadmium" db `shouldBe` Just "7440-43-9"

    it "leaves a flow the registry does not list without a CAS" $ do
        db <- parseRegistryCSV
        casOf "Dinitrogen monoxide" db `shouldBe` Nothing

    it "keeps the first CAS when the registry binds a name twice" $ do
        db <- parseRegistryCSV
        -- The later Waterborne block re-binds Methane to 56-23-5; the
        -- Airborne binding came first in the file and must survive.
        casOf "Methane" db `shouldBe` Just "74-82-8"

{- | Parse, failing the example when the parser refuses the file. The parser
now returns 'Left' for a flow written in two units no conversion relates.
-}
parseOrFail :: UnitConfig -> FilePath -> IO ([Activity], TechFlowDB, BioFlowDB, WasteFlowDB, UnitDB)
parseOrFail cfg path = either (fail . show) pure =<< parseSimaProCSV cfg path
