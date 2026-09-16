module BuiltinSpec (spec) where

import Builtin
import qualified Data.ByteString.Lazy as BL
import Data.List (nub)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Test.Hspec

spec :: Spec
spec = describe "Builtin" $ do
    -- The generator runs before every build; this is what notices a data/
    -- file edited without regenerating, or a literal the generator mangled.
    it "carries every table of data/ byte for byte" $
        mapM_
            (\(t, path) -> BL.readFile path >>= \onDisk -> builtinContent t `shouldBe` onDisk)
            [ (BuiltinFlowSynonyms, "data/flows.csv")
            , (BuiltinCompartments, "data/compartments.csv")
            , (BuiltinUnits, "data/units.csv")
            , (BuiltinEnergyDensities, "data/energy_density.csv")
            ]

    it "carries every method of data/methods/ byte for byte" $
        mapM_
            (\(m, path) -> BL.readFile path >>= \onDisk -> builtinMethodContent m `shouldBe` onDisk)
            [(BuiltinPlainIndicators, "data/methods/plain-indicators.csv")]

    it "carries the geographies byte for byte" $
        BL.readFile "data/geographies.csv" >>= (builtinGeographies `shouldBe`)

    it "reports the version data/VERSION states" $ do
        v <- TIO.readFile "data/VERSION"
        builtinDataVersion `shouldBe` DataVersion (T.strip v)

    it "names every table distinctly" $
        nub (map builtinName builtinTables) `shouldBe` map builtinName builtinTables

    it "names every method distinctly" $
        nub (map builtinMethodName builtinMethods) `shouldBe` map builtinMethodName builtinMethods
