{-# LANGUAGE OverloadedStrings #-}

module BuiltinSpec (spec) where

import Builtin
import qualified Data.ByteString.Lazy as BL
import Data.List (nub)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Test.Hspec

import Method.ParserCSV (parseMethodCSVBytes)
import Method.SimaProScoring (factorClashes)

import API.Routes (groupSetsOf, readGroups, readyGroupSets)
import API.Types (GroupSet (..))
import CrossDBRegionalLCIAFixture (mkDB)
import Data.Bifunctor (first)
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Types (Activity (..), Database (..))

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

    -- Built-in methods skip the load that names such clashes.
    it "gives no flow two factors at one place in any method" $
        mapM_
            (\m -> fmap (concatMap factorClashes) (parseMethodCSVBytes (BL.toStrict (builtinMethodContent m))) `shouldBe` Right [])
            builtinMethods

    it "carries the method catalogue byte for byte" $
        BL.readFile "data/method-catalog.toml" >>= (builtinMethodCatalog `shouldBe`)

    it "carries the sets of groups byte for byte" $
        BL.readFile "data/group-sets.json" >>= (builtinGroupSets `shouldBe`)

    it "carries sets of groups a split reads as they are, each on its own system" $
        case first T.pack readyGroupSets >>= traverse (\s -> first ((gstName s <> ": ") <>) (gstSystem s <$ readGroups Nothing (gstGroups s))) of
            Left why -> expectationFailure (T.unpack why)
            Right systems -> (length systems >= 2, nub systems == systems) `shouldBe` (True, True)

    it "offers a database the sets of the systems its processes carry, and those only" $ do
        let fixture = mkDB 0 ["FR"] []
            carrying classes = fixture{dbActivities = V.map (\a -> a{activityClassification = classes}) (dbActivities fixture)}
            offered classes = map gstSystem <$> groupSetsOf (carrying classes)
        offered (M.singleton "CPC" "6511: Road transport") `shouldBe` Right ["CPC"]
        offered (M.singleton "Category type" "transport") `shouldBe` Right ["Category type"]
        offered (M.singleton "ISIC rev.4 ecoinvent" "4923: Freight transport by road") `shouldBe` Right []

    it "carries the geographies byte for byte" $
        BL.readFile "data/geographies.csv" >>= (builtinGeographies `shouldBe`)

    it "carries the chemical names byte for byte" $
        BL.readFile "data/chem_synonyms.csv" >>= (builtinChemSynonyms `shouldBe`)

    it "reports the version data/VERSION states" $ do
        v <- TIO.readFile "data/VERSION"
        builtinDataVersion `shouldBe` DataVersion (T.strip v)

    it "names every table distinctly" $
        nub (map builtinName builtinTables) `shouldBe` map builtinName builtinTables

    it "names every method distinctly" $
        nub (map builtinMethodName builtinMethods) `shouldBe` map builtinMethodName builtinMethods
