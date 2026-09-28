{-# LANGUAGE OverloadedStrings #-}

{- | Comparing two method collections.

Each collection here is a few factors the test writes by hand, so each case
changes one thing between the two sides and says what the comparison must
make of it.
-}
module CompareMethodsSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.UUID as UUID
import Test.Hspec

import API.Types
import Method.Types (Compartment (..), CompartmentMap (..), FlowDirection (..), Method (..), MethodCF (..))
import Service.Compare (Sides (..))
import Service.CompareMethods (CompareMethodsContext (..), compareCategories)
import SynonymDB (buildFromPairs)
import UnitConversion (Dimension, UnitConfig, UnitDef (..), defaultUnitConfig, mkUnitConfig, ucDimensionOrder, ucUnits)

-- | The default unit table plus the gram.
units :: UnitConfig
units =
    mkUnitConfig
        (ucDimensionOrder defaultUnitConfig)
        (M.insert "g" (UnitDef massSlot 0.001) (ucUnits defaultUnitConfig))
  where
    massSlot :: Dimension
    massSlot = [if d == "mass" then 1 else 0 | d <- ucDimensionOrder defaultUnitConfig]

refData :: CompareMethodsContext
refData =
    CompareMethodsContext
        { cmcSynonyms =
            buildFromPairs
                [ ("carbon dioxide", "carbon dioxide, fossil")
                , ("methane, fossil", "methane, fossil origin")
                , ("methane, biogenic", "methane, non-fossil")
                ]
        , cmcCompartments = CompartmentMap M.empty M.empty
        , cmcUnits = units
        }

factor :: Text -> Double -> MethodCF
factor name value =
    MethodCF
        { mcfFlowRef = UUID.nil
        , mcfFlowName = name
        , mcfDirection = Output
        , mcfValue = value
        , mcfCompartment = Just (Compartment "air" "" "")
        , mcfCAS = Nothing
        , mcfUnit = "kg"
        , mcfConsumerLocation = Nothing
        }

category :: Text -> [MethodCF] -> Method
category name cfs =
    Method
        { methodId = UUID.nil
        , methodName = name
        , methodDescription = Nothing
        , methodUnit = "kg CO2 eq"
        , methodCategory = name
        , methodMethodology = Nothing
        , methodFactors = cfs
        }

compared :: [MethodCF] -> [MethodCF] -> CategoryComparison
compared base other =
    compareCategories refData SameMethodName (Sides (category "Climate change" base) (category "Climate change" other))

-- | (added, removed, changed, unchanged, ambiguous, unconvertible)
counts :: CategoryComparison -> [Int]
counts c = map ($ c) [ccpAddedCount, ccpRemovedCount, ccpChangedCount, ccpUnchangedCount, ccpAmbiguousCount, ccpUnconvertibleCount]

spec :: Spec
spec = describe "compareCategories" $ do
    it "finds nothing between a category and itself" $ do
        let cfs = [factor "carbon dioxide" 1, factor "methane, fossil" 29.7]
        counts (compared cfs cfs) `shouldBe` [0, 0, 0, 2, 0, 0]

    it "pairs two names of one synonym class and says so" $ do
        let c = compared [factor "carbon dioxide" 1] [factor "carbon dioxide, fossil" 2]
        map cfxMatch (ccpChanged c) `shouldBe` [SameSynonymClass]
        map cfxRatio (ccpChanged c) `shouldBe` [Just 2]

    it "pairs on a CAS number a name the registry does not know" $ do
        let c =
                compared
                    [(factor "carbon dioxide" 1){mcfCAS = Just "124-38-9"}]
                    [(factor "CO2" 1){mcfCAS = Just "124-38-9"}]
        counts c `shouldBe` [0, 0, 0, 1, 0, 0]

    it "never pairs on a CAS two names the registry keeps apart" $ do
        let c =
                compared
                    [(factor "methane, fossil" 29.7){mcfCAS = Just "74-82-8"}]
                    [(factor "methane, biogenic" 27){mcfCAS = Just "74-82-8"}]
        counts c `shouldBe` [1, 1, 0, 0, 0, 0]

    it "keeps apart two rows of one name written per two units" $ do
        let cfs = [factor "water/kg" 1, factor "water/m3" 1000]
        counts (compared cfs cfs) `shouldBe` [0, 0, 0, 2, 0, 0]

    it "reports a key several factors of one side answer, and pairs none of them" $ do
        let c = compared [factor "zinc" 1, factor "Zinc" 2] [factor "zinc" 1]
        counts c `shouldBe` [0, 0, 0, 0, 1, 0]
        map (length . afxBase) (ccpAmbiguous c) `shouldBe` [2]

    it "pairs pattern rows on their prefix, never on a substance" $ do
        let c = compared [factor "occupation, forest*" 1, factor "!occupation, forest, intensive" 0] [factor "occupation, forest*" 2, factor "!occupation, forest, intensive" 0]
        map cfxMatch (ccpChanged c) `shouldBe` [SamePattern]
        ccpUnchangedCount c `shouldBe` 1

    it "reads an unspecified subcompartment as the whole medium" $ do
        let c = compared [(factor "zinc" 1){mcfCompartment = Just (Compartment "water" "unspecified" "")}] [(factor "zinc" 1){mcfCompartment = Just (Compartment "water" "" "")}]
        counts c `shouldBe` [0, 0, 0, 1, 0, 0]

    -- Both parsers write the long term into the subcompartment text, never the qualifier.
    it "keeps a long-term factor apart from its short-term twin" $ do
        let c = compared [(factor "zinc" 1){mcfCompartment = Just (Compartment "water" "unspecified (long-term)" "")}] [(factor "zinc" 1){mcfCompartment = Just (Compartment "water" "" "")}]
        counts c `shouldBe` [1, 1, 0, 0, 0, 0]

    it "converts a factor per gram onto the base's kilogram" $ do
        let c = compared [factor "zinc" 1000] [(factor "zinc" 1){mcfUnit = "g"}]
        counts c `shouldBe` [0, 0, 0, 1, 0, 0]

    it "reads a flow unit against an impact unit per the reference unit" $ do
        let c = compared [(factor "zinc" 1){mcfUnit = "kg CO2 eq"}] [(factor "zinc" 0.002){mcfUnit = "g"}]
        map cfxReading (ccpChanged c) `shouldBe` [ReadPerReferenceUnit]
        map cfxRatio (ccpChanged c) `shouldBe` [Just 2]

    it "sets apart two flow units that do not convert" $ do
        let c = compared [factor "zinc" 1] [(factor "zinc" 1){mcfUnit = "m"}]
        counts c `shouldBe` [0, 0, 0, 0, 0, 1]

    it "reports a zero that became a value as a change without a ratio" $ do
        let c = compared [factor "zinc" 0] [factor "zinc" 0.3]
        map cfxRatio (ccpChanged c) `shouldBe` [Nothing]

    it "leads with a sign flip, then the ratio farthest from one" $ do
        let c =
                compared
                    [factor "zinc" 1, factor "lead" 1, factor "copper" 0.5]
                    [factor "zinc" 1.1, factor "lead" 3, factor "copper" (-0.5)]
        map (facFlowName . cfxBase) (ccpChanged c) `shouldBe` ["copper", "lead", "zinc"]
        ccpLargestRatio c `shouldBe` Just (-1)

    it "treats a relative difference of 1e-12 as no change" $ do
        counts (compared [factor "zinc" 1] [factor "zinc" (1 + 1e-12)]) `shouldBe` [0, 0, 0, 1, 0, 0]
