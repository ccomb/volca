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
import Method.Types (Compartment (..), CompartmentMap (..), FlowDirection (..), Method (..), MethodCF (..), MethodCollection (..))
import Service.Compare (Sides (..))
import Service.CompareMethods (CollectionSide (..), CompareMethodsContext (..), CompareMethodsRefusal (..), ForcedPair (..), compareCategories, compareCollections, parseForcedPair)
import SynonymDB (buildFromPairs)
import UnitConversion (Dimension, UnitConfig, UnitDef (..), defaultUnitConfig, mkUnitConfig, ucDimensionOrder, ucUnits)

-- | The default unit table plus the gram, and two units only the case tells apart.
units :: UnitConfig
units =
    mkUnitConfig
        (ucDimensionOrder defaultUnitConfig)
        ( M.insert "g" (UnitDef massSlot 0.001)
            . M.insert "Mt" (UnitDef massSlot 1e9)
            . M.insert "mt" (UnitDef massSlot 1e3)
            $ ucUnits defaultUnitConfig
        )
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
                , ("methane, from soil", "methane, land transformation")
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

-- | A flow identifier a file writes, distinct per number.
flowId :: Word -> UUID.UUID
flowId n = UUID.fromWords 0 0 0 (fromIntegral n)

compared :: [MethodCF] -> [MethodCF] -> CategoryComparison
compared base other =
    compareCategories refData SameMethodName (Sides (category "Climate change" base) (category "Climate change" other))

-- | (added, removed, changed, unchanged, ambiguous, unconvertible)
counts :: CategoryComparison -> [Int]
counts c = map ($ c) [ccpAddedCount, ccpRemovedCount, ccpChangedCount, ccpUnchangedCount, ccpAmbiguousCount, ccpUnconvertibleCount]

spec :: Spec
spec = compareCategoriesSpec >> collectionSpec

compareCategoriesSpec :: Spec
compareCategoriesSpec = describe "compareCategories" $ do
    it "finds nothing between a category and itself" $ do
        let cfs = [factor "carbon dioxide" 1, factor "methane, fossil" 29.7]
        counts (compared cfs cfs) `shouldBe` [0, 0, 0, 2, 0, 0]

    it "pairs on the flow identifier first, whatever the two names" $ do
        let cfs = [(factor "zinc" 1){mcfFlowRef = flowId 1}, (factor "Zinc" 1){mcfFlowRef = flowId 2}]
            renamed = [(factor "zinc (II)" 1){mcfFlowRef = flowId 1}, (factor "Zinc" 1){mcfFlowRef = flowId 2}]
        counts (compared cfs renamed) `shouldBe` [0, 0, 0, 2, 0, 0]

    it "pairs on the name first two rows of one synonym class a category writes" $ do
        let cfs = [factor "carbon dioxide" 1, factor "carbon dioxide, fossil" 1]
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

    it "never lets a CAS number several known names share make them ambiguous" $ do
        let methane name = (factor name 1){mcfCAS = Just "74-82-8"}
            c = compared [methane "methane, fossil", methane "methane, biogenic"] [methane "methane, from soil"]
        counts c `shouldBe` [1, 2, 0, 0, 0, 0]

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

    it "leads with a factor that appeared from zero, as with one that vanished" $ do
        let c = compared [factor "zinc" 1, factor "lead" 0, factor "copper" 1] [factor "zinc" 1.1, factor "lead" 0.3, factor "copper" 0]
        map (facFlowName . cfxBase) (ccpChanged c) `shouldBe` ["copper", "lead", "zinc"]

    it "sets apart a factor whose unit the table cannot settle" $ do
        let c = compared [factor "zinc" 1] [(factor "zinc" 1){mcfUnit = "MT"}]
        counts c `shouldBe` [0, 0, 0, 0, 0, 1]

    it "leads with a sign flip, then the ratio farthest from one" $ do
        let c =
                compared
                    [factor "zinc" 1, factor "lead" 1, factor "copper" 0.5]
                    [factor "zinc" 1.1, factor "lead" 3, factor "copper" (-0.5)]
        map (facFlowName . cfxBase) (ccpChanged c) `shouldBe` ["copper", "lead", "zinc"]
        ccpLargestRatio c `shouldBe` Just (-1)

    it "treats a relative difference of 1e-12 as no change" $ do
        counts (compared [factor "zinc" 1] [factor "zinc" (1 + 1e-12)]) `shouldBe` [0, 0, 0, 1, 0, 0]

collection :: [Method] -> MethodCollection
collection ms = MethodCollection{mcMethods = ms, mcDamageCategories = [], mcNormWeightSets = [], mcScoringSets = []}

collections :: [ForcedPair] -> [Method] -> [Method] -> Either CompareMethodsRefusal MethodCollectionComparison
collections forced base other = compareCollections refData forced (Sides (collection base) (collection other))

-- | A pair of categories a comparison made: how, the base name, the other name.
data Paired = Paired CategoryMatch Text Text
    deriving (Eq, Show)

pairsOf :: MethodCollectionComparison -> [Paired]
pairsOf c = [Paired (ccpMatch p) (csdName (ccpBase p)) (csdName (ccpOther p)) | p <- mccCategories c]

withCategory :: Text -> Method -> Method
withCategory cat m = m{methodCategory = cat}

collectionSpec :: Spec
collectionSpec = describe "compareCollections" $ do
    let zinc = [factor "zinc" 1]
    it "pairs categories on their name, case and spacing aside" $
        fmap pairsOf (collections [] [category "Climate change" zinc] [category "climate  CHANGE" zinc])
            `shouldBe` Right [Paired SameMethodName "Climate change" "climate  CHANGE"]

    it "pairs on the impact category what the names left" $
        fmap pairsOf (collections [] [withCategory "Climate change" (category "GWP100" zinc)] [category "Climate change" zinc])
            `shouldBe` Right [Paired SameImpactCategory "GWP100" "Climate change"]

    it "never pairs two categories whose file states no impact category" $ do
        let Right c = collections [] [withCategory "unknown" (category "GWP" zinc), withCategory "unknown" (category "ODP" zinc)] [withCategory "unknown" (category "Ozone" zinc)]
        mccAmbiguous c `shouldBe` []
        map csdName (mccUnpairedBase c) `shouldBe` ["GWP", "ODP"]

    it "lists a category without a partner on its side" $ do
        let Right c = collections [] [category "Acidification" zinc] [category "Ozone depletion" zinc]
        map csdName (mccUnpairedBase c) `shouldBe` ["Acidification"]
        map csdName (mccUnpairedOther c) `shouldBe` ["Ozone depletion"]

    it "reports two categories of one name as ambiguous, and pairs neither" $ do
        let Right c = collections [] [category "Climate change" zinc, category "Climate change" zinc] [category "Climate change" zinc]
        mccCategories c `shouldBe` []
        map (length . acgBase) (mccAmbiguous c) `shouldBe` [2]

    it "takes a forced pair before any rung" $
        fmap pairsOf (collections [ForcedPair "gwp" "Climate change"] [category "GWP" zinc] [category "Climate change" zinc])
            `shouldBe` Right [Paired ForcedByCaller "GWP" "Climate change"]

    it "refuses a forced pair naming no category" $
        fmap pairsOf (collections [ForcedPair "Nothing" "Climate change"] [category "GWP" zinc] [category "Climate change" zinc])
            `shouldBe` Left (UnknownCategory BaseCollection "Nothing")

    it "refuses a forced pair naming two categories" $
        fmap pairsOf (collections [ForcedPair "GWP" "Climate change"] [category "GWP" zinc, category "gwp" zinc] [category "Climate change" zinc])
            `shouldBe` Left (SeveralCategories BaseCollection "GWP")

    it "refuses a category named in two forced pairs" $
        fmap pairsOf (collections [ForcedPair "GWP" "A", ForcedPair "gwp" "B"] [category "GWP" zinc] [category "A" zinc, category "B" zinc])
            `shouldBe` Left (PairedTwice BaseCollection "gwp")

    it "reads a forced pair written base=other, and refuses any other shape" $ do
        parseForcedPair " GWP = Climate change " `shouldBe` Right (ForcedPair "GWP" "Climate change")
        parseForcedPair "a=b=c" `shouldBe` Left (MalformedPair "a=b=c")
        parseForcedPair "=b" `shouldBe` Left (MalformedPair "=b")
