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
import Method.Types (Compartment (..), CompartmentMap (..), FlowDirection (..), Location (..), Method (..), MethodCF (..), MethodCollection (..), Subcompartment (..))
import Service.Compare (Sides (..))
import Service.CompareMethods (CollectionSide (..), CompareMethodsContext (..), CompareMethodsRefusal (..), ForcedPair (..), Scope (..), compareCollections, factorReading, parseForcedPair, profileCollection)
import SynonymDB (buildFromPairs)
import Types (Medium (..))
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
                , ("occupation, forest, extensive", "forest, extensive")
                , ("transformation, to forest, extensive", "to forest, extensive")
                ]
        , cmcCompartments =
            CompartmentMap
                M.empty
                ( M.fromList
                    [ ((Air, Subcompartment "low population density, long-term"), Subcompartment "unspecified (long-term)")
                    , ((Soil, Subcompartment "forestry"), Subcompartment "non-agricultural")
                    , ((Soil, Subcompartment "industrial"), Subcompartment "non-agricultural")
                    ]
                )
        , cmcUnits = units
        , cmcLocations = M.fromList [(Location "FR", [Location "GLO"]), (Location "Europe, Western", [Location "GLO"]), (Location "GLO", [])]
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

-- | The one pair two collections of one category each make.
compared :: [MethodCF] -> [MethodCF] -> CategoryComparison
compared base other = c
  where
    Right MethodCollectionComparison{mccCategories = [c]} =
        compareCollections refData [] EveryCategory (Sides (collection [category "Climate change" base]) (collection [category "Climate change" other]))

-- | (added, removed, changed, unchanged, ambiguous, unconvertible)
counts :: CategoryComparison -> [Int]
counts c = map ($ c) [ccpAddedCount, ccpRemovedCount, ccpChangedCount, ccpUnchangedCount, ccpAmbiguousCount, ccpUnconvertibleCount]

spec :: Spec
spec = compareCategoriesSpec >> collectionSpec >> profileSpec

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

    it "reports a name whose CAS numbers point to two other factors as ambiguous, naming all four" $ do
        let paraquat name value cas = (factor name value){mcfCAS = Just cas}
            c =
                compared
                    [paraquat "paraquat" 17530 "1910-42-5", paraquat "1,1'-dimethyl-4,4'-bipyridinium" 190600 "4685-14-7"]
                    [paraquat "Paraquat" 190600 "4685-14-7", paraquat "Paraquat dichloride" 17530 "1910-42-5"]
        counts c `shouldBe` [0, 0, 0, 0, 1, 0]
        map (\g -> (length (afxBase g), length (afxOther g))) (ccpAmbiguous c) `shouldBe` [(2, 2)]

    it "keeps a pair of one name whose CAS numbers differ when neither points elsewhere" $ do
        let c =
                compared
                    [(factor "lead dioxide" 1){mcfCAS = Just "1309-60-0"}]
                    [(factor "lead dioxide" 2){mcfCAS = Just "60525-54-4"}]
        map cfxMatch (ccpChanged c) `shouldBe` [SameName]

    it "reads a region the geography table holds at the end of a name as the factor's location" $ do
        let c = compared [(factor "ammonia" 134.42){mcfConsumerLocation = Just "FR"}] [factor "Ammonia, FR" 134.42]
        counts c `shouldBe` [0, 0, 0, 1, 0, 0]

    it "reads a code of the geography table that holds a comma of its own" $ do
        let c = compared [(factor "water" 1){mcfConsumerLocation = Just "Europe, Western"}] [factor "Water, Europe, Western" 1]
        counts c `shouldBe` [0, 0, 0, 1, 0, 0]

    it "reads a land flow filed under the whole natural resource medium in its land subcompartment" $ do
        let land name sub = (factor name 1){mcfDirection = Input, mcfCompartment = Just (Compartment "natural resource" sub ""), mcfUnit = "m2a"}
            c = compared [land "forest, extensive" "land"] [land "Occupation, forest, extensive" ""]
        counts c `shouldBe` [0, 0, 0, 1, 0, 0]

    it "pairs a transformation one file writes as an output to land and another as an input from nature" $ do
        let land name direction sub = (factor name 1){mcfDirection = direction, mcfCompartment = Just (Compartment "natural resource" sub ""), mcfUnit = "m2"}
            c = compared [land "to forest, extensive" Output "land"] [land "Transformation, to forest, extensive" Input ""]
        counts c `shouldBe` [0, 0, 0, 1, 0, 0]

    it "keeps any other flow of the whole natural resource medium apart from the land subcompartment" $ do
        let resource name sub = (factor name 1){mcfDirection = Input, mcfCompartment = Just (Compartment "natural resource" sub "")}
            c = compared [resource "forest, extensive" "land"] [resource "forest, extensive" ""]
        counts c `shouldBe` [1, 1, 0, 0, 0, 0]

    it "keeps in the name a last part the geography table does not hold" $ do
        let c = compared [(factor "methane" 1){mcfConsumerLocation = Just "fossil"}] [factor "methane, fossil" 1]
        counts c `shouldBe` [1, 1, 0, 0, 0, 0]

    it "keeps a regionalized factor apart from the one of its substance written for no region" $ do
        let cfs = [factor "Ammonia" 1, factor "Ammonia, FR" 2]
        counts (compared cfs cfs) `shouldBe` [0, 0, 0, 2, 0, 0]

    it "never reads a region in the name of a factor that states its location" $ do
        let c = compared [(factor "ammonia" 1){mcfConsumerLocation = Just "FR"}] [(factor "Ammonia, FR" 1){mcfConsumerLocation = Just "GLO"}]
        counts c `shouldBe` [1, 1, 0, 0, 0, 0]

    it "writes a factor's direction and unit as the factors of a method do" $ do
        let c = compared [(factor "zinc" 1){mcfUnit = ""}] []
        map (\f -> (facDirection f, facUnit f)) (ccpRemoved c) `shouldBe` [(Output, Nothing)]

    it "names the flow of a listed factor and the method of a listed category, which a change addresses" $ do
        let zinc = (factor "zinc" 1){mcfFlowRef = flowId 7}
            climate = (category "Climate change" [zinc]){methodId = flowId 8}
            paired = compareCollections refData [] EveryCategory (Sides (collection [climate]) (collection [category "Climate change" []]))
        fmap (map (\c -> (csdMethodId (ccpBase c), map facFlowRef (ccpRemoved c))) . mccCategories) paired
            `shouldBe` Right [(flowId 8, [flowId 7])]

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
collection ms = MethodCollection{mcMethods = ms, mcScoringSets = [], mcUnregionalized = []}

collections :: [ForcedPair] -> [Method] -> [Method] -> Either CompareMethodsRefusal MethodCollectionComparison
collections forced = scoped forced EveryCategory

scoped :: [ForcedPair] -> Scope -> [Method] -> [Method] -> Either CompareMethodsRefusal MethodCollectionComparison
scoped forced scope base other = compareCollections refData forced scope (Sides (collection base) (collection other))

-- | A pair of categories a comparison made: how, the base name, the other name.
data Paired = Paired CategoryMatch Text Text
    deriving (Eq, Show)

pairsOf :: MethodCollectionComparison -> [Paired]
pairsOf c = [Paired (ccpMatch p) (csdName (ccpBase p)) (csdName (ccpOther p)) | p <- mccCategories c]

withCategory :: Text -> Method -> Method
withCategory cat m = m{methodCategory = cat}

atAir :: Text -> Text -> MethodCF
atAir sub name = (factor name 1){mcfCompartment = Just (Compartment "air" sub "")}

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

    it "compares one pair alone when asked for its base category, the unpaired still listed" $ do
        let Right c = scoped [] (OneCategory "acidification") [category "Acidification" zinc, category "Climate change" zinc, category "Land use" zinc] [category "Acidification" zinc, category "Climate change" zinc]
        pairsOf c `shouldBe` [Paired SameMethodName "Acidification" "Acidification"]
        map csdName (mccUnpairedBase c) `shouldBe` ["Land use"]

    it "finds a forced pair by its base category" $
        fmap pairsOf (scoped [ForcedPair "GWP" "Climate change"] (OneCategory "GWP") [category "GWP" zinc] [category "Climate change" zinc])
            `shouldBe` Right [Paired ForcedByCaller "GWP" "Climate change"]

    it "refuses a category no pair starts from" $
        fmap pairsOf (scoped [] (OneCategory "Land use") [category "Land use" zinc] [category "Ozone depletion" zinc])
            `shouldBe` Left (NotPaired "Land use")

    it "refuses a category name two pairs start from" $ do
        let landUse cat = withCategory cat (category "Land use" zinc)
            base = [landUse "Occupation", landUse "Transformation"]
            other = [withCategory "Occupation" (category "LU occupation" zinc), withCategory "Transformation" (category "LU transformation" zinc)]
        fmap (map (csdName . ccpOther) . mccCategories) (collections [] base other) `shouldBe` Right ["LU occupation", "LU transformation"]
        fmap pairsOf (scoped [] (OneCategory "Land use") base other) `shouldBe` Left (SeveralPairs "Land use" ["LU occupation", "LU transformation"])

    -- One vocabulary writes the long term of low population density, the other an unspecified long term, and neither writes the other's.
    it "pairs across an if_absent row two places only one side writes each" $ do
        let Right c = collections [] [category "Acidification" [atAir "low population density, long-term" "zinc"]] [category "Acidification" [atAir "unspecified (long-term)" "zinc"]]
        map counts (mccCategories c) `shouldBe` [[0, 0, 0, 1, 0, 0]]

    it "keeps the two places of an if_absent row apart when one side writes both, in any of its categories" $ do
        let Right c =
                collections
                    []
                    [category "Acidification" [atAir "low population density, long-term" "zinc"], category "Ozone depletion" [atAir "unspecified (long-term)" "lead"]]
                    [category "Acidification" [atAir "unspecified (long-term)" "zinc"], category "Ozone depletion" [atAir "unspecified (long-term)" "lead"]]
        map counts (mccCategories c) `shouldBe` [[1, 1, 0, 0, 0, 0], [0, 0, 0, 1, 0, 0]]

    it "follows no if_absent row toward a place another row followed on that side reaches too" $ do
        let atSoil sub name value = (factor name value){mcfCompartment = Just (Compartment "soil" sub "")}
            Right c =
                collections
                    []
                    [category "Acidification" [atSoil "forestry" "zinc" 1, atSoil "industrial" "zinc" 2]]
                    [category "Acidification" [atSoil "non-agricultural" "zinc" 2]]
        map counts (mccCategories c) `shouldBe` [[1, 2, 0, 0, 0, 0]]

    it "reads a forced pair written base=other, and refuses any other shape" $ do
        parseForcedPair " GWP = Climate change " `shouldBe` Right (ForcedPair "GWP" "Climate change")
        parseForcedPair "a=b=c" `shouldBe` Left (MalformedPair "a=b=c")
        parseForcedPair "=b" `shouldBe` Left (MalformedPair "=b")

profileSpec :: Spec
profileSpec = describe "profileCollection" $ do
    let water name = (factor name 1){mcfCompartment = Just (Compartment "water" "" "")}
        profile = head (mcpCategories (profileCollection refData (collection [category "Acidification" cfs])))
        cfs =
            [ factor "carbon dioxide" 1
            , factor "Ammonia, FR" 2
            , (water "ammonia"){mcfConsumerLocation = Just "FR"}
            , (water "nitrate"){mcfConsumerLocation = Just "GLO"}
            , factor "lead" 0
            , factor "occupation, forest*" 1
            , factor "zinc" 1
            , factor "Zinc" 2
            ]
    it "counts the factors of each medium" $
        map (\m -> (mdcMedium m, mdcFactorCount m)) (cpfMedia profile)
            `shouldBe` [(Just "air", 6), (Just "water", 2)]

    it "counts the located factors, those whose name carries the location, and the locations" $
        map ($ profile) [cpfLocatedCount, cpfLocatedInNameCount, cpfLocationCount] `shouldBe` [3, 1, 2]

    it "counts the zero factors and the pattern rows" $
        map ($ profile) [cpfZeroCount, cpfPatternCount] `shouldBe` [1, 1]

    it "lists the factors one key answers to as duplicates" $
        map (map facFlowName . dfxFactors) (cpfDuplicates profile) `shouldBe` [["Zinc", "zinc"]]

    it "never reads across an if_absent row a category written at both its places" $ do
        let both = head (mcpCategories (profileCollection refData (collection [category "Acidification" [atAir "low population density, long-term" "zinc", atAir "unspecified (long-term)" "zinc"]])))
        cpfDuplicates both `shouldBe` []

    it "says where each factor's location was read" $
        map (frLocation . factorReading (cmcCompartments refData) (cmcLocations refData)) (take 4 cfs)
            `shouldBe` [Nothing, Just (ReadLocation "FR" InName), Just (ReadLocation "FR" InField), Just (ReadLocation "GLO" InField)]
