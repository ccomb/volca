{-# LANGUAGE OverloadedStrings #-}

module MappingSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID, fromWords, nil)
import Data.UUID.V4 (nextRandom)
import Test.Hspec

import Data.Either (isLeft)
import Data.Maybe (isJust)
import Method.ChemSynonyms (emptyChemSynonyms, parseChemSynonymsCSV)
import Method.FlowResolver (parseCompartment)
import Method.Mapping
import Method.ParserCSV (parseMethodCSVBytes)
import Method.Types (Compartment (..), CompartmentMap (..), EnergyDensity (..), FlowDirection (..), Method (..), MethodCF (..), Subcompartment (..), buildCompartmentMapFromCSV)
import SynonymDB (BridgeDirection (..), SynEdge (..), buildFromEdges, buildFromPairs, emptySynonymDB, normalizeName)
import TestHelpers (unitDef)
import Types (
    BiosphereFlow (..),
    Medium (..),
    Unit (..),
 )
import qualified Types as VT
import UnitConversion (UnitConfig (..), defaultDimensionOrder, defaultUnitConfig, mkUnitConfig)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

mkFlow :: UUID -> Text -> VT.Medium -> Maybe Text -> BiosphereFlow
mkFlow fid name cat msub =
    BiosphereFlow
        { bfId = fid
        , bfName = name
        , bfUnitId = nil
        , bfSynonyms = M.empty
        , bfCAS = Nothing
        , bfSubstanceId = Nothing
        , bfCompartment = Just (VT.Compartment cat msub)
        }

mkCF :: Text -> Maybe Text -> Double -> MethodCF
mkCF name mCas val =
    MethodCF
        { mcfFlowRef = nil
        , mcfFlowName = name
        , mcfDirection = Output
        , mcfValue = val
        , mcfCompartment = Nothing
        , mcfCAS = mCas
        , mcfUnit = "kg"
        , mcfConsumerLocation = Nothing
        }

mkCFComp :: Text -> Text -> Text -> Double -> MethodCF
mkCFComp name medium subcomp val =
    (mkCF name Nothing val)
        { mcfCompartment = Just (Compartment medium subcomp "")
        }

unitNamed :: Text -> Unit
unitNamed n = Unit{unitId = nil, unitName = n, unitSymbol = n, unitComment = ""}

-- | UnitConfig with both kg and g (mass) so g→kg conversion succeeds.
gKgUnitConfig :: UnitConfig
gKgUnitConfig =
    mkUnitConfig
        defaultDimensionOrder
        ( M.fromList
            [ ("kg", unitDef "mass" 1.0)
            , ("g", unitDef "mass" 0.001)
            ]
        )

{- | UnitConfig whose mass dimension has NO canonical base (g only, no kg at
factor 1.0), so 'normalizeToCanonical' fails – exercises the result-expression
branch's hard-fail to 0.
-}
gOnlyUnitConfig :: UnitConfig
gOnlyUnitConfig =
    mkUnitConfig
        defaultDimensionOrder
        (M.fromList [("g", unitDef "mass" 0.001)])

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
    describe "findFlowByUUID" $ do
        it "finds a flow by its UUID" $ do
            fid <- nextRandom
            let flow = mkFlow fid "CO2" Air Nothing
                db = M.singleton fid flow
            fmap bfId (findFlowByUUID db fid) `shouldBe` Just fid

        it "returns Nothing for unknown UUID" $ do
            fid <- nextRandom
            fmap bfId (findFlowByUUID M.empty fid) `shouldBe` Nothing

    describe "pickByCompartment (via findFlowByNameComp)" $ do
        it "returns Nothing for empty candidate list" $
            fmap bfId (findFlowByNameComp mempty M.empty "co2" Nothing) `shouldBe` Nothing

        it "returns first flow when no compartment preference" $ do
            fid1 <- nextRandom
            fid2 <- nextRandom
            let f1 = mkFlow fid1 "co2" Air Nothing
                f2 = mkFlow fid2 "co2" Water Nothing
                byName = M.singleton "co2" [f1, f2]
            fmap bfId (findFlowByNameComp mempty byName "co2" Nothing) `shouldBe` Just fid1

        it "prefers exact medium+subcomp match" $ do
            fid1 <- nextRandom
            fid2 <- nextRandom
            let fAir = mkFlow fid1 "co2" Air (Just "urban air")
                fWater = mkFlow fid2 "co2" Water (Just "surface water")
                byName = M.singleton "co2" [fWater, fAir]
                comp = Compartment "air" "urban air" ""
            fmap bfId (findFlowByNameComp mempty byName "co2" (Just comp)) `shouldBe` Just fid1

        it "gives a row written for the whole medium to a flow of that medium" $ do
            -- Every flow of the medium reads that line, whatever its own
            -- subcompartment.
            fid1 <- nextRandom
            fid2 <- nextRandom
            let fAir = mkFlow fid1 "co2" Air (Just "non-urban air")
                fWater = mkFlow fid2 "co2" Water Nothing
                byName = M.singleton "co2" [fWater, fAir]
                comp = Compartment "air" "" ""
            fmap bfId (findFlowByNameComp mempty byName "co2" (Just comp)) `shouldBe` Just fid1

        it "gives a row written at unspecified to no flow at another subcompartment" $ do
            -- "unspecified" is a place: the flow at "non-urban air" does not
            -- read its factor, so the row is not attached to it.
            fid <- nextRandom
            let fAir = mkFlow fid "co2" Air (Just "non-urban air")
                comp = Compartment "air" "unspecified" ""
            fmap bfId (findFlowByNameComp mempty (M.singleton "co2" [fAir]) "co2" (Just comp)) `shouldBe` Nothing

        it "takes the flow at unspecified for a row written at unspecified" $ do
            -- A row whose factors differ by location reaches only the flow it
            -- is attached to, so "unspecified" must not read as "any" and hand
            -- the row to whichever flow the index lists first.
            fidSurface <- nextRandom
            fidUnspec <- nextRandom
            let fSurface = mkFlow fidSurface "water" Water (Just "surface water")
                fUnspec = mkFlow fidUnspec "water" Water (Just "unspecified")
                byName = M.singleton "water" [fSurface, fUnspec]
                comp = Compartment "water" "unspecified" ""
            fmap bfId (findFlowByNameComp mempty byName "water" (Just comp)) `shouldBe` Just fidUnspec

        it "answers nothing when no candidate is in the stated medium" $ do
            -- A row for an emission to air does not describe a water flow of
            -- the same name, so the name matcher has found nothing and the
            -- cascade is free to try the next one.
            fid1 <- nextRandom
            let fWater = mkFlow fid1 "co2" Water Nothing
                byName = M.singleton "co2" [fWater]
                comp = Compartment "air" "" ""
            fmap bfId (findFlowByNameComp mempty byName "co2" (Just comp)) `shouldBe` Nothing

        it "reads a medium stated with its direction as that medium" $ do
            -- "emissions to air" is how one family of sources spells the air
            -- medium. The direction is already recorded on the exchange, so
            -- the two words name one medium and need no rule between them.
            -- A spelling that carries more than the medium does need one, and
            -- the "urban air" case below is where that is pinned.
            fid <- nextRandom
            let flow = mkFlow fid "ammonia" Air Nothing
                byName = M.singleton "ammonia" [flow]
                comp = Compartment "emissions to air" "" ""
            fmap bfId (findFlowByNameComp mempty byName "ammonia" (Just comp)) `shouldBe` Just fid

        it "does not read a long-term subcompartment as the immediate one" $ do
            -- "low. pop." is contained in "low. pop., long-term"; a delayed
            -- emission is not the immediate one, so the row takes the flow at
            -- the subcompartment it names and not the one merely containing it.
            fidLongTerm <- nextRandom
            fidNow <- nextRandom
            let fLongTerm = mkFlow fidLongTerm "co2" Air (Just "low. pop., long-term")
                fNow = mkFlow fidNow "co2" Air (Just "low. pop.")
                byName = M.singleton "co2" [fLongTerm, fNow]
                comp = Compartment "air" "low. pop." ""
            fmap bfId (findFlowByNameComp mempty byName "co2" (Just comp)) `shouldBe` Just fidNow

        it "meets a flow through a compartment the table relates" $ do
            -- The medium is a condition now, and "Emissions to air" is the
            -- spelling an ILCD method writes for what a database files under
            -- "air". Both sides go through the table, as scoring does.
            fid <- nextRandom
            let fAir = mkFlow fid "co2" Air Nothing
                byName = M.singleton "co2" [fAir]
                cmap = mempty{cmSpellings = M.singleton ("emissions to air", "", "") (Compartment "air" "" "")}
                comp = Compartment "Emissions to air" "" ""
            fmap bfId (findFlowByNameComp (Placing cmap mempty) byName "co2" (Just comp)) `shouldBe` Just fid

        it "gives a row, among flows reading it alike, to the one bearing its name" $ do
            -- Both rows lose their unit suffix to one name; each must stay on
            -- the flow written in its own unit, whatever the index order.
            fidKg <- nextRandom
            fidM3 <- nextRandom
            let fKg = mkFlow fidKg "Waste water" Water (Just "river")
                fM3 = mkFlow fidM3 "Waste water/m3" Water (Just "unspecified")
                comp = Compartment "water" "" ""
                pick name order = fmap bfId (findFlowByNameComp mempty (M.singleton "waste water" order) name (Just comp))
            pick "Waste water" [fM3, fKg] `shouldBe` Just fidKg
            pick "Waste water/m3" [fKg, fM3] `shouldBe` Just fidM3

        it "gives a row to no flow that would not read it" $ do
            -- Neither candidate is at "low. pop.", and neither reads a factor
            -- written there: not the long-term one, and not the one filed
            -- under no subcompartment, which reads only its medium's line.
            fidLongTerm <- nextRandom
            fidPlain <- nextRandom
            let fLongTerm = mkFlow fidLongTerm "co2" Air (Just "low. pop., long-term")
                fPlain = mkFlow fidPlain "co2" Air Nothing
                comp = Compartment "air" "low. pop." ""
            fmap bfId (findFlowByNameComp mempty (M.singleton "co2" [fLongTerm, fPlain]) "co2" (Just comp))
                `shouldBe` Nothing

        it "gives a row to the flow an if_absent row sends to it, while that row holds" $ do
            -- A method writing no forestry reads its non-agricultural line
            -- for a flow in forest soil, so that flow is the one the line names.
            -- A method that writes forestry keeps the two apart.
            fidForest <- nextRandom
            fidAgri <- nextRandom
            let fForest = mkFlow fidForest "zinc" Soil (Just "forestry")
                fAgri = mkFlow fidAgri "zinc" Soil (Just "agricultural")
                byName = M.singleton "zinc" [fAgri, fForest]
                cmap = mempty{cmIfAbsent = M.singleton (Soil, Subcompartment "forestry") (Subcompartment "non-agricultural")}
                comp = Compartment "soil" "non-agricultural" ""
                speaksForest = methodVocabulary cmap [mkCFComp "zinc" "soil" "forestry" 1.0]
            fmap bfId (findFlowByNameComp (Placing cmap mempty) byName "zinc" (Just comp)) `shouldBe` Just fidForest
            fmap bfId (findFlowByNameComp (Placing cmap speaksForest) byName "zinc" (Just comp)) `shouldBe` Nothing

    describe "spreadLocatedRows" $ do
        it "hands a located row to every flow of its flow's name and medium, an unlocated one to none" $ do
            fidRoot <- nextRandom
            fidDep <- nextRandom
            fidVapour <- nextRandom
            let fRoot = mkFlow fidRoot "Water/m3" Water (Just "unspecified")
                fDep = mkFlow fidDep "Water" Water (Just "unspecified")
                fVapour = mkFlow fidVapour "Water" Air (Just "unspecified")
                byName = M.singleton "water" [fDep, fVapour, fRoot]
                located = (mkCFComp "Water" "water" "unspecified" (-12.1)){mcfConsumerLocation = Just "FR"}
                global = mkCFComp "Water" "water" "unspecified" (-42.955)
                flowsOf = map (fmap (bfId . fst) . snd)
            flowsOf (spreadLocatedRows byName [(located, Just (fDep, ByName))])
                `shouldMatchList` [Just fidDep, Just fidRoot]
            flowsOf (spreadLocatedRows byName [(global, Just (fDep, ByName))]) `shouldBe` [Just fidDep]

    describe "findFlowByCAS" $ do
        it "finds flow by CAS number" $ do
            fid <- nextRandom
            let flow = mkFlow fid "Carbon dioxide" Air Nothing
                byCAS = M.singleton "124-38-9" [flow]
            fmap bfId (findFlowByCAS mempty byCAS "co2" "124-38-9" Nothing) `shouldBe` Just fid

        it "returns Nothing for unknown CAS" $
            fmap bfId (findFlowByCAS mempty M.empty "co2" "000-00-0" Nothing) `shouldBe` Nothing

        -- The index is keyed canonically; a method whose parser kept the
        -- source's zero-padding still has to reach the same flow, or its
        -- factor goes silently missing.
        it "meets a canonically indexed flow from a zero-padded query" $ do
            fid <- nextRandom
            let flow = mkFlow fid "Carbon dioxide" Air Nothing
                byCAS = M.singleton "124-38-9" [flow]
            fmap bfId (findFlowByCAS mempty byCAS "co2" "000124-38-9" Nothing) `shouldBe` Just fid

        -- An all-zeros placeholder is not a substance anchor: indexing it
        -- would collide every CAS-less flow onto one key.
        it "refuses an all-zeros placeholder rather than treating it as a CAS" $ do
            fid <- nextRandom
            let flow = mkFlow fid "Unknown" Air Nothing
                byCAS = M.singleton "0-00-0" [flow]
            fmap bfId (findFlowByCAS mempty byCAS "co2" "000-00-0" Nothing) `shouldBe` Nothing

    describe "findFlowByName" $ do
        it "finds a flow by name (case-insensitive via normalization)" $ do
            fid <- nextRandom
            let flow = mkFlow fid "Carbon dioxide" Air Nothing
                byName = M.singleton "carbon dioxide" [flow]
            fmap bfId (findFlowByName mempty byName "Carbon dioxide") `shouldBe` Just fid

        it "returns Nothing for unknown name" $
            fmap bfId (findFlowByName mempty M.empty "co2") `shouldBe` Nothing

    describe "findFlowBySynonym" $ do
        it "returns Nothing when synonym not in DB" $ do
            fid <- nextRandom
            let flow = mkFlow fid "Carbon dioxide" Air Nothing
                byName = M.singleton "carbon dioxide" [flow]
            fmap bfId (findFlowBySynonym (SynonymSearch emptySynonymDB byName mempty) "CO2") `shouldBe` Nothing

    describe "findFlowBySynonymComp" $ do
        it "finds flow via synonym with compartment preference" $ do
            fid1 <- nextRandom
            fid2 <- nextRandom
            let synDB = buildFromPairs [("CO2", "Carbon dioxide")]
                fAir = mkFlow fid1 "Carbon dioxide" Air Nothing
                fWater = mkFlow fid2 "Carbon dioxide" Water Nothing
                byName = M.singleton "carbon dioxide" [fWater, fAir]
                comp = Compartment "air" "" ""
            fmap bfId (findFlowBySynonymComp (SynonymSearch synDB byName mempty) "CO2" (Just comp))
                `shouldBe` Just fid1

        it "returns Nothing when synonym not in DB" $ do
            fid <- nextRandom
            let synDB = buildFromPairs [("CO2", "Carbon dioxide")]
                flow = mkFlow fid "Carbon dioxide" Air Nothing
                byName = M.singleton "carbon dioxide" [flow]
            fmap bfId (findFlowBySynonymComp (SynonymSearch synDB byName mempty) "methane" Nothing)
                `shouldBe` Nothing

        it "returns Nothing when no flows match any synonym" $ do
            let synDB = buildFromPairs [("CO2", "Carbon dioxide")]
            fmap bfId (findFlowBySynonymComp (SynonymSearch synDB M.empty mempty) "CO2" Nothing)
                `shouldBe` Nothing

    describe "expandSynonymMappings direction" $ do
        -- The water withdrawal bridge "freshwater" → resource flow applies to an
        -- INPUT (withdrawal) CF only. An OUTPUT (release) CF named "freshwater"
        -- must not fan out onto the resource flow through it – else a release
        -- inherits a withdrawal scarcity factor (wrong sign/magnitude).
        let synDB = buildFromEdges [SynEdge "freshwater" "Water, unspecified natural origin" BridgeInput]
            resourceFlow fid = mkFlow fid "Water, unspecified natural origin" NaturalResource Nothing
            flowsByName fid = M.singleton "water unspecified natural origin" [resourceFlow fid]
            inputCF = (mkCF "freshwater" Nothing 1.0){mcfDirection = Input}
            outputCF = (mkCF "freshwater" Nothing 1.0){mcfDirection = Output}
            fannedIds cf fid =
                [bfId flow | (_, Just (flow, _)) <- drop 1 (expandSynonymMappings synDB (flowsByName fid) [(cf, Nothing)])]

        it "fans an INPUT CF out onto the withdrawal resource flow" $ do
            fid <- nextRandom
            fannedIds inputCF fid `shouldBe` [fid]

        it "does NOT fan an OUTPUT CF through the input-only bridge" $ do
            fid <- nextRandom
            fannedIds outputCF fid `shouldBe` []

    describe "expandSynonymMappings transitivity" $ do
        -- A curated chain routinely pivots through an alias that names no loaded
        -- flow: "Energy, from coal" = "hard coal" = "Coal, hard", where only the
        -- endpoints are flow or CF names. The fan-out must follow the closure
        -- through that pivot – requiring every intermediate to be a flow or CF
        -- name silently cut the whole coal family out of energy accounting.
        let synDB =
                buildFromEdges
                    [ SynEdge "Energy, from coal" "hard coal" BridgeBoth
                    , SynEdge "hard coal" "Coal, hard" BridgeBoth
                    ]
            energyCF = (mkCF "Energy, from coal" Nothing 1.0){mcfDirection = Input}

        it "fans out through a pivot alias that is neither a flow nor a CF name" $ do
            fid <- nextRandom
            let coalFlow = mkFlow fid "Coal, hard" NaturalResource Nothing
                flowsByName = M.singleton "coal hard" [coalFlow]
            [ bfId flow
              | (_, Just (flow, BySynonym)) <-
                    drop 1 (expandSynonymMappings synDB flowsByName [(energyCF, Nothing)])
              ]
                `shouldBe` [fid]

    describe "directionExcludedCFs" $ do
        -- An unmapped CF whose name matches through the UNION synonym tables but
        -- not through its own direction's view was excluded by the direction
        -- restriction alone – e.g. a parser defaulted the direction when the
        -- method carried none. The loader surfaces these so the loss is
        -- distinguishable from a genuinely uncharacterized flow.
        let synDB = buildFromEdges [SynEdge "freshwater" "Water, unspecified natural origin" BridgeInput]
            flowsByName fid = M.singleton "water unspecified natural origin" [mkFlow fid "Water, unspecified natural origin" NaturalResource Nothing]
            inputCF = (mkCF "freshwater" Nothing 1.0){mcfDirection = Input}
            outputCF = (mkCF "freshwater" Nothing 1.0){mcfDirection = Output}

        it "flags an unmapped CF whose synonym match exists only outside its direction view" $ do
            fid <- nextRandom
            map mcfFlowName (directionExcludedCFs mempty synDB (flowsByName fid) [(outputCF, Nothing)])
                `shouldBe` ["freshwater"]

        it "does not flag a CF its own direction view still matches, nor a genuinely unmatched one" $ do
            fid <- nextRandom
            directionExcludedCFs mempty synDB (flowsByName fid) [(inputCF, Nothing)] `shouldSatisfy` null
            directionExcludedCFs mempty synDB (flowsByName fid) [(mkCF "unrelated" Nothing 1.0, Nothing)] `shouldSatisfy` null

    describe "computeMappingStats" $ do
        it "counts totals and strategies correctly" $ do
            fid1 <- nextRandom
            fid2 <- nextRandom
            fid3 <- nextRandom
            let f1 = mkFlow fid1 "co2" Air Nothing
                f2 = mkFlow fid2 "methane" Air Nothing
                f3 = mkFlow fid3 "n2o" Air Nothing
                cf1 = mkCF "co2" Nothing 1.0
                cf2 = mkCF "methane" Nothing 25.0
                cf3 = mkCF "n2o" Nothing 298.0
                cf4 = mkCF "hfc" Nothing 1300.0
                mappings =
                    [ (cf1, Just (f1, ByUUID))
                    , (cf2, Just (f2, ByName))
                    , (cf3, Just (f3, ByCAS))
                    , (cf4, Nothing)
                    ]
                stats = computeMappingStats mappings
            msTotal stats `shouldBe` 4
            msByUUID stats `shouldBe` 1
            msByName stats `shouldBe` 1
            msByCAS stats `shouldBe` 1
            msBySynonym stats `shouldBe` 0
            msUnmatched stats `shouldBe` 1

        it "handles all-unmatched" $ do
            let cf = mkCF "xyz" Nothing 1.0
                stats = computeMappingStats [(cf, Nothing)]
            msUnmatched stats `shouldBe` 1
            msByUUID stats `shouldBe` 0

    describe "computeLCIAScore" $ do
        it "sums UUID-matched flows" $ do
            fid <- nextRandom
            let flow = mkFlow fid "co2" Air Nothing
                unit = Unit{unitId = nil, unitName = "kg", unitSymbol = "kg", unitComment = ""}
                cf = mkCF "co2" Nothing 1.0
                mapping = [(cf, Just (flow, ByUUID))]
                inventory = M.singleton fid 100.0
                flowDB = M.singleton fid flow
                unitDB = M.singleton nil unit
                score = loScore (computeLCIAScore defaultUnitConfig unitDB flowDB inventory mapping)
            score `shouldBe` 100.0

        it "returns 0 when inventory is empty" $ do
            let cf = mkCF "co2" Nothing 1.0
                score = loScore (computeLCIAScore defaultUnitConfig M.empty M.empty M.empty [(cf, Nothing)])
            score `shouldBe` 0.0

        it "skips zero-quantity flows" $ do
            fid <- nextRandom
            let flow = mkFlow fid "co2" Air Nothing
                cf = mkCF "co2" Nothing 1.0
                mapping = [(cf, Just (flow, ByUUID))]
                inventory = M.singleton fid 0.0
                score = loScore (computeLCIAScore defaultUnitConfig M.empty (M.singleton fid flow) inventory mapping)
            score `shouldBe` 0.0

        it "scores via fallback CF (name+medium, empty subcomp)" $ do
            fid <- nextRandom
            let flow = mkFlow fid "Carbon dioxide" Air Nothing
                cf = mkCFComp "Carbon dioxide" "air" "" 2.5
                mapping = [(cf, Nothing)] -- unmatched → name-based lookup
                inventory = M.singleton fid 10.0
                flowDB = M.singleton fid flow
                score = loScore (computeLCIAScore defaultUnitConfig M.empty flowDB inventory mapping)
            score `shouldBe` 25.0

        it "scores via exact CF (name+medium+subcomp)" $ do
            fid <- nextRandom
            let flow = mkFlow fid "Carbon dioxide" Air (Just "urban air close to ground")
                cf = mkCFComp "Carbon dioxide" "air" "urban air close to ground" 3.0
                mapping = [(cf, Nothing)]
                inventory = M.singleton fid 5.0
                flowDB = M.singleton fid flow
                score = loScore (computeLCIAScore defaultUnitConfig M.empty flowDB inventory mapping)
            score `shouldBe` 15.0

        it "normalizes 'natural resource' category to 'resource'" $ do
            fid <- nextRandom
            let flow = mkFlow fid "crude oil" NaturalResource Nothing
                cf = mkCFComp "crude oil" "natural resource" "" 1.5
                mapping = [(cf, Nothing)]
                inventory = M.singleton fid 4.0
                flowDB = M.singleton fid flow
                score = loScore (computeLCIAScore defaultUnitConfig M.empty flowDB inventory mapping)
            score `shouldBe` 6.0

        it "returns 0 for flow not in flowDB" $ do
            fid <- nextRandom
            let cf = mkCF "co2" Nothing 1.0
                mapping = [(cf, Nothing)]
                inventory = M.singleton fid 10.0
                score = loScore (computeLCIAScore defaultUnitConfig M.empty M.empty inventory mapping)
            score `shouldBe` 0.0

    describe "buildMethodTables compartment normalization" $ do
        -- Two sides, one vocabulary. A factor table and an inventory each state
        -- a medium in the words their own format uses, and the tables key on
        -- the medium those words name. What the reader can place needs nothing
        -- declared; what it cannot is what a compartment rule is for.
        it "meets a factor whose medium states its direction, with nothing declared" $ do
            fid <- nextRandom
            let flow = mkFlow fid "ammonia" Air (Just "low. pop.")
                cf = mkCFComp "ammonia" "emissions to air" "low. pop." 0.747
                tables = buildMethodTables mempty mempty M.empty [(cf, Nothing)]
                inventory = M.singleton fid 10.0
                flowDB = M.singleton fid flow
                score = loScore (computeLCIAScoreFromTables defaultUnitConfig M.empty flowDB inventory tables)
            score `shouldBe` 7.47

        it "scores zero when the factor's medium is one no reader can place" $ do
            fid <- nextRandom
            let flow = mkFlow fid "ammonia" Air (Just "low. pop.")
                cf = mkCFComp "ammonia" "urban air" "low. pop." 0.747
                tables = buildMethodTables mempty mempty M.empty [(cf, Nothing)]
                inventory = M.singleton fid 10.0
                flowDB = M.singleton fid flow
                score = loScore (computeLCIAScoreFromTables defaultUnitConfig M.empty flowDB inventory tables)
            score `shouldBe` 0.0

        it "bridges 'urban air' → 'air' via a medium-only rule" $ do
            fid <- nextRandom
            let flow = mkFlow fid "ammonia" Air (Just "low. pop.")
                cf = mkCFComp "ammonia" "urban air" "low. pop." 0.747
                cmap = mempty{cmSpellings = M.singleton ("urban air", "", "") (Compartment "air" "" "")}
                tables = buildMethodTables cmap mempty M.empty [(cf, Nothing)]
                inventory = M.singleton fid 10.0
                flowDB = M.singleton fid flow
                score = loScore (computeLCIAScoreFromTables defaultUnitConfig M.empty flowDB inventory tables)
            score `shouldBe` 7.47

        it "bridges full (medium, sub, qual) triples for subcompartment rewrites" $ do
            fid <- nextRandom
            let flow = mkFlow fid "ammonia" Air (Just "non-urban air or from high stacks")
                -- A CF categorized the way BAFU categorizes it, on a different
                -- subcompartment than the flow.
                cf = mkCFComp "ammonia" "emissions to air" "low. pop." 0.747
                cmap =
                    mempty
                        { cmSpellings =
                            M.singleton
                                ("emissions to air", "low. pop.", "")
                                (Compartment "air" "non-urban air or from high stacks" "")
                        }
                tables = buildMethodTables cmap mempty M.empty [(cf, Nothing)]
                inventory = M.singleton fid 10.0
                flowDB = M.singleton fid flow
                score = loScore (computeLCIAScoreFromTables defaultUnitConfig M.empty flowDB inventory tables)
            score `shouldBe` 7.47

        -- Regression: a failed unit conversion used to fall back to the
        -- unconverted quantity, contaminating the score with wrong-unit data.
        -- Now an unconvertible flow contributes 0, matching the "no-CF" branch.
        it "returns 0 when unit conversion fails (dimension mismatch)" $ do
            fid <- nextRandom
            let flow = mkFlow fid "co2" Air Nothing
                cf = mkCF "co2" Nothing 1.0 -- mcfUnit = "kg"
                mapping = [(cf, Just (flow, ByUUID))]
                inventory = M.singleton fid 100.0
                flowDB = M.singleton fid flow
                unitDB = M.singleton nil (unitNamed "m") -- length, not mass
                score = loScore (computeLCIAScore defaultUnitConfig unitDB flowDB inventory mapping)
            score `shouldBe` 0.0

        it "applies conversion factor when units differ but are compatible (g→kg)" $ do
            fid <- nextRandom
            let flow = mkFlow fid "co2" Air Nothing
                cf = mkCF "co2" Nothing 2.0 -- mcfUnit = "kg"
                mapping = [(cf, Just (flow, ByUUID))]
                inventory = M.singleton fid 1000.0 -- 1000 g
                flowDB = M.singleton fid flow
                unitDB = M.singleton nil (unitNamed "g")
                score = loScore (computeLCIAScore gKgUnitConfig unitDB flowDB inventory mapping)
            -- 1000 g → 1.0 kg, * cf 2.0 = 2.0
            score `shouldBe` 2.0

    describe "buildMethodTables unit-suffixed homonym collision" $ do
        -- SimaPro-style methods carry one CF row per denominator unit for the
        -- same substance ("Gas, natural/kg" = 43.1 MJ/kg and "Gas, natural/m3"
        -- = 34.5 MJ/m3). normalizeName strips the suffix, so both rows collide
        -- on one name key. The row whose raw name equals the flow's raw name
        -- must win: the other variant is dimensionally incompatible with the
        -- flow and would silently convert to 0.
        let kgDef = unitDef "mass" 1.0
            m3Def = unitDef "volume" 1.0
            cfg =
                mkUnitConfig
                    []
                    (M.fromList [("kg", kgDef), ("m3", m3Def)])
            cfPerKg = (mkCFComp "Gas, natural/kg" "natural resource" "" 43.1){mcfUnit = "kg"}
            cfPerM3 = (mkCFComp "Gas, natural/m3" "natural resource" "" 34.5){mcfUnit = "m3"}

        it "the verbatim-named CF wins over the higher-valued homonym" $ do
            fid <- nextRandom
            uidM3 <- nextRandom
            let flow = (mkFlow fid "Gas, natural/m3" NaturalResource (Just "in ground")){bfUnitId = uidM3}
                mappings = [(cfPerKg, Just (flow, ByName)), (cfPerM3, Just (flow, ByName))]
                unitDB = M.singleton uidM3 Unit{unitId = uidM3, unitName = "m3", unitSymbol = "m3", unitComment = ""}
                flowDB = M.singleton fid flow
                score ms = loScore (computeLCIAScoreFromTables cfg unitDB flowDB (M.singleton fid 2.0) (buildMethodTables mempty mempty M.empty ms))
            score mappings `shouldBe` 2.0 * 34.5
            -- insertion order must not matter
            score (reverse mappings) `shouldBe` 2.0 * 34.5

    describe "a synonym two substances share" $ do
        -- EcoSpold 2 gives trivalent and hexavalent chromium the same synonyms,
        -- Chromium and Chromium ion: the names a method gives chromium of
        -- unstated valence, characterized as hexavalent. Such a synonym names
        -- neither flow. The rows below follow a method that writes trivalent
        -- chromium at the unspecified compartment only, since its specific
        -- compartments carry the same zero, and writes the unstated one at a
        -- specific compartment too: a row landing on trivalent chromium there
        -- is the only one at that compartment, so no tie-break can undo it.
        -- The flow registry is left empty: only the name index decides here.
        let chromium i name comp =
                (mkFlow (fromWords i 0 0 0) name Air comp)
                    { bfSynonyms = M.singleton "en" (S.fromList ["Chromium", "Chromium ion"])
                    }
            trivalent = chromium 1 "Chromium III" (Just "high. pop.")
            hexavalent = chromium 2 "Chromium VI" Nothing
            flows = M.fromList [(bfId f, f) | f <- [trivalent, hexavalent]]
            context =
                MapContext
                    { mcBioFlowsByUUID = flows
                    , mcBioFlowsByName = VT.buildFlowNameIndex flows
                    , mcBioFlowsByCAS = M.empty
                    , mcSynonymDB = emptySynonymDB
                    , mcActivities = M.empty
                    , mcPlacing = mempty
                    , mcSynGroupFlows = M.empty
                    }
            factors rows = do
                mappings <-
                    mapMethodFlows
                        context
                        Method
                            { methodId = nil
                            , methodName = "Human toxicity, cancer - inorganics"
                            , methodDescription = Nothing
                            , methodUnit = "CTUh"
                            , methodCategory = "Human toxicity, cancer - inorganics"
                            , methodMethodology = Nothing
                            , methodFactors = rows
                            }
                let tables = buildMethodTables mempty mempty M.empty mappings
                pure [cfValue <$> lookupCFForFlow tables (bfId f) (Just f) | f <- [trivalent, hexavalent]]
            spelled unstated iii vi =
                [ mkCFComp unstated "air" "" 7.9836e-5
                , mkCFComp unstated "air" "high. pop." 7.9836e-5
                , mkCFComp iii "air" "" 0
                , mkCFComp vi "air" "" 7.9836e-5
                ]

        it "gives each valence its own factor under the reference package spelling" $
            factors (spelled "chromium" "chromium (iii)" "chromium (vi)") `shouldReturn` [Just 0, Just 7.9836e-5]

        it "gives each valence its own factor under the SimaPro spelling" $
            factors (spelled "Chromium, ion" "Chromium (III)" "Chromium (VI)") `shouldReturn` [Just 0, Just 7.9836e-5]

        -- The arsenic a method writes without valence is the only arsenic
        -- EcoSpold 2 emits, Arsenic ion, and it shares the name with the ore.
        it "still reaches a flow through a synonym only it lists" $ do
            let ore = mkFlow (fromWords 3 0 0 0) "Arsenic" NaturalResource (Just "in ground")
                ion = (mkFlow (fromWords 4 0 0 0) "Arsenic ion" Water Nothing){bfSynonyms = M.singleton "en" (S.singleton "Arsenic")}
                index = VT.buildFlowNameIndex (M.fromList [(bfId f, f) | f <- [ore, ion]])
            S.fromList . map bfId <$> M.lookup "arsenic" index `shouldBe` Just (S.fromList [bfId ore, bfId ion])

    describe "a row no database flow claimed at build time" $ do
        -- Such a row is filed under the method's own flow name, and the
        -- table-key comparison ranks it with a proxy match: the least
        -- discriminating rank there is. Two rows meeting on one name key, one
        -- borrowed through a proxy edge and one nothing resolved, therefore
        -- tie on rank, and the entry is chosen on the rungs below: the
        -- raw-name rank, which neither row here passes, then the factor.
        -- The tie is stated in one rank table and read in one comparison, and
        -- neither says it out loud, so it is pinned here.
        --
        -- One case would not pin it: whichever factor is the larger, a rank
        -- that moved in one direction still returns that row. So both are
        -- asserted, the higher factor once on each side. Together they fail
        -- whichever way the unresolved row's rank moves.
        let borrowedAt = mkCFComp "phosphorus" "water" ""
            unresolvedAt = mkCFComp "phosphate" "water" ""

            servedFor mappings = do
                targetId <- nextRandom
                probeId <- nextRandom
                let target = mkFlow targetId "phosphate" Water Nothing
                    probe = mkFlow probeId "phosphate" Water Nothing
                    tables = buildMethodTables mempty mempty M.empty (mappings target)
                pure (cfValue <$> lookupCFForFlow tables probeId (Just probe))

        it "loses the key to a larger factor a proxy match carries" $ do
            served <-
                servedFor $ \target ->
                    [ (borrowedAt 20.0, Just (target, ByProxy))
                    , (unresolvedAt 5.0, Nothing)
                    ]
            served `shouldBe` Just 20.0

        it "takes the key from a smaller factor a proxy match carries" $ do
            served <-
                servedFor $ \target ->
                    [ (borrowedAt 5.0, Just (target, ByProxy))
                    , (unresolvedAt 20.0, Nothing)
                    ]
            served `shouldBe` Just 20.0

        it "borrows the proxy row's own name, which is what makes the two meet" $ do
            -- Without this the two tests above prove nothing: the rows have
            -- different names, and they compete only because a proxy match
            -- files its row under the flow it borrowed from.
            served <- servedFor $ \target -> [(borrowedAt 5.0, Just (target, ByProxy))]
            served `shouldBe` Just 5.0

    describe "the sea is a subcompartment like any other" $ do
        -- A substance's sea line scores a release to the sea; a substance with
        -- no sea line reads its line for the whole medium there, as at any
        -- subcompartment it writes nothing for. Whether the method writes sea
        -- lines for other substances changes nothing.
        let uns = mkCFComp "Nitrogen, total" "water" "" 1.0
            sea = mkCFComp "Nitrogen, total" "water" "ocean" 0.0
            scoreWith cfs sub = do
                fid <- nextRandom
                let flow = mkFlow fid "Nitrogen, total" Water (Just sub)
                    tables =
                        buildMethodTables
                            mempty
                            mempty
                            M.empty
                            [(cf, Just (flow, ByName)) | cf <- cfs]
                    flowDB = M.singleton fid flow
                pure
                    ( loScore
                        (computeLCIAScoreFromTables defaultUnitConfig M.empty flowDB (M.singleton fid 1.0) tables)
                    )

        it "scores a release to the sea with the substance's sea line" $
            scoreWith [uns, sea] "ocean" `shouldReturn` 0.0

        it "reads the line for the whole medium at the sea when the substance has no sea line" $ do
            scoreWith [uns] "ocean" `shouldReturn` 1.0

        it "leaves every other subcompartment alone, either way" $ do
            scoreWith [uns, sea] "river" `shouldReturn` 1.0
            scoreWith [uns] "river" `shouldReturn` 1.0
            scoreWith [uns, sea] "(unspecified)" `shouldReturn` 1.0
            scoreWith [uns] "(unspecified)" `shouldReturn` 1.0

        it "prefers the sea line itself over the medium-level one" $
            scoreWith [uns, mkCFComp "Nitrogen, total" "water" "ocean" 5.0] "ocean" `shouldReturn` 5.0

        -- The key is lowercased and stripped, so two rows that differ only in
        -- case are one rule with two targets, and the file says nothing about
        -- which was meant.
        it "refuses two rows normalizing the same source compartment" $
            buildCompartmentMapFromCSV "source_medium,source_sub,source_qualifier,target_medium,target_sub,target_qualifier\nwater,sea water,,water,ocean,\nWater,Sea water,,water,river,\n"
                `shouldSatisfy` isLeft

        describe "the kind column" $ do
            let header = "source_medium,source_sub,source_qualifier,target_medium,target_sub,target_qualifier,kind\n"
            it "reads a row without a kind, or of kind same, as a spelling" $
                fmap cmSpellings (buildCompartmentMapFromCSV (header <> "water,sea water,,water,ocean,\nwater,sea,,water,ocean,,same\n"))
                    `shouldBe` Right
                        ( M.fromList
                            [ (("water", "sea water", ""), Compartment "water" "ocean" "")
                            , (("water", "sea", ""), Compartment "water" "ocean" "")
                            ]
                        )
            it "reads an if_absent row as a redirection within one medium, and no spelling" $
                buildCompartmentMapFromCSV (header <> "soil,Forestry,,soil,non-agricultural,,if_absent\n")
                    `shouldBe` Right mempty{cmIfAbsent = M.singleton (Soil, Subcompartment "forestry") (Subcompartment "non-agricultural")}
            it "refuses an if_absent row that leaves its medium, lacks a side, or names one place twice" $ do
                buildCompartmentMapFromCSV (header <> "soil,forestry,,water,surface water,,if_absent\n") `shouldSatisfy` isLeft
                buildCompartmentMapFromCSV (header <> "soil,,,soil,non-agricultural,,if_absent\n") `shouldSatisfy` isLeft
                buildCompartmentMapFromCSV (header <> "soil,forestry,,soil,forestry,,if_absent\n") `shouldSatisfy` isLeft
                buildCompartmentMapFromCSV (header <> "ground,forestry,,ground,industrial,,if_absent\n") `shouldSatisfy` isLeft
            it "refuses a kind it does not know" $
                buildCompartmentMapFromCSV (header <> "water,sea,,water,ocean,,proxy\n") `shouldSatisfy` isLeft

        it "recognizes the sea through the spelling compartments.csv translates" $ do
            cmap <-
                either
                    (fail . ("compartments.csv: " <>))
                    pure
                    (buildCompartmentMapFromCSV "source_medium,source_sub,source_qualifier,target_medium,target_sub,target_qualifier\nwater,sea water,,water,ocean,\n")
            fid <- nextRandom
            let flow = mkFlow fid "Nitrogen, total" Water (Just "sea water")
                score cfs =
                    loScore $
                        computeLCIAScoreFromTables
                            defaultUnitConfig
                            M.empty
                            (M.singleton fid flow)
                            (M.singleton fid 1.0)
                            (buildMethodTables cmap mempty M.empty [(cf, Just (flow, ByName)) | cf <- cfs])
            score [uns, mkCFComp "Nitrogen, total" "water" "sea water" 0.0] `shouldBe` 0.0
            score [uns] `shouldBe` 1.0

    describe "groundwater, long-term or not, under the three rules (read path)" $ do
        -- The method writes the substance's line for the whole medium and an
        -- explicit zero for long-term groundwater, as the SimaPro export of EF
        -- does, both carrying the CAS. Only the second names long-term
        -- groundwater, so every other subcompartment reads the first.
        let cfUns = (mkCFComp "Iron, ion" "water" "" 2108.5){mcfCAS = Just "7439-89-6"}
            cfLt = (mkCFComp "Iron, ion" "water" "groundwater, long-term" 0.0){mcfCAS = Just "7439-89-6"}
            scoreFor name mCas sub = do
                fid <- nextRandom
                mid <- nextRandom
                let flow = (mkFlow fid name Water (Just sub)){bfCAS = mCas}
                    -- cfUns matched ByCAS on a sibling flow, so the CAS bridge
                    -- serves it; cfLt matched nothing, and only takes its place.
                    matched = (mkFlow mid "Iron, ion" Water Nothing){bfCAS = Just "7439-89-6"}
                    tables = buildMethodTables mempty mempty M.empty [(cfUns, Just (matched, ByCAS)), (cfLt, Nothing)]
                    flowDB = M.singleton fid flow
                pure (loScore (computeLCIAScoreFromTables defaultUnitConfig M.empty flowDB (M.singleton fid 1.0) tables))

        it "reads the line for the whole medium at river, lake and immediate groundwater" $ do
            scoreFor "Iron, ion" Nothing "river" `shouldReturn` 2108.5
            scoreFor "Iron, ion" Nothing "lake" `shouldReturn` 2108.5
            scoreFor "Iron, ion" Nothing "groundwater" `shouldReturn` 2108.5

        it "keeps the method's explicit long-term zero" $
            scoreFor "Iron, ion" Nothing "groundwater, long-term" `shouldReturn` 0.0

        it "does not let the CAS bridge read over a place the substance writes" $
            -- A flow of another name sharing the CAS: the substance writes
            -- long-term groundwater on a line the bridge may not serve, and the
            -- bridge stops there rather than lending the immediate 2108.5.
            scoreFor "Iron(2+)" (Just "7439-89-6") "groundwater, long-term" `shouldReturn` 0.0

        it "lets the CAS bridge read the line for the whole medium where the substance writes nothing" $
            scoreFor "Iron(2+)" (Just "7439-89-6") "groundwater" `shouldReturn` 2108.5

    describe "ILCD soil and fresh-water subcompartments (compartments.csv)" $ do
        -- The ILCD package files its factors under "Emissions to agricultural
        -- soil", "Emissions to non-agricultural soil" and "Emissions to fresh
        -- water"; SimaPro writes agricultural, industrial and river, EcoSpold 2
        -- agricultural, forestry, industrial, surface water and ground-. The
        -- published EcoSpold 2 mapping of EF 3.1 sends forestry and industrial
        -- soil to non-agricultural, and ground- to fresh water; the if_absent
        -- rows of compartments.csv write that down, for a method that has no
        -- such subcompartment of its own. The factors take their compartment
        -- through 'parseCompartment', as an ILCD package loads them.
        cmap <- runIO $ do
            csv <- BL.readFile "data/compartments.csv"
            either (fail . ("compartments.csv: " <>)) pure (buildCompartmentMapFromCSV csv)
        let cfUnder medium sub val = (mkCF "Silver (I)" Nothing val){mcfCompartment = parseCompartment ["Emissions", medium, sub]}
            soilCFs =
                [ cfUnder "Emissions to soil" "Emissions to soil, unspecified" 1.0
                , cfUnder "Emissions to soil" "Emissions to agricultural soil" 2.0
                , cfUnder "Emissions to soil" "Emissions to non-agricultural soil" 3.0
                ]
            waterCFs =
                [ cfUnder "Emissions to water" "Emissions to water, unspecified" 1.0
                , cfUnder "Emissions to water" "Emissions to fresh water" 2.0
                ]
            scoreAt cfs medium sub = do
                fid <- nextRandom
                let flow = mkFlow fid "Silver (I)" medium (Just sub)
                    tables = buildMethodTables cmap (methodVocabulary cmap cfs) M.empty [(cf, Just (flow, ByName)) | cf <- cfs]
                pure (loScore (computeLCIAScoreFromTables defaultUnitConfig M.empty (M.singleton fid flow) (M.singleton fid 1.0) tables))

        it "gives agricultural soil its own factor, and forestry and industrial soil the non-agricultural one" $ do
            scoreAt soilCFs Soil "agricultural" `shouldReturn` 2.0
            scoreAt soilCFs Soil "forestry" `shouldReturn` 3.0
            scoreAt soilCFs Soil "industrial" `shouldReturn` 3.0

        it "keeps its own forestry and industrial factors for a method that writes them" $ do
            let own = [mkCFComp "Silver (I)" "soil" "forestry" 4.0, mkCFComp "Silver (I)" "soil" "industrial" 5.0]
            scoreAt (soilCFs ++ own) Soil "forestry" `shouldReturn` 4.0
            scoreAt (soilCFs ++ own) Soil "industrial" `shouldReturn` 5.0

        it "does not count an exclusion line as a place the method writes" $ do
            -- The tables never hold an exclusion line (they are built after
            -- 'dropExcludedMappings'); only the collection's vocabulary sees it.
            fid <- nextRandom
            let flow = mkFlow fid "Silver (I)" Soil (Just "forestry")
                exclusion = mkCFComp "!Silver*" "soil" "forestry" 0.0
                tables = buildMethodTables cmap (methodVocabulary cmap (exclusion : soilCFs)) M.empty [(cf, Just (flow, ByName)) | cf <- soilCFs]
            loScore (computeLCIAScoreFromTables defaultUnitConfig M.empty (M.singleton fid flow) (M.singleton fid 1.0) tables)
                `shouldBe` 3.0

        it "reads an ILCD resource category as the place it names" $ do
            let fromWater = (mkCF "Silver (I)" Nothing 4.0){mcfCompartment = parseCompartment ["Resources", "Resources from water", "Non-renewable element resources from water"]}
                fromGround = (mkCF "Silver (I)" Nothing 5.0){mcfCompartment = parseCompartment ["Resources", "Resources from ground", "Non-renewable material resources from ground"]}
            scoreAt [fromWater, fromGround] NaturalResource "in water" `shouldReturn` 4.0
            scoreAt [fromWater, fromGround] NaturalResource "in ground" `shouldReturn` 5.0

        it "gives a SimaPro lake flow the fresh-water factor of a method with no lake" $
            scoreAt waterCFs Water "lake" `shouldReturn` 2.0

        it "gives an EcoSpold 2 groundwater flow the fresh-water factor" $
            scoreAt waterCFs Water "ground-" `shouldReturn` 2.0

        it "keeps groundwater and surface water apart for a method that writes both" $ do
            -- A method from the EcoSpold 2 matrix writes ground- and surface
            -- water separately, and one spelling row once merged the two.
            let matrix = [mkCFComp "1,1,1-Trichloroethane" "water" "ground-" 0.0, mkCFComp "1,1,1-Trichloroethane" "water" "surface water" 1700.0]
                scoreOf sub = do
                    fid <- nextRandom
                    let flow = mkFlow fid "1,1,1-Trichloroethane" Water (Just sub)
                        tables = buildMethodTables cmap (methodVocabulary cmap matrix) M.empty [(cf, Just (flow, ByName)) | cf <- matrix]
                    pure (loScore (computeLCIAScoreFromTables defaultUnitConfig M.empty (M.singleton fid flow) (M.singleton fid 1.0) tables))
            scoreOf "ground-" `shouldReturn` 0.0
            scoreOf "surface water" `shouldReturn` 1700.0

        it "lets a SimaPro method's unspecified line cover groundwater, before any if_absent row" $
            scoreAt
                [ mkCFComp "Silver (I)" "water" "" 1.0
                , mkCFComp "Silver (I)" "water" "river" 2.0
                ]
                Water
                "groundwater"
                `shouldReturn` 1.0

        it "reads the same soils from a two-level category tree" $ do
            -- With no third level, the ILCD label itself is the medium.
            let twoLevel label val = (mkCF "Silver (I)" Nothing val){mcfCompartment = parseCompartment ["Emissions", label]}
                cfs =
                    [ cfUnder "Emissions to soil" "Emissions to soil, unspecified" 1.0
                    , twoLevel "Emissions to agricultural soil" 2.0
                    , twoLevel "Emissions to non-agricultural soil" 3.0
                    ]
            scoreAt cfs Soil "agricultural" `shouldReturn` 2.0
            scoreAt cfs Soil "industrial" `shouldReturn` 3.0

        it "gives river and surface water the fresh-water factor" $ do
            scoreAt waterCFs Water "river" `shouldReturn` 2.0
            scoreAt waterCFs Water "surface water" `shouldReturn` 2.0

        it "gives surface water the river factor of a method written in SimaPro terms" $
            scoreAt
                [ mkCFComp "Silver (I)" "water" "" 1.0
                , mkCFComp "Silver (I)" "water" "river" 2.0
                ]
                Water
                "surface water"
                `shouldReturn` 2.0

    describe "one spelling for high altitude and for long-term non-urban air (compartments.csv)" $ do
        -- EcoSpold 2 writes "lower stratosphere + upper troposphere" and
        -- "low population density, long-term"; the ILCD package writes the
        -- first with "and", SimaPro writes "stratosphere + troposphere" and
        -- "low. pop., long-term". Each spelling must reach the factor the
        -- method writes for that subcompartment, not its unspecified one.
        cmap <- runIO $ do
            csv <- BL.readFile "data/compartments.csv"
            either (fail . ("compartments.csv: " <>)) pure (buildCompartmentMapFromCSV csv)
        let scoreAt cfs sub = do
                fid <- nextRandom
                let flow = mkFlow fid "Nitrogen oxides" Air (Just sub)
                    tables = buildMethodTables cmap mempty M.empty [(cf, Just (flow, ByName)) | cf <- cfs]
                pure (loScore (computeLCIAScoreFromTables defaultUnitConfig M.empty (M.singleton fid flow) (M.singleton fid 1.0) tables))
            ilcd sub val = (mkCF "Nitrogen oxides" Nothing val){mcfCompartment = parseCompartment ["Emissions", "Emissions to air", sub]}
            highAltitudeILCD =
                [ ilcd "Emissions to air, unspecified" 1.0
                , ilcd "Emissions to lower stratosphere and upper troposphere" 2.0
                ]

        it "gives an EcoSpold 2 and a SimaPro high-altitude flow the ILCD high-altitude factor" $ do
            scoreAt highAltitudeILCD "lower stratosphere + upper troposphere" `shouldReturn` 2.0
            scoreAt highAltitudeILCD "stratosphere + troposphere" `shouldReturn` 2.0

        it "gives a long-term non-urban flow the ILCD long-term default, whatever its spelling" $ do
            let longTermILCD = [ilcd "Emissions to air, unspecified" 1.0, ilcd "Emissions to air, unspecified (long-term)" 0.5]
                scoreLT sub = do
                    fid <- nextRandom
                    let flow = mkFlow fid "Nitrogen oxides" Air (Just sub)
                        tables = buildMethodTables cmap (methodVocabulary cmap longTermILCD) M.empty [(cf, Just (flow, ByName)) | cf <- longTermILCD]
                    pure (loScore (computeLCIAScoreFromTables defaultUnitConfig M.empty (M.singleton fid flow) (M.singleton fid 1.0) tables))
            scoreLT "low population density, long-term" `shouldReturn` 0.5
            scoreLT "low. pop., long-term" `shouldReturn` 0.5
            -- The package has no non-urban line here, and nothing sends a
            -- non-urban flow to the unspecified one.
            scoreLT "non-urban air or from high stacks" `shouldReturn` 0.0

        it "gives a SimaPro long-term flow the factor written for the EcoSpold 2 spelling" $
            scoreAt
                [ mkCFComp "Nitrogen oxides" "air" "unspecified" 1.0
                , mkCFComp "Nitrogen oxides" "air" "low population density, long-term" 3.0
                ]
                "low. pop., long-term"
                `shouldReturn` 3.0

    describe "final waste flows and inventory indicators meet the same factor" $ do
        -- A method writing an ecofactor for landfilled waste puts "Waste" in
        -- its compartment column. One format files that flow under a medium
        -- of the same name; another files it as an inventory indicator whose
        -- subcompartment is "waste". Both must reach the factor, or the same
        -- waste scores differently depending on where the database came from.
        -- The bridge is compartments.csv, so the shipped file is what runs.
        cmap <- runIO $ do
            csv <- BL.readFile "data/compartments.csv"
            either (fail . ("compartments.csv: " <>)) pure (buildCompartmentMapFromCSV csv)
        let cf = mkCFComp "Landfilled waste mass" "waste" "" 24.0
            scoreOf medium msub = do
                fid <- nextRandom
                let flow = mkFlow fid "Landfilled waste mass" medium msub
                    tables = buildMethodTables cmap mempty M.empty [(cf, Nothing)]
                pure . loScore $
                    computeLCIAScoreFromTables
                        defaultUnitConfig
                        M.empty
                        (M.singleton fid flow)
                        (M.singleton fid 1.0)
                        tables

        it "characterizes a flow filed under the medium the method names" $
            scoreOf Waste Nothing `shouldReturn` 24.0

        it "characterizes an inventory indicator whose subcompartment is waste" $
            scoreOf InventoryIndicator (Just "waste") `shouldReturn` 24.0

        it "leaves the indicators that are not waste alone" $ do
            -- The same compartment name also covers secondary materials and
            -- exported energy. A rule keyed on the medium alone would hand
            -- them a waste factor; this one is keyed on the pair.
            scoreOf InventoryIndicator (Just "resource use") `shouldReturn` 0.0
            scoreOf InventoryIndicator (Just "output flow") `shouldReturn` 0.0

    describe "inventoryContributions" $ do
        -- Regression: same fallback bug as computeLCIAScoreFromTables.
        it "yields zero contribution when unit conversion fails" $ do
            fid <- nextRandom
            let flow = mkFlow fid "co2" Air Nothing
                cf = mkCF "co2" Nothing 1.0
                tables = buildMethodTables mempty mempty M.empty [(cf, Just (flow, ByUUID))]
                inventory = M.singleton fid 100.0
                flowDB = M.singleton fid flow
                unitDB = M.singleton nil (unitNamed "m")
                (contribs, unknowns) =
                    inventoryContributions defaultUnitConfig unitDB flowDB inventory tables
            unknowns `shouldBe` []
            map fcContribution contribs `shouldBe` [0.0]

    describe "convertForCharacterization" $ do
        -- Each row encodes a (flowUnit, cfUnit, qty) → expected mapping under a
        -- specific UnitConfig. Semantic groups: pass-through (units match, or no
        -- flow unit), refuse cross-dimension injection (→ 0), apply the factor
        -- when both units are known, and – when the CF unit is a result
        -- expression unknown to the UnitConfig – normalize the flow to its
        -- canonical base unit (a kg flow is unchanged; a g flow scales to kg), or
        -- hard-fail to 0 when that dimension defines no canonical base.
        let cases =
                [ ("units match by name", defaultUnitConfig, "kg", "kg", 5.0, 5.0)
                , ("cfUnit empty (method without unit)", defaultUnitConfig, "kg", "", 7.0, 7.0)
                , ("flowUnit empty (no metadata)", defaultUnitConfig, "", "kg", 9.0, 9.0)
                , ("LCIA-expression CF unit, flow already canonical → unchanged", defaultUnitConfig, "kg", "kg CO2 eq", 3.0, 3.0)
                , ("LCIA-expression CF unit, g flow → normalized to canonical kg", gKgUnitConfig, "g", "kg CO2 eq", 1000.0, 1.0)
                , ("LCIA-expression CF unit, flow dimension has no canonical base → 0", gOnlyUnitConfig, "g", "kg CO2 eq", 1000.0, 0.0)
                , ("dimensionally incompatible → 0", defaultUnitConfig, "m", "kg", 100.0, 0.0)
                , ("compatible units differ → apply factor (1000 g → 1.0 kg)", gKgUnitConfig, "g", "kg", 1000.0, 1.0)
                ]
        mapM_
            ( \(label, cfg, flowU, cfU, qty, expected) ->
                it label $
                    convertForCharacterization cfg flowU (CFUnit cfU) qty `shouldBe` expected
            )
            cases

    describe "computeMappingStats" $ do
        it "counts BySynonym matches" $ do
            fid <- nextRandom
            let flow = mkFlow fid "co2" Air Nothing
                cf = mkCF "co2" Nothing 1.0
                stats = computeMappingStats [(cf, Just (flow, BySynonym))]
            msBySynonym stats `shouldBe` 1

    describe "findFlowBySynonym (finds via synonym)" $ do
        it "finds flow via synonym group (no compartment)" $ do
            fid <- nextRandom
            let synDB = buildFromPairs [("co2", "carbon dioxide")]
                flow = mkFlow fid "carbon dioxide" Air Nothing
                byName = M.singleton "carbon dioxide" [flow]
            fmap bfId (findFlowBySynonym (SynonymSearch synDB byName mempty) "co2")
                `shouldBe` Just fid

    describe "pickByCompartment mempty (matchMedium edge cases)" $ do
        it "null medium matches any flow" $ do
            fid <- nextRandom
            let flow = mkFlow fid "co2" Water Nothing
                byName = M.singleton "co2" [flow]
                comp = Compartment "" "" ""
            fmap bfId (findFlowByNameComp mempty byName "co2" (Just comp)) `shouldBe` Just fid

        it "does not read a medium the stated one is only part of as the stated one (air in urban air)" $ do
            -- A flow filed under the medium "urban air" is not in the row's
            -- medium "air": the tables would index the row under "air" and
            -- never serve that flow. A compartment rule is what says the two
            -- spellings are one medium.
            fid1 <- nextRandom
            fid2 <- nextRandom
            let fUrbanAir = mkFlow fid1 "nox" Air (Just "urban")
                fWater = mkFlow fid2 "nox" Water Nothing
                byName = M.singleton "nox" [fWater, fUrbanAir]
                comp = Compartment "urban air" "" ""
                rule = mempty{cmSpellings = M.singleton ("urban air", "", "") (Compartment "air" "urban" "")}
            fmap bfId (findFlowByNameComp mempty byName "nox" (Just comp)) `shouldBe` Nothing
            fmap bfId (findFlowByNameComp (Placing rule mempty) byName "nox" (Just comp)) `shouldBe` Just fid1

    describe "compartmentGapWarning" $ do
        it "names both vocabularies when a factor's name is filed under another medium" $ do
            fid <- nextRandom
            let flow = mkFlow fid "ammonia" Air Nothing
                cf = mkCFComp "ammonia" "urban air" "" 0.747
            compartmentGapWarning mempty (M.singleton "ammonia" [flow]) [(cf, Nothing)]
                `shouldBe` Just "1 factor(s) name a flow this database files under another compartment (method: \"urban air\"; database: \"air\"). Declare a [[compartment-mappings]] table bridging them."

        it "stays silent when the name is simply absent" $ do
            let cf = mkCFComp "ammonia" "air" "" 0.747
            compartmentGapWarning mempty M.empty [(cf, Nothing)] `shouldBe` Nothing

        it "stays silent when the database speaks the stated medium, only not for that substance" $ do
            -- Nitrite in water and not in air is the database's inventory,
            -- not its vocabulary: nothing to declare.
            fid1 <- nextRandom
            fid2 <- nextRandom
            let nitrite = mkFlow fid1 "nitrite" Water Nothing
                co2 = mkFlow fid2 "co2" Air Nothing
                byName = M.fromList [("nitrite", [nitrite]), ("co2", [co2])]
                cf = mkCFComp "nitrite" "air" "" 1.0
            compartmentGapWarning mempty byName [(cf, Nothing)] `shouldBe` Nothing

        it "stays silent for a factor that resolved" $ do
            fid <- nextRandom
            let flow = mkFlow fid "ammonia" Air Nothing
                cf = mkCFComp "ammonia" "air" "" 0.747
            compartmentGapWarning mempty (M.singleton "ammonia" [flow]) [(cf, Just (flow, ByName))] `shouldBe` Nothing

    describe "fillBroadcastVector + computeLCIAScoreFromTables (Phase 1)" $ do
        let mkUnit uid name = Unit{unitId = uid, unitName = name, unitSymbol = name, unitComment = ""}

        it "scoring with empty broadcast equals scoring with filled broadcast (UUID match)" $ do
            fid <- nextRandom
            uidKg <- nextRandom
            let flow = (mkFlow fid "co2" Air Nothing){bfUnitId = uidKg}
                cf = (mkCF "co2" Nothing 2.5){mcfUnit = "kg"}
                rawTables = buildMethodTables mempty mempty M.empty [(cf, Just (flow, ByUUID))]
                flowDB = M.singleton fid flow
                unitDB = M.singleton uidKg (mkUnit uidKg "kg")
                inv = M.fromList [(fid, 4.0 :: Double)]
                -- empty broadcast → legacy path
                legacyScore = loScore (computeLCIAScoreFromTables defaultUnitConfig unitDB flowDB inv rawTables)
                -- filled broadcast → fast path
                filled = fillBroadcastVector defaultUnitConfig unitDB flowDB rawTables
                fastScore = loScore (computeLCIAScoreFromTables defaultUnitConfig unitDB flowDB inv filled)
            legacyScore `shouldBe` (4.0 * 2.5 :: Double)
            fastScore `shouldBe` legacyScore

        it "pre-multiplied broadcast equals legacy when CF unit absorbs into flow unit" $ do
            -- Build a custom config with both kg and g (default config only has kg).
            -- 1 g = 0.001 kg → factor 0.001 against the SI base.
            let kgDef = unitDef "mass" 1.0
                gDef = unitDef "mass" 1.0e-3
                cfg =
                    UnitConversion.mkUnitConfig
                        []
                        (M.fromList [("kg", kgDef), ("g", gDef)])
            fid <- nextRandom
            uidKg <- nextRandom
            let flow = (mkFlow fid "co2" Air Nothing){bfUnitId = uidKg}
                cf = (mkCF "co2" Nothing 1.0e-3){mcfUnit = "g"}
                tables0 = buildMethodTables mempty mempty M.empty [(cf, Just (flow, ByUUID))]
                flowDB = M.singleton fid flow
                unitDB = M.singleton uidKg (mkUnit uidKg "kg")
                inv = M.fromList [(fid, 1.0 :: Double)]
                filled = fillBroadcastVector cfg unitDB flowDB tables0
                fast = loScore (computeLCIAScoreFromTables cfg unitDB flowDB inv filled)
                legacy = loScore (computeLCIAScoreFromTables cfg unitDB flowDB inv tables0)
            -- Parity: pre-multiplication must match the on-the-fly path.
            fast `shouldBe` legacy
            -- 1 kg × convert(kg→g, 1) × 1e-3 (CF) = 1 × 1000 × 1e-3 = 1.0.
            fast `shouldSatisfy` (\v -> abs (v - 1.0) < 1.0e-12)
            -- Broadcast must be filled.
            M.null (mtBroadcast filled) `shouldBe` False

        it "broadcast covers exact (name, medium, subcomp) cascade" $ do
            fid <- nextRandom
            uidKg <- nextRandom
            let flow = (mkFlow fid "co2" Air (Just "high pop")){bfUnitId = uidKg}
                cf = (mkCFComp "co2" "air" "high pop" 3.0){mcfUnit = "kg"}
                tables0 = buildMethodTables mempty mempty M.empty [(cf, Just (flow, ByName))]
                flowDB = M.singleton fid flow
                unitDB = M.singleton uidKg (mkUnit uidKg "kg")
                inv = M.fromList [(fid, 2.0 :: Double)]
                filled = fillBroadcastVector defaultUnitConfig unitDB flowDB tables0
                fast = loScore (computeLCIAScoreFromTables defaultUnitConfig unitDB flowDB inv filled)
                legacy = loScore (computeLCIAScoreFromTables defaultUnitConfig unitDB flowDB inv tables0)
            fast `shouldBe` legacy
            fast `shouldBe` (2.0 * 3.0 :: Double)

        it "broadcast covers fallback (name, medium) cascade" $ do
            fid <- nextRandom
            uidKg <- nextRandom
            -- Flow has subcomp "high pop", but CF only has medium-level entry (subcomp "")
            let flow = (mkFlow fid "co2" Air (Just "high pop")){bfUnitId = uidKg}
                cf = (mkCFComp "co2" "air" "" 5.0){mcfUnit = "kg"}
                tables0 = buildMethodTables mempty mempty M.empty [(cf, Just (flow, ByName))]
                flowDB = M.singleton fid flow
                unitDB = M.singleton uidKg (mkUnit uidKg "kg")
                inv = M.fromList [(fid, 1.0 :: Double)]
                filled = fillBroadcastVector defaultUnitConfig unitDB flowDB tables0
                fast = loScore (computeLCIAScoreFromTables defaultUnitConfig unitDB flowDB inv filled)
                legacy = loScore (computeLCIAScoreFromTables defaultUnitConfig unitDB flowDB inv tables0)
            fast `shouldBe` legacy
            fast `shouldBe` (5.0 :: Double)

        it "inventory UUID outside broadcast falls back to legacy lookup (cross-DB)" $ do
            fidLocal <- nextRandom
            fidExtra <- nextRandom -- in inventory but NOT in flowDB at fill time
            uidKg <- nextRandom
            let flowLocal = (mkFlow fidLocal "co2" Air Nothing){bfUnitId = uidKg}
                cf = (mkCF "co2" Nothing 1.5){mcfUnit = "kg"}
                tables0 = buildMethodTables mempty mempty M.empty [(cf, Just (flowLocal, ByUUID))]
                flowDBAtBuild = M.singleton fidLocal flowLocal
                unitDB = M.singleton uidKg (mkUnit uidKg "kg")
                filled = fillBroadcastVector defaultUnitConfig unitDB flowDBAtBuild tables0
                -- Scoring time: inventory has fidExtra (cross-DB flow added later)
                inv = M.fromList [(fidLocal, 2.0 :: Double), (fidExtra, 7.0)]
                fast = loScore (computeLCIAScoreFromTables defaultUnitConfig unitDB flowDBAtBuild inv filled)
            -- fidLocal contributes 2.0 * 1.5 = 3.0; fidExtra has no CF → 0.
            -- The fallback path must NOT crash on the unknown UUID.
            fast `shouldBe` (3.0 :: Double)

    describe "zeroedMatchedCFs (matched CF the flow's unit cannot reach)" $ do
        -- kg is a mass and m3 a volume: no conversion path between them, so a
        -- kg-denominated CF matched by an m3 flow is refused and scores 0 –
        -- exactly the silent undercount this scan exists to surface.
        let cfg =
                mkUnitConfig
                    []
                    ( M.fromList
                        [ ("kg", unitDef "mass" 1.0)
                        , ("m3", unitDef "volume" 1.0)
                        , ("mj", unitDef "energy" 1.0)
                        ]
                    )
            fillWith densities unitName' cf = do
                fid <- nextRandom
                uid <- nextRandom
                let flow = (mkFlow fid "gas" Air Nothing){bfUnitId = uid}
                    flowDB = M.singleton fid flow
                    unitDB = M.singleton uid ((unitNamed unitName'){unitId = uid})
                    filled =
                        fillBroadcastVector cfg unitDB flowDB $
                            buildMethodTables mempty mempty densities [(cf, Just (flow, ByUUID))]
                pure (fid, [bfId f | (f, _, _) <- zeroedMatchedCFs cfg unitDB flowDB filled])
            fillFor = fillWith M.empty

        it "flags a kg-denominated CF matched by an m3 flow" $ do
            (fid, zeroed) <- fillFor "m3" (mkCF "gas" Nothing 43.1)
            zeroed `shouldBe` [fid]

        it "stays quiet when the conversion exists" $ do
            (_, zeroed) <- fillFor "kg" (mkCF "gas" Nothing 43.1)
            zeroed `shouldBe` []

        it "stays quiet for a CF the method genuinely declares as 0" $ do
            -- Same dimensional mismatch, but the factor itself is 0: the zero
            -- contribution is the method's own value, not a refusal.
            (_, zeroed) <- fillFor "m3" (mkCF "gas" Nothing 0.0)
            zeroed `shouldBe` []

        it "flags a refused CF that is only regionalized (no broadcast entry)" $ do
            -- A located CF never reaches the broadcast tables, so a
            -- broadcast-only scan would stay silent about its refusal.
            (fid, zeroed) <-
                fillFor "m3" ((mkCF "gas" Nothing 43.1){mcfConsumerLocation = Just "FR"})
            zeroed `shouldBe` [fid]

        it "flags a failed energy-density bridge" $ do
            -- CF per MJ, density native to kg, flow in m3: the bridge fires
            -- (MJ matches the density unit) but m3 cannot reach kg, so the
            -- conversion is refused.
            (fid, zeroed) <-
                fillWith
                    (M.singleton "gas" (EnergyDensity 43.1 "MJ" "kg"))
                    "m3"
                    ((mkCF "gas" Nothing 50.0){mcfUnit = "MJ"})
            zeroed `shouldBe` [fid]

        it "stays quiet when the bridge converts the other way round" $ do
            -- The mirror: flow in the density's target unit (m3), CF in its
            -- native one (kg). The inverse arm divides, so nothing is refused
            -- and the scan must not keep reporting it.
            (_, zeroed) <-
                fillWith (M.singleton "gas" (EnergyDensity 0.001 "m3" "kg")) "m3" (mkCF "gas" Nothing 43.1)
            zeroed `shouldBe` []

        it "still flags a pair neither direction of the bridge can span" $ do
            -- Density between m3 and kg, but the flow is in MJ: neither leg is
            -- reachable, so the refusal stands and stays visible.
            (fid, zeroed) <-
                fillWith (M.singleton "gas" (EnergyDensity 0.001 "m3" "kg")) "mj" (mkCF "gas" Nothing 43.1)
            zeroed `shouldBe` [fid]

    describe "findSimilarCFs (post-scoring suggester)" $ do
        let mkMethod cfs =
                Method
                    { methodId = nil
                    , methodName = "Test"
                    , methodDescription = Nothing
                    , methodUnit = "kg eq"
                    , methodCategory = "Climate change"
                    , methodMethodology = Nothing
                    , methodFactors = cfs
                    }
            airComp = Just (Compartment "air" "" "")

        it "returns no candidates from an empty method" $ do
            fid <- nextRandom
            let flow = (mkFlow fid "Carbon dioxide" Air Nothing){bfCAS = Nothing}
                idx = buildMethodIndex (mkMethod [])
            findSimilarCFs emptyChemSynonyms idx flow 3 `shouldBe` []

        it "matches CO2 to Carbon dioxide via PubChem synonym expansion" $ do
            fid <- nextRandom
            let csv =
                    "cas;canonical_name;synonyms...\n\
                    \124-38-9;Carbon dioxide;CO2;Carbonic anhydride\n"
                Right syns = parseChemSynonymsCSV csv
                co2 = (mkCFComp "CO2" "air" "" 1.0){mcfCompartment = airComp}
                ch4 = (mkCFComp "Methane" "air" "" 27.0){mcfCompartment = airComp}
                idx = buildMethodIndex (mkMethod [co2, ch4])
                flow = (mkFlow fid "Carbon dioxide" Air Nothing){bfCAS = Nothing}
                cands = findSimilarCFs syns idx flow 3
            -- The CO2 candidate must be present, with the synonym-expansion reason.
            let names = map scfMethodFlowName cands
            names `shouldSatisfy` ("CO2" `elem`)
            let co2Cand = head [c | c <- cands, scfMethodFlowName c == "CO2"]
            scfReason co2Cand `shouldBe` SimBySynonymExpansion
            scfScore co2Cand `shouldSatisfy` (> 0)

        it "matches via CAS bridge when names diverge entirely" $ do
            fid <- nextRandom
            let oddName =
                    (mkCFComp "Some weird IUPAC name" "air" "" 1.0)
                        { mcfCAS = Just "124-38-9"
                        , mcfCompartment = airComp
                        }
                idx = buildMethodIndex (mkMethod [oddName])
                flow =
                    (mkFlow fid "Random unrelated text" Air Nothing)
                        { bfCAS = Just "124-38-9"
                        }
                cands = findSimilarCFs emptyChemSynonyms idx flow 3
            map scfReason cands `shouldBe` [SimByCASBridge]
            map scfScore cands `shouldBe` [0.95]

        it "ranks the higher-similarity candidate first" $ do
            fid <- nextRandom
            let close = mkCFComp "Methane biogenic" "air" "" 27.0
                far = mkCFComp "Crude oil" "air" "" 0.0
                idx = buildMethodIndex (mkMethod [far, close])
                flow = mkFlow fid "Methane, biogenic" Air Nothing
                cands = findSimilarCFs emptyChemSynonyms idx flow 2
            map scfMethodFlowName cands `shouldSatisfy` (\ns -> not (null ns) && head ns == "Methane biogenic")

        it "respects maxN cap" $ do
            fid <- nextRandom
            let cfs = [mkCFComp ("foo " <> tShow i) "air" "" 1.0 | i <- [1 .. 10 :: Int]]
                idx = buildMethodIndex (mkMethod cfs)
                flow = mkFlow fid "foo bar" Air Nothing
                cands = findSimilarCFs emptyChemSynonyms idx flow 3
            length cands `shouldSatisfy` (<= 3)

    describe "findUncharacterized" $ do
        let mkMethod cfs =
                Method
                    { methodId = nil
                    , methodName = "Test"
                    , methodDescription = Nothing
                    , methodUnit = "kg eq"
                    , methodCategory = "Climate change"
                    , methodMethodology = Nothing
                    , methodFactors = cfs
                    }

        it "returns [] when uoMaxFlows is 0" $ do
            fid <- nextRandom
            let flow = mkFlow fid "co2" Air Nothing
                inv = M.singleton fid 100.0
                tables = buildMethodTables mempty mempty M.empty []
                idx = buildMethodIndex (mkMethod [])
                opts = defaultUncharacterizedOpts{uoMaxFlows = 0}
            findUncharacterized
                defaultUnitConfig
                M.empty
                (M.singleton fid flow)
                inv
                tables
                emptyChemSynonyms
                idx
                opts
                `shouldBe` []

        it "drops flows below the absolute-weight threshold" $ do
            big <- nextRandom
            small <- nextRandom
            let bigFlow = mkFlow big "tiny stuff" Air Nothing
                smallFlow = mkFlow small "huge stuff" Air Nothing
                inv = M.fromList [(big, 999.0), (small, 1.0)]
                flowDB = M.fromList [(big, bigFlow), (small, smallFlow)]
                tables = buildMethodTables mempty mempty M.empty []
                idx = buildMethodIndex (mkMethod [])
                opts = defaultUncharacterizedOpts{uoMinAbsWeight = 0.5}
                result =
                    findUncharacterized
                        defaultUnitConfig
                        M.empty
                        flowDB
                        inv
                        tables
                        emptyChemSynonyms
                        idx
                        opts
            -- Only the big flow (99.9% of mass) clears the 50% threshold.
            map ucfFlowName result `shouldBe` ["tiny stuff"]

        it "skips flows that DO have a CF (they're characterized)" $ do
            fid <- nextRandom
            let flow = mkFlow fid "co2" Air Nothing
                cf = (mkCF "co2" Nothing 1.0){mcfFlowRef = fid}
                tables = buildMethodTables mempty mempty M.empty [(cf, Just (flow, ByUUID))]
                idx = buildMethodIndex (mkMethod [cf])
                inv = M.singleton fid 100.0
                flowDB = M.singleton fid flow
            findUncharacterized
                defaultUnitConfig
                M.empty
                flowDB
                inv
                tables
                emptyChemSynonyms
                idx
                defaultUncharacterizedOpts
                `shouldBe` []

    describe "wildcard (pattern) CFs" $ do
        let flowDB = M.fromList [(bfId f, f) | f <- allFlows]
            allFlows =
                [ occAnnual
                , occOrchard
                , transformation
                , waterRiver
                , waterWell
                , methaneAir
                , methaneWater
                , occSea
                , occIndustrial
                , occBenthos
                ]
            occAnnual = mkFlow (u 1) "Occupation, annual crop" NaturalResource Nothing
            occOrchard = mkFlow (u 2) "Occupation, permanent crop, fruit" NaturalResource Nothing
            -- The sea floor and a factory yard sit in one occupation family, and
            -- the drowned one shares its prefix with the dry one: the case
            -- exclusions exist for, since no set of prefixes separates them.
            occSea = mkFlow (u 8) "Occupation, sea and ocean" NaturalResource Nothing
            occIndustrial = mkFlow (u 9) "Occupation, industrial area" NaturalResource Nothing
            occBenthos = mkFlow (u 10) "Occupation, industrial area, benthos" NaturalResource Nothing
            transformation = mkFlow (u 3) "Transformation, to annual crop" NaturalResource Nothing
            waterRiver = mkFlow (u 4) "Water, river" NaturalResource Nothing
            waterWell = mkFlow (u 7) "Water, well" NaturalResource (Just "in ground")
            methaneAir = (mkFlow (u 5) "Methane, fossil" Air Nothing){bfCAS = Just "74-82-8"}
            methaneWater = (mkFlow (u 6) "Methane, fossil" Water Nothing){bfCAS = Just "74-82-8"}
            occupationFlows = [occAnnual, occOrchard, occSea, occIndustrial, occBenthos]
            u = uuidFromInt
            -- Compare by UUID: BiosphereFlow has no Eq/Show instance.
            expandedIds = map (fmap (bfId . fst) . snd) . fst

        it "detects a trailing-star name as a pattern, a literal name as not" $ do
            isPatternCF (mkCF "Occupation*" Nothing 1.0) `shouldBe` True
            isPatternCF (mkCF "*" Nothing 1.0) `shouldBe` True
            isPatternCF (mkCF "Occupation, annual crop" Nothing 1.0) `shouldBe` False

        it "expands a prefix pattern to every flow of the compartment, none else" $ do
            let cf = mkCFComp "Occupation*" "natural resource" "" 1.0
            expandedIds (expandPatternCF flowDB [] cf)
                `shouldMatchList` map (Just . bfId) occupationFlows

        it "materializes each match with the flow's own identity, keeping value and unit" $ do
            let cf = (mkCFComp "Occupation*" "natural resource" "" 1.0){mcfUnit = "m2a"}
                (rows, warnings) = expandPatternCF flowDB [] cf
            warnings `shouldBe` []
            [(mcfFlowRef m, mcfFlowName m, mcfValue m, mcfUnit m) | (m, _) <- rows]
                `shouldMatchList` [(bfId f, bfName f, 1.0, "m2a") | f <- occupationFlows]

        it "honors the sub-compartment a pattern row states, widens without one" $ do
            let inGround = mkCFComp "Water*" "natural resource" "in ground" 1.0
                anySub = mkCFComp "Water*" "natural resource" "" 1.0
            expandedIds (expandPatternCF flowDB [] inGround) `shouldBe` [Just (bfId waterWell)]
            expandedIds (expandPatternCF flowDB [] anySub)
                `shouldMatchList` map (Just . bfId) [waterRiver, waterWell]

        it "reads a pattern row written at unspecified as that place, not as any" $ do
            -- The river water names no subcompartment, which is an unspecified
            -- flow; the well water is "in ground", another place.
            let atUnspecified = mkCFComp "Water*" "natural resource" "unspecified" 1.0
            expandedIds (expandPatternCF flowDB [] atUnspecified) `shouldBe` [Just (bfId waterRiver)]

        it "a bare * with a CAS expands by CAS, filtered by the row's compartment" $ do
            let cf = (mkCFComp "*" "air" "" 1.0){mcfCAS = Just "74-82-8"}
            expandedIds (expandPatternCF flowDB [] cf) `shouldBe` [Just (bfId methaneAir)]

        it "a pattern matching no flow surfaces one unmatched row and a warning" $ do
            let cf = mkCFComp "Uranium*" "natural resource" "" 1.0
                (rows, warnings) = expandPatternCF flowDB [] cf
            map (fmap (bfId . fst) . snd) rows `shouldBe` [Nothing]
            length warnings `shouldBe` 1

        it "a bare * constrained by nothing is refused, not matched to everything" $ do
            let cf = mkCF "*" Nothing 1.0
                (rows, warnings) = expandPatternCF flowDB [] cf
            map (fmap (bfId . fst) . snd) rows `shouldBe` [Nothing]
            length warnings `shouldBe` 1

        it "reads a leading ! as an exclusion, not as a pattern" $ do
            isExclusionCF (mkCF "!Occupation, sea*" Nothing 1.0) `shouldBe` True
            isExclusionCF (mkCF "Occupation*" Nothing 1.0) `shouldBe` False
            -- An exclusion ends in a star too; whichever test runs first must
            -- not claim it, or the row would be expanded instead of subtracted.
            isPatternCF (mkCF "!Occupation, sea*" Nothing 1.0) `shouldBe` False

        it "an exclusion takes its flows back out of the family the pattern opened" $ do
            let cf = mkCFComp "Occupation*" "natural resource" "" 1.0
                sea = mkCFComp "!Occupation, sea*" "natural resource" "" 1.0
            expandedIds (expandPatternCF flowDB [sea] cf)
                `shouldMatchList` map (Just . bfId) [occAnnual, occOrchard, occIndustrial, occBenthos]

        it "an exclusion needs no trailing star: a whole flow name is a prefix of nothing else" $ do
            let cf = mkCFComp "Occupation*" "natural resource" "" 1.0
                benthos = mkCFComp "!Occupation, industrial area, benthos" "natural resource" "" 1.0
            -- The dry industrial area survives, though it shares the prefix up
            -- to the comma: the exclusion is read in full, not truncated.
            expandedIds (expandPatternCF flowDB [benthos] cf)
                `shouldMatchList` map (Just . bfId) [occAnnual, occOrchard, occSea, occIndustrial]

        it "an exclusion matching no flow is announced, never silently ignored" $ do
            let typo = mkCFComp "!Occupation, seaa*" "natural resource" "" 1.0
            exclusionWarning flowDB typo `shouldSatisfy` isJust

        it "an exclusion constrained by nothing is refused, like a bare pattern" $
            exclusionWarning flowDB (mkCF "!" Nothing 1.0) `shouldSatisfy` isJust

        it "an exclusion that does its job says nothing" $ do
            let sea = mkCFComp "!Occupation, sea*" "natural resource" "" 1.0
            exclusionWarning flowDB sea `shouldBe` Nothing

        it "a pattern whose every match is excluded is refused, not silently empty" $ do
            let cf = mkCFComp "Transformation*" "natural resource" "" 1.0
                all' = mkCFComp "!Transformation*" "natural resource" "" 1.0
                (rows, warnings) = expandPatternCF flowDB [all'] cf
            map (fmap (bfId . fst) . snd) rows `shouldBe` [Nothing]
            length warnings `shouldBe` 1

        it "mapMethodFlows subtracts the category's exclusions from its patterns" $ do
            let method =
                    Method
                        { methodId = nil
                        , methodName = "Land occupied"
                        , methodDescription = Nothing
                        , methodUnit = "m2a"
                        , methodCategory = "Land occupied"
                        , methodMethodology = Nothing
                        , methodFactors =
                            [ mkCFComp "Occupation*" "natural resource" "" 1.0
                            , mkCFComp "!Occupation, sea*" "natural resource" "" 1.0
                            , mkCFComp "!Occupation, industrial area, benthos" "natural resource" "" 1.0
                            ]
                        }
                ctx = MapContext flowDB (byName allFlows) M.empty emptySynonymDB M.empty mempty M.empty
            mappings <- mapMethodFlows ctx method
            -- The exclusion rows themselves never become factors: a method that
            -- kept them would characterize the very flows it just disowned.
            map (fmap (bfId . fst) . snd) mappings
                `shouldMatchList` map (Just . bfId) [occAnnual, occOrchard, occIndustrial]

        it "an exclusion still holds when the synonym fan-out re-reaches its flow" $ do
            let method =
                    Method
                        { methodId = nil
                        , methodName = "Land occupied"
                        , methodDescription = Nothing
                        , methodUnit = "m2a"
                        , methodCategory = "Land occupied"
                        , methodMethodology = Nothing
                        , methodFactors =
                            [ mkCFComp "Occupation*" "natural resource" "" 1.0
                            , mkCFComp "!Occupation, industrial area, benthos" "natural resource" "" 1.0
                            ]
                        }
                ctx = MapContext flowDB (byName allFlows) M.empty emptySynonymDB M.empty mempty M.empty
                -- The curated registry bridges the dry industrial area and its
                -- drowned namesake through the label they share, as data/flows.csv
                -- does; the fan-out then travels by name, knowing no exceptions.
                synDB =
                    buildFromPairs
                        [ ("Occupation, industrial area", "industrial area")
                        , ("Occupation, industrial area, benthos", "industrial area")
                        ]
            mappings <- mapMethodFlows ctx method
            let expanded = expandSynonymMappings synDB (byName allFlows) mappings
                ids = map (fmap (bfId . fst) . snd)
            -- The bridge really does hand the excluded flow back – without this
            -- the test below would pass on an expansion that never reached it.
            ids expanded `shouldSatisfy` elem (Just (bfId occBenthos))
            ids (dropExcludedMappings (filter isExclusionCF (methodFactors method)) expanded)
                `shouldSatisfy` notElem (Just (bfId occBenthos))

        it "mapMethodFlows resolves literal rows via the cascade and expands patterns" $ do
            let method =
                    Method
                        { methodId = nil
                        , methodName = "Land occupied"
                        , methodDescription = Nothing
                        , methodUnit = "m2a"
                        , methodCategory = "Land occupied"
                        , methodMethodology = Nothing
                        , methodFactors =
                            [ mkCFComp "Water, river" "natural resource" "" 1.0
                            , mkCFComp "Occupation*" "natural resource" "" 1.0
                            ]
                        }
                ctx = MapContext flowDB (byName allFlows) M.empty emptySynonymDB M.empty mempty M.empty
            mappings <- mapMethodFlows ctx method
            map (fmap (bfId . fst) . snd) mappings
                `shouldMatchList` map (Just . bfId) (waterRiver : occupationFlows)

    -- Lint of the shipped method file, like RegistryLintSpec for data/flows.csv:
    -- a header typo or a misplaced comment would otherwise ship silently.
    describe "shipped plain-indicators method (data/methods/plain-indicators.csv)" $ do
        parsed <- runIO (parseMethodCSVBytes <$> BS.readFile "data/methods/plain-indicators.csv")

        it "parses into its seven categories" $
            fmap (map methodName) parsed
                `shouldBe` Right ["Land occupied", "Water used", "Fossil CO2", "Methane", "Primary energy", "Waste heat", "Cadmium"]

        it "carries its wildcard rows as patterns" $
            [mcfFlowName cf | Right ms <- [parsed], m <- ms, cf <- methodFactors m, isPatternCF cf]
                `shouldBe` ["Occupation*", "Water*", "Energy, *", "Heat, waste*"]

        it "carries its marine exceptions as exclusions of Land occupied" $
            [ (methodName m, mcfFlowName cf)
            | Right ms <- [parsed]
            , m <- ms
            , cf <- methodFactors m
            , isExclusionCF cf
            ]
                `shouldBe` [ ("Land occupied", "!Occupation, sea*")
                           , ("Land occupied", "!Occupation, seabed*")
                           , ("Land occupied", "!Occupation, dump site, benthos")
                           , ("Land occupied", "!Occupation, industrial area, benthos")
                           ]

tShow :: (Show a) => a -> Text
tShow = T.pack . show

-- | Deterministic UUID for wildcard-CF fixtures.
uuidFromInt :: Int -> UUID
uuidFromInt n = fromWords (fromIntegral n) 0 0 0

-- | Name index the cascade reads, keyed like the loader keys it.
byName :: [BiosphereFlow] -> M.Map Text [BiosphereFlow]
byName fs = M.fromListWith (++) [(normalizeName (bfName f), [f]) | f <- fs]
