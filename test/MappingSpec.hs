{-# LANGUAGE OverloadedStrings #-}

module MappingSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID, fromWords, nil)
import Data.UUID.V4 (nextRandom)
import Test.Hspec

import Data.Either (isLeft)
import Data.Maybe (isJust)
import Method.ChemSynonyms (emptyChemSynonyms, parseChemSynonymsCSV)
import Method.Mapping
import Method.ParserCSV (parseMethodCSVBytes)
import Method.Types (Compartment (..), EnergyDensity (..), FlowDirection (..), Method (..), MethodCF (..), buildCompartmentMapFromCSV)
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
factor 1.0), so 'normalizeToCanonical' fails - exercises the result-expression
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
            fmap bfId (findFlowByNameComp M.empty M.empty "co2" Nothing) `shouldBe` Nothing

        it "returns first flow when no compartment preference" $ do
            fid1 <- nextRandom
            fid2 <- nextRandom
            let f1 = mkFlow fid1 "co2" Air Nothing
                f2 = mkFlow fid2 "co2" Water Nothing
                byName = M.singleton "co2" [f1, f2]
            fmap bfId (findFlowByNameComp M.empty byName "co2" Nothing) `shouldBe` Just fid1

        it "prefers exact medium+subcomp match" $ do
            fid1 <- nextRandom
            fid2 <- nextRandom
            let fAir = mkFlow fid1 "co2" Air (Just "urban air")
                fWater = mkFlow fid2 "co2" Water (Just "surface water")
                byName = M.singleton "co2" [fWater, fAir]
                comp = Compartment "air" "urban air" ""
            fmap bfId (findFlowByNameComp M.empty byName "co2" (Just comp)) `shouldBe` Just fid1

        it "falls back to medium match when no exact subcomp" $ do
            fid1 <- nextRandom
            fid2 <- nextRandom
            let fAir = mkFlow fid1 "co2" Air (Just "non-urban air")
                fWater = mkFlow fid2 "co2" Water Nothing
                byName = M.singleton "co2" [fWater, fAir]
                comp = Compartment "air" "unspecified" ""
            fmap bfId (findFlowByNameComp M.empty byName "co2" (Just comp)) `shouldBe` Just fid1

        it "answers nothing when no candidate is in the stated medium" $ do
            -- A row for an emission to air does not describe a water flow of
            -- the same name, so the name matcher has found nothing and the
            -- cascade is free to try the next one.
            fid1 <- nextRandom
            let fWater = mkFlow fid1 "co2" Water Nothing
                byName = M.singleton "co2" [fWater]
                comp = Compartment "air" "" ""
            fmap bfId (findFlowByNameComp M.empty byName "co2" (Just comp)) `shouldBe` Nothing

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
            fmap bfId (findFlowByNameComp M.empty byName "ammonia" (Just comp)) `shouldBe` Just fid

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
            fmap bfId (findFlowByNameComp M.empty byName "co2" (Just comp)) `shouldBe` Just fidNow

        it "meets a flow through a compartment the table relates" $ do
            -- The medium is a condition now, and "Emissions to air" is the
            -- spelling an ILCD method writes for what a database files under
            -- "air". Both sides go through the table, as scoring does.
            fid <- nextRandom
            let fAir = mkFlow fid "co2" Air Nothing
                byName = M.singleton "co2" [fAir]
                cmap = M.singleton ("emissions to air", "", "") (Compartment "air" "" "")
                comp = Compartment "Emissions to air" "" ""
            fmap bfId (findFlowByNameComp cmap byName "co2" (Just comp)) `shouldBe` Just fid

        it "takes the flow claiming no subcompartment over an unrelated sibling" $ do
            -- Neither candidate is at "low. pop.". One is at a subcompartment
            -- the row does not name, the other at none: the second is what the
            -- row is left with, and it must not depend on the index order.
            fidLongTerm <- nextRandom
            fidPlain <- nextRandom
            let fLongTerm = mkFlow fidLongTerm "co2" Air (Just "low. pop., long-term")
                fPlain = mkFlow fidPlain "co2" Air Nothing
                comp = Compartment "air" "low. pop." ""
            fmap bfId (findFlowByNameComp M.empty (M.singleton "co2" [fLongTerm, fPlain]) "co2" (Just comp))
                `shouldBe` Just fidPlain
            fmap bfId (findFlowByNameComp M.empty (M.singleton "co2" [fPlain, fLongTerm]) "co2" (Just comp))
                `shouldBe` Just fidPlain

    describe "findFlowByCAS" $ do
        it "finds flow by CAS number" $ do
            fid <- nextRandom
            let flow = mkFlow fid "Carbon dioxide" Air Nothing
                byCAS = M.singleton "124-38-9" [flow]
            fmap bfId (findFlowByCAS M.empty byCAS "124-38-9" Nothing) `shouldBe` Just fid

        it "returns Nothing for unknown CAS" $
            fmap bfId (findFlowByCAS M.empty M.empty "000-00-0" Nothing) `shouldBe` Nothing

        -- The index is keyed canonically; a method whose parser kept the
        -- source's zero-padding still has to reach the same flow, or its
        -- factor goes silently missing.
        it "meets a canonically indexed flow from a zero-padded query" $ do
            fid <- nextRandom
            let flow = mkFlow fid "Carbon dioxide" Air Nothing
                byCAS = M.singleton "124-38-9" [flow]
            fmap bfId (findFlowByCAS M.empty byCAS "000124-38-9" Nothing) `shouldBe` Just fid

        -- An all-zeros placeholder is not a substance anchor: indexing it
        -- would collide every CAS-less flow onto one key.
        it "refuses an all-zeros placeholder rather than treating it as a CAS" $ do
            fid <- nextRandom
            let flow = mkFlow fid "Unknown" Air Nothing
                byCAS = M.singleton "0-00-0" [flow]
            fmap bfId (findFlowByCAS M.empty byCAS "000-00-0" Nothing) `shouldBe` Nothing

    describe "findFlowByName" $ do
        it "finds a flow by name (case-insensitive via normalization)" $ do
            fid <- nextRandom
            let flow = mkFlow fid "Carbon dioxide" Air Nothing
                byName = M.singleton "carbon dioxide" [flow]
            fmap bfId (findFlowByName M.empty byName "Carbon dioxide") `shouldBe` Just fid

        it "returns Nothing for unknown name" $
            fmap bfId (findFlowByName M.empty M.empty "co2") `shouldBe` Nothing

    describe "findFlowBySynonym" $ do
        it "returns Nothing when synonym not in DB" $ do
            fid <- nextRandom
            let flow = mkFlow fid "Carbon dioxide" Air Nothing
                byName = M.singleton "carbon dioxide" [flow]
            fmap bfId (findFlowBySynonym (SynonymSearch emptySynonymDB byName M.empty) "CO2") `shouldBe` Nothing

    describe "findFlowBySynonymComp" $ do
        it "finds flow via synonym with compartment preference" $ do
            fid1 <- nextRandom
            fid2 <- nextRandom
            let synDB = buildFromPairs [("CO2", "Carbon dioxide")]
                fAir = mkFlow fid1 "Carbon dioxide" Air Nothing
                fWater = mkFlow fid2 "Carbon dioxide" Water Nothing
                byName = M.singleton "carbon dioxide" [fWater, fAir]
                comp = Compartment "air" "" ""
            fmap bfId (findFlowBySynonymComp (SynonymSearch synDB byName M.empty) "CO2" (Just comp))
                `shouldBe` Just fid1

        it "returns Nothing when synonym not in DB" $ do
            fid <- nextRandom
            let synDB = buildFromPairs [("CO2", "Carbon dioxide")]
                flow = mkFlow fid "Carbon dioxide" Air Nothing
                byName = M.singleton "carbon dioxide" [flow]
            fmap bfId (findFlowBySynonymComp (SynonymSearch synDB byName M.empty) "methane" Nothing)
                `shouldBe` Nothing

        it "returns Nothing when no flows match any synonym" $ do
            let synDB = buildFromPairs [("CO2", "Carbon dioxide")]
            fmap bfId (findFlowBySynonymComp (SynonymSearch synDB M.empty M.empty) "CO2" Nothing)
                `shouldBe` Nothing

    describe "expandSynonymMappings direction" $ do
        -- The water withdrawal bridge "freshwater" → resource flow applies to an
        -- INPUT (withdrawal) CF only. An OUTPUT (release) CF named "freshwater"
        -- must not fan out onto the resource flow through it - else a release
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
        -- through that pivot - requiring every intermediate to be a flow or CF
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
        -- restriction alone - e.g. a parser defaulted the direction when the
        -- method carried none. The loader surfaces these so the loss is
        -- distinguishable from a genuinely uncharacterized flow.
        let synDB = buildFromEdges [SynEdge "freshwater" "Water, unspecified natural origin" BridgeInput]
            flowsByName fid = M.singleton "water unspecified natural origin" [mkFlow fid "Water, unspecified natural origin" NaturalResource Nothing]
            inputCF = (mkCF "freshwater" Nothing 1.0){mcfDirection = Input}
            outputCF = (mkCF "freshwater" Nothing 1.0){mcfDirection = Output}

        it "flags an unmapped CF whose synonym match exists only outside its direction view" $ do
            fid <- nextRandom
            map mcfFlowName (directionExcludedCFs M.empty synDB (flowsByName fid) [(outputCF, Nothing)])
                `shouldBe` ["freshwater"]

        it "does not flag a CF its own direction view still matches, nor a genuinely unmatched one" $ do
            fid <- nextRandom
            directionExcludedCFs M.empty synDB (flowsByName fid) [(inputCF, Nothing)] `shouldSatisfy` null
            directionExcludedCFs M.empty synDB (flowsByName fid) [(mkCF "unrelated" Nothing 1.0, Nothing)] `shouldSatisfy` null

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
                tables = buildMethodTables OtherCFFamily M.empty M.empty [(cf, Nothing)]
                inventory = M.singleton fid 10.0
                flowDB = M.singleton fid flow
                score = loScore (computeLCIAScoreFromTables defaultUnitConfig M.empty flowDB inventory tables)
            score `shouldBe` 7.47

        it "scores zero when the factor's medium is one no reader can place" $ do
            fid <- nextRandom
            let flow = mkFlow fid "ammonia" Air (Just "low. pop.")
                cf = mkCFComp "ammonia" "urban air" "low. pop." 0.747
                tables = buildMethodTables OtherCFFamily M.empty M.empty [(cf, Nothing)]
                inventory = M.singleton fid 10.0
                flowDB = M.singleton fid flow
                score = loScore (computeLCIAScoreFromTables defaultUnitConfig M.empty flowDB inventory tables)
            score `shouldBe` 0.0

        it "bridges 'urban air' → 'air' via a medium-only rule" $ do
            fid <- nextRandom
            let flow = mkFlow fid "ammonia" Air (Just "low. pop.")
                cf = mkCFComp "ammonia" "urban air" "low. pop." 0.747
                cmap = M.singleton ("urban air", "", "") (Compartment "air" "" "")
                tables = buildMethodTables OtherCFFamily cmap M.empty [(cf, Nothing)]
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
                    M.singleton
                        ("emissions to air", "low. pop.", "")
                        (Compartment "air" "non-urban air or from high stacks" "")
                tables = buildMethodTables OtherCFFamily cmap M.empty [(cf, Nothing)]
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
                score ms = loScore (computeLCIAScoreFromTables cfg unitDB flowDB (M.singleton fid 2.0) (buildMethodTables OtherCFFamily M.empty M.empty ms))
            score mappings `shouldBe` 2.0 * 34.5
            -- insertion order must not matter
            score (reverse mappings) `shouldBe` 2.0 * 34.5

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
                    tables = buildMethodTables OtherCFFamily M.empty M.empty (mappings target)
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

    describe "sea-water gate on wildcard fallbacks" $ do
        -- The same emission to the sea, under two methods that differ in one
        -- thing: whether they write a sea-water factor of their own. A method
        -- that does has an opinion about the sea and the engine defers to it,
        -- keeping the medium-level factor away so the explicit line scores. A
        -- method that never mentions the sea has given nothing to defer to, and
        -- refusing its medium-level factor would score the emission as zero on
        -- an authority the method never gave.
        --
        -- EF 3.1 has both kinds. Freshwater ecotoxicity writes an explicit
        -- near-zero for the sea, so a chromium discharge there must not take
        -- the freshwater factor. Marine eutrophication writes no subcompartment
        -- line at all, because the JRC original gives it the same factor for
        -- fresh water, unspecified water and sea water alike: there was nothing
        -- different to write, and the medium-level factor is the answer.
        let uns = mkCFComp "Nitrogen, total" "water" "(unspecified)" 1.0
            sea = mkCFComp "Nitrogen, total" "water" "ocean" 0.0
            scoreWith cfs sub = do
                fid <- nextRandom
                let flow = mkFlow fid "Nitrogen, total" Water (Just sub)
                    tables =
                        buildMethodTables
                            OtherCFFamily
                            M.empty
                            M.empty
                            [(cf, Just (flow, ByName)) | cf <- cfs]
                    flowDB = M.singleton fid flow
                pure
                    ( loScore
                        (computeLCIAScoreFromTables defaultUnitConfig M.empty flowDB (M.singleton fid 1.0) tables)
                    )

        it "keeps the medium-level factor away from the sea when the method names it" $
            scoreWith [uns, sea] "ocean" `shouldReturn` 0.0

        it "lets the medium-level factor reach the sea when the method never names it" $
            scoreWith [uns] "ocean" `shouldReturn` 1.0

        it "leaves every other subcompartment alone, either way" $ do
            scoreWith [uns, sea] "river" `shouldReturn` 1.0
            scoreWith [uns] "river" `shouldReturn` 1.0
            scoreWith [uns, sea] "(unspecified)" `shouldReturn` 1.0
            scoreWith [uns] "(unspecified)" `shouldReturn` 1.0

        it "still prefers the sea line itself over the medium-level one" $
            -- Naming the sea with a \*larger* factor proves the explicit line is
            -- what scores, not merely that the wildcard was blocked: a gate that
            -- only blocked would leave this uncharacterized, at 0.
            scoreWith [uns, mkCFComp "Nitrogen, total" "water" "ocean" 5.0] "ocean" `shouldReturn` 5.0

        -- The key is lowercased and stripped, so two rows that differ only in
        -- case are one rule with two targets, and the file says nothing about
        -- which was meant.
        it "refuses two rows normalizing the same source compartment" $
            buildCompartmentMapFromCSV "source_medium,source_sub,source_qualifier,target_medium,target_sub,target_qualifier\nwater,sea water,,water,ocean,\nWater,Sea water,,water,river,\n"
                `shouldSatisfy` isLeft

        it "recognizes the sea through the spelling compartments.csv translates" $ do
            -- 'isForeignMediumSub' names the canonical subcompartment only, so
            -- the source spellings are compartments.csv's job. This is the test
            -- that fails if that translation is dropped: both the method's line
            -- and the flow say "sea water", and the gate must still see the sea.
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
                            (buildMethodTables OtherCFFamily cmap M.empty [(cf, Just (flow, ByName)) | cf <- cfs])
            score [uns, mkCFComp "Nitrogen, total" "water" "sea water" 0.0] `shouldBe` 0.0
            score [uns] `shouldBe` 1.0

    describe "groundwater gate on wildcard fallbacks (read path)" $ do
        -- EF SimaPro exports leave immediate groundwater implicit (only
        -- "groundwater, long-term" carries an explicit zero), so SimaPro
        -- subcompartment semantics apply: an implicit sub inherits the
        -- unspecified CF. The USEtox gate must therefore block only the
        -- LONG-TERM groundwater fate - otherwise the method's explicit zero
        -- would be bypassed via the CAS bridge - and never the immediate one.
        cmap <- runIO $ do
            csv <- BL.readFile "data/compartments.csv"
            either (fail . ("compartments.csv: " <>)) pure (buildCompartmentMapFromCSV csv)
        let cfUns = (mkCFComp "Iron, ion" "water" "(unspecified)" 2108.5){mcfCAS = Just "7439-89-6"}
            cfLt = mkCFComp "Iron, ion" "water" "groundwater, long-term" 0.0
            scoreVia cm fam name mCas sub = do
                fid <- nextRandom
                mid <- nextRandom
                let flow = (mkFlow fid name Water (Just sub)){bfCAS = mCas}
                    -- cfUns matched ByCAS on a sibling flow, so it also
                    -- populates the CAS bridge (mtCasCF).
                    matched = (mkFlow mid "Iron, ion" Water Nothing){bfCAS = Just "7439-89-6"}
                    tables = buildMethodTables fam cm M.empty [(cfUns, Just (matched, ByCAS)), (cfLt, Nothing)]
                    flowDB = M.singleton fid flow
                pure (loScore (computeLCIAScoreFromTables defaultUnitConfig M.empty flowDB (M.singleton fid 1.0) tables))
            scoreFor = scoreVia M.empty

        it "lets a USEtox wildcard reach river, lake and IMMEDIATE groundwater" $ do
            scoreFor USEtoxFamily "Iron, ion" Nothing "river" `shouldReturn` 2108.5
            scoreFor USEtoxFamily "Iron, ion" Nothing "lake" `shouldReturn` 2108.5
            scoreFor USEtoxFamily "Iron, ion" Nothing "groundwater" `shouldReturn` 2108.5

        it "keeps the method's explicit long-term zero for a USEtox method" $
            scoreFor USEtoxFamily "Iron, ion" Nothing "groundwater, long-term" `shouldReturn` 0.0

        it "blocks the CAS bridge for a long-term groundwater flow under USEtox" $
            -- Name-mismatched flow sharing the CAS: without the gate it would
            -- borrow the immediate 2108.5 and bypass the explicit zero.
            scoreFor USEtoxFamily "Iron(2+)" (Just "7439-89-6") "groundwater, long-term" `shouldReturn` 0.0

        it "keeps every groundwater fallback for a non-USEtox method" $ do
            scoreFor OtherCFFamily "Iron, ion" Nothing "groundwater" `shouldReturn` 2108.5
            scoreFor OtherCFFamily "Iron(2+)" (Just "7439-89-6") "groundwater, long-term" `shouldReturn` 2108.5

        it "lands the ecoinvent spellings on the same gate (compartments.csv)" $ do
            -- "ground-" / "ground-, long-term" are the ecoinvent spellings of
            -- the same emissions. compartments.csv must map the immediate one
            -- to surface water (inherits the wildcard CF, like the SimaPro
            -- spelling) and the long-term one to "groundwater, long-term"
            -- (explicit zero wins, CAS bridge blocked) - otherwise the same
            -- emission scores differently depending on the source database.
            -- Uses the shipped CSV so the mapping itself is pinned.
            scoreVia cmap USEtoxFamily "Iron, ion" Nothing "ground-" `shouldReturn` 2108.5
            scoreVia cmap USEtoxFamily "Iron, ion" Nothing "ground-, long-term" `shouldReturn` 0.0
            scoreVia cmap USEtoxFamily "Iron(2+)" (Just "7439-89-6") "ground-, long-term" `shouldReturn` 0.0
            scoreVia cmap OtherCFFamily "Iron(2+)" (Just "7439-89-6") "ground-, long-term" `shouldReturn` 2108.5

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
                    tables = buildMethodTables OtherCFFamily cmap M.empty [(cf, Nothing)]
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
                tables = buildMethodTables OtherCFFamily M.empty M.empty [(cf, Just (flow, ByUUID))]
                inventory = M.singleton fid 100.0
                flowDB = M.singleton fid flow
                unitDB = M.singleton nil (unitNamed "m")
                (contribs, unknowns) =
                    inventoryContributions defaultUnitConfig unitDB flowDB inventory tables
            unknowns `shouldBe` []
            map (\(_, _, c) -> c) contribs `shouldBe` [0.0]

    describe "convertForCharacterization" $ do
        -- Each row encodes a (flowUnit, cfUnit, qty) → expected mapping under a
        -- specific UnitConfig. Semantic groups: pass-through (units match, or no
        -- flow unit), refuse cross-dimension injection (→ 0), apply the factor
        -- when both units are known, and - when the CF unit is a result
        -- expression unknown to the UnitConfig - normalize the flow to its
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
            fmap bfId (findFlowBySynonym (SynonymSearch synDB byName M.empty) "co2")
                `shouldBe` Just fid

    describe "pickByCompartment M.empty (matchMedium edge cases)" $ do
        it "null medium matches any flow" $ do
            fid <- nextRandom
            let flow = mkFlow fid "co2" Water Nothing
                byName = M.singleton "co2" [flow]
                comp = Compartment "" "" ""
            fmap bfId (findFlowByNameComp M.empty byName "co2" (Just comp)) `shouldBe` Just fid

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
                rule = M.singleton ("urban air", "", "") (Compartment "air" "urban" "")
            fmap bfId (findFlowByNameComp M.empty byName "nox" (Just comp)) `shouldBe` Nothing
            fmap bfId (findFlowByNameComp rule byName "nox" (Just comp)) `shouldBe` Just fid1

    describe "compartmentGapWarning" $ do
        it "names both vocabularies when a factor's name is filed under another medium" $ do
            fid <- nextRandom
            let flow = mkFlow fid "ammonia" Air Nothing
                cf = mkCFComp "ammonia" "urban air" "" 0.747
            compartmentGapWarning M.empty (M.singleton "ammonia" [flow]) [(cf, Nothing)]
                `shouldBe` Just "1 factor(s) name a flow this database files under another compartment (method: \"urban air\"; database: \"air\"). Declare a [[compartment-mappings]] table bridging them."

        it "stays silent when the name is simply absent" $ do
            let cf = mkCFComp "ammonia" "air" "" 0.747
            compartmentGapWarning M.empty M.empty [(cf, Nothing)] `shouldBe` Nothing

        it "stays silent when the database speaks the stated medium, only not for that substance" $ do
            -- Nitrite in water and not in air is the database's inventory,
            -- not its vocabulary: nothing to declare.
            fid1 <- nextRandom
            fid2 <- nextRandom
            let nitrite = mkFlow fid1 "nitrite" Water Nothing
                co2 = mkFlow fid2 "co2" Air Nothing
                byName = M.fromList [("nitrite", [nitrite]), ("co2", [co2])]
                cf = mkCFComp "nitrite" "air" "" 1.0
            compartmentGapWarning M.empty byName [(cf, Nothing)] `shouldBe` Nothing

        it "stays silent for a factor that resolved" $ do
            fid <- nextRandom
            let flow = mkFlow fid "ammonia" Air Nothing
                cf = mkCFComp "ammonia" "air" "" 0.747
            compartmentGapWarning M.empty (M.singleton "ammonia" [flow]) [(cf, Just (flow, ByName))] `shouldBe` Nothing

    describe "fillBroadcastVector + computeLCIAScoreFromTables (Phase 1)" $ do
        let mkUnit uid name = Unit{unitId = uid, unitName = name, unitSymbol = name, unitComment = ""}

        it "scoring with empty broadcast equals scoring with filled broadcast (UUID match)" $ do
            fid <- nextRandom
            uidKg <- nextRandom
            let flow = (mkFlow fid "co2" Air Nothing){bfUnitId = uidKg}
                cf = (mkCF "co2" Nothing 2.5){mcfUnit = "kg"}
                rawTables = buildMethodTables OtherCFFamily M.empty M.empty [(cf, Just (flow, ByUUID))]
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
                tables0 = buildMethodTables OtherCFFamily M.empty M.empty [(cf, Just (flow, ByUUID))]
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
                tables0 = buildMethodTables OtherCFFamily M.empty M.empty [(cf, Just (flow, ByName))]
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
                tables0 = buildMethodTables OtherCFFamily M.empty M.empty [(cf, Just (flow, ByName))]
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
                tables0 = buildMethodTables OtherCFFamily M.empty M.empty [(cf, Just (flowLocal, ByUUID))]
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
        -- kg-denominated CF matched by an m3 flow is refused and scores 0 -
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
                            buildMethodTables OtherCFFamily M.empty densities [(cf, Just (flow, ByUUID))]
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
                tables = buildMethodTables OtherCFFamily M.empty M.empty []
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
                tables = buildMethodTables OtherCFFamily M.empty M.empty []
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
                tables = buildMethodTables OtherCFFamily M.empty M.empty [(cf, Just (flow, ByUUID))]
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

    describe "cfFamily" $ do
        it "classifies USEtox toxicity units, case/whitespace-insensitively" $ do
            cfFamily "CTUe" `shouldBe` USEtoxFamily
            cfFamily "CTUh" `shouldBe` USEtoxFamily
            cfFamily " ctue " `shouldBe` USEtoxFamily
        it "classifies every other unit (incl. unknown/empty) as OtherCFFamily" $ do
            cfFamily "kg P eq" `shouldBe` OtherCFFamily
            cfFamily "unknown" `shouldBe` OtherCFFamily
            cfFamily "" `shouldBe` OtherCFFamily

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
                ctx = MapContext flowDB (byName allFlows) M.empty emptySynonymDB M.empty M.empty M.empty
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
                ctx = MapContext flowDB (byName allFlows) M.empty emptySynonymDB M.empty M.empty M.empty
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
            -- The bridge really does hand the excluded flow back - without this
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
                ctx = MapContext flowDB (byName allFlows) M.empty emptySynonymDB M.empty M.empty M.empty
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
