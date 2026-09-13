{-# LANGUAGE OverloadedStrings #-}

{- | Excluding delayed long-term emissions from a score.

Long-term emissions (heavy metals leaching from a landfill over > 100 yr, etc.)
are flagged by a sub-compartment carrying the "long-term" marker. When a caller
asks to exclude them, 'excludeLongTermFlows' drops exactly those inventory flows
before characterization, so the score is computed as if the delayed emissions
were out of scope.

These fixtures drive the real table build + broadcast fill + scoring, so they
track the engine's actual behaviour rather than a re-implementation:
  * the long-term flow is dropped, the immediate one kept;
  * a flow absent from the FlowDB is kept, never silently dropped (an unknown
    UUID is not evidence of long-term);
  * the "long-term" marker is matched case-insensitively across spellings;
  * scoring the filtered inventory drops by exactly the long-term flow's
    contribution – the immediate flow's score is untouched.
-}
module LongTermExclusionSpec (spec) where

import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Either (fromRight)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Test.Hspec

import Matrix (Inventory)
import Method.Mapping
import Method.Types (Compartment (..), FlowDirection (..), MethodCF (..))
import Types (
    BioFlowDB,
    BiosphereFlow (..),
    Medium (..),
    Unit (..),
    UnitDB,
 )
import qualified Types as VT
import UnitConversion (UnitConfig, buildFromCSV, defaultUnitConfig)

-- ---------------------------------------------------------------------------
-- Unit metadata: a UnitConfig + UnitDB that know kg (CFs and flows share it,
-- so unit conversion is the identity and the arithmetic stays transparent).
-- ---------------------------------------------------------------------------

unitConfig :: UnitConfig
unitConfig =
    fromRight defaultUnitConfig $
        buildFromCSV (BLC.pack "name,dimension,factor\nkg,mass,1.0\n")

uidKg :: UUID
uidKg = UUID.fromWords64 1 0

unitDB :: UnitDB
unitDB = M.singleton uidKg (Unit{unitId = uidKg, unitName = "kg", unitSymbol = "kg", unitComment = ""})

-- ---------------------------------------------------------------------------
-- Flow + CF fixtures: the same substance emitted to water, once as a delayed
-- long-term emission and once immediately.
-- ---------------------------------------------------------------------------

ltId, stId, ghostId :: UUID
ltId = UUID.fromWords64 100 0
stId = UUID.fromWords64 101 0
ghostId = UUID.fromWords64 999 0 -- referenced by the inventory but absent from the FlowDB

bioFlow :: UUID -> Maybe Text -> BiosphereFlow
bioFlow fid sub =
    BiosphereFlow
        { bfId = fid
        , bfName = "Cadmium"
        , bfUnitId = uidKg
        , bfSynonyms = M.empty
        , bfCAS = Nothing
        , bfSubstanceId = Nothing
        , bfCompartment = Just (VT.Compartment Water sub)
        }

ltFlow, stFlow :: BiosphereFlow
ltFlow = bioFlow ltId (Just "groundwater, long-term")
stFlow = bioFlow stId (Just "river")

flowDB :: BioFlowDB
flowDB = M.fromList [(ltId, ltFlow), (stId, stFlow)]

-- Inventory: 2 kg of the long-term emission, 5 kg of the immediate one, plus a
-- flow the FlowDB does not know about.
inventory :: Inventory
inventory = M.fromList [(ltId, 2.0), (stId, 5.0), (ghostId, 7.0)]

-- A unit-CF (1.0 per kg) matched to a flow by UUID.
cfFor :: BiosphereFlow -> MethodCF
cfFor flow =
    MethodCF
        { mcfFlowRef = bfId flow
        , mcfFlowName = bfName flow
        , mcfDirection = Output
        , mcfValue = 1.0
        , mcfCompartment = Just (Compartment "water" "" "")
        , mcfCAS = Nothing
        , mcfUnit = "kg"
        , mcfConsumerLocation = Nothing
        }

-- Tables (with broadcast) that characterize both flows at 1.0 per kg.
tables :: MethodTables
tables =
    fillBroadcastVector unitConfig unitDB flowDB $
        buildMethodTables OtherCFFamily M.empty M.empty [(cfFor ltFlow, Just (ltFlow, ByUUID)), (cfFor stFlow, Just (stFlow, ByUUID))]

scoreOf :: Inventory -> Double
scoreOf inv = loScore (computeLCIAScoreFromTables unitConfig unitDB flowDB inv tables)

near :: Double -> Double -> Bool
near expected v = abs (v - expected) < 1e-9

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
    describe "excludeLongTermFlows" $ do
        it "drops the long-term flow and keeps the immediate one" $
            M.keys (excludeLongTermFlows flowDB (M.fromList [(ltId, 2.0), (stId, 5.0)]))
                `shouldBe` [stId]

        it "keeps a flow absent from the FlowDB (an unknown UUID is not long-term)" $
            M.keysSet (excludeLongTermFlows flowDB inventory)
                `shouldBe` M.keysSet (M.fromList [(stId, 5.0 :: Double), (ghostId, 7.0)])

        it "matches the long-term marker case-insensitively and across spellings" $ do
            let variants =
                    [ "Groundwater, LONG-TERM"
                    , "low. pop., long term"
                    , "ground-, long-term"
                    ]
                oneFlow sub = M.singleton ltId (bioFlow ltId (Just sub))
            mapM_
                (\sub -> M.null (excludeLongTermFlows (oneFlow sub) (M.singleton ltId 1.0)) `shouldBe` True)
                variants

    describe "longTermModeFromExclude" $ do
        it "maps the exclude flag to the mode, defaulting to include" $ do
            longTermModeFromExclude True `shouldBe` ExcludeLongTerm
            longTermModeFromExclude False `shouldBe` IncludeLongTerm

    describe "applyLongTermMode" $ do
        it "is the identity under IncludeLongTerm (long-term flows kept)" $
            applyLongTermMode flowDB IncludeLongTerm inventory `shouldBe` inventory

        it "drops long-term flows under ExcludeLongTerm (same as excludeLongTermFlows)" $
            applyLongTermMode flowDB ExcludeLongTerm inventory
                `shouldBe` excludeLongTermFlows flowDB inventory

    describe "scoring with vs without long-term emissions" $ do
        it "full score counts both flows (2·1 + 5·1 = 7)" $
            scoreOf (M.fromList [(ltId, 2.0), (stId, 5.0)]) `shouldSatisfy` near 7.0

        it "excluding long-term drops by exactly the long-term contribution (7 → 5)" $ do
            let full = M.fromList [(ltId, 2.0), (stId, 5.0)]
                cut = excludeLongTermFlows flowDB full
            scoreOf cut `shouldSatisfy` near 5.0
            (scoreOf full - scoreOf cut) `shouldSatisfy` near 2.0
