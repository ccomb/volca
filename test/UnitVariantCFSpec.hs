{-# LANGUAGE OverloadedStrings #-}

{- | A SimaPro name can bake the flow's unit into the name ("Gas, natural\/m3").
'normalizeName' strips that suffix so a lone variant can borrow its base
resource's CF - but when the method itself ships one row PER unit variant
(\/kg 43.1 vs \/m3 34.5: same substance, different densities), the collapsed
name key crowns a single winner and the losing variant's flow reads a
dimensionally incompatible CF, whose unit conversion silently zeroes the score.

'mtUnitVariantCF' keys those rows by their full (suffix-preserving) name so
each suffixed flow finds the row declared in its own unit.
-}
module UnitVariantCFSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Test.Hspec

import Method.Mapping (MatchStrategy (..), MethodTables, buildMethodTables, cfValue, lookupCFForFlow)
import Method.Types (CFFamily (..), Compartment (..), FlowDirection (..), MethodCF (..))
import Types (
    BiosphereFlow (..),
    Medium (..),
 )
import qualified Types as VT

mkUUID :: Integer -> UUID
mkUUID n = UUID.fromWords64 (fromIntegral n) 0

mkCF :: Integer -> Text -> Text -> Double -> MethodCF
mkCF ref name unit val =
    MethodCF
        { mcfFlowRef = mkUUID ref
        , mcfFlowName = name
        , mcfDirection = Input
        , mcfValue = val
        , mcfCompartment = Just (Compartment "resource" "" "")
        , mcfCAS = Nothing
        , mcfUnit = unit
        , mcfConsumerLocation = Nothing
        }

-- | Move a row to a named subcompartment ('mkCF' writes the unspecified one).
inSub :: Text -> MethodCF -> MethodCF
inSub sub cf = cf{mcfCompartment = Just (Compartment "resource" sub "")}

mkFlow :: Integer -> Text -> BiosphereFlow
mkFlow i name = mkFlowAt i name Nothing

-- | 'mkFlow' emitted at a named subcompartment.
mkFlowAt :: Integer -> Text -> Maybe Text -> BiosphereFlow
mkFlowAt i name sub =
    BiosphereFlow
        { bfId = mkUUID i
        , bfName = name
        , bfUnitId = mkUUID 0
        , bfSynonyms = M.empty
        , bfCAS = Nothing
        , bfSubstanceId = Nothing
        , bfCompartment = Just (VT.Compartment NaturalResource sub)
        }

lookupFor :: MethodTables -> BiosphereFlow -> Maybe Double
lookupFor tables flow = cfValue <$> lookupCFForFlow tables (bfId flow) (Just flow)

kgFlow, m3Flow :: BiosphereFlow
kgFlow = mkFlow 1 "Gas, natural/kg"
m3Flow = mkFlow 2 "Gas, natural/m3"

-- The method ships one row per unit variant. The /kg row UUID-matched its flow;
-- the /m3 row name-matched - after suffix collapse both compete for the one
-- "gas natural" name key, which is exactly the failure this spec pins.
perUnitTables :: MethodTables
perUnitTables =
    buildMethodTables
        OtherCFFamily
        M.empty
        M.empty
        [ (mkCF 1 "Gas, natural/kg" "kg" 43.1, Just (kgFlow, ByUUID))
        , (mkCF 20 "Gas, natural/m3" "m3" 34.5, Just (m3Flow, ByName))
        ]

spec :: Spec
spec = describe "per-unit method rows (unit-suffixed homonyms)" $ do
    it "serves each unit variant the row declared in its own unit" $ do
        lookupFor perUnitTables m3Flow `shouldBe` Just 34.5
        lookupFor perUnitTables kgFlow `shouldBe` Just 43.1

    it "still resolves a bare (unsuffixed) flow through the collapsed name key" $
        -- No behavior change outside the suffixed variants: the bare name never
        -- keys into 'mtUnitVariantCF' and keeps today's collapsed-key winner.
        lookupFor perUnitTables (mkFlow 3 "Gas, natural") `shouldBe` Just 43.1

    it "keeps the base-row ride for a variant the method has no row for" $ do
        -- Method knows only the base substance: a suffixed flow still borrows
        -- its CF through the suffix-stripping collapse (the reason the strip
        -- exists) - the variant table must not get in the way.
        let baseOnly =
                buildMethodTables
                    OtherCFFamily
                    M.empty
                    M.empty
                    [(mkCF 1 "Gas, natural" "m3" 40.0, Just (mkFlow 1 "Gas, natural", ByUUID))]
        lookupFor baseOnly (mkFlow 4 "Gas, natural/Sm3") `shouldBe` Just 40.0

    it "refuses a variant name whose own rows disagree (true duplicate, never guesses)" $ do
        let dup =
                buildMethodTables
                    OtherCFFamily
                    M.empty
                    M.empty
                    [ (mkCF 1 "Gas, natural/kg" "kg" 10.0, Just (kgFlow, ByUUID))
                    , (mkCF 2 "Gas, natural/kg" "kg" 20.0, Nothing)
                    ]
        -- The variant rung refuses; the collapsed key still answers with its
        -- winner, exactly as before this table existed. The probe flow's UUID
        -- matches no row, so the answer comes from the name cascade, not
        -- 'mtUuidCF'.
        lookupFor dup (mkFlow 99 "Gas, natural/kg") `shouldBe` Just 10.0

    it "outranks a sub-exact row keyed by the collapsed name" $ do
        -- The precedence this rung buys, and its price. The method writes the
        -- /m3 factor at no particular subcompartment and a DIFFERENT factor for
        -- the bare name at "in water"; the flow is /m3 emitted in water, so the
        -- two rows key apart and 'agreedValue' never sees the disagreement.
        -- Before this table the sub-exact row answered - right subcompartment,
        -- wrong unit, hence 0 after conversion. The unit-matched row wins now:
        -- a factor that scores beats one that cannot.
        let subExact =
                buildMethodTables
                    OtherCFFamily
                    M.empty
                    M.empty
                    [ (mkCF 1 "Gas, natural/m3" "m3" 34.5, Nothing)
                    , (inSub "in water" (mkCF 2 "Gas, natural" "kg" 43.1), Nothing)
                    ]
        lookupFor subExact (mkFlowAt 5 "Gas, natural/m3" (Just "in water")) `shouldBe` Just 34.5

    it "stays silent for a subcompartment no medium-level row may reach" $ do
        -- Sub-blind like its siblings, so it takes the same gate: an ocean
        -- emission is a foreign medium and must not borrow the freshwater
        -- factor, unit-matched or not.
        --
        -- The gate applies because this method names the sea somewhere - one
        -- row, for another substance, is enough. Deliberate: a method that
        -- distinguishes the sea at all is trusted to have meant its silence on
        -- the substances it left out, and the engine does not extrapolate for
        -- it.
        let oceanic =
                buildMethodTables
                    OtherCFFamily
                    M.empty
                    M.empty
                    [ (mkCF 1 "Water/m3" "m3" 42.95, Nothing)
                    , (inSub "ocean" (mkCF 2 "Water, salt" "m3" 0.0), Nothing)
                    ]
        lookupFor oceanic (mkFlowAt 6 "Water/m3" (Just "ocean")) `shouldBe` Nothing

    it "reaches the sea when the method never names it" $ do
        -- The same row, from a method with no sea-water row anywhere. Its
        -- medium-level factor is all it has to say, and withholding it would
        -- score the emission as zero on an authority the method never gave.
        let silent =
                buildMethodTables
                    OtherCFFamily
                    M.empty
                    M.empty
                    [(mkCF 1 "Water/m3" "m3" 42.95, Nothing)]
        lookupFor silent (mkFlowAt 6 "Water/m3" (Just "ocean")) `shouldBe` Just 42.95
