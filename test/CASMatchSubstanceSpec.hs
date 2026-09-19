{-# LANGUAGE OverloadedStrings #-}

{- | A CAS number names a molecule, not the substance a method characterizes.
EF splits methane into fossil, biogenic and land transformation under one CAS,
and the flow registry keeps the three apart. A factor row whose own name finds
no flow must not reach a flow the registry files as another substance through
the CAS number they share: neither directly, nor through the CAS bridge that a
CAS match arms. This goes through 'mapMethodFlows' and the scoring tables, the
path a database takes, so what it pins is the factor a flow ends up with.
-}
module CASMatchSubstanceSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Test.Hspec

import Method.Mapping (MapContext (..), buildMethodTables, cfValue, lookupCFForFlow, mapMethodFlows)
import Method.Types (CFFamily (..), Compartment (..), FlowDirection (..), Method (..), MethodCF (..))
import SynonymDB (SynonymDB, buildFromPairs, emptySynonymDB, normalizeName)
import Types (BiosphereFlow (..), Medium (..))
import qualified Types as VT

mkUUID :: Integer -> UUID
mkUUID n = UUID.fromWords64 (fromIntegral n) 0

methane :: Maybe Text
methane = Just "74-82-8"

mkFlow :: Integer -> Text -> BiosphereFlow
mkFlow i name =
    BiosphereFlow
        { bfId = mkUUID i
        , bfName = name
        , bfUnitId = mkUUID 0
        , bfSynonyms = M.empty
        , bfCAS = methane
        , bfSubstanceId = Nothing
        , bfCompartment = Just (VT.Compartment Air Nothing)
        }

-- | The method's only methane row in its land use sub-category.
landTransformation :: MethodCF
landTransformation =
    MethodCF
        { mcfFlowRef = mkUUID 100
        , mcfFlowName = "Methane, land transformation"
        , mcfDirection = Output
        , mcfValue = 29.8
        , mcfCompartment = Just (Compartment "air" "" "")
        , mcfCAS = methane
        , mcfUnit = "kg"
        , mcfConsumerLocation = Nothing
        }

-- | The three methane substances, as the shipped registry groups them.
registry :: SynonymDB
registry =
    buildFromPairs
        [ ("Methane", "Methane, fossil")
        , ("Methane, biogenic", "Methane, non-fossil")
        , ("Methane, from soil or biomass stock", "Methane, land transformation")
        ]

fossil, biogenic :: BiosphereFlow
fossil = mkFlow 1 "Methane, fossil"
biogenic = mkFlow 2 "Methane, biogenic"

{- | The factor each flow of a database ends up with, when a method holding only
the land transformation row is mapped onto it.
-}
factors :: SynonymDB -> [BiosphereFlow] -> IO [Maybe Double]
factors synDB flows = do
    mappings <- mapMethodFlows context method
    let tables = buildMethodTables OtherCFFamily mempty M.empty mappings
    pure [cfValue <$> lookupCFForFlow tables (bfId f) (Just f) | f <- flows]
  where
    context =
        MapContext
            { mcBioFlowsByUUID = M.fromList [(bfId f, f) | f <- flows]
            , mcBioFlowsByName = M.fromListWith (++) [(normalizeName (bfName f), [f]) | f <- flows]
            , mcBioFlowsByCAS = M.fromListWith (++) [(cas, [f]) | f <- flows, Just cas <- [bfCAS f]]
            , mcSynonymDB = synDB
            , mcActivities = M.empty
            , mcCompartmentMap = mempty
            , mcSynGroupFlows = M.empty
            }
    method =
        Method
            { methodId = mkUUID 200
            , methodName = "Climate change - Land use and LU change"
            , methodDescription = Nothing
            , methodUnit = "kg CO2 eq"
            , methodCategory = "Climate change - Land use and LU change"
            , methodMethodology = Nothing
            , methodFactors = [landTransformation]
            }

spec :: Spec
spec = describe "CAS match within a registry substance" $ do
    it "charges neither fossil nor biogenic methane the land transformation factor" $
        factors registry [fossil, biogenic] `shouldReturn` [Nothing, Nothing]

    it "still matches by CAS a flow whose name the registry does not know" $
        factors registry [mkFlow 3 "Methane, from peat"] `shouldReturn` [Just 29.8]

    it "still matches by CAS when the registry knows neither name" $
        factors emptySynonymDB [fossil] `shouldReturn` [Just 29.8]
