{-# LANGUAGE OverloadedStrings #-}

{- | EF 3.1 names its generic water resource bare "water" and charges it the
AWARE scarcity of fresh water. Sea water and brine share its CAS number but
stay outside AWARE. With the shipped registry, that row must reach fresh water
by name: a row matched by CAS would arm the CAS bridge, which lends its factor
to every resource flow carrying the CAS, the ocean among them.
-}
module SeaWaterResourceSpec (spec) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Test.Hspec
import UnitConversion (defaultUnitConfig)

import Method.Mapping (MapContext (..), MethodTables, buildMethodTables, cfValue, lookupCFForFlow, mapMethodFlows, mtCasCF, mtRegionalCasCF)
import Method.Types (Compartment (..), FlowDirection (..), Method (..), MethodCF (..))
import SubstanceRegistry (CASNumber (..))
import SynonymDB (SynonymDB, buildFromCSV, normalizeName)
import Types (BiosphereFlow (..), FlowsByCAS (..), FlowsByName (..), Medium (..))
import qualified Types as VT

mkUUID :: Integer -> UUID
mkUUID n = UUID.fromWords64 (fromIntegral n) 0

water :: Maybe Text
water = Just "7732-18-5"

mkFlow :: Integer -> Text -> BiosphereFlow
mkFlow i name =
    BiosphereFlow
        { bfId = mkUUID i
        , bfName = name
        , bfUnitId = mkUUID 0
        , bfSynonyms = M.empty
        , bfCAS = water
        , bfSubstanceId = Nothing
        , bfCompartment = Just (VT.Compartment NaturalResource Nothing)
        }

fresh, ocean, sole, river :: BiosphereFlow
fresh = mkFlow 1 "Water, unspecified natural origin"
ocean = mkFlow 2 "Water, salt, ocean"
sole = mkFlow 3 "Water, salt, sole"
river = mkFlow 4 "Water, river"

-- | The generic water resource row, once worldwide and once for a region.
waterRow :: Maybe Text -> Double -> MethodCF
waterRow loc val =
    MethodCF
        { mcfFlowRef = mkUUID 100
        , mcfFlowName = "water"
        , mcfDirection = Input
        , mcfValue = val
        , mcfCompartment = Just (Compartment "resource" "" "")
        , mcfCAS = water
        , mcfUnit = "m3"
        , mcfConsumerLocation = loc
        }

tablesFor :: SynonymDB -> [BiosphereFlow] -> IO MethodTables
tablesFor synDB flows = buildMethodTables mempty defaultUnitConfig M.empty <$> mapMethodFlows context method
  where
    context =
        MapContext
            { mcBioFlowsByUUID = M.fromList [(bfId f, f) | f <- flows]
            , mcBioFlowsByName = FlowsByName $ M.fromListWith (++) [(normalizeName (bfName f), [f]) | f <- flows]
            , mcBioFlowsByCAS = FlowsByCAS $ M.fromListWith (++) [(cas, [f]) | f <- flows, Just cas <- [bfCAS f]]
            , mcSynonymDB = synDB
            , mcActivities = M.empty
            , mcPlacing = mempty
            , mcSynGroupFlows = M.empty
            }
    method =
        Method
            { methodId = mkUUID 200
            , methodName = "Water use"
            , methodDescription = Nothing
            , methodUnit = "m3 world eq. deprived"
            , methodCategory = "Water use"
            , methodMethodology = Nothing
            , methodFactors = [waterRow Nothing 42.95, waterRow (Just "FR") 10.0]
            }

factorOf :: MethodTables -> BiosphereFlow -> Maybe Double
factorOf tables f = cfValue <$> lookupCFForFlow tables (bfId f) (Just f)

-- | The water factors the CAS bridges would lend, worldwide and by region.
bridgedWater :: MethodTables -> [CASNumber]
bridgedWater tables =
    [cas | ((cas, _, _), Just _) <- M.toList (mtCasCF tables), cas == water']
        ++ [cas | ((cas, _, _), located) <- M.toList (mtRegionalCasCF tables), cas == water', not (M.null located)]
  where
    water' = CASNumber "7732-18-5"

spec :: Spec
spec = describe "EF's generic water resource with the shipped registry" $ do
    parsed <- runIO (buildFromCSV <$> BL.readFile "data/flows.csv")
    case parsed of
        Left err -> it "parses data/flows.csv" $ expectationFailure err
        Right registry -> do
            it "charges fresh water and neither sea water nor brine" $ do
                tables <- tablesFor registry [fresh, ocean, sole]
                map (factorOf tables) [fresh, ocean, sole] `shouldBe` [Just 42.95, Nothing, Nothing]
                bridgedWater tables `shouldBe` []

            -- With no fresh water to name, the line finds no flow at all: a
            -- line matched on any of these by CAS would lend its factor to all.
            it "leaves the line unmatched where no flow carries the fresh-water name" $ do
                tables <- tablesFor registry [river, ocean, sole]
                map (factorOf tables) [river, ocean, sole] `shouldBe` [Nothing, Nothing, Nothing]
                bridgedWater tables `shouldBe` []
