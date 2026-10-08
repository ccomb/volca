{-# LANGUAGE OverloadedStrings #-}

{- | Every factor the loaded collections give one flow.

The sample database's product D emits fossil carbon dioxide. Two collections
are loaded beside it: one whose climate method gives that flow a factor, whose
energy method writes one in a unit the amount cannot reach, and whose water
method says nothing about it; and a second holding only the water method. The
specs pin which side of the answer each method lands on, on both surfaces.
-}
module FlowFactorsSpec (spec) where

import Control.Concurrent.STM (atomically, modifyTVar')
import Data.Aeson (Value (..))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Servant (ServerError, errBody, errHTTPCode, runHandler)
import Test.Hspec

import API.MCP (callTool, noRequestId)
import API.Routes (flowFactorsHandler)
import API.Types (CollectionFactors (..), ExplainCFResult (..), FlowFactorsResult (..), MethodSummary (..))
import App.Env (AppEnv (..), AppM, runApp)
import Config (defaultConfig)
import Database.Manager (CachePolicy (..), DatabaseManager (..), addDatabase, getMergedFlowMetadata, initDatabaseManager, loadDatabase)
import FlowContributionSpec (payloadOf, sampleConfig)
import Method.Types (Compartment (..), FlowDirection (..), Method (..), MethodCF (..), MethodCollection (..))
import Types (BiosphereFlow (..))

methodNamed :: Integer -> Text -> Text -> [MethodCF] -> Method
methodNamed n name unit factors =
    Method
        { methodId = UUID.fromWords 0x464c4f57 0 0 (fromInteger n)
        , methodName = name
        , methodDescription = Nothing
        , methodUnit = unit
        , methodCategory = name
        , methodMethodology = Nothing
        , methodFactors = factors
        }

-- | A factor on the sample's carbon dioxide, by name, in the unit given.
carbonDioxideIn :: Text -> Double -> MethodCF
carbonDioxideIn unit value =
    MethodCF
        { mcfFlowRef = UUID.fromWords 0x464c4f57 0 1 0
        , mcfFlowName = "Carbon dioxide, fossil"
        , mcfDirection = Output
        , mcfValue = value
        , mcfCompartment = Just (Compartment "air" "unspecified" "")
        , mcfCAS = Nothing
        , mcfUnit = unit
        , mcfConsumerLocation = Nothing
        }

climate, energy, water :: Method
climate = methodNamed 1 "Climate change" "kg CO2 eq" [carbonDioxideIn "kg" 27]
-- Charged per MJ: a mass cannot be carried onto it without an energy content.
energy = methodNamed 2 "Energy" "MJ eq" [carbonDioxideIn "MJ" 3]
water =
    methodNamed
        3
        "Water use"
        "m3 world eq"
        [(carbonDioxideIn "m3" 1){mcfFlowRef = UUID.fromWords 0x464c4f57 0 2 0, mcfFlowName = "Water, river", mcfCompartment = Just (Compartment "natural resource" "in water" "")}]

loadedManager :: IO DatabaseManager
loadedManager = do
    manager <- initDatabaseManager defaultConfig NoCache
    addDatabase manager sampleConfig
    loadDatabase manager "sample" >>= either (fail . T.unpack) (const (pure ()))
    atomically $
        modifyTVar' (dmLoadedMethods manager) $
            M.insert "broad" (MethodCollection [climate, energy, water] [] [])
                . M.insert "water-only" (MethodCollection [water] [] [])
    pure manager

-- | The sample's carbon dioxide, as the database names it.
carbonDioxide :: DatabaseManager -> IO UUID
carbonDioxide manager = do
    (flows, _) <- getMergedFlowMetadata manager
    case [bfId f | f <- M.elems flows, bfName f == "Carbon dioxide, fossil"] of
        [fid] -> pure fid
        found -> fail ("expected one carbon dioxide flow, found " <> show (length found))

run :: DatabaseManager -> AppM a -> IO (Either ServerError a)
run manager =
    runHandler . runApp env
  where
    env =
        AppEnv
            { aeDbManager = manager
            , aeMaxTreeDepth = 5
            , aePassword = Nothing
            , aeHostingConfig = Nothing
            , aeClassificationPresets = []
            , aeDataVersion = Nothing
            , aeUsageLog = Nothing
            , aeReader = Nothing
            }

-- | Ask the route about carbon dioxide, failing the test on an error status.
factorsOfCarbonDioxide :: Maybe Text -> IO FlowFactorsResult
factorsOfCarbonDioxide mCollection = do
    manager <- loadedManager
    fid <- carbonDioxide manager
    run manager (flowFactorsHandler "sample" (UUID.toText fid) mCollection)
        >>= either (\e -> fail (show (errHTTPCode e) <> ": " <> BSL.unpack (errBody e))) pure

{- | Each of this spec's collections, by side: (collection, [(method, outcome)],
[method]). The engine also loads its built-in collections, which are not what
these specs are about.
-}
sides :: FlowFactorsResult -> [(Text, [(Text, Text)], [Text])]
sides result =
    [ (cfcCollection c, [(ecrMethod f, ecrOutcome f) | f <- cfcFactors c], map msmName (cfcNoFactor c))
    | c <- ffrCollections result
    , cfcCollection c `elem` ["broad", "water-only"]
    ]

spec :: Spec
spec = describe "the factors of one flow" $ do
    it "list, in every loaded collection, the methods that reach it and name those that do not" $
        fmap sides (factorsOfCarbonDioxide Nothing)
            `shouldReturn` [ ("broad", [("Climate change", "characterized"), ("Energy", "conversion_refused")], ["Water use"])
                           , ("water-only", [], ["Water use"])
                           ]

    it "carry the factor and the method to ask about it again" $ do
        result <- factorsOfCarbonDioxide (Just "broad")
        [(ecrMethodId f, ecrMethodUnit f) | c <- ffrCollections result, f <- cfcFactors c, ecrOutcome f == "characterized"]
            `shouldBe` [(methodId climate, "kg CO2 eq")]

    it "answer one collection alone when it is named" $
        fmap (map cfcCollection . ffrCollections) (factorsOfCarbonDioxide (Just "water-only"))
            `shouldReturn` ["water-only"]

    it "refuse a collection that is not loaded" $ do
        manager <- loadedManager
        fid <- carbonDioxide manager
        fmap (either (Just . errHTTPCode) (const Nothing)) (run manager (flowFactorsHandler "sample" (UUID.toText fid) (Just "absent")))
            `shouldReturn` Just 404

    it "refuse a flow the database does not hold" $ do
        manager <- loadedManager
        fmap (either (Just . errHTTPCode) (const Nothing)) (run manager (flowFactorsHandler "sample" (UUID.toText UUID.nil) Nothing))
            `shouldReturn` Just 404

    it "are served by the MCP tool on the same sides, linked to the flow search" $ do
        manager <- loadedManager
        fid <- carbonDioxide manager
        reply <-
            callTool manager [] Nothing (Just "http://ui") noRequestId "get_flow_factors" $
                KM.fromList [("database", String "sample"), ("flow_id", String (UUID.toText fid)), ("collection", String "water-only")]
        payload <- maybe (fail ("unexpected reply: " <> show reply)) pure (payloadOf reply)
        (KM.lookup "web_url" payload, noFactorOf payload)
            `shouldBe` (Just (String "http://ui/db/sample/flows?q=Carbon%20dioxide%2C%20fossil"), Just ["Water use"])
  where
    noFactorOf :: KM.KeyMap Value -> Maybe [Text]
    noFactorOf payload = do
        Array collections <- KM.lookup "collections" payload
        [Object c] <- Just (toList collections)
        Array missed <- KM.lookup "noFactor" c
        traverse (\v -> do Object m <- Just v; String n <- KM.lookup "name" m; pure n) (toList missed)
