{-# LANGUAGE OverloadedStrings #-}

{- | Where a characterised flow's two numbers land.

Scoring a flow yields two numbers of the same type: the characterisation factor
the method gives it, and the contribution that factor makes once multiplied by
the inventory amount. Every answer that lists contributing flows publishes
both, and publishing one in the other's place would compile and serialise just
as well. These specs pin each surface on a fixture where the two differ: the
sample database's product D emits 2 kg of fossil carbon dioxide and the method
gives that flow a factor of 27, so the factor is 27 and the contribution 54.
-}
module FlowContributionSpec (spec) where

import Control.Concurrent.STM (atomically, modifyTVar')
import Data.Aeson (Value (..), decodeStrict)
import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.UUID as UUID
import Servant (errBody, errHTTPCode, runHandler)
import Test.Hspec

import API.MCP (callTool)
import API.Routes (getActivityLCIA, getContributingFlows, postImpactsBatch)
import API.Types (
    BatchImpactsEntry (..),
    BatchImpactsRequest (..),
    BatchImpactsResponse (..),
    ContributingFlowsResult (..),
    FlowContributionEntry (..),
    LCIABatchResult (..),
    LCIAResult (..),
 )
import App.Env (AppEnv (..), AppM, runApp)
import Config (DatabaseConfig (..), defaultConfig)
import Database.Manager (CachePolicy (..), CollectionName (..), DatabaseManager (..), addDatabase, initDatabaseManager, loadDatabase)
import Method.Types (Compartment (..), FlowDirection (..), Method (..), MethodCF (..), MethodCollection (..))
import Types (AllocationKey (..), GeographyPolicy (..))

-- | The factor and the contribution, in that order, as every surface reports them.
type FactorAndContribution = (Double, Double)

-- | What each surface must report for the carbon dioxide product D emits.
expected :: [FactorAndContribution]
expected = [(27, 54)]

-- | Product D, whose only elementary exchange is 2 kg of fossil carbon dioxide.
productD :: Text
productD = "dd000004-0000-0000-0000-000000000000"

collectionName :: Text
collectionName = "contribution-test"

climateChange :: Method
climateChange =
    Method
        { methodId = UUID.fromWords 0x464c4f57 0 0 1
        , methodName = "Climate change"
        , methodDescription = Nothing
        , methodUnit = "kg CO2 eq"
        , methodCategory = "Climate change"
        , methodMethodology = Nothing
        , methodFactors =
            [ MethodCF
                { mcfFlowRef = UUID.fromWords 0x464c4f57 0 0 2
                , mcfFlowName = "Carbon dioxide, fossil"
                , mcfDirection = Output
                , mcfValue = 27
                , mcfCompartment = Just (Compartment "air" "unspecified" "")
                , mcfCAS = Nothing
                , mcfUnit = "kg"
                , mcfConsumerLocation = Nothing
                }
            ]
        }

methodIdText :: Text
methodIdText = UUID.toText (methodId climateChange)

-- | The four-activity fixture, as a database the manager can load.
sampleConfig :: DatabaseConfig
sampleConfig =
    DatabaseConfig
        { dcName = "sample"
        , dcDisplayName = "sample"
        , dcPath = "test-data/SAMPLE.min"
        , dcDescription = Nothing
        , dcLoad = False
        , dcDefault = False
        , dcDepends = []
        , dcLocationAliases = M.empty
        , dcFormat = Nothing
        , dcIsUploaded = False
        , dcDeletable = False
        , dcGeographyPolicy = GeoGlobal
        , dcAllocation = Declared
        , dcPatches = []
        , dcSource = Nothing
        }

-- | A manager holding the sample database and the one-method collection.
loadedManager :: IO DatabaseManager
loadedManager = do
    manager <- initDatabaseManager defaultConfig NoCache
    addDatabase manager sampleConfig
    loadDatabase manager "sample" >>= either (fail . T.unpack) (const (pure ()))
    atomically $
        modifyTVar' (dmLoadedMethods manager) $
            M.insert collectionName (MethodCollection [climateChange] [] [] [])
    pure manager

-- | Run a REST handler against that manager, failing the test on an error status.
runOk :: AppM a -> IO a
runOk handler = do
    manager <- loadedManager
    let env =
            AppEnv
                { aeDbManager = manager
                , aeMaxTreeDepth = 5
                , aePassword = Nothing
                , aeHostingConfig = Nothing
                , aeClassificationPresets = []
                , aeDataVersion = Nothing
                }
    runHandler (runApp env handler)
        >>= either (\e -> fail (show (errHTTPCode e) <> ": " <> BSL.unpack (errBody e))) pure

-- | The two numbers of every row a REST answer lists.
restRows :: [FlowContributionEntry] -> [FactorAndContribution]
restRows entries = [(fcoCfValue e, fcoContribution e) | e <- entries]

-- | Call an MCP tool on product D, and read the two numbers of every @top_flows@ row.
mcpRows :: Text -> [(Key, Value)] -> IO [FactorAndContribution]
mcpRows tool extraArgs = do
    manager <- loadedManager
    reply <-
        callTool manager [] Nothing Nothing Null tool $
            KM.fromList $
                [ ("database", String "sample")
                , ("process_id", String productD)
                , ("method_id", String methodIdText)
                , ("collection", String collectionName)
                ]
                    ++ extraArgs
    maybe (fail ("unexpected " <> T.unpack tool <> " reply: " <> show reply)) pure (topFlows reply)
  where
    topFlows :: Value -> Maybe [FactorAndContribution]
    topFlows reply = do
        Object o <- Just reply
        Object r <- KM.lookup "result" o
        Array content <- KM.lookup "content" r
        Object c <- listToMaybe (toList content)
        String text <- KM.lookup "text" c
        Object payload <- decodeStrict (encodeUtf8 text)
        Array rows <- KM.lookup "top_flows" payload
        traverse row (toList rows)
    row :: Value -> Maybe FactorAndContribution
    row v = do
        Object o <- Just v
        Number factor <- KM.lookup "cf_value" o
        Number contribution <- KM.lookup "contribution" o
        pure (realToFrac factor, realToFrac contribution)

spec :: Spec
spec = describe "a contributing flow's factor and contribution" $ do
    it "land in their own fields in the REST contributing flows" $ do
        result <- runOk (getContributingFlows "sample" productD (CollectionName collectionName) methodIdText Nothing Nothing)
        restRows (cfrTopFlows result) `shouldBe` expected

    it "land in their own fields in a REST single-category score" $ do
        result <- runOk (getActivityLCIA "sample" productD (CollectionName collectionName) methodIdText Nothing)
        restRows (lrTopContributors result) `shouldBe` expected

    it "land in their own fields in a REST batch score asking for top flows" $ do
        response <- runOk (postImpactsBatch "sample" (CollectionName collectionName) (Just 5) Nothing (BatchImpactsRequest [productD]))
        concatMap restRows [lrTopContributors r | e <- birResults response, r <- lbrResults (bieImpacts e)]
            `shouldBe` expected

    it "land in their own fields in the MCP get_contributing_flows reply" $
        mcpRows "get_contributing_flows" [] `shouldReturn` expected

    it "land in their own fields in the MCP get_impacts reply" $
        mcpRows "get_impacts" [("top_flows", Number 5)] `shouldReturn` expected
