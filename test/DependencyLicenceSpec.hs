{-# LANGUAGE OverloadedStrings #-}

{- | What an answer reads in a database it reaches through a dependency.

The root emits 1 kg of carbon dioxide (factor 1) and buys one unit of the
dependency's process, which emits 2 kg of methane (factor 10): a score of 21,
20 of it from the dependency. Under a dependency licence keeping what weighs
in its scores, the root's flows and processes stay detailed and the
dependency comes back as one line of 20; under one keeping its amounts, its
contributions stay readable and every answer reading its exchanges is
grouped or refused.
-}
module DependencyLicenceSpec (
    climate,
    collection,
    dependency,
    inventoryKept,
    managerOn,
    referenceOf,
    root,
    rootPid,
    runIn,
    spec,
) where

import Control.Concurrent.STM (atomically, modifyTVar')
import Control.Monad (void)
import Data.Aeson (Object, Value (..))
import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import Servant (errHTTPCode, runHandler)
import Test.Hspec

import API.MCP (callTool, noRequestId)
import API.Routes (batchedScoresFor, getActivityInventory, getActivitySupplyChain)
import API.Types (SupplyChainEntry (..), SupplyChainResponse (..), WithheldProcesses (..))
import App.Env (AppEnv (..), AppM, runApp)
import Config (DatabaseConfig (..), defaultConfig)
import qualified Database.Manager as DM
import FlowContributionSpec (payloadOf)
import Impact (LicencedSolution (..), WithheldPart (..), partitionByLicence, scoreSolution)
import Method.Types (FlowDirection (..), Method (..), MethodCF (..), MethodCollection (..))
import qualified SharedSolver as SS
import Types

import CrossDBRegionalLCIAFixture (actUUID, kgUnit, linkAt, mkDB, mkUUID, prodUUID, testFlow)
import DependencyFlowClosureSpec (install)

methane :: BiosphereFlow
methane = testFlow{bfId = mkUUID 502, bfName = "Methane"}

factor :: BiosphereFlow -> Double -> MethodCF
factor flow value =
    MethodCF
        { mcfFlowRef = bfId flow
        , mcfFlowName = bfName flow
        , mcfDirection = Output
        , mcfValue = value
        , mcfCompartment = Nothing
        , mcfCAS = Nothing
        , mcfUnit = unitName kgUnit
        , mcfConsumerLocation = Nothing
        }

climate :: Method
climate =
    Method
        { methodId = mkUUID 7001
        , methodName = "Climate change"
        , methodDescription = Nothing
        , methodUnit = "kg CO2 eq"
        , methodCategory = "Climate change"
        , methodMethodology = Nothing
        , methodFactors = [factor testFlow 1, factor methane 10]
        }

collection :: Text
collection = "dependency-test"

-- | A fixture database whose one process makes one unit of its own product.
producing :: Int -> [(Int, Double)] -> Database
producing offset emissions = db{dbActivities = V.map (\act -> act{exchanges = [referenceOf offset]}) (dbActivities db)}
  where
    db :: Database
    db = mkDB offset ["FR"] emissions

-- | The reference exchange of the fixture process numbered @offset@: one unit of its own product.
referenceOf :: Int -> Exchange
referenceOf offset =
    TechnosphereExchange
        { techFlowId = prodUUID offset
        , techAmount = 1
        , techUnitId = unitId kgUnit
        , techRole = ReferenceProduct
        , techActivityLinkId = Just (actUUID offset)
        , techSupplierClaim = ClaimByProduct
        , techLocation = ""
        , techComment = Nothing
        , techPedigree = Nothing
        , techShare = Nothing
        , techClassification = M.empty
        , techProperties = noProperties
        }

dependency :: Database
dependency =
    (producing 1 [(0, 2.0)])
        { dbBioFlows = M.singleton (bfId methane) methane
        , dbBiosphereOrder = V.singleton (bfId methane)
        }

root :: Database
root = (linkAt (producing 100 [(0, 1.0)]) dependency "dep" 0 1.0){dbDependsOn = ["dep"]}

rootPid :: Text
rootPid = processIdToText root 0

configFor :: Text -> Licence -> DatabaseConfig
configFor name licence =
    DatabaseConfig
        { dcName = name
        , dcDisplayName = name
        , dcPath = ""
        , dcDescription = Nothing
        , dcLoad = True
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
        , dcLicence = licence
        , dcRelease = Nothing
        , dcRequires = []
        }

own :: [Permission] -> Licence
own refused = LicenceOwn OwnLicence{ownText = "Ours", ownRefused = S.fromList refused, ownAttribution = AttributionRequired}

detailKept, inventoryKept :: Licence
detailKept = own [SeeDetailedScores, ReadInventory, Download]
inventoryKept = own [ReadInventory, Download]

-- | Both databases loaded, the dependency under the licence given.
managerWith :: Licence -> IO DM.DatabaseManager
managerWith = managerOn root dependency

-- | The root and the dependency given, loaded, the dependency under the licence given.
managerOn :: Database -> Database -> Licence -> IO DM.DatabaseManager
managerOn rootDb depDb licence = do
    manager <- DM.initDatabaseManager defaultConfig DM.NoCache
    DM.addDatabase manager (configFor "root" LicenceUnstated)
    DM.addDatabase manager (configFor "dep" licence)
    install manager "dep" depDb
    install manager "root" rootDb
    atomically $ modifyTVar' (DM.dmLoadedMethods manager) (M.insert collection (MethodCollection [climate] [] []))
    pure manager

callOn :: DM.DatabaseManager -> Text -> [(Key, Value)] -> IO Value
callOn manager tool extraArgs =
    callTool manager [] Nothing Nothing noRequestId tool $
        KM.fromList $
            [ ("database", String "root")
            , ("process_id", String rootPid)
            , ("method_id", String (UUID.toText (methodId climate)))
            , ("collection", String collection)
            ]
                ++ extraArgs

-- | The payload of a tool's answer, failing on a tool error.
payload :: Value -> IO Object
payload reply = maybe (fail ("unexpected reply: " <> show reply)) pure (payloadOf reply)

errorText :: Value -> Maybe Text
errorText reply = do
    Object o <- Just reply
    Object r <- KM.lookup "result" o
    Bool True <- KM.lookup "isError" r
    Array content <- KM.lookup "content" r
    Object c <- listToMaybe (toList content)
    String text <- KM.lookup "text" c
    pure text

-- | The databases and contributions of the grouped lines of an answer.
groupedLines :: Object -> [(Value, Value)]
groupedLines o =
    [ (database, contribution)
    | Just (Array rows) <- [KM.lookup "withheld_databases" o]
    , Object row <- toList rows
    , Just database <- [KM.lookup "database" row]
    , Just contribution <- [KM.lookup "contribution" row]
    ]

numbersAt :: Key -> Key -> Object -> [Double]
numbersAt list field o =
    [ realToFrac n
    | Just (Array rows) <- [KM.lookup list o]
    , Object row <- toList rows
    , Just (Number n) <- [KM.lookup field row]
    ]

runIn :: DM.DatabaseManager -> AppM a -> IO (Either Int a)
runIn manager handler = either (Left . errHTTPCode) Right <$> runHandler (runApp env handler)
  where
    env :: AppEnv
    env =
        AppEnv
            { aeDbManager = manager
            , aeMaxTreeDepth = 5
            , aePassword = Nothing
            , aeHostingConfig = Nothing
            , aeClassificationPresets = []
            , aeDataVersion = Nothing
            }

supplyChain :: DM.DatabaseManager -> IO (Either Int SupplyChainResponse)
supplyChain manager =
    runIn manager $
        getActivitySupplyChain "root" rootPid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] [] [] Nothing Nothing Nothing

solution :: DM.DatabaseManager -> IO SS.CrossDBSolution
solution manager = do
    unitCfg <- DM.getMergedUnitConfig manager
    Just loaded <- DM.getDatabase manager "root"
    either (fail . T.unpack) pure
        =<< SS.computeInventoryMatrixWithDepsCached unitCfg (DM.mkDepSolverLookup manager) root "root" (DM.ldSharedSolver loaded) 0

-- | The score of a solution, as the engine scores one.
scoreOf :: DM.DatabaseManager -> SS.CrossDBSolution -> IO Double
scoreOf manager sol = do
    tables <- DM.mapMethodToTablesCached manager "root" (DM.CollectionName collection) root climate
    either (fail . T.unpack) pure =<< scoreSolution manager (DM.CollectionName collection) climate tables sol

spec :: Spec
spec = do
    describe "the parts of a solution" $ do
        it "add up to the score the engine publishes" $ do
            manager <- managerWith detailKept
            sol <- solution manager
            published <- batchedScoresFor manager "root" (DM.CollectionName collection) root sol [climate]
            M.lookup (methodId climate) published `shouldBe` Just (Right 21)
            (mFlows, _) <- DM.getMergedFlowMetadata manager
            let LicencedSolution{lsShown = shown, lsWithheld = parts} = partitionByLicence mFlows (S.singleton "dep") sol
            shownScore <- scoreOf manager shown
            partScores <- mapM (scoreOf manager . wpSolution) parts
            (shownScore, map wpDatabase parts, partScores) `shouldBe` (1, ["dep"], [20])

        it "never withhold the database asked" $ do
            manager <- managerWith LicenceUnstated
            sol <- solution manager
            (mFlows, _) <- DM.getMergedFlowMetadata manager
            map wpDatabase (lsWithheld (partitionByLicence mFlows (S.fromList ["root", "dep"]) sol)) `shouldBe` ["dep"]

        it "move a database reached twice as one part" $ do
            manager <- managerWith LicenceUnstated
            sol <- solution manager
            (mFlows, _) <- DM.getMergedFlowMetadata manager
            let twice = sol{SS.csScalings = SS.csScalings sol <> NE.fromList (NE.tail (SS.csScalings sol))}
                LicencedSolution{lsWithheld = parts} = partitionByLicence mFlows (S.singleton "dep") twice
            partScores <- mapM (scoreOf manager . wpSolution) parts
            (map wpDatabase parts, partScores) `shouldBe` (["dep"], [40])

    describe "MCP, under a dependency keeping what weighs in its scores" $ do
        it "groups the dependency's processes in one line" $ do
            manager <- managerWith detailKept
            o <- payload =<< callOn manager "get_contributing_activities" []
            KM.lookup "total_score" o `shouldBe` Just (Number 21)
            T.pack (show (KM.lookup "processes" o)) `shouldSatisfy` (not . ("dep::" `T.isInfixOf`))
            groupedLines o `shouldBe` [(String "dep", Number 20)]

        it "groups its flows, and the lines sum to the score" $ do
            manager <- managerWith detailKept
            o <- payload =<< callOn manager "get_contributing_flows" []
            numbersAt "top_flows" "contribution" o `shouldBe` [1]
            sum (numbersAt "top_flows" "contribution" o ++ numbersAt "withheld_databases" "contribution" o) `shouldBe` 21

        it "keeps its flows out of a score's top flows" $ do
            manager <- managerWith detailKept
            o <- payload =<< callOn manager "get_impacts" [("top_flows", Number 5)]
            (KM.lookup "score" o, numbersAt "top_flows" "contribution" o, groupedLines o)
                `shouldBe` (Just (Number 21), [1], [(String "dep", Number 20)])

        it "keeps its exchanges out of the diagnostics" $ do
            manager <- managerWith detailKept
            o <- payload =<< callOn manager "get_impacts" [("include_diagnostics", Bool True)]
            KM.lookup "withheld" o
                `shouldBe` Just (Array (V.singleton (String "The licence of dep keeps the amounts of its exchanges to itself.")))

    describe "under a dependency keeping its amounts" $ do
        it "refuses the inventory over REST and MCP" $ do
            manager <- managerWith inventoryKept
            void <$> runIn manager (getActivityInventory "root" rootPid) `shouldReturn` Left 403
            reply <- callOn manager "get_inventory" []
            errorText reply `shouldBe` Just "The inventory of root includes dep's, whose licence keeps the amounts of its exchanges to itself."

        it "leaves its processes out of the supply chain, counted in one line" $ do
            manager <- managerWith inventoryKept
            Right chain <- supplyChain manager
            map sceDatabaseName (scrSupplyChain chain) `shouldSatisfy` all (== "root")
            [(wprDatabase w, wprProcesses w) | w <- scrWithheldDatabases chain] `shouldBe` [("dep", 1)]

        it "refuses the aggregated biosphere, and counts the consumption" $ do
            manager <- managerWith inventoryKept
            biosphere <- callOn manager "aggregate" [("scope", String "biosphere")]
            errorText biosphere `shouldBe` Just "The inventory of root includes dep's, whose licence keeps the amounts of its exchanges to itself."
            o <- payload =<< callOn manager "aggregate" [("scope", String "consumption")]
            [ (database, processes)
              | Just (Array rows) <- [KM.lookup "withheldDatabases" o]
              , Object row <- toList rows
              , Just database <- [KM.lookup "database" row]
              , Just processes <- [KM.lookup "processes" row]
              ]
                `shouldBe` [(String "dep", Number 1)]

        it "keeps its exchanges out of the flow mapping's uncharacterized flows" $ do
            manager <- managerWith inventoryKept
            o <- payload =<< callOn manager "get_flow_mapping" [("verbose", Bool True)]
            (KM.lookup "unmatched_db_flows" o, KM.lookup "withheld" o)
                `shouldBe` (Just (Array mempty), Just (Array (V.singleton (String "The licence of dep keeps the amounts of its exchanges to itself."))))

        it "still details its contributions" $ do
            manager <- managerWith inventoryKept
            o <- payload =<< callOn manager "get_contributing_activities" []
            T.pack (show (KM.lookup "processes" o)) `shouldSatisfy` ("dep::" `T.isInfixOf`)
            KM.lookup "withheld_databases" o `shouldBe` Nothing

    describe "under a dependency keeping nothing" $
        it "answers as before" $ do
            manager <- managerWith LicenceUnstated
            o <- payload =<< callOn manager "get_contributing_flows" []
            (numbersAt "top_flows" "contribution" o, KM.lookup "withheld_databases" o) `shouldBe` ([20, 1], Nothing)
            Right chain <- supplyChain manager
            length (scrWithheldDatabases chain) `shouldBe` 0
