{-# LANGUAGE LambdaCase #-}
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
    configFor,
    dependency,
    inventoryKept,
    managerOn,
    own,
    producing,
    referenceOf,
    root,
    rootPid,
    runIn,
    runInError,
    spec,
) where

import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import Control.Exception (evaluate)
import Control.Monad (forM_, void, (<=<))
import Data.Aeson (Object, Value (..))
import Data.Aeson.Key (Key)
import qualified Data.Aeson.Key as Key
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
import qualified Data.Vector.Unboxed as U
import Servant (ServerError, errHTTPCode, runHandler)
import Test.Hspec

import API.MCP (callTool, noRequestId)
import API.Routes (batchedScoresFor, getActivityInventory, getActivitySupplyChain)
import API.Types (SupplyChainEntry (..), SupplyChainResponse (..), WithheldProcesses (..))
import App.Env (AppEnv (..), AppM, runApp)
import Config (DatabaseConfig (..), Dependencies (..), defaultConfig)
import Database.Cutoffs (CheckedSolution, reshaped)
import qualified Database.Manager as DM
import FlowContributionSpec (payloadOf)
import Groups (maxGroups)
import Impact (LicencedSolution (..), WithheldPart (..), checkSolution, partitionByLicence, scoreSolution)
import Method.Mapping (MethodTables (..), RegionalActivityWeights (..))
import Method.Types (FlowDirection (..), Method (..), MethodCF (..), MethodCollection (..))
import qualified SharedSolver as SS
import System.Mem.StableName (makeStableName)
import Types
import Usage (anonymous)

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
producing offset emissions = db{dbActivities = V.map (\act -> act{activityLines = packExchanges [referenceOf offset]}) (dbActivities db)}
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
        , techFormula = Nothing
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
        , dcDepends = Unsettled
        , dcLocationAliases = M.empty
        , dcFormat = Nothing
        , dcIsUploaded = False
        , dcDeletable = False
        , dcGeographyPolicy = GeoGlobal
        , dcReading = FromFiles ReadDeclared
        , dcPatches = []
        , dcSource = Nothing
        , dcLicence = licence
        , dcRelease = Nothing
        , dcRequires = []
        , dcAdded = Nothing
        }

own :: [Permission] -> Licence
own refused = LicenceOwn OwnLicence{ownText = "Ours", ownRefused = S.fromList refused, ownAttribution = AttributionRequired, ownAcceptance = NoAcceptance}

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
    callTool anonymous manager [] Nothing Nothing noRequestId tool $
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

-- | The databases and contributions of the withheld lines of a group tool's answer.
withheldLines :: Object -> [(Value, Value)]
withheldLines o =
    [ (database, contribution)
    | Just (Array rows) <- [KM.lookup "withheldDatabases" o]
    , Object row <- toList rows
    , Just database <- [KM.lookup "database" row]
    , Just contribution <- [KM.lookup "contribution" row]
    ]

-- | The root emitting 1 kg of carbon dioxide and 0.5 kg of methane; the dependency emits 2 kg of methane, one flow with it.
twoFlowRoot :: Database
twoFlowRoot =
    root
        { dbBiosphere = byColumn (U.fromList [SparseTriple 0 0 1, SparseTriple 1 0 0.5])
        , dbBiosphereOrder = V.fromList [bfId testFlow, bfId methane]
        , dbBiosphereCount = 2
        , dbBioFlows = M.fromList [(bfId testFlow, testFlow), (bfId methane, methane)]
        }

-- | The climate method with factors read where each process stands: 4 for carbon dioxide, 10 for methane, in France.
regionalClimate :: Method
regionalClimate =
    climate
        { methodId = mkUUID 7002
        , methodFactors = [(factor testFlow 4){mcfConsumerLocation = Just "FR"}, (factor methane 10){mcfConsumerLocation = Just "FR"}]
        }

runIn :: DM.DatabaseManager -> AppM a -> IO (Either Int a)
runIn manager handler = either (Left . errHTTPCode) Right <$> runInError manager handler

-- | The handler's answer, or the whole error it was refused with, body included.
runInError :: DM.DatabaseManager -> AppM a -> IO (Either ServerError a)
runInError manager handler = runHandler (runApp env handler)
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
            , aeUsageLog = Nothing
            , aeReader = Nothing
            , aeAuthor = Nothing
            }

supplyChain :: DM.DatabaseManager -> IO (Either Int SupplyChainResponse)
supplyChain manager =
    runIn manager $
        getActivitySupplyChain "root" rootPid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] [] [] Nothing Nothing Nothing

solution :: DM.DatabaseManager -> IO CheckedSolution
solution manager = do
    unitCfg <- DM.getMergedUnitConfig manager
    Just loaded <- DM.getDatabase manager "root"
    raw <-
        either (fail . T.unpack) pure
            =<< SS.computeInventoryMatrixWithDepsCached unitCfg (DM.mkDepSolverLookup manager) root "root" (DM.ldSharedSolver loaded) 0
    either (fail . show) pure =<< checkSolution manager SeeDetailedScores raw

-- | The score of a solution, as the engine scores one.
scoreOf :: DM.DatabaseManager -> CheckedSolution -> IO Double
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
            let twice = reshaped (\s -> s{SS.csScalings = SS.csScalings s <> NE.fromList (NE.tail (SS.csScalings s))}) sol
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

        it "counts its line in the absolute total a cut covers, and refuses a cut or a cap written as text" $ do
            manager <- managerWith detailKept
            flows <- payload =<< callOn manager "get_contributing_flows" [("cover_percent", Number 80)]
            processes <- payload =<< callOn manager "get_contributing_activities" [("cover_percent", Number 80)]
            asText <- callOn manager "get_contributing_flows" [("cover_percent", String "80")]
            -- The root's own flow weighs 1 of 21: the dependency's 20 count in what the rows cover.
            (numbersAt "top_flows" "absolute_percent" flows, numbersAt "top_flows" "cumulative_percent" flows) `shouldBe` ([100 / 21], [100 / 21])
            numbersAt "processes" "cumulative_percent" processes `shouldBe` [100 / 21]
            limitAsText <- callOn manager "get_contributing_flows" [("cover_percent", Number 80), ("limit", String "5")]
            errorText asText `shouldBe` Just "Parameter 'cover_percent' must be a number"
            errorText limitAsText `shouldBe` Just "Parameter 'limit' must be a whole number"

        it "gathers the flows by substance when asked, and refuses the request written as text" $ do
            manager <- managerWith detailKept
            grouped <- payload =<< callOn manager "get_contributing_flows" [("by_substance", Bool True)]
            asText <- callOn manager "get_contributing_flows" [("by_substance", String "true")]
            (numbersAt "top_flows" "contribution" grouped, numbersAt "top_flows" "cumulative_percent" grouped) `shouldBe` ([1], [100 / 21])
            errorText asText `shouldBe` Just "Parameter 'by_substance' must be true or false"

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

        it "gives its processes to no group, and shows it as a line of a split between groups" $ do
            manager <- managerWith detailKept
            -- Every process is in France: only the root, which no group takes, and the dependency's would answer.
            let france = Array (V.singleton (Object (KM.fromList [("name", String "France"), ("filter", Object (KM.singleton "location" (String "FR")))])))
                score :: Value -> Maybe Value
                score = \case
                    Object scores -> KM.lookup (Key.fromText (UUID.toText (methodId climate))) scores
                    _ -> Nothing
                parts o =
                    ( score =<< KM.lookup "total" o
                    , score =<< KM.lookup "rest" o
                    , [score =<< KM.lookup "scores" g | Just (Array gs) <- [KM.lookup "groups" o], Object g <- toList gs]
                    , [(KM.lookup "database" w, score =<< KM.lookup "scores" w) | Just (Array ws) <- [KM.lookup "withheldDatabases" o], Object w <- toList ws]
                    )
            mapM_
                ( \reading -> do
                    o <- payload =<< callOn manager "get_group_contributions" [("groups", france), ("attribution", String reading)]
                    parts o `shouldBe` (Just (Number 21), Just (Number 1), [Just (Number 0)], [(Just (String "dep"), Just (Number 20))])
                )
                ["first-match", "own"]

    describe "MCP, the processes of a group" $ do
        it "add up to the group's part, and those of the rest to the rest, in both readings" $ do
            manager <- managerWith LicenceUnstated
            -- The root goes to no group, so France takes the dependency's one process alone.
            let france = Array (V.singleton (Object (KM.fromList [("name", String "France"), ("filter", Object (KM.singleton "location" (String "FR")))])))
                members reading group = do
                    o <- payload =<< callOn manager "get_group_members" ([("groups", france), ("attribution", String reading)] ++ [("group", String g) | Just g <- [group]])
                    pure (KM.lookup "total_score" o, numbersAt "processes" "contribution" o)
            mapM_
                ( \reading -> do
                    members reading (Just "France") `shouldReturn` (Just (Number 20), [20])
                    members reading Nothing `shouldReturn` (Just (Number 1), [1])
                )
                ["first-match", "own"]

    describe "MCP, a group keeping a substance, under a method read where each process stands" $ do
        it "lists the group's processes and flows first, caches the factors summed over the database as loaded, and adds up" $ do
            manager <- managerOn twoFlowRoot dependency LicenceUnstated
            atomically $ modifyTVar' (DM.dmLoadedMethods manager) (M.insert collection (MethodCollection [regionalClimate] [] []))
            let co2 = Array (V.singleton (Object (KM.fromList [("name", String "CO2"), ("filter", Object (KM.singleton "flow" (Object (KM.singleton "name" (String "Carbon dioxide")))))])))
                regional = ("method_id", String (UUID.toText (methodId regionalClimate)))
                flows group = do
                    o <- payload =<< callOn manager "get_group_flows" ([regional, ("groups", co2)] ++ [("group", String g) | Just g <- [group]])
                    pure (KM.lookup "totalScore" o, numbersAt "topFlows" "contribution" o)
            -- On a cold cache, the group's part is the first thing scored: its database holds only the root's carbon dioxide.
            members <- payload =<< callOn manager "get_group_members" [regional, ("groups", co2), ("group", String "CO2")]
            (KM.lookup "total_score" members, numbersAt "processes" "contribution" members) `shouldBe` (Just (Number 4), [4])
            flows (Just "CO2") `shouldReturn` (Just (Number 4), [4])
            Just loaded <- DM.getDatabase manager "root"
            cached <- M.lookup ("root", DM.CollectionName collection, methodId regionalClimate) <$> readTVarIO (DM.dmMethodTablesCache manager)
            summedOver <- traverse (makeStableName <=< evaluate . rawSummedOver) (mtRegionalActivityWeights =<< cached)
            asLoaded <- makeStableName =<< evaluate (dbBiosphereTriples (DM.ldDatabase loaded))
            (summedOver == Just asLoaded) `shouldBe` True
            flows Nothing `shouldReturn` (Just (Number 25), [25])
            o <- payload =<< callOn manager "get_impacts" [regional]
            KM.lookup "score" o `shouldBe` Just (Number 29)

        -- A group's flows can be one process's own exchanges through the factors: a licence keeping the amounts withholds them too.
        forM_ [("its detail", detailKept), ("the amounts of its exchanges", inventoryKept)] $ \(kept, licence) ->
            it ("lists in the rest only a line for a database whose licence keeps " <> kept <> ", and none in a group") $ do
                manager <- managerOn twoFlowRoot dependency licence
                let co2 = Array (V.singleton (Object (KM.fromList [("name", String "CO2"), ("filter", Object (KM.singleton "flow" (Object (KM.singleton "name" (String "Carbon dioxide")))))])))
                    flows group = do
                        o <- payload =<< callOn manager "get_group_flows" (("groups", co2) : [("group", String g) | Just g <- [group]])
                        pure (KM.lookup "totalScore" o, length (numbersAt "topFlows" "contribution" o), withheldLines o)
                flows (Just "CO2") `shouldReturn` (Just (Number 1), 1, [])
                flows Nothing `shouldReturn` (Just (Number 25), 1, [(String "dep", Number 20)])

        it "refuses the flows of a group of a root whose licence keeps the amounts of its exchanges" $ do
            manager <- managerOn twoFlowRoot dependency LicenceUnstated
            DM.addDatabase manager (configFor "root" inventoryKept)
            let co2 = Array (V.singleton (Object (KM.fromList [("name", String "CO2"), ("filter", Object (KM.singleton "flow" (Object (KM.singleton "name" (String "Carbon dioxide")))))])))
            reply <- callOn manager "get_group_flows" [("groups", co2), ("group", String "CO2")]
            errorText reply `shouldSatisfy` maybe False ("root" `T.isInfixOf`)

        it "adds up, the withheld database's line included" $ do
            manager <- managerOn twoFlowRoot dependency detailKept
            let co2 = Array (V.singleton (Object (KM.fromList [("name", String "CO2"), ("filter", Object (KM.singleton "flow" (Object (KM.singleton "name" (String "Carbon dioxide")))))])))
                score = \case
                    Object scores -> KM.lookup (Key.fromText (UUID.toText (methodId climate))) scores
                    _ -> Nothing
            o <- payload =<< callOn manager "get_group_contributions" [("groups", co2)]
            ( KM.lookup "attribution" o
                , score =<< KM.lookup "total" o
                , [score =<< KM.lookup "scores" g | Just (Array gs) <- [KM.lookup "groups" o], Object g <- toList gs]
                , score =<< KM.lookup "rest" o
                , [score =<< KM.lookup "scores" w | Just (Array ws) <- [KM.lookup "withheldDatabases" o], Object w <- toList ws]
                )
                `shouldBe` (Just (String "own"), Just (Number 26), [Just (Number 1)], Just (Number 5), [Just (Number 20)])

        it "refuses a flow to group by under the whole-chain reading" $ do
            manager <- managerOn twoFlowRoot dependency LicenceUnstated
            reply <- callOn manager "get_group_candidates" [("groups", Array V.empty), ("attribution", String "first-match"), ("candidate_flow", String (UUID.toText (bfId testFlow)))]
            errorText reply `shouldSatisfy` maybe False ("only the own-emissions reading splits" `T.isInfixOf`)
            offered <- payload =<< callOn manager "get_group_candidates" [("groups", Array V.empty), ("candidate_flow", String (UUID.toText (bfId testFlow)))]
            numbersAt "candidates" "score" offered `shouldBe` [1]

        it "links a split by substance to the groups page, in own emissions" $ do
            manager <- managerOn twoFlowRoot dependency LicenceUnstated
            let co2 = Array (V.singleton (Object (KM.fromList [("name", String "CO2"), ("filter", Object (KM.singleton "flow" (Object (KM.singleton "name" (String "Carbon dioxide")))))])))
                france = Array (V.singleton (Object (KM.fromList [("name", String "France"), ("filter", Object (KM.singleton "location" (String "FR")))])))
                linked tool groups' = do
                    o <- payload =<< callTool anonymous manager [] Nothing (Just "https://volca.example") noRequestId tool (KM.fromList [("database", String "root"), ("process_id", String rootPid), ("method_id", String (UUID.toText (methodId climate))), ("collection", String collection), ("groups", groups')])
                    pure (KM.lookup "web_url" o)
                linksTo groups' wanted tool = linked tool groups' >>= (`shouldSatisfy` maybe False (\case String url -> all (`T.isInfixOf` url) wanted; _ -> False))
            mapM_ (linksTo co2 ["reading=own", "g1=CO2", "g1-flow-name=Carbon%20dioxide"]) ["get_group_members", "get_group_flows", "get_group_contributions"]
            mapM_ (linksTo france ["g1=France", "g1-location=FR"]) ["get_group_members", "get_group_flows", "get_group_contributions"]

    describe "MCP, the groups a process could start" $ do
        let candidates manager extra = callOn manager "get_group_candidates" (("candidate", String ("dep::" <> processIdToText dependency 0)) : extra)
        it "offers the broadest level of the dependency's class, with the part a split gives it once added, in both readings" $ do
            -- Every level of the code takes the same one process: only the broadest stays.
            let classed = dependency{dbActivities = V.map (\a -> a{activityClassification = M.singleton "CPC" "6511: Road transport services of freight"}) (dbActivities dependency)}
            manager <- managerOn root classed LicenceUnstated
            mapM_
                ( \reading -> do
                    o <- payload =<< candidates manager [("groups", Array V.empty), ("attribution", String reading)]
                    let names = [name | Just (Array cs) <- [KM.lookup "candidates" o], Object c <- toList cs, Just (Object g) <- [KM.lookup "group" c], Just (String name) <- [KM.lookup "name" g]]
                    (KM.lookup "total" o, names, numbersAt "candidates" "score" o, numbersAt "candidates" "activitiesCounted" o)
                        `shouldBe` (Just (Number 21), ["CPC 65"], [20], [1])
                )
                ["first-match", "own"]

        it "refuses a process whose database keeps what weighs in its scores" $ do
            manager <- managerWith detailKept
            reply <- candidates manager [("groups", Array V.empty)]
            errorText reply `shouldBe` Just (withheldSentence "dep" SeeDetailedScores)

        it "refuses a list that already holds as many groups as it can" $ do
            manager <- managerWith LicenceUnstated
            let full = Array (V.fromList [Object (KM.fromList [("name", String ("Group " <> T.pack (show k))), ("filter", Object (KM.singleton "location" (String "DE")))]) | k <- [1 .. maxGroups]])
            reply <- candidates manager [("groups", full)]
            errorText reply `shouldSatisfy` maybe False ("remove one to add another" `T.isInfixOf`)

    describe "under a dependency keeping its amounts" $ do
        it "refuses the inventory over REST and MCP" $ do
            manager <- managerWith inventoryKept
            void <$> runIn manager (getActivityInventory "root" rootPid Nothing Nothing) `shouldReturn` Left 403
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
