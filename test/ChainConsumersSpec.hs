{-# LANGUAGE OverloadedStrings #-}

{- | The consumers, inside one root's upstream, of a process it lists: each
with its part of the process's quantity and the lines it buys it on.

In @solo@ the root buys 2 of A and 1 of B; A buys 0.5 of C per unit, B buys C
on two lines (0.6 and 0.4); A sends 0.25 per unit of its waste D to be treated,
a negative input. So C is 2 (1 from A, 1 from B) and D is −0.5.
-}
module ChainConsumersSpec (buying, spec) where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Test.Hspec

import API.Routes (getActivitySupplyChain, getChainConsumers)
import API.Types (ChainConsumer (..), ChainConsumers (..), ExchangeWithUnit (..), SupplyChainEntry (..), SupplyChainResponse (..), Unexplained (..))
import Config (defaultConfig)
import qualified Database.Manager as DM
import Types

import CrossDBRegionalLCIAFixture (linkAt, mkDB, prodUUID)
import DependencyLicenceSpec (configFor, dependency, producing, referenceOf, root, rootPid, runIn)
import PathToSpec (behindWithheld, depPid)
import TestHelpers (install)

-- | A line buying one unit of the fixture process numbered @offset@, with a comment.
buying :: Int -> Double -> Text -> Exchange
buying offset amount comment =
    (referenceOf offset)
        { techAmount = amount
        , techRole = Input
        , techComment = Just comment
        }

solo :: Database
solo =
    base
        { dbActivities = V.imap (\i act -> act{activityLines = packExchanges (linesOf i)}) (dbActivities base)
        , dbTechnosphereTriples = U.fromList [SparseTriple 1 0 2.0, SparseTriple 2 0 1.0, SparseTriple 3 1 0.5, SparseTriple 3 2 0.6, SparseTriple 3 2 0.4, SparseTriple 4 1 (-0.25)]
        }
  where
    base :: Database
    base = mkDB 300 ["FR", "FR", "FR", "FR", "FR"] []

    linesOf :: Int -> [Exchange]
    linesOf 2 = [referenceOf 302, buying 303 0.6 "first line", buying 303 0.4 "second line"]
    linesOf i = [referenceOf (300 + i)]

soloId :: Int -> Text
soloId i = processIdToText solo (fromIntegral i)

managerFor :: [(Text, Database)] -> IO DM.DatabaseManager
managerFor dbs = do
    manager <- DM.initDatabaseManager defaultConfig DM.NoCache
    mapM_ (\(name, _) -> DM.addDatabase manager (configFor name LicenceUnstated)) dbs
    mapM_ (uncurry (install manager)) (reverse dbs)
    pure manager

consumersOf :: DM.DatabaseManager -> Text -> Text -> Maybe Text -> IO (Either Int ChainConsumers)
consumersOf manager dbName rootId target = consumersAt manager dbName rootId target Nothing

-- | The consumers at a minimum quantity, as the supply chain lists them at it.
consumersAt :: DM.DatabaseManager -> Text -> Text -> Maybe Text -> Maybe Double -> IO (Either Int ChainConsumers)
consumersAt manager dbName rootId target minQuantity = runIn manager (getChainConsumers dbName rootId target minQuantity)

-- | Each consumer's id and part, in id order.
parts :: ChainConsumers -> [(Text, Double)]
parts answer = L.sort [(sceProcessId (chnProcess c), chnAmount c) | c <- chcConsumers answer]

chainOf :: DM.DatabaseManager -> Text -> Text -> IO (Either Int SupplyChainResponse)
chainOf manager dbName rootId = chainAt manager dbName rootId Nothing

chainAt :: DM.DatabaseManager -> Text -> Text -> Maybe Double -> IO (Either Int SupplyChainResponse)
chainAt manager dbName rootId minQuantity =
    runIn manager $
        getActivitySupplyChain dbName rootId Nothing Nothing minQuantity Nothing Nothing Nothing Nothing Nothing [] [] [] Nothing Nothing Nothing

-- | Each listed process's quantity and consumer count, by id.
rows :: SupplyChainResponse -> M.Map Text (Double, Int)
rows chain = M.fromList [(sceProcessId e, (sceQuantityPerUnit e, sceUpstreamCount e)) | e <- scrSupplyChain chain]

closeTo :: [(Text, Double)] -> [(Text, Double)] -> Bool
closeTo xs ys = length xs == length ys && and (zipWith (\(a, x) (b, y) -> a == b && abs (x - y) < 1e-9) xs ys)

{- | The root buys from @dep@ and from @far@, and @dep@ buys from @far@ too:
@far@ is reached by two routes, 2 bought by the root and 1 through @dep@.
-}
twoRoutes :: IO DM.DatabaseManager
twoRoutes = managerFor [("root", top), ("dep", middle), ("far", far)]
  where
    far, middle, top :: Database
    far = producing 200 []
    middle = (linkAt dependency far "far" 0 1.0){dbDependsOn = ["far"]}
    top = (linkAt (linkAt (producing 100 []) middle "dep" 0 1.0) far "far" 0 2.0){dbDependsOn = ["dep", "far"]}

farId :: Text
farId = "far::" <> processIdToText (producing 200 []) 0

{- | A dependency whose first process buys 0.5 of its second on a line of its
own database, the root buying one of the first.
-}
nextDoor :: IO DM.DatabaseManager
nextDoor = managerFor [("root", (linkAt (producing 100 []) inner "dep" 0 1.0){dbDependsOn = ["dep"]}), ("dep", inner)]

inner :: Database
inner =
    base
        { dbActivities = V.imap (\i act -> act{activityLines = packExchanges (linesOf i)}) (dbActivities base)
        , dbTechnosphereTriples = U.fromList [SparseTriple 1 0 0.5]
        }
  where
    base :: Database
    base = mkDB 200 ["FR", "FR"] []

    linesOf :: Int -> [Exchange]
    linesOf 0 = [referenceOf 200, buying 201 0.5 "bought next door"]
    linesOf i = [referenceOf (200 + i)]

-- | The root of 'DependencyLicenceSpec', its link to the dependency read from a line that says why.
rootOnALine :: Database
rootOnALine =
    root
        { dbCrossDBLinks = map (\l -> l{cdlConsumerFlowId = prodUUID 1}) (dbCrossDBLinks root)
        , dbActivities = V.map (\act -> act{activityLines = packExchanges [referenceOf 100, abroad]}) (dbActivities root)
        }
  where
    abroad :: Exchange
    abroad = (buying 1 1.0 "bought abroad"){techActivityLinkId = Nothing}

spec :: Spec
spec = do
    describe "inside one database" $ do
        it "gives each consumer its part, and the parts add up to the quantity" $ do
            manager <- managerFor [("solo", solo)]
            Right answer <- consumersOf manager "solo" (soloId 0) (Just (soloId 3))
            sceQuantityPerUnit (chcProcess answer) `shouldSatisfy` (\x -> abs (x - 2) < 1e-9)
            parts answer `shouldSatisfy` closeTo (L.sort [(soloId 1, 1.0), (soloId 2, 1.0)])
            fmap unxAmount (chcUnexplained answer) `shouldBe` Nothing

        it "counts a consumer buying on two lines once, and gives both lines with their comments" $ do
            manager <- managerFor [("solo", solo)]
            Right chain <- chainOf manager "solo" (soloId 0)
            fmap snd (M.lookup (soloId 3) (rows chain)) `shouldBe` Just 2
            Right answer <- consumersOf manager "solo" (soloId 0) (Just (soloId 3))
            [map ewuExComment (chnLines c) | c <- chcConsumers answer, sceProcessId (chnProcess c) == soloId 2]
                `shouldBe` [[Just "first line", Just "second line"]]

        it "lists a consumer whose lines name no supplier, with no line" $ do
            manager <- managerFor [("solo", solo)]
            Right answer <- consumersOf manager "solo" (soloId 0) (Just (soloId 3))
            [length (chnLines c) | c <- chcConsumers answer, sceProcessId (chnProcess c) == soloId 1] `shouldBe` [0]

        it "gives a waste sent to treatment a negative part" $ do
            manager <- managerFor [("solo", solo)]
            Right answer <- consumersOf manager "solo" (soloId 0) (Just (soloId 4))
            parts answer `shouldSatisfy` closeTo [(soloId 1, -0.5)]

        it "lists the root among the consumers of a process it buys" $ do
            manager <- managerFor [("solo", solo)]
            Right answer <- consumersOf manager "solo" (soloId 0) (Just (soloId 1))
            parts answer `shouldSatisfy` closeTo [(soloId 0, 2.0)]
            map (sceDepth . chnProcess) (chcConsumers answer) `shouldBe` [0]

        it "lists, at a minimum quantity, the consumers the count counts, and leaves the others unexplained" $ do
            manager <- managerFor [("solo", solo)]
            Right chain <- chainAt manager "solo" (soloId 0) (Just 1.5)
            fmap snd (M.lookup (soloId 3) (rows chain)) `shouldBe` Just 1
            Right answer <- consumersAt manager "solo" (soloId 0) (Just (soloId 3)) (Just 1.5)
            parts answer `shouldSatisfy` closeTo [(soloId 1, 1.0)]
            fmap (\u -> (unxAmount u, unxWithheld u)) (chcUnexplained answer) `shouldBe` Just (1.0, [])

    describe "across databases" $ do
        it "finds the line a dependency's process buys another of its database on" $ do
            manager <- nextDoor
            Right answer <- consumersOf manager "root" (processIdToText (producing 100 []) 0) (Just (depPid inner 1))
            parts answer `shouldSatisfy` closeTo [(depPid inner 0, 0.5)]
            concatMap (map ewuExComment . chnLines) (chcConsumers answer) `shouldBe` [Just "bought next door"]

        it "counts the consumer of a dependency's process in the database buying it" $ do
            manager <- managerFor [("root", root), ("dep", dependency)]
            Right chain <- chainOf manager "root" rootPid
            fmap snd (M.lookup (depPid dependency 0) (rows chain)) `shouldBe` Just 1

        it "finds the line a consumer buys a dependency's process on" $ do
            manager <- managerFor [("root", rootOnALine), ("dep", dependency)]
            Right answer <- consumersOf manager "root" rootPid (Just (depPid dependency 0))
            parts answer `shouldSatisfy` closeTo [(rootPid, 1.0)]
            concatMap (map ewuExComment . chnLines) (chcConsumers answer) `shouldBe` [Just "bought abroad"]

        it "lists a process reached by two routes once, with both consumers" $ do
            manager <- twoRoutes
            Right chain <- chainOf manager "root" (processIdToText (producing 100 []) 0)
            length [e | e <- scrSupplyChain chain, sceProcessId e == farId] `shouldBe` 1
            M.lookup farId (rows chain) `shouldBe` Just (3.0, 2)
            Right answer <- consumersOf manager "root" (processIdToText (producing 100 []) 0) (Just farId)
            parts answer `shouldSatisfy` closeTo (L.sort [(processIdToText (producing 100 []) 0, 2.0), ("dep::" <> processIdToText dependency 0, 1.0)])

        it "counts a process reached by two routes once in the chain's total" $ do
            manager <- twoRoutes
            Right chain <- chainOf manager "root" (processIdToText (producing 100 []) 0)
            (scrTotalActivities chain, scrFilteredActivities chain) `shouldBe` (2, 2)

        it "reads the minimum quantity on the whole quantity of a process reached by two routes" $ do
            manager <- twoRoutes
            let topId = processIdToText (producing 100 []) 0
            -- far needs 3.0 in all, 1.0 of it through dep: above 2.5, with dep under it
            Right chain <- chainAt manager "root" topId (Just 2.5)
            M.lookup farId (rows chain) `shouldBe` Just (3.0, 1)
            Right answer <- consumersAt manager "root" topId (Just farId) (Just 1.5)
            parts answer `shouldSatisfy` closeTo [(topId, 2.0)]
            fmap unxAmount (chcUnexplained answer) `shouldBe` Just 1.0

        it "gives what a withheld database's consumers account for as unexplained, naming it" $ do
            manager <- behindWithheld
            Right answer <- consumersOf manager "root" rootPid (Just farId)
            length (chcConsumers answer) `shouldBe` 0
            fmap (\u -> (unxAmount u, unxWithheld u)) (chcUnexplained answer) `shouldBe` Just (1.0, ["dep"])

    describe "refusals" $ do
        it "refuses a request naming no process" $ do
            manager <- managerFor [("solo", solo)]
            fmap parts <$> consumersOf manager "solo" (soloId 0) Nothing `shouldReturn` Left 400

        it "finds no consumers for a process outside the chain, nor for the root" $ do
            manager <- managerFor [("solo", solo)]
            fmap parts <$> consumersOf manager "solo" (soloId 1) (Just (soloId 2)) `shouldReturn` Left 404
            fmap parts <$> consumersOf manager "solo" (soloId 0) (Just (soloId 0)) `shouldReturn` Left 404
