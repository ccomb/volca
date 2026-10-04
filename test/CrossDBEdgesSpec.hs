{-# LANGUAGE OverloadedStrings #-}

{- | The edges a supply chain draws from one database to another.

On the two databases of 'DependencyLicenceSpec' (the root emits 1 kg of carbon
dioxide and buys one unit of the dependency's process, which emits 2 kg of
methane: a score of 21), the root is joined to its supplier, and a tree read
from the edges gives the root back its own part, 1, once each supplier's
score is taken away.
-}
module CrossDBEdgesSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import API.Routes (batchedScoresFor, getActivitySupplyChain)
import API.Types (SupplyChainEdge (..), SupplyChainResponse (..), WithheldInput (..))
import qualified Database.Manager as DM
import Method.Types (Method (..))
import qualified SharedSolver as SS
import Types

import DependencyLicenceSpec (climate, collection, dependency, inventoryKept, managerOn, root, rootPid, runIn)

depPid :: Text
depPid = "dep::" <> processIdToText dependency 0

-- | The root, its link to the dependency rewritten by @relink@.
relinked :: ([CrossDBLink] -> [CrossDBLink]) -> Database
relinked relink = root{dbCrossDBLinks = relink (dbCrossDBLinks root)}

edgesOn :: Database -> Licence -> IO SupplyChainResponse
edgesOn rootDb licence = do
    manager <- managerOn rootDb licence
    either (fail . ("supply chain answered " <>) . show) pure
        =<< runIn manager (getActivitySupplyChain "root" rootPid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] [] [] Nothing Nothing (Just True))

crossEdges :: SupplyChainResponse -> [(Text, Text, Text, Text, Double)]
crossEdges chain =
    [ (sceEdgeFrom e, sceEdgeFromDb e, sceEdgeTo e, sceEdgeToDb e, sceEdgeAmount e)
    | e <- scrEdges chain
    , sceEdgeFromDb e /= sceEdgeToDb e
    ]

-- | The score of one process of a loaded database, as the engine publishes it.
scoreOf :: DM.DatabaseManager -> Text -> Database -> ProcessId -> IO Double
scoreOf manager name db idx = do
    unitCfg <- DM.getMergedUnitConfig manager
    Just loaded <- DM.getDatabase manager name
    sol <-
        either (fail . T.unpack) pure
            =<< SS.computeInventoryMatrixWithDepsCached unitCfg (DM.mkDepSolverLookup manager) db name (DM.ldSharedSolver loaded) idx
    scores <- batchedScoresFor manager name (DM.CollectionName collection) db sol [climate]
    maybe (fail "no score") (either (fail . show) pure) (M.lookup (methodId climate) scores)

spec :: Spec
spec = do
    it "joins the root to the supplier it buys from in the other database" $ do
        chain <- edgesOn root LicenceUnstated
        crossEdges chain `shouldBe` [(depPid, "dep", rootPid, "root", 1)]
        map wiConsumer (scrWithheldInputs chain) `shouldBe` []

    it "leaves the root its own part once the supplier's score is taken away" $ do
        manager <- managerOn root LicenceUnstated
        rootScore <- scoreOf manager "root" root 0
        depScore <- scoreOf manager "dep" dependency 0
        chain <- edgesOn root LicenceUnstated
        rootScore - sum [amount * depScore | (_, _, _, _, amount) <- crossEdges chain] `shouldBe` 1

    it "draws one edge for two links between the same processes, their amounts summed" $ do
        let split link = [link{cdlCoefficient = 0.75}, link{cdlCoefficient = 0.25}]
        chain <- edgesOn (relinked (concatMap split)) LicenceUnstated
        crossEdges chain `shouldBe` [(depPid, "dep", rootPid, "root", 1)]

    it "draws no edge where two links cancel out" $ do
        let cancel link = [link, link{cdlCoefficient = negate (cdlCoefficient link)}]
        chain <- edgesOn (relinked (concatMap cancel)) LicenceUnstated
        crossEdges chain `shouldBe` []

    it "converts the amount to the supplier's unit" $ do
        let inGrams link = link{cdlCoefficient = 1000, cdlExchangeUnit = "g"}
        chain <- edgesOn (relinked (map inGrams)) LicenceUnstated
        crossEdges chain `shouldBe` [(depPid, "dep", rootPid, "root", 1)]

    it "names the buyer instead of the edge where the supplier keeps its amounts" $ do
        chain <- edgesOn root inventoryKept
        crossEdges chain `shouldBe` []
        [(wiConsumer w, wiDatabase w) | w <- scrWithheldInputs chain] `shouldBe` [(rootPid, "dep")]
