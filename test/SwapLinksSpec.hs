{-# LANGUAGE OverloadedStrings #-}

{- | The links of a supply chain under a swap, and the path read from them.

The root buys one unit of C and one of D; C buys 2 of A, D buys 3 of A. B, in
the same database, is bought by nobody until a swap puts it in A's place; B
buys half a unit of a process of the dependency. Swapping A for B moves the
amounts, and the links drawn must move with them: B linked to C and D, A to
nobody, B at depth 2, its dependency supplier at depth 3.
-}
module SwapLinksSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Test.Hspec

import API.Routes (getActivityPathTo, postActivityPathTo)
import API.Types (Substitution (..), SubstitutionRequest (..), SubstitutionScope (..), SupplyChainEdge (..), SupplyChainEntry (..), SupplyChainResponse (..))
import Database.Cutoffs (gapIndexOf)
import Service
import TestHelpers (mkDepLookupFromMap, mkSolverFromDb, shippedGeographies)
import Types
import UnitConversion (defaultUnitConfig)

import CrossDBRegionalLCIAFixture (mkDB)
import DependencyLicenceSpec (managerOn, producing, referenceOf, runIn)

-- | Root 0 buys C 1 and D 4; C and D buy A 2; B 3 buys from the dependency.
swapRoot :: Database
swapRoot =
    base
        { dbActivities = V.imap (\i act -> act{activityLines = packExchanges [referenceOf (300 + i)]}) (dbActivities base)
        , dbTechnosphereTriples = U.fromList [SparseTriple 1 0 1.0, SparseTriple 4 0 1.0, SparseTriple 2 1 2.0, SparseTriple 2 4 3.0]
        , dbCrossDBLinks = [boughtByB]
        , dbDependsOn = ["dep"]
        }
  where
    base :: Database
    base = mkDB 300 (replicate 5 "FR") []

    boughtByB :: CrossDBLink
    boughtByB =
        CrossDBLink
            { cdlConsumerActUUID = fst (dbProcessIdTable base V.! 3)
            , cdlConsumerProdUUID = snd (dbProcessIdTable base V.! 3)
            , cdlConsumerFlowId = UUID.nil
            , cdlSupplierActUUID = fst (dbProcessIdTable dep V.! 0)
            , cdlSupplierProdUUID = snd (dbProcessIdTable dep V.! 0)
            , cdlCoefficient = 0.5
            , cdlExchangeUnit = "kg"
            , cdlFlowName = "x-link"
            , cdlLocation = "FR"
            , cdlSourceDatabase = "dep"
            , cdlTiedAlternatives = []
            , cdlAnswers = Nothing
            }

dep :: Database
dep = producing 400 []

pidOf :: Int -> Text
pidOf = processIdToText swapRoot . fromIntegral

depSupplier :: Text
depSupplier = "dep::" <> processIdToText dep 0

rootP, cP, aP, bP, dP :: Text
rootP = pidOf 0
cP = pidOf 1
aP = pidOf 2
bP = pidOf 3
dP = pidOf 4

swapAll :: Substitution
swapAll = Substitution aP bP AllConsumers

swapAtC :: Substitution
swapAtC = Substitution aP bP (OneEdge cP)

-- | The chain of the root under these swaps, every entry and every edge.
chainUnder :: [Substitution] -> IO SupplyChainResponse
chainUnder subs = do
    rootSolver <- mkSolverFromDb swapRoot "root"
    depSolver <- mkSolverFromDb dep "dep"
    let lookup_ = mkDepLookupFromMap (M.singleton "dep" (dep, depSolver))
    root <- either (fail . show) pure =<< computeScalingVectorWithSubstitutionsCrossDB defaultUnitConfig lookup_ swapRoot "root" rootSolver 0 subs
    either (fail . show) pure =<< buildSupplyChainFromScalingVectorCrossDB defaultUnitConfig shippedGeographies lookup_ (\_ d -> pure (gapIndexOf d)) swapRoot "root" 0 root withEdges
  where
    withEdges :: SupplyChainFilter
    withEdges =
        SupplyChainFilter
            { scfCore = ActivityFilterCore Nothing Nothing Nothing [] noExclusions Nothing Nothing Nothing Forward
            , scfMaxDepth = Nothing
            , scfMinQuantity = Nothing
            , scfEdges = WithEdges
            , scfWithheld = mempty
            }

edgesOf :: SupplyChainResponse -> M.Map (Text, Text) Double
edgesOf resp = M.fromList [((sceEdgeFrom e, sceEdgeTo e), sceEdgeAmount e) | e <- scrEdges resp]

depthsOf :: SupplyChainResponse -> M.Map Text Int
depthsOf resp = M.fromList [(sceProcessId e, sceDepth e) | e <- scrSupplyChain resp]

-- | The id and the quantity of each step of a path, or the HTTP code it was refused with.
pathUnder :: [Substitution] -> Text -> IO (Either Int [(Value, Value)])
pathUnder subs target = do
    manager <- managerOn swapRoot dep LicenceUnstated
    fmap steps
        <$> runIn
            manager
            ( if null subs
                then getActivityPathTo "root" rootP Nothing (Just target)
                else postActivityPathTo "root" rootP Nothing (Just target) (SubstitutionRequest subs [])
            )
  where
    steps :: Value -> [(Value, Value)]
    steps (Object o)
        | Just (Array path) <- KM.lookup "path" o =
            [ (fromMaybe Null (KM.lookup "processId" s), fromMaybe Null (KM.lookup "cumulativeQuantityPerUnit" s))
            | Object s <- toList path
            ]
    steps _ = []

spec :: Spec
spec = do
    describe "the links of a chain under a swap" $ do
        it "draw the new supplier where the old one was, with its coefficients" $ do
            chain <- chainUnder [swapAll]
            edgesOf chain
                `shouldBe` M.fromList
                    [ ((cP, rootP), 1.0)
                    , ((dP, rootP), 1.0)
                    , ((bP, cP), 2.0)
                    , ((bP, dP), 3.0)
                    , ((depSupplier, bP), 0.5)
                    ]

        it "give the new supplier, and the dependency process above it, their depth" $ do
            chain <- chainUnder [swapAll]
            depthsOf chain `shouldBe` M.fromList [(cP, 1), (dP, 1), (bP, 2), (depSupplier, 3)]

        it "keep the old supplier's link where a swap on one consumer leaves it" $ do
            chain <- chainUnder [swapAtC]
            M.keys (edgesOf chain) `shouldMatchList` [(cP, rootP), (dP, rootP), (bP, cP), (aP, dP), (depSupplier, bP)]

        it "are the loaded ones with no swap" $ do
            chain <- chainUnder []
            edgesOf chain `shouldBe` M.fromList [((cP, rootP), 1.0), ((dP, rootP), 1.0), ((aP, cP), 2.0), ((aP, dP), 3.0)]

    describe "movedTriples" $ do
        let link = TechLink
        it "leaves the links untouched when nothing moved" $
            movedTriples M.empty (dbTechnosphereTriples swapRoot) `shouldBe` dbTechnosphereTriples swapRoot
        it "drops a pair written twice once its moves cancel it, rounding and all" $
            movedTriples (M.fromList [(link 1 2, -0.3), (link 1 3, 0.3)]) (U.fromList [SparseTriple 2 1 0.1, SparseTriple 2 1 0.2])
                `shouldBe` U.fromList [SparseTriple 3 1 0.3]
        it "draws no link from a process to itself" $
            movedTriples (M.fromList [(link 3 3, 1.0)]) U.empty `shouldBe` U.empty
        it "keeps a pair whose moves net to nothing, as a swap of a supplier for itself does" $
            movedTriples (M.fromList [(link 1 2, 0)]) (U.fromList [SparseTriple 2 1 2.0]) `shouldBe` U.fromList [SparseTriple 2 1 2.0]

    describe "the path under a swap" $ do
        it "reaches the new supplier, with the quantity the swapped chain lists" $ do
            answer <- pathUnder [swapAll] bP
            fmap (\s -> (length s, last s)) answer `shouldBe` Right (3, (String bP, Number 5))

        it "continues into the dependency the new supplier buys from" $ do
            answer <- pathUnder [swapAll] depSupplier
            fmap (\s -> (length s, last s)) answer `shouldBe` Right (4, (String depSupplier, Number 2.5))

        it "finds no path to the supplier swapped out" $
            pathUnder [swapAll] aP `shouldReturn` Left 404

        it "is the published one with no swap" $ do
            answer <- pathUnder [] aP
            fmap (\s -> (length s, last s)) answer `shouldBe` Right (3, (String aP, Number 5))
