{-# LANGUAGE OverloadedStrings #-}

{- | A database has to characterize the flows it reaches, not only the ones it
owns.

An activity whose inventory comes from a dependency database carries that
dependency's biosphere flows: scoring reads the merged inventory of the whole
cross-database solve. The mapping cascade, though, used to be built on the root
database's own flows alone, so a dependency's flow reached only the rungs that
need no flow to point at: never the synonym bridge, the proxy edges or the
regional projection. The inventory was right, the factors were missing, and
nothing said so.
-}
module DependencyFlowClosureSpec (spec) where

import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Test.Hspec

import Config (DatabaseConfig (..), defaultConfig)
import qualified Database.Manager as DM
import Method.Mapping (
    LongTermMode (..),
    MatchStrategy (..),
    computeRegionalizedLCIAScore,
    mtRegionalizedCF,
 )
import Method.Types (Method (..), MethodCF (..))
import qualified Method.Types as MT
import qualified SharedSolver as SS
import SynonymDB (buildFromPairs)
import Types

import CrossDBRegionalLCIAFixture (buildTables, kgUnit, kgUnitConfig, mkDB, mkUUID)

collection :: DM.CollectionName
collection = DM.CollectionName "EF-3.1"

-- | The flow only the dependency database owns.
riverWater :: BiosphereFlow
riverWater =
    BiosphereFlow
        { bfId = mkUUID 501
        , bfName = "Water, river"
        , bfUnitId = unitId kgUnit
        , bfSynonyms = M.empty
        , bfCAS = Nothing
        , bfSubstanceId = Nothing
        , bfCompartment = Just (Compartment NaturalResource Nothing)
        }

-- | A flow the root database owns itself, unrelated to the dependency's.
methane :: BiosphereFlow
methane =
    BiosphereFlow
        { bfId = mkUUID 502
        , bfName = "Methane"
        , bfUnitId = unitId kgUnit
        , bfSynonyms = M.empty
        , bfCAS = Nothing
        , bfSubstanceId = Nothing
        , bfCompartment = Just (Compartment Air Nothing)
        }

{- | The root's own flow under the name the dependency also uses, with a lower
UUID so the merged map iterates it before the dependency's homonym.
-}
rootRiverWater :: BiosphereFlow
rootRiverWater = riverWater{bfId = mkUUID 500}

{- | A CF named for the method's own spelling of the substance. Only the synonym
bridge ties it to a flow, so it resolves nothing unless the flow is in reach.
-}
namedCF :: Text -> Double -> MethodCF
namedCF name value =
    MethodCF
        { mcfFlowRef = mkUUID 0
        , mcfFlowName = name
        , mcfDirection = MT.Input
        , mcfValue = value
        , mcfCompartment = Nothing
        , mcfCAS = Nothing
        , mcfUnit = "kg"
        , mcfConsumerLocation = Nothing
        }

mkMethod :: Text -> [MethodCF] -> Method
mkMethod name factors =
    Method
        { methodId = mkUUID 7001
        , methodName = name
        , methodDescription = Nothing
        , methodUnit = "kg"
        , methodCategory = name
        , methodMethodology = Nothing
        , methodFactors = factors
        }

-- | Replace a fixture database's own biosphere flows, rebuilding its indexes.
withOwnFlows :: [BiosphereFlow] -> Database -> Database
withOwnFlows flows db =
    addFlowNameIndexToDatabase db{dbBioFlows = M.fromList [(bfId f, f) | f <- flows]}

dbConfigFor :: Text -> DatabaseConfig
dbConfigFor name =
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
        }

-- | Install a database in the manager's loaded set, solver and config included.
install :: DM.DatabaseManager -> Text -> Database -> IO ()
install manager name db = do
    solver <-
        SS.createSharedSolver
            name
            [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList (dbTechnosphereTriples db)]
            (fromIntegral (dbActivityCount db))
    atomically $
        modifyTVar' (DM.dmLoadedDbs manager) $
            M.insert
                name
                DM.LoadedDatabase
                    { DM.ldDatabase = db
                    , DM.ldSharedSolver = solver
                    , DM.ldConfig = dbConfigFor name
                    }

-- | The flows the effective mappings actually reach.
reachedFlows :: [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))] -> [UUID]
reachedFlows mappings = [bfId f | (_, Just (f, _)) <- mappings]

{- | Root and dependency, both loaded. The root owns @rootFlows@ and declares
the dependency; the dependency owns 'riverWater'.
-}
setup :: [BiosphereFlow] -> IO (DM.DatabaseManager, Database)
setup rootFlows = do
    manager <- DM.initDatabaseManager defaultConfig DM.NoCache
    let dep = withOwnFlows [riverWater] (mkDB 1 ["FR"] [])
        root =
            (withOwnFlows rootFlows (mkDB 100 ["FR"] []))
                { dbDependsOn = ["dep"]
                , dbSynonymDB = Just (buildFromPairs [("river water", "Water, river")])
                }
    install manager "dep" dep
    install manager "root" root
    pure (manager, root)

spec :: Spec
spec = do
    describe "characterization over a database's flow closure" $ do
        it "reaches a dependency's flow through the synonym bridge" $ do
            (manager, root) <- setup []
            mappings <-
                DM.effectiveMethodMappings manager "root" collection root $
                    mkMethod "Water use" [namedCF "river water" 6.98]
            reachedFlows mappings `shouldContain` [bfId riverWater]

        it "leaves the root's own resolution alone when it also has dependencies" $ do
            (managerAlone, rootAlone) <- setup [methane]
            -- Same root, dependency removed: the baseline resolution to compare against.
            let solo = rootAlone{dbDependsOn = []}
            soloMappings <-
                DM.effectiveMethodMappings managerAlone "solo" collection solo $
                    mkMethod "Climate change" [namedCF "Methane" 29.8]
            (manager, root) <- setup [methane]
            mappings <-
                DM.effectiveMethodMappings manager "root" collection root $
                    mkMethod "Climate change" [namedCF "Methane" 29.8]
            reachedFlows mappings `shouldBe` reachedFlows soloMappings

        it "binds a name both databases declare to the root's own flow" $ do
            -- pickByCompartment reads its candidates in order, so a name index
            -- rebuilt over the merged map hands the homonym that sorts first (
            -- the dependency's), a factor the root used to resolve itself. The
            -- synonym fan-out downstream reaches both, which is its job; what
            -- the cascade picks here is the root's.
            (manager, root) <- setup [rootRiverWater]
            mappings <-
                DM.mapMethodToFlowsCached manager "root" collection root $
                    mkMethod "Water use" [namedCF "Water, river" 6.98]
            reachedFlows mappings `shouldBe` [bfId rootRiverWater]

        it "judges a flow synonym shared across the root and its dependencies" $ do
            -- Each database holds one of the two flows listing the synonym, so
            -- each alone would keep it; together it names two substances.
            let listingChromium i name =
                    methane{bfId = mkUUID i, bfName = name, bfSynonyms = M.singleton "en" (S.singleton "Chromium")}
                root = withOwnFlows [listingChromium 510 "Chromium III"] (mkDB 100 ["FR"] [])
                dep = withOwnFlows [listingChromium 511 "Chromium VI"] (mkDB 1 ["FR"] [])
            M.member "chromium" (clByName (flowClosure root [dep])) `shouldBe` False

        it "drops a dependent's cached mapping when the dependency changes" $ do
            (manager, root) <- setup []
            _ <-
                DM.effectiveMethodMappings manager "root" collection root $
                    mkMethod "Water use" [namedCF "river water" 6.98]
            before <- readTVarIO (DM.dmMethodMappingCache manager)
            M.keys before `shouldSatisfy` any (\(dn, _, _) -> dn == "root")
            DM.clearMethodMappingCacheForDb manager "dep"
            after <- readTVarIO (DM.dmMethodMappingCache manager)
            M.keys after `shouldSatisfy` all (\(dn, _, _) -> dn /= "root")

    describe "cross-DB regionalized scoring" $
        it "scores a participating database that carries no regional factor, instead of zero" $ do
            -- One activity emitting 1 kg of the flow, and a method whose only
            -- factor is global: the regionalized dispatch still has to count it.
            let db = withOwnFlows [riverWater] (mkDB 1 ["FR"] [(0, 1.0)])
                emitting = db{dbBiosphereOrder = V.singleton (bfId riverWater)}
                mappings =
                    [
                        ( (namedCF "Water, river" 6.98){mcfFlowRef = bfId riverWater}
                        , Just (riverWater, ByUUID)
                        )
                    ]
                tables = buildTables emitting mappings
            -- No regional factor, so 'fillRegionalActivityWeights' leaves the
            -- precomputed weights unfilled and the old code returned 0 here.
            mtRegionalizedCF tables `shouldBe` M.empty
            computeRegionalizedLCIAScore
                kgUnitConfig
                (dbUnits emitting)
                (dbBioFlows emitting)
                IncludeLongTerm
                emitting
                (U.fromList [1.0])
                M.empty
                tables
                `shouldBe` Right 6.98
