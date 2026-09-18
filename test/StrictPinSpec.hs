{-# LANGUAGE OverloadedStrings #-}

{- | Regression test for the strict-dependency-pin behavior of
'Database.Manager.relinkDatabase'.

Before the fix, relink re-discovered cross-DB links against *every* loaded
database and reset 'dbDependsOn' to whatever produced a link. A database
opened from cache could therefore never have its dependency set reduced:
each relink re-expanded it (and rewrote the cache). This was the GINKO
symptom: pinning a consumer to a single Agribalyse version was impossible
while other versions stayed loaded.

The fix makes 'dbDependsOn' authoritative: relink restricts its candidate
suppliers to the pinned set and never grows or shrinks that set. This test
pins a consumer to @["alpha"]@ while *both* alpha and beta are indexed, and
gives the consumer an input that only beta can supply. The pre-fix code
would expand the pin to @["alpha","beta"]@; the fix keeps it at @["alpha"]@
and leaves the beta-only input unresolved (surfaced, not silently linked).
-}
module StrictPinSpec (spec) where

import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.UUID as UUID
import Test.Hspec

import Config (DatabaseConfig (..), defaultConfig)
import qualified Data.Vector.Unboxed as U
import Database (buildDatabaseWithMatrices)
import Database.CrossLinking (AliasKey (..), AliasMap (..), AliasTarget (..), buildIndexedDatabaseFromDB)
import Database.Manager (
    CachePolicy (..),
    DatabaseManager (..),
    LoadedDatabase (..),
    initDatabaseManager,
    relinkDatabase,
    relinkDatabaseWithMapping,
 )
import SharedSolver (SharedSolver, createSharedSolver)
import SynonymDB (emptySynonymDB)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Types (
    Activity (..),
    AllocationKey (..),
    BuildInputs (..),
    CrossDBLink (..),
    Database (..),
    Exchange (..),
    GeographyPolicy (..),
    LocationSource (..),
    SimpleDatabase (..),
    SparseTriple (..),
    SupplierClaim (..),
    TechRole (..),
    TechnosphereFlow (..),
    UUID,
    Unit (..),
    noProperties,
 )
import UnitConversion (defaultUnitConfig)

spec :: Spec
spec = describe "relinkDatabase strict dependency pin" $ do
    it "keeps dbDependsOn at the pinned set and never re-expands to other loaded DBs" $
        withSystemTempDirectory "volca-strict-pin" $ \tmp -> do
            -- alpha supplies p1 only; beta supplies p1 and p2.
            alphaDb <- buildOrFail (supplierDB 100 ["p1"])
            betaDb <- buildOrFail (supplierDB 200 ["p1", "p2"])
            -- consumer needs both p1 and p2.
            consumerDb0 <- buildOrFail (consumerDB 300 ["p1", "p2"])
            -- Pin the consumer to alpha only, with no links yet, relink populates them.
            let consumerDb = consumerDb0{dbDependsOn = ["alpha"], dbCrossDBLinks = []}

            manager <- initDatabaseManager defaultConfig NoCache
            solver <- mkSolver "consumer" consumerDb
            let consumerLoaded =
                    LoadedDatabase
                        { ldDatabase = consumerDb
                        , ldSharedSolver = solver
                        , ldConfig = consumerConfig (tmp </> "consumer-data")
                        }
            atomically $ do
                modifyTVar' (dmLoadedDbs manager) (M.insert "consumer" consumerLoaded)
                modifyTVar' (dmIndexedDbs manager) $
                    M.insert "alpha" (buildIndexedDatabaseFromDB "alpha" emptySynonymDB alphaDb)
                        . M.insert "beta" (buildIndexedDatabaseFromDB "beta" emptySynonymDB betaDb)

            result <- relinkDatabase manager "consumer"
            result `shouldSatisfy` isRight

            loaded <- readTVarIO (dmLoadedDbs manager)
            let relinked = ldDatabase (loaded M.! "consumer")
                linkSources = S.fromList (map cdlSourceDatabase (dbCrossDBLinks relinked))

            -- The pin is preserved exactly: beta is NOT added even though it is
            -- loaded and is the only supplier of p2.
            dbDependsOn relinked `shouldBe` ["alpha"]
            -- Every resolved link points into the pinned DB; beta never leaks in.
            linkSources `shouldSatisfy` (`S.isSubsetOf` S.fromList ["alpha"])
            -- p1 resolves against alpha; p2 (beta-only) stays unresolved.
            length (dbCrossDBLinks relinked) `shouldBe` 1

    it "a mapping relink against one dependency preserves links to the others" $
        withSystemTempDirectory "volca-relink-multidep" $ \tmp -> do
            -- alpha supplies p1, beta supplies p2; consumer is pinned to both.
            alphaDb <- buildOrFail (supplierDB 100 ["p1"])
            betaDb <- buildOrFail (supplierDB 200 ["p2"])
            consumerDb0 <- buildOrFail (consumerDB 300 ["p1", "p2"])
            let consumerDb = consumerDb0{dbDependsOn = ["alpha", "beta"], dbCrossDBLinks = []}

            manager <- initDatabaseManager defaultConfig NoCache
            consumerSolver <- mkSolver "consumer" consumerDb
            alphaSolver <- mkSolver "alpha" alphaDb
            betaSolver <- mkSolver "beta" betaDb
            -- The deps must be loaded (mapping relink requires depDb loaded) and
            -- indexed (the matcher scans indexed deps).
            atomically $ do
                modifyTVar' (dmLoadedDbs manager) $
                    M.insert "consumer" (loadedFor consumerDb consumerSolver (consumerConfig (tmp </> "consumer-data")))
                        . M.insert "alpha" (loadedFor alphaDb alphaSolver (supplierLoadedConfig "alpha"))
                        . M.insert "beta" (loadedFor betaDb betaSolver (supplierLoadedConfig "beta"))
                modifyTVar' (dmIndexedDbs manager) $
                    M.insert "alpha" (buildIndexedDatabaseFromDB "alpha" emptySynonymDB alphaDb)
                        . M.insert "beta" (buildIndexedDatabaseFromDB "beta" emptySynonymDB betaDb)

            -- Populate the links first: p1 → alpha, p2 → beta.
            _ <- relinkDatabase manager "consumer"
            -- A mapping relink scoped to beta must re-resolve the whole pin, not
            -- drop the alpha link. (The alias is inert here; it only exercises the
            -- mapping path.)
            result <-
                relinkDatabaseWithMapping
                    manager
                    "consumer"
                    "beta"
                    (AliasMap (M.singleton (AliasKey "no-such-input" Nothing) (AliasTarget "no-such-supplier" Nothing)))
            result `shouldSatisfy` isRight

            loaded <- readTVarIO (dmLoadedDbs manager)
            let relinked = ldDatabase (loaded M.! "consumer")
                linkSources = S.fromList (map cdlSourceDatabase (dbCrossDBLinks relinked))
            -- Both dependency links survive; the alpha link is not silently dropped.
            linkSources `shouldBe` S.fromList ["alpha", "beta"]
            length (dbCrossDBLinks relinked) `shouldBe` 2

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _) = False

loadedFor :: Database -> SharedSolver -> DatabaseConfig -> LoadedDatabase
loadedFor db solver cfg =
    LoadedDatabase{ldDatabase = db, ldSharedSolver = solver, ldConfig = cfg}

-- | A minimal loaded-supplier config (no own dependencies, no cache path).
supplierLoadedConfig :: Text -> DatabaseConfig
supplierLoadedConfig name =
    (consumerConfig ""){dcName = name, dcDisplayName = name, dcDepends = []}

buildOrFail :: SimpleParts -> IO Database
buildOrFail (SimpleParts acts flows units) = do
    r <-
        buildDatabaseWithMatrices
            (BuildInputs defaultUnitConfig mempty Declared [])
            SimpleDatabase
                { sdbActivities = acts
                , sdbTechFlows = flows
                , sdbBioFlows = M.empty
                , sdbWasteFlows = M.empty
                , sdbUnits = units
                }
    case r of
        Right db -> pure db
        Left err -> fail ("buildDatabaseWithMatrices: " <> show err)

mkSolver :: Text -> Database -> IO SharedSolver
mkSolver name db =
    let triples = [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList (dbTechnosphereTriples db)]
     in createSharedSolver name triples (fromIntegral (dbActivityCount db))

consumerConfig :: FilePath -> DatabaseConfig
consumerConfig path =
    DatabaseConfig
        { dcName = "consumer"
        , dcDisplayName = "consumer"
        , dcPath = path -- relink writes a cache next to this path when links change
        , dcDescription = Nothing
        , dcLoad = True
        , dcDefault = False
        , dcDepends = ["alpha"]
        , dcLocationAliases = M.empty
        , dcFormat = Nothing
        , dcIsUploaded = False
        , dcDeletable = False
        , dcGeographyPolicy = GeoGlobal
        , dcAllocation = Declared
        , dcPatches = []
        , dcSource = Nothing
        }

-- ---------------------------------------------------------------------------
-- Fixture builders (in-memory, single unit "kg", all activities at GLO)
-- ---------------------------------------------------------------------------

data SimpleParts
    = SimpleParts
        (M.Map (UUID, UUID) Activity)
        (M.Map UUID TechnosphereFlow)
        (M.Map UUID Unit)

mkUUID :: Int -> UUID
mkUUID n = UUID.fromWords64 (fromIntegral n) 0

kgUnitId :: UUID
kgUnitId = mkUUID 0

kgUnit :: Unit
kgUnit = Unit{unitId = kgUnitId, unitName = "kg", unitSymbol = "kg", unitComment = ""}

mkTechFlow :: UUID -> Text -> TechnosphereFlow
mkTechFlow fid name =
    TechnosphereFlow
        { tfId = fid
        , tfName = name
        , tfUnitId = kgUnitId
        , tfSynonyms = M.empty
        , tfCAS = Nothing
        , tfSubstanceId = Nothing
        }

{- | A supplier DB: one activity per product name, each producing that
product (reference output) at GLO. 'offset' keeps UUIDs disjoint across DBs.
-}
supplierDB :: Int -> [Text] -> SimpleParts
supplierDB offset products =
    let entries =
            [ let actUUID = mkUUID (offset + 10 * i + 1)
                  prodUUID = mkUUID (offset + 10 * i + 2)
                  flowUUID = mkUUID (offset + 10 * i + 3)
                  flow = mkTechFlow flowUUID name
                  refOut =
                    TechnosphereExchange
                        { techFlowId = flowUUID
                        , techAmount = 1.0
                        , techUnitId = kgUnitId
                        , techRole = ReferenceProduct
                        , techActivityLinkId = Just actUUID
                        , techSupplierClaim = ClaimByProduct
                        , techLocation = ""
                        , techComment = Nothing
                        , techPedigree = Nothing
                        , techShare = Nothing
                        , techClassification = M.empty
                        , techProperties = noProperties
                        }
                  act =
                    Activity
                        { activityName = "supplier-of-" <> name
                        , activityDescription = []
                        , activityDocumentation = []
                        , activitySynonyms = M.empty
                        , activityClassification = M.empty
                        , activityLocation = "GLO"
                        , activityLocationSource = LocationDeclared
                        , activityUnit = "kg"
                        , exchanges = [refOut]
                        , activityParams = M.empty
                        , activityParamExprs = M.empty
                        , activityNativeType = Nothing
                        , activityNativeId = Nothing
                        , activityFormulaCheck = Nothing
                        }
               in (((actUUID, prodUUID), act), (flowUUID, flow))
            | (i, name) <- zip [0 ..] products
            ]
     in SimpleParts
            (M.fromList (map fst entries))
            (M.fromList (map snd entries))
            (M.singleton kgUnitId kgUnit)

{- | A consumer DB: one activity per required product, each with a reference
output and one unlinked technosphere input for that product (triggers the
cross-DB lookup).
-}
consumerDB :: Int -> [Text] -> SimpleParts
consumerDB offset products =
    let entries =
            [ let actUUID = mkUUID (offset + 10 * i + 1)
                  prodUUID = mkUUID (offset + 10 * i + 2)
                  inFlowUUID = mkUUID (offset + 10 * i + 3)
                  outFlowUUID = mkUUID (offset + 10 * i + 4)
                  inFlow = mkTechFlow inFlowUUID name
                  outFlow = mkTechFlow outFlowUUID ("consumer-out-" <> name)
                  refOut =
                    TechnosphereExchange
                        { techFlowId = outFlowUUID
                        , techAmount = 1.0
                        , techUnitId = kgUnitId
                        , techRole = ReferenceProduct
                        , techActivityLinkId = Just actUUID
                        , techSupplierClaim = ClaimByProduct
                        , techLocation = ""
                        , techComment = Nothing
                        , techPedigree = Nothing
                        , techShare = Nothing
                        , techClassification = M.empty
                        , techProperties = noProperties
                        }
                  unlinkedInput =
                    TechnosphereExchange
                        { techFlowId = inFlowUUID
                        , techAmount = 1.0
                        , techUnitId = kgUnitId
                        , techRole = Input
                        , techActivityLinkId = Nothing
                        , techSupplierClaim = ClaimByProduct
                        , techLocation = "GLO"
                        , techComment = Nothing
                        , techPedigree = Nothing
                        , techShare = Nothing
                        , techClassification = M.empty
                        , techProperties = noProperties
                        }
                  act =
                    Activity
                        { activityName = "consumer-" <> name
                        , activityDescription = []
                        , activityDocumentation = []
                        , activitySynonyms = M.empty
                        , activityClassification = M.empty
                        , activityLocation = "GLO"
                        , activityLocationSource = LocationDeclared
                        , activityUnit = "kg"
                        , exchanges = [refOut, unlinkedInput]
                        , activityParams = M.empty
                        , activityParamExprs = M.empty
                        , activityNativeType = Nothing
                        , activityNativeId = Nothing
                        , activityFormulaCheck = Nothing
                        }
               in (((actUUID, prodUUID), act), [(inFlowUUID, inFlow), (outFlowUUID, outFlow)])
            | (i, name) <- zip [0 ..] products
            ]
     in SimpleParts
            (M.fromList (map fst entries))
            (M.fromList (concatMap snd entries))
            (M.singleton kgUnitId kgUnit)
