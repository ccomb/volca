{-# LANGUAGE OverloadedStrings #-}

{- | Integration test for the minimal-cover staging path.

Exercises the exact sequence that 'Database.Manager.stageUploadedDatabase'
runs when the minimal cover shrinks the dependency set:

    1. link against all candidate supplier DBs
    2. computeMinimalSelectedDeps on the resulting links
    3. re-link restricted to the chosen DBs

The invariant under test: after the restricted re-link, every link's
'cdlSourceDatabase' is a member of the minimal dependency set
(no dangling cross-DB references to dropped DBs).
-}
module MinimalCoverIntegrationSpec (spec) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.UUID as UUID
import Database.CrossLinking (buildIndexedDatabase, locationHierarchy)
import Database.Loader (fixActivityLinksWithCrossDB)
import SynonymDB (emptySynonymDB)
import Test.Hspec
import Types (
    Activity (..),
    CrossDBLink (..),
    CrossDBLinkingStats (..),
    Exchange (..),
    GeographyPolicy (..),
    LocationSource (..),
    SimpleDatabase (..),
    SupplierClaim (..),
    TechRole (..),
    TechnosphereFlow (..),
    UUID,
    Unit (..),
    computeMinimalSelectedDeps,
    crossDBRedundantSources,
    noDates,
    noDocumentation,
    noProperties,
 )
import UnitConversion (defaultUnitConfig)

spec :: Spec
spec = describe "minimal-cover staging integration" $ do
    it "re-links against the minimal cover so every cdlSourceDatabase is in sdSelectedDeps" $ do
        let supplierAlpha = supplierDB 100
            supplierBeta = supplierDB 200
            consumer = consumerDB 300 3 -- 3 unlinked inputs for the shared product
            idxAlpha = buildIndexedDatabase "alpha" emptySynonymDB supplierAlpha
            idxBeta = buildIndexedDatabase "beta" emptySynonymDB supplierBeta

        -- Stage 1: link against both candidate DBs.
        (_, initialStats) <-
            fixActivityLinksWithCrossDB
                [idxAlpha, idxBeta]
                emptySynonymDB
                defaultUnitConfig
                locationHierarchy
                GeoGlobal
                consumer

        -- Both supplier DBs tie on every link: each link records the
        -- non-winner as a tied alternative.
        length (cdlLinks initialStats) `shouldBe` 3
        any (null . cdlTiedAlternatives) (cdlLinks initialStats) `shouldBe` False

        -- Stage 2: minimal cover picks one DB (alphabetical tie-break).
        let minimalDeps = computeMinimalSelectedDeps (cdlLinks initialStats)
        minimalDeps `shouldBe` ["alpha"]
        crossDBRedundantSources (cdlLinks initialStats) minimalDeps `shouldBe` ["beta"]

        -- Stage 3: re-link restricted to the minimal cover.
        let restricted = [idx | (n, idx) <- [("alpha", idxAlpha), ("beta", idxBeta)], n `elem` minimalDeps]
        (_, finalStats) <-
            fixActivityLinksWithCrossDB
                restricted
                emptySynonymDB
                defaultUnitConfig
                locationHierarchy
                GeoGlobal
                consumer

        -- Invariant: every resolved link's source is in the minimal cover.
        length (cdlLinks finalStats) `shouldBe` length (cdlLinks initialStats)
        S.fromList (map cdlSourceDatabase (cdlLinks finalStats))
            `shouldSatisfy` (`S.isSubsetOf` S.fromList minimalDeps)
        -- And no redundant sources remain after the restricted re-link.
        crossDBRedundantSources (cdlLinks finalStats) minimalDeps `shouldBe` []

-- ---------------------------------------------------------------------------
-- Fixture builders
-- ---------------------------------------------------------------------------

mkUUID :: Int -> UUID
mkUUID n = UUID.fromWords64 (fromIntegral n) 0

kgUnitId :: UUID
kgUnitId = mkUUID 0

kgUnit :: Unit
kgUnit = Unit{unitId = kgUnitId, unitName = "kg", unitSymbol = "kg", unitComment = ""}

sharedProductName :: Text
sharedProductName = "shared product"

{- | One supplier DB with a single activity producing 'sharedProductName' at GLO.
The activity carries a reference output exchange, which is what
'buildSupplierEntries' indexes.
-}
supplierDB :: Int -> SimpleDatabase
supplierDB offset =
    let actUUID = mkUUID (offset + 1)
        prodUUID = mkUUID (offset + 2)
        flowUUID = mkUUID (offset + 3)
        techFlow =
            TechnosphereFlow
                { tfId = flowUUID
                , tfName = sharedProductName
                , tfUnitId = kgUnitId
                , tfSynonyms = M.empty
                , tfCAS = Nothing
                , tfSubstanceId = Nothing
                }
        refExchange =
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
                { activityName = "supplier-of-shared-product"
                , activityDescription = []
                , activityDocumentation = []
                , activitySynonyms = M.empty
                , activityClassification = M.empty
                , activityLocation = "GLO"
                , activityLocationSource = LocationDeclared
                , activityUnit = "kg"
                , exchanges = [refExchange]
                , activityParams = M.empty
                , activityParamExprs = M.empty
                , activityNativeType = Nothing
                , activityNativeId = Nothing
                , activityFormulaCheck = Nothing
                , activityDates = noDates
                }
     in SimpleDatabase
            { sdbActivities = M.singleton (actUUID, prodUUID) act
            , sdbTechFlows = M.singleton flowUUID techFlow
            , sdbBioFlows = M.empty
            , sdbWasteFlows = M.empty
            , sdbUnits = M.singleton kgUnitId kgUnit
            , sdbDocumentation = noDocumentation
            }

{- | A consumer DB with @n@ activities, each having one unlinked technosphere
input for the shared product. The flow is in the consumer's own
'sdbTechFlows' (that's where the linker reads the flow name from).
-}
consumerDB :: Int -> Int -> SimpleDatabase
consumerDB offset n =
    let flowUUID = mkUUID (offset + 1)
        techFlow =
            TechnosphereFlow
                { tfId = flowUUID
                , tfName = sharedProductName
                , tfUnitId = kgUnitId
                , tfSynonyms = M.empty
                , tfCAS = Nothing
                , tfSubstanceId = Nothing
                }
        refOutFlowUUID i = mkUUID (offset + 100 + i)
        refOutFlow i =
            TechnosphereFlow
                { tfId = refOutFlowUUID i
                , tfName = "consumer-out"
                , tfUnitId = kgUnitId
                , tfSynonyms = M.empty
                , tfCAS = Nothing
                , tfSubstanceId = Nothing
                }
        mkConsumer i =
            let actUUID = mkUUID (offset + 200 + i)
                prodUUID = mkUUID (offset + 300 + i)
                refOut =
                    TechnosphereExchange
                        { techFlowId = refOutFlowUUID i
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
                        { techFlowId = flowUUID
                        , techAmount = 1.0
                        , techUnitId = kgUnitId
                        , techRole = Input
                        , techActivityLinkId = Nothing -- unlinked → triggers cross-DB lookup
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
                        { activityName = "consumer"
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
                        , activityDates = noDates
                        }
             in ((actUUID, prodUUID), act)
        activities = M.fromList [mkConsumer i | i <- [1 .. n]]
        flows = M.fromList $ (flowUUID, techFlow) : [(refOutFlowUUID i, refOutFlow i) | i <- [1 .. n]]
     in SimpleDatabase
            { sdbActivities = activities
            , sdbTechFlows = flows
            , sdbBioFlows = M.empty
            , sdbWasteFlows = M.empty
            , sdbUnits = M.singleton kgUnitId kgUnit
            , sdbDocumentation = noDocumentation
            }
