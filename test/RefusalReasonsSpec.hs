{-# LANGUAGE OverloadedStrings #-}

{- | One product, two causes, through the linker that produces them.

Every other check of the missing-supplier list writes the refusals onto the
stats by hand, which proves what the page does with two causes but not that two
ever arrive. This one demands the same product twice of a background that
answers neither demand, once in a unit it does not ship and once at a location
its policy refuses, and reads the page at the far end.
-}
module RefusalReasonsSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Test.Hspec

import Config (DatabaseConfig (..))
import Database (buildDatabaseWithMatrices)
import Database.CrossLinking (buildIndexedDatabase, emptyAliasMap)
import Database.Loader (relinkSimpleDatabase)
import Database.Manager (DatabaseSetupInfo (..), MissingSupplier (..), buildLoadedSetupInfo)
import SynonymDB (emptySynonymDB)
import Types
import UnitConversion (defaultUnitConfig)

spec :: Spec
spec = do
    built <- runIO (buildDatabaseWithMatrices (BuildInputs defaultUnitConfig mempty Declared) consumerDB)
    describe "a product two demands were refused for two different reasons" $ case built of
        Left err ->
            it "builds its fixture" $ expectationFailure ("the consumer fixture did not build: " <> show err)
        Right db -> do
            let rows = [r | r <- dsiMissingSuppliers (pageOf db), msProductName r == "wheat"]

            it "names the product once per cause" $
                length rows `shouldBe` 2

            it "keeps each cause with the demands it refused" $
                [(msReason r, msCount r) | r <- rows]
                    `shouldBe` [("location_rejected", 1), ("unit_incompatible", 1)]

{- | The setup page of the consumer, carrying the refusals of one relink against
the background. 'buildLoadedSetupInfo' reads those off the record, so writing
them on is enough to reach the page.
-}
pageOf :: Database -> DatabaseSetupInfo
pageOf db = buildLoadedSetupInfo stubConfig db{dbLinkingStats = stats} M.empty M.empty

-- | What one relink under an exact-match geography policy refused, and why.
stats :: CrossDBLinkingStats
stats =
    relinkSimpleDatabase
        [buildIndexedDatabase "background" emptySynonymDB supplierDB]
        emptySynonymDB
        defaultUnitConfig
        M.empty
        GeoExact
        emptyAliasMap
        consumerDB

-- ---------------------------------------------------------------------------
-- The two databases
-- ---------------------------------------------------------------------------

{- | Bread baking at FR, asking twice for wheat: once by volume where the
background sells it by mass, once at a location the background does not serve.
-}
consumerDB :: SimpleDatabase
consumerDB =
    SimpleDatabase
        { sdbActivities = M.singleton (actBread, breadFlow) baking
        , sdbTechFlows =
            M.fromList
                [ (breadFlow, techFlow breadFlow "bread" kgUnit)
                , (wheatByVolume, techFlow wheatByVolume "wheat" m3Unit)
                , (wheatByMass, techFlow wheatByMass "wheat" kgUnit)
                ]
        , sdbBioFlows = M.empty
        , sdbWasteFlows = M.empty
        , sdbUnits = unitTable
        }
  where
    baking :: Activity
    baking =
        activityAt
            "bread baking"
            "FR"
            [ (techInput breadFlow kgUnit "FR"){techRole = ReferenceProduct}
            , -- The background ships wheat at GLO, so this demand clears
              -- geography and dies on the unit.
              techInput wheatByVolume m3Unit "GLO"
            , -- ... and this one is refused before the unit is ever read.
              techInput wheatByMass kgUnit "FR"
            ]

-- | The background: wheat at GLO, by mass, and nothing else.
supplierDB :: SimpleDatabase
supplierDB =
    SimpleDatabase
        { sdbActivities = M.singleton (actGrower, wheatByMass) growing
        , sdbTechFlows = M.singleton wheatByMass (techFlow wheatByMass "wheat" kgUnit)
        , sdbBioFlows = M.empty
        , sdbWasteFlows = M.empty
        , sdbUnits = unitTable
        }
  where
    growing :: Activity
    growing = activityAt "wheat growing" "GLO" [(techInput wheatByMass kgUnit "GLO"){techRole = ReferenceProduct}]

-- ---------------------------------------------------------------------------
-- Fixture building blocks
-- ---------------------------------------------------------------------------

u :: String -> UUID
u suffix = read ("00000000-0000-0000-0000-0000000000" <> suffix)

kgUnit, m3Unit, breadFlow, wheatByVolume, wheatByMass, actBread, actGrower :: UUID
kgUnit = u "01"
m3Unit = u "02"
breadFlow = u "03"
wheatByVolume = u "04"
wheatByMass = u "05"
actBread = u "0a"
actGrower = u "0b"

unitTable :: M.Map UUID Unit
unitTable =
    M.fromList
        [ (kgUnit, Unit kgUnit "kg" "kg" "")
        , (m3Unit, Unit m3Unit "m3" "m3" "")
        ]

activityAt :: Text -> Text -> [Exchange] -> Activity
activityAt name location exs =
    Activity
        { activityName = name
        , activityDescription = []
        , activityDocumentation = []
        , activitySynonyms = M.empty
        , activityClassification = M.empty
        , activityLocation = location
        , activityLocationSource = LocationDeclared
        , activityUnit = "kg"
        , exchanges = exs
        , activityParams = M.empty
        , activityParamExprs = M.empty
        , activityNativeType = Nothing
        , activityNativeId = Nothing
        , activityFormulaCheck = Nothing
        }

techFlow :: UUID -> Text -> UUID -> TechnosphereFlow
techFlow fid name unitId =
    TechnosphereFlow
        { tfId = fid
        , tfName = name
        , tfUnitId = unitId
        , tfSynonyms = M.empty
        , tfCAS = Nothing
        , tfSubstanceId = Nothing
        }

techInput :: UUID -> UUID -> Text -> Exchange
techInput fid unitId location =
    TechnosphereExchange
        { techFlowId = fid
        , techAmount = 1.0
        , techUnitId = unitId
        , techRole = Input
        , techActivityLinkId = Nothing
        , techSupplierClaim = ClaimByProduct
        , techLocation = location
        , techComment = Nothing
        , techPedigree = Nothing
        , techShare = Nothing
        , techClassification = M.empty
        , techProperties = noProperties
        }

stubConfig :: DatabaseConfig
stubConfig =
    DatabaseConfig
        { dcName = "consumer"
        , dcDisplayName = "Consumer"
        , dcPath = ""
        , dcDescription = Nothing
        , dcLoad = True
        , dcDefault = False
        , dcDepends = []
        , dcLocationAliases = M.empty
        , dcFormat = Nothing
        , dcIsUploaded = False
        , dcDeletable = False
        , dcGeographyPolicy = GeoExact
        , dcAllocation = Declared
        , dcSource = Nothing
        }
