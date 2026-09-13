{-# LANGUAGE OverloadedStrings #-}

{- | Shared synthetic two-DB fixture for cross-DB regional LCIA tests.

Two databases linked by a 'CrossDBLink' such that the only biosphere
emission with a regional CF lives in the dep DB. Reused by:

* 'CrossDBRegionalLCIASpec'   - the original gap-and-fix spec
* 'CrossDBRegionalLCIASubsSpec' - substitution path parity / fan-out

Topology built by 'mkRegionalFixture':

* Root DB R: 1 activity at FR, no biosphere emission. Cross-DB link to
  dep DB D's activity #1 (the one at DE), coefficient 1.0.
* Dep DB D: 3 activities at FR, DE, GLO. D's activity #1 (DE) emits
  1.0 kg of the shared biosphere flow F.
* Method tables M: regional CFs on F: FR=1, DE=5, GLO=0.5.

Demanding R's activity #0 forces 1 unit of D's activity #1 (DE) →
cross-DB regional score = 0 + 1·CF[F,DE] = 5.
-}
module CrossDBRegionalLCIAFixture (
    -- * Topology builders
    mkDB,
    linkAt,

    -- * UUID helpers
    mkUUID,
    actUUID,
    prodUUID,
    flowUUID,

    -- * Method-table builders
    regionalMappings,
    buildTables,

    -- * Units / flow / config used by the fixture
    kgUnit,
    kgUnitConfig,
    testFlow,

    -- * Bundled fixture
    RegionalFixture (..),
    mkRegionalFixture,
) where

import Data.Int (Int32)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Method.Mapping (MatchStrategy (..), MethodTables, buildMethodTables, fillBroadcastVector, fillRegionalActivityWeights)
import Method.Types (CFFamily (..), FlowDirection (..), MethodCF (..))
import TestHelpers (unitDef)
import Types
import qualified Types as VT
import UnitConversion (UnitConfig (..), defaultDimensionOrder, defaultUnitConfig, mkUnitConfig)

-- | One biosphere flow shared by both DBs (cross-DB merging keys on UUID).
flowUUID :: UUID
flowUUID = mkUUID 1

-- | Activity UUIDs are offset-based so root and dep don't collide.
actUUID :: Int -> UUID
actUUID i = mkUUID (toInteger (1000 + i))

prodUUID :: Int -> UUID
prodUUID i = mkUUID (toInteger (2000 + i))

mkUUID :: Integer -> UUID
mkUUID n = UUID.fromWords64 (fromIntegral n) 0

kgUnit :: Unit
kgUnit = Unit{unitId = mkUUID 9, unitName = "kg", unitSymbol = "kg", unitComment = ""}

kgUnitConfig :: UnitConfig
kgUnitConfig =
    mkUnitConfig
        defaultDimensionOrder
        (M.fromList [("kg", unitDef "mass" 1.0)])

testFlow :: BiosphereFlow
testFlow =
    BiosphereFlow
        { bfId = flowUUID
        , bfName = "Carbon dioxide"
        , bfUnitId = unitId kgUnit
        , bfSynonyms = M.empty
        , bfCAS = Nothing
        , bfSubstanceId = Nothing
        , bfCompartment = Just (VT.Compartment Air Nothing)
        }

mkActivity :: Int -> Text -> Activity
mkActivity _ loc =
    Activity
        { activityName = "act-" <> loc
        , activityDescription = []
        , activityDocumentation = []
        , activitySynonyms = M.empty
        , activityClassification = M.empty
        , activityLocation = loc
        , activityLocationSource = LocationDeclared
        , activityUnit = "kg"
        , exchanges = []
        , activityParams = M.empty
        , activityParamExprs = M.empty
        , activityNativeType = Nothing
        , activityNativeId = Nothing
        , activityFormulaCheck = Nothing
        }

emptyIndexes :: Indexes
emptyIndexes = Indexes M.empty M.empty M.empty M.empty

{- | Build a synthetic single-DB fixture parameterized by:
* @offset@: starting index for activity UUIDs (so root and dep don't collide)
* @locs@: locations for each activity, length determines activity count
* @bioTriples@: (flowRow=0, activityCol, value) sparse entries to emit on
  the single biosphere flow @flowUUID@. Empty for DBs that emit nothing.

The technosphere matrix is left empty - the MUMPS layer adds the identity
on each diagonal so (I - 0)·x = d trivially yields x = d.
-}
mkDB :: Int -> [Text] -> [(Int, Double)] -> Database
mkDB offset locs bioTriples =
    let n = length locs
        activities = V.fromList [mkActivity (offset + i) loc | (i, loc) <- zip [0 ..] locs]
        actIdx = V.fromList [fromIntegral i :: Int32 | i <- [0 .. n - 1]]
        triples = U.fromList [SparseTriple 0 (fromIntegral c) v | (c, v) <- bioTriples]
        procIds = V.fromList [(actUUID (offset + i), prodUUID (offset + i)) | i <- [0 .. n - 1]]
        procIdLookup =
            M.fromList
                [((actUUID (offset + i), prodUUID (offset + i)), fromIntegral i :: Int32) | i <- [0 .. n - 1]]
     in Database
            { dbProcessIdTable = procIds
            , dbProcessIdLookup = procIdLookup
            , dbActivityUUIDIndex = M.empty
            , dbProductIndex = emptyProductIndex
            , dbActivities = activities
            , dbTechFlows = M.empty
            , dbBioFlows = M.singleton flowUUID testFlow
            , dbWasteFlows = M.empty
            , dbUnits = M.singleton (unitId kgUnit) kgUnit
            , dbIndexes = emptyIndexes
            , dbTechnosphereTriples = U.empty
            , dbBiosphereTriples = triples
            , dbActivityIndex = actIdx
            , dbBiosphereOrder = V.singleton flowUUID
            , dbActivityCount = fromIntegral n
            , dbBiosphereCount = 1
            , dbCrossDBLinks = []
            , dbDependsOn = []
            , dbLinkingStats = mempty
            , dbBuiltWith = BuildInputs defaultUnitConfig mempty Declared
            , dbSynonymDB = Nothing
            , dbFlowsByName = M.empty
            , dbFlowsByCAS = M.empty
            , dbProductSearchIndex = M.empty
            , dbBM25Index = Nothing
            }

{- | Add a 'CrossDBLink' from the first activity of @consumerDb@ to the
named activity in @supplierDb@ (identified by its index inside the supplier's
'dbProcessIdTable').
-}
linkAt ::
    Database ->
    Database ->
    Text ->
    -- | supplier activity index in supplierDb
    Int ->
    -- | coefficient (mass moved across the link)
    Double ->
    Database
linkAt consumerDb supplierDb supplierName supIdx coeff =
    let (consumerAct, consumerProd) = dbProcessIdTable consumerDb V.! 0
        (supplierAct, supplierProd) = dbProcessIdTable supplierDb V.! supIdx
        link =
            CrossDBLink
                { cdlConsumerActUUID = consumerAct
                , cdlConsumerProdUUID = consumerProd
                , cdlConsumerFlowId = UUID.nil
                , cdlSupplierActUUID = supplierAct
                , cdlSupplierProdUUID = supplierProd
                , cdlCoefficient = coeff
                , cdlExchangeUnit = "kg"
                , cdlFlowName = "x-link"
                , cdlLocation = activityLocation (dbActivities supplierDb V.! supIdx)
                , cdlSourceDatabase = supplierName
                , cdlTiedAlternatives = []
                }
     in consumerDb{dbCrossDBLinks = link : dbCrossDBLinks consumerDb}

regionalMappings :: [(Text, Double)] -> [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))]
regionalMappings = map (\(loc, v) -> (cf loc v, Just (testFlow, ByName)))
  where
    cf loc v =
        MethodCF
            { mcfFlowRef = flowUUID
            , mcfFlowName = "Carbon dioxide"
            , mcfDirection = Output
            , mcfValue = v
            , mcfCompartment = Nothing
            , mcfCAS = Nothing
            , mcfUnit = "kg"
            , mcfConsumerLocation = Just loc
            }

buildTables :: Database -> [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))] -> MethodTables
buildTables db mappings =
    let raw = buildMethodTables OtherCFFamily M.empty M.empty mappings
        withBroadcast = fillBroadcastVector kgUnitConfig (dbUnits db) (dbBioFlows db) raw
     in fillRegionalActivityWeights
            kgUnitConfig
            (dbUnits db)
            (dbBioFlows db)
            db
            M.empty
            withBroadcast

{- | The canonical two-DB fixture used across cross-DB regional specs.

@regional cross-DB score = w_R·x_R + w_D·x_D = 0 + 5 = 5@ when demanding
root activity #0 (which forces 1 unit of dep activity #1 at DE).
-}
data RegionalFixture = RegionalFixture
    { rfRootDb :: Database
    , rfDepDb :: Database
    , rfRootTables :: MethodTables
    , rfDepTables :: MethodTables
    }

mkRegionalFixture :: RegionalFixture
mkRegionalFixture =
    let dep = mkDB 1 ["FR", "DE", "GLO"] [(1, 1.0)] -- emission only on the DE column
        rootBase = mkDB 100 ["FR"] []
        root = linkAt rootBase dep "dep" 1 1.0
        mappings = regionalMappings [("FR", 1), ("DE", 5), ("GLO", 0.5)]
     in RegionalFixture
            { rfRootDb = root
            , rfDepDb = dep
            , rfRootTables = buildTables root mappings
            , rfDepTables = buildTables dep mappings
            }
