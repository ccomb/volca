{-# LANGUAGE OverloadedStrings #-}

{- | Sign correctness for waste-treatment scoring across the two reference
conventions VoLCA's parsers emit:

  * EcoSpold2 (ecoinvent): a treatment's reference is a NEGATIVE technosphere
    output (e.g. -1 kg of the waste it treats) → 'activityNormFactor' = -1.
  * ILCD: a treatment's reference is a POSITIVE 'ReferenceInput' (+1 kg of the
    waste it consumes) → 'activityNormFactor' = +1 (refInputs is abs-summed).

Each convention is self-consistent on its own. Within a single database the
convention is uniform, so the only realistic way to mix them is
cross-database: an ecoinvent-style orphan waste OUTPUT resolved to a
background treatment in another DB via 'findWasteTreatmentAcrossDatabases'.
That path scores through the dep-demand solve ('accumulateDepDemandsWith'),
NOT the static technosphere triples, and the dep-demand path applies no
input/output sign, only 'cdlCoefficient'.

A correctly treated 3 kg of waste at 2 kg CO2 / kg MUST contribute +6 kg CO2 in
every case: treating waste adds burden, it never subtracts it.
-}
module WasteTreatmentSignSpec (spec) where

import API.Types (ExchangeWithUnit (..))
import Data.List (elemIndex)
import qualified Data.Map as M
import qualified Data.Map.Strict as MS
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import Database (buildDatabaseWithMatrices)
import Database.CrossLinking (LinkingContext (..), buildIndexedDatabaseFromDB, defaultLinkingThreshold, emptyAliasMap)
import Database.Loader (findAllCrossDBLinks)
import Matrix (computeInventoryMatrix)
import Service (buildCrossDBLinkMap, toExchangeWithUnit)
import SharedSolver (CrossDBSolution (..), computeInventoryMatrixWithDepsCached)
import SynonymDB (emptySynonymDB)
import Test.Hspec
import TestHelpers (mkDepLookupFromMap, mkSolverFromDb, withinTolerance)
import Types
import UnitConversion (defaultUnitConfig)

-- | A deterministic UUID from its canonical string form.
mkUUID :: String -> UUID
mkUUID s = fromMaybe UUID.nil (UUID.fromString s)

-- Shared identifiers.
kgU, co2, wW, yY, tA, pA :: UUID
kgU = mkUUID "66666666-6666-6666-6666-666666666666"
co2 = mkUUID "55555555-5555-5555-5555-555555555555"
wW = mkUUID "22222222-2222-2222-2222-222222222222"
yY = mkUUID "44444444-4444-4444-4444-444444444444"
tA = mkUUID "11111111-1111-1111-1111-111111111111"
pA = mkUUID "33333333-3333-3333-3333-333333333333"

emptyActivity :: Activity
emptyActivity =
    Activity
        { activityName = ""
        , activityDescription = []
        , activityDocumentation = []
        , activitySynonyms = M.empty
        , activityClassification = M.empty
        , activityLocation = "GLO"
        , activityLocationSource = LocationDeclared
        , activityUnit = "kg"
        , exchanges = []
        , activityParams = M.empty
        , activityParamExprs = M.empty
        , activityNativeType = Nothing
        , activityNativeId = Nothing
        , activityFormulaCheck = Nothing
        }

techEx :: UUID -> Double -> TechRole -> Maybe UUID -> Exchange
techEx flow amt role link =
    TechnosphereExchange
        { techFlowId = flow
        , techAmount = amt
        , techUnitId = kgU
        , techRole = role
        , techActivityLinkId = link
        , techSupplierClaim = ClaimByProduct
        , techLocation = ""
        , techComment = Nothing
        , techPedigree = Nothing
        , techShare = Nothing
        , techClassification = M.empty
        , techProperties = noProperties
        }

co2Emission :: Double -> Exchange
co2Emission amt =
    BiosphereExchange
        { bioFlowId = co2
        , bioAmount = amt
        , bioUnitId = kgU
        , bioDirection = Emission
        , bioLocation = ""
        , bioComment = Nothing
        , bioPedigree = Nothing
        }

wasteEx :: Bool -> Maybe UUID -> Double -> Exchange
wasteEx isInput link amt =
    WasteExchange
        { waFlowId = wW
        , waAmount = amt
        , waUnitId = kgU
        , waIsInput = isInput
        , waActivityLinkId = link
        , waSupplierClaim = ClaimByProduct
        , waLocation = ""
        , waComment = Nothing
        , waPedigree = Nothing
        }

{- | A treatment of waste W: reference of W (role + signed amount vary by
convention), emitting 2 kg CO2 per kg treated.
-}
treatment :: TechRole -> Double -> Activity
treatment role amt =
    emptyActivity
        { activityName = "treatment of waste W"
        , exchanges = [techEx wW amt role Nothing, co2Emission 2.0]
        }

{- | A producer of Y that also emits 3 kg of waste W. @waste@ supplies the
waste exchange (intra-DB link, or orphan for the cross-DB cases).
-}
producer :: Exchange -> Activity
producer waste =
    emptyActivity
        { activityName = "producer of Y"
        , exchanges = [techEx yY 1.0 ReferenceProduct Nothing, waste]
        }

co2Flow :: BiosphereFlow
co2Flow = BiosphereFlow co2 "carbon dioxide" kgU M.empty Nothing Nothing (Just (Compartment Air Nothing))

wasteFlowDB :: M.Map UUID WasteFlow
wasteFlowDB = M.singleton wW (WasteFlow wW "waste W" kgU M.empty Nothing Nothing)

techFlowDB :: M.Map UUID TechnosphereFlow
techFlowDB =
    M.fromList
        [ (wW, TechnosphereFlow wW "waste W" kgU M.empty Nothing Nothing)
        , (yY, TechnosphereFlow yY "product Y" kgU M.empty Nothing Nothing)
        ]

buildDB :: T.Text -> M.Map (UUID, UUID) Activity -> IO Database
buildDB name acts =
    buildDatabaseWithMatrices
        (BuildInputs defaultUnitConfig mempty Declared)
        SimpleDatabase
            { sdbActivities = acts
            , sdbTechFlows = techFlowDB
            , sdbBioFlows = M.singleton co2 co2Flow
            , sdbWasteFlows = wasteFlowDB
            , sdbUnits = M.singleton kgU (Unit kgU "kg" "kg" "")
            }
        >>= either (\e -> fail (T.unpack name <> ": " <> T.unpack e)) pure

co2Of :: M.Map UUID Double -> Double
co2Of = M.findWithDefault 0.0 co2

processIdOf :: Database -> (UUID, UUID) -> Maybe ProcessId
processIdOf db key = fromIntegral <$> elemIndex key (V.toList (dbProcessIdTable db))

-- | Intra-DB scoring of the producer's CO2 (single database, static triples).
scoreIntra :: T.Text -> M.Map (UUID, UUID) Activity -> IO Double
scoreIntra name acts = do
    db <- buildDB name acts
    case processIdOf db (pA, yY) of
        Nothing -> fail "producer not interned"
        Just pid -> co2Of <$> (either (fail . show) pure =<< computeInventoryMatrix db (fromIntegral pid))

-- | A linking context holding one dependency database.
ctxWithDep :: T.Text -> Database -> LinkingContext
ctxWithDep depName depDB =
    LinkingContext
        { lcIndexedDatabases = [buildIndexedDatabaseFromDB depName emptySynonymDB depDB]
        , lcSynonymDB = emptySynonymDB
        , lcUnitConfig = defaultUnitConfig
        , lcThreshold = defaultLinkingThreshold
        , lcLocationHierarchy = M.empty
        , lcGeographyPolicy = GeoExact
        , lcSupplierAliases = emptyAliasMap
        }

-- | The cross-DB links the real linker finds for one root activity map.
linksFor :: LinkingContext -> M.Map (UUID, UUID) Activity -> [CrossDBLink]
linksFor ctx = cdlLinks . findAllCrossDBLinks ctx techFlowDB wasteFlowDB (M.singleton kgU (Unit kgU "kg" "kg" ""))

{- | Cross-DB scoring: a root DB whose waste OUTPUT is resolved by the real
linker ('findAllCrossDBLinks') to a treatment in a separate dependency DB, then
scored through the dep-demand solve. Going through the linker (rather than a
hand-built 'CrossDBLink') is the point: that is where the treatment's
reference-sign correction is applied.

@rootLink@ is what the waste output states about its treatment: 'Nothing' for
an output naming none, @tA@ for one naming the dependency's treatment. Both
must reach the same treatment and the same score.

Returns the score and the waste line as the API reports it, so a test can hold
the two against each other: a charged line that reports no treatment is the
same bug read from the other end.
-}
scoreCross :: Maybe UUID -> T.Text -> TechRole -> Double -> IO (Double, ExchangeWithUnit)
scoreCross rootLink depName depRole depRefAmount = do
    let rootActs = M.singleton (pA, yY) (producer (wasteEx False rootLink 3.0))
    rootBase <- buildDB "root" rootActs
    depDB <- buildDB depName (M.singleton (tA, wW) (treatment depRole depRefAmount))
    let links = linksFor (ctxWithDep depName depDB) rootActs
        rootDB = rootBase{dbCrossDBLinks = links}
    if null links
        then fail "linker created no cross-DB waste link (matcher did not fire)"
        else do
            rootSolver <- mkSolverFromDb rootDB "root"
            depSolver <- mkSolverFromDb depDB depName
            let depLookup = mkDepLookupFromMap (MS.singleton depName (depDB, depSolver))
            case processIdOf rootDB (pA, yY) of
                Nothing -> fail "producer not interned"
                Just pid -> do
                    res <- computeInventoryMatrixWithDepsCached defaultUnitConfig depLookup rootDB "root" rootSolver pid
                    let reported =
                            toExchangeWithUnit
                                rootDB
                                (buildCrossDBLinkMap rootDB pid)
                                (wasteEx False rootLink 3.0)
                    case res of
                        Left err -> fail (T.unpack err)
                        Right sol -> pure (co2Of (csInventory sol), reported)

spec :: Spec
spec = describe "Waste-treatment scoring sign across reference conventions" $ do
    it "intra-DB ecoinvent (negative ReferenceProduct output) scores +6" $ do
        score <-
            scoreIntra
                "intra-eco"
                ( M.fromList
                    [ ((pA, yY), producer (wasteEx False (Just tA) 3.0))
                    , ((tA, wW), treatment ReferenceProduct (-1.0))
                    ]
                )
        withinTolerance 1.0e-9 6.0 score `shouldBe` True

    it "intra-DB ILCD (positive ReferenceInput) scores +6" $ do
        score <-
            scoreIntra
                "intra-ilcd"
                ( M.fromList
                    [ ((pA, yY), producer (wasteEx True (Just tA) 3.0))
                    , ((tA, wW), treatment ReferenceInput 1.0)
                    ]
                )
        withinTolerance 1.0e-9 6.0 score `shouldBe` True

    it "cross-DB to an ILCD (positive ReferenceInput) treatment scores +6" $ do
        (score, _) <- scoreCross Nothing "ilcd-dep" ReferenceInput 1.0
        withinTolerance 1.0e-9 6.0 score `shouldBe` True

    it "cross-DB to an ecoinvent (negative ReferenceProduct) treatment scores +6" $ do
        (score, _) <- scoreCross Nothing "eco-dep" ReferenceProduct (-1.0)
        withinTolerance 1.0e-9 6.0 score `shouldBe` True

    -- An authored waste output states the treatment it goes to. When that
    -- treatment lives in a dependency, naming it must reach the dependency
    -- just as naming nothing does; reading the link as "resolved elsewhere"
    -- cut the waste off with no burden at all.
    it "cross-DB from a waste output that names the dependency's treatment scores +6" $ do
        (score, _) <- scoreCross (Just tA) "eco-dep" ReferenceProduct (-1.0)
        withinTolerance 1.0e-9 6.0 score `shouldBe` True

    -- Both halves of the same statement: the treatment charged is the one
    -- reported. Reporting no treatment for a line the score charged says the
    -- burden is missing when it was counted, which is the reading the role
    -- exists to make impossible.
    it "reports the dependency's treatment for the waste output it charged" $ do
        (_, reported) <- scoreCross (Just tA) "eco-dep" ReferenceProduct (-1.0)
        ewuWasteRole reported `shouldBe` Just SentToTreatment
        (T.isPrefixOf "eco-dep::" <$> ewuTargetProcessId reported) `shouldBe` Just True

    it "reports the dependency's treatment for an output naming none" $ do
        (_, reported) <- scoreCross Nothing "eco-dep" ReferenceProduct (-1.0)
        ewuWasteRole reported `shouldBe` Just SentToTreatment
        (T.isPrefixOf "eco-dep::" <$> ewuTargetProcessId reported) `shouldBe` Just True

    -- The other half of the same gate: a link the root database resolves in
    -- place is the matrix's business, and a cross-DB link on top would charge
    -- the treatment twice.
    it "a waste output linked inside its own database gets no cross-DB link" $ do
        let rootActs =
                M.fromList
                    [ ((pA, yY), producer (wasteEx False (Just tA) 3.0))
                    , ((tA, wW), treatment ReferenceProduct (-1.0))
                    ]
        depDB <- buildDB "eco-dep" (M.singleton (tA, wW) (treatment ReferenceProduct (-1.0)))
        linksFor (ctxWithDep "eco-dep" depDB) rootActs `shouldBe` []
