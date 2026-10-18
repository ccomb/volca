{-# LANGUAGE OverloadedStrings #-}

{- | Cross-database linking by EcoSpold2 @activityLinkId@ identity.

A partial EcoSpold2 import carries non-nil @activityLinkId@s pointing at
background activities it does not ship. These tests pin the resolution cascade
'Database.Loader.findExchangeCrossDBLink' runs for such inputs:

1. exact @(activityLinkId, flowId)@ identity against a loaded dependency;
2. attribute matching (name/location/unit) when the exact identity is absent –
   the cross-version case, flagged in 'cdlAttributeFallbacks';
3. and that an input whose target is present *in the same database* resolves
   internally and gets no cross-DB link (no double counting).
-}
module CrossDBActivityLinkSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.UUID as UUID
import Database.CrossLinking (
    IndexedDatabase,
    LinkingContext (..),
    buildIndexedDatabase,
    defaultLinkingThreshold,
    emptyAliasMap,
    findSupplierByActivityProduct,
    locationHierarchy,
 )
import Database.Loader (collectStagedDanglingProductNames, findAllCrossDBLinks)
import SynonymDB (emptySynonymDB)
import Test.Hspec
import Types
import UnitConversion (UnitConfig, UnitDef (..), defaultDimensionOrder, defaultUnitConfig, mergeUnitConfigs, mkUnitConfig)

-- ---------------------------------------------------------------------------
-- UUIDs
-- ---------------------------------------------------------------------------

cAct, cProd, supAct, supProd, oldAct, newAct, supProd2, kgUnit, gUnit, sUnit :: UUID.UUID
cAct = read "c0000000-0000-0000-0000-000000000001"
cProd = read "c0000000-0000-0000-0000-000000000002"
supAct = read "50000000-0000-0000-0000-000000000001"
supProd = read "50000000-0000-0000-0000-000000000002"
oldAct = read "01d00000-0000-0000-0000-000000000001"
newAct = read "0ee00000-0000-0000-0000-000000000001"
supProd2 = read "50000000-0000-0000-0000-000000000003"
kgUnit = read "00000000-0000-0000-0000-0000000000a1"
gUnit = read "00000000-0000-0000-0000-0000000000a2"
sUnit = read "00000000-0000-0000-0000-0000000000a3"

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

mkFlow :: UUID.UUID -> Text -> TechnosphereFlow
mkFlow fid name =
    TechnosphereFlow
        { tfId = fid
        , tfName = name
        , tfUnitId = kgUnit
        , tfSynonyms = M.empty
        , tfCAS = Nothing
        , tfSubstanceId = Nothing
        }

mkActivity :: Text -> Text -> [Exchange] -> Activity
mkActivity name loc exs =
    Activity
        { activityName = name
        , activityDescription = []
        , activityDocumentation = []
        , activitySynonyms = M.empty
        , activityClassification = M.empty
        , activityLocation = loc
        , activityLocationSource = LocationDeclared
        , activityUnit = "kg"
        , exchanges = exs
        , activityParameters = M.empty
        , activityNativeType = Nothing
        , activityNativeId = Nothing
        , activityFormulaCheck = Nothing
        , activityDates = noDates
        }

refEx :: UUID.UUID -> Exchange
refEx fid =
    TechnosphereExchange
        { techFlowId = fid
        , techAmount = 1.0
        , techUnitId = kgUnit
        , techRole = ReferenceProduct
        , techActivityLinkId = Nothing
        , techSupplierClaim = ClaimByProduct
        , techLocation = "GLO"
        , techComment = Nothing
        , techPedigree = Nothing
        , techFormula = Nothing
        , techShare = Nothing
        , techClassification = M.empty
        , techProperties = noProperties
        }

-- | An input for @flowId@, linked to producer activity @linkId@ (nil = unlinked).

{- | An input designating its supplier the way EcoSpold 2 does, by the
activity's identifier: the claim the source made, and the link the load
resolved it to, which for that format are the same value.
-}
inputEx :: UUID.UUID -> Maybe UUID.UUID -> Exchange
inputEx flowId linkId =
    TechnosphereExchange
        { techFlowId = flowId
        , techAmount = 2.0
        , techUnitId = kgUnit
        , techRole = Input
        , techActivityLinkId = linkId
        , techSupplierClaim = maybe ClaimByProduct ClaimById linkId
        , techLocation = "GLO"
        , techComment = Nothing
        , techPedigree = Nothing
        , techFormula = Nothing
        , techShare = Nothing
        , techClassification = M.empty
        , techProperties = noProperties
        }

mkDB :: [((UUID.UUID, UUID.UUID), Activity)] -> [TechnosphereFlow] -> SimpleDatabase
mkDB acts flows =
    SimpleDatabase
        { sdbActivities = M.fromList acts
        , sdbTechFlows = M.fromList [(tfId f, f) | f <- flows]
        , sdbBioFlows = M.empty
        , sdbWasteFlows = M.empty
        , sdbUnits = M.fromList [(kgUnit, Unit kgUnit "kg" "" ""), (gUnit, Unit gUnit "g" "" ""), (sUnit, Unit sUnit "s" "" "")]
        , sdbDocumentation = noDocumentation
        , sdbParameters = M.empty
        , sdbProductSystems = []
        }

ctxFor :: [IndexedDatabase] -> LinkingContext
ctxFor idbs =
    LinkingContext
        { lcIndexedDatabases = idbs
        , lcSynonymDB = emptySynonymDB
        , lcUnitConfig = withGrams
        , lcThreshold = defaultLinkingThreshold
        , lcLocationHierarchy = locationHierarchy
        , lcGeographyPolicy = GeoGlobal
        , lcSupplierAliases = emptyAliasMap
        }

withGrams :: UnitConfig
withGrams = mergeUnitConfigs [defaultUnitConfig, mkUnitConfig defaultDimensionOrder (M.singleton "g" (UnitDef [if d == "mass" then 1 else 0 | d <- defaultDimensionOrder] 0.001))]

runLinks :: SimpleDatabase -> [IndexedDatabase] -> CrossDBLinkingStats
runLinks fg idbs =
    findAllCrossDBLinks (ctxFor idbs) (sdbTechFlows fg) (sdbWasteFlows fg) (sdbUnits fg) (sdbActivities fg)

indexBg :: SimpleDatabase -> IndexedDatabase
indexBg = buildIndexedDatabase "bg" emptySynonymDB

-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
    describe "findSupplierByActivityProduct" $ do
        let bg = indexBg (mkDB [((supAct, supProd), mkActivity "widget" "GLO" [refEx supProd])] [mkFlow supProd "widget"])
        it "returns the supplier for an exact (activity, product) identity" $
            map snd (findSupplierByActivityProduct [bg] (ProcessRef supAct supProd)) `shouldBe` ["bg"]
        it "returns empty when no database ships that identity" $
            null (findSupplierByActivityProduct [bg] (ProcessRef oldAct supProd)) `shouldBe` True
        it "lists every database shipping the identity, in the order they are given" $ do
            let shipping = mkDB [((supAct, supProd), mkActivity "widget" "GLO" [refEx supProd])] [mkFlow supProd "widget"]
                a = buildIndexedDatabase "a" emptySynonymDB shipping
                b = buildIndexedDatabase "b" emptySynonymDB shipping
            map snd (findSupplierByActivityProduct [a, bg, b] (ProcessRef supAct supProd)) `shouldBe` ["a", "bg", "b"]

    describe "findExchangeCrossDBLink (activityLinkId cascade)" $ do
        it "links a dangling non-nil input to a background supplier by exact identity" $ do
            let consumer = mkActivity "consumer" "GLO" [refEx cProd, inputEx supProd (Just supAct)]
                fg = mkDB [((cAct, cProd), consumer)] [mkFlow cProd "consumer-product", mkFlow supProd "widget"]
                bg = indexBg (mkDB [((supAct, supProd), mkActivity "widget" "GLO" [refEx supProd])] [mkFlow supProd "widget"])
                stats = runLinks fg [bg]
            map (\l -> (cdlSupplierActUUID l, cdlSupplierProdUUID l, cdlSourceDatabase l)) (cdlLinks stats)
                `shouldBe` [(supAct, supProd, "bg")]
            -- Exact identity is not a cross-version stitch.
            cdlAttributeFallbacks stats `shouldBe` []

        it "does not emit a cross-DB link when the target resolves internally" $ do
            let consumer = mkActivity "consumer" "GLO" [refEx cProd, inputEx supProd (Just supAct)]
                supplier = mkActivity "widget" "GLO" [refEx supProd]
                -- supplier present in the SAME database as the consumer
                fg = mkDB [((cAct, cProd), consumer), ((supAct, supProd), supplier)] [mkFlow cProd "consumer-product", mkFlow supProd "widget"]
                -- and also offered by a dependency, to prove internal wins
                bg = indexBg (mkDB [((supAct, supProd), supplier)] [mkFlow supProd "widget"])
            cdlLinks (runLinks fg [bg]) `shouldBe` []

        it "falls back to attribute matching and flags a cross-version stitch" $ do
            -- Consumer references a release whose activity UUID (oldAct) the
            -- background does not ship; the background offers the same product
            -- under a different activity UUID (newAct).
            let consumer = mkActivity "consumer" "GLO" [refEx cProd, inputEx supProd (Just oldAct)]
                fg = mkDB [((cAct, cProd), consumer)] [mkFlow cProd "consumer-product", mkFlow supProd "widget"]
                bg = indexBg (mkDB [((newAct, supProd2), mkActivity "widget" "GLO" [refEx supProd2])] [mkFlow supProd2 "widget"])
                stats = runLinks fg [bg]
            map (\l -> (cdlSupplierActUUID l, cdlSupplierProdUUID l, cdlSourceDatabase l)) (cdlLinks stats)
                `shouldBe` [(newAct, supProd2, "bg")]
            map (\a -> (afProduct a, afSourceDatabase a)) (cdlAttributeFallbacks stats)
                `shouldBe` [("widget", "bg")]

        it "does not flag a nil-link input matched by attributes" $ do
            let consumer = mkActivity "consumer" "GLO" [refEx cProd, inputEx supProd Nothing]
                fg = mkDB [((cAct, cProd), consumer)] [mkFlow cProd "consumer-product", mkFlow supProd "widget"]
                bg = indexBg (mkDB [((supAct, supProd), mkActivity "widget" "GLO" [refEx supProd])] [mkFlow supProd "widget"])
                stats = runLinks fg [bg]
            length (cdlLinks stats) `shouldBe` 1
            cdlAttributeFallbacks stats `shouldBe` []

    -- A link's demand is summed with the other links to its supplier and read
    -- in the unit the link names, so a line written in grams asks for kilograms.
    describe "findExchangeCrossDBLink (a line in another unit than its flow)" $ do
        let bg = indexBg (mkDB [((supAct, supProd), mkActivity "widget" "GLO" [refEx supProd])] [mkFlow supProd "widget"])
            linksOf link unit =
                cdlLinks
                    ( runLinks
                        (mkDB [((cAct, cProd), mkActivity "consumer" "GLO" [refEx cProd, (inputEx supProd link){techAmount = 2000, techUnitId = unit}])] [mkFlow cProd "consumer-product", mkFlow supProd "widget"])
                        [bg]
                    )
            asked = map (\l -> (cdlCoefficient l, cdlExchangeUnit l, maybe 0 aeFactor (cdlAnswers l)))
            near (c, u, f) (c', u', f') = abs (c - c') < 1e-12 && u == u' && abs (f - f') < 1e-15
        it "asks 2 kg of its supplier for a line of 2000 g, found by identity or by attributes" $ do
            and (zipWith near (asked (linksOf (Just supAct) gUnit)) [(2, "kg", 0.001)]) `shouldBe` True
            and (zipWith near (asked (linksOf Nothing gUnit)) [(2, "kg", 0.001)]) `shouldBe` True
            map length [linksOf (Just supAct) gUnit, linksOf Nothing gUnit] `shouldBe` [1, 1]
        it "links no line written in a unit that measures something else" $
            linksOf (Just supAct) sUnit `shouldBe` []

    describe "findExchangeCrossDBLink (two producers of one product)" $ do
        -- The shape a Brightway foreground meets in a background database: one product name,
        -- one location, several activities publishing it.
        let twoProducers =
                indexBg $
                    mkDB
                        [ ((supAct, supProd), mkActivity "market for widget" "GLO" [refEx supProd])
                        , ((newAct, supProd), mkActivity "widget production, on site" "GLO" [refEx supProd])
                        ]
                        [mkFlow supProd "widget"]
            consumerOf ex = mkDB [((cAct, cProd), mkActivity "consumer" "GLO" [refEx cProd, ex])] [mkFlow cProd "consumer-product", mkFlow supProd "widget"]

        it "reports the tie when the input names no supplier activity" $ do
            let stats = runLinks (consumerOf (inputEx supProd Nothing)) [twoProducers]
            map (\a -> (saProduct a, saCandidates a, saSourceDatabase a)) (cdlSupplierAmbiguities stats)
                `shouldBe` [("widget", 2, "bg")]

        it "links to the named activity and reports no tie" $ do
            let named = (inputEx supProd Nothing){techSupplierClaim = ClaimByName "widget production, on site"}
                stats = runLinks (consumerOf named) [twoProducers]
            map (\l -> (cdlSupplierActUUID l, cdlSourceDatabase l)) (cdlLinks stats)
                `shouldBe` [(newAct, "bg")]
            cdlSupplierAmbiguities stats `shouldBe` []

    describe "collectStagedDanglingProductNames (duplicate product flow)" $ do
        it "still names a residual gap when only one of two same-product inputs is covered" $ do
            -- One activity consumes "widget" twice, from two suppliers (same
            -- flowId, different activityLinkId); neither supplier is in the
            -- database. A single cross-DB link covers one of them – the other
            -- must still be reported, not masked by the shared (act, prod, flow)
            -- triple. The engine resolves demands by (activityLinkId, flowId),
            -- so coverage is counted per occurrence, not tested for membership.
            let consumer = mkActivity "consumer" "GLO" [refEx cProd, inputEx supProd (Just supAct), inputEx supProd (Just oldAct)]
                fg = mkDB [((cAct, cProd), consumer)] [mkFlow cProd "consumer-product", mkFlow supProd "widget"]
                coveringLink =
                    CrossDBLink
                        { cdlConsumerActUUID = cAct
                        , cdlConsumerProdUUID = cProd
                        , cdlConsumerFlowId = supProd
                        , cdlSupplierActUUID = supAct
                        , cdlSupplierProdUUID = supProd
                        , cdlCoefficient = 2.0
                        , cdlExchangeUnit = "kg"
                        , cdlFlowName = "widget"
                        , cdlLocation = "GLO"
                        , cdlSourceDatabase = "bg"
                        , cdlTiedAlternatives = []
                        , cdlAnswers = Nothing
                        }
            collectStagedDanglingProductNames fg [coveringLink] `shouldBe` M.singleton "widget" 1
