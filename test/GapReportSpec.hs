{-# LANGUAGE OverloadedStrings #-}

{- | Supplier-gap report tests.

A consumer database demands four products: one satisfied internally, one
supplied by a cross-DB background, and three genuine gaps of different kinds -
a nil-link product the matcher can't place ('GapBlocked'), a non-nil source
identity no dependency ships ('GapDanglingIdentity'), and an unlinked waste
input ('GapWasteInput'). The report must count edges exactly, aggregate per
(name, location, unit), rank by demanding edges, and name the top consumers.
-}
module GapReportSpec (spec) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.UUID (UUID)
import Test.Hspec

import API.DatabaseHandlers (gapReportToAPI)
import API.Types (GapEntryAPI (..), GapReportAPI (..))
import Database.CrossLinking (
    AliasKey (..),
    AliasMap (..),
    AliasTarget (..),
    IndexedDatabase,
    buildIndexedDatabase,
    emptyAliasMap,
 )
import Database.Loader (
    CrossDBLinkingStats (..),
    GapConsumer (..),
    GapEntry (..),
    GapReason (..),
    GapReport (..),
    gapReportForStaged,
    relinkSimpleDatabase,
 )
import SynonymDB (emptySynonymDB)
import Types (
    Activity (..),
    BlockerReason (..),
    Exchange (..),
    GeographyPolicy (..),
    LinkBlocker (..),
    LocationSource (..),
    SimpleDatabase (..),
    SupplierClaim (..),
    TechRole (..),
    TechnosphereFlow (..),
    Unit (..),
    UnresolvedProduct (..),
    WasteFlow (..),
    noProperties,
 )
import UnitConversion (defaultUnitConfig)

-- ---------------------------------------------------------------------------
-- UUID helpers: readable fixed identifiers.
-- ---------------------------------------------------------------------------

u :: String -> UUID
u suffix = read ("00000000-0000-0000-0000-0000000000" <> suffix)

kgUnit, breadFlow, cakeFlow, flourFlow, waterFlow, sugarFlow, wasteFlow :: UUID
kgUnit = u "01"
breadFlow = u "02"
cakeFlow = u "03"
flourFlow = u "04"
waterFlow = u "05"
sugarFlow = u "06"
wasteFlow = u "07"

actBread, actCake, actSupplier, ghostAct :: UUID
actBread = u "0a"
actCake = u "0b"
actSupplier = u "0c"
ghostAct = u "0d" -- named by a dangling activityLinkId, shipped by nobody

-- ---------------------------------------------------------------------------
-- Fixture building blocks
-- ---------------------------------------------------------------------------

mkActivity :: Text -> [Exchange] -> Activity
mkActivity name exs =
    Activity
        { activityName = name
        , activityDescription = []
        , activityDocumentation = []
        , activitySynonyms = M.empty
        , activityClassification = M.empty
        , activityLocation = "FR"
        , activityLocationSource = LocationDeclared
        , activityUnit = "kg"
        , exchanges = exs
        , activityParams = M.empty
        , activityParamExprs = M.empty
        , activityNativeType = Nothing
        , activityNativeId = Nothing
        , activityFormulaCheck = Nothing
        }

mkTechFlow :: UUID -> Text -> TechnosphereFlow
mkTechFlow fid name =
    TechnosphereFlow
        { tfId = fid
        , tfName = name
        , tfUnitId = kgUnit
        , tfSynonyms = M.empty
        , tfCAS = Nothing
        , tfSubstanceId = Nothing
        }

reference :: UUID -> Exchange
reference fid = (techInput fid 1.0){techRole = ReferenceProduct}

techInput :: UUID -> Double -> Exchange
techInput fid amount =
    TechnosphereExchange
        { techFlowId = fid
        , techAmount = amount
        , techUnitId = kgUnit
        , techRole = Input
        , techActivityLinkId = Nothing
        , techSupplierClaim = ClaimByProduct
        , techLocation = "FR"
        , techComment = Nothing
        , techPedigree = Nothing
        , techShare = Nothing
        , techClassification = M.empty
        , techProperties = noProperties
        }

wasteInput :: UUID -> Double -> Exchange
wasteInput fid amount =
    WasteExchange
        { waFlowId = fid
        , waAmount = amount
        , waUnitId = kgUnit
        , waIsInput = True
        , waActivityLinkId = Nothing
        , waSupplierClaim = ClaimByProduct
        , waLocation = ""
        , waComment = Nothing
        , waPedigree = Nothing
        }

units :: M.Map UUID Unit
units = M.singleton kgUnit (Unit kgUnit "kg" "kg" "")

{- | Consumer database. Demands (7 supplier demands in total):

* bread activity: flour ×2 (1.0 + 2.0), water ×1 (3.0),
  sugar ×1 (0.5, non-nil link to 'ghostAct'), waste input ×1 (0.25)
* cake activity: flour ×1 (4.0), bread ×1 (1.0, resolved internally)
-}
consumerDB :: SimpleDatabase
consumerDB =
    SimpleDatabase
        { sdbActivities =
            M.fromList
                [ ((actBread, breadFlow), mkActivity "bread baking" breadExchanges)
                , ((actCake, cakeFlow), mkActivity "cake baking" cakeExchanges)
                ]
        , sdbTechFlows =
            M.fromList
                [ (breadFlow, mkTechFlow breadFlow "bread")
                , (cakeFlow, mkTechFlow cakeFlow "cake")
                , (flourFlow, mkTechFlow flourFlow "flour")
                , (waterFlow, mkTechFlow waterFlow "water")
                , (sugarFlow, mkTechFlow sugarFlow "sugar")
                ]
        , sdbBioFlows = M.empty
        , sdbWasteFlows =
            M.singleton wasteFlow (WasteFlow wasteFlow "plastic waste" kgUnit M.empty Nothing Nothing)
        , sdbUnits = units
        }
  where
    breadExchanges =
        [ reference breadFlow
        , techInput flourFlow 1.0
        , techInput flourFlow 2.0
        , techInput waterFlow 3.0
        , -- An input that named an activity, after a deletion cleared the link
          -- it had resolved to. What the source said outlives it.
          (techInput sugarFlow 0.5){techSupplierClaim = ClaimById ghostAct}
        , wasteInput wasteFlow 0.25
        ]
    cakeExchanges =
        [ reference cakeFlow
        , techInput flourFlow 4.0
        , (techInput breadFlow 1.0){techActivityLinkId = Just actBread, techSupplierClaim = ClaimById actBread}
        ]

-- | Background dependency: supplies "water" @ FR in kg, nothing else.
supplierIndexed :: IndexedDatabase
supplierIndexed = buildIndexedDatabase "background" emptySynonymDB supplierDB

supplierDB :: SimpleDatabase
supplierDB =
    SimpleDatabase
        { sdbActivities = M.singleton (actSupplier, waterFlow) (mkActivity "water supply" [reference waterFlow])
        , sdbTechFlows = M.singleton waterFlow (mkTechFlow waterFlow "water")
        , sdbBioFlows = M.empty
        , sdbWasteFlows = M.empty
        , sdbUnits = units
        }

stats :: CrossDBLinkingStats
stats =
    relinkSimpleDatabase [supplierIndexed] emptySynonymDB defaultUnitConfig M.empty GeoGlobal emptyAliasMap consumerDB

report :: GapReport
report = gapReportForStaged "consumer" consumerDB stats

entryFor :: Text -> Maybe GapEntry
entryFor name = case filter ((== name) . geFlowName) (grGaps report) of
    [e] -> Just e
    _ -> Nothing

-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
    describe "gap report header" $ do
        it "counts every supplier demand in the denominator" $
            grTotalInputs report `shouldBe` 7

        it "credits the internally resolved input" $
            grInternalLinks report `shouldBe` 1

        it "credits the cross-DB water link" $
            grCrossDBLinks report `shouldBe` 1

        it "counts exactly the unsupplied edges" $
            grUnresolvedEdges report `shouldBe` 5

        it "counts distinct (name, location, unit) gap products" $
            grUnresolvedProducts report `shouldBe` 3

        it "keeps completeness consistent with the edge count" $
            grCompleteness report `shouldBe` (100 * 2 / 7)

        it "keeps the header edge count equal to the entry sum" $
            sum (map geEdges (grGaps report)) `shouldBe` grUnresolvedEdges report

    describe "gap entries" $ do
        it "ranks the most-demanded product first" $
            map geFlowName (take 1 (grGaps report)) `shouldBe` ["flour"]

        it "aggregates flour across both consumers with its blocker" $
            case entryFor "flour" of
                Nothing -> expectationFailure "expected exactly one flour entry"
                Just e -> do
                    geEdges e `shouldBe` 3
                    geConsumers e `shouldBe` 2
                    geDemandSum e `shouldBe` 7.0
                    geLocation e `shouldBe` "FR"
                    geUnit e `shouldBe` "kg"
                    geReason e `shouldBe` GapBlocked (NE.singleton (BlockerReason "no_name_match" Nothing))

        it "names the top consumers most-demanding first" $
            case entryFor "flour" of
                Nothing -> expectationFailure "expected exactly one flour entry"
                Just e -> do
                    map gcActivityName (geTopConsumers e) `shouldBe` ["bread baking", "cake baking"]
                    map gcEdges (geTopConsumers e) `shouldBe` [2, 1]
                    map gcProductName (geTopConsumers e) `shouldBe` ["bread", "cake"]

        it "reports a dangling source identity as its own reason" $
            -- Judged on what the source said, not on the link: deleting the
            -- activity an input named clears the link, and judging on that
            -- would move the input to the blocker report, which lists the
            -- products nothing supplies rather than the identities nothing
            -- answers.
            fmap geReason (entryFor "sugar") `shouldBe` Just GapDanglingIdentity

        it "reports an unlinked waste input as its own reason" $
            fmap geReason (entryFor "plastic waste") `shouldBe` Just GapWasteInput

        it "does not list the supplied or internal products" $ do
            entryFor "water" `shouldBe` Nothing
            entryFor "bread" `shouldBe` Nothing

    describe "wire projection limit" $ do
        it "keeps only the biggest gaps but the full header counts" $ do
            let api = gapReportToAPI (Just 1) report
            map gaeName (graGaps api) `shouldBe` ["flour"]
            graUnresolvedProducts api `shouldBe` 3
            graUnresolvedEdges api `shouldBe` 5

        it "returns everything without a limit" $
            length (graGaps (gapReportToAPI Nothing report)) `shouldBe` 3

    describe "a product refused two ways" $ do
        -- The linker records the reasons per product name. An entry of that
        -- product has to carry both: naming one of them attributes the other's
        -- demands to a cause that did not raise them.
        let refusedBy blockers =
                stats
                    { cdlUnresolvedProducts =
                        M.insert "flour" (UnresolvedProduct blockers) (cdlUnresolvedProducts stats)
                    }
            entriesOf refused =
                filter ((== "flour") . gaeName) (graGaps (gapReportToAPI Nothing (gapReportForStaged "consumer" consumerDB refused)))
            flourEntry = entriesOf (refusedBy (M.fromList [(NoNameMatch, 2), (UnitIncompatible "m3" "kg", 1)]))

        it "lists every reason on the entry" $
            map gaeReasons flourEntry
                `shouldBe` [
                               [ BlockerReason "no_name_match" Nothing
                               , BlockerReason "unit_incompatible" (Just "m3 vs kg")
                               ]
                           ]

        it "keeps the singular reason as the first of them" $
            map (\e -> (gaeReason e, gaeDetail e)) flourEntry `shouldBe` [("no_name_match", Nothing)]

        -- What the singular pair costs a client that still reads it: one cause
        -- met at three locations is one reason, and no single location
        -- describes it, so the detail the old field used to carry - whichever
        -- location merged first - is gone rather than misleading.
        it "leaves the singular detail out when the refusals disagree on it" $ do
            let entry = entriesOf (refusedBy (M.fromList [(LocationUnavailable loc, 1) | loc <- ["FR", "DE", "IT"]]))
            map gaeReasons entry `shouldBe` [[BlockerReason "location_unavailable" Nothing]]
            map (\e -> (gaeReason e, gaeDetail e)) entry `shouldBe` [("location_unavailable", Nothing)]

    describe "alias integration" $ do
        it "surfaces a missing designated target as its blocker in the report" $ do
            -- A relink mapping redirects flour to a supplier nobody ships:
            -- the gap report must carry the curated-mapping error, not a
            -- generic no_name_match.
            let aliases = AliasMap (M.singleton (AliasKey "flour" Nothing) (AliasTarget "no such product" (Just "CH")))
                aliasStats =
                    relinkSimpleDatabase [supplierIndexed] emptySynonymDB defaultUnitConfig M.empty GeoGlobal aliases consumerDB
                r = gapReportForStaged "consumer" consumerDB aliasStats
            case filter ((== "flour") . geFlowName) (grGaps r) of
                [e] -> geReason e `shouldBe` GapBlocked (NE.singleton (BlockerReason "alias_target_missing" (Just "no such product")))
                other -> expectationFailure ("expected one flour entry, got: " <> show other)

    describe "partial coverage" $ do
        it "reports only the surplus edges of a partially covered demand" $ do
            -- Two identical water demands, but only one of the two links kept:
            -- the report must show exactly the uncovered occurrence (the
            -- 'tallyDangling' count-based accounting), not zero and not both.
            let twoWaterDB =
                    consumerDB
                        { sdbActivities =
                            M.singleton
                                (actBread, breadFlow)
                                (mkActivity "washing" [reference breadFlow, techInput waterFlow 1.0, techInput waterFlow 2.0])
                        }
                twoWaterStats =
                    relinkSimpleDatabase [supplierIndexed] emptySynonymDB defaultUnitConfig M.empty GeoGlobal emptyAliasMap twoWaterDB
                onlyWater r = filter ((== "water") . geFlowName) (grGaps r)
            -- sanity: both demands link, so the full stats leave no water gap
            length (cdlLinks twoWaterStats) `shouldBe` 2
            onlyWater (gapReportForStaged "consumer" twoWaterDB twoWaterStats) `shouldBe` []
            -- with one of the two links dropped, exactly one edge resurfaces
            let partialStats = twoWaterStats{cdlLinks = take 1 (cdlLinks twoWaterStats)}
            case onlyWater (gapReportForStaged "consumer" twoWaterDB partialStats) of
                [e] -> geEdges e `shouldBe` 1
                other -> expectationFailure ("expected one water entry, got: " <> show other)
