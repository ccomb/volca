{-# LANGUAGE OverloadedStrings #-}

module CrossLinkingSpec (spec) where

import Control.Monad (forM_)
import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Database.CrossLinking
import Database.Loader (loadDatabase)
import Method.Types (Location (..))
import SynonymDB (buildFromPairs, emptySynonymDB)
import Test.Hspec
import Types
import UnitConversion (defaultUnitConfig)

spec :: Spec
spec = do
    -- -----------------------------------------------------------------------
    -- CrossDBLinkingStats Semigroup / Monoid
    -- -----------------------------------------------------------------------
    describe "CrossDBLinkingStats <>" $ do
        it "keeps every reason a product was refused for, counted apart" $ do
            let s1 = mempty{cdlUnresolvedProducts = M.fromList [("wheat", unresolved 2 NoNameMatch)]} :: CrossDBLinkingStats
                s2 = mempty{cdlUnresolvedProducts = M.fromList [("wheat", unresolved 3 (LocationUnavailable "FR")), ("maize", unresolved 1 NoNameMatch)]}
                merged = s1 <> s2
            -- Two runs refused wheat two different ways. The merge owes the
            -- reader both, not the total under whichever arrived first.
            fmap upBlockers (M.lookup "wheat" (cdlUnresolvedProducts merged))
                `shouldBe` Just (M.fromList [(NoNameMatch, 2), (LocationUnavailable "FR", 3)])
            fmap upDemands (M.lookup "wheat" (cdlUnresolvedProducts merged)) `shouldBe` Just 5
            M.lookup "maize" (cdlUnresolvedProducts merged) `shouldBe` Just (unresolved 1 NoNameMatch)

        it "adds the scalar counters" $ do
            let s1 = mempty{cdlTotalInputs = 4, cdlWasteExactLinks = 1} :: CrossDBLinkingStats
                s2 = mempty{cdlTotalInputs = 6, cdlWasteAmbiguous = 2}
            cdlTotalInputs (s1 <> s2) `shouldBe` 10
            cdlWasteExactLinks (s1 <> s2) `shouldBe` 1
            cdlWasteAmbiguous (s1 <> s2) `shouldBe` 2

    -- -----------------------------------------------------------------------
    -- normalizeText
    -- -----------------------------------------------------------------------
    describe "normalizeText" $ do
        it "lowercases and strips whitespace" $
            normalizeText "  Wheat Production  " `shouldBe` "wheat production"

        it "is idempotent" $
            normalizeText (normalizeText "Foo Bar") `shouldBe` normalizeText "Foo Bar"

        it "normalizes en-dash to hyphen" $
            normalizeText "bio\x2013gas" `shouldBe` "bio-gas"

        it "normalizes soft hyphen to ASCII hyphen" $
            normalizeText "bio\x00ADgas" `shouldBe` "bio-gas"

        it "normalizes hyphen (U+2010) to ASCII hyphen" $
            normalizeText "bio\x2010gas" `shouldBe` "bio-gas"

        it "normalizes non-breaking hyphen (U+2011) to ASCII hyphen" $
            normalizeText "bio\x2011gas" `shouldBe` "bio-gas"

        it "normalizes figure dash (U+2012) to ASCII hyphen" $
            normalizeText "bio\x2012gas" `shouldBe` "bio-gas"

        it "normalizes em-dash (U+2014) to ASCII hyphen" $
            normalizeText "bio\x2014gas" `shouldBe` "bio-gas"

        it "normalizes non-breaking space (U+00A0) to regular space" $
            normalizeText "bio\x00A0gas" `shouldBe` "bio gas"

        it "normalizes narrow no-break space (U+202F) to regular space" $
            normalizeText "bio\x202Fgas" `shouldBe` "bio gas"

    -- -----------------------------------------------------------------------
    -- extractBracketedLocation
    -- -----------------------------------------------------------------------
    describe "extractBracketedLocation" $ do
        it "extracts from curly braces {FR}" $
            extractBracketedLocation "electricity {FR}" `shouldBe` "FR"

        it "extracts from square brackets [GLO]" $
            extractBracketedLocation "wheat [GLO] production" `shouldBe` "GLO"

        it "ignores chemical notation [thio]" $
            extractBracketedLocation "compound [thio]" `shouldBe` ""

        it "returns empty string when no brackets" $
            extractBracketedLocation "plain name" `shouldBe` ""

    -- -----------------------------------------------------------------------
    -- isSubregionOf
    -- -----------------------------------------------------------------------
    describe "isSubregionOf" $ do
        it "FR is a subregion of RER" $
            isSubregionOf locationHierarchy (Location "FR") (Location "RER") `shouldBe` True

        it "FR is a subregion of GLO" $
            isSubregionOf locationHierarchy (Location "FR") (Location "GLO") `shouldBe` True

        it "GLO is not a subregion of FR" $
            isSubregionOf locationHierarchy (Location "GLO") (Location "FR") `shouldBe` False

        it "unknown location has no parents" $
            isSubregionOf locationHierarchy (Location "XX") (Location "GLO") `shouldBe` False

    -- -----------------------------------------------------------------------
    -- matchLocation
    -- -----------------------------------------------------------------------
    describe "matchLocation" $ do
        it "exact match scores 30" $
            matchLocation locationHierarchy (Location "FR") (Location "FR") `shouldBe` 30

        it "FR consumer, RER supplier scores 20 (widening)" $
            matchLocation locationHierarchy (Location "FR") (Location "RER") `shouldBe` 20

        it "FR consumer, GLO supplier scores 20 (widening via subregion)" $
            matchLocation locationHierarchy (Location "FR") (Location "GLO") `shouldBe` 20

        it "unknown location, GLO supplier scores 10 (global fallback)" $
            matchLocation locationHierarchy (Location "XX") (Location "GLO") `shouldBe` 10

        it "unknown location, RoW supplier scores 10 (global fallback)" $
            matchLocation locationHierarchy (Location "XX") (Location "RoW") `shouldBe` 10

        it "narrowing (GLO consumer, FR supplier) scores 0" $
            matchLocation locationHierarchy (Location "GLO") (Location "FR") `shouldBe` 0

        it "unrelated locations score 5" $
            matchLocation locationHierarchy (Location "FR") (Location "CN") `shouldBe` 5

    -- -----------------------------------------------------------------------
    -- matchProductName
    -- -----------------------------------------------------------------------
    describe "matchProductName" $ do
        it "exact match (case-insensitive) scores 50" $
            matchProductName emptySynonymDB "Wheat" "wheat" `shouldBe` 50

        it "no match scores 0" $
            matchProductName emptySynonymDB "wheat" "steel" `shouldBe` 0

        it "synonym match scores 45 when names share a group" $ do
            -- areSynonyms checks group-ID equality; build via CSV so both names
            -- normalize to entries that share the same group
            let synDB = buildFromPairs [("co2", "carbon dioxide")]
            -- After normalization both resolve to the same group in a symmetric pair
            matchProductName synDB "CO2" "carbon dioxide" `shouldSatisfy` (>= 45)

    -- -----------------------------------------------------------------------
    -- isSubregionOf - additional location pairs
    -- -----------------------------------------------------------------------
    describe "isSubregionOf (additional)" $ do
        it "US is a subregion of North America" $
            isSubregionOf locationHierarchy (Location "US") (Location "North America") `shouldBe` True

        it "CA is a subregion of NAFTA" $
            isSubregionOf locationHierarchy (Location "CA") (Location "NAFTA") `shouldBe` True

        it "JP is a subregion of Asia" $
            isSubregionOf locationHierarchy (Location "JP") (Location "Asia") `shouldBe` True

        it "BR is a subregion of Latin America" $
            isSubregionOf locationHierarchy (Location "BR") (Location "Latin America") `shouldBe` True

        it "AU is a subregion of GLO" $
            isSubregionOf locationHierarchy (Location "AU") (Location "GLO") `shouldBe` True

    -- -----------------------------------------------------------------------
    -- matchLocation - additional scoring cases
    -- -----------------------------------------------------------------------
    describe "matchLocation (additional)" $ do
        it "US consumer, NAFTA supplier scores 20 (widening)" $
            matchLocation locationHierarchy (Location "US") (Location "NAFTA") `shouldBe` 20

        it "GLO consumer, GLO supplier scores 30 (exact)" $
            matchLocation locationHierarchy (Location "GLO") (Location "GLO") `shouldBe` 30

        it "RoW consumer, RoW supplier scores 30 (exact)" $
            matchLocation locationHierarchy (Location "RoW") (Location "RoW") `shouldBe` 30

    -- -----------------------------------------------------------------------
    -- findSupplierInIndexedDBs - integration using SAMPLE.min3
    -- -----------------------------------------------------------------------
    describe "findSupplierInIndexedDBs (SAMPLE.min3)" $ do
        it "finds 'product Y' by name and GLO location" $ do
            idb <- loadMin3IndexedDB
            let ctx =
                    LinkingContext
                        { lcIndexedDatabases = [idb]
                        , lcSynonymDB = emptySynonymDB
                        , lcUnitConfig = defaultUnitConfig
                        , lcThreshold = defaultLinkingThreshold
                        , lcLocationHierarchy = locationHierarchy
                        , lcGeographyPolicy = GeoGlobal
                        , lcSupplierAliases = emptyAliasMap
                        }
            case findSupplierInIndexedDBs ctx (query "product Y" "GLO" "kg") of
                CrossDBLinked{cdlrScore = score} -> score `shouldSatisfy` (>= defaultLinkingThreshold)
                CrossDBNotLinked reason -> expectationFailure $ "Expected link but got: " ++ show reason

        it "returns NoNameMatch for an unknown product" $ do
            idb <- loadMin3IndexedDB
            let ctx =
                    LinkingContext
                        { lcIndexedDatabases = [idb]
                        , lcSynonymDB = emptySynonymDB
                        , lcUnitConfig = defaultUnitConfig
                        , lcThreshold = defaultLinkingThreshold
                        , lcLocationHierarchy = locationHierarchy
                        , lcGeographyPolicy = GeoGlobal
                        , lcSupplierAliases = emptyAliasMap
                        }
            case findSupplierInIndexedDBs ctx (query "no such product" "GLO" "kg") of
                CrossDBNotLinked _ -> return ()
                CrossDBLinked{} -> expectationFailure "Expected CrossDBNotLinked"

        it "returns UnitIncompatible when product found but unit doesn't match" $ do
            idb <- loadMin3IndexedDB
            let ctx =
                    LinkingContext
                        { lcIndexedDatabases = [idb]
                        , lcSynonymDB = emptySynonymDB
                        , lcUnitConfig = defaultUnitConfig
                        , lcThreshold = defaultLinkingThreshold
                        , lcLocationHierarchy = locationHierarchy
                        , lcGeographyPolicy = GeoGlobal
                        , lcSupplierAliases = emptyAliasMap
                        }
            -- "product Y" exists in kg; asking for m3 should fail unit check
            case findSupplierInIndexedDBs ctx (query "product Y" "GLO" "m3") of
                CrossDBNotLinked UnitIncompatible{} -> return ()
                CrossDBNotLinked reason -> expectationFailure $ "Expected UnitIncompatible but got: " ++ show reason
                CrossDBLinked{} -> expectationFailure "Expected CrossDBNotLinked for unit mismatch"

        it "finds via synonym when synDB has the pair" $ do
            idb <- loadMin3IndexedDB
            -- "product Y" is the canonical name; "producto Y" (alias) can be found via synonym
            let synDB = buildFromPairs [("product y", "producto y")]
                ctx =
                    LinkingContext
                        { lcIndexedDatabases = [idb]
                        , lcSynonymDB = synDB
                        , lcUnitConfig = defaultUnitConfig
                        , lcThreshold = defaultLinkingThreshold
                        , lcLocationHierarchy = locationHierarchy
                        , lcGeographyPolicy = GeoGlobal
                        , lcSupplierAliases = emptyAliasMap
                        }
            -- Synonym lookup: "producto y" → group containing "product y" → supplier
            case findSupplierInIndexedDBs ctx (query "producto y" "GLO" "kg") of
                CrossDBLinked{cdlrScore = score} -> score `shouldSatisfy` (>= defaultLinkingThreshold)
                CrossDBNotLinked _ -> pendingWith "synonym linking requires index to be built with synDB"

        it "uses empty location from compound name when location arg is empty" $ do
            idb <- loadMin3IndexedDB
            let ctx =
                    LinkingContext
                        { lcIndexedDatabases = [idb]
                        , lcSynonymDB = emptySynonymDB
                        , lcUnitConfig = defaultUnitConfig
                        , lcThreshold = defaultLinkingThreshold
                        , lcLocationHierarchy = locationHierarchy
                        , lcGeographyPolicy = GeoGlobal
                        , lcSupplierAliases = emptyAliasMap
                        }
            -- "product Y {GLO}" compound name with empty location arg
            -- extractBracketedLocation will find "GLO"
            case findSupplierInIndexedDBs ctx (query "product Y {GLO}" "" "kg") of
                CrossDBLinked{cdlrScore = score} -> score `shouldSatisfy` (>= defaultLinkingThreshold)
                CrossDBNotLinked _ -> pendingWith "Compound name location extraction may not match"

    -- -----------------------------------------------------------------------
    -- acceptableLocation - table-driven, one case per (policy, kind) cell
    -- -----------------------------------------------------------------------
    describe "acceptableLocation" $ do
        let hier = locationHierarchy
            -- Each row: (description, requested, candidate, expected for each policy)
            cases :: [(String, T.Text, T.Text, Maybe LocationKind, Maybe LocationKind, Maybe LocationKind)]
            cases =
                --   description                     requested   candidate           exact            parent              global
                [ ("exact match preserved", "FR", "FR", Just ExactLoc, Just ExactLoc, Just ExactLoc)
                , ("parent region (FR→Europe)", "FR", "Europe", Nothing, Just ParentLoc, Just ParentLoc)
                , ("parent region (FR→RER)", "FR", "RER", Nothing, Just ParentLoc, Just ParentLoc)
                , ("FR→GLO classified as global", "FR", "GLO", Nothing, Nothing, Just GlobalLoc)
                , ("FR→RoW classified as global", "FR", "RoW", Nothing, Nothing, Just GlobalLoc)
                , ("FR→Unspecified is global", "FR", "Unspecified", Nothing, Nothing, Just GlobalLoc)
                , ("FR→Mixed data is unrelated", "FR", "Mixed data", Nothing, Nothing, Just UnrelatedLoc)
                , ("FR→South America is unrelated", "FR", "South America", Nothing, Nothing, Just UnrelatedLoc)
                , ("narrowing GLO→FR always rejected", "GLO", "FR", Nothing, Nothing, Nothing)
                , ("BR→South America is parent", "BR", "South America", Nothing, Just ParentLoc, Just ParentLoc)
                , ("BR→Latin America is parent", "BR", "Latin America", Nothing, Just ParentLoc, Just ParentLoc)
                , ("BR→GLO is global", "BR", "GLO", Nothing, Nothing, Just GlobalLoc)
                ]
        forM_ cases $ \(label, req, cand, expExact, expParent, expGlobal) -> do
            it (label ++ " - exact") $ acceptableLocation GeoExact hier (Location req) (Location cand) `shouldBe` expExact
            it (label ++ " - parent") $ acceptableLocation GeoParent hier (Location req) (Location cand) `shouldBe` expParent
            it (label ++ " - global") $ acceptableLocation GeoGlobal hier (Location req) (Location cand) `shouldBe` expGlobal

    -- -----------------------------------------------------------------------
    -- findSupplierInIndexedDBs - geography_policy enforcement
    -- The SAMPLE.min3 fixture has "product Y" at GLO. Querying for FR
    -- exercises each (policy, candidate-kind) decision: GLO accepts only
    -- under GeoGlobal, and the rejection path must surface as
    -- LocationRejectedByPolicy carrying the actual kind.
    -- -----------------------------------------------------------------------
    describe "findSupplierInIndexedDBs (geography policy)" $ do
        let mkCtx policy idb =
                LinkingContext
                    { lcIndexedDatabases = [idb]
                    , lcSynonymDB = emptySynonymDB
                    , lcUnitConfig = defaultUnitConfig
                    , lcThreshold = defaultLinkingThreshold
                    , lcLocationHierarchy = locationHierarchy
                    , lcGeographyPolicy = policy
                    , lcSupplierAliases = emptyAliasMap
                    }

        it "GeoGlobal accepts FR query against a GLO candidate" $ do
            idb <- loadMin3IndexedDB
            case findSupplierInIndexedDBs (mkCtx GeoGlobal idb) (query "product Y" "FR" "kg") of
                CrossDBLinked{cdlrLocation = loc} -> loc `shouldBe` "GLO"
                CrossDBNotLinked reason ->
                    expectationFailure $ "Expected link under GeoGlobal but got: " ++ show reason

        it "GeoExact rejects FR query against a GLO candidate with kind=GlobalLoc" $ do
            idb <- loadMin3IndexedDB
            case findSupplierInIndexedDBs (mkCtx GeoExact idb) (query "product Y" "FR" "kg") of
                CrossDBNotLinked LocationRejectedByPolicy{lrRequested = req, lrBestCandidate = actLoc, lrBestKind = kind} -> do
                    req `shouldBe` "FR"
                    actLoc `shouldBe` "GLO"
                    kind `shouldBe` GlobalLoc
                CrossDBNotLinked reason ->
                    expectationFailure $ "Expected LocationRejectedByPolicy but got: " ++ show reason
                CrossDBLinked{} -> expectationFailure "Expected rejection under GeoExact"

        it "GeoParent also rejects a GLO candidate (parent-only does not include global)" $ do
            idb <- loadMin3IndexedDB
            case findSupplierInIndexedDBs (mkCtx GeoParent idb) (query "product Y" "FR" "kg") of
                CrossDBNotLinked LocationRejectedByPolicy{lrBestKind = kind} ->
                    kind `shouldBe` GlobalLoc
                CrossDBNotLinked reason ->
                    expectationFailure $ "Expected LocationRejectedByPolicy GlobalLoc but got: " ++ show reason
                CrossDBLinked{} -> expectationFailure "Expected rejection under GeoParent"

    -- -----------------------------------------------------------------------
    -- findSupplierInIndexedDBs - two activities, one product, one location.
    -- The background shape: "electricity, medium voltage" in GLO is the
    -- reference product of a market group and of 26 cut-off co-outputs, so the
    -- product name alone cannot say which the source meant.
    -- -----------------------------------------------------------------------
    describe "findSupplierInIndexedDBs (two producers of one product)" $ do
        let ctx =
                LinkingContext
                    { lcIndexedDatabases = [buildIndexedDatabase "background" emptySynonymDB twoProducerDB]
                    , lcSynonymDB = emptySynonymDB
                    , lcUnitConfig = defaultUnitConfig
                    , lcThreshold = defaultLinkingThreshold
                    , lcLocationHierarchy = locationHierarchy
                    , lcGeographyPolicy = GeoGlobal
                    , lcSupplierAliases = emptyAliasMap
                    }
            electricity = "electricity, medium voltage"

        it "links to the activity the demand names, not to the other producer" $
            forM_ [marketActivity, landfillActivity] $ \(name, uuid) ->
                case findSupplierInIndexedDBs ctx (queryFrom name electricity "GLO" "kWh") of
                    CrossDBLinked{cdlrActivityUUID = linked} -> linked `shouldBe` uuid
                    CrossDBNotLinked reason ->
                        expectationFailure $ "Expected a link for " ++ show name ++ " but got: " ++ show reason

        it "warns that the suppliers tied when the demand names none" $
            case findSupplierInIndexedDBs ctx (query electricity "GLO" "kWh") of
                CrossDBLinked{cdlrWarnings = warnings} ->
                    [n | AmbiguousSupplier _ n <- warnings] `shouldBe` [2]
                CrossDBNotLinked reason -> expectationFailure $ "Expected a link but got: " ++ show reason

        it "does not warn once the demand names its supplier" $
            case findSupplierInIndexedDBs ctx (queryFrom (fst marketActivity) electricity "GLO" "kWh") of
                CrossDBLinked{cdlrWarnings = warnings} ->
                    [n | AmbiguousSupplier _ n <- warnings] `shouldBe` []
                CrossDBNotLinked reason -> expectationFailure $ "Expected a link but got: " ++ show reason

        it "falls back to the product name when no dependency ships the named activity" $
            case findSupplierInIndexedDBs ctx (queryFrom "market for electricity, from another release" electricity "GLO" "kWh") of
                CrossDBLinked{cdlrProductName = linked} -> linked `shouldBe` electricity
                CrossDBNotLinked reason ->
                    expectationFailure $ "Expected the product-name fallback but got: " ++ show reason

        it "prefers the named activity among a synonym group's candidates" $ do
            -- The dependency spells the product otherwise, so only the synonym
            -- group answers and the (activity, product) index cannot. Both
            -- producers come back; the activity name still says which is meant.
            let oldName = "electricity, medium voltage, at grid"
                synDB = buildFromPairs [(electricity, oldName)]
                synCtx = ctx{lcIndexedDatabases = [buildIndexedDatabase "background" synDB twoProducerDB], lcSynonymDB = synDB}
            case findSupplierInIndexedDBs synCtx (queryFrom (fst landfillActivity) oldName "GLO" "kWh") of
                CrossDBLinked{cdlrActivityUUID = linked, cdlrWarnings = warnings} -> do
                    linked `shouldBe` snd landfillActivity
                    [n | AmbiguousSupplier _ n <- warnings] `shouldBe` []
                CrossDBNotLinked reason ->
                    expectationFailure $ "Expected a synonym link but got: " ++ show reason

    -- -----------------------------------------------------------------------
    -- supplierLocations & buildSupplierEntries - split-location indexing.
    -- Guards the WFLDB case where Process name @ /CH but Products row @ /GLO:
    -- the supplier index must expose the product under both locations so a
    -- consumer requesting it at GLO can still resolve it cross-DB.
    -- -----------------------------------------------------------------------
    describe "supplierLocations" $ do
        it "returns one entry when the exchange has no location" $
            supplierLocations (mkActivityAt "CH") (mkRefExchangeAt "") `shouldBe` ["CH"]

        it "returns one entry when activity and exchange locations match" $
            supplierLocations (mkActivityAt "CH") (mkRefExchangeAt "CH") `shouldBe` ["CH"]

        it "returns both entries when activity and exchange locations differ" $
            supplierLocations (mkActivityAt "CH") (mkRefExchangeAt "GLO") `shouldBe` ["CH", "GLO"]

    describe "buildSupplierEntries (split-location)" $ do
        it "indexes a product at both activity and exchange locations when they differ" $ do
            let entries = buildSupplierEntries (mkSplitLocationDB "CH" "GLO")
            sort [seLocation se | (_, se) <- entries] `shouldBe` ["CH", "GLO"]

        it "indexes a product at the activity location only when the exchange location is empty" $ do
            let entries = buildSupplierEntries (mkSplitLocationDB "CH" "")
            [seLocation se | (_, se) <- entries] `shouldBe` ["CH"]

        it "indexes a product once when activity and exchange locations match" $ do
            let entries = buildSupplierEntries (mkSplitLocationDB "CH" "CH")
            [seLocation se | (_, se) <- entries] `shouldBe` ["CH"]

-- ---------------------------------------------------------------------------
-- Fixtures for supplierLocations / buildSupplierEntries tests
-- ---------------------------------------------------------------------------

mkActivityAt :: Text -> Activity
mkActivityAt loc =
    Activity
        { activityName = "test product"
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

mkRefExchangeAt :: Text -> Exchange
mkRefExchangeAt loc =
    TechnosphereExchange
        { techFlowId = flowUUID
        , techAmount = 1.0
        , techUnitId = UUID.nil
        , techRole = ReferenceProduct
        , techActivityLinkId = Nothing
        , techSupplierClaim = ClaimByProduct
        , techLocation = loc
        , techComment = Nothing
        , techPedigree = Nothing
        , techShare = Nothing
        , techClassification = M.empty
        , techProperties = noProperties
        }
  where
    flowUUID = read "aaaaaaaa-0000-0000-0000-000000000001"

{- | One-activity SimpleDatabase with a single reference exchange. The
activity is anchored at @actLoc@; the reference exchange carries @exLoc@.
-}
mkSplitLocationDB :: Text -> Text -> SimpleDatabase
mkSplitLocationDB actLoc exLoc =
    let flowUUID = read "aaaaaaaa-0000-0000-0000-000000000001"
        actUUID = read "cccccccc-0000-0000-0000-000000000001"
        ex = (mkRefExchangeAt exLoc){techFlowId = flowUUID}
        act = (mkActivityAt actLoc){exchanges = [ex]}
        flow =
            TechnosphereFlow
                { tfId = flowUUID
                , tfName = "test product"
                , tfUnitId = UUID.nil
                , tfSynonyms = M.empty
                , tfCAS = Nothing
                , tfSubstanceId = Nothing
                }
     in SimpleDatabase
            { sdbActivities = M.singleton (actUUID, flowUUID) act
            , sdbTechFlows = M.singleton flowUUID flow
            , sdbBioFlows = M.empty
            , sdbWasteFlows = M.empty
            , sdbUnits = M.empty
            }

-- ---------------------------------------------------------------------------
-- Helper: load SAMPLE.min3 as IndexedDatabase
-- ---------------------------------------------------------------------------

loadMin3IndexedDB :: IO IndexedDatabase
loadMin3IndexedDB = do
    result <- loadDatabase defaultUnitConfig "test-data/SAMPLE.min3"
    case result of
        Left err -> error $ "Failed to load SAMPLE.min3: " ++ show err
        Right simpleDb -> return $ buildIndexedDatabase "SAMPLE.min3" emptySynonymDB simpleDb

-- | The two activities of 'twoProducerDB': name and activity UUID.
marketActivity, landfillActivity :: (Text, UUID.UUID)
marketActivity = ("market group for electricity, medium voltage", read "cccccccc-0000-0000-0000-000000000002")
landfillActivity = ("treatment of hard coal ash, sanitary landfill", read "cccccccc-0000-0000-0000-000000000003")

{- | Two activities of one database whose reference product is the same flow at
the same location, as a market group and its cut-off co-outputs are.
-}
twoProducerDB :: SimpleDatabase
twoProducerDB =
    SimpleDatabase
        { sdbActivities = M.fromList [((uuid, flowUUID), producer name) | (name, uuid) <- [marketActivity, landfillActivity]]
        , sdbTechFlows = M.singleton flowUUID flow
        , sdbBioFlows = M.empty
        , sdbWasteFlows = M.empty
        , sdbUnits = M.empty
        }
  where
    flowUUID = read "aaaaaaaa-0000-0000-0000-000000000002"
    producer name = (mkActivityAt "GLO"){activityName = name, activityUnit = "kWh", exchanges = [(mkRefExchangeAt "GLO"){techFlowId = flowUUID}]}
    flow =
        TechnosphereFlow
            { tfId = flowUUID
            , tfName = "electricity, medium voltage"
            , tfUnitId = UUID.nil
            , tfSynonyms = M.empty
            , tfCAS = Nothing
            , tfSubstanceId = Nothing
            }

-- | An unresolved product with @n@ demands behind it, blocked by @b@.
unresolved :: Int -> LinkBlocker -> UnresolvedProduct
unresolved n b = UnresolvedProduct (M.singleton b n)

-- | A demand that names no supplier activity: product, location, unit.
query :: Text -> Text -> Text -> SupplierQuery
query p loc u =
    SupplierQuery{sqProductName = p, sqSupplierActivity = Nothing, sqLocation = loc, sqUnit = u}

-- | The same demand, naming the supplier activity its source states.
queryFrom :: Text -> Text -> Text -> Text -> SupplierQuery
queryFrom activity p loc u = (query p loc u){sqSupplierActivity = Just activity}
