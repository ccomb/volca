{-# LANGUAGE OverloadedStrings #-}

module ServiceSpec (spec) where

import API.Types (
    EdgeType (..),
    ExportNode (..),
    FlowInfo (..),
    NodeType (..),
    TreeEdge (..),
    TreeExport (..),
    TreeMetadata (..),
 )
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import Data.UUID (fromWords, nil, toText)
import Service (
    NamePattern (..),
    ServiceError (..),
    TargetRef (..),
    buildUnitGroups,
    filterTreeExport,
    isResourceExtraction,
    resolveActivityAndProcessId,
    validateProcessIdInMatrixIndex,
    validateUUID,
    wasteRoleOf,
 )
import Test.Hspec
import TestHelpers (loadSampleDatabase)
import Types
import qualified Types as VT

spec :: Spec
spec = do
    -- -----------------------------------------------------------------------
    -- validateUUID
    -- -----------------------------------------------------------------------
    describe "validateUUID" $ do
        it "accepts a well-formed UUID and returns the parsed value" $
            case validateUUID "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa" of
                Right u -> toText u `shouldBe` "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa"
                Left _ -> expectationFailure "Expected Right"

        it "rejects an empty string" $
            case validateUUID "" of
                Left (InvalidUUID _) -> return ()
                _ -> expectationFailure "Expected InvalidUUID"

        it "rejects a plain word" $
            case validateUUID "not-a-uuid" of
                Left (InvalidUUID _) -> return ()
                _ -> expectationFailure "Expected InvalidUUID"

        it "rejects a truncated UUID" $
            case validateUUID "aaaaaaaa-aaaa-aaaa-aaaa" of
                Left (InvalidUUID _) -> return ()
                _ -> expectationFailure "Expected InvalidUUID"

    -- -----------------------------------------------------------------------
    -- validateProcessIdInMatrixIndex
    -- -----------------------------------------------------------------------
    describe "validateProcessIdInMatrixIndex" $ do
        it "accepts ProcessId 0 in SAMPLE.min3 (3 activities)" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            case validateProcessIdInMatrixIndex db 0 of
                Right () -> return ()
                Left e -> expectationFailure $ "Expected Right but got: " ++ show e

        it "rejects a ProcessId beyond the activity count" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            case validateProcessIdInMatrixIndex db 999 of
                Left (MatrixError _) -> return ()
                _ -> expectationFailure "Expected MatrixError"

        it "rejects a negative ProcessId" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            case validateProcessIdInMatrixIndex db (-1) of
                Left (MatrixError _) -> return ()
                _ -> expectationFailure "Expected MatrixError"

    -- -----------------------------------------------------------------------
    -- resolveActivityAndProcessId
    -- -----------------------------------------------------------------------
    describe "resolveActivityAndProcessId" $ do
        it "resolves activity X by full ProcessId text" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let pidText = processIdToText db 0
            case resolveActivityAndProcessId db pidText of
                Right (pid, act) -> do
                    pid `shouldBe` 0
                    activityName act `shouldBe` "production of product X"
                Left err -> expectationFailure $ "Expected Right but got: " ++ show err

        it "falls back to bare activity UUID (EcoInvent compatibility)" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            -- activity UUID without product UUID part
            case resolveActivityAndProcessId db "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa" of
                Right (_, act) -> activityName act `shouldBe` "production of product X"
                Left err -> expectationFailure $ "Expected Right but got: " ++ show err

        it "returns ActivityNotFound for a well-formed but non-existent ProcessId text" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            -- Valid UUID pair format but not in DB
            let ghost = "99999999-9999-9999-9999-999999999999_99999999-9999-9999-9999-999999999999"
            case resolveActivityAndProcessId db ghost of
                Left (ActivityNotFound _) -> return ()
                Left err -> expectationFailure $ "Expected ActivityNotFound but got: " ++ show err
                Right _ -> expectationFailure "Expected ActivityNotFound but got a hit"

        it "returns ActivityNotFound for a valid but absent bare UUID" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            case resolveActivityAndProcessId db "99999999-9999-9999-9999-999999999999" of
                Left (ActivityNotFound _) -> return ()
                Left err -> expectationFailure $ "Expected ActivityNotFound but got: " ++ show err
                Right _ -> expectationFailure "Expected ActivityNotFound but got a hit"

        it "refuses a bare activity UUID naming an activity written as several processes" $ do
            db <- loadSampleDatabase "SAMPLE.switching"
            case M.keys (M.filter ((> 1) . NE.length) (dbActivityUUIDIndex db)) of
                [] -> expectationFailure "fixture no longer has an activity written as several rows"
                (actUUID : _) -> case resolveActivityAndProcessId db (toText actUUID) of
                    Left (AmbiguousActivity _) -> return ()
                    Left err -> expectationFailure $ "Expected AmbiguousActivity but got: " ++ show err
                    Right _ -> expectationFailure "Expected AmbiguousActivity but got a hit"

        it "returns InvalidProcessId for a genuinely malformed query" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            case resolveActivityAndProcessId db "not-a-process-id" of
                Left (InvalidProcessId _) -> return ()
                Left err -> expectationFailure $ "Expected InvalidProcessId but got: " ++ show err
                Right _ -> expectationFailure "Expected InvalidProcessId but got a hit"

    -- -----------------------------------------------------------------------
    -- isResourceExtraction
    -- -----------------------------------------------------------------------
    describe "isResourceExtraction" $ do
        it "detects natural resource category" $ do
            let flow = mkBioFlow NaturalResource
            isResourceExtraction flow `shouldBe` True

        it "detects resource category prefix" $ do
            let flow = mkBioFlow NaturalResource
            isResourceExtraction flow `shouldBe` True

        it "returns False for air emission" $ do
            let flow = mkBioFlow Air
            isResourceExtraction flow `shouldBe` False
    -- Technosphere flows can't reach this function under the new type system.

    -- -----------------------------------------------------------------------
    -- buildUnitGroups
    -- -----------------------------------------------------------------------
    describe "buildUnitGroups" $ do
        it "classifies mass units" $
            M.lookup "kg" (buildUnitGroups ["kg"]) `shouldBe` Just "mass"

        it "classifies energy units" $
            M.lookup "MJ" (buildUnitGroups ["MJ"]) `shouldBe` Just "energy"

        it "classifies volume units" $
            M.lookup "m3" (buildUnitGroups ["m3"]) `shouldBe` Just "volume"

        it "falls back to other for unknown unit" $
            M.lookup "p" (buildUnitGroups ["p"]) `shouldBe` Just "other"

        it "deduplicates repeated units" $
            M.size (buildUnitGroups ["kg", "kg", "kg"]) `shouldBe` 1

    -- -----------------------------------------------------------------------
    -- filterTreeExport
    -- -----------------------------------------------------------------------
    -- -----------------------------------------------------------------------
    -- wasteRoleOf
    -- -----------------------------------------------------------------------
    describe "wasteRoleOf" $ do
        let treatment = TargetRef "waste oil incineration" "GLO" "aaa_bbb"
            -- A source that names a treatment says so on the claim; linking
            -- puts what it resolved to on the link, and the two agree here.
            wasteLine isInput link =
                WasteExchange
                    { waFlowId = nil
                    , waAmount = 1.0
                    , waUnitId = nil
                    , waIsInput = isInput
                    , waActivityLinkId = link
                    , waSupplierClaim = maybe ClaimByProduct ClaimById link
                    , waLocation = ""
                    , waComment = Nothing
                    , waPedigree = Nothing
                    , waFormula = Nothing
                    }
            linked = Just (fromWords 1 2 3 4)

        it "calls an input a treatment of the waste" $
            wasteRoleOf (Just treatment) (wasteLine True linked) `shouldBe` Just TreatsWaste

        it "calls a resolved output a waste sent to treatment" $
            wasteRoleOf (Just treatment) (wasteLine False linked) `shouldBe` Just SentToTreatment

        it "calls an output naming no treatment a final waste flow" $
            wasteRoleOf Nothing (wasteLine False Nothing) `shouldBe` Just FinalWasteFlow

        -- The distinction the whole field exists for: this output states that
        -- something treats the waste, so calling it final would report a
        -- missing database as an accounted-for end of life.
        it "does not call an output whose named treatment is missing final" $
            wasteRoleOf Nothing (wasteLine False linked) `shouldBe` Just TreatmentNotLoaded

        it "leaves every other kind of line without a role" $ do
            let bio = BiosphereExchange nil 1.0 nil Emission "" Nothing Nothing Nothing
                tech = TechnosphereExchange nil 1.0 nil Input Nothing ClaimByProduct "" Nothing Nothing Nothing M.empty noProperties Nothing
            wasteRoleOf Nothing bio `shouldBe` Nothing
            wasteRoleOf (Just treatment) tech `shouldBe` Nothing

    describe "filterTreeExport" $ do
        it "keeps matching node and its ancestor" $
            let export =
                    mkTreeExport
                        [ ("root", Nothing, "Root Activity")
                        , ("child", Just "root", "Widget Production")
                        , ("sibling", Just "root", "Unrelated Process")
                        ]
                        [("root", "child"), ("root", "sibling")]
                filtered = filterTreeExport (NamePattern "widget") export
             in M.keysSet (teNodes filtered) `shouldBe` S.fromList ["root", "child"]

        it "excludes edges whose endpoints are filtered out" $
            let export =
                    mkTreeExport
                        [ ("root", Nothing, "Root Activity")
                        , ("child", Just "root", "Widget Production")
                        , ("sibling", Just "root", "Unrelated Process")
                        ]
                        [("root", "child"), ("root", "sibling")]
                filtered = filterTreeExport (NamePattern "widget") export
             in length (teEdges filtered) `shouldBe` 1

        it "returns all nodes when pattern matches all" $
            let export =
                    mkTreeExport
                        [("a", Nothing, "Alpha"), ("b", Just "a", "Beta")]
                        [("a", "b")]
                filtered = filterTreeExport (NamePattern "a") export -- matches "Alpha"
             in M.size (teNodes filtered) `shouldBe` 2

        it "returns empty when no match" $
            let export = mkTreeExport [("a", Nothing, "Alpha")] []
                filtered = filterTreeExport (NamePattern "zzz") export
             in M.size (teNodes filtered) `shouldBe` 0

        it "updates tmTotalNodes in metadata" $
            let export =
                    mkTreeExport
                        [ ("root", Nothing, "Root")
                        , ("child", Just "root", "Match Me")
                        ]
                        [("root", "child")]
                filtered = filterTreeExport (NamePattern "match") export
             in tmTotalNodes (teTree filtered) `shouldBe` 2

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

mkBioFlow :: VT.Medium -> BiosphereFlow
mkBioFlow cat =
    BiosphereFlow
        { bfId = nil
        , bfName = "test"
        , bfUnitId = nil
        , bfSynonyms = M.empty
        , bfCAS = Nothing
        , bfSubstanceId = Nothing
        , bfCompartment = Just (VT.Compartment cat Nothing)
        }

-- | Build a minimal TreeExport from a list of (id, parentId, name) and edges (from,to)
mkTreeExport :: [(Text, Maybe Text, Text)] -> [(Text, Text)] -> TreeExport
mkTreeExport nodeSpecs edgeSpecs =
    let mkNode (nid, parent, name) =
            ( nid
            , ExportNode
                { enId = nid
                , enName = name
                , enDescription = []
                , enLocation = ""
                , enUnit = "kg"
                , enNodeType = ActivityNode
                , enDepth = 0
                , enLoopTarget = Nothing
                , enParentId = parent
                , enChildrenCount = 0
                , enCompartment = Nothing
                }
            )
        nodes = M.fromList (map mkNode nodeSpecs)
        dummyFlow = FlowInfo nil "" ""
        mkEdge (f, t) = TreeEdge f t dummyFlow 1.0 "kg" TechnosphereEdge
        edges = map mkEdge edgeSpecs
        meta = TreeMetadata "" 1 (M.size nodes) 0 0 0
     in TreeExport meta nodes edges
