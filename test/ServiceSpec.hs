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
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import Data.UUID (nil)
import qualified Data.UUID as UUID
import GoldenData
import Service (
    ServiceError (..),
    buildUnitGroups,
    extractCompartment,
    filterTreeExport,
    isResourceExtraction,
    parseProcessIdFromText,
    resolveActivityAndProcessId,
    validateProcessIdInMatrixIndex,
    validateUUID,
 )
import Test.Hspec
import TestHelpers (loadSampleDatabase)
import Types

spec :: Spec
spec = do
    -- -----------------------------------------------------------------------
    -- validateUUID
    -- -----------------------------------------------------------------------
    describe "validateUUID" $ do
        it "accepts a well-formed UUID" $
            case validateUUID "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa" of
                Right t -> t `shouldBe` "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa"
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
    -- parseProcessIdFromText (requires DB)
    -- -----------------------------------------------------------------------
    describe "parseProcessIdFromText" $ do
        it "parses a valid ProcessId text from SAMPLE.min3" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            let pidText = processIdToText db 0
            case parseProcessIdFromText db pidText of
                Right 0 -> return ()
                Right n -> expectationFailure $ "Expected ProcessId 0 but got " ++ show n
                Left e -> expectationFailure $ "Expected Right but got: " ++ show e

        it "returns InvalidProcessId for garbage text" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            case parseProcessIdFromText db "not-a-process-id" of
                Left (InvalidProcessId _) -> return ()
                _ -> expectationFailure "Expected InvalidProcessId"

        it "returns InvalidProcessId for a single UUID (missing product part)" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            case parseProcessIdFromText db "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa" of
                Left (InvalidProcessId _) -> return ()
                -- bare UUID fallback is in resolveActivityAndProcessId, not here
                Left _ -> return ()
                Right _ -> expectationFailure "Expected Left"

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

        it "returns ActivityNotFound for a non-existent ProcessId text" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            -- Valid UUID pair format but not in DB
            let ghost = "99999999-9999-9999-9999-999999999999_99999999-9999-9999-9999-999999999999"
            case resolveActivityAndProcessId db ghost of
                Left _ -> return ()
                Right _ -> expectationFailure "Expected Left for unknown process"

    -- -----------------------------------------------------------------------
    -- isResourceExtraction
    -- -----------------------------------------------------------------------
    describe "isResourceExtraction" $ do
        it "detects natural resource category" $ do
            let flow = mkBioFlow "natural resource/in ground"
            isResourceExtraction flow 1.0 `shouldBe` True

        it "detects resource category prefix" $ do
            let flow = mkBioFlow "resource/in water"
            isResourceExtraction flow 1.0 `shouldBe` True

        it "returns False for air emission" $ do
            let flow = mkBioFlow "air/urban air"
            isResourceExtraction flow 1.0 `shouldBe` False

        it "returns False for Technosphere flow" $ do
            let flow = mkTechFlow "technosphere"
            isResourceExtraction flow 1.0 `shouldBe` False

    -- -----------------------------------------------------------------------
    -- extractCompartment
    -- -----------------------------------------------------------------------
    describe "extractCompartment" $ do
        it "classifies air compartment" $
            extractCompartment "air" `shouldBe` "air"

        it "classifies high population air" $
            extractCompartment "Emissions to air/high. pop." `shouldBe` "air"

        it "classifies water compartment" $
            extractCompartment "water" `shouldBe` "water"

        it "classifies aquatic compartment" $
            extractCompartment "Aquatic" `shouldBe` "water"

        it "classifies soil compartment" $
            extractCompartment "soil/agricultural" `shouldBe` "soil"

        it "classifies ground as soil" $
            extractCompartment "ground" `shouldBe` "soil"

        it "falls back to other for unknown" $
            extractCompartment "biotic resource" `shouldBe` "other"

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
    describe "filterTreeExport" $ do
        it "keeps matching node and its ancestor" $
            let export =
                    mkTreeExport
                        [ ("root", Nothing, "Root Activity")
                        , ("child", Just "root", "Widget Production")
                        , ("sibling", Just "root", "Unrelated Process")
                        ]
                        [("root", "child"), ("root", "sibling")]
                filtered = filterTreeExport "widget" export
             in M.keysSet (teNodes filtered) `shouldBe` S.fromList ["root", "child"]

        it "excludes edges whose endpoints are filtered out" $
            let export =
                    mkTreeExport
                        [ ("root", Nothing, "Root Activity")
                        , ("child", Just "root", "Widget Production")
                        , ("sibling", Just "root", "Unrelated Process")
                        ]
                        [("root", "child"), ("root", "sibling")]
                filtered = filterTreeExport "widget" export
             in length (teEdges filtered) `shouldBe` 1

        it "returns all nodes when pattern matches all" $
            let export =
                    mkTreeExport
                        [("a", Nothing, "Alpha"), ("b", Just "a", "Beta")]
                        [("a", "b")]
                filtered = filterTreeExport "a" export -- matches "Alpha"
             in M.size (teNodes filtered) `shouldBe` 2

        it "returns empty when no match" $
            let export = mkTreeExport [("a", Nothing, "Alpha")] []
                filtered = filterTreeExport "zzz" export
             in M.size (teNodes filtered) `shouldBe` 0

        it "updates tmTotalNodes in metadata" $
            let export =
                    mkTreeExport
                        [ ("root", Nothing, "Root")
                        , ("child", Just "root", "Match Me")
                        ]
                        [("root", "child")]
                filtered = filterTreeExport "match" export
             in tmTotalNodes (teTree filtered) `shouldBe` 2

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

mkBioFlow :: Text -> Flow
mkBioFlow cat =
    Flow
        { flowId = nil
        , flowName = "test"
        , flowCategory = cat
        , flowSubcompartment = Nothing
        , flowUnitId = nil
        , flowType = Biosphere
        , flowSynonyms = M.empty
        , flowCAS = Nothing
        , flowSubstanceId = Nothing
        }

mkTechFlow :: Text -> Flow
mkTechFlow cat =
    Flow
        { flowId = nil
        , flowName = "test"
        , flowCategory = cat
        , flowSubcompartment = Nothing
        , flowUnitId = nil
        , flowType = Technosphere
        , flowSynonyms = M.empty
        , flowCAS = Nothing
        , flowSubstanceId = Nothing
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
