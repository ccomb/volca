{-# LANGUAGE OverloadedStrings #-}

module SynonymDBSpec (spec) where

import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text, pack)
import Test.Hspec

import SynonymDB (BridgeDirection (..), RegistryRow (..), SynEdge (..), SynViews (..), buildFromCSV, buildFromEdges, buildFromPairs, excludeJunkSynonyms, excludeOverFrequentSynonyms, getSynonyms, inputView, isJunkSynonymName, loadFromCSVFileWithCache, lookupSynonymGroup, mergeSynonymDBs, normalizeName, outputView, oversizedClasses, parseRegistryCSV, reopenedBridges, synEdges, synViews, uncoveredUnitSuffixes)

spec :: Spec
spec = do
    describe "loadFromCSVFileWithCache" $
        it "returns Left for a missing CSV instead of throwing" $ do
            -- Regression: the load used a bare readFile, so a missing
            -- reference file threw an uncaught IOException and took down
            -- server startup. The type promises Either; a missing file
            -- must surface as Left, like the other reference-data loaders.
            result <- loadFromCSVFileWithCache "test-data/does-not-exist-synonyms.csv"
            case result of
                Left _ -> pure ()
                Right _ -> expectationFailure "expected Left for a missing CSV file"

    describe "buildFromPairs transitive closure" $ do
        let db = buildFromPairs [("alpha", "beta"), ("beta", "gamma")]
            classFor name = S.fromList <$> (lookupSynonymGroup db name >>= getSynonyms db)

        it "groups chained synonyms (a=b, b=c) into one class, reachable from either end" $
            classFor "alpha" `shouldBe` Just (S.fromList ["alpha", "beta", "gamma"])

        it "gives both ends of the chain the same group id" $
            lookupSynonymGroup db "alpha" `shouldBe` lookupSynonymGroup db "gamma"

    describe "directional bridges" $ do
        let groupIn db name = S.fromList <$> (lookupSynonymGroup (inputView db) name >>= getSynonyms (inputView db))
            groupOut db name = S.fromList <$> (lookupSynonymGroup (outputView db) name >>= getSynonyms (outputView db))

        it "leaves untyped data with coinciding views (AllBoth, no duplication)" $ do
            let db = buildFromPairs [("alpha", "beta")]
            synViews db `shouldBe` AllBoth
            inputView db `shouldBe` db
            outputView db `shouldBe` db

        it "parses a 2-column CSV as all-both, a 3-column CSV with a direction" $ do
            let two = buildFromCSV (BLC.pack "name1,name2\nalpha,beta\n")
                three = buildFromCSV (BLC.pack "name1,name2,direction\nriver water,Water river,input\n")
            fmap synViews two `shouldBe` Right AllBoth
            case three of
                Left e -> expectationFailure e
                Right db -> do
                    groupIn db "river water" `shouldBe` Just (S.fromList ["river water", "water river"])
                    groupOut db "river water" `shouldBe` Nothing

        it "treats an empty direction column as both" $
            case buildFromCSV (BLC.pack "name1,name2,direction\nalpha,beta,\n") of
                Left e -> expectationFailure e
                Right db -> synViews db `shouldBe` AllBoth

        it "rejects an unknown direction token instead of silently coercing it" $
            case buildFromCSV (BLC.pack "name1,name2,direction\nalpha,beta,sideways\n") of
                Left _ -> pure ()
                Right _ -> expectationFailure "expected Left for an invalid direction token"

        it "keeps cas/note metadata on parsed rows without feeding it to matching" $ do
            -- 4- and 5-column rows: the metadata is exposed for the registry
            -- lint; the built SynonymDB is the same as without it.
            let csv = BLC.pack "name1,name2,direction,cas,note\nCFC-11,Methane trichlorofluoro-,,75-69-4,ozd bridge\nalpha,beta,, ,\n"
            case parseRegistryCSV csv of
                Left e -> expectationFailure e
                Right rows -> do
                    map rrCas rows `shouldBe` [Just "75-69-4", Nothing]
                    map rrNote rows `shouldBe` [Just "ozd bridge", Nothing]
                    map (seDir . rrEdge) rows `shouldBe` [BridgeBoth, BridgeBoth]
            fmap synViews (buildFromCSV csv) `shouldBe` Right AllBoth

        it "rejects rows with more than 5 columns" $
            case buildFromCSV (BLC.pack "name1,name2,direction,cas,note\na,b,,,,surplus\n") of
                Left _ -> pure ()
                Right _ -> expectationFailure "expected Left for a 6-column row"

        it "splits a group along direction: an input edge chained to a both edge" $ do
            -- a-b [input], b-c [both]. Input view fuses {a,b,c}; output view, missing
            -- the a-b link, keeps only {b,c} - a split the union tables cannot recover.
            let db = buildFromEdges [SynEdge "a" "b" BridgeInput, SynEdge "b" "c" BridgeBoth]
            groupIn db "a" `shouldBe` Just (S.fromList ["a", "b", "c"])
            groupOut db "a" `shouldBe` Nothing
            groupOut db "b" `shouldBe` Just (S.fromList ["b", "c"])

        it "keeps direction tags through a merge" $ do
            let merged = mergeSynonymDBs [buildFromEdges [SynEdge "river water" "Water river" BridgeInput], buildFromPairs [("x", "y")]]
            groupIn merged "river water" `shouldBe` Just (S.fromList ["river water", "water river"])
            groupOut merged "river water" `shouldBe` Nothing

        it "does not re-normalize stored edges on merge (normalizeName is not idempotent)" $ do
            -- "Zinc in ground," single-pass-normalizes to "zinc in ground" (the
            -- trailing comma hides the suffix until the punctuation pass); a second
            -- application would strip it to "zinc". The merged tables must stay
            -- keyed by the single-pass form every lookup applies.
            let merged = mergeSynonymDBs [buildFromPairs [("Zinc in ground,", "Zn")], buildFromPairs [("alpha", "beta")]]
            lookupSynonymGroup merged "Zinc in ground," `shouldSatisfy` (/= Nothing)
            lookupSynonymGroup merged "Zn" `shouldBe` lookupSynonymGroup merged "Zinc in ground,"

        it "demotes a both-duplicate of a directed pair so it cannot reopen the bridge" $ do
            -- A merged untyped duplicate of an input-only pair must NOT resurface it
            -- in the output view.
            let db = buildFromEdges [SynEdge "river water" "Water river" BridgeInput, SynEdge "river water" "Water river" BridgeBoth]
            length (synEdges db) `shouldBe` 1
            groupOut db "river water" `shouldBe` Nothing

        it "loads the shipped data/flows.csv and keeps its water bridges input-only" $ do
            -- Guards the real curated registry: it must parse (3-column direction
            -- schema), and the water withdrawal bridges must reach their resource
            -- flow only in the input view, never the output view - and no untyped
            -- row may void a one-way constraint ('reopenedBridges').
            loaded <- loadFromCSVFileWithCache "data/flows.csv"
            case loaded of
                Left e -> expectationFailure e
                Right db -> do
                    lookupSynonymGroup (inputView db) "freshwater" `shouldSatisfy` (/= Nothing)
                    lookupSynonymGroup (outputView db) "freshwater" `shouldBe` Nothing
                    lookupSynonymGroup (outputView db) "river water" `shouldBe` Nothing
                    reopenedBridges db `shouldBe` []

    describe "reopenedBridges" $ do
        it "flags a one-way bridge reopened by an untyped transitive chain" $ do
            -- 'demoteDuplicates' removes only the exact duplicate pair; a chain of
            -- untyped edges through an intermediate re-links the endpoints in the
            -- view the direction was meant to close. That must be surfaced.
            let db =
                    buildFromEdges
                        [ SynEdge "freshwater" "Water unspecified" BridgeInput
                        , SynEdge "freshwater" "surface water" BridgeBoth
                        , SynEdge "surface water" "Water unspecified" BridgeBoth
                        ]
            reopenedBridges db `shouldBe` [SynEdge "freshwater" "water unspecified" BridgeInput]

        it "flags a pair typed both input and output (contradictory curation widens to both)" $ do
            let db = buildFromEdges [SynEdge "a" "b" BridgeInput, SynEdge "a" "b" BridgeOutput]
            length (reopenedBridges db) `shouldBe` 2

        it "stays silent on consistent data" $ do
            let db = buildFromEdges [SynEdge "river water" "Water river" BridgeInput, SynEdge "x" "y" BridgeBoth]
            reopenedBridges db `shouldBe` []

    describe "oversizedClasses" $ do
        -- A junk hub fuses everything it touches into one transitive class.
        let hub = [("hub", "s" <> pack (show i)) | i <- [1 :: Int .. 12]]

        it "flags a class larger than the bound (a closure that fused a junk hub)" $
            map length (oversizedClasses 10 hub) `shouldBe` [13]

        it "stays silent when every class is within the bound" $
            oversizedClasses 10 [("alpha", "beta"), ("beta", "gamma")]
                `shouldBe` []

    describe "excludeOverFrequentSynonyms" $ do
        -- "organic" acts as a synonym for 3 distinct flows -> a class label, dropped.
        -- "acetaminophen" merely HAS 3 synonyms (out-degree) -> a real flow, kept.
        let pairs =
                [ ("benzene", "organic")
                , ("toluene", "organic")
                , ("phenol", "organic")
                , ("acetaminophen", "paracetamol")
                , ("acetaminophen", "tylenol")
                , ("acetaminophen", "apap")
                ]
            (kept, excluded) = excludeOverFrequentSynonyms 2 pairs

        it "drops the over-frequent synonym and surfaces it with its flow count" $ do
            excluded `shouldBe` [("organic", 3)]
            ("organic" `elem` map snd kept) `shouldBe` False

        it "keeps a real flow that merely has many synonyms (out-degree, not in-degree)" $
            kept `shouldMatchList` [("acetaminophen", s) | s <- ["paracetamol", "tylenol", "apap"]]

        it "counts case/punctuation variants of a synonym together (normalized)" $
            snd (excludeOverFrequentSynonyms 2 [("a", "Organic"), ("b", "organic"), ("c", "ORGANIC")])
                `shouldBe` [("organic", 3)]

    describe "excludeJunkSynonyms" $ do
        -- Dossier placeholders / id stubs are dropped; real substances survive,
        -- including names that contain "(mixture)" or are digit-heavy.
        let pairs =
                [ ("arsenic", "not available")
                , ("benzene", "unknown")
                , ("sodium hydroxide", "98%activematter")
                , ("n-butane", "echa-8600dbe1-6174-49ec-b025-9cd03d318e49")
                , ("toluene diisocyanate", "2,4/2,6-toluenediisocyanate (mixture)")
                , ("hexachlorocyclohexane", "pcb-1254")
                ]
            (kept, dropped) = excludeJunkSynonyms pairs

        it "drops pairs touching a placeholder/id-stub token, keeps real ones" $
            kept
                `shouldMatchList` [ ("toluene diisocyanate", "2,4/2,6-toluenediisocyanate (mixture)")
                                  , ("hexachlorocyclohexane", "pcb-1254")
                                  ]

        it "surfaces the distinct dropped tokens" $
            length dropped `shouldBe` 4

        it "flags dossier prose and ECHA id stubs" $ do
            isJunkSynonymName "not available" `shouldBe` True
            isJunkSynonymName "unknown atom or ion" `shouldBe` True
            isJunkSynonymName "100%activematter" `shouldBe` True
            isJunkSynonymName "echa-8600dbe1-6174-49ec-b025-9cd03d318e49" `shouldBe` True

        it "spares real names with 'mixture', digits, or an inner 'echa'" $ do
            isJunkSynonymName "2,4/2,6-toluenediisocyanate (mixture)" `shouldBe` False
            isJunkSynonymName "pcb-1254" `shouldBe` False
            isJunkSynonymName "carbon 14" `shouldBe` False
            isJunkSynonymName "huile de chauffage" `shouldBe` False

        it "drops bare numeric ids and registry numbers (ENT, CIPAC)" $ do
            isJunkSynonymName "ENT 27164" `shouldBe` True
            isJunkSynonymName "ENT 27,164" `shouldBe` True
            isJunkSynonymName "27164" `shouldBe` True
            isJunkSynonymName "CIPAC 12" `shouldBe` True

        -- The ozd leak: carbon tetrachloride's ODP factor reached carbofuran
        -- because both carry the USDA number "ENT 27164" / "ENT 27,164", which
        -- normalize to the same token. Dropping the id breaks the bridge.
        it "drops a registry-id bridge so it cannot fuse unrelated substances" $ do
            let raw = [("carbon tetrachloride", "ENT 27164"), ("carbofuran", "ENT 27,164")]
                (kept, dropped) = excludeJunkSynonyms raw
            kept `shouldBe` []
            dropped `shouldBe` ["ent 27164"]

    describe "normalizeName" $ do
        it "strips a trailing SimaPro unit suffix (/kg, /m3, /Sm3) so unit variants share a node" $ do
            normalizeName "Gas, natural/m3" `shouldBe` "gas natural"
            normalizeName "Gas, natural/Sm3" `shouldBe` "gas natural"
            normalizeName "Coal, hard/kg" `shouldBe` "coal hard"

        it "leaves a name without a unit suffix unchanged (modulo casing/punctuation)" $
            normalizeName "Gas, natural" `shouldBe` "gas natural"

    describe "uncoveredUnitSuffixes" $ do
        -- A stand-in unit vocabulary; the real predicate is UnitConversion.isKnownUnit.
        let knownUnit = (`elem` (["MJ", "kg", "m3", "Sm3", "ha"] :: [Text]))
            grouped = M.map S.fromList . uncoveredUnitSuffixes knownUnit

        it "flags a flow whose /unit suffix is a real unit not covered by unitSuffixes" $
            grouped ["Electricity/MJ", "Heat/MJ", "Occupation, pasture/ha"]
                `shouldBe` M.fromList
                    [ ("MJ", S.fromList ["Electricity/MJ", "Heat/MJ"])
                    , ("ha", S.fromList ["Occupation, pasture/ha"])
                    ]

        it "ignores suffixes already stripped by unitSuffixes (/kg, /m3, /Sm3, any case)" $
            uncoveredUnitSuffixes knownUnit ["Gas, natural/m3", "Gas/Sm3", "Coal/kg"]
                `shouldBe` M.empty

        it "ignores a trailing slash that is not a known unit" $
            uncoveredUnitSuffixes knownUnit ["cis/trans-isomer", "Carbon dioxide"]
                `shouldBe` M.empty
