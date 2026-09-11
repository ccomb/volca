{-# LANGUAGE OverloadedStrings #-}

{- | Round-trip contract for the EcoSpold2 writer ('EcoSpold.Writer2'), the
inverse of 'EcoSpold.Parser2'.

We exercise three properties against a self-contained fixture 'SimpleDatabase'
built in Haskell:

  (a) __idempotence modulo volatile metadata__: @write(D)@ then
      @write(parse(write(D)))@ produce byte-identical output once volatile
      metadata is excluded (we exclude it by writing with 'noVolatileMeta').
  (b) __semantic round-trip__: @parse(write(D))@ is structurally equal to @D@
      (order-insensitive on exchanges/flows).
  (c) __score-equivalence__: @parse(write(D))@ yields the same LCIA inventory
      (the engine's matrix-level score input) as @D@ within tolerance.

The fixture is constructed directly rather than loaded from a bundled
@.spold@ directory because the bundled @SAMPLE.units@ fixture deliberately
reuses a single @unitId@ across flows with *different* unit-name strings (a
degenerate-input stress test for the parser). The loader collapses those to
one unit per UUID in name-resolution order, which depends on filename
ordering, a property of the loader on malformed input, not of the writer.
A clean fixture (one name per unit UUID, distinct flow UUIDs) isolates the
writer's own determinism, which is what this spec asserts.
-}
module EcoSpold2WriterSpec (spec) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Data.Either (isLeft, isRight)
import Data.List (sort, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import Database (buildDatabaseWithMatrices)
import Database.Loader (loadDatabase)
import EcoSpold.Writer2 (VolatileMeta (..), checkEcoSpold2Exportable, noVolatileMeta, writeEcoSpold2)
import Matrix (computeInventoryMatrix)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Types
import UnitConversion (defaultUnitConfig)

-- | A UUID from its canonical text, or nil if (impossibly) malformed.
uuid :: Text -> UUID.UUID
uuid t = fromMaybe UUID.nil (UUID.fromText t)

-- Distinct, well-formed UUIDs for the fixture.
actA, prodA, actB, prodB :: UUID.UUID
actA = uuid "aaaaaaaa-0000-4000-8000-000000000001"
prodA = uuid "aaaaaaaa-0000-4000-8000-0000000000a1"
actB = uuid "bbbbbbbb-0000-4000-8000-000000000002"
prodB = uuid "bbbbbbbb-0000-4000-8000-0000000000b2"

co2, land :: UUID.UUID
co2 = uuid "cccccccc-0000-4000-8000-0000000000c0"
land = uuid "dddddddd-0000-4000-8000-0000000000d0"

unitKg, unitMJ, unitM2a :: UUID.UUID
unitKg = uuid "11111111-0000-4000-8000-000000000001"
unitMJ = uuid "22222222-0000-4000-8000-000000000002"
unitM2a = uuid "33333333-0000-4000-8000-000000000003"

{- | A self-contained two-activity EcoSpold2 database.

  * Activity A produces product A (1 kg), consumes 2 MJ of product B, and
    emits 0.5 kg CO2 (biosphere).
  * Activity B produces product B (1 MJ) and occupies 0.1 m2*year of land.

Every unit UUID maps to exactly one name; every flow UUID is distinct.
-}
fixtureSimple :: SimpleDatabase
fixtureSimple =
    SimpleDatabase
        { sdbActivities =
            M.fromList
                [ ((actA, prodA), activityA)
                , ((actB, prodB), activityB)
                ]
        , sdbTechFlows =
            M.fromList
                [ (prodA, TechnosphereFlow prodA "product A" unitKg M.empty Nothing Nothing)
                , (prodB, TechnosphereFlow prodB "product B" unitMJ M.empty Nothing Nothing)
                ]
        , sdbBioFlows =
            M.fromList
                [ (co2, BiosphereFlow co2 "Carbon dioxide, fossil" unitKg M.empty (Just "124-38-9") Nothing (Just (Compartment Air (Just "unspecified"))))
                , (land, BiosphereFlow land "Occupation, arable land" unitM2a M.empty Nothing Nothing (Just (Compartment NaturalResource (Just "land"))))
                ]
        , sdbWasteFlows = M.empty
        , sdbUnits =
            M.fromList
                [ (unitKg, Unit unitKg "kg" "kg" "")
                , (unitMJ, Unit unitMJ "MJ" "MJ" "")
                , (unitM2a, Unit unitM2a "m2*year" "m2*year" "")
                ]
        }
  where
    activityA =
        Activity
            "production of A & B <café> ☕"
            ["First fixture activity: a & b <c> — ünïcödé"]
            []
            M.empty
            (M.fromList [("ISIC rev.4 ecoinvent", "2011:Manufacture of food products"), ("CPC", "2399: Other food products n.e.c.")])
            "GLO"
            LocationDeclared
            "kg"
            [ TechnosphereExchange prodA 1.0 unitKg ReferenceProduct Nothing ClaimByProduct "" Nothing Nothing Nothing M.empty noProperties
            , TechnosphereExchange prodB 2.0 unitMJ Input (Just actB) ClaimByProduct "" (Just "energy input") Nothing Nothing M.empty noProperties
            , BiosphereExchange co2 0.5 unitKg Emission "" Nothing Nothing
            ]
            M.empty
            M.empty
            (Just (EcoSpoldActivityType 1 "Ordinary transforming activity" Nothing Nothing))
            Nothing
            Nothing
    activityB =
        Activity
            "production of B"
            []
            []
            M.empty
            M.empty
            "GLO"
            LocationDeclared
            "MJ"
            [ TechnosphereExchange prodB 1.0 unitMJ ReferenceProduct Nothing ClaimByProduct "" Nothing Nothing Nothing M.empty noProperties
            , BiosphereExchange land 0.1 unitM2a Resource "" Nothing Nothing
            ]
            M.empty
            M.empty
            (Just (EcoSpoldActivityType 2 "Market activity" (Just 1) (Just "Hard link")))
            Nothing
            Nothing

-- | The fixture as a 'SimpleDatabase' (matches the previous IO-shaped helper).
loadFixtureSimple :: IO SimpleDatabase
loadFixtureSimple = pure fixtureSimple

{- | A single-activity database whose only activity emits the same biosphere
flow (CO2) twice with distinct amounts (0.5 and 0.3). Used to prove the writer
emits both lines and the round-trip preserves the multiset rather than
collapsing duplicates on the shared sort key.
-}
fixtureDupBio :: SimpleDatabase
fixtureDupBio =
    SimpleDatabase
        { sdbActivities = M.singleton (actA, prodA) activityDup
        , sdbTechFlows = M.singleton prodA (TechnosphereFlow prodA "product A" unitKg M.empty Nothing Nothing)
        , sdbBioFlows = M.singleton co2 (BiosphereFlow co2 "Carbon dioxide, fossil" unitKg M.empty Nothing Nothing (Just (Compartment Air (Just "unspecified"))))
        , sdbWasteFlows = M.empty
        , sdbUnits = M.singleton unitKg (Unit unitKg "kg" "kg" "")
        }
  where
    activityDup =
        Activity
            "production of A"
            []
            []
            M.empty
            M.empty
            "GLO"
            LocationDeclared
            "kg"
            [ TechnosphereExchange prodA 1.0 unitKg ReferenceProduct Nothing ClaimByProduct "" Nothing Nothing Nothing M.empty noProperties
            , BiosphereExchange co2 0.5 unitKg Emission "" Nothing Nothing
            , BiosphereExchange co2 0.3 unitKg Emission "" Nothing Nothing
            ]
            M.empty
            M.empty
            (Just (EcoSpoldActivityType 1 "Ordinary transforming activity" Nothing Nothing))
            Nothing
            Nothing

{- | A single-activity database wrapping one adversarial exchange. The
exchange is the only difference between the rejection fixtures, so the export
guard ('checkEcoSpold2Exportable') is exercised in isolation.
-}
fixtureWithExchange :: Exchange -> SimpleDatabase
fixtureWithExchange ex =
    SimpleDatabase
        { sdbActivities = M.singleton (actA, prodA) activity
        , sdbTechFlows = M.singleton prodA (TechnosphereFlow prodA "product A" unitKg M.empty Nothing Nothing)
        , sdbBioFlows = M.singleton co2 (BiosphereFlow co2 "Carbon dioxide, fossil" unitKg M.empty Nothing Nothing (Just (Compartment Air (Just "unspecified"))))
        , sdbWasteFlows = M.empty
        , sdbUnits = M.singleton unitKg (Unit unitKg "kg" "kg" "")
        }
  where
    activity =
        Activity
            "adversarial activity"
            []
            []
            M.empty
            M.empty
            "GLO"
            LocationDeclared
            "kg"
            [ TechnosphereExchange prodA 1.0 unitKg ReferenceProduct Nothing ClaimByProduct "" Nothing Nothing Nothing M.empty noProperties
            , ex
            ]
            M.empty
            M.empty
            (Just (EcoSpoldActivityType 1 "Ordinary transforming activity" Nothing Nothing))
            Nothing
            Nothing

{- | A single-activity database whose biosphere flow carries synonyms, so the
writer's @\<synonym\>@ emission and the parser's read-back are exercised end to
end (the other fixtures all pass empty synonym maps).
-}
fixtureWithBioSynonyms :: SimpleDatabase
fixtureWithBioSynonyms =
    SimpleDatabase
        { sdbActivities = M.singleton (actA, prodA) activity
        , sdbTechFlows = M.singleton prodA (TechnosphereFlow prodA "product A" unitKg M.empty Nothing Nothing)
        , sdbBioFlows =
            M.singleton
                co2
                (BiosphereFlow co2 "Carbon dioxide, fossil" unitKg syns (Just "124-38-9") Nothing (Just (Compartment Air (Just "unspecified"))))
        , sdbWasteFlows = M.empty
        , sdbUnits = M.singleton unitKg (Unit unitKg "kg" "kg" "")
        }
  where
    syns = M.singleton "en" (S.fromList ["CO2", "carbonic anhydride"])
    activity =
        Activity
            "synonym activity"
            []
            []
            M.empty
            M.empty
            "GLO"
            LocationDeclared
            "kg"
            [ TechnosphereExchange prodA 1.0 unitKg ReferenceProduct Nothing ClaimByProduct "" Nothing Nothing Nothing M.empty noProperties
            , BiosphereExchange co2 0.5 unitKg Emission "" Nothing Nothing
            ]
            M.empty
            M.empty
            Nothing
            Nothing
            Nothing

{- | A single activity exercising the subtlest inversion paths: a coproduct
(outputGroup 2) and waste exchanges in both directions (waIsInput controls
inputGroup 5 vs outputGroup 2). These round-trip paths were previously
untested.
-}
fixtureWasteCoproduct :: SimpleDatabase
fixtureWasteCoproduct =
    SimpleDatabase
        { sdbActivities = M.singleton (actA, prodA) activity
        , sdbTechFlows =
            M.fromList
                [ (prodA, TechnosphereFlow prodA "product A" unitKg M.empty Nothing Nothing)
                , (coprodU, TechnosphereFlow coprodU "co-product" unitKg M.empty Nothing Nothing)
                ]
        , sdbBioFlows = M.empty
        , sdbWasteFlows =
            M.fromList
                [ (wasteInU, WasteFlow wasteInU "incoming waste" unitKg M.empty Nothing Nothing)
                , (wasteOutU, WasteFlow wasteOutU "outgoing waste" unitKg M.empty Nothing Nothing)
                ]
        , sdbUnits = M.singleton unitKg (Unit unitKg "kg" "kg" "")
        }
  where
    coprodU, wasteInU, wasteOutU :: UUID
    coprodU = read "dddddddd-0000-4000-8000-000000000001"
    wasteInU = read "dddddddd-0000-4000-8000-000000000002"
    wasteOutU = read "dddddddd-0000-4000-8000-000000000003"
    activity =
        Activity
            "waste and coproduct activity"
            []
            []
            M.empty
            M.empty
            "GLO"
            LocationDeclared
            "kg"
            [ TechnosphereExchange prodA 1.0 unitKg ReferenceProduct Nothing ClaimByProduct "" Nothing Nothing Nothing M.empty noProperties
            , TechnosphereExchange coprodU 0.4 unitKg Coproduct Nothing ClaimByProduct "" Nothing Nothing Nothing M.empty noProperties
            , WasteExchange wasteInU 0.2 unitKg True Nothing ClaimByProduct "" Nothing Nothing
            , WasteExchange wasteOutU 0.3 unitKg False Nothing ClaimByProduct "" Nothing Nothing
            ]
            M.empty
            M.empty
            (Just (EcoSpoldActivityType 1 "Ordinary transforming activity" Nothing Nothing))
            Nothing
            Nothing

-- | Build a full 'Database' (with matrices) from a 'SimpleDatabase'.
buildDb :: SimpleDatabase -> IO Database
buildDb sdb = do
    res <-
        buildDatabaseWithMatrices (BuildInputs defaultUnitConfig mempty Declared) sdb
    case res of
        Left err -> error $ "matrix build failed: " ++ T.unpack err
        Right db -> pure db

-- | Unwrap the guard-returning writer for a fixture known to be exportable.
writeOrFail :: VolatileMeta -> SimpleDatabase -> [(FilePath, Text)]
writeOrFail meta sdb =
    either (\e -> error ("writeEcoSpold2: " <> T.unpack e)) id (writeEcoSpold2 meta sdb)

{- | Write a 'SimpleDatabase' with the given volatile metadata to a fresh temp
directory and re-load it through the production loader, returning the
round-tripped 'SimpleDatabase'.
-}
roundTripWith :: VolatileMeta -> SimpleDatabase -> IO SimpleDatabase
roundTripWith meta sdb = withSystemTempDirectory "es2-writer" $ \dir -> do
    -- Write UTF-8 bytes explicitly so the unicode round-trip does not depend on
    -- the test runner's locale encoding (the loader always decodes UTF-8).
    forM_ (writeOrFail meta sdb) $ \(fname, doc) ->
        BS.writeFile (dir </> fname) (TE.encodeUtf8 doc)
    res <- loadDatabase defaultUnitConfig dir
    case res of
        Left err -> error $ "reparse failed: " ++ T.unpack err
        Right sdb' -> pure sdb'

-- | Round-trip with the byte-stable default (no volatile metadata).
roundTrip :: SimpleDatabase -> IO SimpleDatabase
roundTrip = roundTripWith noVolatileMeta

{- | Assert the round-tripped database yields the same per-process LCI as the
original, within tolerance. Factored out so several fixtures can be checked.
-}
assertInventoryRoundTrips :: SimpleDatabase -> IO ()
assertInventoryRoundTrips sdb = do
    sdb' <- roundTrip sdb
    db <- buildDb sdb
    db' <- buildDb sdb'
    let pids = [0 .. fromIntegral (V.length (dbProcessIdTable db)) - 1] :: [ProcessId]
    forM_ pids $ \pid -> do
        inv <- either (fail . show) pure =<< computeInventoryMatrix db pid
        -- The round-tripped DB may order activities differently; locate the
        -- matching process by its (actUUID, prodUUID) key.
        let key = dbProcessIdTable db V.! fromIntegral pid
        case V.elemIndex key (dbProcessIdTable db') of
            Nothing -> expectationFailure $ "process key missing after round-trip: " ++ show key
            Just pid' -> do
                inv' <- either (fail . show) pure =<< computeInventoryMatrix db' (fromIntegral pid')
                inventoriesClose inv inv' `shouldBe` True

spec :: Spec
spec = describe "EcoSpold2 writer round-trip" $ do
    -- (a) Idempotence modulo volatile metadata.
    it "is idempotent modulo volatile metadata: write . parse . write == write" $ do
        sdb <- loadFixtureSimple
        let f0 = writeOrFail noVolatileMeta sdb
        sdb' <- roundTrip sdb
        let f1 = writeOrFail noVolatileMeta sdb'
        -- Compare the sorted (filename, document) sequences byte-for-byte.
        sortOn fst f1 `shouldBe` sortOn fst f0

    -- The volatile-metadata paths (creationTimestamp element + generator
    -- comment) are otherwise unexercised, since every other case uses
    -- 'noVolatileMeta'. Pin both and assert they (1) reach the output and
    -- (2) are non-semantic: the parser ignores them, so the round-tripped
    -- activities match the byte-stable default exactly.
    it "emits pinned volatile metadata that the parser then ignores" $ do
        sdb <- loadFixtureSimple
        let meta = VolatileMeta (Just "2020-01-02T03:04:05") (Just "writer-spec <gen> & co")
        let docs = writeOrFail meta sdb
        any (T.isInfixOf "2020-01-02T03:04:05" . snd) docs `shouldBe` True
        any (T.isInfixOf "writer-spec" . snd) docs `shouldBe` True
        pinned <- roundTripWith meta sdb
        plain <- roundTrip sdb
        M.keys (sdbActivities pinned) `shouldMatchList` M.keys (sdbActivities plain)
        forM_ (M.toList (sdbActivities plain)) $ \(key, act) ->
            case M.lookup key (sdbActivities pinned) of
                Nothing -> expectationFailure $ "activity missing under pinned metadata: " ++ show key
                Just act' ->
                    sort (map exchangeFingerprint (exchanges act'))
                        `shouldBe` sort (map exchangeFingerprint (exchanges act))

    -- Regression: two exchanges sharing a sort key (same kind + flow) must both
    -- survive write→parse. A Map-based sort would silently drop one,
    -- undercounting the inventory; we assert the amounts survive as a multiset.
    it "keeps duplicate exchanges of the same flow (no silent dedup)" $ do
        sdb' <- roundTrip fixtureDupBio
        case M.lookup (actA, prodA) (sdbActivities sdb') of
            Nothing -> expectationFailure "duplicate-exchange activity missing after round-trip"
            Just act ->
                sort [exchangeAmount ex | ex <- exchanges act, isBiosphereExchange ex]
                    `shouldBe` [0.3, 0.5]

    -- Regression: the subtlest inversion paths. renderWaste maps waIsInput to
    -- inputGroup 5 / outputGroup 2, and a coproduct goes to outputGroup 2 too
    -- (the Waste classification, not the group, distinguishes them), both must
    -- survive write→parse rather than flip direction or role.
    it "round-trips waste direction (in/out) and a coproduct" $ do
        sdb' <- roundTrip fixtureWasteCoproduct
        case M.lookup (actA, prodA) (sdbActivities sdb') of
            Nothing -> expectationFailure "waste/coproduct activity missing after round-trip"
            Just act -> do
                sort [waIsInput ex | ex@WasteExchange{} <- exchanges act] `shouldBe` [False, True]
                [techRole ex | ex@TechnosphereExchange{techRole = Coproduct} <- exchanges act]
                    `shouldBe` [Coproduct]

    -- Export-boundary guards: data EcoSpold2 cannot faithfully re-encode must be
    -- rejected loudly by 'checkEcoSpold2Exportable' rather than silently
    -- corrupted (a non-finite amount clamps to 0.0; a ReferenceInput re-parses
    -- as a plain Input, losing the treatment activity's reference designation).
    describe "rejects un-encodable data at the export boundary" $ do
        it "rejects a non-finite exchange amount (Infinity clamps to 0.0)" $
            checkEcoSpold2Exportable
                (fixtureWithExchange (BiosphereExchange co2 (1 / 0) unitKg Emission "" Nothing Nothing))
                `shouldSatisfy` isLeft

        it "rejects a reference input (no EcoSpold2 encoding)" $
            checkEcoSpold2Exportable
                (fixtureWithExchange (TechnosphereExchange co2 2.0 unitKg ReferenceInput Nothing ClaimByProduct "" Nothing Nothing Nothing M.empty noProperties))
                `shouldSatisfy` isLeft

        it "rejects an exchange whose unit is absent from the registry" $
            -- unitMJ is not in the single-unit registry, so the writer would emit
            -- no <unitName> and the parser would read back UNKNOWN_UNIT.
            checkEcoSpold2Exportable
                (fixtureWithExchange (BiosphereExchange co2 1.0 unitMJ Emission "" Nothing Nothing))
                `shouldSatisfy` isLeft

        it "rejects an exchange whose flow is absent from the registry" $
            -- `land` is not in the single-flow biosphere registry, so the writer
            -- would emit a nameless, compartment-less exchange (name degrading to
            -- the bare UUID), symmetric to the missing-unit downgrade above.
            checkEcoSpold2Exportable
                (fixtureWithExchange (BiosphereExchange land 1.0 unitKg Emission "" Nothing Nothing))
                `shouldSatisfy` isLeft

        it "accepts a subnormal amount, which round-trips through the correctly-rounded reader" $
            -- 5e-324's fixed-point form re-parses exactly through Amount.readAmount
            -- (Data.Text.Read.double used to lose it to 0), so it is faithfully
            -- representable and the guard must not reject it.
            checkEcoSpold2Exportable
                (fixtureWithExchange (BiosphereExchange co2 5e-324 unitKg Emission "" Nothing Nothing))
                `shouldSatisfy` isRight

    -- (b) Semantic round-trip: parse(write(D)) ≅ D, order-insensitive.
    describe "semantic round-trip (structural equality)" $ do
        it "preserves the activity set keyed by (actUUID, prodUUID)" $ do
            sdb <- loadFixtureSimple
            sdb' <- roundTrip sdb
            M.keys (sdbActivities sdb') `shouldMatchList` M.keys (sdbActivities sdb)

        it "preserves each activity's name, location and exchange multiset" $ do
            sdb <- loadFixtureSimple
            sdb' <- roundTrip sdb
            forM_ (M.toList (sdbActivities sdb)) $ \(key, act) ->
                case M.lookup key (sdbActivities sdb') of
                    Nothing -> expectationFailure $ "missing activity after round-trip: " ++ show key
                    Just act' -> do
                        activityName act' `shouldBe` activityName act
                        activityLocation act' `shouldBe` activityLocation act
                        activityClassification act' `shouldBe` activityClassification act
                        activityNativeType act' `shouldBe` activityNativeType act
                        sort (map exchangeFingerprint (exchanges act'))
                            `shouldBe` sort (map exchangeFingerprint (exchanges act))

        it "preserves the biosphere flow registry (id, name, compartment)" $ do
            sdb <- loadFixtureSimple
            sdb' <- roundTrip sdb
            map bioFingerprint (M.elems (sdbBioFlows sdb'))
                `shouldMatchList` map bioFingerprint (M.elems (sdbBioFlows sdb))

        it "preserves the technosphere flow registry (id, name)" $ do
            sdb <- loadFixtureSimple
            sdb' <- roundTrip sdb
            map (\f -> (tfId f, tfName f)) (M.elems (sdbTechFlows sdb'))
                `shouldMatchList` map (\f -> (tfId f, tfName f)) (M.elems (sdbTechFlows sdb))

        it "preserves biosphere flow synonyms across the round-trip" $ do
            -- The writer emits <synonym> lines and the parser reads them back
            -- (collapsed under "en"); without a fixture carrying synonyms this
            -- path was unexercised end to end.
            sdb' <- roundTrip fixtureWithBioSynonyms
            S.unions (concatMap (M.elems . bfSynonyms) (M.elems (sdbBioFlows sdb')))
                `shouldBe` S.fromList ["CO2", "carbonic anhydride"]

    -- (c) Score-equivalence: same inventory for every activity within tolerance.
    it "yields the same LCIA inventory as the original (within tolerance)" $
        loadFixtureSimple >>= assertInventoryRoundTrips

    -- Duplicate-flow no-dedup, now at the inventory level: the two CO2 emissions
    -- must sum (0.8) on both sides, not collapse to one line.
    it "preserves duplicate-flow inventory mass across the round-trip" $
        assertInventoryRoundTrips fixtureDupBio

-- ----------------------------------------------------------------------------
-- Comparison helpers (order-insensitive, tolerance-aware)
-- ----------------------------------------------------------------------------

{- | Structural fingerprint of an exchange, stable under reordering. Carries the
per-exchange comment so the semantic round-trip actually pins it (the fixture
gives one input a comment).
-}
exchangeFingerprint :: Exchange -> (Int, Text, Bool, Bool, Integer, Maybe Text)
exchangeFingerprint ex =
    ( kindRank ex
    , UUID.toText (exchangeFlowId ex)
    , exchangeIsInput ex
    , exchangeIsReference ex
    , roundAmount (exchangeAmount ex)
    , exchangeComment ex
    )
  where
    kindRank TechnosphereExchange{} = 0
    kindRank WasteExchange{} = 1
    kindRank BiosphereExchange{} = 2

-- | Biosphere flow fingerprint: identity, CAS, plus compartment medium/sub.
bioFingerprint :: BiosphereFlow -> (Text, Text, Maybe Text, Text, Maybe Text)
bioFingerprint f =
    ( UUID.toText (bfId f)
    , bfName f
    , bfCAS f
    , maybe "" (mediumText . compartmentName) (bfCompartment f)
    , bfCompartment f >>= compartmentSub
    )

-- | Quantise an amount so float jitter doesn't break equality (9 sig decimals).
roundAmount :: Double -> Integer
roundAmount d = round (d * 1e9)

{- | Two inventories agree within relative+absolute tolerance on every flow
present in either map. Missing flows are treated as zero.
-}
inventoriesClose :: M.Map UUID.UUID Double -> M.Map UUID.UUID Double -> Bool
inventoriesClose a b =
    let keys = S.toList (S.fromList (M.keys a) `S.union` S.fromList (M.keys b))
        close k =
            let x = M.findWithDefault 0 k a
                y = M.findWithDefault 0 k b
             in abs (x - y) <= 1e-9 + 1e-6 * max (abs x) (abs y)
     in all close keys
