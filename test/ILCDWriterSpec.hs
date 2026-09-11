{-# LANGUAGE OverloadedStrings #-}

{- | Round-trip contract for "ILCD.Writer", the inverse of "ILCD.Parser".

The original database @D@ is obtained by parsing the @SAMPLE.ilcd@ fixture with
'parseILCDDirectory'. That guarantees @D@ already lives in the parser's
canonical internal form (resolved units, classified flows, linked exchanges),
so the write→parse cycle has a fair fixed point to land on.

Three properties are pinned, exactly as for the SimaPro/Brightway writers:

  (a) idempotence modulo volatile metadata: @write(D)@ then
      @write(parse(write(D)))@ produce byte-identical output. The only volatile
      fields (export timestamp, generator string) are /omitted/ by
      'defaultWriteOptions', so this holds without any normalization;

  (b) semantic round-trip: @parse(write(D))@ is structurally equal to @D@,
      order-insensitively, over activities, exchanges and the flow catalog;

  (c) score-equivalence: @parse(write(D))@ yields the same biosphere inventory
      (the LCIA-precursor vector; an LCIA score is linear in it) as @D@ within
      tolerance, via the engine's 'computeInventoryMatrix'.
-}
module ILCDWriterSpec (spec) where

import Codec.Archive.Zip (ZipOption (OptDestination), extractFilesFromArchive, toArchive)
import qualified Data.ByteString.Lazy as BL
import Data.Either (isLeft, isRight)
import Data.List (isPrefixOf, sort)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import Database (buildDatabaseWithMatrices)
import ILCD.Parser (parseILCDDirectory)
import ILCD.Writer (
    WriteOptions,
    checkILCDExportable,
    defaultWriteOptions,
    escapeXml,
    escapeXmlAttr,
    formatDouble,
    ilcdFiles,
    ilcdProcessUUID,
    sharedActivityUUIDs,
    splitWarnings,
    writeILCDArchive,
    writeILCDDatabase,
 )
import Matrix (computeInventoryMatrix)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Types
import UnitConversion (defaultUnitConfig)

fixtureDir :: FilePath
fixtureDir = "test-data/SAMPLE.ilcd"

-- | Parse the fixture into a 'SimpleDatabase' (fails the test on Left).
loadFixture :: IO SimpleDatabase
loadFixture = do
    r <- parseILCDDirectory defaultUnitConfig Declared fixtureDir
    case r of
        Left err -> error ("fixture parse failed: " <> T.unpack err)
        Right db -> pure db

{- | Write a 'SimpleDatabase' to a fresh temp ILCD tree and parse it back.
A fresh directory per call avoids the parser's on-disk flow cache leaking
between round-trips.
-}
roundTrip :: SimpleDatabase -> IO SimpleDatabase
roundTrip db = withSystemTempDirectory "ilcd-writer-spec" $ \dir -> do
    writeILCDDatabase defaultWriteOptions dir db
        >>= either (\e -> error ("ILCD write failed: " <> T.unpack e)) pure
    r <- parseILCDDirectory defaultUnitConfig Declared dir
    case r of
        Left err -> error ("round-trip parse failed: " <> T.unpack err)
        Right db' -> pure db'

-- | Unwrap the guard-returning archive writer for a fixture known to be exportable.
archiveOrFail :: WriteOptions -> SimpleDatabase -> BL.ByteString
archiveOrFail opts db =
    either (\e -> error ("writeILCDArchive: " <> T.unpack e)) id (writeILCDArchive opts db)

{- | Round-trip through the production zip export path: serialize with
'writeILCDArchive', extract the archive to a fresh temp tree and parse it back.
This is the byte stream 'Database.Export' ships to clients.
-}
archiveRoundTrip :: SimpleDatabase -> IO SimpleDatabase
archiveRoundTrip db = withSystemTempDirectory "ilcd-archive-spec" $ \dir -> do
    extractFilesFromArchive [OptDestination dir] (toArchive (archiveOrFail defaultWriteOptions db))
    r <- parseILCDDirectory defaultUnitConfig Declared dir
    case r of
        Left err -> error ("archive round-trip parse failed: " <> T.unpack err)
        Right db' -> pure db'

-- ---------------------------------------------------------------------------
-- Structural comparison (order-insensitive)
-- ---------------------------------------------------------------------------

data ActivityShape = ActivityShape
    { asName :: Text
    , asLocation :: Text
    , asClass :: [(Text, Text)]
    , asNative :: Maybe Text
    , asExchanges :: [ExchangeShape]
    }
    deriving (Eq, Ord, Show)

data ExchangeShape = ExchangeShape
    { esKind :: Text
    , esFlow :: Text
    , esUnit :: Text
    , esAmount :: Double
    , esInput :: Bool
    , esRef :: Bool
    , esLocation :: Text
    , esComment :: Maybe Text
    }
    deriving (Eq, Ord, Show)

exchangeShape :: SimpleDatabase -> Exchange -> ExchangeShape
exchangeShape db ex =
    let unitNm uid = maybe "" unitName (M.lookup uid (sdbUnits db))
        roundAmt v = fromIntegral (round (v * 1e9) :: Integer) / 1e9 :: Double
     in case ex of
            TechnosphereExchange{techFlowId = fid, techAmount = amt, techUnitId = uid, techComment = c} ->
                ExchangeShape
                    "tech"
                    (maybe "?" tfName (M.lookup fid (sdbTechFlows db)))
                    (unitNm uid)
                    (roundAmt amt)
                    (exchangeIsInput ex)
                    (exchangeIsReference ex)
                    (exchangeLocation ex)
                    c
            BiosphereExchange{bioFlowId = fid, bioAmount = amt, bioUnitId = uid, bioComment = c} ->
                ExchangeShape
                    "bio"
                    (maybe "?" bfName (M.lookup fid (sdbBioFlows db)))
                    (unitNm uid)
                    (roundAmt amt)
                    (exchangeIsInput ex)
                    (exchangeIsReference ex)
                    (exchangeLocation ex)
                    c
            WasteExchange{waFlowId = fid, waAmount = amt, waUnitId = uid, waComment = c} ->
                ExchangeShape
                    "waste"
                    (maybe "?" wfName (M.lookup fid (sdbWasteFlows db)))
                    (unitNm uid)
                    (roundAmt amt)
                    (exchangeIsInput ex)
                    (exchangeIsReference ex)
                    (exchangeLocation ex)
                    c

activityShape :: SimpleDatabase -> Activity -> ActivityShape
activityShape db a =
    ActivityShape
        { asName = activityName a
        , asLocation = activityLocation a
        , asClass = M.toAscList (activityClassification a)
        , asNative = nativeLabel (activityNativeType a)
        , asExchanges = sort (map (exchangeShape db) (exchanges a))
        }

nativeLabel :: Maybe NativeActivityType -> Maybe Text
nativeLabel nt = case nt of
    Just (ILCDProcessType l) -> Just l
    Just (SimaProProcessType l) -> Just l
    Just (EcoSpoldActivityType{}) -> Nothing
    Nothing -> Nothing

activityShapes :: SimpleDatabase -> S.Set ActivityShape
activityShapes db = S.fromList (map (activityShape db) (M.elems (sdbActivities db)))

-- | Order-insensitive view of the flow catalog: (kind, name, unit, cas).
data FlowShape = FlowShape Text Text Text (Maybe Text)
    deriving (Eq, Ord, Show)

flowShapes :: SimpleDatabase -> S.Set FlowShape
flowShapes db =
    S.fromList $
        [FlowShape "tech" (tfName f) (unitNm (tfUnitId f)) (tfCAS f) | f <- M.elems (sdbTechFlows db)]
            ++ [FlowShape "bio" (bfName f) (unitNm (bfUnitId f)) (bfCAS f) | f <- M.elems (sdbBioFlows db)]
            ++ [FlowShape "waste" (wfName f) (unitNm (wfUnitId f)) (wfCAS f) | f <- M.elems (sdbWasteFlows db)]
  where
    unitNm uid = maybe "" unitName (M.lookup uid (sdbUnits db))

-- ---------------------------------------------------------------------------
-- Inventory (score-equivalence) helper
-- ---------------------------------------------------------------------------

{- | Build a Database from a 'SimpleDatabase' and compute the inventory of the
named activity, keyed by biosphere flow name.
-}
inventoryByName :: SimpleDatabase -> Text -> IO (M.Map Text Double)
inventoryByName db target = do
    built <-
        buildDatabaseWithMatrices (BuildInputs defaultUnitConfig mempty Declared) db
    case built of
        Left err -> expectationFailure (T.unpack err) >> pure M.empty
        Right d -> do
            let pids =
                    [ pid
                    | (pid, (actU, _)) <- zip [0 ..] (V.toList (dbProcessIdTable d))
                    , Just act <- [activityAtUUID d actU]
                    , activityName act == target
                    ]
            case pids of
                (pid : _) -> do
                    inv <- either (fail . show) pure =<< computeInventoryMatrix d pid
                    pure (M.mapKeys (flowName d) inv)
                [] -> expectationFailure ("no activity named " <> T.unpack target) >> pure M.empty
  where
    flowName d uid = maybe (T.pack (show uid)) bfName (M.lookup uid (dbBioFlows d))
    activityAtUUID d actU =
        case [i | (i, (a, _)) <- zip [0 ..] (V.toList (dbProcessIdTable d)), a == actU] of
            (i : _) -> Just (dbActivities d V.! i)
            [] -> Nothing

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "ILCD.Writer round-trip" $ do
    it "(pure) formatDouble emits fixed-point (never scientific) doubles" $ do
        formatDouble 1.0 `shouldBe` "1.0"
        formatDouble 2.5 `shouldBe` "2.5"
        formatDouble (-3.0) `shouldBe` "-3.0"
        formatDouble (-0.0) `shouldBe` "0.0"
        -- The fix: small magnitudes stay fixed-point so they re-read losslessly.
        T.isInfixOf "e" (formatDouble 3.3e-20) `shouldBe` False

    it "(pure) escapeXml escapes the predefined entities" $ do
        escapeXml "a & b < c > d" `shouldBe` "a &amp; b &lt; c &gt; d"
        escapeXml "plain" `shouldBe` "plain"

    it "(pure) escapeXmlAttr encodes newlines so they survive in an attribute" $
        -- A raw \n/\r in an attribute value is normalised to a space by XML
        -- parsers; encode it as a numeric character reference instead.
        escapeXmlAttr "line1\nline2\rx" `shouldBe` "line1&#10;line2&#13;x"

    it "round-trips a small-exponent amount without scientific-notation loss" $ do
        -- show 3.3e-20 emits scientific notation the parser re-reads lossily;
        -- showFFloatTrim keeps it value-identical end-to-end through the writer.
        let sdb =
                oneActivityDb
                    (M.singleton fEmitU fEmission)
                    [ TechnosphereExchange fProdU 1.0 fUnitU ReferenceProduct Nothing ClaimByProduct "" Nothing Nothing Nothing M.empty noProperties
                    , BiosphereExchange fEmitU 3.3e-20 fUnitU Emission "" Nothing Nothing
                    ]
        db' <- roundTrip sdb
        [ bioAmount ex
          | a <- M.elems (sdbActivities db')
          , ex@BiosphereExchange{} <- exchanges a
          ]
            `shouldSatisfy` elem 3.3e-20

    it "emits one process file per activity" $ do
        db <- loadFixture
        length (processPaths (ilcdFiles defaultWriteOptions db)) `shouldBe` M.size (sdbActivities db)

    describe "checkILCDExportable" $ do
        it "accepts a single-output database" $ do
            db <- loadFixture
            checkILCDExportable db `shouldBe` Right ()

        it "accepts a multi-output activity, now that each product is its own process" $
            checkILCDExportable multiOutputDb `shouldBe` Right ()

    describe "ilcdProcessUUID (process dataset identity)" $ do
        it "leaves an unshared activity UUID alone" $ do
            db <- loadFixture
            sharedActivityUUIDs db `shouldBe` S.empty
            ilcdProcessUUID S.empty (moActU, moProdA) `shouldBe` moActU

        it "sees the shared activity UUID of a multi-output database" $
            sharedActivityUUIDs multiOutputDb `shouldBe` S.singleton moActU

        it "derives a distinct, deterministic UUID per product of a shared activity" $ do
            let shared = sharedActivityUUIDs multiOutputDb
                dsA = ilcdProcessUUID shared (moActU, moProdA)
                dsB = ilcdProcessUUID shared (moActU, moProdB)
            dsA `shouldNotBe` moActU
            dsB `shouldNotBe` moActU
            dsA `shouldNotBe` dsB
            ilcdProcessUUID shared (moActU, moProdA) `shouldBe` dsA

    describe "splitWarnings (multi-output approximation is reported)" $ do
        it "is empty when every activity UUID is unique" $ do
            db <- loadFixture
            splitWarnings db `shouldBe` []

        it "names each split activity and its product count" $
            case splitWarnings multiOutputDb of
                [w] -> do
                    w `shouldSatisfy` T.isInfixOf (UUID.toText moActU)
                    w `shouldSatisfy` T.isInfixOf "2 products"
                ws -> expectationFailure ("expected exactly one warning, got " ++ show ws)

    describe "multi-output export" $ do
        it "writes one process file per product, not one per activity UUID" $ do
            let procFiles = processPaths (ilcdFiles defaultWriteOptions multiOutputDb)
            length procFiles `shouldBe` 2
            S.size (S.fromList procFiles) `shouldBe` 2

        it "round-trips both products, structurally" $ do
            db' <- roundTrip multiOutputDb
            M.size (sdbActivities db') `shouldBe` 2
            activityShapes db' `shouldBe` activityShapes multiOutputDb

        it "is idempotent: the derived dataset UUID is a fixed point" $ do
            -- The re-imported activities are keyed by their derived UUIDs, which
            -- no longer collide, so the second export mints nothing new.
            let f0 = ilcdFiles defaultWriteOptions multiOutputDb
            db' <- roundTrip multiOutputDb
            sharedActivityUUIDs db' `shouldBe` S.empty
            ilcdFiles defaultWriteOptions db' `shouldBe` f0

    it "produces a parseable ILCD tree with the same activity count" $ do
        db <- loadFixture
        db' <- roundTrip db
        M.size (sdbActivities db') `shouldBe` M.size (sdbActivities db)

    it "(a) is idempotent modulo volatile metadata" $ do
        db <- loadFixture
        let f0 = ilcdFiles defaultWriteOptions db
        db' <- roundTrip db
        let f1 = ilcdFiles defaultWriteOptions db'
        f1 `shouldBe` f0

    it "(b) semantic round-trip: activities are structurally equal" $ do
        db <- loadFixture
        db' <- roundTrip db
        activityShapes db' `shouldBe` activityShapes db

    it "(b) semantic round-trip: the flow catalog is structurally equal" $ do
        db <- loadFixture
        db' <- roundTrip db
        flowShapes db' `shouldBe` flowShapes db

    describe "writeILCDArchive (production export path)" $ do
        it "is byte-deterministic across runs (epoch-0 mtimes)" $ do
            db <- loadFixture
            let bytes = archiveOrFail defaultWriteOptions db
            archiveOrFail defaultWriteOptions db `shouldBe` bytes
            BL.null bytes `shouldBe` False

        it "extracts and reparses to a structurally equal database" $ do
            db <- loadFixture
            db' <- archiveRoundTrip db
            activityShapes db' `shouldBe` activityShapes db
            flowShapes db' `shouldBe` flowShapes db

    it "(c) score-equivalence: same inventory for the sample activity within tolerance" $ do
        db <- loadFixture
        db' <- roundTrip db
        invOrig <- inventoryByName db "Coal extraction"
        invRound <- inventoryByName db' "Coal extraction"
        M.keysSet invRound `shouldBe` M.keysSet invOrig
        let diffs =
                [ abs (a - b)
                | (k, a) <- M.toList invOrig
                , let b = M.findWithDefault 0 k invRound
                ]
        all (< 1e-9) diffs `shouldBe` True

    describe "feature round-trip and export guards" $ do
        it "round-trips a bio subcompartment, a natural-resource flow and a non-zero reference index" $ do
            db' <- roundTrip richDb
            -- Emission medium + sub, resource medium, and the special-char
            -- name/comment all survive via the shared structural views.
            activityShapes db' `shouldBe` activityShapes richDb
            flowShapes db' `shouldBe` flowShapes richDb
            -- Compartment sub is not part of FlowShape, so assert it directly.
            map bfCompartment (M.elems (sdbBioFlows db'))
                `shouldMatchList` map bfCompartment (M.elems (sdbBioFlows richDb))

        it "round-trips a name and comment with & < > \", entity literals and unicode verbatim" $ do
            db' <- roundTrip richDb
            let names = map activityName (M.elems (sdbActivities db'))
            names `shouldBe` [specialText]
            let comments =
                    [ c
                    | act <- M.elems (sdbActivities db')
                    , ex <- exchanges act
                    , Just c <- [exchangeComment ex]
                    ]
            comments `shouldContain` [specialText]

        it "round-trips a non-empty per-exchange location verbatim" $ do
            db' <- roundTrip richDb
            let locs =
                    [ exchangeLocation ex
                    | act <- M.elems (sdbActivities db')
                    , ex <- exchanges act
                    , not (T.null (exchangeLocation ex))
                    ]
            locs `shouldBe` ["RER"]

        it "places the reference at a non-zero index after round-trip" $ do
            db' <- roundTrip richDb
            let refIdxs =
                    [ i
                    | act <- M.elems (sdbActivities db')
                    , (i, ex) <- zip [0 :: Int ..] (exchanges act)
                    , exchangeIsReference ex
                    ]
            refIdxs `shouldBe` [2]

        it "rejects a medium the ILCD classification cannot name" $
            checkILCDExportable (bioMediumDb Waste) `shouldSatisfy` isLeft

        it "accepts a canonical biosphere medium" $
            checkILCDExportable (bioMediumDb Air) `shouldBe` Right ()

        it "round-trips a natural resource under its own medium" $ do
            -- Every format spells this medium its own way and each writer emits
            -- the spelling its own reader inverts, so the medium that comes back
            -- is the medium that went out.
            checkILCDExportable (bioMediumDb NaturalResource) `shouldBe` Right ()
            db' <- roundTrip (bioMediumDb NaturalResource)
            map (fmap compartmentName . bfCompartment) (M.elems (sdbBioFlows db'))
                `shouldBe` [Just NaturalResource]

        it "reports every violation category in one message, not just the first" $ do
            -- A flow with both a medium the classification cannot name and a
            -- non-finite amount trips two independent checks; the collected
            -- report must carry both.
            let db =
                    oneActivityDb
                        (M.singleton fEmitU (fEmission{bfCompartment = Just (Compartment Waste Nothing)}))
                        [BiosphereExchange fEmitU (1 / 0) fUnitU Emission "" Nothing Nothing, refProductEx]
            case checkILCDExportable db of
                Left msg -> do
                    msg `shouldSatisfy` T.isInfixOf "waste"
                    msg `shouldSatisfy` T.isInfixOf "non-finite"
                Right () -> expectationFailure "expected violations"

        it "preserves the no-reference state on a reference-less activity" $ do
            checkILCDExportable noRefDb `shouldBe` Right ()
            db' <- roundTrip noRefDb
            let refs =
                    [ ()
                    | act <- M.elems (sdbActivities db')
                    , ex <- exchanges act
                    , exchangeIsReference ex
                    ]
            refs `shouldBe` []

        it "rejects a non-finite amount rather than silently clamping it to zero" $ do
            -- formatDouble renders the non-parseable form so a bad re-import
            -- fails loudly; the export guard rejects it before that can happen.
            formatDouble (0 / 0) `shouldBe` "NaN"
            formatDouble (1 / 0) `shouldBe` "Infinity"
            checkILCDExportable (nonFiniteDb (1 / 0)) `shouldSatisfy` isLeft
            checkILCDExportable (nonFiniteDb (0 / 0)) `shouldSatisfy` isLeft

        it "accepts a subnormal amount, which round-trips through the correctly-rounded reader" $
            -- 5e-324 (smallest positive subnormal) re-parses exactly through
            -- Amount.readAmount (Data.Text.Read.double used to lose it to 0), so it
            -- is faithfully representable; the guard must not reject it.
            checkILCDExportable (nonFiniteDb 5.0e-324) `shouldSatisfy` isRight

isPrefixOfFp :: String -> FilePath -> Bool
isPrefixOfFp = isPrefixOf

-- | The @processes/@ entries of an 'ilcdFiles' listing, one per exported process.
processPaths :: [(FilePath, a)] -> [FilePath]
processPaths files = [p | (p, _) <- files, "processes/" `isPrefixOfFp` p]

-- ---------------------------------------------------------------------------
-- Multi-output fixture (two products sharing one activity UUID)
-- ---------------------------------------------------------------------------

-- | UUIDs of 'multiOutputDb', named so the dataset-UUID tests can address them.
moActU, moProdA, moProdB, moUnitU :: UUID
moActU = read "aaaaaaaa-0000-4000-8000-000000000001"
moProdA = read "aaaaaaaa-0000-4000-8000-0000000000a1"
moProdB = read "aaaaaaaa-0000-4000-8000-0000000000b2"
moUnitU = read "11111111-0000-4000-8000-000000000001"

{- | A database where one activity UUID exposes two reference products, the
shape an ES2/SimaPro multi-output activity takes internally, and the shape a
truncated SimaPro process name gives two unrelated blocks. Each product exports
as its own ILCD process dataset, under the UUID 'ilcdProcessUUID' derives for
its @(activity, product)@ pair.
-}
multiOutputDb :: SimpleDatabase
multiOutputDb =
    SimpleDatabase
        { sdbActivities =
            M.fromList
                [ ((moActU, moProdA), prodAct "co-product A" moProdA)
                , ((moActU, moProdB), prodAct "co-product B" moProdB)
                ]
        , sdbTechFlows =
            M.fromList
                [ (moProdA, techFlow moProdA "product A")
                , (moProdB, techFlow moProdB "product B")
                ]
        , sdbBioFlows = M.empty
        , sdbWasteFlows = M.empty
        , sdbUnits = M.singleton moUnitU (Unit moUnitU "kg" "kg" "")
        }
  where
    techFlow :: UUID -> Text -> TechnosphereFlow
    techFlow fid nm = TechnosphereFlow fid nm moUnitU M.empty Nothing Nothing
    prodAct :: Text -> UUID -> Activity
    prodAct nm prod =
        Activity
            nm
            []
            []
            M.empty
            M.empty
            "GLO"
            LocationDeclared
            "kg"
            [TechnosphereExchange prod 1.0 moUnitU ReferenceProduct Nothing ClaimByProduct "" Nothing Nothing Nothing M.empty noProperties]
            M.empty
            M.empty
            Nothing
            Nothing
            Nothing

-- ---------------------------------------------------------------------------
-- Feature fixtures (single-output, exercising the recent ILCD writer fixes)
-- ---------------------------------------------------------------------------

{- | A name/comment carrying every XML-significant char plus non-ASCII text.
The literal entity references @&lt;@ and @&amp;@ are deliberate: the writer
escapes their @&@ to @&amp;@ (so @&lt;@ → @&amp;lt;@), and a decoder that resolves
@&amp;@ before @&lt;@ would corrupt them back to @<@/@&@. They guard that
asymmetry.
-}
specialText :: Text
specialText = "Name & <tag> \"q\" &lt; &amp; café ☕"

{- | UUIDs shared by the feature fixtures. Distinct constructors keep each flow
addressable; the reference product is deliberately the /third/ exchange so a
non-zero reference index has to survive the round-trip.
-}
fActU, fProdU, fEmitU, fResU, fUnitU :: UUID
fActU = read "bbbbbbbb-0000-4000-8000-000000000001"
fProdU = read "bbbbbbbb-0000-4000-8000-0000000000c1"
fEmitU = read "bbbbbbbb-0000-4000-8000-0000000000e1"
fResU = read "bbbbbbbb-0000-4000-8000-0000000000d1"
fUnitU = read "22222222-0000-4000-8000-000000000001"

fUnits :: M.Map UUID Unit
fUnits = M.singleton fUnitU (Unit fUnitU "kg" "kg" "")

fProduct :: TechnosphereFlow
fProduct = TechnosphereFlow fProdU "product P" fUnitU M.empty Nothing Nothing

fEmission :: BiosphereFlow
fEmission =
    BiosphereFlow fEmitU "Carbon dioxide" fUnitU M.empty Nothing Nothing $
        Just (Compartment Air (Just "high. pop."))

fResource :: BiosphereFlow
fResource =
    BiosphereFlow fResU "Iron ore" fUnitU M.empty Nothing Nothing $
        Just (Compartment NaturalResource (Just "in ground"))

{- | A one-activity database wrapping the given biosphere catalog and exchanges,
sharing one technosphere product flow and one unit. The activity name carries
'specialText' so XML-significant chars are exercised on every round-trip.
-}
oneActivityDb :: M.Map UUID BiosphereFlow -> [Exchange] -> SimpleDatabase
oneActivityDb bios exs =
    SimpleDatabase
        { sdbActivities = M.singleton (fActU, fProdU) act
        , sdbTechFlows = M.singleton fProdU fProduct
        , sdbBioFlows = bios
        , sdbWasteFlows = M.empty
        , sdbUnits = fUnits
        }
  where
    act =
        Activity
            { activityName = specialText
            , activityDescription = []
            , activityDocumentation = []
            , activitySynonyms = M.empty
            , activityClassification = M.empty
            , activityLocation = "GLO"
            , activityLocationSource = LocationDeclared
            , activityUnit = "kg"
            , exchanges = exs
            , activityParams = M.empty
            , activityParamExprs = M.empty
            , activityNativeType = Nothing
            , activityNativeId = Nothing
            , activityFormulaCheck = Nothing
            }

refProductEx :: Exchange
refProductEx = TechnosphereExchange fProdU 1.0 fUnitU ReferenceProduct Nothing ClaimByProduct "" Nothing Nothing Nothing M.empty noProperties

{- | The reference product sits at index 2, after an air emission with a
subcompartment and a natural-resource input. One exchange carries a non-empty
location and a comment of 'specialText', so location, names and comments are all
exercised together.
-}
richDb :: SimpleDatabase
richDb =
    oneActivityDb
        (M.fromList [(fEmitU, fEmission), (fResU, fResource)])
        [ BiosphereExchange fEmitU 2.0 fUnitU Emission "RER" (Just specialText) Nothing
        , BiosphereExchange fResU 3.0 fUnitU Resource "" Nothing Nothing
        , refProductEx
        ]

{- | One activity with a single biosphere emission under the given medium.
A medium the ILCD classification has no place for ('Waste', 'Economic',
'InventoryIndicator') is what 'checkILCDExportable' must reject; the four the
classification names pass.
-}
bioMediumDb :: Medium -> SimpleDatabase
bioMediumDb medium =
    oneActivityDb
        (M.singleton fEmitU (fEmission{bfCompartment = Just (Compartment medium Nothing)}))
        [BiosphereExchange fEmitU 1.0 fUnitU Emission "" Nothing Nothing, refProductEx]

-- | An activity with no exchange marked as reference (preserve-no-reference path).
noRefDb :: SimpleDatabase
noRefDb =
    oneActivityDb
        (M.singleton fEmitU fEmission)
        [BiosphereExchange fEmitU 1.0 fUnitU Emission "" Nothing Nothing]

{- | One activity whose single biosphere amount is the given value. Used with a
non-finite or subnormal amount that 'checkILCDExportable' must reject because it
does not re-parse to itself through 'formatDouble'.
-}
nonFiniteDb :: Double -> SimpleDatabase
nonFiniteDb amt =
    oneActivityDb
        (M.singleton fEmitU fEmission)
        [BiosphereExchange fEmitU amt fUnitU Emission "" Nothing Nothing]
