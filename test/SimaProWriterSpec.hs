{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- | Round-trip contract for "SimaPro.Writer", the inverse of
"SimaPro.Parser".

The original database @D@ is built by parsing a small in-line SimaPro CSV with
'parseSimaProCSV'. That guarantees @D@ already lives in the parser's canonical
internal form (canonical units, resolved amounts, generated UUIDs), so the
write→parse cycle has a fair fixed point to land on.

Three properties are pinned:

  (a) idempotence modulo volatile metadata: @write(D)@ then
      @write(parse(write(D)))@ produce byte-identical output (the version
      banner is the only volatile field and it is pinned by 'WriterConfig');

  (b) semantic round-trip: @parse(write(D))@ is structurally equal to @D@,
      order-insensitively, on activities, flows and units;

  (c) score-equivalence: @parse(write(D))@ yields the same biosphere
      inventory (the LCIA-precursor vector) as @D@ within tolerance, via the
      engine's 'computeInventoryMatrix'.
-}
module SimaProWriterSpec (spec) where

import qualified Data.ByteString as BS
import Data.Either (isLeft, isRight)
import Data.List (sort)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import Data.Word (Word32)
import Database (buildDatabaseWithMatrices)
import Database.Loader (defaultLoadOptions, loadSimaProCSV)
import Matrix (computeInventoryMatrix)
import SimaPro.Parser (parseSimaProCSV)
import SimaPro.Writer (
    checkSimaProExportable,
    defaultWriterConfig,
    escapeField,
    formatAmount,
    serializeSimaProCSV,
 )
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.Hspec
import Types
import UnitConversion (UnitConfig, defaultUnitConfig)

-- ---------------------------------------------------------------------------
-- Fixture: a small SimaPro CSV exercising every section the writer emits
-- ---------------------------------------------------------------------------

fixtureCSV :: BS.ByteString
fixtureCSV =
    BS.intercalate
        "\r\n"
        [ "{SimaPro 9.6.0.1}"
        , "{CSV separator: Semicolon}"
        , "{Decimal separator: .}"
        , ""
        , "Process"
        , ""
        , "Category type"
        , "material"
        , ""
        , "Process name"
        , "Butter production"
        , ""
        , "Type"
        , "Unit process"
        , ""
        , "Geography"
        , "FR"
        , ""
        , "Comment"
        , -- Multi-line free text the way SimaPro writes it: one physical line,
          -- \x7f standing in for the newline.
          "Churned in a batch churn.\x7f\&Milk comes from the same farm."
        , ""
        , "Products"
        , "Butter;kg;1;100;not defined;material;(2,1,1,1,1),churned on site\x7f\&from pasture milk"
        , ""
        , "Materials/fuels"
        , -- name;unit;amount;Undefined;0;0;0;<pedigree>,<comment>
          "Cow milk;kg;20.53;Undefined;0;0;0;(3,3,2,1,2),farm milk"
        , ""
        , "Resources"
        , -- name;compartment;unit;amount;Undefined;;;;;;<comment>
          "Water, river;in water;m3;0.1;Undefined;;;;;;river withdrawal"
        , ""
        , "Emissions to air"
        , "Carbon dioxide, fossil;high. pop.;kg;0.5;Undefined;;;;;;(2,4,3,3,1),combustion"
        , ""
        , "Emissions to water"
        , "Phosphate;river;kg;0.01;Undefined;;;;;;"
        , ""
        , "End"
        , ""
        , "Process"
        , ""
        , "Category type"
        , "material"
        , ""
        , "Process name"
        , "Cow milk supply"
        , ""
        , "Type"
        , "Unit process"
        , ""
        , "Geography"
        , "FR"
        , ""
        , "Products"
        , "Cow milk;kg;1;100;not defined;material;"
        , ""
        , "Emissions to air"
        , "Methane, fossil;high. pop.;kg;0.02;Undefined;0;0;"
        , ""
        , "End"
        ]

-- | How many lines of a written file start with the given prefix.
countOf :: BS.ByteString -> BS.ByteString -> Int
countOf prefix written = length (filter (BS.isPrefixOf prefix) (BS.split 10 written))

{- | A single process with one reference product and one substitution, in the
@Avoided products@ section the parser reads back as an 'AvoidedProduct'.
-}
coproductCSV :: BS.ByteString
coproductCSV =
    BS.intercalate
        "\r\n"
        [ "{SimaPro 9.6.0.1}"
        , "{CSV separator: Semicolon}"
        , "{Decimal separator: .}"
        , ""
        , "Process"
        , ""
        , "Category type"
        , "material"
        , ""
        , "Process name"
        , "Margarine production"
        , ""
        , "Type"
        , "Unit process"
        , ""
        , "Geography"
        , "FR"
        , ""
        , "Products"
        , "Margarine;kg;1;100;not defined;material;"
        , ""
        , "Avoided products"
        , "Butter;kg;0.3;100;not defined;material;"
        , ""
        , "End"
        ]

-- | A process with no @Type@ line, so the parser yields @activityNativeType = Nothing@.
noTypeCSV :: BS.ByteString
noTypeCSV =
    BS.intercalate
        "\r\n"
        [ "{SimaPro 9.6.0.1}"
        , "{CSV separator: Semicolon}"
        , "{Decimal separator: .}"
        , ""
        , "Process"
        , ""
        , "Category type"
        , "material"
        , ""
        , "Process name"
        , "Untyped process"
        , ""
        , "Geography"
        , "FR"
        , ""
        , "Products"
        , "Thing;kg;1;100;not defined;material;"
        , ""
        , "End"
        ]

-- | A process with a @Final waste flows@ section, whose rows are elementary.
wasteCSV :: BS.ByteString
wasteCSV =
    BS.intercalate
        "\r\n"
        [ "{SimaPro 9.6.0.1}"
        , "{CSV separator: Semicolon}"
        , "{Decimal separator: .}"
        , ""
        , "Process"
        , ""
        , "Category type"
        , "material"
        , ""
        , "Process name"
        , "Landfill process"
        , ""
        , "Geography"
        , "FR"
        , ""
        , "Products"
        , "Treated waste;kg;1;100;not defined;material;"
        , ""
        , "Final waste flows"
        , -- name;compartment;unit;amount;Undefined;;;;;;<comment>
          "Municipal solid waste;;kg;0.5;Undefined;;;;;;to landfill"
        , ""
        , "End"
        ]

-- | Parse a SimaPro CSV blob through a temp file.
parseBytes :: BS.ByteString -> IO ([Activity], TechFlowDB, BioFlowDB, WasteFlowDB, UnitDB)
parseBytes bytes = withSystemTempFile "writer-spec.csv" $ \path h -> do
    BS.hPut h bytes
    hClose h
    parseOrFail defaultUnitConfig path

-- | The re-import as the engine does it: parsed, then split by the loader.
loadBytes :: BS.ByteString -> IO SimpleDatabase
loadBytes bytes = withSystemTempFile "writer-spec.csv" $ \path h -> do
    BS.hPut h bytes
    hClose h
    either (fail . T.unpack) pure =<< loadSimaProCSV (defaultLoadOptions defaultUnitConfig) path

-- | Wrap parser output in a 'SimpleDatabase' keyed by generated UUIDs.
toSimple :: ([Activity], TechFlowDB, BioFlowDB, WasteFlowDB, UnitDB) -> SimpleDatabase
toSimple (acts, tech, bio, waste, units) =
    SimpleDatabase
        { sdbActivities = M.fromList [(activityKey a, a) | a <- acts]
        , sdbTechFlows = tech
        , sdbBioFlows = bio
        , sdbWasteFlows = waste
        , sdbUnits = units
        }

{- | Stable key for an activity: its reference product flow UUID paired with a
synthetic activity UUID derived from name+location. The parser does not hand
back the (UUID,UUID) keys directly, so we mint a deterministic pair from
fields we control; this only needs to be injective across the fixture.
-}
activityKey :: Activity -> (UUID, UUID)
activityKey a =
    case [exchangeFlowId ex | ex <- exchanges a, exchangeIsReference ex] of
        (prod : _) -> (prod, prod)
        [] -> (UUID.nil, UUID.nil)

-- Re-export of nil without importing the whole module qualifier dance.
-- (Types re-exports UUID; nil lives in Data.UUID.)

-- ---------------------------------------------------------------------------
-- Structural comparison helpers (order-insensitive)
-- ---------------------------------------------------------------------------

-- | Normalised, order-insensitive view of an activity for structural equality.
activityShape :: TechFlowDB -> BioFlowDB -> WasteFlowDB -> UnitDB -> Activity -> ActivityShape
activityShape tech bio waste units a =
    ActivityShape
        { asName = activityName a
        , asLocation = activityLocation a
        , asUnit = activityUnit a
        , asExchanges = sort (map (exchangeShape tech bio waste units) (exchanges a))
        }

data ActivityShape = ActivityShape
    { asName :: Text
    , asLocation :: Text
    , asUnit :: Text
    , asExchanges :: [ExchangeShape]
    }
    deriving (Eq, Ord, Show)

{- | (kind, flow-name, unit-name, rounded-amount, is-input, is-reference,
comment, pedigree). Comment and pedigree are carried so the semantic
round-trip (property b) actually pins them: without them the per-exchange
metadata could be silently dropped on parse-back and the test would not notice.
-}
data ExchangeShape = ExchangeShape
    { esKind :: Text
    , esFlow :: Text
    , esUnit :: Text
    , esAmount :: Double
    , esInput :: Bool
    , esRef :: Bool
    , esComment :: Maybe Text
    , esPedigree :: Maybe (Int, Int, Int, Int, Int)
    }
    deriving (Eq, Ord, Show)

-- | Flatten a pedigree to a sortable quintuple so 'ExchangeShape' can derive 'Ord'.
pedigreeTuple :: Pedigree -> (Int, Int, Int, Int, Int)
pedigreeTuple Pedigree{..} =
    (pedReliability, pedCompleteness, pedTemporal, pedGeographical, pedTechnological)

exchangeShape :: TechFlowDB -> BioFlowDB -> WasteFlowDB -> UnitDB -> Exchange -> ExchangeShape
exchangeShape tech bio waste units ex =
    let unitNm uid = maybe "" unitName (M.lookup uid units)
        roundAmt v = fromIntegral (round (v * 1e6) :: Integer) / 1e6 :: Double
        meta kind flow uid amt =
            ExchangeShape
                kind
                flow
                (unitNm uid)
                (roundAmt amt)
                (exchangeIsInput ex)
                (exchangeIsReference ex)
                (exchangeComment ex)
                (pedigreeTuple <$> exchangePedigree ex)
     in case ex of
            TechnosphereExchange{techFlowId = fid, techAmount = amt, techUnitId = uid} ->
                meta "tech" (maybe "?" tfName (M.lookup fid tech)) uid amt
            BiosphereExchange{bioFlowId = fid, bioAmount = amt, bioUnitId = uid} ->
                meta "bio" (maybe "?" bfName (M.lookup fid bio)) uid amt
            WasteExchange{waFlowId = fid, waAmount = amt, waUnitId = uid} ->
                meta "waste" (maybe "?" wfName (M.lookup fid waste)) uid amt

shapeSet :: ([Activity], TechFlowDB, BioFlowDB, WasteFlowDB, UnitDB) -> S.Set ActivityShape
shapeSet (acts, tech, bio, waste, units) =
    S.fromList (map (activityShape tech bio waste units) acts)

-- ---------------------------------------------------------------------------
-- Inventory (score-equivalence) helper
-- ---------------------------------------------------------------------------

{- | Build a Database from parser output and compute the inventory of the
activity whose name matches, keyed by biosphere flow NAME (UUIDs are stable
across the round-trip, but keying by name is robust either way).
-}
inventoryByName :: ([Activity], TechFlowDB, BioFlowDB, WasteFlowDB, UnitDB) -> Text -> IO (M.Map Text Double)
inventoryByName (acts, tech, bio, waste, units) target = do
    let actMap = M.fromList [(activityKey a, a) | a <- acts]
    built <-
        buildDatabaseWithMatrices
            (BuildInputs defaultUnitConfig mempty Declared)
            SimpleDatabase
                { sdbActivities = actMap
                , sdbTechFlows = tech
                , sdbBioFlows = bio
                , sdbWasteFlows = waste
                , sdbUnits = units
                }
    case built of
        Left err -> expectationFailure (T.unpack err) >> pure M.empty
        Right db -> do
            let pidFor =
                    [ pid
                    | (pid, (actU, _)) <- zip [0 ..] (V.toList (dbProcessIdTable db))
                    , Just act <- [activityAtUUID db actU]
                    , activityName act == target
                    ]
            case pidFor of
                (pid : _) -> do
                    inv <- either (fail . show) pure =<< computeInventoryMatrix db pid
                    pure (M.mapKeys (flowName db) inv)
                [] -> expectationFailure ("no activity named " <> T.unpack target) >> pure M.empty

flowName :: Database -> UUID -> Text
flowName db uid = maybe (T.pack (show uid)) bfName (M.lookup uid (dbBioFlows db))

activityAtUUID :: Database -> UUID -> Maybe Activity
activityAtUUID db actU =
    case [i | (i, (a, _)) <- zip [0 ..] (V.toList (dbProcessIdTable db)), a == actU] of
        (i : _) -> Just (dbActivities db V.! i)
        [] -> Nothing

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

-- | Serialize through the guard-returning writer, failing the test on a 'Left'.
serBytes :: SimpleDatabase -> IO BS.ByteString
serBytes db =
    either (\e -> expectationFailure (T.unpack e) >> pure BS.empty) pure $
        serializeSimaProCSV defaultWriterConfig db

spec :: Spec
spec = describe "SimaPro.Writer round-trip" $ do
    it "(pure) formatAmount round-trips integral and fractional values" $ do
        formatAmount 100 `shouldBe` "100"
        formatAmount 1 `shouldBe` "1"
        formatAmount 0.5 `shouldBe` "0.5"
        formatAmount 20.53 `shouldBe` "20.53"
        formatAmount (-1.5) `shouldBe` "-1.5"

    it "(pure) escapeField quotes only when needed" $ do
        escapeField "Butter" `shouldBe` "Butter"
        escapeField "a;b" `shouldBe` "\"a;b\""
        escapeField "say \"hi\"" `shouldBe` "\"say \"\"hi\"\"\""

    it "produces a parseable CSV with the pinned header" $ do
        original <- parseBytes fixtureCSV
        bytes <- serBytes (toSimple original)
        BS.take 16 bytes `shouldSatisfy` (\b -> "{SimaPro" `BS.isInfixOf` b)
        reparsed <- parseBytes bytes
        let (acts, _, _, _, _) = reparsed
        length acts `shouldBe` 2

    it "(a) is idempotent modulo the volatile version banner" $ do
        original <- parseBytes fixtureCSV
        f0 <- serBytes (toSimple original)
        d' <- parseBytes f0
        f1 <- serBytes (toSimple d')
        f1 `shouldBe` f0

    it "(b) semantic round-trip: parse(write(D)) is structurally equal to D" $ do
        original <- parseBytes fixtureCSV
        f0 <- serBytes (toSimple original)
        reparsed <- parseBytes f0
        -- The shape now carries esComment/esPedigree, so the structural equality
        -- below also pins per-exchange comment and pedigree across the round-trip.
        shapeSet reparsed `shouldBe` shapeSet original
        -- Guard against a vacuous pass (both sides dropping the metadata): the
        -- fixture deliberately carries a pedigree quintuple and free-text
        -- comments, so the original parse must actually surface them.
        let origExchanges = concatMap exchanges (activitiesOf original)
        map exchangeComment origExchanges `shouldSatisfy` elem (Just "farm milk")
        map exchangePedigree origExchanges `shouldSatisfy` elem (Just (Pedigree 3 3 2 1 2))
        -- Same guard for the Products row: the reference product's comment and
        -- pedigree must survive the round-trip, not just the input rows'. The
        -- comment is multi-line (\x7f in the file, \n in memory), pinning the
        -- writer's \n → \x7f re-encoding as well.
        map exchangeComment origExchanges `shouldSatisfy` elem (Just "churned on site\nfrom pasture milk")
        map exchangePedigree origExchanges `shouldSatisfy` elem (Just (Pedigree 2 1 1 1 1))
        -- Same again for the activity description, the other multi-line free
        -- text field. It reaches the writer holding a \n, and only survives
        -- because the Comment metadata line is \x7f-encoded on the way out.
        map activityDescription (activitiesOf original)
            `shouldSatisfy` elem ["Churned in a batch churn.\nMilk comes from the same farm."]

    it "(c) score-equivalence: same inventory for a sample activity within tolerance" $ do
        original <- parseBytes fixtureCSV
        f0 <- serBytes (toSimple original)
        reparsed <- parseBytes f0
        invOrig <- inventoryByName original "Butter production"
        invRound <- inventoryByName reparsed "Butter production"
        M.keysSet invOrig `shouldBe` M.keysSet invRound
        let diffs =
                [ abs (a - b)
                | (k, a) <- M.toList invOrig
                , let b = M.findWithDefault 0 k invRound
                ]
        all (< 1e-9) diffs `shouldBe` True

    it "(regression) routes coproducts to Avoided products, not Products" $ do
        original <- parseBytes coproductCSV
        f0 <- serBytes (toSimple original)
        reparsed <- parseBytes f0
        let acts0 = activitiesOf original
            acts1 = activitiesOf reparsed
            roles = [techRole e | a <- acts1, e@TechnosphereExchange{} <- exchanges a]
        -- An avoided product written to "Products" would re-parse as a coproduct
        -- of the block; routing it to "Avoided products" keeps the activity
        -- count stable and preserves the AvoidedProduct role.
        length acts1 `shouldBe` length acts0
        (AvoidedProduct `elem` roles) `shouldBe` True

    it "(regression) writes an avoided product once, not as an input as well" $ do
        original <- parseBytes coproductCSV
        f0 <- serBytes (toSimple original)
        -- Written under "Avoided products" and nowhere else. Under
        -- "Materials/fuels" as well, the re-import pairs the credit with an
        -- equal input and the substitution cancels itself out.
        countOf "Butter;kg;0.3" f0 `shouldBe` 1
        reparsed <- parseBytes f0
        let roles = [techRole e | a <- activitiesOf reparsed, e@TechnosphereExchange{} <- exchanges a]
        length (filter (== AvoidedProduct) roles) `shouldBe` 1
        length (filter (== Input) roles) `shouldBe` 0

    it "(regression) omits the Type line for an activity with no native type" $ do
        original <- parseBytes noTypeCSV
        f0 <- serBytes (toSimple original)
        reparsed <- parseBytes f0
        -- No Type line written → re-parse yields Nothing again, not the
        -- invented "Unit process".
        map activityNativeType (activitiesOf reparsed) `shouldBe` [Nothing]

    it "(regression) allocation ≠ 100% round-trips without double-scaling inputs" $ do
        -- The in-memory amounts are already allocation-scaled (the parser scales
        -- shared exchanges by allocFraction on import). A 50%-allocated activity
        -- stores its 20 kg input as 10 kg; the writer must emit 20 kg again so the
        -- re-import lands back on 10 kg, not 5 kg (the double-allocation bug).
        bytes <- serBytes allocationDb
        db <- loadBytes bytes
        case M.elems (sdbActivities db) of
            [a] -> do
                [techAmount e | e@TechnosphereExchange{techRole = Input} <- exchanges a]
                    `shouldBe` [10.0]
                (dsPercent <$> activityReferenceShare a) `shouldBe` Just 50
            other -> expectationFailure ("expected one activity, got " <> show (length other))

    it "(regression) serializes a 0%-allocated activity without a divide-by-zero" $ do
        -- The parser scaled the 0%-allocated co-product's shared amounts to 0; the
        -- writer emits those zeros as-is rather than dividing by zero, so nothing
        -- non-finite leaks into the file.
        bytes <- serBytes zeroAllocationDb
        ("NaN" `BS.isInfixOf` bytes || "Infinity" `BS.isInfixOf` bytes) `shouldBe` False

    it "(regression) round-trips a Final waste flows section" $ do
        original <- parseBytes wasteCSV
        f0 <- serBytes (toSimple original)
        reparsed <- parseBytes f0
        shapeSet reparsed `shouldBe` shapeSet original
        -- Guard against a vacuous pass: the original must actually carry the
        -- section's flow, so the structural equality above is pinning
        -- something. A final waste flow is elementary, filed under its own
        -- medium, and the section is what the writer has to find it again by.
        let (_, _, bioFlows, wasteFlows, _) = original
        map bfCompartment (M.elems bioFlows)
            `shouldBe` [Just (Compartment Waste Nothing)]
        M.size wasteFlows `shouldBe` 0

    describe "checkSimaProExportable (emission-medium guard)" $ do
        it "accepts air / water / soil emissions" $
            checkSimaProExportable (emissionDb (Just (Compartment Air Nothing)))
                `shouldBe` Right ()

        it "rejects an emission whose medium has no faithful SimaPro section" $
            -- A "raw" medium would otherwise be silently filed under
            -- "Emissions to air" and re-parse as air; reject it loudly instead.
            checkSimaProExportable (emissionDb (Just (Compartment NaturalResource Nothing)))
                `shouldSatisfy` either (const True) (const False)

        -- An inventory indicator is neither an emission to a medium nor an
        -- extraction from one. It has a section of its own, so the medium guard
        -- must let it through and the writer must file it there: under
        -- "Emissions to air" it would re-parse as an emission to air, and the
        -- guard alone would refuse every database carrying one.
        it "accepts an inventory indicator and files it under Final waste flows" $ do
            let db = emissionDb (Just (Compartment InventoryIndicator (Just "waste")))
            checkSimaProExportable db `shouldBe` Right ()
            case serializeSimaProCSV defaultWriterConfig db of
                Left err -> expectationFailure (T.unpack err)
                Right out -> do
                    firstRowUnder out "Final waste flows"
                        `shouldBe` ["Some emission;waste;kg;0.5;Undefined;;;;;;"]
                    firstRowUnder out "Emissions to air" `shouldBe` []

        -- A flow the map knows but that records no compartment keeps its row,
        -- so it must keep a section. Dropping the section drops the pair, and
        -- the export loses the exchange with the check still reporting success.
        it "files an emission whose flow records no compartment under air" $ do
            let db = emissionDb Nothing
            checkSimaProExportable db `shouldBe` Right ()
            case serializeSimaProCSV defaultWriterConfig db of
                Left err -> expectationFailure (T.unpack err)
                Right out ->
                    firstRowUnder out "Emissions to air"
                        `shouldBe` ["Some emission;;kg;0.5;Undefined;;;;;;"]

        it "accepts a final waste flow, whose medium is the section's own name" $
            checkSimaProExportable (emissionDb (Just (Compartment Waste Nothing)))
                `shouldBe` Right ()

    -- This format has no waste axis of its own, so a waste exchange from
    -- another one has to be written into one of its two waste sections, and
    -- the section decides what a re-import makes of it. Naming a treatment is
    -- what tells the two apart.
    describe "a waste exchange imported from another format" $ do
        it "writes one that names its treatment under Waste to treatment" $
            case serializeSimaProCSV defaultWriterConfig (wasteDb (Just (testUUID 0xbb))) of
                Left err -> expectationFailure (T.unpack err)
                Right out -> do
                    firstRowUnder out "Waste to treatment"
                        `shouldBe` ["spent solvent;kg;0.3;Undefined;0;0;0;"]
                    firstRowUnder out "Final waste flows" `shouldBe` []

        it "writes one that names none under Final waste flows" $
            case serializeSimaProCSV defaultWriterConfig (wasteDb Nothing) of
                Left err -> expectationFailure (T.unpack err)
                Right out -> do
                    firstRowUnder out "Final waste flows"
                        `shouldBe` ["spent solvent;;kg;0.3;Undefined;;;;;;"]
                    firstRowUnder out "Waste to treatment" `shouldBe` []

    describe "checkSimaProExportable (round-trip guards)" $ do
        it "refuses an activity whose product rows carry no share: each row would claim the whole" $
            case checkSimaProExportable (guardDb Nothing Nothing [refProd, refProd2{techRole = Coproduct}]) of
                Left msg -> do
                    msg `shouldSatisfy` T.isInfixOf "guard maker"
                    msg `shouldSatisfy` T.isInfixOf "without a declared share"
                Right () -> expectationFailure "an unallocated activity was accepted"
        it "accepts a pedigree-less exchange with an ordinary comment" $
            checkSimaProExportable (commentDb Nothing (Just "ordinary free text"))
                `shouldBe` Right ()

        it "rejects a pedigree-less comment that begins with a pedigree quintuple" $
            -- Written verbatim, "(3,3,2,1,2),free text" would re-parse as a
            -- fabricated Pedigree with a truncated comment; reject it loudly.
            checkSimaProExportable (commentDb Nothing (Just "(3,3,2,1,2),free text"))
                `shouldSatisfy` either (const True) (const False)

        it "still accepts a genuine pedigree (it is emitted as its own prefix)" $
            checkSimaProExportable (commentDb (Just (Pedigree 3 3 2 1 2)) (Just "free text"))
                `shouldBe` Right ()

        it "rejects an activity name that collides with a SimaPro metadata key" $
            -- An activity named "Comment" would be read back as a new metadata
            -- field, silently dropping the name; reject it at the boundary.
            checkSimaProExportable (namedDb "Comment")
                `shouldSatisfy` either (const True) (const False)

        it "accepts an ordinary activity name" $
            checkSimaProExportable (namedDb "Butter production")
                `shouldBe` Right ()

    describe "checkSimaProExportable (field-shape guards)" $ do
        it "accepts a baseline single-reference activity" $
            checkSimaProExportable (guardDb Nothing Nothing [refProd])
                `shouldBe` Right ()

        it "rejects a newline in the native-type (Type) label" $
            -- The Type label is emitted as a bare metadata value line, so a
            -- newline would split the Process block across physical rows on
            -- re-import. Regression: the newline guard now covers the Type label
            -- (it is derived from the same 'activityMetaLines' as the emitter).
            checkSimaProExportable (guardDb Nothing (Just (SimaProProcessType "Unit\nprocess")) [refProd])
                `shouldSatisfy` isLeft

        it "accepts a multi-paragraph description and encodes it as \\x7f" $ do
            -- Free text is not an identity field: a line break there carries
            -- meaning, and SimaPro's own convention holds it on one physical
            -- line as \x7f. Refusing it would make every database that came
            -- from a SimaPro export inexportable, since the parser decodes
            -- \x7f to \n on the way in.
            let db = describedDb ["First paragraph.\nStill the first.", "Second paragraph."]
            checkSimaProExportable db `shouldBe` Right ()
            out <- serBytes db
            out `shouldSatisfy` BS.isInfixOf "First paragraph.\x7fStill the first.\x7fSecond paragraph."

        it "rejects an activity with no reference product" $
            -- An empty Products section makes the parser discard the whole block.
            checkSimaProExportable (guardDb Nothing Nothing [matInput])
                `shouldSatisfy` isLeft

        it "rejects an activity with more than one reference product" $
            -- Two Products rows re-parse into two separate activities.
            checkSimaProExportable (guardDb Nothing Nothing [refProd, refProd2])
                `shouldSatisfy` isLeft

        it "rejects a non-finite exchange amount" $
            -- NaN/±Infinity has no parseable literal; 'formatAmount' would flatten
            -- it to a number and silently mis-state the inventory.
            checkSimaProExportable (guardDb Nothing Nothing [refProd, bioInf])
                `shouldSatisfy` isLeft

        it "accepts a zero allocation percentage" $
            -- A 0% co-product carries 0% of the shared burden; the parser already
            -- scaled its shared amounts to 0, so the writer emits those zeros as-is
            -- (no divide-by-zero) and the round-trip is faithful.
            checkSimaProExportable (guardDb (Just 0) Nothing [refProd])
                `shouldSatisfy` isRight

        it "rejects a non-finite allocation percentage" $
            -- A non-finite percentage cannot be written as a number and would
            -- corrupt the divide-back-out, so it is rejected.
            checkSimaProExportable (guardDb (Just (1 / 0)) Nothing [refProd])
                `shouldSatisfy` isLeft

        it "rejects an exchange whose unit is absent from the registry" $
            -- A missing unit would be written blank and re-parsed as UNKNOWN.
            checkSimaProExportable (guardDb Nothing Nothing [refProd, matMissingUnit])
                `shouldSatisfy` isLeft
  where
    activitiesOf (acts, _, _, _, _) = acts

-- ---------------------------------------------------------------------------
-- Emission-medium guard fixture
-- ---------------------------------------------------------------------------

{- | A distinct, valid UUID for a fixture, built totally from a tag, no partial
'read' that would crash the suite on a typo.
-}
testUUID :: Word32 -> UUID
testUUID n = UUID.fromWords n 0x4000 0x8000 1

{- | One activity emitting a single biosphere flow whose compartment is @comp@.
Used to exercise 'checkSimaProExportable': air/water/soil pass, anything else is
rejected.
-}
emissionDb :: Maybe Compartment -> SimpleDatabase
emissionDb comp =
    SimpleDatabase
        { sdbActivities = M.singleton (actU, prodU) act
        , sdbTechFlows = M.singleton prodU (TechnosphereFlow prodU "thing" unitU M.empty Nothing Nothing)
        , sdbBioFlows = M.singleton bioU (BiosphereFlow bioU "Some emission" unitU M.empty Nothing Nothing comp)
        , sdbWasteFlows = M.empty
        , sdbUnits = M.singleton unitU (Unit unitU "kg" "kg" "")
        }
  where
    actU, prodU, bioU, unitU :: UUID
    actU = testUUID 0x10
    prodU = testUUID 0xa0
    bioU = testUUID 0xc0
    unitU = testUUID 0x01
    act =
        Activity
            "thing maker"
            []
            []
            M.empty
            M.empty
            "GLO"
            LocationDeclared
            "kg"
            [ TechnosphereExchange prodU 1.0 unitU ReferenceProduct Nothing ClaimByProduct "" Nothing Nothing Nothing M.empty noProperties
            , BiosphereExchange bioU 0.5 unitU Emission "" Nothing Nothing
            ]
            M.empty
            M.empty
            Nothing
            Nothing
            Nothing

{- | One activity carrying a waste exchange of the kind only another format
produces, optionally naming the activity that treats it. Which section the
writer files it under is what decides the kind it comes back as.
-}
wasteDb :: Maybe UUID -> SimpleDatabase
wasteDb mTreatment =
    SimpleDatabase
        { sdbActivities = M.singleton (actU, prodU) act
        , sdbTechFlows = M.singleton prodU (TechnosphereFlow prodU "thing" unitU M.empty Nothing Nothing)
        , sdbBioFlows = M.empty
        , sdbWasteFlows = M.singleton wasteU (WasteFlow wasteU "spent solvent" unitU M.empty Nothing Nothing)
        , sdbUnits = M.singleton unitU (Unit unitU "kg" "kg" "")
        }
  where
    actU, prodU, wasteU, unitU :: UUID
    actU = testUUID 0x20
    prodU = testUUID 0xa1
    wasteU = testUUID 0xc1
    unitU = testUUID 0x01
    act =
        Activity
            "solvent user"
            []
            []
            M.empty
            M.empty
            "GLO"
            LocationDeclared
            "kg"
            [ TechnosphereExchange prodU 1.0 unitU ReferenceProduct Nothing ClaimByProduct "" Nothing Nothing Nothing M.empty noProperties
            , WasteExchange wasteU 0.3 unitU False mTreatment ClaimByProduct "" Nothing Nothing
            ]
            M.empty
            M.empty
            Nothing
            Nothing
            Nothing

{- | The first row a serialized export carries under a section header, or an
empty list when the section is absent. The writer joins with CRLF, so the
carriage returns come off before anything is compared.
-}
firstRowUnder :: BS.ByteString -> Text -> [Text]
firstRowUnder out header =
    take 1 (takeWhile (not . T.null) (drop 1 (dropWhile (/= header) ls)))
  where
    ls :: [Text]
    ls = map (T.dropWhileEnd (== '\r')) (T.lines (TE.decodeUtf8 out))

-- ---------------------------------------------------------------------------
-- Allocation round-trip fixture
-- ---------------------------------------------------------------------------

{- | One 50%-allocated activity: a reference product (never scaled) and a
material input stored at its allocation-scaled amount (10 kg = 20 kg × 0.5).
A correct writer emits the 20 kg pre-allocation amount so the parser's re-scale
returns 10 kg.
-}
allocationDb :: SimpleDatabase
allocationDb =
    SimpleDatabase
        { sdbActivities = M.singleton (actU, prodU) act
        , sdbTechFlows =
            M.fromList
                [ (prodU, TechnosphereFlow prodU "main product" unitU M.empty Nothing Nothing)
                , (matU, TechnosphereFlow matU "some material" unitU M.empty Nothing Nothing)
                ]
        , sdbBioFlows = M.empty
        , sdbWasteFlows = M.empty
        , sdbUnits = M.singleton unitU (Unit unitU "kg" "kg" "")
        }
  where
    actU, prodU, matU, unitU :: UUID
    actU = testUUID 0x20
    prodU = testUUID 0xb0
    matU = testUUID 0xb1
    unitU = testUUID 0x02
    act =
        Activity
            "alloc maker"
            []
            []
            M.empty
            M.empty
            "GLO"
            LocationDeclared
            "kg"
            [ TechnosphereExchange prodU 1.0 unitU ReferenceProduct Nothing ClaimByProduct "" Nothing Nothing (Just (DeclaredShare 50 Nothing)) M.empty noProperties
            , TechnosphereExchange matU 10.0 unitU Input Nothing ClaimByProduct "" Nothing Nothing Nothing M.empty noProperties
            ]
            M.empty
            M.empty
            Nothing
            Nothing
            Nothing

{- | A 0%-allocated activity: its shared material input is stored at 0 (the parser
scaled every shared amount by allocFraction = 0 on import). A correct writer emits
that 0 as-is (no divide-by-zero) so the re-import scales 0 by 0 back to 0.
-}
zeroAllocationDb :: SimpleDatabase
zeroAllocationDb =
    SimpleDatabase
        { sdbActivities = M.singleton (actU, prodU) act
        , sdbTechFlows =
            M.fromList
                [ (prodU, TechnosphereFlow prodU "main product" unitU M.empty Nothing Nothing)
                , (matU, TechnosphereFlow matU "some material" unitU M.empty Nothing Nothing)
                ]
        , sdbBioFlows = M.empty
        , sdbWasteFlows = M.empty
        , sdbUnits = M.singleton unitU (Unit unitU "kg" "kg" "")
        }
  where
    actU, prodU, matU, unitU :: UUID
    actU = testUUID 0x21
    prodU = testUUID 0xb2
    matU = testUUID 0xb3
    unitU = testUUID 0x02
    act =
        Activity
            "zero alloc maker"
            []
            []
            M.empty
            M.empty
            "GLO"
            LocationDeclared
            "kg"
            [ TechnosphereExchange prodU 1.0 unitU ReferenceProduct Nothing ClaimByProduct "" Nothing Nothing (Just (DeclaredShare 0 Nothing)) M.empty noProperties
            , TechnosphereExchange matU 0.0 unitU Input Nothing ClaimByProduct "" Nothing Nothing Nothing M.empty noProperties
            ]
            M.empty
            M.empty
            Nothing
            Nothing
            Nothing

-- ---------------------------------------------------------------------------
-- Round-trip guard fixtures (comment-as-pedigree, metadata-key collision)
-- ---------------------------------------------------------------------------

{- | One activity with a reference product and a single technosphere input
carrying the given pedigree/comment. Exercises the comment-as-pedigree guard.
-}
commentDb :: Maybe Pedigree -> Maybe Text -> SimpleDatabase
commentDb ped cmt =
    SimpleDatabase
        { sdbActivities = M.singleton (actU, prodU) act
        , sdbTechFlows =
            M.fromList
                [ (prodU, TechnosphereFlow prodU "main product" unitU M.empty Nothing Nothing)
                , (matU, TechnosphereFlow matU "some material" unitU M.empty Nothing Nothing)
                ]
        , sdbBioFlows = M.empty
        , sdbWasteFlows = M.empty
        , sdbUnits = M.singleton unitU (Unit unitU "kg" "kg" "")
        }
  where
    actU, prodU, matU, unitU :: UUID
    actU = testUUID 0x30
    prodU = testUUID 0xd0
    matU = testUUID 0xd1
    unitU = testUUID 0x03
    act =
        Activity
            "comment maker"
            []
            []
            M.empty
            M.empty
            "GLO"
            LocationDeclared
            "kg"
            [ TechnosphereExchange prodU 1.0 unitU ReferenceProduct Nothing ClaimByProduct "" Nothing Nothing Nothing M.empty noProperties
            , TechnosphereExchange matU 2.0 unitU Input Nothing ClaimByProduct "" cmt ped Nothing M.empty noProperties
            ]
            M.empty
            M.empty
            Nothing
            Nothing
            Nothing

{- | One activity with the given name and a single reference product. Exercises
the metadata-key collision guard: a name equal to a SimaPro key is rejected.
-}
namedDb :: Text -> SimpleDatabase
namedDb name =
    SimpleDatabase
        { sdbActivities = M.singleton (actU, prodU) act
        , sdbTechFlows = M.singleton prodU (TechnosphereFlow prodU "main product" unitU M.empty Nothing Nothing)
        , sdbBioFlows = M.empty
        , sdbWasteFlows = M.empty
        , sdbUnits = M.singleton unitU (Unit unitU "kg" "kg" "")
        }
  where
    actU, prodU, unitU :: UUID
    actU = testUUID 0x40
    prodU = testUUID 0xe0
    unitU = testUUID 0x04
    act =
        Activity
            name
            []
            []
            M.empty
            M.empty
            "GLO"
            LocationDeclared
            "kg"
            [TechnosphereExchange prodU 1.0 unitU ReferenceProduct Nothing ClaimByProduct "" Nothing Nothing Nothing M.empty noProperties]
            M.empty
            M.empty
            Nothing
            Nothing
            Nothing

-- ---------------------------------------------------------------------------
-- Field-shape guard fixtures (shared by the field-shape guard specs)
-- ---------------------------------------------------------------------------

-- | Catalog UUIDs shared by every 'guardDb' fixture.
gProd, gMat, gBio, gUnit :: UUID
gProd = testUUID 0x50
gMat = testUUID 0x51
gBio = testUUID 0x52
gUnit = testUUID 0x53

-- | A valid reference product output for the catalog above.
refProd :: Exchange
refProd = TechnosphereExchange gProd 1.0 gUnit ReferenceProduct Nothing ClaimByProduct "" Nothing Nothing Nothing M.empty noProperties

-- | A second reference product (on the material flow), an invalid second head.
refProd2 :: Exchange
refProd2 = TechnosphereExchange gMat 1.0 gUnit ReferenceProduct Nothing ClaimByProduct "" Nothing Nothing Nothing M.empty noProperties

-- | A plain material input (no reference product in the activity).
matInput :: Exchange
matInput = TechnosphereExchange gMat 2.0 gUnit Input Nothing ClaimByProduct "" Nothing Nothing Nothing M.empty noProperties

-- | An emission carrying a non-finite (+Infinity) amount.
bioInf :: Exchange
bioInf = BiosphereExchange gBio (1 / 0) gUnit Emission "" Nothing Nothing

-- | A material input referencing a unit UUID absent from the unit registry.
matMissingUnit :: Exchange
matMissingUnit = TechnosphereExchange gMat 2.0 (testUUID 0xbad) Input Nothing ClaimByProduct "" Nothing Nothing Nothing M.empty noProperties

{- | A one-activity database whose exchanges are supplied verbatim, against a
fixed catalog (a reference product flow, a material flow, an emission flow, and
a kg unit). Lets the field-shape guard specs vary allocation, native type, and
the exchange list while keeping every flow/unit resolvable.
-}
guardDb :: Maybe Double -> Maybe NativeActivityType -> [Exchange] -> SimpleDatabase
guardDb alloc ntype exs =
    SimpleDatabase
        { sdbActivities = M.singleton (testUUID 0x5f, gProd) act
        , sdbTechFlows =
            M.fromList
                [ (gProd, TechnosphereFlow gProd "main product" gUnit M.empty Nothing Nothing)
                , (gMat, TechnosphereFlow gMat "some material" gUnit M.empty Nothing Nothing)
                ]
        , sdbBioFlows =
            M.singleton gBio (BiosphereFlow gBio "an emission" gUnit M.empty Nothing Nothing (Just (Compartment Air Nothing)))
        , sdbWasteFlows = M.empty
        , sdbUnits = M.singleton gUnit (Unit gUnit "kg" "kg" "")
        }
  where
    act =
        Activity "guard maker" [] [] M.empty M.empty "GLO" LocationDeclared "kg" (map withShare exs) M.empty M.empty ntype Nothing Nothing
    -- The share a source declares lives on the reference exchange.
    withShare ex = case ex of
        TechnosphereExchange{techRole = ReferenceProduct} -> ex{techShare = (`DeclaredShare` Nothing) <$> alloc}
        TechnosphereExchange{} -> ex
        BiosphereExchange{} -> ex
        WasteExchange{} -> ex

-- | The baseline guard fixture, carrying the given description paragraphs.
describedDb :: [Text] -> SimpleDatabase
describedDb paragraphs =
    let db = guardDb Nothing Nothing [refProd]
     in db{sdbActivities = M.map (\a -> a{activityDescription = paragraphs}) (sdbActivities db)}

{- | Parse, failing the example when the parser refuses the file. The parser
now returns 'Left' for a flow written in two units no conversion relates.
-}
parseOrFail :: UnitConfig -> FilePath -> IO ([Activity], TechFlowDB, BioFlowDB, WasteFlowDB, UnitDB)
parseOrFail cfg path = either (fail . show) pure =<< parseSimaProCSV cfg path
