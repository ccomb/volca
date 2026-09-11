{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Round-trip tests for the Brightway Excel (.xlsx) /writer/, the inverse of
"BrightwayExcel.Parser".

The fixture 'SimpleDatabase' is built in Haskell using the very UUID generators
the parser feeds its flows/units/activities through ('generateFlowUUID',
'generateUnitUUID', 'generateActivityUUID'), so @parse (write D)@ reconstructs
the same identities: no committed binary fixture, and no dependence on the
cross-database link pass (technosphere links are left @nil@, exactly the state
the parser produces for a freshly imported workbook).

Three contracts are proven:

  (a) /logical-cell idempotence/: re-exporting the parsed content yields the
      same worksheet rows. Byte identity is not attempted (zip metadata is
      volatile); we compare the parsed-then-rewritten logical cells.
  (b) /semantic round-trip/: @parse (write D)@ is structurally equal to @D@,
      order-insensitively (activities, exchanges, flows, units).
  (c) /score equivalence/: the biosphere inventory of a sample activity is
      preserved across the round-trip, within tolerance, using the engine's
      matrix solver.
-}
module BrightwayExcelWriterSpec (spec) where

import BrightwayExcel.Parser (parseBrightwayExcel)
import BrightwayExcel.Writer (
    Cell (..),
    WriterConfig (..),
    activityRows,
    checkBrightwayExportable,
    defaultWriterConfig,
    formatAmount,
    renderCategories,
    renderWorkbook,
    wasteManifest,
 )
import Codec.Archive.Zip (filesInArchive, toArchive)
import Control.Monad ((>=>))
import qualified Data.ByteString.Lazy as BL
import Data.Either (isLeft, isRight)
import Data.List (find, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Database (buildDatabaseWithMatrices)
import Database.Loader (getReferenceProductUUID)
import Matrix (computeInventoryMatrix)
import SimaPro.Parser (generateActivityUUID, generateFlowUUID, generateUnitUUID)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.Hspec
import Types
import UnitConversion (UnitConfig, buildFromCSV, defaultUnitConfig)

spec :: Spec
spec = describe "BrightwayExcel.Writer" $ do
    describe "formatAmount" $ do
        it "prints integers without a decimal point" $
            formatAmount 1 `shouldBe` "1"
        it "prints fractions with full precision" $
            formatAmount 8.5 `shouldBe` "8.5"
        it "renders a small magnitude as fixed-point that re-parses exactly" $ do
            -- show 3.3e-20 emits scientific notation the parser's TR.double
            -- re-reads lossily; fixed-point keeps it value-identical end to end.
            T.isInfixOf "e" (formatAmount 3.3e-20) `shouldBe` False
            TR.double (formatAmount 3.3e-20) `shouldBe` Right (3.3e-20, "")

    describe "renderCategories" $ do
        it "joins compartment and subcompartment with ::" $
            renderCategories (Just (Compartment NaturalResource (Just "in water")))
                `shouldBe` "natural resource::in water"
        it "emits the bare medium when there is no sub" $
            renderCategories (Just (Compartment Air Nothing)) `shouldBe` "air"
        it "emits empty for no recorded compartment" $
            renderCategories Nothing `shouldBe` ""

    describe "package" $
        it "carries the OPC manifest every xlsx reader opens the archive through" $
            -- openpyxl (so bw2io, so Excel) resolves parts through
            -- [Content_Types].xml and fails with a bare KeyError without it;
            -- our own parser reads the sheet straight out of the zip and would
            -- never notice.
            case renderWorkbook defaultWriterConfig fixtureDb of
                Left err -> expectationFailure (T.unpack err)
                Right bytes ->
                    filesInArchive (toArchive bytes)
                        `shouldBe` [ "[Content_Types].xml"
                                   , "_rels/.rels"
                                   , "xl/workbook.xml"
                                   , "xl/_rels/workbook.xml.rels"
                                   , "xl/worksheets/sheet1.xml"
                                   ]

    describe "round-trip" $ do
        it "(b) parse (write D) is structurally equal to D" $
            withWritten fixtureDb $ \path -> do
                parseBrightwayExcel defaultUnitConfig path >>= \case
                    Left err -> expectationFailure (T.unpack err)
                    Right parsed -> normalizeParsed parsed `shouldBe` expectedNormalized

        it "(a) re-exporting the parsed content yields the same logical cells" $
            withWritten fixtureDb $ \path -> do
                parseBrightwayExcel defaultUnitConfig path >>= \case
                    Left err -> expectationFailure (T.unpack err)
                    Right parsed -> do
                        let db' = rebuild parsed
                        logicalCells db' `shouldBe` logicalCells fixtureDb

        it "(c) preserves a biosphere inventory amount across the round-trip" $
            withWritten fixtureDb $ \path -> do
                parseBrightwayExcel defaultUnitConfig path >>= \case
                    Left err -> expectationFailure (T.unpack err)
                    Right parsed -> do
                        before <- co2Inventory fixtureDb
                        after <- co2Inventory (rebuild parsed)
                        case (before, after) of
                            (Right (Just b), Right (Just a)) ->
                                abs (a - b) < 1.0e-9 `shouldBe` True
                            other -> expectationFailure ("CO2 inventory missing: " <> show other)

        it "(d) reconstructs XML-special and non-ASCII names verbatim" $
            -- Drives the real emit path: the writer escapes & < > " through
            -- 'escapeXml' and emits non-ASCII as UTF-8; the parser unescapes via
            -- 'decodeEntities' and decodes UTF-8. Round-tripping a fixture whose
            -- names carry all four predefined entities plus accented and CJK
            -- characters proves the two halves are inverses, not that the cells
            -- merely store raw strings.
            withWritten specialDb $
                parseBrightwayExcel defaultUnitConfig >=> \case
                    Left err -> expectationFailure (T.unpack err)
                    Right parsed -> normalizeParsed parsed `shouldBe` specialNormalized

        it "(e) round-trips a coproduct as a second production row" $
            -- Both outputs serialize with type "production"; the parser maps the
            -- first to ReferenceProduct and every further production row to
            -- Coproduct. Asserting the parsed roles and amounts proves the
            -- multi-output activity survives, where 'normalizeParsed' (which only
            -- projects reference + Input rows) cannot see the coproduct.
            withWritten coproductDb $
                parseBrightwayExcel defaultUnitConfig >=> \case
                    Left err -> expectationFailure (T.unpack err)
                    Right (acts, _, _, _, _) -> case acts of
                        [a] -> do
                            roleAmounts a `shouldBe` [(ReferenceProduct, 1), (Coproduct, 0.25)]
                            -- The coproduct's comment also flows through productRowOut.
                            [techComment ex | ex@TechnosphereExchange{} <- exchanges a]
                                `shouldBe` [Just "ref note", Just "heat note"]
                        other -> expectationFailure ("expected one activity, got " <> show (length other))

        it "(f) round-trips an empty database to no activities" $
            -- The writer still emits the Database header sheet; the parser reads
            -- it back as a valid, activity-free import rather than erroring.
            withWritten emptyDb $
                parseBrightwayExcel defaultUnitConfig >=> \case
                    Left err -> expectationFailure (T.unpack err)
                    Right (acts, _, _, _, _) -> length acts `shouldBe` 0

        it "(g) round-trips a boundary-magnitude (~1e15) amount" $
            -- At 1e15 'formatAmount' crosses from integer to fixed-point rendering;
            -- the parser reads it back through TR.double. The amount must survive
            -- the format switch exactly.
            withWritten bigAmountDb $
                parseBrightwayExcel defaultUnitConfig >=> \case
                    Left err -> expectationFailure (T.unpack err)
                    Right (acts, _, _, _, _) -> case acts of
                        [a] -> [techAmount ex | ex@TechnosphereExchange{techRole = Input} <- exchanges a] `shouldBe` [1.0e15]
                        other -> expectationFailure ("expected one activity, got " <> show (length other))

        it "(h) round-trips exchange-level comments, including the reference product" $
            -- The reference product is a production row; its comment column flows
            -- through 'productRowOut' (the same path coproducts take), which once
            -- dropped it. Assert the product, input, and biosphere comments all
            -- survive.
            withWritten commentDb $
                parseBrightwayExcel defaultUnitConfig >=> \case
                    Left err -> expectationFailure (T.unpack err)
                    Right (acts, _, _, _, _) -> case acts of
                        [a] -> do
                            [techComment ex | ex@TechnosphereExchange{techRole = ReferenceProduct} <- exchanges a]
                                `shouldBe` [Just "product note"]
                            [techComment ex | ex@TechnosphereExchange{techRole = Input} <- exchanges a]
                                `shouldBe` [Just "input note"]
                            [bioComment ex | ex@BiosphereExchange{} <- exchanges a]
                                `shouldBe` [Just "emission note"]
                        other -> expectationFailure ("expected one activity, got " <> show (length other))

        it "round-trips the supplier activity an input names apart from its product" $
            -- The name column carries the supplier activity, the reference
            -- product column the product: the parser reads its flow from the
            -- latter, so the two must not be written as one.
            withWritten supplierNamedDb $
                parseBrightwayExcel defaultUnitConfig >=> \case
                    Left err -> expectationFailure (T.unpack err)
                    Right (acts, techDB, _, _, _) -> case acts of
                        [a] -> do
                            [techSupplierClaim ex | ex@TechnosphereExchange{techRole = Input} <- exchanges a]
                                `shouldBe` [ClaimByName "market for natural gas, high pressure"]
                            [tfName f | ex@TechnosphereExchange{techRole = Input} <- exchanges a, Just f <- [M.lookup (techFlowId ex) techDB]]
                                `shouldBe` ["natural gas, high pressure"]
                        other -> expectationFailure ("expected one activity, got " <> show (length other))

        it "(i) canonicalizes a non-canonical reference unit, then is a fixed point" $
            -- The parser normalizes a reference product's unit to canonical at
            -- ingest (g→kg here, scaling 1000→1). So parse(write D) ≠ D for a
            -- non-canonical reference unit: the contract is fixed-point over the
            -- parser's image: a second round-trip reproduces the first exactly.
            withWritten gramRefDb $
                parseBrightwayExcel gramConfig >=> \case
                    Left err -> expectationFailure (T.unpack err)
                    Right parsed1 -> do
                        refUnitAmount parsed1 `shouldBe` Just ("kg", 1.0)
                        withWritten (rebuild parsed1) $
                            parseBrightwayExcel gramConfig >=> \case
                                Left err -> expectationFailure (T.unpack err)
                                Right parsed2 -> refUnitAmount parsed2 `shouldBe` refUnitAmount parsed1

        it "(j) round-trips a single-paragraph activity description" $
            -- The writer newline-joins 'activityDescription' into the one comment
            -- the format allows; the parser reads it back as a one-element list.
            -- A single paragraph is therefore a fixed point, the realistic case,
            -- since the parser's own image has ≤1 element (see 'activityRows').
            withWritten describedDb $
                parseBrightwayExcel defaultUnitConfig >=> \case
                    Left err -> expectationFailure (T.unpack err)
                    Right (acts, _, _, _, _) -> case acts of
                        [a] -> activityDescription a `shouldBe` ["a milling note"]
                        other -> expectationFailure ("expected one activity, got " <> show (length other))

    describe "export guard" $ do
        it "best-efforts an orphan waste exchange as technosphere instead of rejecting it" $ do
            -- Brightway has no waste type. An orphan waste exchange (no producer link)
            -- never enters the matrix, so the writer rewrites it as a technosphere flow
            -- (inventory-neutral) and records it in the manifest, rather than refusing.
            checkBrightwayExportable wasteDb `shouldBe` Right ()
            wasteManifest wasteDb `shouldSatisfy` (not . null)
            renderWorkbook defaultWriterConfig wasteDb `shouldSatisfy` isRight
        it "rejects a linked waste exchange, which would invert its sign on re-import" $ do
            -- A waste exchange that resolves to a producer contributes a negative
            -- coefficient to the matrix; written as technosphere it re-imports as a
            -- positive Input, inverting the inventory. The guard must reject it, and
            -- it is not best-efforted (so it never appears in the manifest).
            checkBrightwayExportable linkedWasteDb `shouldSatisfy` isLeft
            wasteManifest linkedWasteDb `shouldBe` []
        it "rejects a non-finite amount" $
            -- A non-finite amount has no numeric-cell form that re-parses; the
            -- guard rejects the database instead of emitting a misleading value.
            checkBrightwayExportable nonFiniteDb `shouldSatisfy` isLeft
        it "accepts a subnormal amount, which round-trips through the correctly-rounded reader" $
            -- 5e-324 (smallest positive subnormal) re-parses exactly through
            -- Amount.readAmount (Data.Text.Read.double used to lose it to 0), so it
            -- is faithfully representable; the guard must not reject it.
            checkBrightwayExportable subnormalDb `shouldSatisfy` isRight

    describe "ReferenceInput regression" $
        it "rejects a reference input rather than round-tripping it to a duplicated row" $ do
            -- Brightway has no marker for a reference input. Emitting it would
            -- re-parse to a synthetic reference product (from the meta row) PLUS an
            -- ordinary input (from the data row): two exchanges on one flow, whose
            -- duplicate (i,j) matrix entries sum and double the coefficient.
            let db =
                    refInputDb
                        { sdbActivities = M.singleton (generateActivityUUID refInputAct, tfId solventFlow) refInputAct
                        }
            checkBrightwayExportable db `shouldSatisfy` isLeft

-- ---------------------------------------------------------------------------
-- Fixture database, built with the parser's UUID conventions
-- ---------------------------------------------------------------------------

cfg :: WriterConfig
cfg = WriterConfig{wcDatabaseName = "Test_Inventory_DB"}

-- One activity producing electricity, consuming gas, emitting CO2.
fixtureDb :: SimpleDatabase
fixtureDb =
    SimpleDatabase
        { sdbActivities = M.singleton (generateActivityUUID elec, getReferenceProductUUID elec) elec
        , sdbTechFlows = M.fromList [(tfId f, f) | f <- [elecFlow, gasFlow]]
        , sdbBioFlows = M.fromList [(bfId co2, co2)]
        , sdbWasteFlows = M.empty
        , sdbUnits = M.fromList [(unitId u, u) | u <- [kwh, m3, kg]]
        }

elec :: Activity
elec =
    Activity
        { activityName = "Electricity production, natural gas"
        , activityDescription = []
        , activityDocumentation = []
        , activitySynonyms = M.empty
        , activityClassification = M.empty
        , activityLocation = "GLO"
        , activityLocationSource = LocationDeclared
        , activityUnit = "kilowatt hour"
        , exchanges = [prodExch, gasExch, co2Exch]
        , activityParams = M.empty
        , activityParamExprs = M.empty
        , activityNativeType = Nothing
        , activityNativeId = Nothing
        , activityFormulaCheck = Nothing
        }

prodExch :: Exchange
prodExch =
    TechnosphereExchange
        { techFlowId = generateFlowUUID "electricity, high voltage" ""
        , techAmount = 1
        , techUnitId = generateUnitUUID "kilowatt hour"
        , techRole = ReferenceProduct
        , techActivityLinkId = Nothing
        , techSupplierClaim = ClaimByProduct
        , techLocation = "GLO"
        , techComment = Nothing
        , techPedigree = Nothing
        , techShare = Nothing
        , techClassification = M.empty
        , techProperties = noProperties
        }

gasExch :: Exchange
gasExch =
    TechnosphereExchange
        { techFlowId = generateFlowUUID "natural gas, high pressure" ""
        , techAmount = 8.5
        , techUnitId = generateUnitUUID "cubic meter"
        , techRole = Input
        , techActivityLinkId = Nothing
        , techSupplierClaim = ClaimByProduct
        , techLocation = "RoW"
        , techComment = Nothing
        , techPedigree = Nothing
        , techShare = Nothing
        , techClassification = M.empty
        , techProperties = noProperties
        }

co2Exch :: Exchange
co2Exch =
    BiosphereExchange
        { bioFlowId = bfId co2
        , bioAmount = 0.5
        , bioUnitId = generateUnitUUID "kilogram"
        , bioDirection = Emission
        , bioLocation = ""
        , bioComment = Nothing
        , bioPedigree = Nothing
        }

elecFlow :: TechnosphereFlow
elecFlow =
    TechnosphereFlow
        (generateFlowUUID "electricity, high voltage" "")
        "electricity, high voltage"
        (generateUnitUUID "kilowatt hour")
        M.empty
        Nothing
        Nothing

gasFlow :: TechnosphereFlow
gasFlow =
    TechnosphereFlow
        (generateFlowUUID "natural gas, high pressure" "")
        "natural gas, high pressure"
        (generateUnitUUID "cubic meter")
        M.empty
        Nothing
        Nothing

-- ---------------------------------------------------------------------------
-- Special-character fixture (XML-special + non-ASCII names)
-- ---------------------------------------------------------------------------

{- | An activity whose name and every flow name carry the four entity-escaped
characters (@& < > "@) and non-ASCII (accents + CJK), to exercise the writer's
'escapeXml' / UTF-8 emit and the parser's entity decode end to end. Units stay
canonical ("kilogram") so unit normalization does not perturb the names.
-}
specialDb :: SimpleDatabase
specialDb =
    SimpleDatabase
        { sdbActivities = M.singleton (generateActivityUUID specialAct, getReferenceProductUUID specialAct) specialAct
        , sdbTechFlows = M.fromList [(tfId f, f) | f <- [specialProdFlow, specialInputFlow]]
        , sdbBioFlows = M.fromList [(bfId specialBio, specialBio)]
        , sdbWasteFlows = M.empty
        , sdbUnits = M.singleton (unitId kg) kg
        }

specialProductName, specialInputName, specialBioName :: Text
specialProductName = "Steel <profile> & \"bar\", éàü 鋼材"
specialInputName = "Coke & coal <feedstock> \"raw\", café"
specialBioName = "Carbon dioxide <CO₂> & \"fossil\""

specialAct :: Activity
specialAct =
    elec
        { activityName = "Steelmaking & forging <hot> \"strip\", München 製鋼"
        , activityUnit = "kilogram"
        , exchanges =
            [ TechnosphereExchange
                { techFlowId = tfId specialProdFlow
                , techAmount = 1
                , techUnitId = generateUnitUUID "kilogram"
                , techRole = ReferenceProduct
                , techActivityLinkId = Nothing
                , techSupplierClaim = ClaimByProduct
                , techLocation = "GLO"
                , techComment = Nothing
                , techPedigree = Nothing
                , techShare = Nothing
                , techClassification = M.empty
                , techProperties = noProperties
                }
            , TechnosphereExchange
                { techFlowId = tfId specialInputFlow
                , techAmount = 0.7
                , techUnitId = generateUnitUUID "kilogram"
                , techRole = Input
                , techActivityLinkId = Nothing
                , techSupplierClaim = ClaimByProduct
                , techLocation = "RoW"
                , techComment = Nothing
                , techPedigree = Nothing
                , techShare = Nothing
                , techClassification = M.empty
                , techProperties = noProperties
                }
            , BiosphereExchange
                { bioFlowId = bfId specialBio
                , bioAmount = 0.3
                , bioUnitId = generateUnitUUID "kilogram"
                , bioDirection = Emission
                , bioLocation = ""
                , bioComment = Nothing
                , bioPedigree = Nothing
                }
            ]
        }

specialProdFlow :: TechnosphereFlow
specialProdFlow =
    TechnosphereFlow
        (generateFlowUUID specialProductName "")
        specialProductName
        (generateUnitUUID "kilogram")
        M.empty
        Nothing
        Nothing

specialInputFlow :: TechnosphereFlow
specialInputFlow =
    TechnosphereFlow
        (generateFlowUUID specialInputName "")
        specialInputName
        (generateUnitUUID "kilogram")
        M.empty
        Nothing
        Nothing

specialBio :: BiosphereFlow
specialBio =
    co2
        { bfId = generateFlowUUID specialBioName "air"
        , bfName = specialBioName
        }

specialNormalized :: [NormActivity]
specialNormalized =
    normalizeParsed
        ( M.elems (sdbActivities specialDb)
        , sdbTechFlows specialDb
        , sdbBioFlows specialDb
        , sdbWasteFlows specialDb
        , sdbUnits specialDb
        )

-- Regression fixtures: a treatment-style activity whose reference is a
-- technosphere *input* (ReferenceInput), which must serialize as a single row.
solventFlow :: TechnosphereFlow
solventFlow =
    TechnosphereFlow
        (generateFlowUUID "spent solvent" "")
        "spent solvent"
        (generateUnitUUID "kilogram")
        M.empty
        Nothing
        Nothing

refInputAct :: Activity
refInputAct =
    elec
        { activityName = "Solvent treatment"
        , activityUnit = "kilogram"
        , exchanges =
            [ TechnosphereExchange
                { techFlowId = tfId solventFlow
                , techAmount = 1
                , techUnitId = generateUnitUUID "kilogram"
                , techRole = ReferenceInput
                , techActivityLinkId = Nothing
                , techSupplierClaim = ClaimByProduct
                , techLocation = "GLO"
                , techComment = Nothing
                , techPedigree = Nothing
                , techShare = Nothing
                , techClassification = M.empty
                , techProperties = noProperties
                }
            ]
        }

refInputDb :: SimpleDatabase
refInputDb =
    fixtureDb
        { sdbTechFlows = M.singleton (tfId solventFlow) solventFlow
        , sdbUnits = M.singleton (unitId kg) kg
        }

{- | The base fixture with a comment attached to its reference product, its
technosphere input, and its biosphere emission, to prove exchange-level comments
survive the round-trip, including on production rows ('productRowOut').
-}
commentDb :: SimpleDatabase
commentDb =
    fixtureDb{sdbActivities = M.singleton (generateActivityUUID a, getReferenceProductUUID a) a}
  where
    a = elec{exchanges = map withComment (exchanges elec)}
    withComment ex = case ex of
        TechnosphereExchange{techRole = ReferenceProduct} -> ex{techComment = Just "product note"}
        TechnosphereExchange{techRole = Input} -> ex{techComment = Just "input note"}
        TechnosphereExchange{} -> ex
        BiosphereExchange{} -> ex{bioComment = Just "emission note"}
        WasteExchange{} -> ex

{- | The base fixture whose technosphere input names its supplier activity, as a
Brightway workbook's @name@ column does when @reference product@ holds the
product.
-}
supplierNamedDb :: SimpleDatabase
supplierNamedDb =
    fixtureDb{sdbActivities = M.singleton (generateActivityUUID a, getReferenceProductUUID a) a}
  where
    a = elec{exchanges = map named (exchanges elec)}
    named ex = case ex of
        TechnosphereExchange{techRole = Input} -> ex{techSupplierClaim = ClaimByName "market for natural gas, high pressure"}
        TechnosphereExchange{} -> ex
        BiosphereExchange{} -> ex
        WasteExchange{} -> ex

-- | The base fixture carrying a single-paragraph activity-level description.
describedDb :: SimpleDatabase
describedDb =
    fixtureDb{sdbActivities = M.singleton (generateActivityUUID a, getReferenceProductUUID a) a}
  where
    a = elec{activityDescription = ["a milling note"]}

-- ---------------------------------------------------------------------------
-- Coproduct / empty / boundary-amount fixtures
-- ---------------------------------------------------------------------------

{- | The CHP reference output and its coproduct, both in canonical kilograms so
amounts are preserved verbatim across the round-trip.
-}
chpRefFlow, chpHeatFlow :: TechnosphereFlow
chpRefFlow = kgFlow "electricity, high voltage"
chpHeatFlow = kgFlow "recovered heat"

kgFlow :: Text -> TechnosphereFlow
kgFlow n =
    TechnosphereFlow
        (generateFlowUUID n "")
        n
        (generateUnitUUID "kilogram")
        M.empty
        Nothing
        Nothing

{- | An activity with a reference product and a coproduct, both emitted as
@production@ rows; the parser maps the first to 'ReferenceProduct' and the
second to 'Coproduct'.
-}
coproductAct :: Activity
coproductAct =
    elec
        { activityName = "Combined heat and power"
        , activityUnit = "kilogram"
        , exchanges =
            [ (kgProduction chpRefFlow ReferenceProduct 1){techComment = Just "ref note"}
            , (kgProduction chpHeatFlow Coproduct 0.25){techComment = Just "heat note"}
            ]
        }

kgProduction :: TechnosphereFlow -> TechRole -> Double -> Exchange
kgProduction flow role amount =
    TechnosphereExchange
        { techFlowId = tfId flow
        , techAmount = amount
        , techUnitId = generateUnitUUID "kilogram"
        , techRole = role
        , techActivityLinkId = Nothing
        , techSupplierClaim = ClaimByProduct
        , techLocation = "GLO"
        , techComment = Nothing
        , techPedigree = Nothing
        , techShare = Nothing
        , techClassification = M.empty
        , techProperties = noProperties
        }

coproductDb :: SimpleDatabase
coproductDb =
    SimpleDatabase
        { sdbActivities = M.singleton (generateActivityUUID coproductAct, getReferenceProductUUID coproductAct) coproductAct
        , sdbTechFlows = M.fromList [(tfId f, f) | f <- [chpRefFlow, chpHeatFlow]]
        , sdbBioFlows = M.empty
        , sdbWasteFlows = M.empty
        , sdbUnits = M.singleton (unitId kg) kg
        }

{- | The (role, amount) pairs of an activity's technosphere production rows,
in exchange order.
-}
roleAmounts :: Activity -> [(TechRole, Double)]
roleAmounts a = [(role, techAmount ex) | ex@TechnosphereExchange{techRole = role} <- exchanges a]

-- | A database with no activities, flows, or units.
emptyDb :: SimpleDatabase
emptyDb = SimpleDatabase M.empty M.empty M.empty M.empty M.empty

{- | A single input exchange whose amount sits at the 1e15 integer/scientific
boundary of 'formatAmount'.
-}
bigAmountDb :: SimpleDatabase
bigAmountDb =
    fixtureDb
        { sdbActivities = M.singleton (generateActivityUUID act, getReferenceProductUUID act) act
        }
  where
    act = elec{exchanges = [prodExch, gasExch{techAmount = 1.0e15}, co2Exch]}

-- ---------------------------------------------------------------------------
-- Reference-unit normalization fixture (non-canonical unit → canonical)
-- ---------------------------------------------------------------------------

{- | A unit config that knows grams as a non-canonical mass unit (factor 1e-3),
so the parser canonicalizes a @g@ reference product to @kg@ at ingest.
-}
gramConfig :: UnitConfig
gramConfig =
    either (error . T.unpack) id $
        buildFromCSV "name,dimension,factor\nkg,mass,1.0\ng,mass,1.0e-3\n"

gramUnit :: Unit
gramUnit = mkUnit "g"

gramProdFlow :: TechnosphereFlow
gramProdFlow = flowInUnit "milled flour" "g"

-- | A flow whose generated UUIDs key off the given unit string.
flowInUnit :: Text -> Text -> TechnosphereFlow
flowInUnit n u =
    TechnosphereFlow (generateFlowUUID n "") n (generateUnitUUID u) M.empty Nothing Nothing

{- | One activity whose reference product is stated in grams (1000 g). On import
under 'gramConfig' the parser rewrites it to 1 kg, so it is not a fixed point of
@parse . write@ until it has been through the parser once.
-}
gramRefDb :: SimpleDatabase
gramRefDb =
    SimpleDatabase
        { sdbActivities = M.singleton (generateActivityUUID act, getReferenceProductUUID act) act
        , sdbTechFlows = M.singleton (tfId gramProdFlow) gramProdFlow
        , sdbBioFlows = M.empty
        , sdbWasteFlows = M.empty
        , sdbUnits = M.singleton (unitId gramUnit) gramUnit
        }
  where
    act =
        elec
            { activityName = "Flour milling"
            , activityUnit = "g"
            , exchanges =
                [ TechnosphereExchange
                    { techFlowId = tfId gramProdFlow
                    , techAmount = 1000
                    , techUnitId = generateUnitUUID "g"
                    , techRole = ReferenceProduct
                    , techActivityLinkId = Nothing
                    , techSupplierClaim = ClaimByProduct
                    , techLocation = "GLO"
                    , techComment = Nothing
                    , techPedigree = Nothing
                    , techShare = Nothing
                    , techClassification = M.empty
                    , techProperties = noProperties
                    }
                ]
            }

-- | The (unit name, amount) of a parsed activity's reference exchange.
refUnitAmount :: Parsed -> Maybe (Text, Double)
refUnitAmount (acts, _tech, _bio, _waste, unitDB) = do
    a <- listToMaybe acts
    ex <- listToMaybe [e | e <- exchanges a, exchangeIsReference e]
    pure (maybe "" unitName (M.lookup (exchangeUnitId ex) unitDB), exchangeAmount ex)

-- ---------------------------------------------------------------------------
-- Export-guard fixtures (rejected at the boundary)
-- ---------------------------------------------------------------------------

{- | A database whose activity carries an /orphan/ waste exchange (B4): no
producer link, so it never enters the matrix. Brightway has no native waste type,
so the writer best-efforts it as a technosphere flow and records it in
'wasteManifest'.
-}
wasteDb :: SimpleDatabase
wasteDb =
    fixtureDb
        { sdbActivities = M.singleton (generateActivityUUID act, getReferenceProductUUID act) act
        , sdbWasteFlows = M.singleton (wfId scrapFlow) scrapFlow
        }
  where
    act = elec{exchanges = [prodExch, scrapExch]}

scrapFlow :: WasteFlow
scrapFlow =
    WasteFlow
        { wfId = generateFlowUUID "metal scrap" ""
        , wfName = "metal scrap"
        , wfUnitId = generateUnitUUID "kilogram"
        , wfSynonyms = M.empty
        , wfCAS = Nothing
        , wfSubstanceId = Nothing
        }

scrapExch :: Exchange
scrapExch =
    WasteExchange
        { waFlowId = wfId scrapFlow
        , waAmount = 0.1
        , waUnitId = generateUnitUUID "kilogram"
        , waIsInput = False
        , waActivityLinkId = Nothing
        , waSupplierClaim = ClaimByProduct
        , waLocation = ""
        , waComment = Nothing
        , waPedigree = Nothing
        }

{- | 'wasteDb' but the waste exchange links to a producer (a non-nil activity
link), so it would contribute a signed coefficient to the matrix. Brightway
cannot encode that without inverting the sign on re-import, so the guard rejects
it rather than best-efforting it.
-}
linkedWasteDb :: SimpleDatabase
linkedWasteDb =
    wasteDb
        { sdbActivities = M.singleton (generateActivityUUID act, getReferenceProductUUID act) act
        }
  where
    act = elec{exchanges = [prodExch, scrapExch{waActivityLinkId = Just (generateActivityUUID elec)}]}

{- | A database whose input exchange carries a non-finite amount: rejected, since
it has no numeric-cell form that re-parses to the same value.
-}
nonFiniteDb :: SimpleDatabase
nonFiniteDb =
    fixtureDb
        { sdbActivities = M.singleton (generateActivityUUID act, getReferenceProductUUID act) act
        }
  where
    act = elec{exchanges = [prodExch, gasExch{techAmount = 1 / 0}, co2Exch]}

{- | A database whose input exchange carries the smallest positive subnormal
(@5e-324@): rejected, since fixed-point rendering collapses it to @0@.
-}
subnormalDb :: SimpleDatabase
subnormalDb =
    fixtureDb
        { sdbActivities = M.singleton (generateActivityUUID act, getReferenceProductUUID act) act
        }
  where
    act = elec{exchanges = [prodExch, gasExch{techAmount = 5.0e-324}, co2Exch]}

co2 :: BiosphereFlow
co2 =
    BiosphereFlow
        { bfId = generateFlowUUID "Carbon dioxide, fossil" "air"
        , bfName = "Carbon dioxide, fossil"
        , bfUnitId = generateUnitUUID "kilogram"
        , bfSynonyms = M.empty
        , bfCAS = Nothing
        , bfSubstanceId = Nothing
        , bfCompartment = Just (Compartment Air Nothing)
        }

kwh, m3, kg :: Unit
kwh = mkUnit "kilowatt hour"
m3 = mkUnit "cubic meter"
kg = mkUnit "kilogram"

mkUnit :: Text -> Unit
mkUnit n = Unit (generateUnitUUID n) n n ""

-- ---------------------------------------------------------------------------
-- Structural comparison (order-insensitive)
-- ---------------------------------------------------------------------------

{- | A normalized, comparable projection of a parsed database: activity names
each with their reference product, sorted technosphere inputs, and sorted
biosphere flows (name, amount, compartment, direction). This captures the
semantic content the format can carry without depending on Map/Set ordering or
on fields the format does not represent (links, pedigree).
-}
data NormActivity = NormActivity
    { nName :: Text
    , nLocation :: Text
    , nRefProduct :: Maybe Text
    , nInputs :: [(Text, Double, Text)]
    , nBio :: [(Text, Double, Text, BioDirection)]
    }
    deriving (Eq, Show)

type Parsed = ([Activity], TechFlowDB, BioFlowDB, WasteFlowDB, UnitDB)

normalizeParsed :: Parsed -> [NormActivity]
normalizeParsed (acts, techDB, bioDB, _waste, unitDB) =
    sortOn nName (map norm acts)
  where
    norm a =
        NormActivity
            { nName = activityName a
            , nLocation = activityLocation a
            , nRefProduct = listTo [tfName f | ex <- exchanges a, exchangeIsReference ex, Just f <- [M.lookup (exchangeFlowId ex) techDB]]
            , nInputs =
                sortOn (\(n, _, _) -> n) $
                    [ (tfName f, techAmount ex, unitNameOf (techUnitId ex))
                    | ex@TechnosphereExchange{techRole = Input} <- exchanges a
                    , Just f <- [M.lookup (techFlowId ex) techDB]
                    ]
            , nBio =
                sortOn (\(n, _, _, _) -> n) $
                    [ (bfName f, bioAmount ex, bfCompartmentName f, bioDirection ex)
                    | ex@BiosphereExchange{} <- exchanges a
                    , Just f <- [M.lookup (bioFlowId ex) bioDB]
                    ]
            }
    unitNameOf uid = maybe "" unitName (M.lookup uid unitDB)
    listTo = \case
        (x : _) -> Just x
        [] -> Nothing

-- | The same projection over the original fixture database.
expectedNormalized :: [NormActivity]
expectedNormalized =
    normalizeParsed
        ( M.elems (sdbActivities fixtureDb)
        , sdbTechFlows fixtureDb
        , sdbBioFlows fixtureDb
        , sdbWasteFlows fixtureDb
        , sdbUnits fixtureDb
        )

-- ---------------------------------------------------------------------------
-- Logical-cell extraction (for idempotence)
-- ---------------------------------------------------------------------------

{- | Re-export the parsed 5-tuple to the writer's input shape. Activities are
keyed exactly as 'Database.Loader.loadBrightwayExcel' keys them.
-}
rebuild :: Parsed -> SimpleDatabase
rebuild (acts, techDB, bioDB, wasteDB, unitDB) =
    SimpleDatabase
        { sdbActivities = M.fromList [((generateActivityUUID a, getReferenceProductUUID a), a) | a <- acts]
        , sdbTechFlows = techDB
        , sdbBioFlows = bioDB
        , sdbWasteFlows = wasteDB
        , sdbUnits = unitDB
        }

{- | The deterministic logical cells the writer would emit for a database: the
per-activity 'activityRows' (which already carry every value the format records,
addressed by column). Comparing these is the logical-cell idempotence contract:
byte identity of the zip is deliberately not required.
-}
logicalCells :: SimpleDatabase -> [[[Cell]]]
logicalCells db =
    [ activityRows cfg db a
    | a <- sortOn (\x -> (activityName x, activityLocation x)) (M.elems (sdbActivities db))
    ]

-- ---------------------------------------------------------------------------
-- Scoring (c)
-- ---------------------------------------------------------------------------

-- | CO2 biosphere inventory for the electricity activity (ProcessId 0).
co2Inventory :: SimpleDatabase -> IO (Either Text (Maybe Double))
co2Inventory sdb = do
    built <-
        buildDatabaseWithMatrices (BuildInputs defaultUnitConfig mempty Declared) sdb
    case built of
        Left err -> pure (Left err)
        Right db -> do
            inv <- either (fail . show) pure =<< computeInventoryMatrix db 0
            pure (Right (lookupCo2 db inv))
  where
    lookupCo2 db inv =
        case find ((== "Carbon dioxide, fossil") . bfName) (M.elems (dbBioFlows db)) of
            Just f -> M.lookup (bfId f) inv
            Nothing -> Nothing

-- ---------------------------------------------------------------------------
-- File helper
-- ---------------------------------------------------------------------------

withWritten :: SimpleDatabase -> (FilePath -> IO a) -> IO a
withWritten db action =
    withSystemTempFile "brightway-writer.xlsx" $ \path h -> do
        bytes <- either (\e -> error ("renderWorkbook: " <> T.unpack e)) pure (renderWorkbook cfg db)
        BL.hPut h bytes
        hClose h
        action path
