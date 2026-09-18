{-# LANGUAGE OverloadedStrings #-}

{- | Tests for authoring activities into an editable database
('Database.Author.validateAuthored', 'Database.Edit.insertActivities' and
'Database.Edit.replaceActivities').

Authoring is the strict counterpart of importing. Where the loader warns and
drops a row it cannot resolve, authoring refuses, so most of what follows is
one red case per refusal, plus the two properties that make repeated authoring
safe: identity is a function of what was written (author twice, get the same
key), and writing then deleting leaves the activity set it started from.
-}
module AuthorSpec (spec) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text, isInfixOf)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Test.Hspec

import API.Types (ActivityForAPI (..), ExchangeWithUnit (..))
import Database (buildDatabaseWithMatrices)
import Database.Author (
    AuthorContext (..),
    AuthoredActivity (..),
    AuthoredExchange (..),
    EditedActivity (..),
    ExchangeEdit (..),
    ExchangeSelector (..),
    FlowRef (..),
    ResolvedInsert (..),
    applyExchangeEdits,
    authoredActivityUUID,
    authoredProductUUID,
    validateAuthored,
 )
import Database.Rebuild (deleteActivities, insertActivities, replaceActivities)
import Service (convertActivityForAPI)
import Types (
    Activity (..),
    AllocationKey (..),
    BioDirection (..),
    BiosphereFlow (..),
    BuildInputs (..),
    Compartment (..),
    Database (..),
    DeclaredShare (..),
    Exchange (..),
    LocationSource (..),
    Medium (..),
    Pedigree (..),
    SimpleDatabase (..),
    SparseTriple (..),
    SupplierClaim (..),
    TechRole (..),
    TechnosphereFlow (..),
    UUID,
    Unit (..),
    WasteFlow (..),
    WasteRole (..),
    activityReferenceShare,
    exchangeAmount,
    findProcessId,
    getActivity,
    isTechnosphereExchange,
    noProperties,
    processIdToText,
 )
import UnitConversion (UnitConfig, buildFromCSV, defaultUnitConfig)

spec :: Spec
spec = do
    fixtureDb <- runIO buildFixture
    let failsWith = refusalOf fixtureDb
    describe "Database.Author" $ do
        describe "validateAuthored (refusals)" $ do
            it "refuses an empty activity name" $
                failsWith "the activity name is empty" (baseActivity{aaName = "  "})

            it "refuses an empty product name" $
                failsWith "the product name is empty" (baseActivity{aaProductName = ""})

            it "refuses a zero product amount" $
                -- Zero output would divide the whole column by a zero normalization
                -- factor; it is a line that should not have been written.
                failsWith "finite non-zero" (baseActivity{aaProductAmount = 0})

            it "refuses a non-finite product amount" $
                failsWith "finite non-zero" (baseActivity{aaProductAmount = 0 / 0})

            it "refuses a product unit the database does not know" $
                failsWith "unknown unit \"furlong\"" (baseActivity{aaProductUnit = "furlong"})

            it "refuses a supplier that resolves to nothing" $
                -- The loader warns and drops this case; authoring will not, because
                -- a dropped input silently undercounts the activity's inventory.
                failsWith
                    "unknown provider"
                    (baseActivity{aaExchanges = [techInput "no-such-process" 1 Nothing]})

            it "refuses an input whose unit cannot reach the supplier's" $
                failsWith
                    "cannot convert \"m\" into the supplier's \"kg\""
                    (baseActivity{aaExchanges = [techInput supplierPid 1 (Just "m")]})

            it "refuses a waste output restated in a unit its treatment does not use" $
                -- The matrix never converts into a reference input, so a
                -- mismatched unit would land as a wrong raw number.
                failsWith
                    "amounts to a provider with no produced output are not converted"
                    (baseActivity{aaExchanges = [wasteOut treatPid 1 (Just "m")]})

            it "refuses a non-finite exchange amount" $
                failsWith
                    "finite non-zero"
                    (baseActivity{aaExchanges = [techInput supplierPid (1 / 0) Nothing]})

            it "refuses a biosphere flow identifier nothing declares" $
                failsWith
                    "no biosphere flow"
                    (baseActivity{aaExchanges = [bioOf (FlowById (mkUUID 999)) 1 Nothing]})

            it "refuses a biosphere amount restated in another unit" $
                -- The biosphere matrix carries amounts through unconverted, so a
                -- unit the flow does not use would land as a wrong number.
                failsWith
                    "biosphere amounts are not converted"
                    (baseActivity{aaExchanges = [bioOf (FlowById co2Id) 1 (Just "m")]})

            it "reports a bad product unit and a bad exchange together" $
                case validateAuthored (contextOf fixtureDb) [baseActivity{aaProductUnit = "furlong", aaExchanges = [techInput "nope" 1 Nothing]}] of
                    Right _ -> expectationFailure "expected both defects to be refused"
                    Left errs -> do
                        length errs `shouldBe` 2
                        errs `shouldSatisfy` any (isInfixOf "unknown unit \"furlong\"")
                        errs `shouldSatisfy` any (isInfixOf "unknown provider")

            it "reports every defect of a batch at once, each naming its activity" $ do
                let bad =
                        baseActivity
                            { aaName = "two-problems"
                            , aaExchanges = [techInput "nope" 1 Nothing, bioOf (FlowById (mkUUID 998)) 1 Nothing]
                            }
                case validateAuthored (contextOf fixtureDb) [bad, baseActivity{aaProductUnit = "furlong"}] of
                    Right _ -> expectationFailure "expected the batch to be refused"
                    Left errs -> do
                        length errs `shouldBe` 3
                        errs `shouldSatisfy` any (isInfixOf "two-problems {FR}: input from \"nope\": unknown provider")
                        errs `shouldSatisfy` any (isInfixOf ": no biosphere flow")
                        errs `shouldSatisfy` any (isInfixOf "unknown unit \"furlong\"")

            it "refuses a batch that mints one identity twice" $
                case validateAuthored (contextOf fixtureDb) [baseActivity, baseActivity] of
                    Right _ -> expectationFailure "expected the duplicate identity to be refused"
                    Left errs -> errs `shouldSatisfy` any (isInfixOf "mint the same identity")

        describe "validateAuthored (resolution)" $ do
            it "mints the same identity for the same description, twice" $ do
                a <- resolveOrFail fixtureDb baseActivity
                b <- resolveOrFail fixtureDb baseActivity
                riKey a `shouldBe` riKey b
                riKey a
                    `shouldBe` ( authoredActivityUUID "cheese, at dairy" "FR"
                               , authoredProductUUID "cheese" "kg"
                               )

            it "gives the same product two units two identities" $ do
                a <- resolveOrFail fixtureDb baseActivity
                b <- resolveOrFail fixtureDb baseActivity{aaProductUnit = "item"}
                snd (riKey a) `shouldNotBe` snd (riKey b)

            -- The unit table holds `L` and `l` as two rows, so the two spellings
            -- no longer meet on a key: what makes them one unit is the factor of
            -- 1 between them. `kilogram` against a database recording `kg` is
            -- the same case.
            it "takes a unit the database records under another spelling of it" $
                withSpellings $ \cfg -> do
                    let ctx = (contextOf fixtureDb){acUnitConfig = cfg}
                    case validateAuthored ctx [baseActivity{aaProductUnit = "kilogram", aaExchanges = [bioOf (FlowById co2Id) 1 (Just "kilogram")]}] of
                        Left errs -> expectationFailure ("expected acceptance, got " <> show errs)
                        Right ([r], _) -> do
                            activityUnit (riActivity r) `shouldBe` "kg"
                            snd (riKey r) `shouldBe` authoredProductUUID "cheese" "kg"
                        Right (rs, _) -> expectationFailure ("expected one insert, got " <> show (length rs))

            it "names the units a spelling could be when the database records two of them" $
                withSpellings $ \cfg -> do
                    let kilogramId = mkUUID 13
                        twoKilos = fixtureDb{dbUnits = M.insert kilogramId Unit{unitId = kilogramId, unitName = "kilogram", unitSymbol = "kilogram", unitComment = ""} (dbUnits fixtureDb)}
                        ctx = (contextOf twoKilos){acUnitConfig = cfg}
                    case validateAuthored ctx [baseActivity{aaProductUnit = "kilo"}] of
                        Right _ -> expectationFailure "expected a refusal naming both units"
                        Left errs -> errs `shouldSatisfy` any (isInfixOf "unit \"kilo\" could be \"kg\" or \"kilogram\"")

            it "links a local supplier by UUIDs, never by process id" $ do
                -- Process ids renumber on every rebuild; an embedded one would
                -- silently point at whichever row inherits the number.
                r <- resolveOrFail fixtureDb baseActivity{aaExchanges = [techInput supplierPid 2 Nothing]}
                case [ex | ex <- exchanges (riActivity r), isTechnosphereExchange ex, exchangeAmount ex == 2] of
                    [TechnosphereExchange{techActivityLinkId = Just link}] -> link `shouldBe` supplierActId
                    other -> expectationFailure ("expected one resolved input, got " <> show (length other))

            it "defaults a waste output to the treatment's reference unit" $ do
                -- A treatment has no produced unit to borrow; the omitted unit
                -- falls back to its reference input's.
                r <- resolveOrFail fixtureDb baseActivity{aaExchanges = [wasteOut treatPid 1 Nothing]}
                case [ex | ex@WasteExchange{} <- exchanges (riActivity r)] of
                    [WasteExchange{waActivityLinkId = Just link, waUnitId = unit}] -> do
                        link `shouldBe` treatActId
                        unit `shouldBe` kgUnitId
                    other -> expectationFailure ("expected one waste output, got " <> show (length other))

            it "leaves a dependency's supplier for cross-database relinking" $ do
                -- A supplier in another database has no process id here; the link
                -- carries the activity UUID and waits for the relink.
                depDb <- buildFixture
                let ctx = (contextOf fixtureDb){acDeps = [depDb]}
                    authored = baseActivity{aaName = "from-dep", aaExchanges = [techInput supplierPid 3 Nothing]}
                case validateAuthored ctx{acDb = emptyOf fixtureDb} [authored] of
                    Left errs -> expectationFailure ("expected resolution, got " <> show errs)
                    Right ([r], _) ->
                        case [ex | ex <- exchanges (riActivity r), exchangeAmount ex == 3] of
                            [TechnosphereExchange{techActivityLinkId = Just link}] -> link `shouldBe` supplierActId
                            other -> expectationFailure ("expected one dependency input, got " <> show (length other))
                    Right (rs, _) -> expectationFailure ("expected one insert, got " <> show (length rs))

            it "copies a dependency's product flow into the edited database" $ do
                -- Cross-database relinking reads the consumer's own flow table
                -- and drops in silence what is not in it, so an exchange on a
                -- flow only the dependency declares would score zero.
                depDb <- buildFixture
                let writing = (emptyOf fixtureDb){dbTechFlows = M.empty}
                    ctx = (contextOf writing){acDeps = [depDb]}
                    authored = baseActivity{aaExchanges = [techInput supplierPid 3 Nothing]}
                case validateAuthored ctx [authored] of
                    Left errs -> expectationFailure ("expected acceptance, got " <> show errs)
                    Right ([r], _) -> do
                        map tfName (riNewTechFlows r) `shouldSatisfy` elem "milk"
                        [tfUnitId f | f <- riNewTechFlows r, tfName f == "milk"] `shouldBe` [kgUnitId]
                    Right (rs, _) -> expectationFailure ("expected one insert, got " <> show (length rs))

            it "refuses an amount to a dependency stated in another unit than its product's" $ do
                -- The link into a dependency carries the flow's unit, not the
                -- exchange's, and the matrix converts the raw amount from it.
                -- A unit the local path would convert would land here as a
                -- number in the wrong one, which is the silent kind of wrong.
                built <- buildFixture
                let depDb =
                        built
                            { dbTechFlows =
                                M.adjust (\f -> f{tfUnitId = metreUnitId}) supplierProdId (dbTechFlows built)
                            }
                    writing = (emptyOf fixtureDb){dbTechFlows = M.empty}
                    ctx = (contextOf writing){acDeps = [depDb]}
                case validateAuthored ctx [baseActivity{aaExchanges = [techInput supplierPid 3 Nothing]}] of
                    Left errs ->
                        errs `shouldSatisfy` any (isInfixOf "is not converted, so restate it")
                    Right _ -> expectationFailure "expected a refusal on the unit the amount is stated in"

            it "takes a dependency's supplier in its product's own unit" $ do
                -- The ordinary case, and the one the refusal above must not
                -- catch: the flow and the reference exchange agree, so an
                -- omitted unit defaults to the right one.
                depDb <- buildFixture
                let writing = (emptyOf fixtureDb){dbTechFlows = M.empty}
                    ctx = (contextOf writing){acDeps = [depDb]}
                case validateAuthored ctx [baseActivity{aaExchanges = [techInput supplierPid 3 (Just "kg")]}] of
                    Left errs -> expectationFailure ("expected acceptance, got " <> show errs)
                    Right ([_], _) -> pure ()
                    Right (rs, _) -> expectationFailure ("expected one insert, got " <> show (length rs))

            it "refuses a dependency's supplier whose product unit is not here" $ do
                -- Same rule the biosphere side states: a flow copied in has to
                -- carry a unit this database can name, or the link would read
                -- its unit out of the wrong table. Two databases are two unit
                -- tables, so the dependency records its product in one the
                -- writing database does not have.
                built <- buildFixture
                let depDb =
                        built
                            { dbTechFlows =
                                M.adjust (\f -> f{tfUnitId = metreUnitId}) supplierProdId (dbTechFlows built)
                            }
                    writing =
                        (emptyOf fixtureDb)
                            { dbTechFlows = M.empty
                            , dbUnits = M.delete metreUnitId (dbUnits fixtureDb)
                            }
                    ctx = (contextOf writing){acDeps = [depDb]}
                case validateAuthored ctx [baseActivity{aaExchanges = [techInput supplierPid 3 Nothing]}] of
                    Left errs ->
                        errs `shouldSatisfy` any (isInfixOf "a unit this database does not have")
                    Right _ -> expectationFailure "expected a refusal on the copied flow's unit"

            it "warns about a biosphere flow new to the database without refusing it" $ do
                let authored = baseActivity{aaExchanges = [bioOf (FlowByName "Nitrous oxide" air "kg") 0.5 Nothing]}
                case validateAuthored (contextOf fixtureDb) [authored] of
                    Left errs -> expectationFailure ("expected acceptance, got " <> show errs)
                    Right ([r], warnings) -> do
                        map bfName (riNewBioFlows r) `shouldBe` ["Nitrous oxide"]
                        warnings `shouldSatisfy` any (isInfixOf "no characterization factor matches it")
                    Right (rs, _) -> expectationFailure ("expected one insert, got " <> show (length rs))

            it "copies a dependency's biosphere flow into the edited database" $ do
                -- Characterization resolves flows through the edited database's
                -- own vocabulary; a flow left only in the dependency would
                -- score zero in silence.
                bare <- buildBareFixture
                dep <- buildFixture
                let ctx = AuthorContext{acDb = bare, acDeps = [dep], acUnitConfig = defaultUnitConfig}
                case validateAuthored ctx [baseActivity{aaExchanges = [bioOf (FlowById co2Id) 1 Nothing]}] of
                    Left errs -> expectationFailure ("expected acceptance, got " <> show errs)
                    Right ([r], _) -> do
                        map bfName (riNewBioFlows r) `shouldBe` ["Carbon dioxide"]
                        map bfUnitId (riNewBioFlows r) `shouldBe` [kgUnitId]
                    Right (rs, _) -> expectationFailure ("expected one insert, got " <> show (length rs))

            it "reuses a biosphere flow the database already declares" $ do
                -- Same three coordinates the flow was minted on: re-declaring it is
                -- not a second flow.
                r <- resolveOrFail fixtureDb baseActivity{aaExchanges = [bioOf (FlowById co2Id) 1 Nothing]}
                map bfName (riNewBioFlows r) `shouldBe` []

            it "reads a flow named in words as the one the database already declares" $ do
                -- The name an inventory shows is what an author writes back;
                -- minting a twin of a curated flow would carry no
                -- characterization factor and score as zero beside it.
                r <- resolveOrFail fixtureDb baseActivity{aaExchanges = [bioOf (FlowByName "carbon dioxide" air "kg") 2 Nothing]}
                map bfName (riNewBioFlows r) `shouldBe` []
                bioFlowIds r `shouldBe` [co2Id]

            it "keeps a name in another compartment apart" $ do
                let urban = Compartment{compartmentName = Air, compartmentSub = Just "urban air"}
                r <- resolveOrFail fixtureDb baseActivity{aaExchanges = [bioOf (FlowByName "Carbon dioxide" urban "kg") 2 Nothing]}
                map bfName (riNewBioFlows r) `shouldBe` ["Carbon dioxide"]
                bioFlowIds r `shouldSatisfy` (/= [co2Id])

            it "refuses a named flow restated in another unit" $
                failsWith
                    "biosphere amounts are not converted"
                    (baseActivity{aaExchanges = [bioOf (FlowByName "Carbon dioxide" air "m") 1 Nothing]})

            it "tells two flows of one name apart by the unit the exchange states" $ do
                -- An energy carrier recorded in kg and in MJ is the real case:
                -- the author has already written which one they mean.
                r <- resolveOrFail (withTwinFlow fixtureDb) baseActivity{aaExchanges = [bioOf (FlowByName "Carbon dioxide" air "kg") 2 Nothing]}
                bioFlowIds r `shouldBe` [co2Id]

            it "refuses a name two flows answer to in the unit stated, naming their identifiers" $
                -- The refusal is where the author reads the identifier they
                -- then have to write, so it carries them.
                refusalOf
                    (withSameUnitFlow fixtureDb)
                    (UUID.toText sameUnitFlowId)
                    (baseActivity{aaExchanges = [bioOf (FlowByName "Carbon dioxide" air "kg") 1 Nothing]})

            it "names the units on offer when the stated one is none of them" $
                refusalOf
                    (withTwinFlow fixtureDb)
                    "restate it in one of them"
                    (baseActivity{aaExchanges = [bioOf (FlowByName "Carbon dioxide" air "item") 1 Nothing]})

        describe "insertActivities" $ do
            it "adds the activity, its product flow and its new biosphere flow together" $ do
                let authored =
                        baseActivity
                            { aaExchanges =
                                [ techInput supplierPid 2 Nothing
                                , bioOf (FlowByName "Nitrous oxide" air "kg") 0.5 Nothing
                                ]
                            }
                r <- resolveOrFail fixtureDb authored
                case insertActivities defaultUnitConfig [r] fixtureDb of
                    Left err -> expectationFailure ("insertActivities: " <> show err)
                    Right db' -> do
                        dbActivityCount db' `shouldBe` dbActivityCount fixtureDb + 1
                        M.member (snd (riKey r)) (dbTechFlows db') `shouldBe` True
                        map bfName (M.elems (dbBioFlows db')) `shouldSatisfy` elem "Nitrous oxide"
                        uncurry (findProcessId db') (riKey r) `shouldSatisfy` (/= Nothing)

            it "names the treatment a waste output was linked to" $ do
                -- A waste output with no target is what a final waste flow looks
                -- like. An authored one always has a treatment, so reading its
                -- link as no target would report every one of them as final.
                r <- resolveOrFail fixtureDb baseActivity{aaExchanges = [wasteOut treatPid 0.5 Nothing]}
                case insertedWasteOutput fixtureDb r of
                    Left err -> expectationFailure (T.unpack err)
                    Right (ewu, treatment) -> do
                        ewuFlowName ewu `shouldBe` "used oil"
                        ewuTargetProcessId ewu `shouldBe` Just treatment
                        ewuTargetActivityName ewu `shouldBe` Just "waste oil incineration"
                        ewuWasteRole ewu `shouldBe` Just SentToTreatment

            it "names the treatment's own row, not another product of the same activity" $ do
                -- The matrix routes the waste on the pair (activity, flow); an
                -- activity that answers to two process ids would otherwise be
                -- reported at whichever product the UUID index happens to hold,
                -- naming a row the score never charged.
                db <- buildTwoProductTreatment
                r <- resolveOrFail db baseActivity{aaExchanges = [wasteOut treatPid 0.5 Nothing]}
                case insertedWasteOutput db r of
                    Left err -> expectationFailure (T.unpack err)
                    Right (ewu, treatment) -> ewuTargetProcessId ewu `shouldBe` Just treatment

            it "refuses a key the database already holds" $ do
                r <- resolveOrFail fixtureDb baseActivity
                case insertActivities defaultUnitConfig [r] fixtureDb >>= insertActivities defaultUnitConfig [r] of
                    Left err -> err `shouldSatisfy` isInfixOf "already exists"
                    Right _ -> expectationFailure "expected the second insert to be refused"

            it "leaves the database untouched on an empty batch" $
                -- A rebuild would clear cross-database links; "nothing to write"
                -- must not silently unlink the database.
                case insertActivities defaultUnitConfig [] fixtureDb{dbDependsOn = ["other"]} of
                    Left err -> expectationFailure ("expected a no-op, got " <> show err)
                    Right db' -> dbDependsOn db' `shouldBe` ["other"]

            it "keeps the supplier linked when insertion renumbers the rows" $ do
                -- An authored key sorting before the supplier's shifts every
                -- process id at rebuild; the input must still reach the
                -- supplier's row, so links carry UUIDs and never a row number.
                highDb <- buildFixtureAt highActId highProdId
                let pidText = UUID.toText highActId <> "_" <> UUID.toText highProdId
                r <- resolveOrFail highDb baseActivity{aaExchanges = [techInput pidText 2 Nothing]}
                riKey r < (highActId, highProdId) `shouldBe` True
                case insertActivities defaultUnitConfig [r] highDb of
                    Left err -> expectationFailure ("insertActivities: " <> show err)
                    Right db' ->
                        case (uncurry (findProcessId db') (riKey r), findProcessId db' highActId highProdId) of
                            (Just col, Just row) ->
                                [ v
                                | SparseTriple i j v <- VU.toList (dbTechnosphereTriples db')
                                , i == row
                                , j == col
                                ]
                                    `shouldBe` [2]
                            other -> expectationFailure ("missing process ids after insert: " <> show other)

            it "restores the original activity set when the insert is deleted again" $ do
                r <- resolveOrFail fixtureDb baseActivity
                case insertActivities defaultUnitConfig [r] fixtureDb of
                    Left err -> expectationFailure ("insertActivities: " <> show err)
                    Right db' -> case uncurry (findProcessId db') (riKey r) of
                        Nothing -> expectationFailure "the inserted activity has no process id"
                        Just pid -> case deleteActivities [pid] db' of
                            Left err -> expectationFailure ("deleteActivities: " <> show err)
                            Right db'' -> do
                                processKeys db'' `shouldBe` processKeys fixtureDb
                                dbActivityCount db'' `shouldBe` dbActivityCount fixtureDb

        describe "replaceActivities" $ do
            it "refuses a key the database does not hold" $ do
                r <- resolveOrFail fixtureDb baseActivity
                case replaceActivities defaultUnitConfig [r] fixtureDb of
                    Left err -> err `shouldSatisfy` isInfixOf "does not exist"
                    Right _ -> expectationFailure "expected the replace to be refused"

            it "rewrites the activity in place, keeping its identity" $ do
                first <- resolveOrFail fixtureDb baseActivity{aaExchanges = [techInput supplierPid 2 Nothing]}
                case insertActivities defaultUnitConfig [first] fixtureDb of
                    Left err -> expectationFailure ("insertActivities: " <> show err)
                    Right db' -> do
                        second <- resolveOrFail db' baseActivity{aaExchanges = [techInput supplierPid 7 Nothing]}
                        riKey second `shouldBe` riKey first
                        case replaceActivities defaultUnitConfig [second] db' of
                            Left err -> expectationFailure ("replaceActivities: " <> show err)
                            Right db'' -> do
                                dbActivityCount db'' `shouldBe` dbActivityCount db'
                                inputAmounts db'' (fst (riKey second)) `shouldBe` [7]

        -- Editing an imported activity is the operation authoring cannot do:
        -- the row keeps its identity and everything a description would drop.
        describe "applyExchangeEdits" $ do
            let editOrFail edits = case applyExchangeEdits (contextOf fixtureDb) edits importedActivity of
                    Left errs -> fail ("applyExchangeEdits: " <> show errs)
                    Right edited -> pure edited
                editRefused needle edits =
                    case applyExchangeEdits (contextOf fixtureDb) edits importedActivity of
                        Right _ -> expectationFailure ("expected a refusal mentioning " <> show needle)
                        Left errs -> errs `shouldSatisfy` any (isInfixOf needle)
                inputsOf act = [ex | ex@TechnosphereExchange{techRole = Input} <- exchanges act]

            it "keeps every field an edit does not name" $ do
                -- The reason this operation exists: classification, synonyms,
                -- parameters and allocation survive an edit, where
                -- re-describing the activity would silently drop them.
                edited <-
                    editOrFail
                        [ RemoveExchange (SelectBiosphere co2Id)
                        , SetAmount (SelectInput supplierPid) 3
                        , AddExchange (wasteOut treatPid 0.1 Nothing)
                        ]
                activityFacts (eaActivity edited) `shouldBe` activityFacts importedActivity

            it "keeps the pedigree of the lines it leaves alone" $ do
                edited <- editOrFail [RemoveExchange (SelectBiosphere co2Id)]
                map techPedigree (inputsOf (eaActivity edited)) `shouldBe` [Just milkPedigree]

            it "never addresses the product side" $ do
                -- Both products link to the activity itself, so a selector can
                -- name their key; neither is an input, so neither is reachable.
                editRefused "matches no exchange" [RemoveExchange (SelectInput importedPid)]
                editRefused "matches no exchange" [RemoveExchange (SelectInput coproductPid)]

            it "leaves a waste input where it is" $ do
                -- Same provider key as the waste output, opposite direction:
                -- one line matches, the other is not an edit's business.
                edited <- editOrFail [RemoveExchange (SelectWaste treatPid)]
                eaMatched edited `shouldBe` [1]
                [waAmount ex | ex@WasteExchange{waIsInput = True} <- exchanges (eaActivity edited)]
                    `shouldBe` [0.2]

            it "applies one selector to every line it matches, and says how many" $ do
                -- Two emissions of one substance, in two compartments.
                edited <- editOrFail [RemoveExchange (SelectBiosphere co2Id)]
                eaMatched edited `shouldBe` [2]
                length (exchanges (eaActivity edited)) `shouldBe` length (exchanges importedActivity) - 2

            it "addresses a provider by bare activity UUID" $ do
                edited <- editOrFail [RemoveExchange (SelectInput (UUID.toText supplierActId))]
                eaMatched edited `shouldBe` [1]

            it "sets an amount and leaves the rest of the line alone" $ do
                edited <- editOrFail [SetAmount (SelectInput supplierPid) 3]
                map (\ex -> (techAmount ex, techComment ex, techPedigree ex)) (inputsOf (eaActivity edited))
                    `shouldBe` [(3, Just "milk in", Just milkPedigree)]

            it "adds a line the way authoring resolves one" $ do
                edited <- editOrFail [AddExchange (bioOf (FlowByName "Nitrous oxide" air "kg") 0.5 Nothing)]
                eaMatched edited `shouldBe` [1]
                map bfName (eaNewBioFlows edited) `shouldBe` ["Nitrous oxide"]
                eaWarnings edited `shouldSatisfy` any (isInfixOf "no characterization factor matches it")

            it "brings along the product flow an added dependency line needs" $ do
                -- The technosphere twin of the line above: an added input whose
                -- supplier lives in a dependency has to bring that supplier's
                -- product flow with it, or the relink drops the line.
                depDb <- buildFixture
                let writing = (emptyOf fixtureDb){dbTechFlows = M.empty}
                    ctx = (contextOf writing){acDeps = [depDb]}
                case applyExchangeEdits ctx [AddExchange (techInput supplierPid 2 Nothing)] importedActivity of
                    Left errs -> expectationFailure ("expected acceptance, got " <> show errs)
                    Right edited -> map tfName (eaNewTechFlows edited) `shouldBe` ["milk"]

            it "refuses a selector that matches nothing" $
                editRefused "matches no exchange" [RemoveExchange (SelectBiosphere (mkUUID 998))]

            it "refuses a provider that is not a process id" $
                -- Otherwise "matches nothing" would send the author looking for
                -- a missing line instead of a mistyped identifier.
                editRefused "is not a process id" [RemoveExchange (SelectInput "milk production")]

            it "refuses a set on the line the remove before it took away" $
                -- Edits apply in order; reordering them into something that
                -- works would not be the edit that was asked for.
                editRefused
                    "matches no exchange"
                    [RemoveExchange (SelectBiosphere co2Id), SetAmount (SelectBiosphere co2Id) 1]

            it "reports every defect of an edit list at once" $
                case applyExchangeEdits
                    (contextOf fixtureDb)
                    [ RemoveExchange (SelectBiosphere (mkUUID 998))
                    , SetAmount (SelectInput supplierPid) (0 / 0)
                    , AddExchange (techInput "nope" 1 Nothing)
                    ]
                    importedActivity of
                    Right _ -> expectationFailure "expected the edits to be refused"
                    Left errs -> do
                        length errs `shouldBe` 3
                        errs `shouldSatisfy` any (isInfixOf "matches no exchange")
                        errs `shouldSatisfy` any (isInfixOf "finite non-zero")
                        errs `shouldSatisfy` any (isInfixOf "unknown provider")

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | "Authoring this against that database is refused, and says why."
refusalOf :: Database -> Text -> AuthoredActivity -> Expectation
refusalOf db needle authored =
    case validateAuthored (contextOf db) [authored] of
        Right _ -> expectationFailure ("expected a refusal mentioning " <> show needle)
        Left errs -> errs `shouldSatisfy` any (isInfixOf needle)

resolveOrFail :: Database -> AuthoredActivity -> IO ResolvedInsert
resolveOrFail db authored = case validateAuthored (contextOf db) [authored] of
    Left errs -> fail ("validateAuthored: " <> show errs)
    Right ([r], _) -> pure r
    Right (rs, _) -> fail ("expected one insert, got " <> show (length rs))

contextOf :: Database -> AuthorContext
contextOf db = AuthorContext{acDb = db, acDeps = [], acUnitConfig = defaultUnitConfig}

-- | A unit table holding three spellings of the kilogram, each a row of its own.
withSpellings :: (UnitConfig -> Expectation) -> Expectation
withSpellings check =
    either
        (expectationFailure . T.unpack)
        check
        (buildFromCSV "name,dimension,factor\nkg,mass,1.0\nkilogram,mass,1.0\nkilo,mass,1.0\n")

-- | The same database with no activities, used to prove a dependency link resolves.
emptyOf :: Database -> Database
emptyOf db = db{dbActivities = V.empty, dbProcessIdTable = V.empty, dbProcessIdLookup = M.empty}

processKeys :: Database -> S.Set (UUID, UUID)
processKeys = S.fromList . V.toList . dbProcessIdTable

-- | Amounts of the non-reference technosphere inputs of one activity.
inputAmounts :: Database -> UUID -> [Double]
inputAmounts db actUUID =
    [ exchangeAmount ex
    | (key, act) <- zip (V.toList (dbProcessIdTable db)) (V.toList (dbActivities db))
    , fst key == actUUID
    , ex@TechnosphereExchange{techRole = Input} <- exchanges act
    ]

baseActivity :: AuthoredActivity
baseActivity =
    AuthoredActivity
        { aaName = "cheese, at dairy"
        , aaLocation = "FR"
        , aaDescription = ["An authored activity."]
        , aaProductName = "cheese"
        , aaProductAmount = 1.0
        , aaProductUnit = "kg"
        , aaExchanges = []
        }

techInput :: Text -> Double -> Maybe Text -> AuthoredExchange
techInput provider amount unit =
    AuthoredTechInput{atiProvider = provider, atiAmount = amount, atiUnit = unit, atiComment = Nothing}

wasteOut :: Text -> Double -> Maybe Text -> AuthoredExchange
wasteOut provider amount unit =
    AuthoredWasteOutput{awProvider = provider, awAmount = amount, awUnit = unit, awComment = Nothing}

treatPid :: Text
treatPid = UUID.toText treatActId <> "_" <> UUID.toText usedOilId

{- | Insert one resolved activity and read back its single waste-output line as
the activity view describes it, next to the process id its treatment sits at.
-}
insertedWasteOutput :: Database -> ResolvedInsert -> Either Text (ExchangeWithUnit, Text)
insertedWasteOutput db r = do
    db' <- insertActivities defaultUnitConfig [r] db
    pid <- note "the authored activity has no process id" (uncurry (findProcessId db') (riKey r))
    act <- note "the inserted activity is not in the database" (getActivity db' pid)
    treatment <- note "the treatment has no process id" (findProcessId db' treatActId usedOilId)
    case [ewu | ewu <- pfaExchanges (convertActivityForAPI db' pid act), WasteExchange{waIsInput = False} <- [ewuExchange ewu]] of
        [ewu] -> Right (ewu, processIdToText db' treatment)
        other -> Left ("expected one waste output, got " <> T.pack (show (length other)))
  where
    note :: Text -> Maybe a -> Either Text a
    note msg = maybe (Left msg) Right

-- | The biosphere flows one resolved activity points at, in exchange order.
bioFlowIds :: ResolvedInsert -> [UUID]
bioFlowIds r = [flowId | BiosphereExchange{bioFlowId = flowId} <- exchanges (riActivity r)]

-- | The same database with a second "Carbon dioxide" in air, recorded in another unit.
withTwinFlow :: Database -> Database
withTwinFlow db =
    db{dbBioFlows = M.insert twinFlowId co2Flow{bfId = twinFlowId, bfUnitId = metreUnitId} (dbBioFlows db)}

{- | The same database with a second "Carbon dioxide" in air in the very same
unit - two rows a file left behind, which no stated unit can tell apart.
-}
withSameUnitFlow :: Database -> Database
withSameUnitFlow db =
    db{dbBioFlows = M.insert sameUnitFlowId co2Flow{bfId = sameUnitFlowId} (dbBioFlows db)}

twinFlowId, sameUnitFlowId :: UUID
twinFlowId = mkUUID 6
sameUnitFlowId = mkUUID 7

bioOf :: FlowRef -> Double -> Maybe Text -> AuthoredExchange
bioOf flow amount unit =
    AuthoredBio
        { abFlow = flow
        , abDirection = Emission
        , abAmount = amount
        , abUnit = unit
        , abComment = Nothing
        }

air :: Compartment
air = Compartment{compartmentName = Air, compartmentSub = Nothing}

-- ---------------------------------------------------------------------------
-- Fixture: an activity as a database file would have brought it in
-- ---------------------------------------------------------------------------

{- | Everything about an activity except its inventory. An edit must carry all
of it through untouched, and it is exactly what a re-description would lose,
since an authored activity has none of it to state.
-}
type ActivityFacts =
    ( (Text, Text, [Text], Text)
    , (M.Map Text Text, M.Map Text (S.Set Text))
    , (M.Map Text Double, M.Map Text Text)
    , Maybe DeclaredShare
    )

activityFacts :: Activity -> ActivityFacts
activityFacts act =
    ( (activityName act, activityLocation act, activityDescription act, activityUnit act)
    , (activityClassification act, activitySynonyms act)
    , (activityParams act, activityParamExprs act)
    , activityReferenceShare act
    )

importedActId, importedProdId, coproductId :: UUID
importedActId = mkUUID 20
importedProdId = mkUUID 21
coproductId = mkUUID 22

importedPid, coproductPid :: Text
importedPid = UUID.toText importedActId <> "_" <> UUID.toText importedProdId
coproductPid = UUID.toText importedActId <> "_" <> UUID.toText coproductId

milkPedigree :: Pedigree
milkPedigree =
    Pedigree
        { pedReliability = 2
        , pedCompleteness = 3
        , pedTemporal = 4
        , pedGeographical = 5
        , pedTechnological = 1
        }

{- | An imported activity, with everything on it that authoring cannot express:
a coproduct, a classification, synonyms, parameters, an allocation, a pedigree.
Its inventory holds one line of every addressable kind, plus two emissions of
one substance (a selector matching more than one line) and a waste input (a
line no selector reaches).
-}
importedActivity :: Activity
importedActivity =
    Activity
        { activityName = "cheese production"
        , activityDescription = ["Imported from a database file, never authored."]
        , activityDocumentation = []
        , activitySynonyms = M.singleton "en" (S.fromList ["cheese making"])
        , activityClassification = M.singleton "ISIC rev.4" "1050:Manufacture of dairy products"
        , activityLocation = "FR"
        , activityLocationSource = LocationDeclared
        , activityUnit = "kg"
        , exchanges =
            [ TechnosphereExchange
                { techFlowId = importedProdId
                , techAmount = 1.0
                , techUnitId = kgUnitId
                , techRole = ReferenceProduct
                , techActivityLinkId = Just importedActId
                , techSupplierClaim = ClaimByProduct
                , techLocation = ""
                , techComment = Nothing
                , techPedigree = Nothing
                , techShare = Nothing
                , techClassification = M.empty
                , techProperties = noProperties
                }
            , TechnosphereExchange
                { techFlowId = coproductId
                , techAmount = 0.3
                , techUnitId = kgUnitId
                , techRole = Coproduct
                , techActivityLinkId = Just importedActId
                , techSupplierClaim = ClaimByProduct
                , techLocation = ""
                , techComment = Nothing
                , techPedigree = Nothing
                , techShare = Nothing
                , techClassification = M.empty
                , techProperties = noProperties
                }
            , TechnosphereExchange
                { techFlowId = supplierProdId
                , techAmount = 8.0
                , techUnitId = kgUnitId
                , techRole = Input
                , techActivityLinkId = Just supplierActId
                , techSupplierClaim = ClaimByProduct
                , techLocation = "FR"
                , techComment = Just "milk in"
                , techPedigree = Just milkPedigree
                , techShare = Nothing
                , techClassification = M.empty
                , techProperties = noProperties
                }
            , BiosphereExchange
                { bioFlowId = co2Id
                , bioAmount = 1.2
                , bioUnitId = kgUnitId
                , bioDirection = Emission
                , bioLocation = ""
                , bioComment = Nothing
                , bioPedigree = Nothing
                }
            , BiosphereExchange
                { bioFlowId = co2Id
                , bioAmount = 0.4
                , bioUnitId = kgUnitId
                , bioDirection = Emission
                , bioLocation = "FR"
                , bioComment = Just "second compartment"
                , bioPedigree = Nothing
                }
            , WasteExchange
                { waFlowId = usedOilId
                , waAmount = 0.5
                , waUnitId = kgUnitId
                , waIsInput = False
                , waActivityLinkId = Just treatActId
                , waSupplierClaim = ClaimByProduct
                , waLocation = ""
                , waComment = Nothing
                , waPedigree = Nothing
                }
            , WasteExchange
                { waFlowId = usedOilId
                , waAmount = 0.2
                , waUnitId = kgUnitId
                , waIsInput = True
                , waActivityLinkId = Just treatActId
                , waSupplierClaim = ClaimByProduct
                , waLocation = ""
                , waComment = Nothing
                , waPedigree = Nothing
                }
            ]
        , activityParams = M.singleton "yield" 0.85
        , activityParamExprs = M.singleton "yield" "0.9 * 0.94"
        , activityNativeType = Nothing
        , activityNativeId = Nothing
        , activityFormulaCheck = Nothing
        }

-- ---------------------------------------------------------------------------
-- Fixture: one supplier producing "milk" in kg, emitting CO2
-- ---------------------------------------------------------------------------

buildFixture :: IO Database
buildFixture = buildFixtureAt supplierActId supplierProdId

{- | The same fixture with the supplier under chosen UUIDs, high ones sort
after any authored key, which forces a renumbering on insert.
-}
buildFixtureAt :: UUID -> UUID -> IO Database
buildFixtureAt actId prodId = do
    r <-
        buildDatabaseWithMatrices
            (BuildInputs defaultUnitConfig mempty Declared [])
            SimpleDatabase
                { sdbActivities =
                    M.fromList
                        [ ((actId, prodId), supplierActivityAt actId prodId)
                        , ((treatActId, usedOilId), treatmentActivity)
                        ]
                , sdbTechFlows =
                    M.fromList
                        [ (prodId, milkFlowAt prodId)
                        , (usedOilId, usedOilFlow)
                        ]
                , sdbBioFlows = M.singleton co2Id co2Flow
                , sdbWasteFlows = M.singleton usedOilId usedOilWasteFlow
                , sdbUnits = unitTable
                }
    either (fail . show) pure r

{- | The same fixture with a second product on the treatment, so its activity
UUID answers to two process ids. Which of them a waste output resolves to is
then a real question, and only the pair the matrix routes on answers it.
-}
buildTwoProductTreatment :: IO Database
buildTwoProductTreatment = do
    r <-
        buildDatabaseWithMatrices
            (BuildInputs defaultUnitConfig mempty Declared [])
            SimpleDatabase
                { sdbActivities =
                    M.fromList
                        [ ((supplierActId, supplierProdId), supplierActivityAt supplierActId supplierProdId)
                        , ((treatActId, usedOilId), treatmentWithHeat)
                        , ((treatActId, heatId), treatmentWithHeat)
                        ]
                , sdbTechFlows =
                    M.fromList
                        [ (supplierProdId, milkFlowAt supplierProdId)
                        , (usedOilId, usedOilFlow)
                        , (heatId, heatFlow)
                        ]
                , sdbBioFlows = M.singleton co2Id co2Flow
                , sdbWasteFlows = M.singleton usedOilId usedOilWasteFlow
                , sdbUnits = unitTable
                }
    either (fail . show) pure r

{- | A database that declares no biosphere flow at all, bio vocabulary can
only come from a dependency.
-}
buildBareFixture :: IO Database
buildBareFixture = do
    let act = supplierActivityAt supplierActId supplierProdId
        noBio = act{exchanges = filter isTechnosphereExchange (exchanges act)}
    r <-
        buildDatabaseWithMatrices
            (BuildInputs defaultUnitConfig mempty Declared [])
            SimpleDatabase
                { sdbActivities = M.singleton (supplierActId, supplierProdId) noBio
                , sdbTechFlows = M.singleton supplierProdId (milkFlowAt supplierProdId)
                , sdbBioFlows = M.empty
                , sdbWasteFlows = M.empty
                , sdbUnits = unitTable
                }
    either (fail . show) pure r

mkUUID :: Int -> UUID
mkUUID n = UUID.fromWords64 (fromIntegral n) 0

supplierActId, supplierProdId, co2Id, treatActId, usedOilId, heatId, highActId, highProdId, kgUnitId, itemUnitId, metreUnitId :: UUID
supplierActId = mkUUID 1
supplierProdId = mkUUID 2
co2Id = mkUUID 3
treatActId = mkUUID 4
usedOilId = mkUUID 5
heatId = mkUUID 30
highActId = UUID.fromWords64 maxBound 1
highProdId = UUID.fromWords64 maxBound 2
kgUnitId = mkUUID 10
itemUnitId = mkUUID 11
metreUnitId = mkUUID 12

supplierPid :: Text
supplierPid = UUID.toText supplierActId <> "_" <> UUID.toText supplierProdId

unitTable :: M.Map UUID Unit
unitTable =
    M.fromList
        [ (kgUnitId, Unit{unitId = kgUnitId, unitName = "kg", unitSymbol = "kg", unitComment = ""})
        , (itemUnitId, Unit{unitId = itemUnitId, unitName = "item", unitSymbol = "item", unitComment = ""})
        , (metreUnitId, Unit{unitId = metreUnitId, unitName = "m", unitSymbol = "m", unitComment = ""})
        ]

milkFlowAt :: UUID -> TechnosphereFlow
milkFlowAt prodId =
    TechnosphereFlow
        { tfId = prodId
        , tfName = "milk"
        , tfUnitId = kgUnitId
        , tfSynonyms = M.empty
        , tfCAS = Nothing
        , tfSubstanceId = Nothing
        }

co2Flow :: BiosphereFlow
co2Flow =
    BiosphereFlow
        { bfId = co2Id
        , bfName = "Carbon dioxide"
        , bfUnitId = kgUnitId
        , bfSynonyms = M.empty
        , bfCAS = Just "124-38-9"
        , bfSubstanceId = Nothing
        , bfCompartment = Just air
        }

{- | The same waste declared on the waste axis. A treatment's reference stays a
technosphere product while the lines that send waste to it sit on the waste
axis, so an import declares the flow in both tables.
-}
usedOilWasteFlow :: WasteFlow
usedOilWasteFlow =
    WasteFlow
        { wfId = usedOilId
        , wfName = "used oil"
        , wfUnitId = kgUnitId
        , wfSynonyms = M.empty
        , wfCAS = Nothing
        , wfSubstanceId = Nothing
        }

-- | The heat a treatment recovers, its second product.
heatFlow :: TechnosphereFlow
heatFlow =
    TechnosphereFlow
        { tfId = heatId
        , tfName = "heat, recovered"
        , tfUnitId = kgUnitId
        , tfSynonyms = M.empty
        , tfCAS = Nothing
        , tfSubstanceId = Nothing
        }

usedOilFlow :: TechnosphereFlow
usedOilFlow =
    TechnosphereFlow
        { tfId = usedOilId
        , tfName = "used oil"
        , tfUnitId = kgUnitId
        , tfSynonyms = M.empty
        , tfCAS = Nothing
        , tfSubstanceId = Nothing
        }

{- | The same treatment, recovering heat as a second product. Its activity UUID
then answers to two process ids, only one of which treats the waste.
-}
treatmentWithHeat :: Activity
treatmentWithHeat =
    treatmentActivity
        { exchanges =
            exchanges treatmentActivity
                ++ [ TechnosphereExchange
                        { techFlowId = heatId
                        , techAmount = 3.0
                        , techUnitId = kgUnitId
                        , techRole = Coproduct
                        , techActivityLinkId = Nothing
                        , techSupplierClaim = ClaimByProduct
                        , techLocation = ""
                        , techComment = Nothing
                        , techPedigree = Nothing
                        , techShare = Nothing
                        , techClassification = M.empty
                        , techProperties = noProperties
                        }
                   ]
        }

{- | A treatment process: its only reference is an input, so it has no
produced unit for the matrix to convert into.
-}
treatmentActivity :: Activity
treatmentActivity =
    Activity
        { activityName = "waste oil incineration"
        , activityDescription = []
        , activityDocumentation = []
        , activitySynonyms = M.empty
        , activityClassification = M.empty
        , activityLocation = "FR"
        , activityLocationSource = LocationDeclared
        , activityUnit = "kg"
        , exchanges =
            [ TechnosphereExchange
                { techFlowId = usedOilId
                , techAmount = 1.0
                , techUnitId = kgUnitId
                , techRole = ReferenceInput
                , techActivityLinkId = Just treatActId
                , techSupplierClaim = ClaimByProduct
                , techLocation = ""
                , techComment = Nothing
                , techPedigree = Nothing
                , techShare = Nothing
                , techClassification = M.empty
                , techProperties = noProperties
                }
            ]
        , activityParams = M.empty
        , activityParamExprs = M.empty
        , activityNativeType = Nothing
        , activityNativeId = Nothing
        , activityFormulaCheck = Nothing
        }

supplierActivityAt :: UUID -> UUID -> Activity
supplierActivityAt actId prodId =
    Activity
        { activityName = "milk production"
        , activityDescription = []
        , activityDocumentation = []
        , activitySynonyms = M.empty
        , activityClassification = M.empty
        , activityLocation = "FR"
        , activityLocationSource = LocationDeclared
        , activityUnit = "kg"
        , exchanges =
            [ TechnosphereExchange
                { techFlowId = prodId
                , techAmount = 1.0
                , techUnitId = kgUnitId
                , techRole = ReferenceProduct
                , techActivityLinkId = Just actId
                , techSupplierClaim = ClaimByProduct
                , techLocation = ""
                , techComment = Nothing
                , techPedigree = Nothing
                , techShare = Nothing
                , techClassification = M.empty
                , techProperties = noProperties
                }
            , BiosphereExchange
                { bioFlowId = co2Id
                , bioAmount = 1.2
                , bioUnitId = kgUnitId
                , bioDirection = Emission
                , bioLocation = ""
                , bioComment = Nothing
                , bioPedigree = Nothing
                }
            ]
        , activityParams = M.empty
        , activityParamExprs = M.empty
        , activityNativeType = Nothing
        , activityNativeId = Nothing
        , activityFormulaCheck = Nothing
        }
