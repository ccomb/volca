{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the DELETE-BY-SELECTION primitive
('Database.Edit.deleteActivities', 'Database.Edit.resolveDeleteSelection' and
the effectful 'Database.Edit.deleteActivitiesInDB').

Delete is reconstruction over an immutable 'Database':

* deleting a set of ProcessIds drops exactly those activities and renumbers the
  survivors, rebuilding the interning tables, indexes, and sparse matrices;
* an exchange in a surviving activity that pointed at a deleted activity is
  UNLINKED (activity link reset to nil), never silently dropped: the database
  is left ready for relinking;
* the selection resolver computes @(filtered ∪ extra) \\ keep@, so the UI's
  "delete the whole filtered set" honours per-row checkbox overrides;
* deleting by filter removes the WHOLE matching set, independent of pagination.
-}
module DeleteSelectionSpec (spec) where

import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import Control.Monad (forM_)
import qualified Data.ByteString.Lazy as BL
import Data.JournalFile (noOrigin)
import Data.List (sort)
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Text (Text, isInfixOf, unpack)
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Database.Journal (nothingStated)
import Test.Hspec

import API.Types (DeleteSelectionRequest (..))
import CLI.Client (deleteSelectionBody)
import CLI.Types (DbDeleteArgs (..))
import Config (DatabaseConfig (..), defaultConfig)
import Data.Aeson (eitherDecode, encode)
import Database (buildDatabaseWithMatrices)
import Database.Allocation (Allocating (..), allocate)
import Database.CrossLinking (
    IndexedDatabase,
    LinkingContext (..),
    buildIndexedDatabase,
    defaultLinkingThreshold,
    emptyAliasMap,
    locationHierarchy,
 )
import Database.Edit (
    DeleteOutcome (..),
    DeletePlan (..),
    DeleteRequest (..),
    DeleteRun (..),
    DeleteSelection (..),
    SpareChoice (..),
    deleteActivitiesInDB,
    planDeletion,
    resolveDeleteSelection,
 )
import Database.Loader (findAllCrossDBLinks)
import Database.Manager (
    CachePolicy (..),
    DatabaseManager (..),
    LoadedDatabase (..),
    initDatabaseManager,
 )
import Database.Rebuild (deleteActivities)
import Matrix (Inventory, computeInventoryMatrix)
import Service (Exclusions (..), noExclusions)
import SharedSolver (SharedSolver, createSharedSolver)
import SynonymDB (emptySynonymDB)
import Types (
    Activity (..),
    AllocationKey (..),
    BioDirection (..),
    BuildInputs (..),
    ClassificationFilter (..),
    ClassificationMatch (..),
    CrossDBLink (..),
    CrossDBLinkingStats (..),
    Database (..),
    DeclaredShare (..),
    Exchange (..),
    FileReading (..),
    GeographyPolicy (..),
    Licence (..),
    LocationSource (..),
    ProcessId,
    Reading (..),
    ReferenceConvention (..),
    SimpleDatabase (..),
    SupplierClaim (..),
    TechRole (..),
    TechnosphereFlow (..),
    UUID,
    Unit (..),
    exchanges,
    findProcessId,
    noDates,
    noDocumentation,
    noProperties,
    packExchanges,
    processIdToText,
    toSimpleDatabase,
 )
import UnitConversion (defaultUnitConfig)

-- | Empty CLI delete args; tests tweak only the fields they need.
emptyDeleteArgs :: DbDeleteArgs
emptyDeleteArgs =
    DbDeleteArgs
        { ddaDb = "db"
        , ddaName = Nothing
        , ddaLocation = Nothing
        , ddaProduct = Nothing
        , ddaClassSystem = Nothing
        , ddaClassValue = Nothing
        , ddaExact = False
        , ddaKeep = []
        , ddaExtra = []
        , ddaIds = []
        , ddaExcludeName = []
        , ddaExcludeLocation = []
        , ddaSpare = CutSuppliers
        , ddaDryRun = False
        }

-- | Empty 'DeleteRequest'; tests tweak only the fields they need.
emptyDelete :: DeleteRequest
emptyDelete =
    DeleteRequest
        { drName = Nothing
        , drLocation = Nothing
        , drProduct = Nothing
        , drClassifications = []
        , drExactName = False
        , drKeep = []
        , drExtra = []
        , drIds = Nothing
        , drExcluded = noExclusions
        , drSpare = CutSuppliers
        , drRun = RealRun
        , drStated = nothingStated
        }

spec :: Spec
spec = describe "Database.Edit delete-by-selection primitive" $ do
    describe "resolveDeleteSelection" $ do
        it "deletes the whole filtered set when there are no overrides" $
            S.fromList (resolveDeleteSelection (DeleteSelection [0, 1, 2, 3] [] []))
                `shouldBe` S.fromList [0, 1, 2, 3]

        it "spares the explicit keep set (unticked checkboxes)" $
            S.fromList (resolveDeleteSelection (DeleteSelection [0, 1, 2, 3] [1, 2] []))
                `shouldBe` S.fromList [0, 3]

        it "adds the explicit extra set (ticked rows outside the filter)" $
            S.fromList (resolveDeleteSelection (DeleteSelection [0, 1] [] [5, 6]))
                `shouldBe` S.fromList [0, 1, 5, 6]

        it "lets keep win over both filtered and extra" $
            S.fromList (resolveDeleteSelection (DeleteSelection [0, 1] [1, 5] [5, 6]))
                `shouldBe` S.fromList [0, 6]

    describe "deleteActivities (pure rebuild)" $ do
        it "removes exactly the requested activities and renumbers survivors" $ do
            db <- buildOrFail (chainDB 100)
            dbActivityCount db `shouldBe` 3
            -- Delete the leaf supplier (index 0 = "supplier" sorts first).
            let supplierPid = pidFor db 100 0
            case deleteActivities [supplierPid] db of
                Left err -> expectationFailure ("deleteActivities failed: " <> show err)
                Right db' -> do
                    dbActivityCount db' `shouldBe` 2
                    V.length (dbActivities db') `shouldBe` 2
                    -- ProcessId table and lookup agree after renumbering.
                    V.length (dbProcessIdTable db') `shouldBe` 2
                    M.size (dbProcessIdLookup db') `shouldBe` 2
                    -- The biosphere/tech matrices were rebuilt: the deleted
                    -- supplier's row no longer contributes any tech triple.
                    U.length (dbTechnosphereTriples db')
                        `shouldSatisfy` (< U.length (dbTechnosphereTriples db))

        it "unlinks surviving exchanges that pointed at a deleted activity" $ do
            db <- buildOrFail (chainDB 200)
            let supplierPid = pidFor db 200 0
            case deleteActivities [supplierPid] db of
                Left err -> expectationFailure ("deleteActivities failed: " <> show err)
                Right db' -> do
                    -- The mid activity kept its input exchange, but the link to
                    -- the deleted supplier is now nil, ready for relinking.
                    let midInputLinks =
                            [ techActivityLinkId ex
                            | act <- V.toList (dbActivities db')
                            , activityName act == "mid"
                            , ex@TechnosphereExchange{techRole = Input} <- exchanges act
                            ]
                    midInputLinks `shouldBe` [Nothing]

        it "keeps the location an unlinked line bought at" $ do
            db <- buildOrFail (locatedChainDB 250)
            let indianPid = pidFor2 db (mkUUID 251) (mkUUID 251)
                brazilianPid = pidFor2 db (mkUUID 261) (mkUUID 261)
            case deleteActivities [indianPid, brazilianPid] db of
                Left err -> expectationFailure ("deleteActivities failed: " <> show err)
                Right db' -> do
                    let inputs =
                            [ (techFlowId ex, techLocation ex)
                            | act <- V.toList (dbActivities db')
                            , activityName act == "mid"
                            , ex@TechnosphereExchange{techRole = Input} <- exchanges act
                            ]
                    -- The line that stated nothing takes the location its
                    -- supplier supplied from; the line that stated CN keeps
                    -- what its source said, which is not where the supplier is.
                    lookup (mkUUID 251) inputs `shouldBe` Just "IN"
                    lookup (mkUUID 261) inputs `shouldBe` Just "CN"

        it "keeps a multi-product link target when only one product is deleted" $ do
            db <- buildOrFail (multiProductDB 300)
            -- supplier exposes two products; delete only product B.
            let prodBPid = pidFor2 db (mkUUID 301) (mkUUID 303)
            case deleteActivities [prodBPid] db of
                Left err -> expectationFailure ("deleteActivities failed: " <> show err)
                Right db' -> do
                    dbActivityCount db' `shouldBe` 2
                    -- The consumer linking to product A (a surviving key) keeps its link.
                    let consumerLinks =
                            [ techActivityLinkId ex
                            | act <- V.toList (dbActivities db')
                            , activityName act == "consumer-A"
                            , ex@TechnosphereExchange{techRole = Input} <- exchanges act
                            ]
                    consumerLinks `shouldBe` [Just (mkUUID 301)]

        it "unlinks a surviving waste generator when its treatment is deleted" $ do
            db <- buildOrFail (wasteDB 350)
            -- The treatment activity sorts first; delete it.
            let treatmentPid = pidFor db 350 0
            case deleteActivities [treatmentPid] db of
                Left err -> expectationFailure ("deleteActivities failed: " <> show err)
                Right db' -> do
                    dbActivityCount db' `shouldBe` 1
                    -- The surviving generator's waste output no longer resolves to
                    -- the deleted treatment: the link is nil, ready for relinking.
                    let generatorLinks =
                            [ waActivityLinkId ex
                            | act <- V.toList (dbActivities db')
                            , activityName act == "generator"
                            , ex@WasteExchange{} <- exchanges act
                            ]
                    generatorLinks `shouldBe` [Nothing]

        it "fails loudly on an out-of-range ProcessId" $ do
            db <- buildOrFail (chainDB 400)
            case deleteActivities [999] db of
                Left _ -> pure ()
                Right _ -> expectationFailure "expected out-of-range ProcessId to fail"

        it "refuses to delete every activity" $ do
            db <- buildOrFail (chainDB 500)
            let allPids = [0 .. dbActivityCount db - 1]
            case deleteActivities allPids db of
                Left _ -> pure ()
                Right _ -> expectationFailure "expected delete-all to be refused"

    describe "planDeletion" $ do
        it "spares the suppliers the remaining activities use, however far upstream" $ do
            let c = keptChain 2000
            db <- buildOrFail (chParts c)
            S.fromList [chA c, chB c] `shouldSatisfy` (`S.isSubsetOf` snd (planKeys SpareSuppliers db (chMatched c)))

        it "spares a waste treatment the remaining activities send to" $ do
            let c = keptChain 2100
            db <- buildOrFail (chParts c)
            S.member (chT c) (snd (planKeys SpareSuppliers db (chMatched c))) `shouldBe` True

        it "deletes what nothing remaining uses" $ do
            let c = keptChain 2200
            db <- buildOrFail (chParts c)
            planKeys SpareSuppliers db (chMatched c) `shouldBe` (S.fromList [chX c], S.fromList [chA c, chB c, chT c])

        it "cuts every matched activity when asked to" $ do
            let c = keptChain 2300
            db <- buildOrFail (chParts c)
            planKeys CutSuppliers db (chMatched c) `shouldBe` (S.fromList (chMatched c), S.empty)

        it "spares a supplier of a remaining activity the allocation refuses" $ do
            let (parts, supplier) = refusedConsumerDB 2400
            db <- buildOrFail parts
            planKeys SpareSuppliers db [supplier] `shouldBe` (S.empty, S.singleton supplier)

        it "spares a coproduct bought from a block kept whole" $ do
            let (parts, block) = wholeBlockDB 2500
            db <- buildOrFail parts
            planKeys SpareSuppliers db [block] `shouldBe` (S.empty, S.singleton block)

        it "spares a supplier linked at amount zero" $ do
            let (parts, supplier) = zeroLinkDB 2600
            db <- buildOrFail parts
            planKeys SpareSuppliers db [supplier] `shouldBe` (S.empty, S.singleton supplier)

        it "leaves the scores of every remaining activity unchanged" $ do
            let c = keptChain 2700
            db <- buildOrFail (chParts c)
            db' <- afterDelete db (dpDeleted (planDeletion SpareSuppliers db (map (pidOfKey db) (chMatched c))))
            keptScoresUnchanged db db'

        it "leaves a spared process as the allocation of its block made it when its sibling goes" $ do
            let (parts, kept, deleted) = siblingsDB 2800
            db <- buildOrFail parts
            planKeys SpareSuppliers db [kept, deleted] `shouldBe` (S.singleton deleted, S.singleton kept)
            db' <- afterDelete db [pidOfKey db deleted]
            keptScoresUnchanged db db'

        it "keeps every link the remaining activities make to another database" $ do
            let c = keptChain 2900
                bg = indexBackground (chBackground c)
            db <- buildOrFail (chParts c)
            let matched = map (pidOfKey db) (chMatched c)
            spared <- afterDelete db (dpDeleted (planDeletion SpareSuppliers db matched))
            crossLinks spared bg spared `shouldBe` crossLinks spared bg db
            -- Cutting the suppliers is what hands the remaining line to the
            -- other database: the behaviour the opt-out asks for.
            cut <- afterDelete db (dpDeleted (planDeletion CutSuppliers db matched))
            let boughtAt links = [(consumer, supplier) | (consumer, supplier, _) <- links]
            boughtAt (crossLinks cut bg cut) `shouldSatisfy` elem (chK c, chA c)
            boughtAt (crossLinks db bg db) `shouldNotSatisfy` elem (chK c, chA c)

        it "spares every block that could answer a coproduct its link leaves ambiguous" $ do
            let (parts, background, oneBlock, unrelated) = ambiguousBlocksDB 3000
                bg = indexBackground background
            db <- buildOrFail parts
            planKeys SpareSuppliers db [oneBlock, unrelated] `shouldBe` (S.singleton unrelated, S.singleton oneBlock)
            db' <- afterDelete db (dpDeleted (planDeletion SpareSuppliers db (map (pidOfKey db) [oneBlock, unrelated])))
            crossLinks db' bg db' `shouldBe` crossLinks db' bg db

    describe "deleteActivitiesInDB (filter-driven, in-place)" $ do
        it "deletes the whole filtered set and respects keep" $ do
            manager <- initDatabaseManager defaultConfig NoCache
            db <- buildOrFail (classifiedDB 600)
            installLoaded manager "edit-me" db
            -- Filter matches the two "food" activities; keep one by its
            -- canonical process-id string (the value the UI carries).
            let foodA = processIdToText db (pidFor2 db (mkUUID 601) (mkUUID 601))
            r <-
                deleteActivitiesInDB
                    manager
                    "edit-me"
                    noOrigin
                    emptyDelete{drClassifications = [ClassificationFilter{clfSystem = "category", clfValue = "food", clfMatch = MatchContains}], drKeep = [foodA]}
            case r of
                Left err -> expectationFailure ("deleteActivitiesInDB failed: " <> show err)
                Right deleted -> do
                    doRemoved deleted `shouldBe` 1 -- two matched, one kept
                    loaded <- readTVarIO (dmLoadedDbs manager)
                    let db' = ldDatabase (loaded M.! "edit-me")
                    dbActivityCount db' `shouldBe` 2
                    -- The kept food activity and the non-food activity survive.
                    map activityName (V.toList (dbActivities db'))
                        `shouldSatisfy` elem "food-A"

        it "fails loudly on an unknown keep process id" $ do
            manager <- initDatabaseManager defaultConfig NoCache
            db <- buildOrFail (classifiedDB 700)
            installLoaded manager "edit-me-2" db
            r <-
                deleteActivitiesInDB
                    manager
                    "edit-me-2"
                    noOrigin
                    emptyDelete{drClassifications = [ClassificationFilter{clfSystem = "category", clfValue = "food", clfMatch = MatchContains}], drKeep = ["not-a-real-process-id"]}
            case r of
                Left _ -> pure ()
                Right _ -> expectationFailure "expected unknown keep id to fail"

        it "fails when the database is not loaded" $ do
            manager <- initDatabaseManager defaultConfig NoCache
            r <- deleteActivitiesInDB manager "ghost" noOrigin emptyDelete
            fmap doRemoved r `shouldBe` Left "Database not loaded: ghost"

        it "refuses to delete while a loaded database depends on it" $ do
            -- Deleting from "base" would renumber/remove activities that the loaded
            -- "dependent" links to across databases; those links would then silently
            -- drop at solve time. The delete must be refused, mirroring unloadDatabase.
            manager <- initDatabaseManager defaultConfig NoCache
            base <- buildOrFail (classifiedDB 800)
            installLoaded manager "base" base
            dep <- buildOrFail (classifiedDB 900)
            installLoaded manager "dependent" dep{dbDependsOn = ["base"]}
            r <-
                deleteActivitiesInDB
                    manager
                    "base"
                    noOrigin
                    emptyDelete{drClassifications = [ClassificationFilter{clfSystem = "category", clfValue = "food", clfMatch = MatchContains}]}
            case r of
                Left err -> err `shouldSatisfy` isInfixOf "still required by"
                Right _ -> expectationFailure "expected delete to be refused while a dependent is loaded"

    describe "deleteActivitiesInDB (delete exactly these ids)" $ do
        it "deletes exactly the listed process ids, no base filter" $ do
            manager <- initDatabaseManager defaultConfig NoCache
            db <- buildOrFail (classifiedDB 1000)
            installLoaded manager "by-ids" db
            let foodA = processIdToText db (pidFor2 db (mkUUID 1001) (mkUUID 1001))
            r <- deleteActivitiesInDB manager "by-ids" noOrigin emptyDelete{drIds = Just [foodA]}
            case r of
                Left err -> expectationFailure ("deleteActivitiesInDB failed: " <> show err)
                Right deleted -> do
                    doRemoved deleted `shouldBe` 1
                    loaded <- readTVarIO (dmLoadedDbs manager)
                    let db' = ldDatabase (loaded M.! "by-ids")
                    dbActivityCount db' `shouldBe` 2
                    map activityName (V.toList (dbActivities db'))
                        `shouldNotSatisfy` elem "food-A"

        it "refuses ids combined with a filter field" $ do
            manager <- initDatabaseManager defaultConfig NoCache
            db <- buildOrFail (classifiedDB 1100)
            installLoaded manager "by-ids-2" db
            let foodA = processIdToText db (pidFor2 db (mkUUID 1101) (mkUUID 1101))
            r <-
                deleteActivitiesInDB
                    manager
                    "by-ids-2"
                    noOrigin
                    emptyDelete{drIds = Just [foodA], drClassifications = [ClassificationFilter{clfSystem = "category", clfValue = "food", clfMatch = MatchContains}]}
            case r of
                Left err -> err `shouldSatisfy` isInfixOf "cannot be combined"
                Right _ -> expectationFailure "expected ids+filter to be refused"

        it "fails loudly on an unknown id" $ do
            manager <- initDatabaseManager defaultConfig NoCache
            db <- buildOrFail (classifiedDB 1200)
            installLoaded manager "by-ids-3" db
            r <- deleteActivitiesInDB manager "by-ids-3" noOrigin emptyDelete{drIds = Just ["not-a-real-process-id"]}
            case r of
                Left err -> err `shouldSatisfy` isInfixOf "Unknown process id"
                Right _ -> expectationFailure "expected unknown id to fail"

        it "refuses ids combined with exact (it would silently do nothing)" $ do
            manager <- initDatabaseManager defaultConfig NoCache
            db <- buildOrFail (classifiedDB 1300)
            installLoaded manager "by-ids-4" db
            let foodA = processIdToText db (pidFor2 db (mkUUID 1301) (mkUUID 1301))
            r <- deleteActivitiesInDB manager "by-ids-4" noOrigin emptyDelete{drIds = Just [foodA], drExactName = True}
            case r of
                Left err -> err `shouldSatisfy` isInfixOf "cannot be combined"
                Right _ -> expectationFailure "expected ids+exact to be refused"

        it "composes ids with keep and extra: (ids ∪ extra) \\ keep" $ do
            manager <- initDatabaseManager defaultConfig NoCache
            db <- buildOrFail (classifiedDB 1400)
            installLoaded manager "by-ids-5" db
            let foodA = processIdToText db (pidFor2 db (mkUUID 1401) (mkUUID 1401))
                foodB = processIdToText db (pidFor2 db (mkUUID 1411) (mkUUID 1411))
                energ = processIdToText db (pidFor2 db (mkUUID 1421) (mkUUID 1421))
            r <-
                deleteActivitiesInDB
                    manager
                    "by-ids-5"
                    noOrigin
                    emptyDelete{drIds = Just [foodA, foodB], drKeep = [foodA], drExtra = [energ]}
            case r of
                Left err -> expectationFailure ("deleteActivitiesInDB failed: " <> show err)
                Right deleted -> do
                    doRemoved deleted `shouldBe` 2 -- foodB and energy; foodA kept
                    loaded <- readTVarIO (dmLoadedDbs manager)
                    let db' = ldDatabase (loaded M.! "by-ids-5")
                    map activityName (V.toList (dbActivities db')) `shouldBe` ["food-A"]

        it "deletes only the extras when ids is the empty selection" $ do
            -- The official replacement for the old "unsatisfiable filter +
            -- extra" hack: an empty ids selection plus explicit extras.
            manager <- initDatabaseManager defaultConfig NoCache
            db <- buildOrFail (classifiedDB 1500)
            installLoaded manager "by-ids-6" db
            let foodA = processIdToText db (pidFor2 db (mkUUID 1501) (mkUUID 1501))
            r <- deleteActivitiesInDB manager "by-ids-6" noOrigin emptyDelete{drIds = Just [], drExtra = [foodA]}
            case r of
                Left err -> expectationFailure ("deleteActivitiesInDB failed: " <> show err)
                Right deleted -> doRemoved deleted `shouldBe` 1

    describe "deleteActivitiesInDB (what a delete spares, and what it announces first)" $ do
        it "counts in a dry run what would go and what is spared, and changes nothing" $ do
            let c = keptChain 3100
            (manager, db) <- installedFrom "dry-run" (chParts c)
            before <- heldKeys manager "dry-run"
            outcome <- outcomeOf manager "dry-run" (namingKeys db SpareSuppliers (chMatched c)){drRun = DryRun}
            (doRemoved outcome, doSpared outcome) `shouldBe` (1, 3)
            heldKeys manager "dry-run" `shouldReturn` before

        it "runs in a dry run the refusals the delete runs" $ do
            (manager, db) <- installedFrom "refused" (classifiedDB 3200)
            everyone <- heldKeys manager "refused"
            let request = namingKeys db CutSuppliers everyone
            dry <- deleteActivitiesInDB manager "refused" noOrigin request{drRun = DryRun}
            real <- deleteActivitiesInDB manager "refused" noOrigin request
            refusal real `shouldSatisfy` maybe False (isInfixOf "no processes")
            refusal dry `shouldBe` refusal real

        it "refuses in a dry run, as the delete does, while a loaded database depends on it" $ do
            manager <- initDatabaseManager defaultConfig NoCache
            base <- buildOrFail (classifiedDB 3250)
            installLoaded manager "base" base
            dep <- buildOrFail (classifiedDB 3260)
            installLoaded manager "dependent" dep{dbDependsOn = ["base"]}
            let request = (namingKeys base CutSuppliers [(mkUUID 3251, mkUUID 3251)]){drRun = DryRun}
            dry <- deleteActivitiesInDB manager "base" noOrigin request
            refusal dry `shouldSatisfy` maybe False (isInfixOf "still required by")

        it "deletes exactly what its dry run announced" $ do
            let c = keptChain 3300
            (manager, db) <- installedFrom "announced" (chParts c)
            before <- heldKeys manager "announced"
            let request = namingKeys db SpareSuppliers (chMatched c)
            announced <- outcomeOf manager "announced" request{drRun = DryRun}
            done <- outcomeOf manager "announced" request
            (doRemoved done, doSpared done) `shouldBe` (doRemoved announced, doSpared announced)
            after <- heldKeys manager "announced"
            S.fromList after `shouldBe` S.delete (chX c) (S.fromList before)

        it "cuts every matched process when asked to, and spares none" $ do
            let c = keptChain 3350
            (manager, db) <- installedFrom "cut" (chParts c)
            outcome <- outcomeOf manager "cut" (namingKeys db CutSuppliers (chMatched c))
            (doRemoved outcome, doSpared outcome) `shouldBe` (4, 0)

        it "an everything-except delete made of exclusions only spares the suppliers" $ do
            (manager, _) <- installedFrom "except" (exceptDB 3400)
            let request =
                    emptyDelete
                        { drExcluded = noExclusions{exNames = ["CIQUAL"], exClassifications = [categoryHolding "Agricultural", categoryHolding "Packaging"]}
                        , drSpare = SpareSuppliers
                        }
            outcome <- outcomeOf manager "except" request
            (doRemoved outcome, doSpared outcome) `shouldBe` (1, 1)
            names <- activityNamesIn manager "except"
            sort names `shouldBe` ["CIQUAL yogurt", "carton", "dairy farm", "mill"]

        it "exclusions that leave nothing to delete change nothing" $ do
            (manager, _) <- installedFrom "nothing-left" (classifiedDB 3500)
            before <- heldKeys manager "nothing-left"
            outcome <- outcomeOf manager "nothing-left" emptyDelete{drExcluded = noExclusions{exClassifications = [categoryHolding "food", categoryHolding "energy"]}}
            (doRemoved outcome, doSpared outcome) `shouldBe` (0, 0)
            heldKeys manager "nothing-left" `shouldReturn` before

        -- A misspelt exclusion removes nothing, so the delete would take
        -- what it was meant to keep.
        it "refuses an exclusion that leaves out no process, dry run or not" $ do
            (manager, _) <- installedFrom "idle-exclusion" (classifiedDB 3700)
            before <- heldKeys manager "idle-exclusion"
            let misspelt =
                    [ noExclusions{exNames = ["zzzqx"]}
                    , noExclusions{exLocations = ["Atlantis"]}
                    , noExclusions{exClassifications = [categoryHolding "foood"]}
                    ]
            forM_ misspelt $ \ex -> forM_ [DryRun, RealRun] $ \run -> do
                r <- deleteActivitiesInDB manager "idle-exclusion" noOrigin emptyDelete{drExcluded = ex, drRun = run}
                refusal r `shouldSatisfy` maybe False (isInfixOf "removes no process")
            heldKeys manager "idle-exclusion" `shouldReturn` before

        it "ids refuse exclusions as they refuse filter fields" $ do
            (manager, db) <- installedFrom "ids-and-exclusions" (classifiedDB 3600)
            let foodA = processIdToText db (pidFor2 db (mkUUID 3601) (mkUUID 3601))
            r <- deleteActivitiesInDB manager "ids-and-exclusions" noOrigin emptyDelete{drIds = Just [foodA], drExcluded = noExclusions{exNames = ["energy"]}}
            refusal r `shouldSatisfy` maybe False (isInfixOf "cannot be combined")

    describe "DeleteSelectionRequest wire compatibility" $ do
        it "decodes a request without the ids key (old clients)" $ do
            let json = "{\"classifications\":[],\"exact\":false,\"keep\":[],\"extra\":[],\"spare\":false}"
            case eitherDecode json :: Either String DeleteSelectionRequest of
                Left err -> expectationFailure ("decode failed: " <> err)
                Right req -> dsqIds req `shouldBe` Nothing

        it "reads the exclusions, the spare choice and the dry run" $ do
            let json = "{\"classifications\":[{\"system\":\"category\",\"value\":\"food\",\"mode\":\"not-exact\"}],\"keep\":[],\"extra\":[],\"excludeName\":[\"CIQUAL\"],\"excludeLocation\":[\"FR\"],\"spare\":true,\"dryRun\":true}"
            case eitherDecode json :: Either String DeleteSelectionRequest of
                Left err -> expectationFailure ("decode failed: " <> err)
                Right req -> do
                    (dsqExcludeName req, dsqExcludeLocation req) `shouldBe` (Just ["CIQUAL"], Just ["FR"])
                    (dsqSpare req, dsqDryRun req) `shouldBe` (True, Just True)

        it "refuses a body without spare, naming the field" $
            refusedNaming "spare" (decodedDelete "{\"classifications\":[],\"keep\":[],\"extra\":[]}") `shouldBe` True

        it "refuses a body with an unknown field" $
            refusedNaming "dryrun" (decodedDelete "{\"classifications\":[],\"keep\":[],\"extra\":[],\"spare\":true,\"dryrun\":true}") `shouldBe` True

        it "refuses a classification entry without mode" $
            refusedNaming "mode" (decodedDelete "{\"classifications\":[{\"system\":\"category\",\"value\":\"food\"}],\"keep\":[],\"extra\":[],\"spare\":true}") `shouldBe` True

        it "refuses a classification entry with a field it does not have" $
            refusedNaming "exact" (decodedDelete "{\"classifications\":[{\"system\":\"category\",\"value\":\"food\",\"mode\":\"exact\",\"exact\":true}],\"keep\":[],\"extra\":[],\"spare\":true}") `shouldBe` True

        it "CLI --id values reach the server as the ids selection" $ do
            -- The CLI builds this body by hand (CLI.Client.deleteSelectionBody);
            -- a dropped field here silently falls back to filter mode, whose
            -- empty filter selects the WHOLE database. Pin the contract.
            let args = emptyDeleteArgs{ddaIds = ["pid-a", "pid-b"]}
            case eitherDecode (encode (deleteSelectionBody args)) :: Either String DeleteSelectionRequest of
                Left err -> expectationFailure ("decode failed: " <> err)
                Right req -> dsqIds req `shouldBe` Just ["pid-a", "pid-b"]

        it "CLI filter mode still sends no ids (old servers keep working)" $ do
            let args = emptyDeleteArgs{ddaName = Just "electricity"}
            case eitherDecode (encode (deleteSelectionBody args)) :: Either String DeleteSelectionRequest of
                Left err -> expectationFailure ("decode failed: " <> err)
                Right req -> do
                    dsqIds req `shouldBe` Nothing
                    dsqName req `shouldBe` Just "electricity"

        it "CLI spare choice, exclusions and dry run reach the server" $ do
            let args = emptyDeleteArgs{ddaSpare = SpareSuppliers, ddaExcludeName = ["CIQUAL"], ddaExcludeLocation = ["FR"], ddaDryRun = True}
            case eitherDecode (encode (deleteSelectionBody args)) :: Either String DeleteSelectionRequest of
                Left err -> expectationFailure ("decode failed: " <> err)
                Right req -> do
                    (dsqSpare req, dsqDryRun req) `shouldBe` (True, Just True)
                    (dsqExcludeName req, dsqExcludeLocation req) `shouldBe` (Just ["CIQUAL"], Just ["FR"])

        it "CLI --cut-suppliers sends spare false" $
            case eitherDecode (encode (deleteSelectionBody emptyDeleteArgs)) :: Either String DeleteSelectionRequest of
                Left err -> expectationFailure ("decode failed: " <> err)
                Right req -> (dsqSpare req, dsqDryRun req) `shouldBe` (False, Just False)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | The body of a delete request as a client sent it.
decodedDelete :: BL.ByteString -> Either String DeleteSelectionRequest
decodedDelete = eitherDecode

-- | Whether a decoding was refused with a message that names this word.
refusedNaming :: String -> Either String a -> Bool
refusedNaming word = either (L.isInfixOf word) (const False)

-- | Why a delete was refused, if it was.
refusal :: Either Text DeleteOutcome -> Maybe Text
refusal = either Just (const Nothing)

-- | A manager holding the database, and the database as installed.
installedFrom :: Text -> SimpleParts -> IO (DatabaseManager, Database)
installedFrom name parts = do
    manager <- initDatabaseManager defaultConfig NoCache
    db <- buildOrFail parts
    installLoaded manager name db
    pure (manager, db)

-- | A delete naming these processes, sparing or cutting what they supply.
namingKeys :: Database -> SpareChoice -> [(UUID, UUID)] -> DeleteRequest
namingKeys db choice keys =
    emptyDelete{drIds = Just [processIdToText db (pidOfKey db key) | key <- keys], drSpare = choice}

-- | What a delete did, the example failing when it was refused.
outcomeOf :: DatabaseManager -> Text -> DeleteRequest -> IO DeleteOutcome
outcomeOf manager name request = deleteActivitiesInDB manager name noOrigin request >>= either (fail . unpack) pure

-- | The keys of the processes a loaded database holds.
heldKeys :: DatabaseManager -> Text -> IO [(UUID, UUID)]
heldKeys manager name =
    maybe [] (V.toList . dbProcessIdTable . ldDatabase) . M.lookup name <$> readTVarIO (dmLoadedDbs manager)

activityNamesIn :: DatabaseManager -> Text -> IO [Text]
activityNamesIn manager name =
    maybe [] (map activityName . V.toList . dbActivities . ldDatabase) . M.lookup name <$> readTVarIO (dmLoadedDbs manager)

-- | A classification filter on the @category@ system.
categoryHolding :: Text -> ClassificationFilter
categoryHolding value = ClassificationFilter{clfSystem = "category", clfValue = value, clfMatch = MatchContains}

pidFor :: Database -> Int -> Int -> ProcessId
pidFor db offset i = pidFor2 db (mkUUID (offset + 10 * i + 1)) (mkUUID (offset + 10 * i + 1))

pidFor2 :: Database -> UUID -> UUID -> ProcessId
pidFor2 db actUUID prodUUID =
    fromMaybe (error "pidFor2: key not found") (findProcessId db actUUID prodUUID)

installLoaded :: DatabaseManager -> Text -> Database -> IO ()
installLoaded manager name db = do
    solver <- mkSolver name db
    let loaded =
            LoadedDatabase
                { ldDatabase = db
                , ldSharedSolver = solver
                , ldConfig = mkConfig name
                , ldSupplierMapping = Nothing
                }
    atomically $ do
        modifyTVar' (dmLoadedDbs manager) (M.insert name loaded)
        modifyTVar' (dmAvailableDbs manager) (M.insert name (mkConfig name))

mkSolver :: Text -> Database -> IO SharedSolver
mkSolver name db =
    let triples = dbTechnosphereTriples db
     in createSharedSolver name triples (fromIntegral (dbActivityCount db))

mkConfig :: Text -> DatabaseConfig
mkConfig name =
    DatabaseConfig
        { dcName = name
        , dcDisplayName = name
        , dcPath = ""
        , dcDescription = Nothing
        , dcLoad = True
        , dcDefault = False
        , dcDepends = []
        , dcLocationAliases = M.empty
        , dcFormat = Nothing
        , dcIsUploaded = False
        , dcDeletable = True
        , dcGeographyPolicy = GeoGlobal
        , dcReading = FromFiles ReadDeclared
        , dcPatches = []
        , dcSource = Nothing
        , dcLicence = LicenceUnstated
        , dcRelease = Nothing
        , dcRequires = []
        , dcAdded = Nothing
        }

buildOrFail :: SimpleParts -> IO Database
buildOrFail parts = do
    r <- buildDatabaseWithMatrices (BuildInputs defaultUnitConfig mempty ReadDeclared []) (simpleOf parts)
    case r of
        Right db -> pure db
        Left err -> fail ("buildDatabaseWithMatrices: " <> show err)

simpleOf :: SimpleParts -> SimpleDatabase
simpleOf (SimpleParts acts flows unitTable) =
    SimpleDatabase
        { sdbActivities = acts
        , sdbTechFlows = flows
        , sdbBioFlows = M.empty
        , sdbWasteFlows = M.empty
        , sdbUnits = unitTable
        , sdbDocumentation = noDocumentation
        , sdbParameters = M.empty
        , sdbProductSystems = []
        , sdbReferenceConvention = SignedReferences
        }

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

data SimpleParts
    = SimpleParts
        (M.Map (UUID, UUID) Activity)
        (M.Map UUID TechnosphereFlow)
        (M.Map UUID Unit)

mkUUID :: Int -> UUID
mkUUID n = UUID.fromWords64 (fromIntegral n) 0

kgUnitId :: UUID
kgUnitId = mkUUID 0

kgUnit :: Unit
kgUnit = Unit{unitId = kgUnitId, unitName = "kg", unitSymbol = "kg", unitComment = ""}

mkTechFlow :: UUID -> Text -> TechnosphereFlow
mkTechFlow fid name =
    TechnosphereFlow
        { tfId = fid
        , tfName = name
        , tfUnitId = kgUnitId
        , tfSynonyms = M.empty
        , tfCAS = Nothing
        , tfSubstanceId = Nothing
        }

{- | A reference-product output exchange whose product UUID equals its flow UUID
(so @(actUUID, prodUUID)@ is also the @(actUUID, flowId)@ link key).
-}
refOut :: UUID -> UUID -> Exchange
refOut actUUID prodUUID =
    TechnosphereExchange
        { techFlowId = prodUUID
        , techAmount = 1.0
        , techUnitId = kgUnitId
        , techRole = ReferenceProduct
        , techActivityLinkId = Just actUUID
        , techSupplierClaim = ClaimByProduct
        , techLocation = ""
        , techComment = Nothing
        , techPedigree = Nothing
        , techFormula = Nothing
        , techShare = Nothing
        , techClassification = M.empty
        , techProperties = noProperties
        }

-- | An input exchange linking to a supplier's @(supplierActUUID, supplierProdUUID)@.
inputFrom :: UUID -> UUID -> Exchange
inputFrom supplierActUUID supplierProdUUID =
    TechnosphereExchange
        { techFlowId = supplierProdUUID
        , techAmount = 0.5
        , techUnitId = kgUnitId
        , techRole = Input
        , techActivityLinkId = Just supplierActUUID
        , techSupplierClaim = ClaimByProduct
        , techLocation = ""
        , techComment = Nothing
        , techPedigree = Nothing
        , techFormula = Nothing
        , techShare = Nothing
        , techClassification = M.empty
        , techProperties = noProperties
        }

{- | A waste output linking to a treatment activity's
@(treatmentActUUID, treatmentProdUUID)@, the same @(activityLink, flowId)@
key resolution a technosphere input uses.
-}
wasteOut :: UUID -> UUID -> Exchange
wasteOut treatmentActUUID treatmentProdUUID =
    WasteExchange
        { waFlowId = treatmentProdUUID
        , waAmount = 0.5
        , waUnitId = kgUnitId
        , waIsInput = False
        , waActivityLinkId = Just treatmentActUUID
        , waSupplierClaim = ClaimByProduct
        , waLocation = ""
        , waComment = Nothing
        , waPedigree = Nothing
        , waFormula = Nothing
        }

mkActivity :: Text -> Text -> M.Map Text Text -> [Exchange] -> Activity
mkActivity name loc classif exs =
    Activity
        { activityName = name
        , activityDescription = []
        , activityDocumentation = []
        , activitySynonyms = M.empty
        , activityClassification = classif
        , activityLocation = loc
        , activityLocationSource = LocationDeclared
        , activityUnit = "kg"
        , activityLines = packExchanges exs
        , activityParameters = M.empty
        , activityNativeType = Nothing
        , activityNativeId = Nothing
        , activityFormulaCheck = Nothing
        , activityDates = noDates
        }

units :: M.Map UUID Unit
units = M.singleton kgUnitId kgUnit

{- | A three-activity chain: top → mid → supplier. UUIDs are
@offset + 10*i + 1@ for activity i (i: 0=supplier, 1=mid, 2=top), with product
UUID equal to the activity UUID for simple linking.
-}
chainDB :: Int -> SimpleParts
chainDB offset =
    let supA = mkUUID (offset + 1)
        midA = mkUUID (offset + 11)
        topA = mkUUID (offset + 21)
        supplier = mkActivity "supplier" "GLO" M.empty [refOut supA supA]
        mid = mkActivity "mid" "GLO" M.empty [refOut midA midA, inputFrom supA supA]
        top = mkActivity "top" "GLO" M.empty [refOut topA topA, inputFrom midA midA]
     in SimpleParts
            ( M.fromList
                [ ((supA, supA), supplier)
                , ((midA, midA), mid)
                , ((topA, topA), top)
                ]
            )
            ( M.fromList
                [ (supA, mkTechFlow supA "supplier-product")
                , (midA, mkTechFlow midA "mid-product")
                , (topA, mkTechFlow topA "top-product")
                ]
            )
            units

{- | Two suppliers in two countries, and one consumer buying from both: from the
Indian one saying nothing about where, from the Brazilian one saying @CN@.
Deleting both suppliers is what a foreground export does to its background.
-}
locatedChainDB :: Int -> SimpleParts
locatedChainDB offset =
    let indian = mkUUID (offset + 1)
        brazilian = mkUUID (offset + 11)
        midA = mkUUID (offset + 21)
        statedCN = (inputFrom brazilian brazilian){techLocation = "CN"}
        mid = mkActivity "mid" "GLO" M.empty [refOut midA midA, inputFrom indian indian, statedCN]
     in SimpleParts
            ( M.fromList
                [ ((indian, indian), mkActivity "indian" "IN" M.empty [refOut indian indian])
                , ((brazilian, brazilian), mkActivity "brazilian" "BR" M.empty [refOut brazilian brazilian])
                , ((midA, midA), mid)
                ]
            )
            ( M.fromList
                [ (indian, mkTechFlow indian "indian-product")
                , (brazilian, mkTechFlow brazilian "brazilian-product")
                , (midA, mkTechFlow midA "mid-product")
                ]
            )
            units

{- | A supplier exposing two products (A, B) plus a consumer linking to product
A only. Deleting product B must keep the consumer's link to A intact.
-}
multiProductDB :: Int -> SimpleParts
multiProductDB offset =
    let supA = mkUUID (offset + 1)
        prodA = mkUUID (offset + 2)
        prodB = mkUUID (offset + 3)
        consA = mkUUID (offset + 11)
        supplierA = mkActivity "supplier" "GLO" M.empty [refOut supA prodA]
        supplierB = mkActivity "supplier" "GLO" M.empty [refOut supA prodB]
        consumer = mkActivity "consumer-A" "GLO" M.empty [refOut consA consA, inputFrom supA prodA]
     in SimpleParts
            ( M.fromList
                [ ((supA, prodA), supplierA)
                , ((supA, prodB), supplierB)
                , ((consA, consA), consumer)
                ]
            )
            ( M.fromList
                [ (prodA, mkTechFlow prodA "product-A")
                , (prodB, mkTechFlow prodB "product-B")
                , (consA, mkTechFlow consA "consumer-product")
                ]
            )
            units

{- | A waste generator wired to a treatment activity via a 'WasteExchange'.
UUIDs are @offset + 10*i + 1@ (i: 0=treatment, 1=generator) so the treatment
sorts first. Deleting the treatment must unlink the generator's waste output.
-}
wasteDB :: Int -> SimpleParts
wasteDB offset =
    let treatA = mkUUID (offset + 1)
        genA = mkUUID (offset + 11)
        treatment = mkActivity "treatment" "GLO" M.empty [refOut treatA treatA]
        generator = mkActivity "generator" "GLO" M.empty [refOut genA genA, wasteOut treatA treatA]
     in SimpleParts
            ( M.fromList
                [ ((treatA, treatA), treatment)
                , ((genA, genA), generator)
                ]
            )
            ( M.fromList
                [ (treatA, mkTechFlow treatA "treatment-product")
                , (genA, mkTechFlow genA "generator-product")
                ]
            )
            units

{- | Two "food" activities and one "energy" activity, classified under the
@category@ system so a classification filter selects the food pair.
-}
classifiedDB :: Int -> SimpleParts
classifiedDB offset =
    let foodA = mkUUID (offset + 1)
        foodB = mkUUID (offset + 11)
        energ = mkUUID (offset + 21)
        food = M.singleton "category" "food"
        energy = M.singleton "category" "energy"
     in SimpleParts
            ( M.fromList
                [ ((foodA, foodA), mkActivity "food-A" "GLO" food [refOut foodA foodA])
                , ((foodB, foodB), mkActivity "food-B" "GLO" food [refOut foodB foodB])
                , ((energ, energ), mkActivity "energy" "GLO" energy [refOut energ energ])
                ]
            )
            ( M.fromList
                [ (foodA, mkTechFlow foodA "food-a")
                , (foodB, mkTechFlow foodB "food-b")
                , (energ, mkTechFlow energ "energy")
                ]
            )
            units

{- | A named yogurt, a farm and a carton, the mill the yogurt buys from, and a
process nothing uses. Deleting everything but what the name @CIQUAL@ and the
classifications @Agricultural@ and @Packaging@ name leaves the mill to be
spared and the unused process to go.
-}
exceptDB :: Int -> SimpleParts
exceptDB offset =
    let yogurt = mkUUID (offset + 1)
        farm = mkUUID (offset + 11)
        carton = mkUUID (offset + 21)
        mill = mkUUID (offset + 31)
        junk = mkUUID (offset + 41)
        classifiedAs = M.singleton "category"
     in SimpleParts
            ( M.fromList
                [ ((yogurt, yogurt), mkActivity "CIQUAL yogurt" "GLO" (classifiedAs "Food") [refOut yogurt yogurt, inputFrom mill mill])
                , ((farm, farm), mkActivity "dairy farm" "GLO" (classifiedAs "Agricultural\\Milk") [refOut farm farm])
                , ((carton, carton), mkActivity "carton" "GLO" (classifiedAs "Packaging\\Box") [refOut carton carton])
                , ((mill, mill), mkActivity "mill" "GLO" (classifiedAs "Industry") [refOut mill mill])
                , ((junk, junk), mkActivity "junk" "GLO" (classifiedAs "Industry") [refOut junk junk])
                ]
            )
            (techFlows [(yogurt, "yogurt"), (farm, "milk"), (carton, "box"), (mill, "flour"), (junk, "scrap")])
            units

-- ---------------------------------------------------------------------------
-- What a delete spares
-- ---------------------------------------------------------------------------

-- | A plan read back as process keys: what goes, and what is spared.
planKeys :: SpareChoice -> Database -> [(UUID, UUID)] -> (S.Set (UUID, UUID), S.Set (UUID, UUID))
planKeys choice db keys =
    let plan = planDeletion choice db (map (pidOfKey db) keys)
        keysOf = S.fromList . map (\pid -> dbProcessIdTable db V.! fromIntegral pid)
     in (keysOf (dpDeleted plan), keysOf (dpSpared plan))

pidOfKey :: Database -> (UUID, UUID) -> ProcessId
pidOfKey db = uncurry (pidFor2 db)

afterDelete :: Database -> [ProcessId] -> IO Database
afterDelete db pids = either (fail . show) pure (deleteActivities pids db)

{- | Every inventory left after the delete equals the one before it, flow by
flow, to a relative 1e-9: the solve runs on a smaller matrix.
-}
keptScoresUnchanged :: Database -> Database -> Expectation
keptScoresUnchanged db db' = do
    before <- inventoriesByKey db
    after <- inventoriesByKey db'
    M.null after `shouldBe` False
    M.keys after `shouldBe` M.keys (M.restrictKeys before (M.keysSet (dbProcessIdLookup db')))
    [ (key, flow, x, y)
      | (key, inventory') <- M.toList after
      , let inventory = M.findWithDefault M.empty key before
      , flow <- S.toList (M.keysSet inventory <> M.keysSet inventory')
      , let x = M.findWithDefault 0 flow inventory
            y = M.findWithDefault 0 flow inventory'
      , abs (x - y) > 1e-9 * max (abs x) (abs y)
      ]
        `shouldBe` []

inventoriesByKey :: Database -> IO (M.Map (UUID, UUID) Inventory)
inventoriesByKey db = M.fromList . concat <$> traverse one [0 .. dbActivityCount db - 1]
  where
    one pid = either (const []) (\inventory -> [(dbProcessIdTable db V.! fromIntegral pid, inventory)]) <$> computeInventoryMatrix db pid

-- | The links to another database the processes of @kept@ make in @db@: consumer, supplier, amount.
crossLinks :: Database -> IndexedDatabase -> Database -> [((UUID, UUID), (UUID, UUID), Double)]
crossLinks kept bg db =
    sort
        [ (consumer, (cdlSupplierActUUID l, cdlSupplierProdUUID l), cdlCoefficient l)
        | l <- cdlLinks (findAllCrossDBLinks (linkingTo bg) (toSimpleDatabase db))
        , let consumer = (cdlConsumerActUUID l, cdlConsumerProdUUID l)
        , M.member consumer (dbProcessIdLookup kept)
        ]

linkingTo :: IndexedDatabase -> LinkingContext
linkingTo bg =
    LinkingContext
        { lcIndexedDatabases = [bg]
        , lcSynonymDB = emptySynonymDB
        , lcUnitConfig = defaultUnitConfig
        , lcThreshold = defaultLinkingThreshold
        , lcLocationHierarchy = locationHierarchy
        , lcGeographyPolicy = GeoGlobal
        , lcSupplierAliases = emptyAliasMap
        }

indexBackground :: SimpleParts -> IndexedDatabase
indexBackground = buildIndexedDatabase "bg" emptySynonymDB . simpleOf

{- | @K → A → B@, @K@ sending waste to the treatment @T@ and buying from a
supplier of another database by its identifier, and an unrelated @X@. The
background ships @A@ under the same identifier, and the supplier @K@ buys there.
-}
data Chain = Chain
    { chParts :: SimpleParts
    , chBackground :: SimpleParts
    , chK :: (UUID, UUID)
    , chA :: (UUID, UUID)
    , chB :: (UUID, UUID)
    , chT :: (UUID, UUID)
    , chX :: (UUID, UUID)
    }

chMatched :: Chain -> [(UUID, UUID)]
chMatched c = [chA c, chB c, chT c, chX c]

keptChain :: Int -> Chain
keptChain offset =
    let k = mkUUID (offset + 1)
        a = mkUUID (offset + 11)
        b = mkUUID (offset + 21)
        t = mkUUID (offset + 31)
        x = mkUUID (offset + 41)
        elsewhere = mkUUID (offset + 51)
        co2 = mkUUID (offset + 99)
        flows = techFlows [(k, "kept"), (a, "a"), (b, "b"), (t, "treatment"), (x, "unrelated"), (elsewhere, "elsewhere")]
     in Chain
            { chParts =
                SimpleParts
                    ( M.fromList
                        [ ((k, k), mkActivity "kept" "GLO" M.empty [refOut k k, inputById a, wasteOut t t, inputById elsewhere, emits co2 1])
                        , ((a, a), mkActivity "a" "GLO" M.empty [refOut a a, inputFrom b b, emits co2 0.25])
                        , ((b, b), mkActivity "b" "GLO" M.empty [refOut b b, emits co2 2])
                        , ((t, t), mkActivity "treatment" "GLO" M.empty [refOut t t, emits co2 3])
                        , ((x, x), mkActivity "unrelated" "GLO" M.empty [refOut x x, emits co2 5])
                        ]
                    )
                    flows
                    units
            , chBackground =
                SimpleParts
                    ( M.fromList
                        [ ((a, a), mkActivity "a" "GLO" M.empty [refOut a a])
                        , ((elsewhere, elsewhere), mkActivity "elsewhere" "GLO" M.empty [refOut elsewhere elsewhere])
                        ]
                    )
                    flows
                    units
            , chK = (k, k)
            , chA = (a, a)
            , chB = (b, b)
            , chT = (t, t)
            , chX = (x, x)
            }

-- | A remaining activity the allocation refuses (a coproduct with no share), buying from a supplier.
refusedConsumerDB :: Int -> (SimpleParts, (UUID, UUID))
refusedConsumerDB offset =
    let r = mkUUID (offset + 1)
        whey = mkUUID (offset + 2)
        s = mkUUID (offset + 11)
        refused = mkActivity "refused" "GLO" M.empty [refOut r r, (refOut r whey){techRole = Coproduct}, inputFrom s s]
     in ( SimpleParts
            (M.fromList [((r, r), refused), ((s, s), mkActivity "supplier" "GLO" M.empty [refOut s s])])
            (techFlows [(r, "cheese"), (whey, "whey"), (s, "milk")])
            units
        , (s, s)
        )

-- | A block kept whole (every product declares a share), whose coproduct a remaining activity buys.
wholeBlockDB :: Int -> (SimpleParts, (UUID, UUID))
wholeBlockDB offset =
    let w = mkUUID (offset + 1)
        cheese = mkUUID (offset + 2)
        cream = mkUUID (offset + 3)
        c = mkUUID (offset + 11)
        block = mkActivity "dairy" "GLO" M.empty [productRow w cheese ReferenceProduct 51, productRow w cream Coproduct 49]
     in ( SimpleParts
            (M.fromList [((w, cheese), block), ((c, c), mkActivity "consumer" "GLO" M.empty [refOut c c, inputFrom w cream])])
            (techFlows [(cheese, "cheese"), (cream, "cream"), (c, "consumer")])
            units
        , (w, cheese)
        )

-- | A remaining activity linked to its supplier by a line of amount zero.
zeroLinkDB :: Int -> (SimpleParts, (UUID, UUID))
zeroLinkDB offset =
    let z = mkUUID (offset + 1)
        s = mkUUID (offset + 11)
     in ( SimpleParts
            ( M.fromList
                [ ((z, z), mkActivity "zero" "GLO" M.empty [refOut z z, (inputFrom s s){techAmount = 0}])
                , ((s, s), mkActivity "supplier" "GLO" M.empty [refOut s s])
                ]
            )
            (techFlows [(z, "zero"), (s, "supplier")])
            units
        , (s, s)
        )

{- | A block split by its declared shares into two processes, one bought by a
remaining activity, the other by nothing: the kept one and the deleted one.
-}
siblingsDB :: Int -> (SimpleParts, (UUID, UUID), (UUID, UUID))
siblingsDB offset =
    let sib = mkUUID (offset + 1)
        cheese = mkUUID (offset + 2)
        whey = mkUUID (offset + 3)
        k = mkUUID (offset + 11)
        s = mkUUID (offset + 21)
        co2 = mkUUID (offset + 99)
        block = mkActivity "dairy" "GLO" M.empty [productRow sib cheese ReferenceProduct 60, productRow sib whey Coproduct 40, (inputFrom s s){techAmount = 10}, emits co2 4]
        processes = NE.toList (allocate Allocating{alKey = Declared, alUnitConfig = defaultUnitConfig, alUnitDB = units} block)
        keyOf p = (sib, head [techFlowId ex | ex@TechnosphereExchange{techRole = ReferenceProduct} <- exchanges p])
     in ( SimpleParts
            ( M.fromList
                ( [(keyOf p, p) | p <- processes]
                    <> [ ((k, k), mkActivity "kept" "GLO" M.empty [refOut k k, inputFrom sib cheese])
                       , ((s, s), mkActivity "milk" "GLO" M.empty [refOut s s, emits co2 1])
                       ]
                )
            )
            (techFlows [(cheese, "cheese"), (whey, "whey"), (k, "kept"), (s, "milk")])
            units
        , (sib, cheese)
        , (sib, whey)
        )

{- | Two blocks kept whole under one activity, both making cream, so a link to
that cream names neither; a remaining activity buys it by the activity's
identifier, which the background ships. Returns the database, the background,
one of the two blocks, and an unrelated process.
-}
ambiguousBlocksDB :: Int -> (SimpleParts, SimpleParts, (UUID, UUID), (UUID, UUID))
ambiguousBlocksDB offset =
    let mill = mkUUID (offset + 1)
        cheese = mkUUID (offset + 2)
        whey = mkUUID (offset + 3)
        cream = mkUUID (offset + 4)
        c = mkUUID (offset + 11)
        x = mkUUID (offset + 21)
        flows = techFlows [(cheese, "cheese"), (whey, "whey"), (cream, "cream"), (c, "consumer"), (x, "unrelated")]
     in ( SimpleParts
            ( M.fromList
                [ ((mill, cheese), mkActivity "cheese" "GLO" M.empty [productRow mill cheese ReferenceProduct 51, productRow mill cream Coproduct 49])
                , ((mill, whey), mkActivity "whey" "GLO" M.empty [productRow mill whey ReferenceProduct 60, productRow mill cream Coproduct 40])
                , ((c, c), mkActivity "consumer" "GLO" M.empty [refOut c c, (inputFrom mill cream){techSupplierClaim = ClaimById mill}])
                , ((x, x), mkActivity "unrelated" "GLO" M.empty [refOut x x])
                ]
            )
            flows
            units
        , SimpleParts (M.fromList [((mill, cream), mkActivity "cream" "GLO" M.empty [refOut mill cream])]) flows units
        , (mill, whey)
        , (x, x)
        )

techFlows :: [(UUID, Text)] -> M.Map UUID TechnosphereFlow
techFlows named = M.fromList [(fid, mkTechFlow fid name) | (fid, name) <- named]

-- | An input naming its supplier by the activity's identifier, as EcoSpold 2 does.
inputById :: UUID -> Exchange
inputById supplier = (inputFrom supplier supplier){techSupplierClaim = ClaimById supplier}

-- | A product output of a block, with the share the source declares for it.
productRow :: UUID -> UUID -> TechRole -> Double -> Exchange
productRow act flow role share = (refOut act flow){techRole = role, techShare = Just (DeclaredShare share Nothing Nothing)}

emits :: UUID -> Double -> Exchange
emits flow amount =
    BiosphereExchange
        { bioFlowId = flow
        , bioAmount = amount
        , bioUnitId = kgUnitId
        , bioDirection = Emission
        , bioLocation = ""
        , bioComment = Nothing
        , bioPedigree = Nothing
        , bioFormula = Nothing
        }
