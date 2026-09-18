{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the endpoints that change a database's activities
('API.DatabaseHandlers.createActivitiesHandler',
'API.DatabaseHandlers.replaceActivityHandler' and
'API.DatabaseHandlers.editExchangesHandler').

What the HTTP layer owns, and what these pin, is the mapping from a refusal to
a status a client can act on: a request a caller can fix is a 400 carrying
every complaint at once, writing over a row that exists is a 409, addressing
one that does not is a 404, and a database the engine only reads is refused
outright. The validation itself is "AuthorSpec"'s subject, not this file's.
-}
module ActivityWriteHandlerSpec (spec) where

import Control.Concurrent.STM (atomically, modifyTVar')
import Control.Monad (void)
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List (isInfixOf)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.UUID as UUID
import qualified Data.Vector.Unboxed as U
import Servant (ServerError, errBody, errHTTPCode, runHandler)
import System.Directory (createDirectoryIfMissing)
import System.Environment (setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import API.DatabaseHandlers (createActivitiesHandler, editExchangesHandler, replaceActivityHandler)
import Database.Author (authoredActivityUUID, authoredProductUUID)

import API.Types (
    ActivityInput (..),
    ActivityWriteRequest (..),
    ActivityWriteResponse (..),
    BioExchangeAPI (..),
    ExchangeEditRequest (..),
    ExchangeEditResponse (..),
    ExchangeSelectorAPI (..),
    SetAmountAPI (..),
    TechInputAPI (..),
 )
import App.Env (AppEnv (..), runApp)
import Config (DatabaseConfig (..), defaultConfig)
import Control.Exception (bracket_)
import Database (buildDatabaseWithMatrices)
import Database.Manager (CachePolicy (..), DatabaseManager (..), LoadedDatabase (..), initDatabaseManager)
import Database.Upload (DatabaseFormat (..))
import SharedSolver (createSharedSolver)
import Types (
    Activity (..),
    AllocationKey (..),
    BioDirection (..),
    BiosphereFlow (..),
    BuildInputs (..),
    Compartment (..),
    Database (..),
    Exchange (..),
    GeographyPolicy (..),
    LocationSource (..),
    Medium (..),
    SimpleDatabase (..),
    SparseTriple (..),
    SupplierClaim (..),
    TechRole (..),
    TechnosphereFlow (..),
    UUID,
    Unit (..),
    noProperties,
 )
import UnitConversion (defaultUnitConfig)

spec :: Spec
spec = do
    writingSpec
    editingSpec

writingSpec :: Spec
writingSpec = describe "writing activities over HTTP" $ do
    it "writes a batch and answers with the process ids it can now be asked for" $
        withWritableDb $ \env -> do
            res <- create env "authored" [cheese]
            case res of
                Left err -> expectationFailure ("expected a successful write: " <> showErr err)
                Right written -> do
                    awpTransient written `shouldBe` False
                    -- The identity is the pair, and it is the caller's handle
                    -- on the row from here on.
                    awpWritten written `shouldBe` [keyOf cheese]

    it "reports every defect of a batch at once, as a 400" $
        withWritableDb $ \env -> do
            let broken =
                    cheese
                        { aiProductUnit = "furlong"
                        , aiInputs = [TechInputAPI{tiProvider = "nope", tiAmount = 1, tiUnit = Nothing, tiComment = Nothing}]
                        }
            res <- create env "authored" [broken]
            case res of
                Right _ -> expectationFailure "expected the batch to be refused"
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    -- Both complaints travel together: fixing a ten-line
                    -- inventory should take one round trip, not ten.
                    bodyOf err `shouldSatisfy` isInfixOf "unknown provider"
                    bodyOf err `shouldSatisfy` isInfixOf "unknown unit \"furlong\""

    it "answers 409 when the batch would write over a row that already exists" $
        withWritableDb $ \env -> do
            _ <- create env "authored" [cheese]
            res <- create env "authored" [cheese]
            case res of
                Right _ -> expectationFailure "expected the second write to conflict"
                Left err -> do
                    errHTTPCode err `shouldBe` 409
                    bodyOf err `shouldSatisfy` isInfixOf "Already in this database"

    it "answers 404 when rewriting an activity the database does not hold" $
        withWritableDb $ \env -> do
            res <- replace env "authored" (keyOf cheese) cheese
            case res of
                Right _ -> expectationFailure "expected the rewrite to find nothing"
                Left err -> errHTTPCode err `shouldBe` 404

    it "rewrites in place once the activity is there" $
        withWritableDb $ \env -> do
            _ <- create env "authored" [cheese]
            res <- replace env "authored" (keyOf cheese) cheese{aiProductAmount = 2}
            case res of
                Left err -> expectationFailure ("expected the rewrite to land: " <> showErr err)
                Right written -> awpWritten written `shouldBe` [keyOf cheese]

    it "accepts the bare activity UUID as the PUT address, like every read does" $
        -- A process is addressed by the canonical pair or by the bare
        -- activity UUID when it is unambiguous; the handle a caller got from
        -- a read must work on the rewrite too.
        withWritableDb $ \env -> do
            _ <- create env "authored" [cheese]
            let bare = UUID.toText (authoredActivityUUID (aiName cheese) (aiLocation cheese))
            res <- replace env "authored" bare cheese{aiProductAmount = 2}
            case res of
                Left err -> expectationFailure ("expected the rewrite to land: " <> showErr err)
                Right written -> awpWritten written `shouldBe` [keyOf cheese]

    it "refuses a body whose identity is not the one the path addresses" $
        -- Identity comes from the name, location, product and unit, so a body
        -- that would land elsewhere must not be written to a second row.
        withWritableDb $ \env -> do
            _ <- create env "authored" [cheese]
            res <- replace env "authored" (keyOf cheese) cheese{aiName = "butter, at dairy"}
            case res of
                Right _ -> expectationFailure "expected the mismatch to be refused"
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    bodyOf err `shouldSatisfy` isInfixOf "Identity comes from the name"

    it "refuses to author into a database the engine only reads" $
        -- Configured databases are background data the whole installation
        -- shares; authoring belongs in a database of one's own.
        withDb (\name dataDir -> (uploadedConfig name dataDir){dcIsUploaded = False}) $ \env -> do
            res <- create env "authored" [cheese]
            case res of
                Right _ -> expectationFailure "expected authoring into a configured database to be refused"
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    bodyOf err `shouldSatisfy` isInfixOf "reads from its configuration"

    it "refuses an empty batch instead of rewriting the database for nothing" $
        -- Committing re-serializes the whole database and rebuilds its
        -- solver; an empty batch would pay all of that to write nothing.
        withWritableDb $ \env -> do
            res <- create env "authored" []
            case res of
                Right _ -> expectationFailure "expected the empty batch to be refused"
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    bodyOf err `shouldSatisfy` isInfixOf "nothing to write"

    it "answers 404 for a database that is not loaded" $
        withWritableDb $ \env -> do
            res <- create env "no-such-db" [cheese]
            case res of
                Right _ -> expectationFailure "expected a 404"
                Left err -> errHTTPCode err `shouldBe` 404

    it "refuses a biosphere line that names its flow twice, or not at all" $
        withWritableDb $ \env -> do
            let bio flow name = cheese{aiBiosphere = [emission{beFlow = flow, beName = name}]}
            both <- create env "authored" [bio (Just (UUID.toText co2Id)) (Just "Carbon dioxide")]
            void both `shouldSatisfy` refusedWith "not both"
            neither <- create env "authored" [bio Nothing Nothing]
            void neither `shouldSatisfy` refusedWith "needs either a flow identifier"

    it "reports every shape defect of a batch at once, each naming its activity" $
        -- Shape complaints accumulate like the validator's do: a batch with a
        -- bad direction in one activity and a doubly-named flow in another is
        -- fixed in one round trip, and each line says where to look.
        withWritableDb $ \env -> do
            let badDirection =
                    cheese
                        { aiName = "yogurt, at dairy"
                        , aiBiosphere = [emission{beName = Just "Carbon dioxide", beDirection = "released"}]
                        }
                doublyNamed =
                    cheese
                        { aiName = "butter, at dairy"
                        , aiBiosphere = [emission{beFlow = Just (UUID.toText co2Id), beName = Just "Carbon dioxide"}]
                        }
            res <- create env "authored" [badDirection, doublyNamed]
            case res of
                Right _ -> expectationFailure "expected the batch to be refused"
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    bodyOf err `shouldSatisfy` isInfixOf "yogurt, at dairy {FR}: unknown biosphere direction"
                    bodyOf err `shouldSatisfy` isInfixOf "butter, at dairy {FR}: "

    it "refuses a named biosphere flow that comes without its unit" $
        -- The unit is half of a named flow's identity, so it cannot be
        -- defaulted; the complaint says so instead of "unknown unit \"\"".
        withWritableDb $ \env -> do
            res <- create env "authored" [cheese{aiBiosphere = [emission{beName = Just "Dust", beUnit = Nothing}]}]
            void res `shouldSatisfy` refusedWith "\"Dust\" needs a unit"

editingSpec :: Spec
editingSpec = describe "editing an activity's exchanges over HTTP" $ do
    -- The fixture's milk activity is an imported row: no description mints its
    -- identity, so a PUT cannot address it and only this endpoint can change it.
    it "changes the inventory of an activity the database already holds" $
        withWritableDb $ \env -> do
            res <- edit env "authored" supplierPid noEdits{eerRemove = [bioSelector co2Id]}
            case res of
                Left err -> expectationFailure ("expected the edit to land: " <> showErr err)
                Right report -> do
                    eepRemoved report `shouldBe` [1]
                    eepAdded report `shouldBe` 0
                    eepTransient report `shouldBe` False

    it "reads a document that states only the lists it uses" $
        -- The same leniency the assistant tool has: a document that only
        -- removes a line does not say four times that it does nothing else.
        case decode "{\"remove\":[{\"kind\":\"biosphere\",\"flow\":\"f-1\"}]}" of
            Nothing -> expectationFailure "expected the document to be read"
            Just req -> do
                map esKind (eerRemove req) `shouldBe` ["biosphere"]
                let unstated =
                        [ length (eerSetAmounts req)
                        , length (eerAddInputs req)
                        , length (eerAddBiosphere req)
                        , length (eerAddWasteOutputs req)
                        ]
                unstated `shouldBe` [0, 0, 0, 0]

    it "reports each part of an edit separately" $
        withWritableDb $ \env -> do
            let addMilk = TechInputAPI{tiProvider = supplierPid, tiAmount = 2, tiUnit = Nothing, tiComment = Nothing}
            res <-
                edit env "authored" supplierPid $
                    noEdits
                        { eerSetAmounts = [SetAmountAPI{saSelect = bioSelector co2Id, saAmount = 3}]
                        , eerAddInputs = [addMilk]
                        }
            case res of
                Left err -> expectationFailure ("expected the edit to land: " <> showErr err)
                Right report -> do
                    eepRemoved report `shouldBe` []
                    eepAmountsSet report `shouldBe` [1]
                    eepAdded report `shouldBe` 1

    it "accepts the bare activity UUID as the address, like every read does" $
        withWritableDb $ \env -> do
            res <- edit env "authored" (UUID.toText supplierActId) noEdits{eerRemove = [bioSelector co2Id]}
            case res of
                Left err -> expectationFailure ("expected the edit to land: " <> showErr err)
                Right report -> eepRemoved report `shouldBe` [1]

    it "answers 404 for an activity the database does not hold" $
        withWritableDb $ \env -> do
            res <- edit env "authored" (keyOf cheese) noEdits{eerRemove = [bioSelector co2Id]}
            case res of
                Right _ -> expectationFailure "expected the edit to find nothing"
                Left err -> errHTTPCode err `shouldBe` 404

    it "answers 400 when a selector reaches no exchange" $
        -- Never a no-op: an edit that changed nothing reads exactly like one
        -- that worked, and only the caller can tell the two apart.
        withWritableDb $ \env -> do
            res <- edit env "authored" supplierPid noEdits{eerRemove = [bioSelector (mkUUID 998)]}
            case res of
                Right _ -> expectationFailure "expected the selector to be refused"
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    bodyOf err `shouldSatisfy` isInfixOf "matches no exchange"

    it "refuses a selector whose kind and key disagree" $
        withWritableDb $ \env -> do
            let confused = (bioSelector co2Id){esKind = "input"}
            res <- edit env "authored" supplierPid noEdits{eerRemove = [confused]}
            void res `shouldSatisfy` refusedWith "names its provider"

    it "refuses to edit a database the engine only reads" $
        withDb (\name dataDir -> (uploadedConfig name dataDir){dcIsUploaded = False}) $ \env -> do
            res <- edit env "authored" supplierPid noEdits{eerRemove = [bioSelector co2Id]}
            case res of
                Right _ -> expectationFailure "expected editing a configured database to be refused"
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    bodyOf err `shouldSatisfy` isInfixOf "reads from its configuration"

    it "refuses an edit that names nothing instead of rebuilding for nothing" $
        withWritableDb $ \env -> do
            res <- edit env "authored" supplierPid noEdits
            case res of
                Right _ -> expectationFailure "expected the empty edit to be refused"
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    bodyOf err `shouldSatisfy` isInfixOf "nothing to change"

-- ---------------------------------------------------------------------------
-- Driving the handlers
-- ---------------------------------------------------------------------------

create :: AppEnv -> Text -> [ActivityInput] -> IO (Either ServerError ActivityWriteResponse)
create env dbName activities =
    runHandler (runApp env (createActivitiesHandler dbName (ActivityWriteRequest activities)))

replace :: AppEnv -> Text -> Text -> ActivityInput -> IO (Either ServerError ActivityWriteResponse)
replace env dbName processId body =
    runHandler (runApp env (replaceActivityHandler dbName processId body))

edit :: AppEnv -> Text -> Text -> ExchangeEditRequest -> IO (Either ServerError ExchangeEditResponse)
edit env dbName processId body =
    runHandler (runApp env (editExchangesHandler dbName processId body))

-- | An edit that names nothing, for a test to fill in the one part it is about.
noEdits :: ExchangeEditRequest
noEdits =
    ExchangeEditRequest
        { eerRemove = []
        , eerSetAmounts = []
        , eerAddInputs = []
        , eerAddBiosphere = []
        , eerAddWasteOutputs = []
        }

bioSelector :: UUID -> ExchangeSelectorAPI
bioSelector flowId =
    ExchangeSelectorAPI{esKind = "biosphere", esProvider = Nothing, esFlow = Just (UUID.toText flowId)}

refusedWith :: String -> Either ServerError () -> Bool
refusedWith needle = either ((needle `isInfixOf`) . bodyOf) (const False)

bodyOf :: ServerError -> String
bodyOf = BSL.unpack . errBody

showErr :: ServerError -> String
showErr err = show (errHTTPCode err) <> " " <> bodyOf err

-- | An environment holding one writable EcoSpold 2 database called @authored@.
withWritableDb :: (AppEnv -> IO ()) -> IO ()
withWritableDb = withDb uploadedConfig

withDb :: (Text -> FilePath -> DatabaseConfig) -> (AppEnv -> IO ()) -> IO ()
withDb mkConfig act =
    withSystemTempDirectory "volca-write" $ \root ->
        bracket_ (setEnv "VOLCA_DATA_DIR" root) (unsetEnv "VOLCA_DATA_DIR") $ do
            let dataDir = root </> "uploads" </> "databases" </> "authored" </> "data"
            createDirectoryIfMissing True dataDir
            dbm <- initDatabaseManager defaultConfig NoCache
            db <- buildFixture
            solver <- createSharedSolver "authored" (triplesOf db) (fromIntegral (dbActivityCount db))
            let config = mkConfig "authored" dataDir
                loaded = LoadedDatabase{ldDatabase = db, ldSharedSolver = solver, ldConfig = config}
            atomically $ do
                modifyTVar' (dmLoadedDbs dbm) (M.insert "authored" loaded)
                modifyTVar' (dmAvailableDbs dbm) (M.insert "authored" config)
            act
                AppEnv
                    { aeDbManager = dbm
                    , aeMaxTreeDepth = 10
                    , aePassword = Nothing
                    , aeHostingConfig = Nothing
                    , aeClassificationPresets = []
                    , aeDataVersion = Nothing
                    }
  where
    triplesOf db = [(fromIntegral i, fromIntegral j, v) | SparseTriple i j v <- U.toList (dbTechnosphereTriples db)]

uploadedConfig :: Text -> FilePath -> DatabaseConfig
uploadedConfig name dataDir =
    DatabaseConfig
        { dcName = name
        , dcDisplayName = name
        , dcPath = dataDir
        , dcDescription = Nothing
        , dcLoad = True
        , dcDefault = False
        , dcDepends = []
        , dcLocationAliases = M.empty
        , dcFormat = Just EcoSpold2
        , dcIsUploaded = True
        , dcDeletable = True
        , dcGeographyPolicy = GeoGlobal
        , dcAllocation = Declared
        , dcPatches = []
        , dcSource = Nothing
        }

-- ---------------------------------------------------------------------------
-- Request fixtures
-- ---------------------------------------------------------------------------

cheese :: ActivityInput
cheese =
    ActivityInput
        { aiName = "cheese, at dairy"
        , aiLocation = "FR"
        , aiDescription = []
        , aiProductName = "cheese"
        , aiProductAmount = 1
        , aiProductUnit = "kg"
        , aiInputs = [TechInputAPI{tiProvider = supplierPid, tiAmount = 8, tiUnit = Nothing, tiComment = Nothing}]
        , aiBiosphere = []
        , aiWasteOutputs = []
        }

emission :: BioExchangeAPI
emission =
    BioExchangeAPI
        { beFlow = Nothing
        , beName = Nothing
        , beCompartment = Just "air"
        , beSubCompartment = Nothing
        , beDirection = "emission"
        , beAmount = 0.5
        , beUnit = Just "kg"
        , beComment = Nothing
        }

{- | The identity the engine will mint for an activity input, the same
function 'Database.Author' uses, restated here as the caller's expectation
rather than borrowed, so a change to the minting rule fails this test loudly.
-}
keyOf :: ActivityInput -> Text
keyOf ai =
    UUID.toText (authoredActivityUUID (aiName ai) (aiLocation ai))
        <> "_"
        <> UUID.toText (authoredProductUUID (aiProductName ai) (aiProductUnit ai))

-- ---------------------------------------------------------------------------
-- Database fixture
-- ---------------------------------------------------------------------------

buildFixture :: IO Database
buildFixture = do
    r <-
        buildDatabaseWithMatrices
            (BuildInputs defaultUnitConfig mempty Declared [])
            SimpleDatabase
                { sdbActivities = M.singleton (supplierActId, supplierProdId) milkActivity
                , sdbTechFlows = M.singleton supplierProdId milkFlow
                , sdbBioFlows = M.singleton co2Id co2Flow
                , sdbWasteFlows = M.empty
                , sdbUnits = unitTable
                }
    either (fail . show) pure r

mkUUID :: Int -> UUID
mkUUID n = UUID.fromWords64 (fromIntegral n) 0

supplierActId, supplierProdId, co2Id, kgUnitId :: UUID
supplierActId = mkUUID 1
supplierProdId = mkUUID 2
co2Id = mkUUID 3
kgUnitId = mkUUID 10

supplierPid :: Text
supplierPid = UUID.toText supplierActId <> "_" <> UUID.toText supplierProdId

unitTable :: M.Map UUID Unit
unitTable = M.singleton kgUnitId Unit{unitId = kgUnitId, unitName = "kg", unitSymbol = "kg", unitComment = ""}

milkFlow :: TechnosphereFlow
milkFlow =
    TechnosphereFlow
        { tfId = supplierProdId
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
        , bfCompartment = Just Compartment{compartmentName = Air, compartmentSub = Nothing}
        }

milkActivity :: Activity
milkActivity =
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
                { techFlowId = supplierProdId
                , techAmount = 1.0
                , techUnitId = kgUnitId
                , techRole = ReferenceProduct
                , techActivityLinkId = Just supplierActId
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
