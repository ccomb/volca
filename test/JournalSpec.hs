{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the edit journal ("Database.Journal").

The journal is what makes an edit outlive the process without rewriting the
database's own files, so two properties carry the whole design: what it writes
it can read back, and replaying it produces the database the edit produced.
The rest is about the ways a file can be wrong, and each of them says so
rather than quietly losing an edit.
-}
module JournalSpec (spec) where

import Control.Monad (void)
import Data.Aeson (Value, decodeStrict, encode, toJSON)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text, isInfixOf)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Database (buildDatabaseWithMatrices)
import Database.Author (
    AuthorContext (..),
    AuthoredActivity (..),
    AuthoredExchange (..),
    ExchangeEdit (..),
    ExchangeSelector (..),
    FlowRef (..),
    ResolvedInsert (..),
    validateAuthored,
 )
import Database.Journal (
    JournalEvent (..),
    JournalOp (..),
    appendEvent,
    journalPath,
    readJournal,
    replayJournal,
 )
import Database.Rebuild (renderKey)
import Types (
    Activity (..),
    AllocationKey (..),
    BioDirection (..),
    BiosphereFlow (..),
    BuildInputs (..),
    Compartment (..),
    Database (..),
    Exchange (..),
    LocationSource (..),
    Medium (..),
    SimpleDatabase (..),
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
    fixture <- runIO buildFixture
    let ctx = AuthorContext{acDb = fixture, acDeps = [], acUnitConfig = defaultUnitConfig}
        cheeseKey = mintedKey fixture cheese

    describe "Database.Journal (codec)" $ do
        it "writes the shape it documents" $
            -- A golden on names and nesting rather than on bytes: what has to
            -- stay stable is the vocabulary on disk, not the order aeson
            -- happens to serialize an object in.
            Just (toJSON (event (Created [cheese] ["a_b"])))
                `shouldBe` (decodeStrict expectedCreateJSON :: Maybe Value)

        it "writes the shape an inventory edit documents" $
            Just (toJSON (event editedInventory))
                `shouldBe` (decodeStrict expectedEditJSON :: Maybe Value)

        it "reads back every event it writes" $
            -- One event per operation, and one exchange per kind, so a field
            -- lost in the codec cannot hide behind the others.
            mapM_ (\written -> decodeStrict (BL.toStrict (encode written)) `shouldBe` Just written) richEvents

    describe "Database.Journal (file)" $ do
        it "reads a database that has never been edited as an empty journal" $
            withSystemTempDirectory "journal" $ \home ->
                readJournal home `shouldReturn` Right []

        it "appends and reads back in order" $
            withSystemTempDirectory "journal" $ \home -> do
                mapM_ (appendOrFail home . jeOp) richEvents
                read' <- readJournal home
                fmap (map jeOp) read' `shouldBe` Right (map jeOp richEvents)

        it "refuses a version it does not read" $
            withJournalFile
                ["{\"v\":2,\"at\":\"now\",\"op\":\"delete\",\"targets\":[]}"]
                (`shouldSatisfy` failsWith "version 2")

        it "refuses a line it cannot parse, naming it" $
            withJournalFile
                [deleteLine, "{ this is not json", deleteLine]
                (`shouldSatisfy` failsWith "line 2")

        it "drops a torn last line, because its edit was never acknowledged" $
            -- The line is written and flushed before the caller is told the
            -- edit happened, so a half-written final line belongs to an edit
            -- nobody knows about.
            withJournalFile
                [deleteLine, deleteLine, "{\"v\":1,\"at\":\"now\",\"op\":\"del"]
                (\result -> fmap length result `shouldBe` Right 2)

        it "never fuses an edit onto the torn tail a crash left behind" $
            -- A torn tail has no newline, so a blind append would glue the
            -- next line onto it, and that line's edit IS acknowledged. The
            -- append truncates the tail first: what is dropped is exactly the
            -- unacknowledged debris, never the edit being recorded.
            withSystemTempDirectory "journal" $ \home -> do
                appendOrFail home (Deleted ["a_b"])
                appendFile (journalPath home) "{\"v\":1,\"at\":\"now\",\"op\":\"del"
                appendOrFail home (Deleted ["c_d"])
                read' <- readJournal home
                fmap (map jeOp) read' `shouldBe` Right [Deleted ["a_b"], Deleted ["c_d"]]

    describe "Database.Journal (replay)" $ do
        it "applies a create" $
            case replayJournal ctx [event (Created [cheese] [cheeseKey])] of
                Left err -> expectationFailure ("replay: " <> show err)
                Right db -> processKeys db `shouldBe` S.insert cheeseKey (processKeys fixture)

        it "gives the same database every time it is replayed" $
            let journal = [event (Created [cheese] [cheeseKey]), event (Replaced cheeseKey cheese{aaProductAmount = 2})]
             in fmap processKeys (replayJournal ctx journal)
                    `shouldBe` fmap processKeys (replayJournal ctx journal)

        it "applies a delete, leaving the database it started from" $
            case replayJournal ctx [event (Created [cheese] [cheeseKey]), event (Deleted [cheeseKey])] of
                Left err -> expectationFailure ("replay: " <> show err)
                Right db -> processKeys db `shouldBe` processKeys fixture

        it "refuses an identity that no longer mints the same way" $
            -- The guard the whole file exists for: if minting ever moves, a
            -- replay must stop rather than land the activity somewhere else.
            replayOutcome ctx [event (Created [cheese] ["8f3c-not-what-this-mints"])]
                `shouldSatisfy` failsWith "now mints"

        it "names the event that failed" $
            replayOutcome ctx [event (Deleted [cheeseKey]), event (Created [cheese] [cheeseKey])]
                `shouldSatisfy` failsWith "journal event 1 (delete)"

        it "refuses a delete of something the database does not have" $
            replayOutcome ctx [event (Deleted [cheeseKey])]
                `shouldSatisfy` failsWith "Unknown process id"

        it "refuses a replace whose description no longer addresses its target" $
            replayOutcome ctx [event (Replaced supplierPid cheese)]
                `shouldSatisfy` failsWith "now mints"

        it "edits an inventory in place, leaving the activity's identity alone" $
            case replayJournal ctx [event (Edited supplierPid [(SetAmount (SelectBiosphere co2Id) 3, 1)])] of
                Left err -> expectationFailure ("replay: " <> show err)
                Right db -> do
                    processKeys db `shouldBe` processKeys fixture
                    emissionsOf db `shouldBe` [3]

        it "brings along the flow an added line introduces" $
            case replayJournal ctx [event (Edited supplierPid [(AddExchange methane, 1)])] of
                Left err -> expectationFailure ("replay: " <> show err)
                Right db -> map bfName (M.elems (dbBioFlows db)) `shouldSatisfy` elem "Methane"

        it "brings along the product flow an added dependency line needs" $ do
            -- The technosphere twin of the line above. A replay that dropped
            -- the flow would rebuild a database whose exchange links nothing,
            -- and the edit would score zero where the live one did not.
            depDb <- buildDepFixture
            let depCtx = ctx{acDeps = [depDb]}
                added = AuthoredTechInput depPid 2.0 (Just "kg") Nothing
            case replayJournal depCtx [event (Edited supplierPid [(AddExchange added, 1)])] of
                Left err -> expectationFailure ("replay: " <> show err)
                Right db -> M.member depProdId (dbTechFlows db) `shouldBe` True

        it "refuses an edit whose selectors no longer match what they matched" $
            -- The guard that makes a recorded edit safe to replay: the
            -- inventory it was made against must still be the one it named.
            replayOutcome ctx [event (Edited supplierPid [(RemoveExchange (SelectBiosphere co2Id), 2)])]
                `shouldSatisfy` failsWith "now match"

        it "refuses an edit of an activity the database does not have" $
            replayOutcome ctx [event (Edited cheeseKey [(RemoveExchange (SelectBiosphere co2Id), 1)])]
                `shouldSatisfy` failsWith "Unknown process id"

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

event :: JournalOp -> JournalEvent
event = JournalEvent "2026-08-03T09:12:41Z"

-- | Replay, keeping only what a failed assertion can print.
replayOutcome :: AuthorContext -> [JournalEvent] -> Either Text ()
replayOutcome ctx = void . replayJournal ctx

failsWith :: Text -> Either Text a -> Bool
failsWith needle = either (isInfixOf needle) (const False)

appendOrFail :: FilePath -> JournalOp -> IO ()
appendOrFail home op = appendEvent home op >>= either (fail . show) pure

{- | Run a check against a journal file written verbatim, so a line this
engine would never write can still be read.
-}
withJournalFile :: [BS.ByteString] -> (Either Text [JournalEvent] -> Expectation) -> Expectation
withJournalFile lines' check =
    withSystemTempDirectory "journal" $ \home -> do
        BS.writeFile (journalPath home) (BS.intercalate "\n" lines')
        readJournal home >>= check

deleteLine :: BS.ByteString
deleteLine = "{\"v\":1,\"at\":\"now\",\"op\":\"delete\",\"targets\":[\"" <> ascii supplierPid <> "\"]}"

-- | A 'Text' that is known to be ASCII, spliced into a JSON literal.
ascii :: Text -> BS.ByteString
ascii = BS.pack . T.unpack

{- | The identity an authored activity mints against a given database. A
fixture that does not resolve yields a key nothing matches, so the test that
depends on it fails on the assertion rather than here.
-}
mintedKey :: Database -> AuthoredActivity -> Text
mintedKey db activity = case validateAuthored ctx [activity] of
    Right ([resolved], _) -> renderKey (riKey resolved)
    Right _ -> "the fixture activity resolved to something other than one process"
    Left errs -> "the fixture activity does not resolve: " <> T.intercalate "; " errs
  where
    ctx = AuthorContext{acDb = db, acDeps = [], acUnitConfig = defaultUnitConfig}

processKeys :: Database -> S.Set Text
processKeys = S.fromList . map renderKey . V.toList . dbProcessIdTable

-- | Every biosphere amount in the database, so an edit's effect is visible.
emissionsOf :: Database -> [Double]
emissionsOf db =
    [ bioAmount ex
    | act <- V.toList (dbActivities db)
    , ex@BiosphereExchange{} <- exchanges act
    ]

-- ---------------------------------------------------------------------------
-- What the journal carries
-- ---------------------------------------------------------------------------

-- | An activity using every exchange kind the journal has to record.
cheese :: AuthoredActivity
cheese =
    AuthoredActivity
        { aaName = "cheese, at dairy"
        , aaLocation = "FR"
        , aaDescription = ["An authored activity."]
        , aaProductName = "cheese"
        , aaProductAmount = 1.0
        , aaProductUnit = "kg"
        , aaExchanges =
            [ AuthoredTechInput
                { atiProvider = supplierPid
                , atiAmount = 8.0
                , atiUnit = Just "kg"
                , atiComment = Just "milk in"
                }
            , AuthoredBio
                { abFlow = FlowById co2Id
                , abDirection = Emission
                , abAmount = 0.5
                , abUnit = Nothing
                , abComment = Nothing
                }
            , AuthoredBio
                { abFlow =
                    FlowByName
                        "Methane"
                        Compartment{compartmentName = Air, compartmentSub = Just "low population density"}
                        "kg"
                , abDirection = Emission
                , abAmount = 0.01
                , abUnit = Just "kg"
                , abComment = Nothing
                }
            ]
        }

-- | A biosphere line introducing a flow the fixture does not have.
methane :: AuthoredExchange
methane =
    AuthoredBio
        { abFlow =
            FlowByName
                "Methane"
                Compartment{compartmentName = Air, compartmentSub = Just "low population density"}
                "kg"
        , abDirection = Emission
        , abAmount = 0.01
        , abUnit = Just "kg"
        , abComment = Nothing
        }

usedOilOut :: AuthoredExchange
usedOilOut =
    AuthoredWasteOutput{awProvider = supplierPid, awAmount = 0.2, awUnit = Nothing, awComment = Nothing}

{- | One edit of every kind an inventory edit records, each with the number of
exchanges it matched, including a selector that named more than one line.
-}
editedInventory :: JournalOp
editedInventory =
    Edited
        supplierPid
        [ (RemoveExchange (SelectBiosphere co2Id), 2)
        , (RemoveExchange (SelectWaste supplierPid), 1)
        , (SetAmount (SelectInput supplierPid) 4, 1)
        , (AddExchange usedOilOut, 1)
        ]

richEvents :: [JournalEvent]
richEvents =
    [ event (Created [cheese] ["a_b"])
    , event (Replaced "a_b" cheese)
    , event (Deleted ["a_b", "c_d"])
    , event (Created [cheese{aaExchanges = [usedOilOut]}] ["a_b"])
    , event editedInventory
    ]

expectedCreateJSON :: BS.ByteString
expectedCreateJSON =
    "{\"v\":1,\"at\":\"2026-08-03T09:12:41Z\",\"op\":\"create\",\"written\":[\"a_b\"],\
    \\"activities\":[{\
    \\"name\":\"cheese, at dairy\",\"location\":\"FR\",\
    \\"description\":[\"An authored activity.\"],\
    \\"product\":{\"name\":\"cheese\",\"amount\":1,\"unit\":\"kg\"},\
    \\"exchanges\":[\
    \{\"kind\":\"input\",\"provider\":\""
        <> ascii supplierPid
        <> "\",\"amount\":8,\"unit\":\"kg\",\"comment\":\"milk in\"},\
           \{\"kind\":\"biosphere\",\"flow\":{\"id\":\""
        <> ascii (UUID.toText co2Id)
        <> "\"},\"direction\":\"emission\",\"amount\":0.5},\
           \{\"kind\":\"biosphere\",\"flow\":{\"name\":\"Methane\",\"compartment\":\"air\",\
           \\"sub_compartment\":\"low population density\",\"unit\":\"kg\"},\
           \\"direction\":\"emission\",\"amount\":0.01,\"unit\":\"kg\"}]}]}"

{- | An inventory edit as it lands on disk. A selector's flow is a bare
identifier where a written biosphere line nests an object, because a selector
names a flow that exists and never introduces one.
-}
expectedEditJSON :: BS.ByteString
expectedEditJSON =
    "{\"v\":1,\"at\":\"2026-08-03T09:12:41Z\",\"op\":\"edit\",\"target\":\""
        <> ascii supplierPid
        <> "\",\"edits\":[\
           \{\"matched\":2,\"edit\":\"remove\",\
           \\"select\":{\"kind\":\"biosphere\",\"flow\":\""
        <> ascii (UUID.toText co2Id)
        <> "\"}},\
           \{\"matched\":1,\"edit\":\"remove\",\
           \\"select\":{\"kind\":\"waste\",\"provider\":\""
        <> ascii supplierPid
        <> "\"}},\
           \{\"matched\":1,\"edit\":\"set\",\
           \\"select\":{\"kind\":\"input\",\"provider\":\""
        <> ascii supplierPid
        <> "\"},\"amount\":4},\
           \{\"matched\":1,\"edit\":\"add\",\
           \\"exchange\":{\"kind\":\"waste\",\"provider\":\""
        <> ascii supplierPid
        <> "\",\"amount\":0.2}}]}"

-- ---------------------------------------------------------------------------
-- Fixture: one supplier producing milk in kg, emitting CO2
-- ---------------------------------------------------------------------------

{- | A second database, sharing nothing with the fixture but its unit table:
the dependency a replayed edit consumes from.
-}
buildDepFixture :: IO Database
buildDepFixture = do
    built <-
        buildDatabaseWithMatrices
            (BuildInputs defaultUnitConfig mempty Declared [])
            SimpleDatabase
                { sdbActivities = M.singleton (depActId, depProdId) depActivity
                , sdbTechFlows = M.singleton depProdId wheatFlow
                , sdbBioFlows = M.empty
                , sdbWasteFlows = M.empty
                , sdbUnits = unitTable
                }
    either (fail . show) pure built

buildFixture :: IO Database
buildFixture = do
    built <-
        buildDatabaseWithMatrices
            (BuildInputs defaultUnitConfig mempty Declared [])
            SimpleDatabase
                { sdbActivities = M.singleton (supplierActId, supplierProdId) supplierActivity
                , sdbTechFlows = M.singleton supplierProdId milkFlow
                , sdbBioFlows = M.singleton co2Id co2Flow
                , sdbWasteFlows = M.empty
                , sdbUnits = unitTable
                }
    either (fail . show) pure built

mkUUID :: Int -> UUID
mkUUID n = UUID.fromWords64 (fromIntegral n) 0

supplierActId, supplierProdId, co2Id, depActId, depProdId, kgUnitId :: UUID
supplierActId = mkUUID 1
supplierProdId = mkUUID 2
co2Id = mkUUID 3
depActId = mkUUID 4
depProdId = mkUUID 5
kgUnitId = mkUUID 10

supplierPid, depPid :: Text
supplierPid = renderKey (supplierActId, supplierProdId)
depPid = renderKey (depActId, depProdId)

wheatFlow :: TechnosphereFlow
wheatFlow =
    TechnosphereFlow
        { tfId = depProdId
        , tfName = "wheat"
        , tfUnitId = kgUnitId
        , tfSynonyms = M.empty
        , tfCAS = Nothing
        , tfSubstanceId = Nothing
        }

-- | The dependency's one activity: a reference product and nothing else.
depActivity :: Activity
depActivity =
    supplierActivity
        { activityName = "wheat production"
        , exchanges =
            [ TechnosphereExchange
                { techFlowId = depProdId
                , techAmount = 1.0
                , techUnitId = kgUnitId
                , techRole = ReferenceProduct
                , techActivityLinkId = Just depActId
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

unitTable :: M.Map UUID Unit
unitTable =
    M.singleton kgUnitId Unit{unitId = kgUnitId, unitName = "kg", unitSymbol = "kg", unitComment = ""}

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
        , bfCAS = Nothing
        , bfSubstanceId = Nothing
        , bfCompartment = Just Compartment{compartmentName = Air, compartmentSub = Nothing}
        }

supplierActivity :: Activity
supplierActivity =
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
