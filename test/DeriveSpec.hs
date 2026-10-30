{-# LANGUAGE OverloadedStrings #-}

{- | Tests for deriving a database ('Database.Edit.deriveDatabase').

Deriving is not copying. A parent is its source's files read again, each block
of several products kept whole; a child is a parent divided by a system model.
What that has to get right is checked here:

* a child's blocks are divided by its model's property instead of by the
  shares its source declares, and the inventory of every product follows;
* an exception of the model divides the blocks it takes by its own property,
  and one that divides none of them is refused, named;
* a derivation that divides nothing is refused and leaves nothing behind;
* a child is asked of a parent, and a parent of a database read from its
  files, never anything else;
* the model survives a restart, which is the one thing a child cannot work out
  for itself -- it owns no files, so nothing else on disk says how it is divided;
* a child is divided from the parent as it stands in memory: a price edited on
  the parent reaches the child, before and after a restart, an input consuming
  a coproduct finds the piece that now produces it, and a block of one product
  is not cut to its share twice;
* a parent answers an input buying a coproduct of a block it keeps whole
  itself, never from another database, and keeps that link through a delete.
-}
module DeriveSpec (spec) where

import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import qualified Data.ByteString.Char8 as BS
import Data.Either (fromLeft, isRight)
import Data.List (sort, sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (isJust, isNothing)
import Data.Text (Text, isInfixOf)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Config (DatabaseConfig (..), defaultConfig)
import Database.Author (ExchangeEdit (..))
import Database.Child (childOf)
import Database.Edit (DeleteRequest (..), Derivation (..), databaseHistory, deleteActivitiesInDB, deriveDatabase, editExchanges)
import Database.ExportSupplier (noDependencies, unnamedWarnings)
import Database.History (Carried (..), HistoryLine (..), Journal (..), Provenance (..), changesReport)
import Data.Foldable (toList)
import Database.Journal (noteFrom)
import Database.Loader (GapReport (..), countUnlinkedExchanges, gapReportForLoaded)
import Database.Manager (
    CachePolicy (..),
    DatabaseManager (..),
    LoadedDatabase (..),
    databaseModification,
    getDatabase,
    initDatabaseManager,
    loadDatabase,
    processTraced,
    unloadDatabase,
 )
import Database.SystemModel (Traced (..), parseSystemModel, takenProcesses, traceOf, tracedRule)
import Database.Upload (DatabaseFormat (..))
import TestHelpers (modelDividingBy, withScratchDataDir, withinTolerance)
import Types (
    Activity (..),
    AllocationProperty (..),
    Database (..),
    Exchange (..),
    ExchangeFormula (..),
    FileReading (..),
    GeographyPolicy (..),
    Licence (..),
    Modification (..),
    Parameter (..),
    ParameterDefinition (..),
    ProcessRef (..),
    Reading (..),
    SimpleDatabase (..),
    StatedAmount (..),
    TechRole (..),
    TechnosphereFlow (..),
    exchangeAmount,
    exchangeFlowId,
    exchangeIsProductOutput,
    exchangeIsReference,
    processRefText,
    toSimpleDatabase,
    withFormula,
 )
import UnitConversion (defaultUnitConfig)

spec :: Spec
spec = around_ withScratchDataDir $ describe "Database.Edit.deriveDatabase" $ do
    it "divides a block by the model's property instead of by the declared shares" $
        withSource coproductCSV $ \manager -> do
            _ <- either (fail . show) pure =<< deriveDatabase manager source parentName AsParent
            db <- either (fail . show) (pure . ldDatabase . fst) =<< deriveDatabase manager parentName "cheese-wet" (AsChild (modelDividingBy WetMass))
            V.length (dbActivities db) `shouldBe` 2
            M.size (dbActivityUUIDIndex db) `shouldBe` 1
            -- The source declares 51 / 49 and would put 5.1 kg of milk behind
            -- the cheese. 1 kg of cheese beside 3 kg of whey is 25 / 75, and
            -- the whole inventory follows the property that divided the block.
            milkAmounts db `shouldSatisfy` near [2.5, 7.5]

    it "names the blocks a property could not divide, and registers nothing" $
        withSource farmCSV $ \manager -> do
            _ <- either (fail . show) pure =<< deriveDatabase manager source parentName AsParent
            derived <- deriveDatabase manager parentName "cheese-dry" (AsChild (modelDividingBy DryMass))
            case derived of
                Right _ -> expectationFailure "expected a refusal, the source states no dry mass"
                Left err -> do
                    err `shouldSatisfy` isInfixOf "divided 0 of the 1 blocks"
                    available <- readTVarIO (dmAvailableDbs manager)
                    M.member "cheese-dry" available `shouldBe` False

    it "reads a block whole as a parent, the cheese and the whey in one process" $
        withSource coproductCSV $ \manager -> do
            derived <- deriveDatabase manager source parentName AsParent
            db <- either (fail . show) (pure . ldDatabase . fst) derived
            V.length (dbActivities db) `shouldBe` 1
            [length (filter exchangeIsProductOutput (exchanges a)) | a <- V.toList (dbActivities db)] `shouldBe` [2]

    it "refuses a parent of a source with no block of several products" $
        withSource singleProductCSV $ \manager -> do
            derived <- deriveDatabase manager source "steel-parent" AsParent
            fromLeft "" derived `shouldSatisfy` isInfixOf "kept no process of several products whole"

    it "refuses a parent of a parent, and a child of a database that is not one" $
        withSource coproductCSV $ \manager -> do
            _ <- either (fail . show) pure =<< deriveDatabase manager source parentName AsParent
            again <- deriveDatabase manager parentName "dairy-again" AsParent
            fromLeft "" again `shouldSatisfy` isInfixOf "is already a parent"
            orphan <- deriveDatabase manager source "dairy-orphan" (AsChild (modelDividingBy Price))
            fromLeft "" orphan `shouldSatisfy` isInfixOf "is not a parent"
            available <- readTVarIO (dmAvailableDbs manager)
            filter (`M.member` available) ["dairy-again", "dairy-orphan"] `shouldBe` []

    it "is still divided by its model after a restart" $
        withSource coproductCSV $ \manager -> do
            _ <- either (fail . show) pure =<< deriveDatabase manager source parentName AsParent
            _ <- either (fail . show) pure =<< deriveDatabase manager parentName "cheese-wet" (AsChild (modelDividingBy WetMass))
            -- A boot rebuilds every uploaded database's configuration from its
            -- meta.toml and the model file beside it. Without them the child
            -- would come back with nothing to divide it.
            rebooted <- initDatabaseManager defaultConfig NoCache
            available <- readTVarIO (dmAvailableDbs rebooted)
            fmap dcReading (M.lookup "cheese-wet" available) `shouldBe` Just (ByModel (modelDividingBy WetMass))
            fmap dcSource (M.lookup "cheese-wet" available) `shouldBe` Just (Just parentName)
            loadedDb rebooted "cheese-wet" >>= (`shouldSatisfy` near [2.5, 7.5]) . milkAmounts

    describe "a child of a parent" $ do
        it "divides the parent's blocks by the prices their declared shares became" $
            withParent $ \manager -> do
                child <- deriveChild manager
                -- Deduced from 51 / 49, the prices give the declared shares back.
                inputOf "Cow milk" child `shouldSatisfy` near [5.1, 4.9]

        it "cuts a block of one product to its declared share once, not twice" $
            withParent $ \manager -> do
                child <- deriveChild manager
                -- The parent already holds 2 kg of the 4 the butter's 50 % names.
                inputOf "Cream" child `shouldSatisfy` near [2]

        it "links an input consuming a coproduct to the piece producing it" $
            withParent $ \manager -> do
                parent <- loadedDb manager parentName
                child <- deriveChild manager
                -- The parent links the whey to its dairy block under the key
                -- the child gives the piece making the whey, and the link holds.
                wheyLinks parent `shouldBe` [True]
                wheySuppliers child `shouldBe` wheySuppliers parent
                wheySuppliers child `shouldSatisfy` all (`elem` V.toList (dbProcessIdTable child))

        it "answers its own coproducts, though the source it reads is loaded beside it" $
            withSource farmCSV $ \manager -> do
                _ <- loadedDb manager source
                _ <- either (fail . show) pure =<< deriveDatabase manager source parentName AsParent
                parent <- loadedDb manager parentName
                -- The source sells the whey cut to its declared share. A
                -- parent drawing on it would mix the two readings.
                length (dbCrossDBLinks parent) `shouldBe` 0
                wheyLinks parent `shouldBe` [True]
                -- Left: the milk and the cream, which nothing here makes.
                countUnlinkedExchanges (toSimpleDatabase parent) `shouldBe` 2
                grUnresolvedEdges (gapReportForLoaded parentName parent) `shouldBe` 2
                unnamedWarnings noDependencies (toSimpleDatabase parent) `shouldBe` []

        it "keeps the coproduct links when another process of the parent is deleted" $
            withParent $ \manager -> do
                parent <- loadedDb manager parentName
                deleted <- deleteActivitiesInDB manager parentName (deleteIds [processOf parent "Creamery"])
                isRight deleted `shouldBe` True
                loadedDb manager parentName >>= (`shouldBe` [True]) . wheyLinks

        it "loads a parent whose coproduct is in another unit than the reference" $
            withSource steamCSV $ \manager -> do
                derived <- deriveDatabase manager source parentName AsParent
                parent <- either (fail . show) (pure . ldDatabase . fst) derived
                -- Steam in MJ beside acid in kg: drawn on the block's column,
                -- the kilograms would not convert and the load would fail.
                [isJust (techActivityLinkId ex) | a <- V.toList (dbActivities parent), ex <- snd (partitionProducts a), named parent "Steam" ex]
                    `shouldBe` [True]

        it "divides the blocks an exception takes by the exception's property" $
            withParent $ \manager -> do
                parent <- loadedDb manager parentName
                child <- deriveWith manager (exceptionTaking "wet mass" (dairyActivity parent))
                -- The dairy by mass, 25 / 75; the rest priced as by default.
                inputOf "Cow milk" child `shouldSatisfy` near [2.5, 7.5]
                inputOf "Cream" child `shouldSatisfy` near [2]

        it "traces each piece to its block and the rule that gave its share, and a share the source declared" $
            withParent $ \manager -> do
                parent <- loadedDb manager parentName
                let model = exceptionTaking "wet mass" (dairyActivity parent)
                child <- deriveWith manager model
                -- The pig farm's 100 % cuts nothing and is not listed.
                traced model parent child
                    `shouldReturn` [ ("Butter", Just 50, "share declared by the source")
                                   , ("Cheese", Just 25, "wet mass, exception: Dairy by its solids.")
                                   , ("Whey", Just 75, "wet mass, exception: Dairy by its solids.")
                                   ]

        it "names the parent's process each piece was cut from: the block, or the process itself when it is no block's" $
            withParent $ \manager -> do
                parent <- loadedDb manager parentName
                let model = exceptionTaking "wet mass" (dairyActivity parent)
                child <- deriveWith manager model
                tracedTo model parent child
                    `shouldReturn` [("Butter", ["Butter"]), ("Cheese", ["Cheese", "Whey"]), ("Whey", ["Cheese", "Whey"])]

        it "gives one process's row of the sheet, nothing for a database read from its files, and says so when the parent is unloaded" $
            withParent $ \manager -> do
                parent <- loadedDb manager parentName
                _ <- deriveWith manager (exceptionTaking "wet mass" (dairyActivity parent))
                child <- maybe (fail "the child is not loaded") pure =<< getDatabase manager childName
                rowOf manager child "Cheese" `shouldReturn` Right (Just (parentName, "Cheese", Just 25))
                rowOf manager child "Pig" `shouldReturn` Right Nothing
                sourceDb <- either (fail . show) (pure . fst) =<< loadDatabase manager source
                fromFiles <- processTraced manager sourceDb (head (V.toList (dbProcessIdTable (ldDatabase sourceDb))))
                either (const False) isNothing fromFiles `shouldBe` True
                _ <- unloadDatabase manager parentName
                unloaded <- rowOf manager child "Cheese"
                unloaded `shouldSatisfy` either ("is not loaded" `isInfixOf`) (const False)

        it "traces a piece divided by the model's default key" $
            withParent $ \manager -> do
                parent <- loadedDb manager parentName
                let model = T.unlines ["name = \"Priced\"", "version = \"1.0.0\"", "[allocation]", "default = \"price\""]
                child <- deriveWith manager model
                (filter (\(product, _, _) -> product == "Cheese") <$> traced model parent child)
                    `shouldReturn` [("Cheese", Just 51, "price, default key")]

        it "refuses an exception that divides none of the blocks it takes, named" $
            withParent $ \manager -> do
                parent <- loadedDb manager parentName
                model <- either (fail . show) pure (parseSystemModel (exceptionTaking "dry mass" (dairyActivity parent)))
                derived <- deriveDatabase manager parentName childName (AsChild model)
                fromLeft "" derived `shouldSatisfy` isInfixOf "the dry mass exception \"Dairy by its solids.\" divides none of the 1 blocks it takes"

        it "divides by the dry masses stated on the parent, where the exception takes the block" $
            withParent $ \manager -> do
                parent <- loadedDb manager parentName
                let (cheese, whey) = dairyProducts parent
                    dried flow amount = SetProperty DryMass flow (Just (StatedAmount "kg" amount))
                edited <- editExchanges manager parentName Nothing (dairyActivity parent) [dried cheese 0.6, dried whey 0.065]
                isRight edited `shouldBe` True
                child <- deriveWith manager (exceptionTaking "dry mass" (dairyActivity parent))
                -- 1 kg at 0.6 kg dry beside 3 kg at 0.065: 0.6 / 0.795 and 0.195 / 0.795 of the milk.
                inputOf "Cow milk" child `shouldSatisfy` near [6 / 0.795, 1.95 / 0.795]

        it "weighs a wet mass stated on the parent before the product's own amount" $
            withParent $ \manager -> do
                parent <- loadedDb manager parentName
                let (cheese, _) = dairyProducts parent
                edited <- editExchanges manager parentName Nothing (dairyActivity parent) [SetProperty WetMass cheese (Just (StatedAmount "kg" 2))]
                isRight edited `shouldBe` True
                child <- deriveWith manager (exceptionTaking "wet mass" (dairyActivity parent))
                -- 1 unit of cheese stated at 2 kg beside 3 kg of whey: 2 / 5 and 3 / 5 of the milk.
                inputOf "Cow milk" child `shouldSatisfy` near [4, 6]

        it "refuses an edit of the parent while a child divided from it is loaded" $
            withParent $ \manager -> do
                _ <- deriveChild manager
                parent <- loadedDb manager parentName
                let (cheese, _) = dairyProducts parent
                -- The child would keep the old shares until loaded again.
                edited <- editExchanges manager parentName Nothing (dairyActivity parent) [SetProperty Price cheese (Just (StatedAmount "EUR" 4))]
                isRight edited `shouldBe` False

        it "follows a price edited on the parent, and still does after a restart" $
            withParent $ \manager -> do
                parent <- loadedDb manager parentName
                let (cheese, whey) = dairyProducts parent
                    priced flow amount = SetProperty Price flow (Just (StatedAmount "EUR" amount))
                edited <- editExchanges manager parentName Nothing (dairyActivity parent) [priced cheese 4, priced whey 1]
                isRight edited `shouldBe` True
                -- 1 kg at 4 beside 3 kg at 1: 4 / 7 and 3 / 7 of the milk.
                let repriced = near [40 / 7, 30 / 7]
                deriveChild manager >>= (`shouldSatisfy` repriced) . inputOf "Cow milk"
                rebooted <- initDatabaseManager defaultConfig NoCache
                loadedDb rebooted childName >>= (`shouldSatisfy` repriced) . inputOf "Cow milk"

        it "says the price edited on the parent in the child's history and changes report, the child edited" $
            withParent $ \manager -> do
                parent <- loadedDb manager parentName
                let (cheese, whey) = dairyProducts parent
                    priced flow amount = SetProperty Price flow (Just (StatedAmount "EUR" amount))
                edited <- editExchanges manager parentName (noteFrom "Prices 2026") (dairyActivity parent) [priced cheese 4, priced whey 1]
                isRight edited `shouldBe` True
                _ <- deriveChild manager
                configs <- readTVarIO (dmAvailableDbs manager)
                childConfig <- maybe (fail "no child") pure (M.lookup childName configs)
                modificationEdited <$> databaseModification configs childConfig `shouldReturn` True
                history <- either (fail . T.unpack) pure =<< databaseHistory manager childName
                journalLines (carriedOwn history) `shouldBe` []
                [(dcName (journalOf journal), map hlNote (journalLines journal)) | journal <- toList (carriedFromParent history)] `shouldBe` [(parentName, [noteFrom "Prices 2026"])]
                let report = changesReport (Provenance childName "dairy" Nothing Nothing) (fmap dcName <$> history)
                report `shouldSatisfy` isInfixOf ("Changes made on " <> parentName <> ", the parent this database is divided from")
                report `shouldSatisfy` isInfixOf "Reason: Prices 2026"

    describe "a child substituting a coproduct" $ do
        it "leaves the whole block to the cheese, credited with the milk powder the whey displaces" $
            withSource wheyCSV $ \manager -> do
                parent <- parentOf manager
                child <- deriveWith manager (substitutingWhey parent (snd (dairyProducts parent)))
                -- The cheese carries the 10 kg of milk, and 3 kg of whey at
                -- 0.25 kg of powder each come off it.
                inputOf "Cow milk" child `shouldSatisfy` near [10]
                avoidedOf "Milk powder" child `shouldSatisfy` near [0.75]
                creditedTo child `shouldBe` [powderPlant parent]

        it "has a line that bought the coproduct buy what it displaces, converted to the coproduct's unit first" $
            withSource wheyCSV $ \manager -> do
                parent <- parentOf manager
                child <- deriveWith manager (substitutingWhey parent (snd (dairyProducts parent)))
                -- 2000 g of whey are 2 kg, at 0.25 kg of powder each.
                wheyLinks child `shouldBe` []
                let bought = [ex | a <- V.toList (dbActivities child), ex <- snd (partitionProducts a), named child "Milk powder" ex, not (isAvoided ex)]
                map exchangeAmount bought `shouldSatisfy` near [0.5]
                map techActivityLinkId bought `shouldBe` [Just (powderPlant parent)]

        it "takes the reference product too, the whey left to carry the block" $
            withSource wheyCSV $ \manager -> do
                parent <- parentOf manager
                child <- deriveWith manager (substitutingWhey parent (fst (dairyProducts parent)))
                [exchangeAmount ex | a <- V.toList (dbActivities child), ex <- exchanges a, exchangeIsReference ex, named child "Whey" ex] `shouldBe` [3]
                inputOf "Cow milk" child `shouldSatisfy` near [10]

        it "keeps a name the substituted row carried as a parameter of the block, at the amount the parent stated" $
            withSource wheyCSV $ \manager -> do
                parent <- parentOf manager
                model <- either (fail . show) pure (parseSystemModel (substitutingWhey parent (snd (dairyProducts parent))))
                let nameWhey ex
                        | named parent "Whey" ex && exchangeIsProductOutput ex = withFormula (Just ExchangeFormula{efName = Just "whey_out", efText = "3"}) ex
                        | otherwise = ex
                    namedParent = parent{dbActivities = V.map (\a -> a{exchanges = map nameWhey (exchanges a)}) (dbActivities parent)}
                child <- either (fail . T.unpack) pure =<< childOf defaultUnitConfig model namedParent
                [parameterDefinition p | a <- V.toList (dbActivities child), Just p <- [M.lookup "whey_out" (activityParameters a)]]
                    `shouldBe` [CalculatedParameter "3" (Just 3)]

        it "traces the cheese to the dairy block, whole, by the substitution's reason" $
            withSource wheyCSV $ \manager -> do
                parent <- parentOf manager
                let model = substitutingWhey parent (snd (dairyProducts parent))
                child <- deriveWith manager model
                -- The pig farm and the powder plant make one product each, at 100 %: nothing to trace.
                traced model parent child `shouldReturn` [("Cheese", Just 100, "substitution: Whey replaces milk powder in pig feed, by its solids.")]
                tracedTo model parent child `shouldReturn` [("Cheese", ["Cheese", "Whey"])]

        it "is still substituted after a restart" $
            withSource wheyCSV $ \manager -> do
                parent <- parentOf manager
                _ <- deriveWith manager (substitutingWhey parent (snd (dairyProducts parent)))
                rebooted <- initDatabaseManager defaultConfig NoCache
                loadedDb rebooted childName >>= (`shouldSatisfy` near [0.75]) . avoidedOf "Milk powder"

    describe "a child setting a method parameter" $ do
        it "takes the recycling credit away at A = 1, and leaves the parent as it was" $
            withSource recyclingCSV $ \manager -> do
                parent <- parentOf manager
                child <- deriveWith manager (settingA "")
                -- (1 - A) * R2 at A = 0.2 and R2 = 0.7: 0.56 kg recycled, as much virgin carton avoided.
                linesOf "Carton recycling" parent `shouldSatisfy` near [0.56]
                avoidedOf "Virgin carton" parent `shouldSatisfy` near [0.56]
                linesOf "Carton recycling" child `shouldSatisfy` near [0]
                avoidedOf "Virgin carton" child `shouldSatisfy` near [0]
                linesOf "Landfill" child `shouldSatisfy` near [0.3]
                [parameterDefinition p | a <- V.toList (dbActivities child), activityName a == "Carton end of life", Just p <- [M.lookup "A" (activityParameters a)]]
                    `shouldBe` [InputParameter 1 Nothing]

        it "traces each process the parameter moved, the pieces of the block it moved too" $
            withSource recyclingCSV $ \manager -> do
                parent <- parentOf manager
                let model = settingA ""
                child <- deriveWith manager model
                traced model parent child
                    -- At A = 1 the rejects weigh nothing, so the bale carries the whole sorting.
                    `shouldReturn` [ ("Bale", Just 100, "price, default key; parameter A = 1 \"Cut-off.\"")
                                   , ("Carton waste", Just 100, "as the parent has it; parameter A = 1 \"Cut-off.\"")
                                   , ("Rejects", Just 0, "price, default key; parameter A = 1 \"Cut-off.\"")
                                   ]

        it "refuses a parameter that moves no process, one a process computes, or one it spells twice" $
            withSource recyclingCSV $ \manager -> do
                parent <- parentOf manager
                let refusedBy p text = do
                        m <- either (fail . show) pure (parseSystemModel text)
                        fromLeft "" <$> childOf defaultUnitConfig m p
                refusedBy parent (T.replace "name = \"A\"" "name = \"Z\"" (settingA "")) >>= (`shouldSatisfy` isInfixOf "moves no process of the parent: none declares it")
                refusedBy parent (T.replace "value = 1" "value = 0.2" (settingA "")) >>= (`shouldSatisfy` isInfixOf "already has it at that value")
                let onCarton f a
                        | activityName a == "Carton end of life" = a{activityParameters = f (activityParameters a)}
                        | otherwise = a
                refusedBy parent{dbActivities = V.map (onCarton (M.adjust (\p -> p{parameterDefinition = CalculatedParameter "0.1 * 2" (Just 0.2)}) "A")) (dbActivities parent)} (settingA "")
                    >>= (`shouldSatisfy` isInfixOf "Carton end of life computes A by a formula")
                refusedBy parent{dbActivities = V.map (onCarton (\ps -> maybe ps (\p -> M.insert "a" p ps) (M.lookup "A" ps))) (dbActivities parent)} (settingA "")
                    >>= (`shouldSatisfy` isInfixOf "Carton end of life spells A twice, as A and a")

        it "refuses a substitution of a row the parameter empties, on the model's page as in the child" $
            withSource recyclingCSV $ \manager -> do
                parent <- parentOf manager
                let text = settingA (substitutingRejects parent)
                model <- either (fail . show) pure (parseSystemModel text)
                either (T.intercalate "; " . NE.toList) (const "") (takenProcesses model parent) `shouldSatisfy` isInfixOf "displaces nothing"
                childOf defaultUnitConfig model parent >>= (`shouldSatisfy` isInfixOf "displaces nothing") . fromLeft ""

        it "is still set after a restart" $
            withSource recyclingCSV $ \manager -> do
                _ <- parentOf manager
                _ <- deriveWith manager (settingA "")
                rebooted <- initDatabaseManager defaultConfig NoCache
                loadedDb rebooted childName >>= (`shouldSatisfy` near [0]) . linesOf "Carton recycling"

{- | The milk behind each product of the block, ordered by the product's own
amount, which is what tells the 1 kg of cheese from the 3 kg of whey.
-}
milkAmounts :: Database -> [Double]
milkAmounts db = map snd (sortOn fst [(amountOf products a, amountOf inputs a) | a <- V.toList (dbActivities db)])
  where
    amountOf :: (Activity -> [Exchange]) -> Activity -> Double
    amountOf pick = sum . map exchangeAmount . pick

    products, inputs :: Activity -> [Exchange]
    products = filter exchangeIsProductOutput . exchanges
    inputs = filter (not . exchangeIsProductOutput) . exchanges

{- | The amounts of one input, a line per process consuming it, ordered by the
amount of the process's products, which tells the cheese from the whey.
-}
inputOf :: Text -> Database -> [Double]
inputOf flowName db =
    map snd $
        sortOn
            fst
            [ (sum (map exchangeAmount products), sum (map exchangeAmount consumed))
            | a <- V.toList (dbActivities db)
            , let (products, inputs) = partitionProducts a
                  consumed = filter (named db flowName) inputs
            , not (null consumed)
            ]

-- | Whether each whey input of the database has a supplier.
wheyLinks :: Database -> [Bool]
wheyLinks db =
    [ isJust (techActivityLinkId ex)
    | a <- V.toList (dbActivities db)
    , ex <- snd (partitionProducts a)
    , named db "Whey" ex
    ]

-- | The process each whey input of the database names as its supplier.
wheySuppliers :: Database -> [(UUID.UUID, UUID.UUID)]
wheySuppliers db =
    [ (supplier, exchangeFlowId ex)
    | a <- V.toList (dbActivities db)
    , ex <- snd (partitionProducts a)
    , named db "Whey" ex
    , Just supplier <- [techActivityLinkId ex]
    ]

-- | The key of the process of this name, as a request names it.
processOf :: Database -> Text -> Text
processOf db name = head [processRefText (uncurry ProcessRef key) | (key, a) <- zip (V.toList (dbProcessIdTable db)) (V.toList (dbActivities db)), activityName a == name]

deleteIds :: [Text] -> DeleteRequest
deleteIds ids =
    DeleteRequest
        { drName = Nothing
        , drLocation = Nothing
        , drProduct = Nothing
        , drClassifications = []
        , drExactName = False
        , drKeep = []
        , drExtra = []
        , drIds = Just ids
        , drNote = Nothing
        }

partitionProducts :: Activity -> ([Exchange], [Exchange])
partitionProducts a = (filter exchangeIsProductOutput (exchanges a), filter (not . exchangeIsProductOutput) (exchanges a))

named :: Database -> Text -> Exchange -> Bool
named db flowName ex = (tfName <$> M.lookup (exchangeFlowId ex) (dbTechFlows db)) == Just flowName

-- | The cheese and the whey of the dairy block, told apart by their amounts.
dairyProducts :: Database -> (UUID.UUID, UUID.UUID)
dairyProducts db = case sortOn exchangeAmount (filter exchangeIsProductOutput (exchanges (dairyBlock db))) of
    [cheese, whey] -> (exchangeFlowId cheese, exchangeFlowId whey)
    other -> error ("the dairy block has " <> show (length other) <> " products")

-- | The dairy block, the parent's one process of several products.
dairyBlock :: Database -> Activity
dairyBlock db = head [a | a <- V.toList (dbActivities db), length (filter exchangeIsProductOutput (exchanges a)) > 1]

-- | The dairy block addressed by its activity, as an edit names it.
dairyActivity :: Database -> Text
dairyActivity db = head [UUID.toText activityId | (i, a) <- zip [0 ..] (V.toList (dbActivities db)), activityName a == activityName (dairyBlock db), let (activityId, _) = dbProcessIdTable db V.! i]

parentName, childName :: Text
parentName = "dairy-parent"
childName = "dairy-price"

{- | 'source' with three blocks, and its parent derived under @none@: a
derivation has a home, so the parent can be edited.
-}
withParent :: (DatabaseManager -> IO ()) -> IO ()
withParent k = withSource farmCSV $ \manager -> do
    _ <- either (fail . show) pure =<< deriveDatabase manager source parentName AsParent
    k manager

deriveChild :: DatabaseManager -> IO Database
deriveChild manager = either (fail . show) (pure . ldDatabase . fst) =<< deriveDatabase manager parentName childName (AsChild (modelDividingBy Price))

-- | The child divided by the model the text of a file describes.
deriveWith :: DatabaseManager -> Text -> IO Database
deriveWith manager text = do
    model <- either (fail . show) pure (parseSystemModel text)
    either (fail . show) (pure . ldDatabase . fst) =<< deriveDatabase manager parentName childName (AsChild model)

-- | A model pricing every block but one, divided by @key@.
exceptionTaking :: Text -> Text -> Text
exceptionTaking key activity =
    T.unlines
        [ "name = \"Dairy apart\""
        , "version = \"1.0.0\""
        , "[allocation]"
        , "default = \"price\""
        , "[[allocation.exceptions]]"
        , "key = \"" <> key <> "\""
        , "reason = \"Dairy by its solids.\""
        , "processes = [\"" <> activity <> "\"]"
        ]

-- | The parent of the source, derived under @none@.
parentOf :: DatabaseManager -> IO Database
parentOf manager = either (fail . show) (pure . ldDatabase . fst) =<< deriveDatabase manager source parentName AsParent

-- | The milk powder plant, by activity, as a link names it.
powderPlant :: Database -> UUID.UUID
powderPlant db = head [activityId | ((activityId, _), a) <- zip (V.toList (dbProcessIdTable db)) (V.toList (dbActivities db)), activityName a == "Milk powder plant"]

-- | The amounts of a product the database's processes avoid.
avoidedOf :: Text -> Database -> [Double]
avoidedOf flowName db = [exchangeAmount ex | a <- V.toList (dbActivities db), ex <- exchanges a, isAvoided ex, named db flowName ex]

-- | The processes the database's avoided lines credit.
creditedTo :: Database -> [UUID.UUID]
creditedTo db = [supplier | a <- V.toList (dbActivities db), ex <- exchanges a, isAvoided ex, Just supplier <- [techActivityLinkId ex]]

isAvoided :: Exchange -> Bool
isAvoided ex = case ex of
    TechnosphereExchange{techRole = AvoidedProduct} -> True
    _ -> False

-- | A model pricing every block, and substituting this product of the dairy by milk powder, 0.25 kg per kg.
substitutingWhey :: Database -> UUID.UUID -> Text
substitutingWhey parent product =
    T.unlines
        [ "name = \"Whey feeds\""
        , "version = \"1.0.0\""
        , "[allocation]"
        , "default = \"price\""
        , "[[substitutions]]"
        , "coproduct = \"" <> dairyActivity parent <> "_" <> UUID.toText product <> "\""
        , "replaces = \"" <> processOf parent "Milk powder plant" <> "\""
        , "ratio = 0.25"
        , "reason = \"Whey replaces milk powder in pig feed, by its solids.\""
        ]

-- | A model pricing every block and setting A to 1, with these lines after.
settingA :: Text -> Text
settingA rest =
    T.unlines
        [ "name = \"Cut-off\""
        , "version = \"1.0.0\""
        , "[allocation]"
        , "default = \"price\""
        , "[[method_parameters]]"
        , "name = \"A\""
        , "value = 1"
        , "reason = \"Cut-off.\""
        ]
        <> rest

-- | A substitution of the sorting block's rejects, which A = 1 empties, by virgin carton.
substitutingRejects :: Database -> Text
substitutingRejects parent =
    T.unlines
        [ "[[substitutions]]"
        , "coproduct = \"" <> T.takeWhile (/= '_') (processOf parent "Sorting") <> "_" <> UUID.toText (rejectsOf parent) <> "\""
        , "replaces = \"" <> processOf parent "Virgin carton plant" <> "\""
        , "ratio = 1"
        , "reason = \"Rejects replace carton.\""
        ]

-- | The rejects of the sorting block, by product.
rejectsOf :: Database -> UUID.UUID
rejectsOf db = head [exchangeFlowId ex | a <- V.toList (dbActivities db), activityName a == "Sorting", ex <- exchanges a, exchangeIsProductOutput ex, named db "Rejects" ex]

-- | The amounts of every line of this flow the database's processes buy.
linesOf :: Text -> Database -> [Double]
linesOf flowName db = [exchangeAmount ex | a <- V.toList (dbActivities db), ex <- exchanges a, not (exchangeIsProductOutput ex), not (isAvoided ex), named db flowName ex]

-- | The child's trace back to its parent under the model of this text: product, share and rule, by product.
traced :: Text -> Database -> Database -> IO [(Text, Maybe Double, Text)]
traced text parent child = do
    model <- either (fail . show) pure (parseSystemModel text)
    onParent <- either (fail . show) pure (takenProcesses model parent)
    pure (sortOn (\(product, _, _) -> product) [(trProduct t, trShare t, tracedRule t) | t <- traceOf onParent parent child])

{- | The parent's process each traced piece names, by product: the products
that process makes in the parent, so a piece of a block names the whole block
and a process no block holds names itself.
-}
tracedTo :: Text -> Database -> Database -> IO [(Text, [Text])]
tracedTo text parent child = do
    model <- either (fail . show) pure (parseSystemModel text)
    onParent <- either (fail . show) pure (takenProcesses model parent)
    let simple = toSimpleDatabase parent
        productsOf act = sort [maybe "?" tfName (M.lookup (exchangeFlowId ex) (sdbTechFlows simple)) | ex <- exchanges act, exchangeIsProductOutput ex]
    pure (sortOn fst [(trProduct t, maybe [] productsOf (M.lookup (trParent t) (sdbActivities simple))) | t <- traceOf onParent parent child])

{- | The row of the sheet 'processTraced' gives for the child's process making
this product: the parent it names, the product and the share.
-}
rowOf :: DatabaseManager -> LoadedDatabase -> Text -> IO (Either Text (Maybe (Text, Text, Maybe Double)))
rowOf manager child product = do
    let simple = toSimpleDatabase (ldDatabase child)
        makes act = [() | ex <- exchanges act, exchangeIsReference ex, (tfName <$> M.lookup (exchangeFlowId ex) (sdbTechFlows simple)) == Just product]
    key <- case [k | (k, act) <- M.toList (sdbActivities simple), not (null (makes act))] of
        [k] -> pure k
        keys -> fail ("the child makes " <> T.unpack product <> " in " <> show (length keys) <> " processes")
    fmap (fmap (\(parent, t) -> (dcName parent, trProduct t, trShare t))) <$> processTraced manager child key

loadedDb :: DatabaseManager -> Text -> IO Database
loadedDb manager name = either (fail . show) (pure . ldDatabase . fst) =<< loadDatabase manager name

-- | Whether every amount lands on the one expected of it.
near :: [Double] -> [Double] -> Bool
near expected actual =
    length expected == length actual
        && and (zipWith (withinTolerance 1e-9) expected actual)

source :: Text
source = "dairy"

{- | A manager holding one SimaPro CSV under 'source', unloaded: deriving
reads the files, so the source need not be in memory.
-}
withSource :: BS.ByteString -> (DatabaseManager -> IO ()) -> IO ()
withSource csv k = withSystemTempDirectory "derive-source" $ \dir -> do
    let csvPath = dir </> "source.csv"
    BS.writeFile csvPath csv
    manager <- initDatabaseManager defaultConfig NoCache
    atomically $ modifyTVar' (dmAvailableDbs manager) (M.insert source (sourceConfig csvPath))
    k manager

sourceConfig :: FilePath -> DatabaseConfig
sourceConfig csvPath =
    DatabaseConfig
        { dcName = source
        , dcDisplayName = "Dairy"
        , dcPath = csvPath
        , dcDescription = Nothing
        , dcLoad = False
        , dcDefault = False
        , dcDepends = []
        , dcLocationAliases = M.empty
        , dcFormat = Just SimaProCSV
        , dcIsUploaded = False
        , dcDeletable = False
        , dcGeographyPolicy = GeoGlobal
        , dcReading = FromFiles ReadDeclared
        , dcPatches = []
        , dcSource = Nothing
        , dcLicence = LicenceUnstated
        , dcRelease = Nothing
        , dcRequires = []
        , dcAdded = Nothing
        }

{- | One block of two products, declared 51 / 49, whose masses say 25 / 75:
the disagreement a property key exists to settle.
-}
coproductCSV :: BS.ByteString
coproductCSV =
    simaProFile
        [ "Products"
        , "Cheese;kg;1;51;not defined;material;"
        , "Whey;kg;3;49;not defined;material;"
        , ""
        , "Materials/fuels"
        , "Cow milk;kg;10;Undefined;0;0;0;"
        ]

-- | One block of one product: nothing a key could divide.
singleProductCSV :: BS.ByteString
singleProductCSV =
    simaProFile
        [ "Products"
        , "Steel;kg;1;100;not defined;material;"
        , ""
        , "Materials/fuels"
        , "Iron ore;kg;2;Undefined;0;0;0;"
        ]

{- | The dairy block of 'coproductCSV', a pig farm feeding on its whey, and a
butter block that declares 50 % of its one product.
-}
farmCSV :: BS.ByteString
farmCSV =
    simaProBlocks
        [ ("Dairy", ["Products", "Cheese;kg;1;51;not defined;material;", "Whey;kg;3;49;not defined;material;", "", "Materials/fuels", "Cow milk;kg;10;Undefined;0;0;0;"])
        , ("Pig farm", ["Products", "Pig;kg;1;100;not defined;material;", "", "Materials/fuels", "Whey;kg;2;Undefined;0;0;0;"])
        , ("Creamery", ["Products", "Butter;kg;1;50;not defined;material;", "", "Materials/fuels", "Cream;kg;4;Undefined;0;0;0;"])
        ]

{- | The dairy block, a pig farm buying its whey by the gram, and a milk
powder plant: what a substitution of the whey has to credit and reroute.
-}
wheyCSV :: BS.ByteString
wheyCSV =
    simaProBlocks
        [ ("Dairy", ["Products", "Cheese;kg;1;51;not defined;material;", "Whey;kg;3;49;not defined;material;", "", "Materials/fuels", "Cow milk;kg;10;Undefined;0;0;0;"])
        , ("Pig farm", ["Products", "Pig;kg;1;100;not defined;material;", "", "Materials/fuels", "Whey;g;2000;Undefined;0;0;0;"])
        , ("Milk powder plant", ["Products", "Milk powder;kg;1;100;not defined;material;", "", "Materials/fuels", "Skimmed milk;kg;8;Undefined;0;0;0;"])
        ]

{- | A nitric acid plant selling its steam, in MJ beside the acid's kilograms,
and a dryer buying that steam.
-}
steamCSV :: BS.ByteString
steamCSV =
    simaProBlocks
        [ ("Nitric acid plant", ["Products", "Nitric acid;kg;1;80;not defined;chemicals;", "Steam;MJ;5;20;not defined;energy;", "", "Materials/fuels", "Ammonia;kg;0.3;Undefined;0;0;0;"])
        , ("Dryer", ["Products", "Dried grain;kg;1;100;not defined;material;", "", "Materials/fuels", "Steam;MJ;2;Undefined;0;0;0;"])
        ]

{- | A carton end of life writing the recycling formula as Agribalyse does,
its credit and its recycling read through A, a plant making the virgin carton
it credits, and a sorting block whose rejects A empties at 1.
-}
recyclingCSV :: BS.ByteString
recyclingCSV =
    simaProBlocks
        [
            ( "Carton end of life"
            ,
                [ "Products"
                , "Carton waste;kg;1;100;not defined;material;"
                , ""
                , "Avoided products"
                , "Virgin carton;kg;(1-A)*R2;Undefined;0;0;0;"
                , ""
                , "Materials/fuels"
                , "Carton recycling;kg;(1-A)*R2;Undefined;0;0;0;"
                , "Landfill;kg;1-R2;Undefined;0;0;0;"
                , ""
                , "Input parameters"
                , "A;0.2;Undefined;1;0;0;No;"
                , "R2;0.7;Undefined;1;0;0;No;"
                ]
            )
        , ("Virgin carton plant", ["Products", "Virgin carton;kg;1;100;not defined;material;", "", "Materials/fuels", "Wood;kg;2;Undefined;0;0;0;"])
        ,
            ( "Sorting"
            ,
                [ "Products"
                , "Bale;kg;1;50;not defined;material;"
                , "Rejects;kg;(1-A)*2;50;not defined;material;"
                , ""
                , "Materials/fuels"
                , "Carton waste;kg;3;Undefined;0;0;0;"
                , ""
                , "Input parameters"
                , "A;0.2;Undefined;1;0;0;No;"
                ]
            )
        ]

-- | One SimaPro process block, wrapped in the header its parser expects.
simaProFile :: [BS.ByteString] -> BS.ByteString
simaProFile body = simaProBlocks [("Dairy", body)]

-- | Named SimaPro process blocks, after the header its parser expects.
simaProBlocks :: [(BS.ByteString, [BS.ByteString])] -> BS.ByteString
simaProBlocks blocks =
    BS.intercalate "\r\n" (["{SimaPro 9.6.0.1}", "{CSV separator: semicolon}", "{Decimal separator: .}", ""] ++ concatMap block blocks)
  where
    block (name, body) =
        ["Process", "", "Category type", "material", "", "Process name", name, "", "Type", "Unit process", "", "Geography", "GLO", ""]
            ++ body
            ++ ["", "End", ""]
