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
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.Text (Text, isInfixOf)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Config (DatabaseConfig (..), defaultConfig)
import Database.Author (ExchangeEdit (..))
import Database.Edit (DeleteRequest (..), Derivation (..), deleteActivitiesInDB, deriveDatabase, editExchanges)
import Database.ExportSupplier (noDependencies, unnamedWarnings)
import Database.Loader (GapReport (..), countUnlinkedExchanges, gapReportForLoaded)
import Database.Manager (
    CachePolicy (..),
    DatabaseManager (..),
    LoadedDatabase (..),
    initDatabaseManager,
    loadDatabase,
 )
import Database.SystemModel (parseSystemModel)
import Database.Upload (DatabaseFormat (..))
import TestHelpers (modelDividingBy, withScratchDataDir, withinTolerance)
import Types (
    Activity (..),
    AllocationProperty (..),
    Database (..),
    Exchange,
    FileReading (..),
    GeographyPolicy (..),
    Licence (..),
    ProcessRef (..),
    Reading (..),
    StatedAmount (..),
    TechnosphereFlow (..),
    exchangeAmount,
    exchangeFlowId,
    exchangeIsProductOutput,
    processRefText,
    techActivityLinkId,
    toSimpleDatabase,
 )

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
        withSource coproductCSV $ \manager -> do
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

        it "refuses an exception that divides none of the blocks it takes, named" $
            withParent $ \manager -> do
                parent <- loadedDb manager parentName
                model <- either (fail . show) pure (parseSystemModel (exceptionTaking "dry mass" (dairyActivity parent)))
                derived <- deriveDatabase manager parentName childName (AsChild model)
                fromLeft "" derived `shouldSatisfy` isInfixOf "the dry mass exception \"Dairy by its solids.\" divides none of the 1 blocks it takes"

        it "refuses an edit of the parent while a child divided from it is loaded" $
            withParent $ \manager -> do
                _ <- deriveChild manager
                parent <- loadedDb manager parentName
                let (cheese, _) = dairyProducts parent
                -- The child would keep the old shares until loaded again.
                edited <- editExchanges manager parentName Nothing (dairyActivity parent) [SetPrice cheese (Just (StatedAmount "EUR" 4))]
                isRight edited `shouldBe` False

        it "follows a price edited on the parent, and still does after a restart" $
            withParent $ \manager -> do
                parent <- loadedDb manager parentName
                let (cheese, whey) = dairyProducts parent
                    priced flow amount = SetPrice flow (Just (StatedAmount "EUR" amount))
                edited <- editExchanges manager parentName Nothing (dairyActivity parent) [priced cheese 4, priced whey 1]
                isRight edited `shouldBe` True
                -- 1 kg at 4 beside 3 kg at 1: 4 / 7 and 3 / 7 of the milk.
                let repriced = near [40 / 7, 30 / 7]
                deriveChild manager >>= (`shouldSatisfy` repriced) . inputOf "Cow milk"
                rebooted <- initDatabaseManager defaultConfig NoCache
                loadedDb rebooted childName >>= (`shouldSatisfy` repriced) . inputOf "Cow milk"

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

{- | A nitric acid plant selling its steam, in MJ beside the acid's kilograms,
and a dryer buying that steam.
-}
steamCSV :: BS.ByteString
steamCSV =
    simaProBlocks
        [ ("Nitric acid plant", ["Products", "Nitric acid;kg;1;80;not defined;chemicals;", "Steam;MJ;5;20;not defined;energy;", "", "Materials/fuels", "Ammonia;kg;0.3;Undefined;0;0;0;"])
        , ("Dryer", ["Products", "Dried grain;kg;1;100;not defined;material;", "", "Materials/fuels", "Steam;MJ;2;Undefined;0;0;0;"])
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
