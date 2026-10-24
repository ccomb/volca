{-# LANGUAGE OverloadedStrings #-}

{- | Tests for re-keying a database ('Database.Edit.deriveDatabase').

Deriving is not copying: a copy shares the value the source's own key
produced, and the key is what produced it, so a derived database is read from
the source's files again. What that has to get right is checked here:

* a block is divided by the property instead of by the shares its source
  declares, and the inventory of every product follows;
* a key that divides nothing is refused and leaves nothing behind: such a
  load is its source under a second name, at the price of a second database
  in memory and a second cache on disk;
* so is the key the source already reads under, which is the duplicate no
  count of divided blocks can see;
* the key survives a restart, which is the one thing a derived database
  cannot work out for itself -- it owns no files, so nothing else on disk
  says its shares are not the ones its source declares;
* a child of a parent read under @none@ is divided from the parent as it
  stands in memory: a price edited on the parent reaches the child, before
  and after a restart, an input consuming a coproduct finds the piece that
  now produces it, and a block of one product is not cut to its share twice.
-}
module DeriveSpec (spec) where

import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import qualified Data.ByteString.Char8 as BS
import Data.Either (fromLeft, isRight)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.Text (Text, isInfixOf)
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Config (DatabaseConfig (..), defaultConfig)
import Database.Author (ExchangeEdit (..))
import Database.Edit (deriveDatabase, editExchanges)
import Database.Manager (
    CachePolicy (..),
    DatabaseManager (..),
    LoadedDatabase (..),
    initDatabaseManager,
    loadDatabase,
 )
import Database.Upload (DatabaseFormat (..))
import TestHelpers (withScratchDataDir, withinTolerance)
import Types (
    Activity (..),
    AllocationKey (..),
    AllocationProperty (..),
    Database (..),
    Exchange,
    GeographyPolicy (..),
    Licence (..),
    StatedAmount (..),
    TechnosphereFlow (..),
    exchangeAmount,
    exchangeFlowId,
    exchangeIsProductOutput,
    techActivityLinkId,
 )

spec :: Spec
spec = around_ withScratchDataDir $ describe "Database.Edit.deriveDatabase" $ do
    it "divides a block by the property instead of by the declared shares" $
        withSource coproductCSV $ \manager -> do
            derived <- deriveDatabase manager source "cheese-wet" (ByProperty WetMass)
            db <- either (fail . show) (pure . ldDatabase . fst) derived
            V.length (dbActivities db) `shouldBe` 2
            M.size (dbActivityUUIDIndex db) `shouldBe` 1
            -- The source declares 51 / 49 and would put 5.1 kg of milk behind
            -- the cheese. 1 kg of cheese beside 3 kg of whey is 25 / 75, and
            -- the whole inventory follows the key that divided the block.
            milkAmounts db `shouldSatisfy` near [2.5, 7.5]

    it "refuses a key that divides nothing, and registers nothing" $
        withSource singleProductCSV $ \manager -> do
            derived <- deriveDatabase manager source "steel-wet" (ByProperty WetMass)
            case derived of
                Right _ -> expectationFailure "expected a refusal, the derivation was kept"
                Left err -> do
                    err `shouldSatisfy` isInfixOf "divided 0 of the 1 blocks"
                    available <- readTVarIO (dmAvailableDbs manager)
                    loaded <- readTVarIO (dmLoadedDbs manager)
                    M.member "steel-wet" available `shouldBe` False
                    M.member "steel-wet" loaded `shouldBe` False

    it "names the blocks a key could not divide, and why" $
        withSource coproductCSV $ \manager -> do
            derived <- deriveDatabase manager source "cheese-dry" (ByProperty DryMass)
            case derived of
                Right _ -> expectationFailure "expected a refusal, the source states no dry mass"
                Left err -> do
                    err `shouldSatisfy` isInfixOf "divided 0 of the 1 blocks"
                    err `shouldSatisfy` isInfixOf "dry mass: no product of the block states one"

    it "reads a block whole under none, the cheese and the whey in one process" $
        withSource coproductCSV $ \manager -> do
            derived <- deriveDatabase manager source "dairy-parent" Unallocated
            db <- either (fail . show) (pure . ldDatabase . fst) derived
            V.length (dbActivities db) `shouldBe` 1
            [length (filter exchangeIsProductOutput (exchanges a)) | a <- V.toList (dbActivities db)] `shouldBe` [2]

    it "refuses none over a source with no block of several products" $
        withSource singleProductCSV $ \manager -> do
            derived <- deriveDatabase manager source "steel-parent" Unallocated
            fromLeft "" derived `shouldSatisfy` isInfixOf "kept no process of several products whole"

    it "refuses the key its source already reads under" $
        withSource coproductCSV $ \manager -> do
            -- The block divides under 'Declared' as well as it does under a
            -- property, so the count of divided blocks says nothing here: the
            -- duplicate is the key, and it is known before any load.
            derived <- deriveDatabase manager source "dairy-again" Declared
            case derived of
                Right _ -> expectationFailure "expected a refusal, the derivation was kept"
                Left err -> do
                    err `shouldSatisfy` isInfixOf "is already read under declared"
                    available <- readTVarIO (dmAvailableDbs manager)
                    M.member "dairy-again" available `shouldBe` False

    it "is still under its key after a restart" $
        withSource coproductCSV $ \manager -> do
            _ <- either (fail . show) pure =<< deriveDatabase manager source "cheese-wet" (ByProperty WetMass)
            -- A boot rebuilds every uploaded database's configuration from its
            -- meta.toml. Written without the key, the derivation came back
            -- divided the way its source declares, under a name promising the
            -- opposite.
            rebooted <- initDatabaseManager defaultConfig NoCache
            available <- readTVarIO (dmAvailableDbs rebooted)
            fmap dcAllocation (M.lookup "cheese-wet" available) `shouldBe` Just (ByProperty WetMass)
            fmap dcSource (M.lookup "cheese-wet" available) `shouldBe` Just (Just source)

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
                -- The parent knows its products by the reference alone, so the
                -- whey finds no supplier there; in the child it is a reference.
                wheyLinks parent `shouldBe` [False]
                wheyLinks child `shouldBe` [True]

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
    _ <- either (fail . show) pure =<< deriveDatabase manager source parentName Unallocated
    k manager

deriveChild :: DatabaseManager -> IO Database
deriveChild manager = either (fail . show) (pure . ldDatabase . fst) =<< deriveDatabase manager parentName childName (ByProperty Price)

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
        , dcAllocation = Declared
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
