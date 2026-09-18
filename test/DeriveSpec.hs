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
  says its shares are not the ones its source declares.
-}
module DeriveSpec (spec) where

import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import qualified Data.ByteString.Char8 as BS
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Text (Text, isInfixOf)
import qualified Data.Vector as V
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Config (DatabaseConfig (..), defaultConfig)
import Database.Edit (deriveDatabase)
import Database.Manager (
    CachePolicy (..),
    DatabaseManager (..),
    LoadedDatabase (..),
    initDatabaseManager,
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
    exchangeAmount,
    exchangeIsProductOutput,
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

-- | One SimaPro process block, wrapped in the header its parser expects.
simaProFile :: [BS.ByteString] -> BS.ByteString
simaProFile body =
    BS.intercalate
        "\r\n"
        ( [ "{SimaPro 9.6.0.1}"
          , "{CSV separator: semicolon}"
          , "{Decimal separator: .}"
          , ""
          , "Process"
          , ""
          , "Category type"
          , "material"
          , ""
          , "Process name"
          , "Dairy"
          , ""
          , "Type"
          , "Unit process"
          , ""
          , "Geography"
          , "GLO"
          , ""
          ]
            ++ body
            ++ ["", "End"]
        )
