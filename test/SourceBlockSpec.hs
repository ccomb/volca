{-# LANGUAGE OverloadedStrings #-}

{- | The source block: the name an activity summary carries for it
('Types.sourceBlockOf'), and reaching one by the identifier its file gave it.

A listing that wants to show the coproducts of one block together cannot group
on the process id, which names the product. The block name is the activity the
products were written under, and it travels with the count of products the
block holds so a page can say when it is showing only part of one.

Someone with the source file open beside the engine has that block's
identifier in front of them and nothing else that is certain: the process name
is truncated to 80 characters and reused across unrelated blocks. Typing the
identifier has to land on that block, all of it, and on nothing that merely
resembles it.
-}
module SourceBlockSpec (spec) where

import qualified Data.ByteString.Char8 as BS
import Data.List (nub, sort)
import Data.Text (Text)
import qualified Data.Vector as V
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import API.Types (ActivitySummary (..))
import qualified Data.Text as T
import Database (buildDatabaseWithMatrices)
import Database.Loader (defaultLoadOptions, loadDatabaseWithLocationAliases)
import Service (ActivityFilterCore (..), SearchFilter (..), activityMatches, mkActivitySummary)
import TestHelpers (shippedGeographies)
import Types (AllocationKey (..), BuildInputs (..), Database (..))
import UnitConversion (defaultUnitConfig)

spec :: Spec
spec = do
    describe "the source block an activity summary names" $ do
        it "separates two blocks that share a process name" $
            withDatabase twoBlocksSharingAName $ \db -> do
                -- Four processes, two blocks of two products.
                map prsProductName (summaries db)
                    `shouldMatchList` ["Cheese", "Whey", "Steel", "Slag"]
                length (nub (map prsBlock (summaries db))) `shouldBe` 2
                sort (map prsBlockProducts (summaries db)) `shouldBe` [2, 2, 2, 2]

        it "gives the coproducts of one block the same name" $
            withDatabase twoBlocksSharingAName $ \db -> do
                let blockOf name = [prsBlock s | s <- summaries db, prsProductName s == name]
                blockOf "Cheese" `shouldBe` blockOf "Whey"
                blockOf "Cheese" `shouldNotBe` blockOf "Steel"

        it "names a block of one product too, so every row has one" $
            withDatabase oneBlockOneProduct $ \db -> do
                map prsBlockProducts (summaries db) `shouldBe` [1]
                map (T.null . prsBlock) (summaries db) `shouldBe` [False]

    describe "finding a source block by its identifier" $ do
        it "answers the whole block an identifier names, and only it" $
            withDatabase twoBlocksSharingAName $ \db ->
                productsFound db "AGRIBALU000000003103728" `shouldMatchList` ["Cheese", "Whey"]

        -- An identifier is read off a page and pasted, so it arrives with
        -- whatever the copy picked up, and SimaPro writes its own in upper case.
        it "reads an identifier pasted with its surrounding spaces, in any case" $
            withDatabase twoBlocksSharingAName $ \db ->
                productsFound db "  agribalu000000003103728 " `shouldMatchList` ["Cheese", "Whey"]

        {- Eighteen of the twenty-three characters are the same on every dataset
        of a real export, so what a reader has in hand is the tail.
        -}
        it "answers a fragment of an identifier that names one block" $
            withDatabase twoBlocksSharingAName $ \db ->
                productsFound db "3103728" `shouldMatchList` ["Cheese", "Whey"]

        {- The part every identifier of a database shares distinguishes nothing,
        so it answers with every block carrying it. Which is honest, and costs
        the order of a page rather than the answer, the ordinary search still
        being underneath.
        -}
        it "answers a fragment every identifier shares with all of them" $
            withDatabase twoBlocksSharingAName $ \db ->
                productsFound db "AGRIBALU" `shouldMatchList` ["Cheese", "Whey", "Steel", "Slag"]

        {- Below four characters nothing is ever selective: measured on a real
        export, a three character fragment names 41 datasets at the median and
        never a single one. So it is a word, and only the ordinary search reads
        it, which here finds nothing.
        -}
        it "reads a fragment too short to name anything as a word" $
            withDatabase twoBlocksSharingAName $ \db ->
                productsFound db "728" `shouldBe` []

        it "leaves an ordinary search alone" $
            withDatabase twoBlocksSharingAName $ \db ->
                productsFound db "One name for two blocks"
                    `shouldMatchList` ["Cheese", "Slag", "Steel", "Whey"]

        it "puts the block a query names outright before the one it is only part of" $
            withDatabase blocksWhoseIdentifiersNest $ \db ->
                productsFound db "3103728" `shouldBe` ["Butter", "Cream"]

        {- A caller asking for an exact match asked for equality, so a fragment
        is not one. Without this the stamp every identifier shares would answer
        with the whole database under the flag that exists to narrow it.
        -}
        it "reads only whole identifiers when the caller asked for an exact match" $
            withDatabase twoBlocksSharingAName $ \db -> do
                exactProductsFound db "AGRIBALU" `shouldBe` []
                exactProductsFound db "AGRIBALU000000003103728" `shouldMatchList` ["Cheese", "Whey"]

-- | Every process of a SimaPro file, loaded and built the way a served one is.
withDatabase :: BS.ByteString -> (Database -> IO ()) -> IO ()
withDatabase csv k = withSystemTempDirectory "source-block" $ \dir -> do
    let path = dir </> "source.csv"
    BS.writeFile path csv
    loaded <- loadDatabaseWithLocationAliases (defaultLoadOptions defaultUnitConfig) path
    simpleDb <- either (fail . T.unpack) pure loaded
    built <- buildDatabaseWithMatrices (BuildInputs defaultUnitConfig mempty Declared) simpleDb
    either (fail . T.unpack) k built

-- | Every process of the database, summarised the way a search result is.
summaries :: Database -> [ActivitySummary]
summaries db = [mkActivitySummary db pid act | (pid, act) <- zip [0 ..] (V.toList (dbActivities db))]

{- | The products a query brings back, in the order it brings them, through the
funnel a listing and its tab counter both use.
-}
productsFound :: Database -> Text -> [Text]
productsFound db = productsMatching db False

-- | 'productsFound' with the exact-match flag the caller can set on a search.
exactProductsFound :: Database -> Text -> [Text]
exactProductsFound db = productsMatching db True

-- | Both of the above, carrying the exact-match flag a 'SearchFilter' holds.
productsMatching :: Database -> Bool -> Text -> [Text]
productsMatching db exact query =
    [prsProductName (mkActivitySummary db pid act) | (pid, act) <- activityMatches shippedGeographies db (SearchFilter (nameOnly query) exact)]
  where
    nameOnly :: Text -> ActivityFilterCore
    nameOnly q =
        ActivityFilterCore
            { afcName = Just q
            , afcLocation = Nothing
            , afcProduct = Nothing
            , afcClassifications = []
            , afcLimit = Nothing
            , afcOffset = Nothing
            , afcSort = Nothing
            , afcOrder = Nothing
            }

{- | Two blocks written under one process name, each with two coproducts. The
producer tells them apart by their @Process identifier@ line and nothing else,
which is what a block name has to survive. The two identifiers are shaped like
the ones a real SimaPro export carries: a long stamp every block of the
database shares, then the few characters that actually say which block, here
differing by one, which is what a fuzzy matcher would confuse them on.
-}
twoBlocksSharingAName :: BS.ByteString
twoBlocksSharingAName =
    simaProFile
        ( block "AGRIBALU000000003103728" ["Cheese;kg;1;60;not defined;material;", "Whey;kg;3;40;not defined;material;"]
            ++ block "AGRIBALU000000003103729" ["Steel;kg;1;70;not defined;material;", "Slag;kg;2;30;not defined;material;"]
        )

{- | Two blocks of one product whose identifiers nest: the first is the whole of
what the second ends with. One query therefore names one of them outright and
is part of the other, which is the only way to see which comes first.
-}
blocksWhoseIdentifiersNest :: BS.ByteString
blocksWhoseIdentifiersNest =
    simaProFile
        ( block "3103728" ["Butter;kg;1;100;not defined;material;"]
            ++ block "AGRIBALU000000003103728" ["Cream;kg;1;100;not defined;material;"]
        )

-- | One block, one product: it is still a block, and it holds one product.
oneBlockOneProduct :: BS.ByteString
oneBlockOneProduct = simaProFile (block "AGRIBALU000000003103728" ["Cheese;kg;1;100;not defined;material;"])

-- | One SimaPro process block under a fixed name, identified by @identifier@.
block :: BS.ByteString -> [BS.ByteString] -> [BS.ByteString]
block identifier products =
    [ "Process"
    , ""
    , "Category type"
    , "material"
    , ""
    , "Process identifier"
    , identifier
    , ""
    , "Process name"
    , "One name for two blocks"
    , ""
    , "Type"
    , "Unit process"
    , ""
    , "Geography"
    , "GLO"
    , ""
    , "Products"
    ]
        ++ products
        ++ ["", "End", ""]

simaProFile :: [BS.ByteString] -> BS.ByteString
simaProFile body =
    BS.intercalate
        "\r\n"
        ( [ "{SimaPro 9.6.0.1}"
          , "{CSV separator: semicolon}"
          , "{Decimal separator: .}"
          , ""
          ]
            ++ body
        )
