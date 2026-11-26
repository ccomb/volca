{-# LANGUAGE OverloadedStrings #-}

-- | Reading an EcoSpold 1 dataset of several products: its groups and its allocation keys.
module EcoSpold1AllocationSpec (spec) where

import Control.Applicative ((<|>))
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Database.Loader (LoadOptions (..), defaultLoadOptions, loadDatabaseWithLocationAliases)
import EcoSpold.Common (ParsedDataset (..))
import EcoSpold.Parser1 (parseAllWithXeno, parseWithXeno)
import Types
import UnitConversion (defaultUnitConfig)

{- | Cheese and whey made together from milk and electricity, with an emission:
dataset 7 at CH, its products numbered 10 and 11, its shared lines 20, 21, 30.
The products' lines and the keys are the fixture's parameters.
-}
cheeseBlock :: [BC.ByteString] -> [BC.ByteString] -> BC.ByteString
cheeseBlock = numberedBlock 7 "cheese and whey production"

numberedBlock :: Int -> BC.ByteString -> [BC.ByteString] -> [BC.ByteString] -> BC.ByteString
numberedBlock number name products keys =
    document [dataset number name (products <> sharedLines) keys]

document :: [BC.ByteString] -> BC.ByteString
document datasets =
    BC.unlines
        ( [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
          , "<ecoSpold xmlns=\"http://www.EcoInvent.org/EcoSpold01\">"
          ]
            <> datasets
            <> ["</ecoSpold>"]
        )

dataset :: Int -> BC.ByteString -> [BC.ByteString] -> [BC.ByteString] -> BC.ByteString
dataset number name lines' keys =
    BC.unlines
        ( [ "<dataset number=\"" <> BC.pack (show number) <> "\">"
          , "<metaInformation><processInformation>"
          , "<referenceFunction name=\"" <> name <> "\" unit=\"kg\"/>"
          , "<geography location=\"CH\"/>"
          , "</processInformation></metaInformation>"
          , "<flowData>"
          ]
            <> lines'
            <> keys
            <> ["</flowData>", "</dataset>"]
        )

-- | An exchange line: its number, name, unit, amount and group element.
line :: Int -> BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString
line number name unit amount group =
    "<exchange number=\"" <> BC.pack (show number) <> "\" name=\"" <> name <> "\" location=\"CH\" unit=\"" <> unit <> "\" category=\"c\" subCategory=\"s\" meanValue=\"" <> amount <> "\">" <> group <> "</exchange>"

output, input :: BC.ByteString -> BC.ByteString
output g = "<outputGroup>" <> g <> "</outputGroup>"
input g = "<inputGroup>" <> g <> "</inputGroup>"

cheeseAndWhey :: [BC.ByteString]
cheeseAndWhey = [line 10 "cheese" "kg" "1" (output "0"), line 11 "whey" "kg" "9" (output "2")]

sharedLines :: [BC.ByteString]
sharedLines =
    [ line 20 "milk" "kg" "10" (input "5")
    , line 21 "electricity" "kWh" "2" (input "5")
    , "<exchange number=\"30\" name=\"Carbon dioxide\" unit=\"kg\" category=\"air\" subCategory=\"unspecified\" meanValue=\"0.5\">" <> output "4" <> "</exchange>"
    ]

-- | A key: the product's number, the fraction, the lines' numbers.
key :: Int -> BC.ByteString -> [Int] -> BC.ByteString
key product fraction lines' =
    "<allocation referenceToCoProduct=\""
        <> BC.pack (show product)
        <> "\" allocationMethod=\"1\" fraction=\""
        <> fraction
        <> "\">"
        <> foldMap (\n -> "<referenceToInputOutput>" <> BC.pack (show n) <> "</referenceToInputOutput>") lines'
        <> "</allocation>"

everyLine :: [Int]
everyLine = [20, 21, 30]

parsed :: BC.ByteString -> IO ParsedDataset
parsed xml = either fail pure (parseWithXeno xml)

-- | Load one file as a database, under a reading.
loadAs :: FileReading -> BC.ByteString -> IO SimpleDatabase
loadAs reading xml = withSystemTempDirectory "es1-keys" $ \dir -> do
    BC.writeFile (dir </> "block.xml") xml
    loaded <- loadDatabaseWithLocationAliases (defaultLoadOptions defaultUnitConfig){loAllocation = reading} (dir </> "block.xml")
    either (fail . T.unpack) pure loaded

-- | The lines of the process making this product, by flow name, summed.
inventoryOf :: SimpleDatabase -> Text -> M.Map Text Double
inventoryOf db product =
    M.fromListWith
        (+)
        [ (nameOf ex, exchangeAmount ex)
        | act <- M.elems (sdbActivities db)
        , [only] <- [filter exchangeIsReference (exchanges act)]
        , nameOf only == product
        , ex <- exchanges act
        , not (exchangeIsReference ex)
        ]
  where
    nameOf ex = fromMaybe "" ((tfName <$> M.lookup (exchangeFlowId ex) (sdbTechFlows db)) <|> (bfName <$> M.lookup (exchangeFlowId ex) (sdbBioFlows db)))

warns :: ParsedDataset -> Text -> Expectation
warns p part = pdWarnings p `shouldSatisfy` any (part `T.isInfixOf`)

shouldBeNear :: Maybe Double -> Double -> Expectation
shouldBeNear actual expected = case actual of
    Just a | abs (a - expected) < 1e-9 -> pure ()
    _ -> expectationFailure (show actual <> " is not " <> show expected)

spec :: Spec
spec = do
    describe "output groups" $ do
        it "reads group 2 as a coproduct, and says nothing of it" $ do
            p <- parsed (cheeseBlock cheeseAndWhey [])
            [techRole ex | ex@TechnosphereExchange{} <- exchanges (pdActivity p), techRole ex /= Input] `shouldBe` [ReferenceProduct, Coproduct]
            pdWarnings p `shouldBe` []

        it "reads a group the format does not use for a product as a coproduct, and says which" $ do
            p <- parsed (cheeseBlock [line 10 "cheese" "kg" "1" (output "0"), line 11 "whey" "kg" "9" (output "1"), line 12 "sludge" "kg" "1" (output "3"), line 13 "cream" "kg" "1" (output "7"), line 14 "butter" "kg" "1" ""] [])
            length [() | TechnosphereExchange{techRole = Coproduct} <- exchanges (pdActivity p)] `shouldBe` 4
            p `warns` "\"whey\" carry group 1"
            p `warns` "\"sludge\" carry group 3"
            p `warns` "\"cream\" carry a group the format does not define"
            p `warns` "\"butter\" carry no group"

        it "takes the first allocated product as the reference of a dataset naming none" $ do
            p <- parsed (cheeseBlock [line 10 "cheese" "kg" "1" (output "2"), line 11 "whey" "kg" "9" (output "2")] [])
            [techRole ex | ex@TechnosphereExchange{} <- exchanges (pdActivity p), techRole ex /= Input] `shouldBe` [ReferenceProduct, Coproduct]

    describe "allocation keys" $ do
        it "divides a block by keys giving each product one fraction of every line" $ do
            db <- loadAs ReadDeclared (cheeseBlock cheeseAndWhey [key 10 "80" everyLine, key 11 "20" everyLine])
            let cheese = inventoryOf db "cheese"
                whey = inventoryOf db "whey"
            M.lookup "milk" cheese `shouldBeNear` 8
            M.lookup "electricity" cheese `shouldBeNear` 1.6
            M.lookup "Carbon dioxide" cheese `shouldBeNear` 0.4
            M.lookup "milk" whey `shouldBeNear` 2
            M.lookup "Carbon dioxide" whey `shouldBeNear` 0.1

        it "declares those fractions as the products' shares, which a block kept whole keeps" $ do
            p <- parsed (cheeseBlock cheeseAndWhey [key 10 "80" everyLine, key 11 "20" everyLine])
            [dsPercent <$> techShare ex | ex@TechnosphereExchange{} <- exchanges (pdActivity p), techRole ex /= Input] `shouldBe` [Just 80, Just 20]
            pdWarnings p `shouldBe` []

        it "divides line by line when the fractions differ, a line no key gives a product counting 0 for it" $ do
            let xml = cheeseBlock cheeseAndWhey [key 10 "80" [20, 30], key 10 "100" [21], key 11 "20" [20, 30]]
            p <- parsed xml
            p `warns` "line by line"
            p `warns` "whey: electricity"
            db <- loadAs ReadDeclared xml
            M.lookup "electricity" (inventoryOf db "cheese") `shouldBeNear` 2
            M.lookup "milk" (inventoryOf db "cheese") `shouldBeNear` 8
            M.lookup "electricity" (inventoryOf db "whey") `shouldBeNear` 0
            M.lookup "milk" (inventoryOf db "whey") `shouldBeNear` 2

        it "gives nothing to a product whose keys cover no shared line" $ do
            db <- loadAs ReadDeclared (cheeseBlock cheeseAndWhey [key 10 "100" everyLine, key 11 "100" [11]])
            M.lookup "milk" (inventoryOf db "cheese") `shouldBeNear` 10
            M.lookup "milk" (inventoryOf db "whey") `shouldBeNear` 0

        it "says when the fractions of a line do not add up to 100" $ do
            p <- parsed (cheeseBlock cheeseAndWhey [key 10 "80" everyLine, key 11 "19.9" everyLine])
            p `warns` "add up to other than 100 %: milk (99.9"

        it "keeps the block whole and says why when a key names no line" $ do
            let xml = cheeseBlock cheeseAndWhey [key 10 "80" everyLine, key 11 "20" [20, 99]]
            p <- parsed xml
            p `warns` "number 99, which names no line the products share"
            [techShare ex | ex@TechnosphereExchange{} <- exchanges (pdActivity p), techRole ex /= Input] `shouldBe` [Nothing, Nothing]

        it "keeps the block whole when a key is for no product of the dataset" $ do
            p <- parsed (cheeseBlock cheeseAndWhey [key 10 "80" everyLine, key 20 "20" everyLine])
            p `warns` "a key is for number 20, which is no product of the dataset"

        it "keeps the block whole when one number names two lines" $ do
            p <- parsed (numberedBlock 7 "cheese and whey production" (cheeseAndWhey <> [line 20 "milk, skimmed" "kg" "1" (input "5")]) [key 10 "80" everyLine, key 11 "20" everyLine])
            p `warns` "number 20 names several lines"

        it "keeps the block whole when two keys give one line two fractions" $ do
            p <- parsed (cheeseBlock cheeseAndWhey [key 10 "80" everyLine, key 10 "70" [20], key 11 "20" everyLine])
            p `warns` "two keys give milk to cheese different fractions"

        it "keeps the block whole when a key's fraction does not read" $ do
            p <- parsed (cheeseBlock cheeseAndWhey [key 10 "eighty" everyLine, key 11 "20" everyLine])
            p `warns` "an allocation key states a number or a fraction that does not read"

        it "leaves a dataset without keys as it was" $ do
            p <- parsed (cheeseBlock cheeseAndWhey [])
            [techShare ex | ex@TechnosphereExchange{} <- exchanges (pdActivity p), techRole ex /= Input] `shouldBe` [Nothing, Nothing]

        it "reads the keys of each dataset of a file apart" $
            case parseAllWithXeno (document [dataset 7 "first" (cheeseAndWhey <> sharedLines) [key 10 "80" everyLine, key 11 "20" everyLine], dataset 8 "second" (cheeseAndWhey <> sharedLines) []]) of
                Right [Right first, Right second] -> do
                    [dsPercent <$> techShare ex | ex@TechnosphereExchange{} <- exchanges (pdActivity first), techRole ex /= Input] `shouldBe` [Just 80, Just 20]
                    [techShare ex | ex@TechnosphereExchange{} <- exchanges (pdActivity second), techRole ex /= Input] `shouldBe` [Nothing, Nothing]
                other -> expectationFailure (show (fmap (fmap (fmap (activityName . pdActivity))) other))

    describe "a file holding one dataset of several products" $ do
        it "names its processes the same whether it is loaded alone or from a directory" $
            withSystemTempDirectory "es1-one-block" $ \dir -> do
                BC.writeFile (dir </> "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa.xml") (cheeseBlock cheeseAndWhey [key 10 "80" everyLine, key 11 "20" everyLine])
                alone <- loadDatabaseWithLocationAliases (defaultLoadOptions defaultUnitConfig) (dir </> "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa.xml")
                fromDir <- loadDatabaseWithLocationAliases (defaultLoadOptions defaultUnitConfig) dir
                (M.keys . sdbActivities <$> alone) `shouldBe` (M.keys . sdbActivities <$> fromDir)
                (M.size . sdbActivities <$> alone) `shouldBe` Right 2

    describe "an input buying a coproduct of a block read whole" $
        it "is linked to the block its number names, among two making the coproduct" $ do
            let plant number = "cheese and whey production, plant " <> BC.pack (show (number :: Int))
                block number = dataset number (plant number) (cheeseAndWhey <> sharedLines) [key 10 "80" everyLine, key 11 "20" everyLine]
                buyerOf number = dataset (number + 40) ("whey drink from " <> BC.pack (show number)) [line 40 ("whey drink from " <> BC.pack (show number)) "kg" "1" (output "0"), line number "whey" "kg" "3" (input "5")] []
            db <- loadAs ReadWhole (document [block 7, block 9, buyerOf 7, buyerOf 9])
            let keyOf name = [Just actUUID | ((actUUID, _), act) <- M.toList (sdbActivities db), activityName act == name]
                linkOf name = [techActivityLinkId ex | act <- M.elems (sdbActivities db), activityName act == name, ex@TechnosphereExchange{techRole = Input} <- exchanges act]
            linkOf "whey drink from 7" `shouldBe` keyOf "cheese and whey production, plant 7"
            linkOf "whey drink from 9" `shouldBe` keyOf "cheese and whey production, plant 9"
