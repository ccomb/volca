{-# LANGUAGE OverloadedStrings #-}

-- | Reading and writing an EcoSpold 1 dataset of several products: its groups and its allocation keys.
module EcoSpold1AllocationSpec (spec) where

import Control.Applicative ((<|>))
import Control.Monad (forM_, join)
import qualified Data.ByteString.Char8 as BC
import Data.Either (fromLeft)
import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Calendar (fromGregorian)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Database.Allocation (
    Allocating (..),
    AllocationRefusal (..),
    PropertyRefusal (..),
    allocate,
    asAllocated,
    describePropertyRefusal,
    describeRefusal,
    producerKeyOf,
    propertyKeyRefusals,
 )
import Database.Loader (LoadOptions (..), defaultLoadOptions, loadDatabaseWithLocationAliases)
import Database.Quality (QualityCheck (..), QualityReport (..), qualityReport)
import EcoSpold.Allocation1 (Allocation1 (..), Division (..), divide)
import EcoSpold.Common (ParsedDataset (..))
import EcoSpold.Parser1 (parseAllWithXeno, parseWithXeno)
import EcoSpold.Writer1 (Written1 (..), blockWarnings, canonicalWriterOptions, writeSimpleDatabase, writtenDatasets)
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

-- | Keys giving the electricity wholly to the cheese, the milk and the CO2 80/20.
lineByLine :: [BC.ByteString]
lineByLine = [key 10 "80" [20, 30], key 10 "100" [21], key 11 "20" [20, 30]]

-- | The product rows of every process of a database.
productRows :: SimpleDatabase -> [Exchange]
productRows db = [ex | act <- M.elems (sdbActivities db), ex <- exchanges act, exchangeIsProductOutput ex]

-- | A key by a property, over the database's units.
byProperty :: SimpleDatabase -> AllocationProperty -> Allocating
byProperty db prop = Allocating{alKey = ByProperty prop, alUnitConfig = defaultUnitConfig, alUnitDB = sdbUnits db}

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

-- | The roles of the dataset's product rows, in order.
roles :: ParsedDataset -> [TechRole]
roles p = [techRole ex | ex@TechnosphereExchange{} <- exchanges (pdActivity p), techRole ex /= Input]

warns :: ParsedDataset -> Text -> Expectation
warns p part = pdWarnings p `shouldSatisfy` any (part `T.isInfixOf`)

-- | The gate's refusal, if any.
asAllocated' :: Activity -> Maybe AllocationRefusal
asAllocated' act = either Just (const Nothing) (asAllocated act)

-- | The block kept whole with Pizza buying its cheese and Ricotta its whey, under these keys.
withBuyers :: [BC.ByteString] -> BC.ByteString
withBuyers keys = document [dataset 7 "cheese and whey production" (cheeseAndWhey <> sharedLines) keys, buyer 50 "pizza" "cheese" "0.2", buyer 51 "ricotta" "whey" "3"]
  where
    buyer :: Int -> BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString
    buyer number name bought amount = dataset number name [line 60 name "kg" "1" (output "0"), line 7 bought "kg" amount (input "5")] []

-- | The product each input of the processes of this name is bought as, from the process making it.
boughtProducts :: SimpleDatabase -> Text -> [Maybe Text]
boughtProducts db name =
    [ tfName <$> (producerKeyOf (sdbActivities db) (link, flow) >> M.lookup flow (sdbTechFlows db))
    | act <- M.elems (sdbActivities db)
    , activityName act == name
    , TechnosphereExchange{techRole = Input, techActivityLinkId = Just link, techFlowId = flow} <- exchanges act
    ]

-- | The file the writer makes of a database; the test fails on its refusal.
writtenFile :: SimpleDatabase -> IO BC.ByteString
writtenFile db = either (fail . T.unpack) (pure . TE.encodeUtf8) (writeSimpleDatabase (canonicalWriterOptions (fromGregorian 2026 1 1)) db)

-- | What the writer answers for a database, refusal included.
writing :: SimpleDatabase -> Either Text Text
writing = writeSimpleDatabase (canonicalWriterOptions (fromGregorian 2026 1 1))

-- | The share each product declares, by product name.
sharesOf :: SimpleDatabase -> [(Text, Maybe Double)]
sharesOf db = sort [(maybe "" tfName (M.lookup (exchangeFlowId ex) (sdbTechFlows db)), dsPercent <$> exchangeDeclaredShare ex) | ex <- productRows db, exchangeShareStated ex]

-- | The activity each process of this name buys from.
linkOf :: SimpleDatabase -> Text -> [Maybe UUID]
linkOf db name = [techActivityLinkId ex | act <- M.elems (sdbActivities db), activityName act == name, ex@TechnosphereExchange{techRole = Input} <- exchanges act]

-- | The activity of the processes of this name.
activityOf :: SimpleDatabase -> Text -> [Maybe UUID]
activityOf db name = [Just actUUID | ((actUUID, _), act) <- M.toList (sdbActivities db), activityName act == name]

-- | Every product row of a database restated by a function.
onProducts :: (Exchange -> Exchange) -> SimpleDatabase -> SimpleDatabase
onProducts f db = db{sdbActivities = M.map restate (sdbActivities db)}
  where
    restate :: Activity -> Activity
    restate act = act{activityLines = packExchanges [if exchangeIsProductOutput ex then f ex else ex | ex <- exchanges act]}

-- | A product row selling for this many euros per unit.
pricedAt :: Double -> Exchange -> Exchange
pricedAt euros ex = case ex of
    TechnosphereExchange{} -> ex{techProperties = noProperties{epPrice = Just (StatedPrice (StatedAmount "EUR" euros))}}
    BiosphereExchange{} -> ex
    WasteExchange{} -> ex

-- | Whether a text occurs in another.
isIn :: String -> String -> Bool
isIn part = T.isInfixOf (T.pack part) . T.pack

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

        it "takes the first allocated product as the reference of a dataset naming none, and says so" $ do
            p <- parsed (cheeseBlock [line 10 "cheese" "kg" "1" (output "2"), line 11 "whey" "kg" "9" (output "2")] [])
            roles p `shouldBe` [ReferenceProduct, Coproduct]
            p `warns` "takes its first allocated product with an amount, \"cheese\", as its reference"

        it "takes the reference among the products made, not one listed at zero" $ do
            p <- parsed (cheeseBlock [line 10 "cheese" "kg" "0" (output "2"), line 11 "whey" "kg" "9" (output "2"), line 12 "cream" "kg" "1" (output "2")] [])
            roles p `shouldBe` [Coproduct, ReferenceProduct, Coproduct]

        it "takes the reference among the allocated products only" $ do
            p <- parsed (cheeseBlock [line 12 "sludge" "kg" "1" (output "3"), line 10 "cheese" "kg" "1" (output "2"), line 11 "whey" "kg" "9" (output "2")] [])
            roles p `shouldBe` [Coproduct, ReferenceProduct, Coproduct]

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
            [dsPercent <$> exchangeDeclaredShare ex | ex@TechnosphereExchange{} <- exchanges (pdActivity p), techRole ex /= Input] `shouldBe` [Just 80, Just 20]
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

        it "states no share for a product divided line by line, only that it is" $ do
            p <- parsed (cheeseBlock cheeseAndWhey lineByLine)
            [techShare ex | ex@TechnosphereExchange{} <- exchanges (pdActivity p), techRole ex /= Input] `shouldBe` [Just ShareByLine, Just ShareByLine]

        it "gives the processes of a block divided line by line no allocation percentage, and the quality report no sum to judge" $ do
            db <- loadAs ReadDeclared (cheeseBlock cheeseAndWhey lineByLine)
            map activityReferenceShare (M.elems (sdbActivities db)) `shouldBe` [Nothing, Nothing]
            qcOffenders (qrAllocationSums (qualityReport "es1" db)) `shouldBe` []

        it "deduces no price for a block divided line by line and read whole, where a block of shares gets one" $ do
            byLine <- loadAs ReadWhole (cheeseBlock cheeseAndWhey lineByLine)
            map (epPrice . exchangeProperties) (productRows byLine) `shouldBe` [Nothing, Nothing]
            shared <- loadAs ReadWhole (cheeseBlock cheeseAndWhey [key 10 "80" everyLine, key 11 "20" everyLine])
            map (epPrice . exchangeProperties) (productRows shared) `shouldBe` [Just (DeducedFromShare 80), Just (DeducedFromShare (20 / 9))]

        it "refuses to divide such a block read whole by price, saying its source divides it line by line" $ do
            db <- loadAs ReadWhole (cheeseBlock cheeseAndWhey lineByLine)
            map snd (propertyKeyRefusals (byProperty db Price) (M.elems (sdbActivities db))) `shouldBe` [DividedByLine]
            map snd (propertyKeyRefusals (byProperty db DryMass) (M.elems (sdbActivities db))) `shouldBe` [NotStated]
            describePropertyRefusal Price DividedByLine `shouldSatisfy` T.isInfixOf "divides each line"

        it "keeps such a block whole under the declared key once its lines' fractions are gone, and says how to divide it" $ do
            db <- loadAs ReadWhole (cheeseBlock cheeseAndWhey lineByLine)
            let declared = Allocating{alKey = Declared, alUnitConfig = defaultUnitConfig, alUnitDB = sdbUnits db}
            [length (allocate declared act) | act <- M.elems (sdbActivities db)] `shouldBe` [1]
            map asAllocated' (M.elems (sdbActivities db)) `shouldBe` [Just (ProductsDividedByLine 2)]
            describeRefusal (ProductsDividedByLine 2) `shouldSatisfy` T.isInfixOf "load its files again under the declared key"

        it "gives nothing to a product whose keys cover no shared line, and says so" $ do
            let xml = cheeseBlock cheeseAndWhey [key 10 "100" everyLine, key 11 "100" [11]]
            p <- parsed xml
            p `warns` "give no line it shares to whey, which is declared at 0 %"
            db <- loadAs ReadDeclared xml
            M.lookup "milk" (inventoryOf db "cheese") `shouldBeNear` 10
            M.lookup "milk" (inventoryOf db "whey") `shouldBeNear` 0

        it "keeps the block whole when its keys give no product a line it shares" $ do
            p <- parsed (cheeseBlock cheeseAndWhey [key 10 "100" [10], key 11 "100" [11]])
            p `warns` "give no product a line it shares"
            [techShare ex | ex@TechnosphereExchange{} <- exchanges (pdActivity p), techRole ex /= Input] `shouldBe` [Nothing, Nothing]

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
                    [dsPercent <$> exchangeDeclaredShare ex | ex@TechnosphereExchange{} <- exchanges (pdActivity first), techRole ex /= Input] `shouldBe` [Just 80, Just 20]
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

    describe "writing a block of several products kept whole" $ do
        it "writes keys a reader divides it by at its shares, without the file" $ do
            db <- loadAs ReadWhole (withBuyers [key 10 "80" everyLine, key 11 "20" everyLine])
            case [w | w <- writtenDatasets (canonicalWriterOptions (fromGregorian 2026 1 1)) db, activityName (w1Activity w) == "cheese and whey production"] of
                [w] -> do
                    let division = divide (const "") (w1Lines w) (w1Keys w)
                    sort [dsPercent <$> exchangeDeclaredShare ex | ex <- dvLines division, exchangeIsProductOutput ex] `shouldBe` [Just 20, Just 80]
                    -- Each product also has the whole of its own line.
                    [alFraction k | k <- w1Keys w, alLines k == [alCoProduct k]] `shouldBe` [100, 100]
                found -> expectationFailure (show (length found) <> " blocks")

        it "reads back kept whole at the same shares, its buyers linked to it, and writes again to the byte" $ do
            db <- loadAs ReadWhole (withBuyers [key 10 "80" everyLine, key 11 "20" everyLine])
            xml <- writtenFile db
            back <- loadAs ReadWhole xml
            sharesOf back `shouldBe` [("cheese", Just 80), ("whey", Just 20)]
            linkOf back "pizza" `shouldBe` activityOf back "cheese and whey production"
            linkOf back "ricotta" `shouldBe` activityOf back "cheese and whey production"
            writtenFile back `shouldReturn` xml

        it "names the block after its activity and each product line after its product" $ do
            db <- loadAs ReadWhole (withBuyers [key 10 "80" everyLine, key 11 "20" everyLine])
            xml <- writtenFile db
            let says part = BC.unpack xml `shouldSatisfy` (part `isIn`)
            says "<referenceFunction name=\"cheese and whey production\""
            BC.unpack xml `shouldSatisfy` (not . ("allocationMethod" `isIn`))
            says "name=\"cheese\" location=\"CH\" unit=\"kg\" meanValue=\"0.2\""
            says "name=\"whey\" location=\"CH\" unit=\"kg\" meanValue=\"3.0\""

        it "divides the block read back as the source divides it" $ do
            let source = withBuyers [key 10 "80" everyLine, key 11 "20" everyLine]
            xml <- writtenFile =<< loadAs ReadWhole source
            original <- loadAs ReadDeclared source
            back <- loadAs ReadDeclared xml
            mapM_ (\product -> M.toList (inventoryOf back product) `shouldBe` M.toList (inventoryOf original product)) ["cheese", "whey"]

        it "weighs the products by their prices first, as an economic key" $ do
            db <- onProducts (\ex -> pricedAt (if exchangeIsReference ex then 5 else 0.1) ex) <$> loadAs ReadWhole (withBuyers [key 10 "80" everyLine, key 11 "20" everyLine])
            xml <- writtenFile db
            BC.unpack xml `shouldSatisfy` ("allocationMethod=\"1\"" `isIn`)
            back <- loadAs ReadWhole xml
            join (lookup "cheese" (sharesOf back)) `shouldBeNear` (100 * 5 / 5.9)
            blockWarnings (canonicalWriterOptions (fromGregorian 2026 1 1)) db `shouldSatisfy` any (T.isInfixOf "not by the shares they declare")

        it "scales shares adding up to other than 100, and says so" $ do
            db <- loadAs ReadWhole (withBuyers [key 10 "60" everyLine, key 11 "30" everyLine])
            back <- loadAs ReadWhole =<< writtenFile db
            join (lookup "cheese" (sharesOf back)) `shouldBeNear` (100 * 60 / 90)
            blockWarnings (canonicalWriterOptions (fromGregorian 2026 1 1)) db `shouldSatisfy` any (T.isInfixOf "adding up to other than 100 %")

        it "writes again to the byte keys whose fractions add up to 100 only to the last digit" $ do
            let withCream = numberedBlock 7 "cheese, whey and cream production" (cheeseAndWhey <> [line 12 "cream" "kg" "1" (output "2")])
                blocks =
                    [withBuyers [key 10 cheese everyLine, key 11 whey everyLine] | (cheese, whey) <- [("0.7", "99.3"), ("2.7", "97.3"), ("1", "5")]]
                        -- Scaled, these add up to 100.00000000000001.
                        <> [withCream [key 10 "1" everyLine, key 11 "4" everyLine, key 12 "1" everyLine]]
            forM_ blocks $ \source -> do
                xml <- writtenFile =<< loadAs ReadWhole source
                back <- loadAs ReadWhole xml
                writtenFile back `shouldReturn` xml

        it "keeps a product at 0 % at 0 %, its own line keyed to it" $ do
            db <- loadAs ReadWhole (withBuyers [key 10 "100" everyLine, key 11 "0" everyLine])
            back <- loadAs ReadWhole =<< writtenFile db
            sharesOf back `shouldBe` [("cheese", Just 100), ("whey", Just 0)]

        it "calls no key economic for the price of a product held at 0 %, which weighs nothing" $ do
            db <- onProducts (\ex -> if exchangeIsReference ex then ex else pricedAt 3 ex) <$> loadAs ReadWhole (withBuyers [key 10 "100" everyLine, key 11 "0" everyLine])
            xml <- writtenFile db
            BC.unpack xml `shouldSatisfy` (not . ("allocationMethod" `isIn`))

        it "writes no key when every product declares 0 %, and says so" $ do
            db <- loadAs ReadWhole (cheeseBlock cheeseAndWhey [key 10 "0" everyLine, key 11 "0" everyLine])
            xml <- writtenFile db
            BC.unpack xml `shouldSatisfy` (not . ("<allocation" `isIn`))
            blockWarnings (canonicalWriterOptions (fromGregorian 2026 1 1)) db `shouldSatisfy` any (T.isInfixOf "every product declares 0 %")

        it "writes no key for a block divided line by line, and refuses an input buying its coproduct" $ do
            db <- loadAs ReadWhole (withBuyers lineByLine)
            fromLeft "" (writing db) `shouldSatisfy` T.isInfixOf "writes without allocation keys"
            let alone = db{sdbActivities = M.filter ((/= "ricotta") . activityName) (sdbActivities db)}
            either (const False) (not . T.isInfixOf "<allocation") (writing alone) `shouldBe` True

        it "buys each product of a block under the number its line carries, linked again read whole or divided" $ do
            let both = dataset 52 "mix" [line 61 "mix" "kg" "1" (output "0"), line 7 "cheese" "kg" "1" (input "5"), line 7 "whey" "kg" "1" (input "5")] []
                source = document [dataset 7 "cheese and whey production" (cheeseAndWhey <> sharedLines) [key 10 "80" everyLine, key 11 "20" everyLine], both]
            db <- loadAs ReadWhole source
            let numbersOf name pick = [n | w <- writtenDatasets (canonicalWriterOptions (fromGregorian 2026 1 1)) db, activityName (w1Activity w) == name, (n, ex) <- w1Lines w, pick ex]
                bought ex = case ex of
                    TechnosphereExchange{techRole = Input} -> True
                    _ -> False
            case (numbersOf "mix" bought, numbersOf "cheese and whey production" exchangeIsProductOutput) of
                ([cheese, whey], productLines) -> do
                    cheese `shouldNotBe` whey
                    productLines `shouldSatisfy` elem whey
                found -> expectationFailure (show found)
            xml <- writtenFile db
            whole <- loadAs ReadWhole xml
            divided <- loadAs ReadDeclared xml
            sort (boughtProducts whole "mix") `shouldBe` [Just "cheese", Just "whey"]
            sort (boughtProducts divided "mix") `shouldBe` [Just "cheese", Just "whey"]

        it "links a buyer of a product two blocks make to its own block again, read whole or divided" $ do
            let butterAndWhey = [line 12 "butter" "kg" "1" (output "0"), line 11 "whey" "kg" "9" (output "2")]
                buyer number name supplier = dataset number name [line 60 name "kg" "1" (output "0"), line supplier "whey" "kg" "1" (input "5")] []
                source =
                    document
                        [ dataset 7 "cheese and whey production" (cheeseAndWhey <> sharedLines) [key 10 "80" everyLine, key 11 "20" everyLine]
                        , dataset 8 "butter and whey production" (butterAndWhey <> sharedLines) [key 12 "70" everyLine, key 11 "30" everyLine]
                        , buyer 50 "pizza" 7
                        , buyer 51 "ricotta" 8
                        ]
                linksTo db name block = all (`elem` activityOf db block) (linkOf db name) && not (null (linkOf db name))
            db <- loadAs ReadWhole source
            (linksTo db "pizza" "cheese and whey production", linksTo db "ricotta" "butter and whey production") `shouldBe` (True, True)
            xml <- writtenFile db
            whole <- loadAs ReadWhole xml
            divided <- loadAs ReadDeclared xml
            (linksTo whole "pizza" "cheese and whey production", linksTo whole "ricotta" "butter and whey production") `shouldBe` (True, True)
            (linksTo divided "pizza" "cheese and whey production", linksTo divided "ricotta" "butter and whey production") `shouldBe` (True, True)

        it "refuses a block buying its own product, which a key could not tell from the product's line" $ do
            let consuming = document [dataset 7 "cheese and whey production" (cheeseAndWhey <> [line 7 "whey" "kg" "1" (input "5")] <> drop 1 sharedLines) [key 10 "80" [7, 21, 30], key 11 "20" [7, 21, 30]]]
            db <- loadAs ReadWhole consuming
            fromLeft "" (writing db) `shouldSatisfy` T.isInfixOf "carry one number"

        it "writes a block without keys whose lines share a number, since no key addresses them" $ do
            let consuming = document [dataset 7 "cheese and whey production" (cheeseAndWhey <> [line 7 "cheese" "kg" "0.1" (input "5")] <> drop 1 sharedLines) [key 10 "0" [7, 21, 30], key 11 "0" [7, 21, 30]]]
            db <- loadAs ReadWhole consuming
            either T.unpack (const "") (writing db) `shouldBe` ""

        it "refuses a coproduct a dataset of the file makes under the same activity, which one number would name" $ do
            db <- loadAs ReadWhole (withBuyers [key 10 "80" everyLine, key 11 "20" everyLine])
            let acts = sdbActivities db
                coproducts = [(activity, techFlowId ex) | ((activity, _), act) <- M.toList acts, ex@TechnosphereExchange{techRole = Coproduct} <- exchanges act]
            case (coproducts, [act | act <- M.elems acts, activityName act == "pizza"]) of
                ([made], [pizza]) -> fromLeft "" (writing db{sdbActivities = M.insert made pizza acts}) `shouldSatisfy` T.isInfixOf "one number would name both"
                found -> expectationFailure (show (length (fst found)) <> " coproducts")

        it "refuses a block making one product on two lines" $ do
            db <- loadAs ReadWhole (cheeseBlock cheeseAndWhey [key 10 "80" everyLine, key 11 "20" everyLine])
            let cheese = [exchangeFlowId ex | ex <- productRows db, exchangeIsReference ex]
                twice ex = case (ex, cheese) of
                    (TechnosphereExchange{}, [flow]) -> ex{techFlowId = flow}
                    _ -> ex
            fromLeft "" (writing (onProducts twice db)) `shouldSatisfy` T.isInfixOf "one product on two lines"

    describe "writing repeated lines" $
        it "writes one flow stated twice in a dataset as one line of their total, and says so" $ do
            let twice = document [dataset 7 "cheese production" [line 10 "cheese" "kg" "1" (output "0"), line 20 "milk" "kg" "4" (input "5"), line 20 "milk" "kg" "6" (input "5")] []]
            db <- loadAs ReadWhole twice
            xml <- writtenFile db
            length (filter ("name=\"milk\"" `isIn`) (lines (BC.unpack xml))) `shouldBe` 1
            BC.unpack xml `shouldSatisfy` ("meanValue=\"10.0\"" `isIn`)
            blockWarnings (canonicalWriterOptions (fromGregorian 2026 1 1)) db `shouldSatisfy` any (T.isInfixOf "one line of their total")
