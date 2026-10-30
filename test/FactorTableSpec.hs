{-# LANGUAGE OverloadedStrings #-}

{- | A table of characterization factors uploaded as a workbook: read into one
collection per method family, its flows' units and CAS numbers taken from
EcoSpold 2 master data, and written to columnar CSV that reads back the same.
-}
module FactorTableSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Either (isLeft)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import API.DatabaseHandlers (chooseMasterData)
import Database.Upload (DatabaseFormat (..), detectMethodFormat)
import EcoSpold.Parser2 (MasterFlow (..), parseElementaryExchanges)
import Method.ParserCSV (parseMethodCSVBytes)
import Method.ParserFactorTable (Dropped (..), familyCollections, isFactorTable, readFactorTable)
import Method.Types
import Method.WriterCSV (serializeColumnarMethodCSV)
import TestWorkbook (Cell (..), buildWorkbook)
import Xlsx (openWorkbook)

cfsLabels :: [Cell]
cfsLabels = map CT ["Method", "Category", "Indicator", "Name", "Compartment", "Subcompartment", "CF"]

cf :: Text -> Text -> Text -> Text -> Text -> Text -> Double -> [Cell]
cf family category indicator name top sub value = map CT [family, category, indicator, name, top, sub] <> [CN value]

indicator :: Text -> Text -> Text -> Text -> [Cell]
indicator family category ind unit = map CT [family, category, ind, unit]

master :: [MasterFlow]
master =
    [ MasterFlow "Carbon dioxide, fossil" "air" "unspecified" "kg" (Just "000124-38-9")
    , MasterFlow "Occupation, forest" "natural resource" "land" "m2*year" Nothing
    , MasterFlow "Nitrogen" "water" "unspecified" "kg" (Just "007727-37-9")
    , MasterFlow "Nitrogen, organic bound" "water" "unspecified" "kg" (Just "7727-37-9")
    ]

-- | The collections a workbook of these sheets reads to, against 'master'.
families :: [(Text, [[Cell]])] -> Either Text ([(Text, MethodCollection)], Dropped)
families sheets = familyCollections master =<< readFactorTable =<< openWorkbook (buildWorkbook sheets)

standard :: [(Text, [[Cell]])]
standard =
    [ ("Read me", [[CT "anything"]])
    ,
        ( "CFs"
        ,
            [ cfsLabels
            , cf "IPCC" "climate change" "GWP100" "Carbon dioxide, fossil" "air" "unspecified" 1
            , cf "IPCC" "climate change" "GWP20" "Carbon dioxide, fossil" "air" "unspecified" 1
            , cf "EF" "land use" "soil quality index" "Occupation, forest" "natural resource" "land" 0.5
            , cf "EF" "eutrophication" "accumulated exceedance" "Nitrogen" "water" "unspecified" 0.2
            , cf "EF" "eutrophication" "accumulated exceedance" "Mystery" "water" "unspecified" 9
            ]
        )
    ,
        ( "Indicators"
        ,
            [ map CT ["Method", "Category", "Indicator", "Indicator Unit"]
            , indicator "IPCC" "climate change" "GWP100" "kg CO2-Eq"
            , indicator "IPCC" "climate change" "GWP20" "kg CO2-Eq"
            , indicator "EF" "land use" "soil quality index" "dimensionless"
            , indicator "EF" "eutrophication" "accumulated exceedance" "mol N-Eq"
            ]
        )
    ]

methodsOf :: Text -> [(Text, MethodCollection)] -> [Method]
methodsOf family = maybe [] mcMethods . lookup family

factorsOf :: Text -> Text -> [(Text, MethodCollection)] -> [MethodCF]
factorsOf family name = concatMap methodFactors . filter ((== name) . methodName) . methodsOf family

spec :: Spec
spec = do
    describe "familyCollections" $ do
        it "reads one collection per family, in the order the table gives them" $
            fmap (map fst . fst) (families standard) `shouldBe` Right ["IPCC", "EF"]

        it "names a category's columns by their indicator only when the category heads several" $
            fmap (map methodName . methodsOf "IPCC" . fst) (families standard)
                `shouldBe` Right ["climate change / GWP100", "climate change / GWP20"]

        it "writes a factor per the flow's own unit, not the indicator's" $
            fmap (map mcfUnit . factorsOf "EF" "land use" . fst) (families standard) `shouldBe` Right ["m2*year"]

        it "takes the indicator's unit as the method's" $
            fmap (map methodUnit . methodsOf "EF" . fst) (families standard) `shouldBe` Right ["dimensionless", "mol N-Eq"]

        it "reads a natural resource as an input" $
            fmap (map mcfDirection . factorsOf "EF" "land use" . fst) (families standard) `shouldBe` Right [Input]

        it "drops a CAS number the master data gives to two names, padded or not" $
            fmap (map mcfCAS . factorsOf "EF" "eutrophication" . fst) (families standard) `shouldBe` Right [Nothing]

        it "keeps a CAS number one name owns, unpadded" $
            fmap (map mcfCAS . factorsOf "IPCC" "climate change / GWP100" . fst) (families standard) `shouldBe` Right [Just "124-38-9"]

        it "counts the factors whose flow the master data lacks, naming them" $
            fmap snd (families standard) `shouldBe` Right (Dropped 1 ["Mystery (water/unspecified)"])

        it "reads the older label of the unit column, and sheets in any order" $ do
            let renamed = [(n, if n == "Indicators" then (map CT ["Method", "Category", "Indicator", "Unit"] : drop 1 rows) else rows) | (n, rows) <- reverse standard]
            fmap (map (map methodUnit . mcMethods . snd) . fst) (families renamed)
                `shouldBe` fmap (map (map methodUnit . mcMethods . snd) . fst) (families standard)

        it "refuses a table whose columns are labelled otherwise, naming the label it lacks" $ do
            let older = [(n, if n == "CFs" then map CT ["Method", "Category", "Indicator", "Name", "Compartment", "Subcompartment", "CF 3.6"] : drop 1 rows else rows) | (n, rows) <- standard]
            fmap (const ()) (families older) `shouldBe` Left "the CFs sheet has no column labelled CF"

    describe "isFactorTable" $ do
        it "knows a workbook with a CFs and an Indicators sheet" $
            fmap isFactorTable (openWorkbook (buildWorkbook standard)) `shouldBe` Right True
        it "does not take another workbook for one" $
            fmap isFactorTable (openWorkbook (buildWorkbook [("data", [[CT "Activity"]])])) `shouldBe` Right False

    describe "a family written to columnar CSV" $ do
        it "reads back with its units and CAS numbers" $ do
            let Right (written, _) = families standard
                roundTrip c = parseMethodCSVBytes . fst =<< either (Left . T.unpack) Right (serializeColumnarMethodCSV c)
                facts ms = M.fromList [(methodName m, [(mcfFlowName f, mcfUnit f, mcfCAS f, mcfValue f) | f <- methodFactors m]) | m <- ms]
            mapM_ (\(_, c) -> fmap facts (roundTrip c) `shouldBe` Right (facts (mcMethods c))) written

        it "is detected as a columnar CSV" $
            withSystemTempDirectory "factor-table" $ \dir -> do
                let Right (written, _) = families standard
                    Just ipcc = lookup "IPCC" written
                    Right (bytes, _) = serializeColumnarMethodCSV ipcc
                BS.writeFile (dir </> "methods.csv") bytes
                detectMethodFormat dir `shouldReturn` ColumnarCSV

    describe "parseElementaryExchanges" $ do
        it "reads name, compartment, unit and CAS number" $
            parseElementaryExchanges masterXml
                `shouldBe` Right [MasterFlow "Carbon dioxide, fossil" "air" "unspecified" "kg" (Just "000124-38-9"), MasterFlow "Water" "natural resource" "" "m3" Nothing]
        it "refuses an exchange without a unit" $
            parseElementaryExchanges (BS8.pack "<validElementaryExchanges><elementaryExchange><name xml:lang=\"en\">X</name><compartment><compartment xml:lang=\"en\">air</compartment></compartment></elementaryExchange></validElementaryExchanges>")
                `shouldSatisfy` isLeft

    describe "chooseMasterData" $ do
        let candidates = [("ei-3.12", Just "/a/MasterData/ElementaryExchanges.xml"), ("agb", Nothing)]
        it "takes the only database that has master data" $
            chooseMasterData Nothing candidates `shouldBe` Right "/a/MasterData/ElementaryExchanges.xml"
        it "takes the one named" $
            chooseMasterData (Just "ei-3.12") (("ei-3.11", Just "/b") : candidates) `shouldBe` Right "/a/MasterData/ElementaryExchanges.xml"
        it "refuses to guess between several, naming them" $
            chooseMasterData Nothing (("ei-3.11", Just "/b") : candidates) `shouldSatisfy` either (T.isInfixOf "ei-3.11, ei-3.12") (const False)
        it "refuses a database without master data" $
            chooseMasterData (Just "agb") candidates `shouldSatisfy` isLeft
        it "refuses when no database has master data" $
            chooseMasterData Nothing [("agb", Nothing)] `shouldSatisfy` isLeft

masterXml :: BS.ByteString
masterXml =
    BS8.pack . concat $
        [ "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
        , "<validElementaryExchanges xmlns=\"http://www.EcoInvent.org/EcoSpold02\">"
        , "<elementaryExchange id=\"1\" casNumber=\"000124-38-9\">"
        , "<name xml:lang=\"en\">Carbon dioxide, fossil</name><unitName xml:lang=\"en\">kg</unitName>"
        , "<compartment><compartment xml:lang=\"en\">air</compartment><subcompartment xml:lang=\"en\">unspecified</subcompartment></compartment>"
        , "</elementaryExchange>"
        , "<elementaryExchange id=\"2\" casNumber=\"\">"
        , "<name xml:lang=\"en\">Water</name><unitName xml:lang=\"en\">m3</unitName>"
        , "<compartment><compartment xml:lang=\"en\">natural resource</compartment></compartment>"
        , "</elementaryExchange>"
        , "</validElementaryExchanges>"
        ]
