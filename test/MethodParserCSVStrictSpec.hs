{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the strict reading of a method CSV ("Method.ParserCSV").
module MethodParserCSVStrictSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Hspec

import Method.ParserCSV (CsvDefect (..), knownTops, parseMethodCSVBytes, parseMethodCSVStrict)
import Method.Types

{- | A one-category table under a comment line, so the first data row is on
line 5: comment, names, units, label.
-}
table :: [Text] -> Text
table rows =
    T.unlines $
        [ "# methodology: test"
        , ";;;;GWP"
        , ";;;;kg CO2 eq"
        , "substance;compartment;cas;unit;"
        ]
            <> rows

strict :: Text -> Either [CsvDefect] [Method]
strict = parseMethodCSVStrict . TE.encodeUtf8

-- | The single defect of a refused table, failing the test on anything else.
oneDefect :: Text -> IO CsvDefect
oneDefect txt = case strict txt of
    Left [d] -> pure d
    other -> expectationFailure ("expected one defect, got " <> show other) >> fail "unreachable"

-- | The lines a refused table names, in order.
defectLines :: Text -> Either [Int] ()
defectLines = either (Left . map cdLine) (const (Right ())) . strict

spec :: Spec
spec = describe "Method.ParserCSV strict" $ do
    it "reads a well-formed table" $ do
        let Right [m] =
                strict $
                    table
                        [ "Nitrogen oxides;air;;g;1.5"
                        , "Ammonia;water/groundwater;7664-41-7;kg;54"
                        , "Water;water/groundwater, long-term;;m3;2"
                        ]
        map mcfFlowName (methodFactors m) `shouldBe` ["Nitrogen oxides", "Ammonia", "Water"]
        map mcfValue (methodFactors m) `shouldBe` [1.5, 54, 2]
        map mcfUnit (methodFactors m) `shouldBe` ["g", "kg", "m3"]
        map mcfCompartment (methodFactors m)
            `shouldBe` [ Just (Compartment "air" "" "")
                       , Just (Compartment "water" "groundwater" "")
                       , Just (Compartment "water" "groundwater, long-term" "")
                       ]

    it "refuses a decimal comma, naming the line" $
        strict (table ["Nitrogen oxides;air;;g;1,5"])
            `shouldBe` Left [CsvDefect 5 "'1,5' is not a number"]

    it "refuses a decimal comma in a comma-delimited table" $ do
        let commaTable =
                T.unlines
                    [ ",,,,GWP"
                    , ",,,,kg CO2 eq"
                    , "substance,compartment,cas,unit,"
                    , "Ammonia,air,,kg,2"
                    , "Nitrogen oxides,air,,g,1,5"
                    ]
        CsvDefect line reason <- oneDefect commaTable
        line `shouldBe` 5
        reason `shouldSatisfy` T.isPrefixOf "the row has one cell too many"

    it "accepts trailing blank cells past the header" $
        fmap (map (length . methodFactors)) (strict (table ["Nitrogen oxides;air;;g;1.5;;"]))
            `shouldBe` Right [1]

    it "refuses a compartment that is not a path with a known top" $ do
        CsvDefect line reason <- oneDefect (table ["Ammonia;groundwater;;g;54"])
        line `shouldBe` 5
        reason `shouldSatisfy` T.isInfixOf "'groundwater'"
        mapM_ (\top -> reason `shouldSatisfy` T.isInfixOf top) knownTops

    it "refuses a compartment path with a third segment, naming the comma form" $ do
        CsvDefect line reason <- oneDefect (table ["Ammonia;water/groundwater/long-term;;g;54"])
        line `shouldBe` 5
        reason `shouldSatisfy` T.isInfixOf "'water/groundwater, long-term'"

    it "refuses a bad compartment once, whatever the number of categories" $ do
        let twoCategories =
                T.unlines
                    [ ";;;;GWP;Acidification"
                    , ";;;;kg CO2 eq;mol H+ eq"
                    , "substance;compartment;cas;unit;;"
                    , "Ammonia;groundwater;;g;54;2"
                    ]
        CsvDefect line _ <- oneDefect twoCategories
        line `shouldBe` 4

    it "refuses a value under a column the header does not name" $
        strict
            ( T.unlines
                [ ";;;;;GWP"
                , ";;;;;kg CO2 eq"
                , "substance;compartment;cas;unit;"
                , "Ammonia;air;;kg;2"
                ]
            )
            `shouldBe` Left [CsvDefect 4 "a value under a column the header does not name: count the delimiters of the header rows"]

    it "refuses a row with a value and a blank unit" $
        strict (table ["Ammonia;air;;;54"])
            `shouldBe` Left [CsvDefect 5 "blank unit on a row that carries a value"]

    it "refuses a row with a value and a blank compartment" $
        strict (table ["Ammonia;;;g;54"])
            `shouldBe` Left [CsvDefect 5 "blank compartment on a row that carries a value"]

    it "refuses a row with a value and a blank substance" $
        strict (table [";air;;g;54"])
            `shouldBe` Left [CsvDefect 5 "blank substance on a row that carries a value"]

    it "refuses a table without a unit key column" $
        strict
            ( T.unlines
                [ ";;;GWP"
                , ";;;kg CO2 eq"
                , "substance;compartment;cas;"
                , "Ammonia;air;;54"
                ]
            )
            `shouldBe` Left [CsvDefect 3 "no 'unit' key column: each row must name the unit of its flow"]

    it "refuses two rows with one substance, compartment and location" $ do
        let located =
                T.unlines
                    [ ";;;;;GWP"
                    , ";;;;;kg CO2 eq"
                    , "substance;compartment;cas;unit;location;"
                    , "Water;air;;m3;CH;1"
                    , "Ammonia;air;;kg;;2"
                    , "Water;Air;;m3;CH;3"
                    ]
        strict located
            `shouldBe` Left
                [ CsvDefect 4 "same substance, compartment and location as line 6"
                , CsvDefect 6 "same substance, compartment and location as line 4"
                ]

    it "accepts the same substance at two locations" $ do
        let located =
                T.unlines
                    [ ";;;;;GWP"
                    , ";;;;;kg CO2 eq"
                    , "substance;compartment;cas;unit;location;"
                    , "Water;air;;m3;CH;1"
                    , "Water;air;;m3;FR;3"
                    ]
            Right [m] = strict located
        map mcfConsumerLocation (methodFactors m) `shouldBe` [Just "CH", Just "FR"]

    it "reads a quoted name holding the delimiter" $ do
        let Right [m] = strict (table ["\"Nitrogen oxides; as NO2\";air;;g;33"])
        map mcfFlowName (methodFactors m) `shouldBe` ["Nitrogen oxides; as NO2"]

    it "refuses an unbalanced quote" $ do
        CsvDefect line _ <- oneDefect (table ["\"Nitrogen oxides;air;;g;33"])
        line `shouldBe` 5

    it "reports every defect, not only the first" $
        defectLines (table ["Ammonia;air;;g;54", "Ammonia;soil;;g;1,5", ";air;;g;2", "Water;soil;;;3"])
            `shouldBe` Left [6, 7, 8]

    it "refuses a header without a label row on line 1" $
        defectLines ";;GWP\n;;kg\n" `shouldBe` Left [1]

    it "leaves the lenient reader as it was" $ do
        let Right [m] = parseMethodCSVBytes (TE.encodeUtf8 (table ["Ammonia;groundwater;;g;54"]))
        map mcfCompartment (methodFactors m) `shouldBe` [Just (Compartment "water" "" "")]
