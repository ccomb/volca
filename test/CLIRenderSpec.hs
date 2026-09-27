{-# LANGUAGE OverloadedStrings #-}

module CLIRenderSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import Data.Either (isLeft)
import Data.List (isInfixOf)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Vector as V
import Test.Hspec

import CLI.Render (csvRows, renderResult, selectPath)
import CLI.Types (OutputFormat (..))

{- | A search response, spelled with the wire names the engine emits (the
record prefix is stripped on the way out). Two arrays, so nothing can guess
which one to flatten.
-}
searchResponse :: Value
searchResponse =
    object
        [ "results"
            .= [ object ["name" .= T.pack "electricity", "location" .= T.pack "FR"]
               , object ["name" .= T.pack "steel", "location" .= T.pack "RER"]
               ]
        , "warnings" .= ([] :: [Value])
        , "total" .= (2 :: Int)
        ]

nested :: Value
nested = object ["activity" .= object ["exchanges" .= [object ["amount" .= (1.5 :: Double)]]]]

arr :: [Value] -> Value
arr = Array . V.fromList

spec :: Spec
spec = do
    describe "selectPath" $ do
        it "resolves a single field" $
            selectPath "total" searchResponse `shouldBe` Right (Number 2)

        it "walks a dotted path through nested objects" $
            selectPath "activity.exchanges" nested
                `shouldBe` Right (arr [object ["amount" .= (1.5 :: Double)]])

        it "names the fields that are there when the path names one that is not" $
            case selectPath "result" searchResponse of
                Left err -> do
                    err `shouldSatisfy` T.isInfixOf "no field \"result\""
                    err `shouldSatisfy` T.isInfixOf "results"
                Right v -> expectationFailure ("expected a refusal, got " <> show v)

        it "says where it stopped when an intermediate step is not an object" $
            case selectPath "total.deeper" searchResponse of
                Left err -> err `shouldSatisfy` T.isInfixOf "which is a number"
                Right v -> expectationFailure ("expected a refusal, got " <> show v)

        it "reports the walked prefix in reading order" $
            case selectPath "activity.exchanges.nope" nested of
                Left err -> err `shouldSatisfy` T.isInfixOf "in \"activity.exchanges\""
                Right v -> expectationFailure ("expected a refusal, got " <> show v)

    describe "csvRows" $ do
        it "takes the array the path names, not whichever one comes first" $
            fmap length (csvRows (Just "results") searchResponse) `shouldBe` Right 2

        it "refuses a path that names something other than an array" $
            csvRows (Just "total") searchResponse `shouldSatisfy` isLeft

        it "refuses to guess when a response holds several arrays" $
            csvRows Nothing searchResponse `shouldSatisfy` isLeft

        -- The engine answers /methods and the flow routes with a bare array;
        -- naming a field in one would be naming nothing.
        it "takes a bare top-level array with no path" $
            fmap length (csvRows Nothing (arr [Number 1, Number 2])) `shouldBe` Right 2

        it "takes the sole array field with no path" $
            fmap length (csvRows Nothing (object ["databases" .= [Number 1]])) `shouldBe` Right 1

    describe "renderResult" $ do
        it "flattens the named array into a header row plus one row per element" $
            renderResult CSV (Just "results") searchResponse
                `shouldBe` Right "location,name\r\nFR,electricity\r\nRER,steel\r\n"

        it "reports the bad path instead of printing JSON where a table was asked for" $
            renderResult CSV (Just "nope") searchResponse `shouldSatisfy` isLeft

        it "emits nothing for an empty selection, not a blank line" $
            renderResult CSV (Just "warnings") searchResponse `shouldBe` Right ""

        it "ignores the path for a format that does not select an array" $
            renderResult JSON (Just "results") (object ["a" .= (1 :: Int)])
                `shouldBe` Right "{\"a\":1}\n"

        it "writes UTF-8 bytes whatever the process locale" $
            renderResult JSON Nothing (object ["n" .= T.pack "électricité"])
                `shouldBe` Right "{\"n\":\"\195\169lectricit\195\169\"}\n"

        -- A spreadsheet reads 1.0e-2 as text, and an inventory is full of
        -- amounts that small.
        it "writes small amounts in fixed notation, not exponent notation" $
            renderResult CSV Nothing (arr [object ["amount" .= (0.01 :: Double)]])
                `shouldBe` Right "amount\r\n0.01\r\n"

        -- Same guard the engine's own CSV routes apply.
        it "keeps a leading = from becoming a spreadsheet formula" $
            renderResult CSV Nothing (arr [object ["name" .= T.pack "=1+1"]])
                `shouldBe` Right "name\r\n =1+1\r\n"

    describe "renderResult Table" $ do
        let table = either (error . T.unpack) (TL.unpack . TLE.decodeUtf8) . renderResult Table Nothing
            longId = T.replicate 80 "a"
            prose = T.unwords (replicate 20 "word")

        it "shows the headline fields beside the list, not the list alone" $
            table
                ( object
                    [ "score" .= (0.96 :: Double)
                    , "unit" .= T.pack "kg"
                    , "topContributors" .= [object ["flowName" .= T.pack "carbon dioxide"]]
                    ]
                )
                `shouldBe` unlines
                    [ "score  0.96"
                    , "unit   kg"
                    , ""
                    , "topContributors (1)"
                    , "  flowName"
                    , "  --------------"
                    , "  carbon dioxide"
                    ]

        it "says an empty list is empty" $
            table (object ["topContributors" .= ([] :: [Value])]) `shouldBe` "topContributors  none\n"

        it "spreads a nested object into dotted columns and leaves out empty ones" $
            table (arr [object ["flow" .= object ["name" .= T.pack "zinc", "cas" .= Null], "note" .= Null]])
                `shouldBe` unlines ["flow.name", "---------", "zinc"]

        it "writes false rather than leaving the cell blank" $
            table (arr [object ["isEmission" .= False]]) `shouldBe` unlines ["isEmission", "----------", "no"]

        -- An identifier is there to be copied; a cut one names nothing.
        it "keeps an identifier whole and cuts long prose" $ do
            let out = table (arr [object ["id" .= longId, "text" .= prose]])
            out `shouldSatisfy` isInfixOf (T.unpack longId)
            out `shouldNotSatisfy` isInfixOf (T.unpack prose)
            out `shouldSatisfy` isInfixOf "…"

        it "keeps a value with a line break on its own row" $
            table (arr [object ["comment" .= T.pack "first\nsecond", "unit" .= T.pack "kg"]])
                `shouldBe` unlines ["comment      | unit", "-------------+-----", "first second | kg"]

        it "says the rows are there when none has a field to show" $
            table (arr [object ["note" .= Null]]) `shouldBe` "1 row, every field empty\n"

        it "leaves out an object with nothing in it to show" $
            table (object ["unit" .= T.pack "kg", "meta" .= object ["note" .= Null]]) `shouldBe` "unit  kg\n"
