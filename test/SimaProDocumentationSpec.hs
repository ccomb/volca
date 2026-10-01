{-# LANGUAGE OverloadedStrings #-}

{- | What a SimaPro export says about itself and about each of its processes:
the header that stamps the export, the System description blocks of its
trailer, and the documentation fields of a process block. Read through the
loader, then written and read again.
-}
module SimaProDocumentationSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Time.Calendar (fromGregorian)
import Data.Time.LocalTime (TimeOfDay (..))
import Database.Loader (defaultLoadOptions, loadSimaProCSV)
import Method.ParserSimaPro (isSimaProMethodCSV)
import SimaPro.Parser (SimaProConfig (..), exportWarnings, extractConfig)
import SimaPro.Writer (defaultWriterConfig, serializeSimaProCSV)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.Hspec
import Types
import UnitConversion (defaultUnitConfig)

spec :: Spec
spec = describe "SimaPro documentation" $ do
    describe "the header" $ do
        it "stamps the export with its tool, format, day, time and project" $ do
            db <- load bakeryCSV
            dbdocExport (sdbDocumentation db)
                `shouldBe` Just
                    ExportStamp
                        { exportTool = "SimaPro 10.2.0.3"
                        , exportFormatVersion = Just "9.0.0"
                        , exportDate = Just (fromGregorian 2026 5 13)
                        , exportTime = Just (TimeOfDay 17 13 58)
                        , exportProject = Just "Bakery"
                        }

        it "says so when a header is not one it can take as written" $
            exportWarnings
                (extractConfig ["{SimaPro 8.5}", "{Date: 13.05.2026}", "{Time: 5:13 PM}", "{CSV Format version: 8.0.5}"])
                `shouldBe` [ "header Date \"13.05.2026\" is not a date written dd/MM/yyyy: the export is read with no date"
                           , "header Time \"5:13 PM\" is not a time written H:mm:ss: the export is read with no time"
                           , "CSV Format version 8.0.5 is not one this reader was written against (9.0.0): it is read as if it were"
                           ]

        it "takes no stamp from a file with no banner, and says so" $ do
            let cfg = extractConfig ["{processes}", "{Date: 13/05/2026}"]
            spTool cfg `shouldBe` Nothing
            exportWarnings cfg `shouldBe` ["the header states an export but names no tool on its first line: the export is read with no stamp"]

    describe "the System description blocks" $
        it "reads each one, its quoted text unquoted and its blank fields left out" $ do
            db <- load bakeryCSV
            dbdocSystems (sdbDocumentation db)
                `shouldBe` [ SystemDescription
                                { systemName = "Bakery"
                                , systemCategory = "Food"
                                , systemSections =
                                    [ DocSection "Description" "Ovens and mixers\n\nThe bakery's own \"rules\""
                                    , DocSection "Cut-off rules" "Less than 1%"
                                    ]
                                }
                           ]

    describe "a process's documentation" $ do
        it "reads every field it fills, in the file's order" $ do
            db <- load bakeryCSV
            map activityDocumentation (M.elems (sdbActivities db))
                `shouldBe` [
                               [ DocSection "Time period" "2010 and after"
                               , DocSection "Record" "Entered by the baker"
                               , DocSection "Literature references" "Bread book (chapter 2)\nFlour study"
                               , DocSection "Collection method" "Sampling: daily; weighed"
                               , DocSection "System description" "Bakery"
                               ]
                           ]

        it "reads a quoted comment without its quotes" $ do
            db <- load bakeryCSV
            map activityDescription (M.elems (sdbActivities db)) `shouldBe` [["Plain \"white\" bread"]]

    describe "written and read again" $ do
        it "names the engine in its banner and the format version it follows" $ do
            written <- write =<< load bakeryCSV
            written `shouldSatisfy` BS.isPrefixOf "{VoLCA "
            written `shouldSatisfy` BS.isInfixOf "}\r\n{CSV Format version: 9.0.0}\r\n"

        it "keeps the System descriptions and the free-text documentation" $ do
            original <- load bakeryCSV
            again <- load =<< write original
            dbdocSystems (sdbDocumentation again) `shouldBe` dbdocSystems (sdbDocumentation original)
            map activityDescription (M.elems (sdbActivities again)) `shouldBe` [["Plain \"white\" bread"]]
            map (map docLabel . activityDocumentation) (M.elems (sdbActivities again))
                `shouldBe` [["Record", "Collection method", "System description"]]

        it "reads its own method export as a method file" $
            isSimaProMethodCSV "{VoLCA 0.15.0}\r\n{methods}\r\n" `shouldBe` True

load :: BS.ByteString -> IO SimpleDatabase
load bytes = withSystemTempFile "documentation-spec.csv" $ \path h -> do
    BS.hPut h bytes
    hClose h
    either (fail . T.unpack) pure =<< loadSimaProCSV (defaultLoadOptions defaultUnitConfig) path

write :: SimpleDatabase -> IO BS.ByteString
write = either (fail . T.unpack) pure . serializeSimaProCSV defaultWriterConfig

-- | One process naming one System description, the way SimaPro 10 exports them.
bakeryCSV :: BS.ByteString
bakeryCSV =
    BS.intercalate
        "\r\n"
        [ "{SimaPro 10.2.0.3}"
        , "{processes}"
        , "{Date: 13.05.2026}"
        , "{Time: 17:13:58}"
        , "{Project: Bakery}"
        , "{CSV Format version: 9.0.0}"
        , "{CSV separator: Semicolon}"
        , "{Decimal separator: .}"
        , "{Date separator: .}"
        , "{Short date format: dd/MM/yyyy}"
        , ""
        , "Process"
        , ""
        , "Category type"
        , "material"
        , ""
        , "Process name"
        , "Bread production"
        , ""
        , "Time period"
        , "2010 and after"
        , ""
        , "Technology"
        , "Unspecified"
        , ""
        , "Record"
        , "Entered by the baker\x7f"
        , ""
        , "Generator"
        , ""
        , ""
        , "Literature references"
        , "Bread book;chapter 2"
        , "Flour study;"
        , ""
        , "Collection method"
        , "\"Sampling: daily; weighed\""
        , ""
        , "Comment"
        , "\"Plain \"\"white\"\" bread\""
        , ""
        , "System description"
        , "Bakery;"
        , ""
        , "Products"
        , "Bread;kg;1;100;not defined;material;"
        , ""
        , "End"
        , ""
        , "System description"
        , ""
        , "Name"
        , "Bakery"
        , ""
        , "Category"
        , "Food"
        , ""
        , "Description"
        , "\"Ovens and mixers\x7f\x7fThe bakery's own \"\"rules\"\"\""
        , ""
        , "Sub-systems"
        , ""
        , ""
        , "Cut-off rules"
        , "Less than 1%"
        , ""
        , "Energy model"
        , ""
        , ""
        , "End"
        , ""
        ]
