{-# LANGUAGE OverloadedStrings #-}

module DetectFormatSpec (spec) where

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import qualified Data.Map.Strict as M
import Database.Loader (defaultLoadOptions, loadDatabaseWithLocationAliases)
import Database.Manager (DirectoryFormat (..), detectDirectoryFormat)
import Database.Upload (DatabaseFormat (..), detectDatabaseFormat)
import OlcaPackageFixture (writePackage)
import Types (SimpleDatabase (..))
import UnitConversion (defaultUnitConfig)

spec :: Spec
spec = describe "detectDirectoryFormat" $ do
    it "detects EcoSpold2 when .spold datasets sit in a subdirectory beside a root CSV" $
        -- Native ecoinvent layout: datasets/*.spold plus a top-level
        -- FilenameToActivityLookup.csv that must not mask the .spold files
        -- (otherwise the package misdetects as SimaPro CSV and loads nothing).
        withSystemTempDirectory "ecospold-detect" $ \dir -> do
            createDirectoryIfMissing True (dir </> "datasets")
            writeFile (dir </> "datasets" </> "a_b.spold") "<x/>"
            writeFile (dir </> "FilenameToActivityLookup.csv") "Filename;ActivityName\n"
            detectDirectoryFormat dir `shouldReturn` FormatSpold

    it "still detects SimaPro CSV for a directory of only CSV files" $
        withSystemTempDirectory "csv-detect" $ \dir -> do
            writeFile (dir </> "export.csv") "{ SimaPro\n"
            detectDirectoryFormat dir `shouldReturn` FormatCSV

    -- The detector knew four formats while the loader read five, so an
    -- uploaded workbook extracted into a directory came back "No supported
    -- database files found" naming the four it did know.
    it "detects a Brightway workbook sitting in a directory" $
        withSystemTempDirectory "xlsx-detect" $ \dir -> do
            writeFile (dir </> "inventory.xlsx") "PK\003\004"
            detectDirectoryFormat dir `shouldReturn` FormatExcel

    it "detects a workbook handed over as the path itself" $
        withSystemTempDirectory "xlsx-file-detect" $ \dir -> do
            let file = dir </> "inventory.xlsx"
            writeFile file "PK\003\004"
            detectDirectoryFormat file `shouldReturn` FormatExcel

    -- .spold beats a workbook the same way it beats a CSV: an EcoSpold package
    -- may ship a spreadsheet beside its datasets.
    it "keeps EcoSpold2 ahead of a workbook at the package root" $
        withSystemTempDirectory "xlsx-vs-spold" $ \dir -> do
            createDirectoryIfMissing True (dir </> "datasets")
            writeFile (dir </> "datasets" </> "a_b.spold") "<x/>"
            writeFile (dir </> "summary.xlsx") "PK\003\004"
            detectDirectoryFormat dir `shouldReturn` FormatSpold

    -- Zipping a folder puts the workbook one level down, which is how an
    -- upload usually arrives. The .spold probe has always been recursive for
    -- the same reason.
    it "finds a workbook in a subdirectory, as it does .spold datasets" $
        withSystemTempDirectory "xlsx-nested" $ \dir -> do
            createDirectoryIfMissing True (dir </> "myinventory")
            writeFile (dir </> "myinventory" </> "inventory.xlsx") "PK\003\004"
            detectDirectoryFormat dir `shouldReturn` FormatExcel

    -- Database.Upload.detectDatabaseFormat ranks .xlsx ahead of .csv. If this
    -- ranked them the other way an upload would be announced as a workbook and
    -- parsed as SimaPro CSV, yielding an empty database and no warning.
    it "prefers a workbook to a CSV sitting beside it, as the upload check does" $
        withSystemTempDirectory "xlsx-vs-csv" $ \dir -> do
            writeFile (dir </> "inventory.xlsx") "PK\003\004"
            writeFile (dir </> "units.csv") "name;unit\n"
            detectDirectoryFormat dir `shouldReturn` FormatExcel

    it "recognises none of them for a directory holding none of them" $
        withSystemTempDirectory "unknown-detect" $ \dir -> do
            writeFile (dir </> "notes.txt") "nothing to load here\n"
            detectDirectoryFormat dir `shouldReturn` FormatUnknown

    describe "an openLCA package" $ do
        it "is not taken for ILCD, though both have a processes folder" $
            withSystemTempDirectory "olca" $ \dir -> do
                writePackage dir
                detectDirectoryFormat dir `shouldReturn` FormatOpenLca
                detectDatabaseFormat dir `shouldReturn` OpenLcaPackage

        it "still leaves a processes folder without openlca.json to ILCD" $
            withSystemTempDirectory "ilcd" $ \dir -> do
                createDirectoryIfMissing True (dir </> "processes")
                detectDirectoryFormat dir `shouldReturn` FormatILCD
                detectDatabaseFormat dir `shouldReturn` ILCDProcess

        it "loads through the loader, as the configuration names it" $
            withSystemTempDirectory "olca" $ \dir -> do
                writePackage dir
                loaded <- loadDatabaseWithLocationAliases (defaultLoadOptions defaultUnitConfig) dir
                fmap (M.size . sdbActivities) loaded `shouldBe` Right 16
