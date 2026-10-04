{-# LANGUAGE OverloadedStrings #-}

module DocumentFileSpec (spec) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.Directory (copyFile, createDirectoryIfMissing, createFileLink, listDirectory, removeFile)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Info (os)
import Test.Hspec

import Config (DatabaseConfig (..), defaultConfig)
import Database.Manager (CachePolicy (..), DatabaseManager, addDatabase, initDatabaseManager, loadDatabase, readDocumentFile)
import Types (AllocationKey (..), GeographyPolicy (..), Licence (..))

ilcdConfig :: DatabaseConfig
ilcdConfig =
    DatabaseConfig
        { dcName = "ilcd"
        , dcDisplayName = "ilcd"
        , dcPath = "test-data/SAMPLE.ilcd"
        , dcDescription = Nothing
        , dcLoad = False
        , dcDefault = False
        , dcDepends = []
        , dcLocationAliases = M.empty
        , dcFormat = Nothing
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

loadedManager :: IO DatabaseManager
loadedManager = managerOn "test-data/SAMPLE.ilcd"

managerOn :: FilePath -> IO DatabaseManager
managerOn dir = do
    manager <- initDatabaseManager defaultConfig NoCache
    addDatabase manager ilcdConfig{dcPath = dir}
    loadDatabase manager "ilcd" >>= either (fail . T.unpack) (const (pure ()))
    pure manager

-- | The fixture copied, with the listed file replaced by a link to one outside it.
withLinkedOut :: (FilePath -> IO a) -> IO a
withLinkedOut k = withSystemTempDirectory "ilcd-link" $ \tmp -> do
    let package = tmp </> "package"
    forM_ ["processes", "flows", "flowproperties", "unitgroups", "sources", "external_docs"] $ \sub -> do
        createDirectoryIfMissing True (package </> sub)
        names <- listDirectory ("test-data/SAMPLE.ilcd" </> sub)
        forM_ names $ \n -> copyFile ("test-data/SAMPLE.ilcd" </> sub </> n) (package </> sub </> n)
    BS.writeFile (tmp </> "secret.txt") "not part of the package"
    removeFile (package </> "external_docs" </> "coal report.pdf")
    createFileLink (tmp </> "secret.txt") (package </> "external_docs" </> "coal report.pdf")
    k package

spec :: Spec
spec = describe "readDocumentFile" $ do
    it "serves a file a literature entry lists" $ do
        manager <- loadedManager
        expected <- BS.readFile "test-data/SAMPLE.ilcd/external_docs/coal report.pdf"
        readDocumentFile manager "ilcd" "external_docs/coal report.pdf" `shouldReturn` Right expected

    it "refuses a file of the package the documentation does not list" $ do
        manager <- loadedManager
        readDocumentFile manager "ilcd" "processes/mean-amount.xml"
            `shouldReturn` Left "The documentation of ilcd lists no file processes/mean-amount.xml"

    -- Creating a link on Windows takes a privilege a test machine may not hold.
    it "refuses a listed file that is a link leading out of the package" $
        if os == "mingw32"
            then pendingWith "symbolic links need a privilege on Windows"
            else withLinkedOut $ \package -> do
                manager <- managerOn package
                readDocumentFile manager "ilcd" "external_docs/coal report.pdf"
                    `shouldReturn` Left "The documentation of ilcd lists external_docs/coal report.pdf, which leads out of its package"

    it "refuses a database that is not loaded" $ do
        manager <- initDatabaseManager defaultConfig NoCache
        readDocumentFile manager "ilcd" "external_docs/coal report.pdf"
            `shouldReturn` Left "Database not loaded: ilcd"
