{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the HTTP export handler's error mapping. A failed export must
surface as the right HTTP status (400 bad format / unexportable data, 404 not
loaded), never as a 200 body carrying a success flag: the response type (raw
bytes) cannot represent the latter.

The cheapest fixture that still drives the @Servant.runHandler@ boundary is an
empty 'Database.Manager.DatabaseManager' (no databases loaded), exactly as
"BatchImpactsSpec" does for its handlers.
-}
module ExportHandlerSpec (spec) where

import API.DatabaseHandlers (encodeExportWarnings, exportDatabaseHandler)
import API.Types (BinaryContent, DatabaseExportRequest (..))
import App.Env (AppEnv (..), runApp)
import Config (defaultConfig)
import qualified Data.Aeson as A
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Manager (CachePolicy (..), initDatabaseManager)
import Servant (Header, Headers, ServerError, errHTTPCode, runHandler)
import Test.Hspec

{- | Run the export handler against an empty database manager. The success type
has no Eq/Show, but the tests only inspect the 'Left', and the @Right _@
pattern never forces it.
-}
runExport :: Text -> Text -> IO (Either ServerError (Headers '[Header "X-Volca-Export-Warnings" Text] BinaryContent))
runExport dbName fmt = do
    dbm <- initDatabaseManager defaultConfig NoCache
    let env =
            AppEnv
                { aeDbManager = dbm
                , aeMaxTreeDepth = 10
                , aePassword = Nothing
                , aeHostingConfig = Nothing
                , aeClassificationPresets = []
                , aeDataVersion = Nothing
                }
    runHandler (runApp env (exportDatabaseHandler dbName (DatabaseExportRequest fmt Nothing)))

spec :: Spec
spec = do
    describe "exportDatabaseHandler (HTTP error mapping)" $ do
        it "returns 404 when the database is not loaded" $ do
            res <- runExport "no-such-db" "simapro"
            case res of
                Left e -> errHTTPCode e `shouldBe` 404
                Right _ -> expectationFailure "expected a 404, got a successful export"

        it "returns 400 for an unknown export format" $ do
            res <- runExport "no-such-db" "not-a-format"
            case res of
                Left e -> errHTTPCode e `shouldBe` 400
                Right _ -> expectationFailure "expected a 400, got a successful export"

    describe "DatabaseExportRequest" $ do
        it "refuses a misspelt package rather than export without it" $
            isNothing (A.decode "{\"format\":\"ecospold2\",\"packge\":\"ro-crate\"}" :: Maybe DatabaseExportRequest) `shouldBe` True

        it "reads a request that names no package" $
            (derPackage <$> A.decode "{\"format\":\"ecospold2\"}") `shouldBe` Just Nothing

    describe "encodeExportWarnings" $ do
        it "carries a short list whole" $ do
            let encoded = encodeExportWarnings ["first thing", "second thing"]
            T.isInfixOf "first" encoded `shouldBe` True
            T.isInfixOf "second" encoded `shouldBe` True
            T.isInfixOf "further" encoded `shouldBe` False

        it "stays small enough to be a header when a writer has thousands to say" $ do
            let plenty = [T.pack ("approximated activity number " <> show n) | n <- [1 :: Int .. 20000]]
            T.length (encodeExportWarnings plenty) `shouldSatisfy` (< 4096)

        it "says how many it left out rather than dropping them in silence" $ do
            let plenty = [T.pack ("warning " <> show n) | n <- [1 :: Int .. 20000]]
                encoded = encodeExportWarnings plenty
            T.isInfixOf "warning%201%0A" encoded `shouldBe` True
            T.isInfixOf "further%20warnings" encoded `shouldBe` True
