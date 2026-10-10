{-# LANGUAGE OverloadedStrings #-}

{- | Tests for "API.BatchImpacts": the typed-error wrappers around the
hoisted LCIA batch entry points.

Two layers :

  1. 'translateError' is exercised as a pure function over synthetic
     'ServerError' values. This is where the heuristic-translation
     contract is locked down so any future drift on a throw-site body
     in "API.Routes" surfaces here, not in a downstream MCP consumer.

  2. End-to-end smoke tests on the wrappers themselves, using an empty
     'DatabaseManager' (no databases / no collections loaded). This is
     the cheapest fixture that still exercises the Servant.runHandler
     boundary and proves that error translation lands the right
     constructor on a real failure path.
-}
module BatchImpactsSpec (spec) where

import API.BatchImpacts (BatchError (..), runActivityLCIABatch, runBatchImpacts)
import API.Routes (collectionNotLoadedMessage)
import Config (defaultConfig)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as TE
import Database.Manager (CachePolicy (..), initDatabaseManager)
import Servant (err400, err404, err422, err500, errBody)
import Test.Hspec

import qualified API.BatchImpacts as BI
import Data.Text (Text)
import Method.Mapping (LongTermMode (..))

{- | Drive 'translateError' with a synthetic 'ServerError' of the given
status code and body. Picks the canonical err400/404/422/500
constructors and falls through to err500 for any other code.
-}
translateError' :: [Text] -> Int -> Text -> BatchError
translateError' avail code body =
    BI.translateError avail $ case code of
        400 -> err400{errBody = bodyBS}
        404 -> err404{errBody = bodyBS}
        422 -> err422{errBody = bodyBS}
        _ -> err500{errBody = bodyBS}
  where
    bodyBS = BSL.fromStrict (TE.encodeUtf8 body)

spec :: Spec
spec = do
    describe "translateError" $ do
        it "maps 404 + 'Collection not loaded: X' to CollectionNotLoaded" $
            translateError' ["a", "b"] 404 "Collection not loaded: EF-3.1"
                `shouldBe` CollectionNotLoaded "EF-3.1" ["a", "b"]

        it "reads the name off the real message, which also names the loaded collections" $
            translateError' ["EF-3.1", "plain-indicators"] 404 (collectionNotLoadedMessage "methods" ["EF-3.1", "plain-indicators"])
                `shouldBe` CollectionNotLoaded "methods" ["EF-3.1", "plain-indicators"]

        it "reads it back the same way when nothing is loaded" $
            translateError' [] 404 (collectionNotLoadedMessage "methods" [])
                `shouldBe` CollectionNotLoaded "methods" []

        it "maps 404 + 'Database not loaded: X' to DatabaseNotLoaded" $
            translateError' [] 404 "Database not loaded: agribalyse"
                `shouldBe` DatabaseNotLoaded "agribalyse"

        it "maps a bare 404 to ActivityResolutionFailed (verbatim body)" $
            translateError' [] 404 "Activity not found"
                `shouldBe` ActivityResolutionFailed "Activity not found"

        it "maps 400 to ActivityResolutionFailed (verbatim body)" $
            translateError' [] 400 "Invalid ProcessId format: x"
                `shouldBe` ActivityResolutionFailed "Invalid ProcessId format: x"

        it "maps 422 to Unprocessable (verbatim body)" $
            translateError' [] 422 "Cannot convert 1 m3 to kg"
                `shouldBe` Unprocessable "Cannot convert 1 m3 to kg"

        it "falls through to OtherBatchError for anything else" $
            translateError' [] 500 "internal error"
                `shouldBe` OtherBatchError 500 "internal error"

    describe "runActivityLCIABatch (empty DatabaseManager)" $ do
        it "returns DatabaseNotLoaded when the requested DB is not loaded" $ do
            dbm <- initDatabaseManager defaultConfig NoCache
            res <- runActivityLCIABatch dbm Nothing "no-such-db" "no-pid" "no-coll" Nothing IncludeLongTerm
            -- LCIABatchResult has no Show instance, so we pattern-match
            -- rather than rely on 'shouldBe' over the whole Either.
            case res of
                Left e -> e `shouldBe` DatabaseNotLoaded "no-such-db"
                Right _ -> expectationFailure "expected DatabaseNotLoaded; got a successful result"

    describe "runBatchImpacts (empty DatabaseManager)" $ do
        it "returns DatabaseNotLoaded when the requested DB is not loaded" $ do
            dbm <- initDatabaseManager defaultConfig NoCache
            res <- runBatchImpacts dbm Nothing "no-such-db" "no-coll" Nothing IncludeLongTerm ["pidA", "pidB"]
            case res of
                Left e -> e `shouldBe` DatabaseNotLoaded "no-such-db"
                Right _ -> expectationFailure "expected DatabaseNotLoaded; got a successful result"
