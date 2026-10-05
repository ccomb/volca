{-# LANGUAGE OverloadedStrings #-}

{- | What the HTTP layer of a method collection's changes owns: the request
read from its JSON, the refusal mapped to a status a client can branch on, and
the journal read back in the words of the collection.
-}
module MethodEditHandlerSpec (spec) where

import Control.Monad ((>=>))
import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List (isInfixOf, nub, sort)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Servant (ServerError, errBody, errHTTPCode, runHandler)
import Test.Hspec

import API.MethodEditHandlers
import API.Types (CategoryEditRequest, FactorEditRequest, FactorSide (..), MethodChangeAPI (..), MethodCollectionStatusAPI (..), MethodEditResponse (..), MethodFlowAPI (..), MethodHistoryEntry (..), RowTermAPI (..), RowTermsAPI (..), ScoreAPI (..), ScoringEditRequest, ScoringGestureAPI (..), ScoringRowAPI (..), ScoringSetAPI (..))
import App.Env (AppEnv (..), AppM, runApp)
import Config (defaultConfig)
import Database.Manager (CachePolicy (..), getMethodCollection, initDatabaseManager)
import Method.Types (Method (..), MethodCF (..), MethodCollection (..), ScoringSet (..))
import TestHelpers (withScratchDataDir)

env :: IO AppEnv
env = do
    manager <- initDatabaseManager defaultConfig NoCache
    pure
        AppEnv
            { aeDbManager = manager
            , aeMaxTreeDepth = 5
            , aePassword = Nothing
            , aeHostingConfig = Nothing
            , aeClassificationPresets = []
            , aeDataVersion = Nothing
            , aeUsageLog = Nothing
            , aeReader = Nothing
            }

call :: AppEnv -> AppM a -> IO (Either ServerError a)
call e h = runHandler (runApp e h)

-- | A request as a client sends it: bytes, read by the instance the route uses.
request :: String -> IO FactorEditRequest
request body = either (\err -> fail ("the request did not decode: " <> err)) pure (eitherDecode (BSL.pack body))

scoringRequest :: String -> IO ScoringEditRequest
scoringRequest body = either (\err -> fail ("the request did not decode: " <> err)) pure (eitherDecode (BSL.pack body))

-- | A row grouping one category, as a client writes it.
rowBody :: String -> UUID -> String
rowBody label category =
    "{\"label\":\"" <> label <> "\",\"terms\":[{\"methodId\":\"" <> UUID.toString category <> "\",\"coefficient\":1}],\"normalization\":2,\"weight\":0.5}"

categoryRequest :: String -> IO CategoryEditRequest
categoryRequest body = either (\err -> fail ("the request did not decode: " <> err)) pure (eitherDecode (BSL.pack body))

-- | A copy of the built-in collection, and its « Methane » category with its « Methane, fossil » flow.
copied :: IO (AppEnv, UUID, UUID)
copied = do
    e <- env
    call e (copyMethodCollectionHandler "plain-indicators" "copy") >>= either (fail . show) (\r -> mcaName r `shouldBe` "copy")
    collection <- getMethodCollection (aeDbManager e) "copy"
    case [(methodId m, mcfFlowRef f) | m <- maybe [] mcMethods collection, methodName m == "Methane", f <- methodFactors m, mcfFlowName f == "Methane, fossil"] of
        [(category, flow)] -> pure (e, category, flow)
        found -> fail ("expected one Methane, fossil factor, found " <> show (length found))

setBody :: UUID -> UUID -> String -> String
setBody category flow rest =
    "{\"op\":\"set\",\"methodId\":\"" <> UUID.toString category <> "\",\"flowId\":\"" <> UUID.toString flow <> "\"" <> rest <> "}"

-- | Why a request did not decode, when it did not.
refused :: Either String FactorEditRequest -> Maybe String
refused = either Just (const Nothing)

failure :: Either ServerError a -> Maybe (Int, String)
failure = either (\err -> Just (errHTTPCode err, BSL.unpack (errBody err))) (const Nothing)

spec :: Spec
spec = describe "changing a method collection over HTTP" $ do
    it "answers a copy with the collection made, under the name it is known by" $
        withScratchDataDir $ do
            e <- env
            made <- call e (copyMethodCollectionHandler "plain-indicators" "My Indicators")
            fmap (\c -> (mcaName c, mcaSource c, mcaStatus c)) made `shouldBe` Right ("my-indicators", Just "plain-indicators", "loaded")
    it "refuses a set that does not name its category, naming the field" $
        withScratchDataDir $ do
            (e, _, flow) <- copied
            body <- request ("{\"op\":\"set\",\"flowId\":\"" <> UUID.toString flow <> "\",\"newValue\":1}")
            answer <- call e (editMethodFactorsHandler "copy" body)
            fmap fst (failure answer) `shouldBe` Just 400
            maybe "" snd (failure answer) `shouldSatisfy` ("methodId" `isInfixOf`)

    it "sets a factor of a copy and says what it was" $
        withScratchDataDir $ do
            (e, category, flow) <- copied
            body <- request (setBody category flow ",\"newValue\":27")
            answer <- call e (editMethodFactorsHandler "copy" body)
            either (Left . errHTTPCode) Right answer `shouldBe` Right (MethodEditResponse 1 1 (Just 1.0) (Just 27.0) (Just category))

    it "refuses the same change on the built-in collection, and says to copy it" $
        withScratchDataDir $ do
            (e, category, flow) <- copied
            body <- request (setBody category flow ",\"newValue\":27")
            answer <- call e (editMethodFactorsHandler "plain-indicators" body)
            fmap fst (failure answer) `shouldBe` Just 409
            maybe "" snd (failure answer) `shouldSatisfy` ("Copy it" `isInfixOf`)

    it "finds a factor by a value written with every digit a double carries" $
        withScratchDataDir $ do
            (e, category, flow) <- copied
            first <- request (setBody category flow ",\"newValue\":0.30000000000000004")
            _ <- call e (editMethodFactorsHandler "copy" first)
            second <- request (setBody category flow ",\"value\":0.30000000000000004,\"newValue\":2")
            answer <- call e (editMethodFactorsHandler "copy" second)
            either (Left . errHTTPCode) (Right . merLine) answer `shouldBe` Right 2

    it "reads the history back in the words of the collection" $
        withScratchDataDir $ do
            (e, category, flow) <- copied
            body <- request (setBody category flow ",\"newValue\":27")
            _ <- call e (editMethodFactorsHandler "copy" body)
            _ <- call e (undoMethodEditHandler "copy" Nothing)
            history <- call e (methodHistoryHandler "copy")
            case history of
                Right [edit, undo] -> do
                    mheInEffect edit `shouldBe` False
                    mheUndoes undo `shouldBe` Just 1
                    case mheChange edit of
                        FactorSet{fstCategory = c, fstFactor = f, fstAfter = a} -> (c, facFlowName f, a) `shouldBe` ("Methane", "Methane, fossil", 27)
                        _ -> expectationFailure "expected the first line to set a factor"
                _ -> expectationFailure "expected two lines of history"

    it "refuses a value too large for a double, which the journal could not write back" $
        withScratchDataDir $ do
            (e, category, flow) <- copied
            body <- request (setBody category flow ",\"newValue\":1e400")
            answer <- call e (editMethodFactorsHandler "copy" body)
            fmap fst (failure answer) `shouldBe` Just 400
            maybe "" snd (failure answer) `shouldSatisfy` ("newValue" `isInfixOf`)

    it "refuses a selector field it does not know rather than reach more factors" $
        refused (eitherDecode (BSL.pack "{\"op\":\"scale\",\"match\":{\"category\":\"Methane\",\"flow_name\":\"Methane, fossil\"},\"scale\":2}") :: Either String FactorEditRequest)
            `shouldSatisfy` maybe False ("flow_name" `isInfixOf`)

    it "refuses a request field it does not know rather than address another factor" $
        refused (eitherDecode (BSL.pack "{\"op\":\"remove\",\"methodId\":\"00000000-0000-0000-0000-000000000000\",\"flowId\":\"00000000-0000-0000-0000-000000000000\",\"loc\":\"FR\"}") :: Either String FactorEditRequest)
            `shouldSatisfy` maybe False ("loc" `isInfixOf`)

    it "lists the flows a collection characterizes whose name holds the words, once each, by name" $
        withScratchDataDir $ do
            (e, _, _) <- copied
            flows <- either (fail . show) pure =<< call e (methodFlowsHandler "copy" (Just "meth") Nothing)
            let names = map mflName flows
            names `shouldSatisfy` all (T.isInfixOf "meth" . T.toLower)
            names `shouldSatisfy` elem "Methane, fossil"
            names `shouldBe` sort names
            flows `shouldBe` nub flows

    it "adds a category over HTTP and answers with its identifier" $
        withScratchDataDir $ do
            (e, _, _) <- copied
            body <- categoryRequest "{\"op\":\"add\",\"name\":\"A category of my own\",\"unit\":\"kg\"}"
            answer <- call e (editMethodCategoriesHandler "copy" body)
            either (Left . errHTTPCode) (Right . isJust . merMethodId) answer `shouldBe` Right True

    it "refuses a rename that names no category, naming the field" $
        withScratchDataDir $ do
            (e, _, _) <- copied
            body <- categoryRequest "{\"op\":\"rename\",\"name\":\"Other\"}"
            answer <- call e (editMethodCategoriesHandler "copy" body)
            fmap fst (failure answer) `shouldBe` Just 400
            maybe "" snd (failure answer) `shouldSatisfy` ("methodId" `isInfixOf`)

    it "reads a category's rename back in the history, by name" $
        withScratchDataDir $ do
            (e, category, _) <- copied
            body <- categoryRequest ("{\"op\":\"rename\",\"methodId\":\"" <> UUID.toString category <> "\",\"name\":\"Methane, all\"}")
            _ <- call e (editMethodCategoriesHandler "copy" body)
            history <- call e (methodHistoryHandler "copy")
            case map mheChange <$> history of
                Right [CategoryRenamed{crnBefore = b, crnAfter = a}] -> (b, a) `shouldBe` ("Methane", "Methane, all")
                _ -> expectationFailure "expected one line renaming the category"

    it "names a category removed since by the name it last had" $
        withScratchDataDir $ do
            (e, category, _) <- copied
            let edit op = categoryRequest ("{\"op\":\"" <> op <> "\",\"methodId\":\"" <> UUID.toString category <> "\",\"unit\":\"t\"}")
            mapM_ (edit >=> call e . editMethodCategoriesHandler "copy") ["set-unit", "remove"]
            history <- call e (methodHistoryHandler "copy")
            case map mheChange <$> history of
                Right [CategoryUnitSet{cusCategory = c}, CategoryRemoved{}] -> c `shouldBe` "Methane"
                _ -> expectationFailure "expected a change of unit, then a removal"

    it "creates a scoring set over HTTP, reads it back as rows, and names the gesture in the history" $
        withScratchDataDir $ do
            (e, category, _) <- copied
            create <- scoringRequest ("{\"op\":\"create\",\"set\":\"Mine\",\"rows\":[" <> rowBody "Gas" category <> "]}")
            created <- call e (editScoringSetsHandler "copy" create)
            fmap merLine created `shouldBe` Right 1
            sets <- call e (scoringSetsHandler "copy")
            case sets of
                Right [ScoringSetAPI{ssaName = "Mine", ssaRows = [ScoringRowAPI{sraLabel = "Gas", sraTerms = RowGrouped [term], sraWeight = Just 0.5}], ssaScores = [score]}] -> do
                    (rtaCategory term, rtaMethodId term) `shouldBe` ("Methane", Just category)
                    (scoName score, scoSumOfRows score) `shouldBe` ("Single score", True)
                _ -> expectationFailure "expected one set of one row grouping Methane, and its single score"
            add <- scoringRequest ("{\"op\":\"add-row\",\"set\":\"Mine\",\"row\":" <> rowBody "Gas twice" category <> "}")
            _ <- call e (editScoringSetsHandler "copy" add)
            history <- call e (methodHistoryHandler "copy")
            case map mheChange <$> history of
                Right [ScoringSetCreated{}, ScoringSetChanged{sschSet = "Mine", sschGesture = RowAdded{rwaLabel = label}}] -> label `shouldBe` "Gas twice"
                _ -> expectationFailure "expected a creation, then a row added"

    it "says a row counted as zero, whose normalization is infinite, and refuses one asked for in a sentence" $
        withScratchDataDir $ do
            (e, category, _) <- copied
            create <- scoringRequest ("{\"op\":\"create\",\"set\":\"Mine\",\"rows\":[" <> rowBody "Gas" category <> "]}")
            _ <- call e (editScoringSetsHandler "copy" create)
            collection <- getMethodCollection (aeDbManager e) "copy"
            -- A file writing a normalization of 0 is read as a divisor of infinity.
            let zeroed set = set{ssNormalization = M.map (const (1 / 0)) (ssNormalization set)}
            case maybe [] (\c -> map (scoringSetAPI c . zeroed) (mcScoringSets c)) collection of
                [ScoringSetAPI{ssaRows = [row]}] -> BSL.unpack (encode row) `shouldContain` "\"normalization\":\"Infinity\""
                _ -> expectationFailure "expected one set of one row"
            let infinite = "{\"label\":\"Zero\",\"terms\":[{\"methodId\":\"" <> UUID.toString category <> "\",\"coefficient\":1}],\"normalization\":\"Infinity\"}"
            add <- scoringRequest ("{\"op\":\"add-row\",\"set\":\"Mine\",\"row\":" <> infinite <> "}")
            answer <- call e (editScoringSetsHandler "copy" add)
            failure answer `shouldBe` Just (400, "the normalization of 'Zero' is Infinity, which is not a number a score can use")

    it "refuses an add-row that names no row, naming the field" $
        withScratchDataDir $ do
            (e, _, _) <- copied
            body <- scoringRequest "{\"op\":\"add-row\",\"set\":\"Mine\"}"
            answer <- call e (editScoringSetsHandler "copy" body)
            fmap fst (failure answer) `shouldBe` Just 400
            maybe "" snd (failure answer) `shouldSatisfy` ("row" `isInfixOf`)
