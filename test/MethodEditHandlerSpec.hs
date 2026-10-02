{-# LANGUAGE OverloadedStrings #-}

{- | What the HTTP layer of a method collection's changes owns: the request
read from its JSON, the refusal mapped to a status a client can branch on, and
the journal read back in the words of the collection.
-}
module MethodEditHandlerSpec (spec) where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List (isInfixOf, nub, sort)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Servant (ServerError, errBody, errHTTPCode, runHandler)
import Test.Hspec

import API.MethodEditHandlers
import API.Types (ActivateResponse (..), FactorEditRequest, FactorSide (..), MethodChangeAPI (..), MethodEditResponse (..), MethodFlowAPI (..), MethodHistoryEntry (..))
import App.Env (AppEnv (..), AppM, runApp)
import Config (defaultConfig)
import Database.Manager (CachePolicy (..), getMethodCollection, initDatabaseManager)
import Method.Types (Method (..), MethodCF (..), MethodCollection (..))
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
            }

call :: AppEnv -> AppM a -> IO (Either ServerError a)
call e h = runHandler (runApp e h)

-- | A request as a client sends it: bytes, read by the instance the route uses.
request :: String -> IO FactorEditRequest
request body = either (\err -> fail ("the request did not decode: " <> err)) pure (eitherDecode (BSL.pack body))

-- | A copy of the built-in collection, and its « Methane » category with its « Methane, fossil » flow.
copied :: IO (AppEnv, UUID, UUID)
copied = do
    e <- env
    call e (copyMethodCollectionHandler "plain-indicators" "copy") >>= either (fail . show) (\r -> arSuccess r `shouldBe` True)
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
            either (Left . errHTTPCode) Right answer `shouldBe` Right (MethodEditResponse 1 1 (Just 1.0) (Just 27.0))

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
