{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Every answer on @\/mcp@ carries back the id its request was sent with.

The engine never reads that id: JSON-RPC lets a client pair its requests with
their answers by it, so what goes out must be exactly what came in, whatever
its shape, and @null@ when the request could not be read or named none.
-}
module MCPRequestIdSpec (spec) where

import Data.Aeson (Value (..), decode, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import Network.HTTP.Types (Method)
import Network.Wai (defaultRequest, requestMethod, setRequestBodyChunks)
import Network.Wai.Internal (Response (..), ResponseReceived (..))
import Test.Hspec

import API.MCP (mcpApp)
import Config (defaultConfig)
import Database.Manager (CachePolicy (..), initDatabaseManager)

-- | The raw body the endpoint answers one request with.
answerBody :: Method -> BL.ByteString -> IO BL.ByteString
answerBody m body = do
    manager <- initDatabaseManager defaultConfig NoCache
    app <- mcpApp manager [] False Nothing Nothing id
    chunks <- newIORef (BL.toChunks body)
    let next = atomicModifyIORef' chunks $ \case
            [] -> ([], mempty)
            (c : rest) -> (rest, c)
    ref <- newIORef Nothing
    _ <- app (setRequestBodyChunks next defaultRequest{requestMethod = m}) $ \resp -> do
        writeIORef ref (Just resp)
        pure ResponseReceived
    readIORef ref >>= \case
        Just (ResponseBuilder _ _ b) -> pure (BB.toLazyByteString b)
        _ -> fail "no buffered response"

-- | The decoded answer to a POST.
answer :: BL.ByteString -> IO Value
answer body = answerBody "POST" body >>= maybe (fail "answer is not JSON") pure . decode

field :: KM.Key -> Value -> Maybe Value
field k (Object o) = KM.lookup k o
field _ _ = Nothing

errorCode :: Value -> Maybe Value
errorCode v = field "error" v >>= field "code"

spec :: Spec
spec = describe "the JSON-RPC id on an /mcp answer" $ do
    it "echoes a number as it was written" $
        answerBody "POST" "{\"jsonrpc\":\"2.0\",\"id\":7,\"method\":\"ping\"}"
            `shouldReturn` "{\"id\":7,\"jsonrpc\":\"2.0\",\"result\":{}}"

    it "echoes a string" $
        field "id" <$> answer "{\"jsonrpc\":\"2.0\",\"id\":\"abc\",\"method\":\"ping\"}"
            `shouldReturn` Just (String "abc")

    it "echoes an object, which it does not refuse" $
        field "id" <$> answer "{\"jsonrpc\":\"2.0\",\"id\":{\"k\":1},\"method\":\"ping\"}"
            `shouldReturn` Just (object ["k" .= (1 :: Int)])

    it "answers null to an explicit null id" $
        field "id" <$> answer "{\"jsonrpc\":\"2.0\",\"id\":null,\"method\":\"ping\"}"
            `shouldReturn` Just Null

    it "answers null to a request that names no id" $
        field "id" <$> answer "{\"jsonrpc\":\"2.0\",\"method\":\"ping\"}"
            `shouldReturn` Just Null

    it "answers null to a body it cannot read" $ do
        a <- answer "{"
        (field "id" a, errorCode a) `shouldBe` (Just Null, Just (Number (-32700)))

    it "answers null to a batch, which it does not read" $ do
        a <- answer "[]"
        (field "id" a, errorCode a) `shouldBe` (Just Null, Just (Number (-32700)))

    it "echoes the id on an unknown method" $ do
        a <- answer "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"nope\"}"
        (field "id" a, errorCode a) `shouldBe` (Just (Number 3), Just (Number (-32601)))

    it "echoes the id on a tool call whose params it cannot read" $ do
        a <- answer "{\"jsonrpc\":\"2.0\",\"id\":9,\"method\":\"tools/call\",\"params\":5}"
        (field "id" a, errorCode a) `shouldBe` (Just (Number 9), Just (Number (-32602)))

    it "echoes the id through a tool's own answer" $ do
        a <- answer "{\"jsonrpc\":\"2.0\",\"id\":9,\"method\":\"tools/call\",\"params\":{\"name\":\"nope\"}}"
        (field "id" a, field "result" a >>= field "isError") `shouldBe` (Just (Number 9), Just (Bool True))

    it "answers null on a method other than POST" $ do
        a <- answerBody "GET" "" >>= maybe (fail "answer is not JSON") pure . decode
        field "id" a `shouldBe` Just Null
