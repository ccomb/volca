{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The @\/mcp@ endpoint speaks two eras of the protocol on one address.

A client of the 2026-07-28 revision opens with @server/discover@, names its
version in every request's @_meta@ and repeats it, with the method and the
tool, in headers; the server checks them at the door. An older client sends
none of that and is answered as it always was.
-}
module MCPEraSpec (spec) where

import Data.Aeson (Value (..), decode, encode, object, (.=))
import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types (RequestHeaders, statusCode)
import Network.Wai (defaultRequest, requestHeaders, requestMethod, setRequestBodyChunks)
import Network.Wai.Internal (Response (..), ResponseReceived (..))
import Test.Hspec

import API.MCP (Era (..), Rejection (..), RequestId (..), RpcRequest (..), classify, mcpApp, modernVersion, sameOrigin)
import Config (ServerName (..), defaultConfig)
import Database.Manager (CachePolicy (..), initDatabaseManager)

-- | What the endpoint answered: status, headers and the decoded body.
data Answer = Answer {status :: Int, headers :: RequestHeaders, body :: Maybe Value}

-- | Post one body with the given headers to an endpoint whose instance is named "atelier".
post :: RequestHeaders -> Value -> IO Answer
post hdrs payload = do
    manager <- initDatabaseManager defaultConfig NoCache
    app <- mcpApp manager [] False Nothing (Just (ServerName "atelier")) id Nothing
    chunks <- newIORef (BL.toChunks (encode payload))
    let next = atomicModifyIORef' chunks $ \case
            [] -> ([], mempty)
            (c : rest) -> (rest, c)
    ref <- newIORef Nothing
    _ <- app (setRequestBodyChunks next defaultRequest{requestMethod = "POST", requestHeaders = hdrs}) $ \resp -> do
        writeIORef ref (Just resp)
        pure ResponseReceived
    readIORef ref >>= \case
        Just (ResponseBuilder s h b) -> pure (Answer (statusCode s) h (decode (BB.toLazyByteString b)))
        _ -> fail "no buffered response"

-- | A request of the old era: no @_meta@, no header.
legacy :: Text -> [(Key, Value)] -> Value
legacy method params = object ["jsonrpc" .= ("2.0" :: Text), "id" .= (1 :: Int), "method" .= method, "params" .= object params]

-- | A request of the modern era, its @_meta@ naming the given version.
modern :: Text -> Text -> [(Key, Value)] -> Value
modern version method params = legacy method (("_meta", meta) : params)
  where
    meta = object ["io.modelcontextprotocol/protocolVersion" .= version, "io.modelcontextprotocol/clientCapabilities" .= object ["elicitation" .= object []]]

-- | The headers a modern client sends with a request of that method, and tool if any.
modernHeaders :: Text -> Maybe Text -> RequestHeaders
modernHeaders method tool =
    [("MCP-Protocol-Version", "2026-07-28"), ("Mcp-Method", encodeText method), ("Accept", "application/json, text/event-stream")]
        <> [("Mcp-Name", encodeText t) | Just t <- [tool]]
  where
    encodeText = BL.toStrict . BB.toLazyByteString . BB.stringUtf8 . T.unpack

at :: [Key] -> Value -> Maybe Value
at [] v = Just v
at (k : ks) (Object o) = KM.lookup k o >>= at ks
at _ _ = Nothing

errorCode :: Answer -> Maybe Value
errorCode a = body a >>= at ["error", "code"]

callUnknownDb :: [(Key, Value)]
callUnknownDb = ["name" .= ("search_activities" :: Text), "arguments" .= object ["database" .= ("nowhere" :: Text), "name" .= ("x" :: Text)]]

spec :: Spec
spec = do
    describe "the old era" $ do
        it "opens with initialize, answered as 2025-03-26 under a session" $ do
            a <- post [] (legacy "initialize" [])
            (body a >>= at ["result", "protocolVersion"]) `shouldBe` Just (String "2025-03-26")
            lookup "Mcp-Session-Id" (headers a) `shouldSatisfy` (/= Nothing)

        it "answers a tool call without saying the result is complete" $ do
            a <- post [] (legacy "tools/call" callUnknownDb)
            (body a >>= at ["result", "isError"]) `shouldBe` Just (Bool True)
            (body a >>= at ["result", "resultType"]) `shouldBe` Nothing

        it "keeps a request whose _meta names no version, such as a progress token" $ do
            a <- post [] (legacy "tools/call" (("_meta", object ["progressToken" .= (1 :: Int)]) : callUnknownDb))
            status a `shouldBe` 200
            (body a >>= at ["result", "resultType"]) `shouldBe` Nothing

    describe "the 2026-07-28 era" $ do
        it "answers server/discover with its version, tools, instructions and caching hints" $ do
            a <- post (modernHeaders "server/discover" Nothing) (modern modernVersion "server/discover" [])
            let result k = body a >>= at ("result" : k)
            status a `shouldBe` 200
            result ["resultType"] `shouldBe` Just (String "complete")
            result ["supportedVersions"] `shouldBe` Just (toJSONList [modernVersion])
            result ["capabilities", "tools"] `shouldBe` Just (object [])
            result ["ttlMs"] `shouldBe` Just (Number 3600000)
            result ["cacheScope"] `shouldBe` Just (String "public")
            result ["_meta", "io.modelcontextprotocol/serverInfo", "name"] `shouldBe` Just (String "atelier")
            (result ["instructions"] >>= text) `shouldSatisfy` maybe False ("named atelier" `T.isInfixOf`)

        it "lists the same tools as the old era, with caching hints" $ do
            old <- post [] (legacy "tools/list" [])
            new <- post (modernHeaders "tools/list" Nothing) (modern modernVersion "tools/list" [])
            (body new >>= at ["result", "tools"]) `shouldBe` (body old >>= at ["result", "tools"])
            (body new >>= at ["result", "resultType"]) `shouldBe` Just (String "complete")
            (body new >>= at ["result", "cacheScope"]) `shouldBe` Just (String "public")

        it "says a failed tool call is complete, the failure being the tool's" $ do
            a <- post (modernHeaders "tools/call" (Just "search_activities")) (modern modernVersion "tools/call" callUnknownDb)
            (body a >>= at ["result", "resultType"]) `shouldBe` Just (String "complete")
            (body a >>= at ["result", "isError"]) `shouldBe` Just (Bool True)

        it "answers in JSON under no session, even to a client that accepts a stream" $ do
            a <- post (modernHeaders "tools/list" Nothing) (modern modernVersion "tools/list" [])
            lookup "Content-Type" (headers a) `shouldBe` Just "application/json"
            lookup "Mcp-Session-Id" (headers a) `shouldBe` Nothing

        it "refuses a version it does not speak, naming the one it does" $ do
            a <- post (("MCP-Protocol-Version", "2099-01-01") : drop 1 (modernHeaders "tools/list" Nothing)) (modern "2099-01-01" "tools/list" [])
            status a `shouldBe` 400
            errorCode a `shouldBe` Just (Number (-32022))
            (body a >>= at ["error", "data", "supported"]) `shouldBe` Just (toJSONList [modernVersion])
            (body a >>= at ["error", "data", "requested"]) `shouldBe` Just (String "2099-01-01")

        it "refuses headers that are missing or say something else than the body" $ do
            wrongMethod <- post (modernHeaders "tools/call" Nothing) (modern modernVersion "tools/list" [])
            wrongTool <- post (modernHeaders "tools/call" (Just "get_activity")) (modern modernVersion "tools/call" callUnknownDb)
            noVersion <- post (drop 1 (modernHeaders "tools/list" Nothing)) (modern modernVersion "tools/list" [])
            map (\a -> (status a, errorCode a)) [wrongMethod, wrongTool, noVersion]
                `shouldBe` replicate 3 (400, Just (Number (-32020)))

        it "refuses a _meta that does not say what the client can do" $ do
            let bare = legacy "tools/list" [("_meta", object ["io.modelcontextprotocol/protocolVersion" .= modernVersion])]
            a <- post (modernHeaders "tools/list" Nothing) bare
            (status a, errorCode a) `shouldBe` (400, Just (Number (-32602)))

        it "does not know the methods this revision removed, nor invented ones" $ do
            answers <- mapM (\m -> post (modernHeaders m Nothing) (modern modernVersion m [])) ["ping", "initialize", "tools/invented"]
            map (\a -> (status a, errorCode a)) answers `shouldBe` replicate 3 (404, Just (Number (-32601)))

    describe "a request from another site's page" $ do
        it "is refused" $ do
            evil <- post [("Origin", "http://evil.example"), ("Host", "localhost:8080")] (legacy "tools/list" [])
            sandboxed <- post [("Origin", "null"), ("Host", "localhost:8080")] (legacy "tools/list" [])
            map status [evil, sandboxed] `shouldBe` [403, 403]

        it "is served when its page comes from the server's own address" $ do
            a <- post [("Origin", "http://localhost:8080"), ("Host", "localhost:8080")] (legacy "tools/list" [])
            status a `shouldBe` 200

    describe "classify" $ do
        let req method params = RpcRequest (Just (RequestId (Number 1))) method (Just (object params))
        it "takes a request with no version anywhere, or an older header, for the old era" $ do
            classify [] (req "tools/list" []) `shouldBe` Right Legacy
            classify [("MCP-Protocol-Version", "2025-03-26")] (req "tools/list" []) `shouldBe` Right Legacy

        it "takes a newer header with no _meta for a malformed modern request" $
            classify [("MCP-Protocol-Version", "2026-07-28")] (req "tools/list" [])
                `shouldSatisfy` either isMalformed (const False)

        it "lets a sound modern request in" $
            classify (modernHeaders "tools/list" Nothing) (req "tools/list" (metaOf modernVersion)) `shouldBe` Right Modern

        it "sees no origin as no page, and compares the origin's host and port with Host" $ do
            sameOrigin [] `shouldBe` True
            sameOrigin [("Origin", "https://app.example"), ("Host", "app.example")] `shouldBe` True
            sameOrigin [("Origin", "http://localhost:3000"), ("Host", "localhost:8080")] `shouldBe` False
  where
    toJSONList = Array . foldMap (pure . String)
    text = \case
        String t -> Just t
        _ -> Nothing
    isMalformed = \case
        MalformedMeta _ -> True
        _ -> False
    metaOf version = [("_meta", object ["io.modelcontextprotocol/protocolVersion" .= (version :: Text), "io.modelcontextprotocol/clientCapabilities" .= object []])]
