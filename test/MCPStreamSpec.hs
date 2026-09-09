{-# LANGUAGE OverloadedStrings #-}

{- | The @\/mcp@ endpoint answers POST and refuses everything else.

A GET opens the stream a server uses to speak to a client unprompted. VoLCA
never speaks first, and an empty stream closed at once reads to a client as a
dropped connection: it reconnects, and the pair loops for as long as both are
up. One such pair sent 71 644 GETs in 21 hours. 405 says there is no stream,
and the client stops asking.
-}
module MCPStreamSpec (spec) where

import Data.ByteString (ByteString)
import Data.IORef
import Network.HTTP.Types (Method)
import Network.HTTP.Types.Header (hAllow)
import Network.HTTP.Types.Status (statusCode)
import Network.Wai (defaultRequest, requestMethod, responseHeaders, responseStatus)
import Network.Wai.Internal (ResponseReceived (..))
import Test.Hspec

import API.MCP (mcpApp)
import Config (defaultConfig)
import Database.Manager (CachePolicy (..), initDatabaseManager)

{- | Drive one request of the given method through the endpoint,
report its status and the methods it says are allowed.
-}
answer :: Method -> IO (Int, Maybe ByteString)
answer m = do
    manager <- initDatabaseManager defaultConfig NoCache
    app <- mcpApp manager [] False Nothing Nothing id
    ref <- newIORef Nothing
    _ <- app defaultRequest{requestMethod = m} $ \resp -> do
        writeIORef ref (Just resp)
        pure ResponseReceived
    let read' resp = (statusCode (responseStatus resp), lookup hAllow (responseHeaders resp))
    maybe (fail "no response") (pure . read') =<< readIORef ref

spec :: Spec
spec = describe "the /mcp endpoint" $ do
    it "refuses a GET rather than hand back a stream that closes at once" $
        answer "GET" `shouldReturn` (405, Just "POST")

    it "refuses a DELETE the same way" $
        answer "DELETE" `shouldReturn` (405, Just "POST")
