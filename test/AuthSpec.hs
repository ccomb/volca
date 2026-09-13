{-# LANGUAGE OverloadedStrings #-}

module AuthSpec (spec) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import Data.IORef
import Network.HTTP.Types (Method, RequestHeaders, status200)
import Network.HTTP.Types.Header (hAuthorization)
import Network.HTTP.Types.Status (statusCode)
import Network.Wai (
    Application,
    defaultRequest,
    rawPathInfo,
    requestHeaders,
    requestMethod,
    responseHeaders,
    responseLBS,
    responseStatus,
 )
import Network.Wai.Internal (ResponseReceived (..))
import Test.Hspec

import API.Auth (authMiddleware)

password :: ByteString
password = "s3cret"

-- An inner app that records being called and returns 200.
okApp :: IORef Bool -> Application
okApp called _req respond = do
    writeIORef called True
    respond (responseLBS status200 [] "ok")

-- Run the middleware against a fabricated request and report (innerWasCalled, statusCode).
runAuth :: Method -> ByteString -> RequestHeaders -> IO (Bool, Int)
runAuth m path hs = do
    let req =
            defaultRequest
                { requestMethod = m
                , rawPathInfo = path
                , requestHeaders = hs
                }
    called <- newIORef False
    respVar <- newIORef Nothing
    _ <- authMiddleware password (okApp called) req $ \resp -> do
        writeIORef respVar (Just resp)
        pure ResponseReceived
    Just resp <- readIORef respVar
    wasCalled <- readIORef called
    pure (wasCalled, statusCode (responseStatus resp))

basicHeader :: ByteString -> ByteString -> RequestHeaders
basicHeader user pass =
    [(hAuthorization, "Basic " <> B64.encode (user <> ":" <> pass))]

bearerHeader :: ByteString -> RequestHeaders
bearerHeader tok = [(hAuthorization, "Bearer " <> tok)]

cookieHeader :: ByteString -> RequestHeaders
cookieHeader v = [("Cookie", v)]

spec :: Spec
spec = do
    describe "public routes bypass auth" $ do
        it "lets GET /index.html through without credentials" $ do
            (called, code) <- runAuth "GET" "/index.html" []
            called `shouldBe` True
            code `shouldBe` 200

        it "lets GET / through without credentials" $ do
            (called, code) <- runAuth "GET" "/" []
            called `shouldBe` True
            code `shouldBe` 200

        it "lets POST /api/v1/auth (the login endpoint) through without credentials" $ do
            (called, code) <- runAuth "POST" "/api/v1/auth" []
            called `shouldBe` True
            code `shouldBe` 200

        it "does NOT bypass when /api/v1/auth is GET (only POST is the login)" $ do
            (called, code) <- runAuth "GET" "/api/v1/auth" []
            called `shouldBe` False
            code `shouldBe` 401

    describe "the assistant protocol is guarded too" $ do
        -- /mcp reaches the same operations as the REST API, so a password that
        -- closed one and left the other open would read as protection and be
        -- none: an unauthenticated caller could load, upload and delete.
        it "rejects POST /mcp without credentials" $ do
            (called, code) <- runAuth "POST" "/mcp" []
            called `shouldBe` False
            code `shouldBe` 401

        it "lets POST /mcp through with the password" $ do
            (called, code) <- runAuth "POST" "/mcp" (bearerHeader password)
            called `shouldBe` True
            code `shouldBe` 200

    describe "protected /api/ routes – no credentials" $ do
        it "rejects with 401 when Authorization and Cookie are absent" $ do
            (called, code) <- runAuth "GET" "/api/v1/db" []
            called `shouldBe` False
            code `shouldBe` 401

        it "rejects with 401 on POST /api/v1/db without credentials" $ do
            (called, code) <- runAuth "POST" "/api/v1/db" []
            called `shouldBe` False
            code `shouldBe` 401

    describe "Bearer token" $ do
        it "accepts a correct Bearer token" $ do
            (called, code) <- runAuth "GET" "/api/v1/db" (bearerHeader password)
            called `shouldBe` True
            code `shouldBe` 200

        it "rejects an empty Bearer token" $ do
            (called, code) <- runAuth "GET" "/api/v1/db" (bearerHeader "")
            called `shouldBe` False
            code `shouldBe` 401

        it "rejects a wrong Bearer token" $ do
            (called, code) <- runAuth "GET" "/api/v1/db" (bearerHeader "not-the-pwd")
            called `shouldBe` False
            code `shouldBe` 401

        it "rejects lowercase 'bearer' scheme (prefix match is case-sensitive)" $ do
            (called, code) <- runAuth "GET" "/api/v1/db" [(hAuthorization, "bearer " <> password)]
            called `shouldBe` False
            code `shouldBe` 401

        it "rejects 'Bearer' with extra leading whitespace before the token" $ do
            (called, code) <- runAuth "GET" "/api/v1/db" [(hAuthorization, "Bearer   " <> password)]
            called `shouldBe` False
            code `shouldBe` 401

    describe "Basic auth" $ do
        it "accepts a correct Basic auth password (any username)" $ do
            (called, code) <- runAuth "GET" "/api/v1/db" (basicHeader "alice" password)
            called `shouldBe` True
            code `shouldBe` 200

        it "rejects a wrong password under Basic auth" $ do
            (called, code) <- runAuth "GET" "/api/v1/db" (basicHeader "alice" "nope")
            called `shouldBe` False
            code `shouldBe` 401

        it "rejects when the Base64 payload is malformed" $ do
            (called, code) <- runAuth "GET" "/api/v1/db" [(hAuthorization, "Basic !!notbase64!!")]
            called `shouldBe` False
            code `shouldBe` 401

        it "rejects when the Base64 payload has no colon (RFC 7617 requires user:pass)" $ do
            -- A payload without a colon is not a valid RFC 7617 Basic credential.
            -- We reject it rather than treating the whole payload as a password.
            let encoded = B64.encode password
            (called, code) <- runAuth "GET" "/api/v1/db" [(hAuthorization, "Basic " <> encoded)]
            called `shouldBe` False
            code `shouldBe` 401

    describe "Cookie auth" $ do
        it "accepts the correct volca_session cookie" $ do
            (called, code) <- runAuth "GET" "/api/v1/db" (cookieHeader ("volca_session=" <> password))
            called `shouldBe` True
            code `shouldBe` 200

        it "rejects a wrong cookie value" $ do
            (called, code) <- runAuth "GET" "/api/v1/db" (cookieHeader "volca_session=wrong")
            called `shouldBe` False
            code `shouldBe` 401

        it "rejects when the cookie name is unrelated" $ do
            (called, code) <- runAuth "GET" "/api/v1/db" (cookieHeader ("other_cookie=" <> password))
            called `shouldBe` False
            code `shouldBe` 401

        it "rejects when volca_session has an empty value" $ do
            (called, code) <- runAuth "GET" "/api/v1/db" (cookieHeader "volca_session=")
            called `shouldBe` False
            code `shouldBe` 401

        it "picks volca_session out of a multi-cookie header" $ do
            (called, code) <-
                runAuth
                    "GET"
                    "/api/v1/db"
                    (cookieHeader ("foo=bar; volca_session=" <> password <> "; baz=qux"))
            called `shouldBe` True
            code `shouldBe` 200

        it "tolerates whitespace around cookie pairs" $ do
            (called, code) <-
                runAuth
                    "GET"
                    "/api/v1/db"
                    (cookieHeader ("foo=bar;   volca_session=" <> password))
            called `shouldBe` True
            code `shouldBe` 200

    describe "response shape on rejection" $ do
        let mkReq =
                defaultRequest
                    { requestMethod = "GET"
                    , rawPathInfo = "/api/v1/db"
                    , requestHeaders = []
                    }
        let captureRejection = do
                respVar <- newIORef Nothing
                _ <- authMiddleware password (\_ _ -> error "inner must not run") mkReq $ \resp -> do
                    writeIORef respVar (Just resp)
                    pure ResponseReceived
                readIORef respVar >>= maybe (error "no response") pure

        it "returns 401 with Content-Type application/json" $ do
            resp <- captureRejection
            statusCode (responseStatus resp) `shouldBe` 401
            lookup "Content-Type" (responseHeaders resp) `shouldBe` Just "application/json"

        it "does NOT emit a WWW-Authenticate header (no browser auth dialog)" $ do
            resp <- captureRejection
            lookup "WWW-Authenticate" (responseHeaders resp) `shouldBe` Nothing
