{-# LANGUAGE OverloadedStrings #-}

module API.Auth (
    authMiddleware,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as C8
import Network.HTTP.Types.Header (hAuthorization, hContentType)
import Network.HTTP.Types.Status (status401)
import Network.Wai

{- | WAI middleware for cookie + Bearer + Basic authentication
Skips auth for POST /api/v1/auth (login endpoint)
Checks: 1) volca_session cookie  2) Bearer token  3) Basic auth password
Returns plain 401 with JSON body (no WWW-Authenticate header, so no browser dialog)
-}
authMiddleware :: ByteString -> Middleware
authMiddleware expectedPassword app req respond =
    let path = rawPathInfo req
        isApiRoute = "/api/" `BS.isPrefixOf` path
        isLoginEndpoint = requestMethod req == "POST" && path == "/api/v1/auth"
     in -- Only protect /api/ routes; static files and SPA index are public
        -- (so the browser can load the login page)
        if not isApiRoute || isLoginEndpoint
            then app req respond
            else
                if isAuthenticated expectedPassword req
                    then app req respond
                    else respond unauthorized
  where
    unauthorized =
        responseLBS
            status401
            [(hContentType, "application/json")]
            "{\"error\":\"unauthorized\"}"

-- | Check if request is authenticated via cookie, Bearer, or Basic auth
isAuthenticated :: ByteString -> Request -> Bool
isAuthenticated expectedPassword req =
    checkCookie expectedPassword req
        || checkBearer expectedPassword req
        || checkBasic expectedPassword req

-- | Check volca_session cookie
checkCookie :: ByteString -> Request -> Bool
checkCookie expectedPassword req =
    case lookup "Cookie" (requestHeaders req) of
        Just cookieHeader -> parseCookieValue "volca_session" cookieHeader == Just expectedPassword
        Nothing -> False

{- | Parse a specific cookie value from a Cookie header
Cookie header format: "name1=value1; name2=value2"
-}
parseCookieValue :: ByteString -> ByteString -> Maybe ByteString
parseCookieValue name header =
    let pairs = map C8.strip $ C8.split ';' header
        findCookie [] = Nothing
        findCookie (p : ps) =
            case C8.break (== '=') p of
                (k, v)
                    | C8.strip k == name && not (BS.null v) ->
                        Just (BS.drop 1 v) -- drop the '='
                _ -> findCookie ps
     in findCookie pairs

-- | Check Authorization: Bearer <token>
checkBearer :: ByteString -> Request -> Bool
checkBearer expectedPassword req =
    case lookup hAuthorization (requestHeaders req) of
        Just authHeader ->
            case BS.stripPrefix "Bearer " authHeader of
                Just token -> token == expectedPassword
                Nothing -> False
        Nothing -> False

-- | Check Authorization: Basic <base64(user:pass)> (backward compat)
checkBasic :: ByteString -> Request -> Bool
checkBasic expectedPassword req =
    case lookup hAuthorization (requestHeaders req) of
        Just authHeader ->
            case parseBasicAuth authHeader of
                Just providedPassword -> providedPassword == expectedPassword
                Nothing -> False
        Nothing -> False

{- | Parse HTTP Basic Auth header
Format: "Basic base64(username:password)"
We ignore the username and only check the password
-}
parseBasicAuth :: ByteString -> Maybe ByteString
parseBasicAuth header =
    case BS.stripPrefix "Basic " header of
        Just encoded ->
            case B64.decode encoded of
                Right decoded ->
                    -- Format is "username:password", we take everything after the colon
                    case C8.elemIndex ':' decoded of
                        Just idx -> Just $ BS.drop (idx + 1) decoded
                        Nothing -> Just decoded -- No colon, treat whole thing as password
                Left _ -> Nothing
        Nothing -> Nothing
