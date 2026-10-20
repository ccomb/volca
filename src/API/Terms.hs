{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Holding a database back until its reader accepts its licence, when its
publisher asks it. A reader shows they accepted by sending the fingerprint of
the text in a cookie, which a browser does on every request by itself.
-}
module API.Terms (
    termsMiddleware,
    acceptedFromCookie,
    databasesNamed,
    referencedDatabases,
    termsRefusal,
) where

import API.Auth (parseCookieValue)
import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import Data.Char (toLower)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (toList)
import Data.IORef (atomicModifyIORef', newIORef)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Network.HTTP.Types (Method, Query, methodDelete)
import Network.HTTP.Types.Header (hContentType, hCookie)
import Network.HTTP.Types.Status (status403)
import Network.Wai
import Types (Terms (..), missingTerms)

{- | Refuse a request that reads a database whose terms the reader has not
accepted, with those terms and how to accept them. The terms of a list of
databases come from the caller, so the middleware is tested without an engine.
-}
termsMiddleware :: ([Text] -> IO [Terms]) -> Middleware
termsMiddleware asked app req respond = case pathInfo req of
    "api" : "v1" : "db" : _ -> do
        (body, req') <- jsonBody req
        terms <- asked (databasesNamed (requestMethod req) (pathInfo req) (queryString req) body)
        case missingTerms (acceptedFromCookie (lookup hCookie (requestHeaders req))) terms of
            [] -> app req' respond
            missing -> respond (responseLBS status403 [(hContentType, "application/json")] (encode (termsRefusal missing)))
    _ -> app req respond

-- | The fingerprints a reader accepted, sent as @volca_terms=<one>.<another>@.
acceptedFromCookie :: Maybe ByteString -> S.Set Text
acceptedFromCookie header =
    S.fromList
        [ decodeUtf8Lenient fingerprint
        | Just value <- [parseCookieValue "volca_terms" =<< header]
        , fingerprint <- C8.split '.' value
        , not (BS.null fingerprint)
        ]

{- | The databases a request reads: the one its path names, unless the route
only manages it; the one it compares with; and every one a reference in its
body names as @database::process@, the way a substitution reaches into another
database.
-}
databasesNamed :: Method -> [Text] -> Query -> Maybe Value -> [Text]
databasesNamed method path query body = named path ++ compared ++ maybe [] referencedDatabases body
  where
    named ("api" : "v1" : "db" : name : rest) | not (manages rest) = [name]
    named _ = []

    -- Managing a database reads none of its data, and its list of databases shows the terms to accept.
    manages [] = method == methodDelete
    manages (route : _) = route `elem` ["setup", "load", "unload", "licence", "release", "accept-substitution", "add-dependency", "remove-dependency", "set-data-path", "finalize"]

    compared = [decodeUtf8Lenient v | ("other_database", Just v) <- query]

-- | The databases named by the @database::process@ references anywhere in a request.
referencedDatabases :: Value -> [Text]
referencedDatabases (String s) | (database, rest) <- T.breakOn "::" s, not (T.null database), not (T.null rest) = [database]
referencedDatabases (Array values) = concatMap referencedDatabases (toList values)
referencedDatabases (Object fields) = concatMap referencedDatabases (toList fields)
referencedDatabases _ = []

-- | What a reader is answered when terms are missing: the texts, and how to accept them.
termsRefusal :: [Terms] -> Value
termsRefusal missing =
    object
        [ "error" .= ("Reading " <> T.intercalate ", " (map termsDatabase missing) <> " needs its terms of use accepted first." :: Text)
        , "terms" .= missing
        , "accept"
            .= ( "Once the reader has read and accepted these terms, send their fingerprints in the cookie volca_terms, separated by dots: volca_terms="
                    <> T.intercalate "." (map termsFingerprint missing) ::
                    Text
               )
        ]

{- | A JSON body, read and handed on to the route unchanged. Any other body is
left unread: an upload streams megabytes the route reads as they come.
-}
jsonBody :: Request -> IO (Maybe Value, Request)
jsonBody req
    | maybe False (("application/json" `BS.isPrefixOf`) . C8.map toLower) (lookup hContentType (requestHeaders req)) = do
        bytes <- strictRequestBody req
        chunks <- newIORef (BL.toChunks bytes)
        let next = atomicModifyIORef' chunks (\case [] -> ([], BS.empty); c : cs -> (cs, c))
        pure (A.decode bytes, setRequestBodyChunks next req)
    | otherwise = pure (Nothing, req)
