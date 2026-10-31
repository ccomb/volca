{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | The method collections an engine offers to download and load in one
step, as @data/method-catalog.toml@ states them, and the download that keeps a
file only when it is the one the catalogue states.
-}
module Method.Catalog (
    CatalogEntry (..),
    methodCatalog,
    parseMethodCatalog,
    FetchFailure (..),
    fetchEntry,
) where

import Builtin (builtinMethodCatalog)
import Control.Exception (try)
import Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Database.Digest (fileDigest)
import Network.HTTP.Client (BodyReader, HttpException (..), HttpExceptionContent (..), Request (..), brRead, managerResponseTimeout, parseRequest, responseBody, responseStatus, responseTimeoutMicro, throwErrorStatusCodes, withResponse)
import Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)
import Network.HTTP.Types (Status (..), hUserAgent)
import System.IO (Handle, IOMode (WriteMode), withBinaryFile)
import System.Timeout (timeout)
import TOML (DecodeTOML (..), Decoder, decodeWith, getArrayOf, getField, getFieldOpt, getFieldWith, renderTOMLError)
import Types (StandardLicence, parseSpdx)
import qualified Version

data CatalogEntry = CatalogEntry
    { ceName :: !Text
    -- ^ Also the name of the collection once installed.
    , cePublisher :: !Text
    , ceVersion :: !Text
    , ceLicence :: !(Maybe StandardLicence)
    -- ^ 'Nothing' when the file states none.
    , ceSourceUrl :: !Text
    -- ^ The page the publisher describes it on, to cite.
    , ceUrl :: !Text
    -- ^ The file itself, over https.
    , ceSize :: !Int
    , ceSha256 :: !Text
    , ceDescription :: !Text
    }
    deriving (Show, Eq)

instance DecodeTOML CatalogEntry where
    tomlDecoder = do
        ceName <- getField "name"
        cePublisher <- getField "publisher"
        ceVersion <- getField "version"
        ceLicence <- traverse (either (fail . T.unpack) pure . parseSpdx) =<< getFieldOpt "licence"
        ceSourceUrl <- getField "source_url"
        ceUrl <- getFieldWith https "url"
        ceSize <- getField "size"
        ceSha256 <- getField "sha256"
        ceDescription <- getField "description"
        pure CatalogEntry{..}
      where
        https :: Decoder Text
        https = do
            url <- tomlDecoder
            unless ("https://" `T.isPrefixOf` url) (fail ("url: a file is downloaded over https only, got " <> T.unpack url))
            pure url

parseMethodCatalog :: Text -> Either Text [CatalogEntry]
parseMethodCatalog =
    either (Left . renderTOMLError) Right . decodeWith (getFieldWith (getArrayOf tomlDecoder) "collection")

-- | The catalogue the binary carries; 'Left' only for a file the tests refuse.
methodCatalog :: Either Text [CatalogEntry]
methodCatalog = parseMethodCatalog (T.decodeUtf8 (BL.toStrict builtinMethodCatalog))

-- | Why a downloaded file was not kept.
data FetchFailure
    = -- | The server could not be reached, or did not answer with the file.
      Unreachable !Text
    | -- | The file received is not the one the catalogue states.
      NotAsCatalogued !Text
    deriving (Show, Eq)

{- | Download the file of an entry into a path the caller owns and removes,
following no redirect, stopping one byte past the size the catalogue states,
so a wrong server cannot fill the disk, and giving up on a server that stops
sending, so it cannot hold the request open.
-}
fetchEntry :: CatalogEntry -> FilePath -> IO (Either FetchFailure ())
fetchEntry entry path = do
    manager <- newTlsManagerWith tlsManagerSettings{managerResponseTimeout = responseTimeoutMicro silence}
    received <- try $ do
        request <- parseRequest (T.unpack (ceUrl entry))
        withBinaryFile path WriteMode $ \h ->
            withResponse (asked request) manager $
                copyAtMost h . responseBody
    case received of
        Left (e :: HttpException) -> pure (Left (Unreachable (ceUrl entry <> " could not be downloaded: " <> why e)))
        Right (Left failure) -> pure (Left failure)
        Right (Right ()) -> do
            digest <- fileDigest path
            pure $
                if digest == ceSha256 entry
                    then Right ()
                    else Left (NotAsCatalogued ("the file received has SHA-256 " <> digest <> ", the catalogue states " <> ceSha256 entry))
  where
    expected = ceSize entry
    silence = 60000000

    -- Some file servers refuse a request that does not say what sends it.
    asked :: Request -> Request
    asked r =
        r
            { redirectCount = 0
            , checkResponse = throwErrorStatusCodes
            , requestHeaders = (hUserAgent, T.encodeUtf8 ("VoLCA/" <> T.pack Version.version)) : requestHeaders r
            }

    why :: HttpException -> Text
    why (HttpExceptionRequest _ (StatusCodeException response _)) =
        let Status code message = responseStatus response
         in "the server answered " <> T.pack (show code) <> " " <> T.decodeUtf8Lenient message <> "."
    why (HttpExceptionRequest _ content) = T.pack (show content)
    why (InvalidUrlException _ reason) = T.pack reason

    copyAtMost :: Handle -> BodyReader -> IO (Either FetchFailure ())
    copyAtMost h body = go 0
      where
        go !n =
            timeout silence (brRead body) >>= \case
                Nothing -> pure (Left (Unreachable (ceUrl entry <> " could not be downloaded: the server stopped sending.")))
                Just chunk -> next n chunk
        next n chunk = do
            let n' = n + BS.length chunk
            if
                | BS.null chunk -> pure (if n == expected then Right () else Left (sizeMismatch (T.pack (show n))))
                | n' > expected -> pure (Left (sizeMismatch ("more than " <> T.pack (show expected))))
                | otherwise -> BS.hPut h chunk >> go n'

    sizeMismatch :: Text -> FetchFailure
    sizeMismatch got = NotAsCatalogued ("the file received is " <> got <> " bytes long, the catalogue states " <> T.pack (show expected))
