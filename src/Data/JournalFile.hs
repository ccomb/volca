{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Data.JournalFile
Description : A file of changes, one JSON line each, only ever appended to

A database and a method collection each keep a journal beside files that are
never rewritten. The two speak different words, so each says what its lines
mean ('JournalVocabulary'); the file they are written in is the same, and this
module is that file.

Every line carries the version of its vocabulary. A line this engine cannot
read is refused, never skipped.

The last line is the exception, and only when it is the last: a line is
written and flushed before its change is acknowledged, so a torn final line
belongs to a change no caller was ever told had happened. It is dropped with a
warning. A line that fails to parse anywhere else refuses the whole journal.
-}
module Data.JournalFile (
    Entry (..),
    Author (..),
    Channel (..),
    channelCode,
    Origin (..),
    noOrigin,
    Source,
    sourceFrom,
    sourceCitation,
    parseKeptSource,
    JournalVocabulary (..),
    journalPath,
    appendEntry,
    appendEntries,
    readEntries,
) where

import Control.Exception (SomeException, try)
import Control.Monad (mfilter, when)
import Data.Aeson (
    FromJSON (..),
    Object,
    ToJSON (..),
    Value,
    eitherDecodeStrict,
    encode,
    object,
    withObject,
    withText,
    (.:),
    (.:?),
    (.=),
 )
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Pair, Parser, parseEither)
import Data.Bifunctor (first)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (isSpace)
import Data.List.NonEmpty (NonEmpty)
import Data.OpenApi (ToParamSchema (..), ToSchema)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import GHC.Generics (Generic)
import Servant (FromHttpApiData (..))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.IO (
    Handle,
    IOMode (ReadWriteMode),
    SeekMode (AbsoluteSeek, SeekFromEnd),
    hFileSize,
    hSeek,
    hSetFileSize,
    withFile,
 )

import API.JsonOptions (Stripped (..))
import Progress (ProgressLevel (..), reportProgress)

{- | One line of a journal: what was done, when, by whom and through what when
that is known. All but the operation is provenance for whoever reads the file;
a replay never looks at it.
-}
data Entry op = Entry
    { jeAt :: Text
    , jeBy :: Maybe Author
    , jeVia :: Maybe Channel
    , jeOp :: op
    }
    deriving (Eq, Show)

{- | Who made a change: the account, which never changes, and the public name
it signed with that day, which may. An engine on its own (the desktop, a local
server) knows neither, and its lines have no author.
-}
data Author = Author
    { authorAccount :: Text
    , authorHandle :: Text
    }
    deriving (Eq, Show)

instance ToJSON Author where
    toJSON a = object ["account" .= authorAccount a, "handle" .= authorHandle a]

instance FromJSON Author where
    parseJSON = withObject "author" $ \o -> Author <$> o .: "account" <*> o .: "handle"

{- | The surface a change came through, as the engine saw it: never declared
by a client. HTTP does not mean a person: a script an assistant drives comes
through it too.
-}
data Channel = ThroughMcp | ThroughHttp
    deriving (Eq, Show)

channelCode :: Channel -> Text
channelCode = \case
    ThroughMcp -> "mcp"
    ThroughHttp -> "http"

instance ToJSON Channel where
    toJSON = toJSON . channelCode

instance FromJSON Channel where
    parseJSON = withText "channel" $ \case
        "mcp" -> pure ThroughMcp
        "http" -> pure ThroughHttp
        other -> fail ("unknown channel: " <> T.unpack other)

-- | What the engine itself establishes about a change it is asked to record.
data Origin = Origin
    { originBy :: Maybe Author
    , originVia :: Maybe Channel
    }
    deriving (Eq, Show)

-- | A line nobody asked for, such as those seeding a copied method collection.
noOrigin :: Origin
noOrigin = Origin Nothing Nothing

{- | Where the figures of a change come from, as its author cites it: a
document, and the page and the passage when given. Built only by 'sourceFrom',
so no source lacks a document or points at page zero.
-}
data Source = Source
    { sourceDocument :: Text
    , sourcePage :: Maybe Int
    , sourcePassage :: Maybe Text
    }
    deriving (Eq, Show, Generic)
    deriving (ToSchema) via (Stripped Source)

sourceFrom :: Text -> Maybe Int -> Maybe Text -> Either Text Source
sourceFrom document page passage
    | T.null (T.strip document) = Left "A source needs the name of its document."
    | Just number <- page, number < 1 = Left ("A source's page is counted from 1, not " <> T.pack (show number) <> ".")
    | otherwise = Right (Source (T.strip document) page (mfilter (not . T.null) (T.strip <$> passage)))

-- | A source in one line of a report: @Camplone 2025, p. 7: "63 kg N/ha"@.
sourceCitation :: Source -> Text
sourceCitation source =
    sourceDocument source
        <> maybe "" (\number -> ", p. " <> T.pack (show number)) (sourcePage source)
        <> maybe "" (\passage -> ": \"" <> passage <> "\"") (sourcePassage source)

instance ToJSON Source where
    toJSON source =
        object $
            ["document" .= sourceDocument source]
                <> maybe [] (\number -> ["page" .= number]) (sourcePage source)
                <> maybe [] (\passage -> ["passage" .= passage]) (sourcePassage source)

-- | A source as a request gives it, where a misspelt key would otherwise vanish in silence.
instance FromJSON Source where
    parseJSON = withObject "source" $ \o ->
        case filter (`notElem` ["document", "page", "passage"]) (KM.keys o) of
            [] -> sourceFields o
            unknown -> fail ("a source takes document, page and passage, not " <> T.unpack (T.intercalate ", " (map Key.toText unknown)))

-- | A source as a journal keeps it. Unknown keys pass, so an engine can still read what a later one wrote.
parseKeptSource :: Value -> Parser Source
parseKeptSource = withObject "source" sourceFields

sourceFields :: Object -> Parser Source
sourceFields o = do
    document <- o .: "document"
    page <- o .:? "page"
    passage <- o .:? "passage"
    either (fail . T.unpack) pure (sourceFrom document page passage)

-- | For the one write that carries its reason in the address: the same JSON, encoded.
instance FromHttpApiData Source where
    parseUrlPiece = first T.pack . eitherDecodeStrict . TE.encodeUtf8

instance ToParamSchema Source where
    toParamSchema _ = toParamSchema (Proxy :: Proxy Text)

{- | What the lines of one kind of journal say, and the version of those
words. Each vocabulary is versioned on its own: a database's journal and a
method collection's move apart.
-}
class JournalVocabulary op where
    -- | The version this engine writes for this vocabulary, and the only one it reads.
    vocabularyVersion :: Proxy op -> Int

    -- | The fields of one line besides @v@, @at@, @by@ and @via@.
    opFields :: op -> [Pair]

    -- | Read those fields back.
    parseOp :: Object -> Parser op

instance (JournalVocabulary op) => ToJSON (Entry op) where
    toJSON entry =
        object $
            ["v" .= vocabularyVersion (Proxy :: Proxy op), "at" .= jeAt entry]
                <> maybe [] (\by -> ["by" .= by]) (jeBy entry)
                <> maybe [] (\via -> ["via" .= via]) (jeVia entry)
                <> opFields (jeOp entry)

instance (JournalVocabulary op) => FromJSON (Entry op) where
    parseJSON = withObject "journal entry" $ \o -> do
        version <- o .: "v"
        let expected = vocabularyVersion (Proxy :: Proxy op)
        if version /= expected
            then
                fail $
                    "journal format version "
                        <> show (version :: Int)
                        <> ", but this engine reads version "
                        <> show expected
            else Entry <$> o .: "at" <*> o .:? "by" <*> o .:? "via" <*> parseOp o

-- | A journal, given the directory it lives in.
journalPath :: FilePath -> FilePath
journalPath home = home </> "journal.jsonl"

{- | Record a change. Appends one line and closes the file before returning, so
a change is on disk by the time its caller answers.

Stamps the line with the current time, which is why this takes the operation
rather than a whole entry: when it happened is the journal's business, not its
caller's.
-}
appendEntry :: (JournalVocabulary op) => FilePath -> Origin -> op -> IO (Either Text ())
appendEntry home origin = appendEntries home origin . pure

{- | Record several changes made as one: one line each, so a reader of the
file sees the same lines it would for changes made one by one, stamped with
one time and written in one call.
-}
appendEntries :: (JournalVocabulary op) => FilePath -> Origin -> NonEmpty op -> IO (Either Text ())
appendEntries home origin ops = do
    now <- getCurrentTime
    let entry op = Entry{jeAt = T.pack (iso8601Show now), jeBy = originBy origin, jeVia = originVia origin, jeOp = op}
    written <- try $ do
        createDirectoryIfMissing True home
        withFile (journalPath home) ReadWriteMode $ \handle -> do
            dropTornTail handle
            hSeek handle SeekFromEnd 0
            BL.hPut handle (foldMap (\op -> encode (entry op) <> "\n") ops)
    pure $ case written of
        Right () -> Right ()
        Left (err :: SomeException) ->
            Left $
                "could not record the edit in "
                    <> T.pack (journalPath home)
                    <> ": "
                    <> T.pack (show err)

{- | Remove a torn tail before appending, so the new line starts a line.

A file that does not end in a newline carries the tail of an append that was
cut short, which belongs to a change that was never acknowledged (the line is
on disk before the caller is answered). Appending straight after it would fuse
the new line with the debris, turning a change that /was/ acknowledged into a
line no replay can read. Truncating to the last newline drops exactly what the
torn-last-line rule would have dropped, one write earlier.
-}
dropTornTail :: Handle -> IO ()
dropTornTail handle = do
    size <- hFileSize handle
    when (size > 0) $ do
        hSeek handle AbsoluteSeek (size - 1)
        lastByte <- BS.hGet handle 1
        when (lastByte /= "\n") $ do
            hSeek handle AbsoluteSeek 0
            bytes <- BS.hGet handle (fromIntegral size)
            hSetFileSize handle (fromIntegral (BS.length (BS.dropWhileEnd (/= '\n') bytes)))

{- | Read a journal. A directory with no journal has made no changes, which is
not an error.

A torn last line is dropped with a warning (see the module header); anything
else unreadable refuses the whole file, naming the line.
-}
readEntries :: (JournalVocabulary op) => FilePath -> IO (Either Text [Entry op])
readEntries home = do
    let path = journalPath home
    exists <- doesFileExist path
    if not exists
        then pure (Right [])
        else
            try (BS.readFile path) >>= \case
                Left (err :: SomeException) ->
                    pure $ Left $ "could not read " <> T.pack path <> ": " <> T.pack (show err)
                Right bytes -> decodeLines path bytes

{- | What a line turned out to be.

The distinction is what keeps the last-line exception honest. A line that is
not JSON at all is a write that was cut short. A line that is complete JSON
says something definite, and if this engine cannot read what it says – a
version it does not know, an operation it has no verb for – that is a refusal
wherever the line sits, including at the end. Otherwise a newer engine's
entries, which are exactly the ones at the end of the file, would be dropped
as debris.
-}
data LineProblem
    = Torn String
    | Unreadable String

decodeLines :: (JournalVocabulary op) => FilePath -> BS.ByteString -> IO (Either Text [Entry op])
decodeLines path bytes =
    case problems of
        [] -> pure (Right entries)
        [(i, Torn err)] | i == lastLine -> do
            reportProgress Warning $
                "The last line of "
                    <> path
                    <> " is incomplete and was dropped ("
                    <> err
                    <> "). A line is written before its edit is acknowledged, so no\
                       \ edit anyone was told about is lost."
            pure (Right entries)
        ((i, problem) : _) -> pure (Left (situate i problem))
  where
    numbered =
        [ (i, line)
        | (i, line) <- zip [1 :: Int ..] (BS.lines bytes)
        , not (BS.all isSpace line)
        ]
    results = [(i, readLine line) | (i, line) <- numbered]
    entries = [entry | (_, Right entry) <- results]
    problems = [(i, problem) | (i, Left problem) <- results]
    lastLine = length numbered
    situate i problem =
        T.pack path <> " line " <> T.pack (show i) <> " " <> case problem of
            Torn err -> "is not complete JSON: " <> T.pack err
            Unreadable err -> "is not an entry this engine reads: " <> T.pack err

readLine :: (JournalVocabulary op) => BS.ByteString -> Either LineProblem (Entry op)
readLine line = case eitherDecodeStrict line of
    Left err -> Left (Torn err)
    Right value -> first Unreadable (parseEither parseJSON value)
