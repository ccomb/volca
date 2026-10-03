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
    JournalVocabulary (..),
    journalPath,
    appendEntry,
    readEntries,
) where

import Control.Exception (SomeException, bracketOnError, try)
import Control.Monad (when)
import Data.Aeson (
    FromJSON (..),
    Object,
    ToJSON (..),
    eitherDecodeStrict,
    encode,
    object,
    withObject,
    (.:),
    (.=),
 )
import Data.Aeson.Types (Pair, Parser, parseEither)
import Data.Bifunctor (first)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (isSpace)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.IO (
    Handle,
    IOMode (ReadWriteMode),
    SeekMode (AbsoluteSeek, SeekFromEnd),
    hClose,
    hFileSize,
    hFlush,
    hSeek,
    hSetFileSize,
    openFile,
 )

import Progress (ProgressLevel (..), reportProgress)

{- | One line of a journal: what was done, and when. The timestamp is
provenance for whoever reads the file; a replay never looks at it.
-}
data Entry op = Entry
    { jeAt :: Text
    , jeOp :: op
    }
    deriving (Eq, Show)

{- | What the lines of one kind of journal say, and the version of those
words. Each vocabulary is versioned on its own: a database's journal and a
method collection's move apart.
-}
class JournalVocabulary op where
    -- | The version this engine writes for this vocabulary, and the only one it reads.
    vocabularyVersion :: Proxy op -> Int

    -- | The fields of one line besides @v@ and @at@.
    opFields :: op -> [Pair]

    -- | Read those fields back.
    parseOp :: Object -> Parser op

instance (JournalVocabulary op) => ToJSON (Entry op) where
    toJSON entry =
        object $
            ["v" .= vocabularyVersion (Proxy :: Proxy op), "at" .= jeAt entry] <> opFields (jeOp entry)

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
            else Entry <$> o .: "at" <*> parseOp o

-- | A journal, given the directory it lives in.
journalPath :: FilePath -> FilePath
journalPath home = home </> "journal.jsonl"

{- | Record a change. Appends one line and closes the file before returning, so
a change is on disk by the time its caller answers.

Stamps the line with the current time, which is why this takes the operation
rather than a whole entry: when it happened is the journal's business, not its
caller's.

The line is recorded once it is flushed. A close that fails after that is
warned about, not refused: the line is on disk and the next replay reads it,
so refusing would tell the caller a change was not made that was.
-}
appendEntry :: (JournalVocabulary op) => FilePath -> op -> IO (Either Text ())
appendEntry home op = do
    now <- getCurrentTime
    let entry = Entry{jeAt = T.pack (iso8601Show now), jeOp = op}
    written <- try $ do
        createDirectoryIfMissing True home
        bracketOnError (openFile (journalPath home) ReadWriteMode) hClose $ \handle -> do
            dropTornTail handle
            hSeek handle SeekFromEnd 0
            BL.hPut handle (encode entry <> "\n")
            hFlush handle
            pure handle
    case written of
        Right handle -> Right () <$ (try (hClose handle) >>= either warnClose pure)
        Left (err :: SomeException) -> pure (Left ("could not record the edit in " <> T.pack (journalPath home) <> ": " <> T.pack (show err)))
  where
    warnClose :: SomeException -> IO ()
    warnClose err = reportProgress Warning ("recorded the edit in " <> journalPath home <> " but could not close it: " <> show err)

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
