{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | What reading a method archive comes to, kept beside its extraction.

The files of an extracted archive do not change, yet reading them takes
seconds on a large method: the whole extraction is walked through to find
where the methods are, and its flow definitions are turned into candidate
synonyms. What that reading comes to is kept in one file inside the extraction
directory, and a later start reads that file without opening the directory.

The file holds what the method's files say and nothing else. What a
configuration or a journal adds to a collection is applied afterwards, to a
reading taken from the cache as to a fresh one.
-}
module Method.Cache (
    MethodRead (..),
    BinaryStamp,
    binaryStamp,
    methodCacheFile,
    readMethodCache,
    writeMethodCache,
) where

import qualified Codec.Compression.Zstd as Zstd
import Control.Exception (IOException, try)
import qualified Data.ByteString as BS
import Data.List (isSuffixOf)
import Data.Store (Store, decode, encode)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Directory (doesFileExist, renameFile)
import System.FilePath ((</>))

import Method.Types (MethodCollection)
import SynonymDB.Extract (SynonymCandidate)

-- | What the files of a method come to.
data MethodRead = MethodRead
    { mrCollection :: !MethodCollection
    , mrWarnings :: ![Text]
    {- ^ What the reading found wrong in the files. Said at every start, since
    the cache stands in for the reading: a line said only by the start that
    happened to read the files would be lost to every later one.
    -}
    , mrSynonyms :: Maybe SynonymCandidate
    {- ^ The synonyms its flow definitions offer. Left unevaluated: it takes
    seconds to work out, and only a start that has no candidate set for this
    method yet, or one writing the cache, needs it.
    -}
    }
    deriving (Eq, Show, Generic, Store)

{- | The binary a cache is written by. A cache is read back only by the very
binary that wrote it, so nothing has to be bumped by hand when the reading or
the shape of what is kept changes: each release reads its methods once.
-}
newtype BinaryStamp = BinaryStamp Text
    deriving (Eq, Show, Generic, Store)

{- | Nothing for a binary built outside git or from a modified tree: two such
builds carry the same commit and may read the files differently, so neither
reads a cache nor writes one.
-}
binaryStamp :: String -> String -> Maybe BinaryStamp
binaryStamp version commit
    | null commit || commit == "unknown" || "-dirty" `isSuffixOf` commit = Nothing
    | otherwise = Just (BinaryStamp (T.pack (version <> " " <> commit)))

-- | Where the reading of an archive is kept, given the directory it is extracted to.
methodCacheFile :: FilePath -> FilePath
methodCacheFile extraction = extraction </> "volca.cache.method.bin.zst"

{- | The reading kept in a file, for this binary and this method of an openLCA
package. 'Left' says why there is none to use, for the log; the caller then
reads the method's files and writes over the file.
-}
readMethodCache :: BinaryStamp -> Maybe Text -> FilePath -> IO (Either String MethodRead)
readMethodCache stamp packageMethod file = do
    present <- doesFileExist file
    if not present
        then pure (Left "none written yet")
        else either (\(_ :: IOException) -> Left unreadable) kept <$> try (BS.readFile file)
  where
    kept :: BS.ByteString -> Either String MethodRead
    kept bytes = case decode bytes of
        Right (key, compressed)
            | key /= (stamp, packageMethod) -> Left "the one there is another engine version's, or another method's of the package"
            | Zstd.Decompress raw <- Zstd.decompress compressed -> either (const (Left unreadable)) Right (decode raw)
        _ -> Left unreadable

    unreadable :: String
    unreadable = "the file could not be read"

{- | Keep a reading. Written under another name and renamed: a machine stopped
in the middle of the write leaves no half file under the name that is read
back.
-}
writeMethodCache :: BinaryStamp -> Maybe Text -> FilePath -> MethodRead -> IO ()
writeMethodCache stamp packageMethod file reading = do
    BS.writeFile partial (encode ((stamp, packageMethod), Zstd.compress 1 (encode reading)))
    renameFile partial file
  where
    partial :: FilePath
    partial = file <> ".part"
