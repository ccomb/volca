{-# LANGUAGE OverloadedStrings #-}

module JournalFileSpec (spec) where

import Data.Aeson ((.:), (.=))
import qualified Data.ByteString.Char8 as BS
import Data.JournalFile (Entry (..), JournalVocabulary (..), appendEntry, journalPath, readEntries)
import Data.Text (Text)
import qualified Data.Text as T
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

-- | A vocabulary of one verb, to exercise the file without any domain.
newtype Note = Note Text deriving (Eq, Show)

instance JournalVocabulary Note where
    vocabularyVersion _ = 3
    opFields (Note t) = ["op" .= ("note" :: Text), "text" .= t]
    parseOp o = Note <$> o .: "text"

notes :: FilePath -> IO (Either Text [Note])
notes home = fmap (map jeOp) <$> readEntries home

spec :: Spec
spec = describe "a journal file" $ do
    it "reads back what it appended, in order" $
        withSystemTempDirectory "journal" $ \home -> do
            mapM_ (appendEntry home . Note) ["a", "b"]
            notes home `shouldReturn` Right [Note "a", Note "b"]

    it "drops a torn last line, and the next append starts a line of its own" $
        withSystemTempDirectory "journal" $ \home -> do
            appendEntry home (Note "a") `shouldReturn` Right ()
            BS.appendFile (journalPath home) "{\"v\":3,\"at\":\"x\",\"op\":\"no"
            notes home `shouldReturn` Right [Note "a"]
            appendEntry home (Note "b") `shouldReturn` Right ()
            notes home `shouldReturn` Right [Note "a", Note "b"]

    it "refuses a line of another version, even the last one" $
        withSystemTempDirectory "journal" $ \home -> do
            BS.writeFile (journalPath home) "{\"v\":2,\"at\":\"x\",\"op\":\"note\",\"text\":\"a\"}\n"
            result <- notes home
            either T.unpack (const "read") result `shouldContain` "version 2"

    it "reads no journal as no entries" $
        withSystemTempDirectory "journal" $ \home ->
            notes home `shouldReturn` Right []
