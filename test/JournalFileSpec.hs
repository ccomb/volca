{-# LANGUAGE OverloadedStrings #-}

module JournalFileSpec (spec) where

import Data.Aeson ((.:), (.=))
import qualified Data.ByteString.Char8 as BS
import Data.JournalFile (Author (..), Entry (..), JournalVocabulary (..), appendEntry, journalPath, readEntries)
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
            mapM_ (appendEntry home Nothing . Note) ["a", "b"]
            notes home `shouldReturn` Right [Note "a", Note "b"]

    it "drops a torn last line, and the next append starts a line of its own" $
        withSystemTempDirectory "journal" $ \home -> do
            appendEntry home Nothing (Note "a") `shouldReturn` Right ()
            BS.appendFile (journalPath home) "{\"v\":3,\"at\":\"x\",\"op\":\"no"
            notes home `shouldReturn` Right [Note "a"]
            appendEntry home Nothing (Note "b") `shouldReturn` Right ()
            notes home `shouldReturn` Right [Note "a", Note "b"]

    it "refuses a line of another version, even the last one" $
        withSystemTempDirectory "journal" $ \home -> do
            BS.writeFile (journalPath home) "{\"v\":2,\"at\":\"x\",\"op\":\"note\",\"text\":\"a\"}\n"
            result <- notes home
            either T.unpack (const "read") result `shouldContain` "version 2"

    it "keeps who made a change, and reads a line written before authors were kept as made by nobody" $
        withSystemTempDirectory "journal" $ \home -> do
            BS.writeFile (journalPath home) "{\"v\":3,\"at\":\"x\",\"op\":\"note\",\"text\":\"a\"}\n"
            appendEntry home (Just (Author "acc-1" "ana")) (Note "b") `shouldReturn` Right ()
            appendEntry home Nothing (Note "c") `shouldReturn` Right ()
            authors <- fmap (map jeBy) <$> (readEntries home :: IO (Either Text [Entry Note]))
            authors `shouldBe` Right [Nothing, Just (Author "acc-1" "ana"), Nothing]
            written <- BS.lines <$> BS.readFile (journalPath home)
            (written !! 1) `shouldSatisfy` BS.isInfixOf "\"by\":{\"account\":\"acc-1\",\"handle\":\"ana\"}"
            (written !! 2) `shouldNotSatisfy` BS.isInfixOf "\"by\""

    it "reads no journal as no entries" $
        withSystemTempDirectory "journal" $ \home ->
            notes home `shouldReturn` Right []
