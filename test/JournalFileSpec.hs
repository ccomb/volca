{-# LANGUAGE OverloadedStrings #-}

module JournalFileSpec (spec) where

import Data.Aeson (decodeStrict, eitherDecodeStrict, (.:), (.=))
import qualified Data.ByteString.Char8 as BS
import Data.Either (fromLeft)
import Data.JournalFile (Author (..), Channel (..), Entry (..), JournalVocabulary (..), Origin (..), Source, appendEntry, journalPath, noOrigin, readEntries, sourceCitation, sourceFrom)
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
            mapM_ (appendEntry home noOrigin . Note) ["a", "b"]
            notes home `shouldReturn` Right [Note "a", Note "b"]

    it "drops a torn last line, and the next append starts a line of its own" $
        withSystemTempDirectory "journal" $ \home -> do
            appendEntry home noOrigin (Note "a") `shouldReturn` Right ()
            BS.appendFile (journalPath home) "{\"v\":3,\"at\":\"x\",\"op\":\"no"
            notes home `shouldReturn` Right [Note "a"]
            appendEntry home noOrigin (Note "b") `shouldReturn` Right ()
            notes home `shouldReturn` Right [Note "a", Note "b"]

    it "refuses a line of another version, even the last one" $
        withSystemTempDirectory "journal" $ \home -> do
            BS.writeFile (journalPath home) "{\"v\":2,\"at\":\"x\",\"op\":\"note\",\"text\":\"a\"}\n"
            result <- notes home
            either T.unpack (const "read") result `shouldContain` "version 2"

    it "keeps who made a change, and reads a line written before authors were kept as made by nobody" $
        withSystemTempDirectory "journal" $ \home -> do
            BS.writeFile (journalPath home) "{\"v\":3,\"at\":\"x\",\"op\":\"note\",\"text\":\"a\"}\n"
            appendEntry home (Origin (Just (Author "acc-1" "ana")) Nothing) (Note "b") `shouldReturn` Right ()
            appendEntry home noOrigin (Note "c") `shouldReturn` Right ()
            authors <- fmap (map jeBy) <$> (readEntries home :: IO (Either Text [Entry Note]))
            authors `shouldBe` Right [Nothing, Just (Author "acc-1" "ana"), Nothing]
            written <- BS.lines <$> BS.readFile (journalPath home)
            (written !! 1) `shouldSatisfy` BS.isInfixOf "\"by\":{\"account\":\"acc-1\",\"handle\":\"ana\"}"
            (written !! 2) `shouldNotSatisfy` BS.isInfixOf "\"by\""

    it "keeps what a change came through, and reads a line written before that as saying nothing" $
        withSystemTempDirectory "journal" $ \home -> do
            BS.writeFile (journalPath home) "{\"v\":3,\"at\":\"x\",\"op\":\"note\",\"text\":\"a\"}\n"
            appendEntry home (Origin Nothing (Just ThroughMcp)) (Note "b") `shouldReturn` Right ()
            appendEntry home (Origin Nothing (Just ThroughHttp)) (Note "c") `shouldReturn` Right ()
            channels <- fmap (map jeVia) <$> (readEntries home :: IO (Either Text [Entry Note]))
            channels `shouldBe` Right [Nothing, Just ThroughMcp, Just ThroughHttp]
            written <- BS.lines <$> BS.readFile (journalPath home)
            (written !! 1) `shouldSatisfy` BS.isInfixOf "\"via\":\"mcp\""

    it "refuses a line naming a channel it does not know" $
        withSystemTempDirectory "journal" $ \home -> do
            BS.writeFile (journalPath home) "{\"v\":3,\"at\":\"x\",\"via\":\"sms\",\"op\":\"note\",\"text\":\"a\"}\n"
            result <- notes home
            either T.unpack (const "read") result `shouldContain` "unknown channel: sms"

    describe "a source" $ do
        let refusal :: Either String Source -> String
            refusal = fromLeft "accepted"
        it "needs a document and a page counted from 1" $ do
            sourceFrom "   " Nothing Nothing `shouldSatisfy` either (T.isInfixOf "document") (const False)
            sourceFrom "Camplone 2025" (Just 0) Nothing `shouldSatisfy` either (T.isInfixOf "page") (const False)
        it "reads a blank passage as none, and cites what it has" $ do
            sourceCitation <$> sourceFrom " Camplone 2025 " Nothing (Just "  ") `shouldBe` Right "Camplone 2025"
            sourceCitation <$> sourceFrom "Camplone 2025" (Just 7) (Just "63 kg N/ha") `shouldBe` Right "Camplone 2025, p. 7: \"63 kg N/ha\""
        it "refuses a request's misspelt key and a page written in words" $ do
            refusal (eitherDecodeStrict "{\"document\":\"d\",\"pages\":7}") `shouldContain` "not pages"
            refusal (eitherDecodeStrict "{\"document\":\"d\",\"page\":\"7\"}") `shouldContain` "page"
        it "reads what it is sent" $
            decodeStrict "{\"document\":\"d\",\"page\":7}" `shouldBe` either (const Nothing) Just (sourceFrom "d" (Just 7) Nothing)

    it "reads no journal as no entries" $
        withSystemTempDirectory "journal" $ \home ->
            notes home `shouldReturn` Right []
