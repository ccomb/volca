{-# LANGUAGE OverloadedStrings #-}

module ArchiveExtractionSpec (spec) where

import qualified Data.Text as T
import System.Info (os)
import Test.Hspec

import Database.Upload (firstExtraction)

spec :: Spec
spec = describe "firstExtraction" $ do
    -- The engine said "Please install an extraction tool" when unzip ran and
    -- stopped on a full disk, and the reader went looking for a tool they had.
    it "says what a tool that ran wrote, not to install one" $ unlessWindows $ do
        result <- firstExtraction "ZIP" "Please install unzip" [(missing, []), shell "echo 'write error (disk full?)' >&2; exit 50"]
        case result of
            Right () -> expectationFailure "a failing tool was taken for a success"
            Left why -> do
                T.unpack why `shouldContain` "write error (disk full?)"
                T.unpack why `shouldContain` "code 50"
                T.unpack why `shouldNotContain` "Please install"

    it "advises installing a tool only when none could start" $ do
        result <- firstExtraction "ZIP" "Please install unzip" [(missing, []), (missing ++ "-too", [])]
        case result of
            Right () -> expectationFailure "no tool could run, yet extraction succeeded"
            Left why -> T.unpack why `shouldContain` "Please install unzip"

    it "takes the next tool when one fails" $
        unlessWindows $
            firstExtraction "ZIP" "Please install unzip" [shell "exit 9", shell "exit 0"] `shouldReturn` Right ()
  where
    missing = "volca-no-such-extraction-tool"
    shell script = ("sh", ["-c", script])
    unlessWindows act
        | os == "mingw32" = pendingWith "needs a POSIX shell"
        | otherwise = act
