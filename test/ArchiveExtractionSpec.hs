{-# LANGUAGE OverloadedStrings #-}

module ArchiveExtractionSpec (spec) where

import qualified Data.Text as T
import System.Info (os)
import System.Process (CreateProcess, proc)
import Test.Hspec

import Database.Upload (InstallAdvice (..), firstExtraction)

spec :: Spec
spec = describe "firstExtraction" $ do
    -- The engine said "Please install an extraction tool" when unzip ran and
    -- stopped on a full disk, and the reader went looking for a tool they had.
    it "says what a tool that ran wrote, not to install one" $ unlessWindows $ do
        result <- firstExtraction "ZIP" advice [missing, script "echo 'write error (disk full?)' >&2; exit 50", script "echo 'the same again' >&2; exit 50"]
        case result of
            Right () -> expectationFailure "a failing tool was taken for a success"
            Left why -> do
                T.unpack why `shouldContain` "write error (disk full?)"
                T.unpack why `shouldContain` "code 50"
                T.unpack why `shouldNotContain` "Please install"
                T.unpack why `shouldNotContain` "the same again"

    -- A directory is there but cannot be run, as a bundled tool that lost its
    -- execute permission: installing another tool is not the cure.
    it "says why a tool that is there could not be run, not to install one" $ unlessWindows $ do
        result <- firstExtraction "ZIP" advice [proc "/" []]
        case result of
            Right () -> expectationFailure "a tool that cannot run was taken for a success"
            Left why -> do
                T.unpack why `shouldContain` "could not be run"
                T.unpack why `shouldNotContain` "Please install"

    it "advises installing a tool only when none is there" $ do
        result <- firstExtraction "ZIP" advice [missing, proc "volca-no-such-extraction-tool-either" []]
        case result of
            Right () -> expectationFailure "no tool could run, yet extraction succeeded"
            Left why -> T.unpack why `shouldContain` "Please install unzip"

    it "takes the next tool when one fails" $
        unlessWindows $
            firstExtraction "ZIP" advice [script "exit 9", script "exit 0"] `shouldReturn` Right ()
  where
    advice :: InstallAdvice
    advice = InstallAdvice "Please install unzip"
    missing :: CreateProcess
    missing = proc "volca-no-such-extraction-tool" []
    script :: String -> CreateProcess
    script line = proc "sh" ["-c", line]
    unlessWindows :: Expectation -> Expectation
    unlessWindows act
        | os == "mingw32" = pendingWith "needs a POSIX shell"
        | otherwise = act
