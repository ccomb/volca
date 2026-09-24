module ReplSpec (spec) where

import CLI.Repl (ServerOwner (..), idleOnExit, replArgs, replIdleTimeoutSeconds)
import Test.Hspec

spec :: Spec
spec = do
    describe "what a leaving REPL session leaves counting down" $ do
        it "ends a server it started itself, after the grace period" $
            idleOnExit StartedByThisRepl Nothing `shouldBe` Just replIdleTimeoutSeconds

        it "still ends one it started, whatever countdown it held off" $
            idleOnExit StartedByThisRepl (Just 600) `shouldBe` Just replIdleTimeoutSeconds

        it "leaves a server started by hand standing" $
            -- The one this is all for: nothing was counting down before the
            -- session, so nothing counts down after it, and the server the user
            -- started in another terminal survives the REPL being closed.
            idleOnExit FoundRunning Nothing `shouldBe` Nothing

        it "puts back the countdown it found, rather than its own" $
            -- A server left counting down by an earlier REPL keeps going, so it
            -- does not outlive everyone; and a server started by hand with
            -- --idle-timeout keeps the timeout its user asked for, not ten seconds.
            idleOnExit FoundRunning (Just 600) `shouldBe` Just 600

    describe "reading a line into arguments" $ do
        it "keeps a quoted name as one argument" $
            replArgs "activities --name \"tomato juice\"" `shouldBe` Right ["activities", "--name", "tomato juice"]

        it "reads single quotes the same way, and the other quote inside literally" $
            replArgs "activities --name 'the \"best\" juice'" `shouldBe` Right ["activities", "--name", "the \"best\" juice"]

        it "joins a quoted part to the word it touches" $
            replArgs "activities --name=\"tomato juice\"" `shouldBe` Right ["activities", "--name=tomato juice"]

        it "passes an empty quoted argument" $
            replArgs "activities --name \"\"" `shouldBe` Right ["activities", "--name", ""]

        it "leaves a backslash alone, as a Windows path needs" $
            replArgs "export C:\\data\\out.csv" `shouldBe` Right ["export", "C:\\data\\out.csv"]

        it "splits on any run of blanks, as before" $
            replArgs "  flow   abc \t activities " `shouldBe` Right ["flow", "abc", "activities"]

        it "refuses a quote left open" $
            replArgs "activities --name \"tomato" `shouldBe` Left "Unterminated \" quote."

