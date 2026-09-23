module ReplSpec (spec) where

import CLI.Repl (ServerOwner (..), idleOnExit, replIdleTimeoutSeconds)
import Test.Hspec

spec :: Spec
spec = describe "what a leaving REPL session leaves counting down" $ do
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
