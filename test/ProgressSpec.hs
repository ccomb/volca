{-# LANGUAGE OverloadedStrings #-}

module ProgressSpec (spec) where

import Progress (
    ProgressLevel (..),
    formatBytes,
    formatDuration,
    getLogLines,
    reportCacheOperation,
    reportError,
    reportMatrixOperation,
    reportProgress,
    reportSolverOperation,
 )
import Test.Hspec

spec :: Spec
spec = do
    describe "formatDuration" $ do
        it "formats sub-millisecond durations with 2 decimal places" $
            formatDuration 0.0005 `shouldBe` "0.50ms"

        it "formats millisecond durations as integer ms" $
            formatDuration 0.5 `shouldBe` "500ms"

        it "formats second durations with 2 decimal places" $
            formatDuration 1.5 `shouldBe` "1.50s"

        it "formats exact 1.0s boundary as seconds" $
            formatDuration 1.0 `shouldBe` "1.00s"

        it "formats durations >= 60s as minutes" $
            formatDuration 90.0 `shouldBe` "1.5min"

        it "formats exactly 60s as 1.0min" $
            formatDuration 60.0 `shouldBe` "1.0min"

    describe "formatBytes" $ do
        it "formats bytes below 1024 as B" $
            formatBytes 512.0 `shouldBe` "512 B"

        it "formats exactly 1024 bytes as KB" $
            formatBytes 1024.0 `shouldBe` "1.0 KB"

        it "formats KB with one decimal place" $
            formatBytes 1536.0 `shouldBe` "1.5 KB"

        it "formats MB with one decimal place" $
            formatBytes (1.5 * 1024 * 1024) `shouldBe` "1.5 MB"

        it "formats GB with two decimal places" $
            formatBytes (1.5 * 1024 * 1024 * 1024) `shouldBe` "1.50 GB"

        it "formats zero bytes" $
            formatBytes 0.0 `shouldBe` "0 B"

    -- -----------------------------------------------------------------------
    -- Log buffer IO functions (read-only; global state is append-only)
    -- -----------------------------------------------------------------------
    describe "getLogLines" $ do
        it "returns a non-negative next index" $ do
            (idx, _) <- getLogLines 0
            idx `shouldSatisfy` (>= 0)

        it "returns empty list when since >= nextIndex" $ do
            (idx, _) <- getLogLines 0
            (_, lines2) <- getLogLines idx
            lines2 `shouldBe` []

    describe "reportProgress" $ do
        it "appends a line to the log buffer (Info level)" $ do
            (before, _) <- getLogLines 0
            reportProgress Info "test-info-message"
            (after, newLines) <- getLogLines before
            after `shouldSatisfy` (> before)
            concatLines newLines `shouldSatisfy` ("test-info-message" `isInfixOf`)

        it "prefixes Error level with [ERROR]" $ do
            (before, _) <- getLogLines 0
            reportError "something failed"
            (_, newLines) <- getLogLines before
            concatLines newLines `shouldSatisfy` ("[ERROR]" `isInfixOf`)

        it "prefixes Cache level with [CACHE]" $ do
            (before, _) <- getLogLines 0
            reportCacheOperation "cache hit"
            (_, newLines) <- getLogLines before
            concatLines newLines `shouldSatisfy` ("[CACHE]" `isInfixOf`)

        it "prefixes Matrix level with [MATRIX]" $ do
            (before, _) <- getLogLines 0
            reportMatrixOperation "building matrix"
            (_, newLines) <- getLogLines before
            concatLines newLines `shouldSatisfy` ("[MATRIX]" `isInfixOf`)

        it "prefixes Solver level with [SOLVER]" $ do
            (before, _) <- getLogLines 0
            reportSolverOperation "solving"
            (_, newLines) <- getLogLines before
            concatLines newLines `shouldSatisfy` ("[SOLVER]" `isInfixOf`)

concatLines :: [String] -> String
concatLines = concatMap (\l -> l ++ "\n")

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (needle `isPrefixOf'`) (tails' haystack)
  where
    isPrefixOf' [] _ = True
    isPrefixOf' _ [] = False
    isPrefixOf' (x : xs) (y : ys) = x == y && isPrefixOf' xs ys
    tails' [] = [[]]
    tails' s@(_ : rest) = s : tails' rest
