{-# LANGUAGE OverloadedStrings #-}

module UploadSizeSpec (spec) where

import API.DatabaseHandlers (streamToTempFile, uploadBodyCeiling, uploadSizeCap)
import API.Types (UploadChunk (..))
import Config (HostingConfig (..))
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.Word (Word64)
import qualified Servant.Types.SourceT as S
import System.Directory (removeFile)
import Test.Hspec

-- | A hosting config with the given upload-size limit (in MB); other fields are irrelevant here.
hostingWithLimit :: Int -> HostingConfig
hostingWithLimit limitMb =
    HostingConfig
        { hcMaxUploads = -1
        , hcMaxUploadMb = limitMb
        , hcMaxLoadedUploads = -1
        , hcApiAccess = True
        , hcReadOnly = False
        , hcReadOnlyMessage = ""
        , hcUpgradeUpload = ""
        , hcUpgradeApi = ""
        , hcUpgradeVmSize = ""
        , hcMaxBatchActivities = Nothing
        , hcMaxTopFlows = Nothing
        }

mb :: Int -> Int
mb n = n * 1024 * 1024

spec :: Spec
spec = do
    describe "uploadSizeCap" $ do
        it "is unlimited with no hosting config (local / CLI / desktop)" $
            uploadSizeCap Nothing `shouldBe` Right Nothing

        it "is unlimited when the configured limit is negative" $
            uploadSizeCap (Just (hostingWithLimit (-1))) `shouldBe` Right Nothing

        it "disables uploads entirely when the limit is zero" $
            uploadSizeCap (Just (hostingWithLimit 0))
                `shouldBe` Left "Uploads are disabled on this plan."

        it "caps the streamed body at the policy limit in bytes" $
            uploadSizeCap (Just (hostingWithLimit 100)) `shouldBe` Right (Just (mb 100))

    describe "uploadBodyCeiling" $ do
        let dbUpload = ["api", "v1", "db", "upload"]
            methodUpload = ["api", "v1", "method-collections", "upload"]

        it "does not bound non-upload paths" $
            uploadBodyCeiling (Just (hostingWithLimit 100)) ["api", "v1", "search"] `shouldBe` Nothing

        it "does not bound the reference-data CSV upload routes (out of policy)" $
            uploadBodyCeiling (Just (hostingWithLimit 100)) ["api", "v1", "units", "upload"] `shouldBe` Nothing

        it "does not bound uploads with no hosting config (local / CLI)" $
            uploadBodyCeiling Nothing dbUpload `shouldBe` Nothing

        it "does not bound uploads on the unlimited tier" $
            uploadBodyCeiling (Just (hostingWithLimit (-1))) dbUpload `shouldBe` Nothing

        it "does not bound at the HTTP layer when uploads are disabled (handler rejects)" $
            uploadBodyCeiling (Just (hostingWithLimit 0)) dbUpload `shouldBe` Nothing

        it "caps the db upload route at the policy limit + 1 MiB slack (raw octet-stream)" $
            uploadBodyCeiling (Just (hostingWithLimit 100)) dbUpload
                `shouldBe` Just (101 * 1024 * 1024 :: Word64)

        it "caps the method upload route the same way" $
            uploadBodyCeiling (Just (hostingWithLimit 50)) methodUpload
                `shouldBe` Just (51 * 1024 * 1024 :: Word64)

    describe "streamToTempFile" $ do
        let chunks = map (UploadChunk . BS.pack) [[1, 2, 3], [4, 5], [6, 7, 8, 9]] -- 9 bytes total
            source = S.source chunks

        it "streams all chunks to a temp file when under the cap" $ do
            result <- streamToTempFile (Just 100) source
            case result of
                Left err -> expectationFailure ("unexpected rejection: " <> show err)
                Right path -> do
                    written <- BS.readFile path
                    written `shouldBe` BS.pack [1, 2, 3, 4, 5, 6, 7, 8, 9]
                    removeFile path

        it "streams with no cap (unlimited tier)" $ do
            result <- streamToTempFile Nothing source
            case result of
                Left err -> expectationFailure ("unexpected rejection: " <> show err)
                Right path -> BS.readFile path >>= (`shouldBe` BS.pack [1 .. 9]) >> removeFile path

        it "rejects and cleans up when the running size exceeds the cap" $ do
            result <- streamToTempFile (Just 4) source -- 9 bytes > 4
            result `shouldSatisfy` isLeft
