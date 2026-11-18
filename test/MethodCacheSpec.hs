{-# LANGUAGE OverloadedStrings #-}

{- | What reading a method archive comes to is kept beside its extraction, and
a later start takes it from there. These lock what makes that safe: only the
binary that wrote a cache reads it, and a run told to keep no cache keeps none.
-}
module MethodCacheSpec (spec) where

import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.Maybe (isNothing)
import System.Directory (copyFile, createDirectory, doesFileExist, removeFile)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Config (MethodConfig (..), MethodOrigin (..))
import Database.Manager (CachePolicy (..), loadMethodCollectionFromConfig, loadMethodCollectionFromPath)
import Method.Cache (BinaryStamp, MethodRead (..), binaryStamp, methodCacheFile, readMethodCache, writeMethodCache)
import Method.Types (MethodCollection (MethodCollection, mcMethods))
import SynonymDB.Extract (synonymCandidate)

spec :: Spec
spec = do
    describe "binaryStamp" $ do
        it "names a binary built from a commit" $
            binaryStamp "1.2.3" "abc1234" `shouldSatisfy` (/= binaryStamp "1.2.3" "def5678")
        -- Two builds of a modified tree carry the same commit and may read
        -- the files differently.
        it "gives none to a binary built from a modified tree, or outside git" $ do
            binaryStamp "1.2.3" "abc1234-dirty" `shouldSatisfy` isNothing
            binaryStamp "1.2.3" "unknown" `shouldSatisfy` isNothing

    describe "a reading written to a file" $ do
        it "comes back whole, with its warnings and its candidate synonyms" $
            withStamps $ \this _ dir -> do
                let file = dir </> "cache"
                writeMethodCache this (Just "EF 3.1") file reading
                readMethodCache this (Just "EF 3.1") file `shouldReturn` Right reading

        it "is refused to another binary, and to another method of the package" $
            withStamps $ \this other dir -> do
                let file = dir </> "cache"
                writeMethodCache this (Just "EF 3.1") file reading
                readMethodCache other (Just "EF 3.1") file >>= (`shouldSatisfy` isLeft)
                readMethodCache this (Just "ReCiPe") file >>= (`shouldSatisfy` isLeft)
                readMethodCache this Nothing file >>= (`shouldSatisfy` isLeft)

        it "is refused when the file is not one, or is not there" $
            withStamps $ \this _ dir -> do
                BS.writeFile (dir </> "cache") "not a cache"
                readMethodCache this Nothing (dir </> "cache") >>= (`shouldSatisfy` isLeft)
                readMethodCache this Nothing (dir </> "absent") >>= (`shouldSatisfy` isLeft)

    describe "loading a method archive" $ do
        -- The files are taken away after the first load: only the cache can
        -- answer the second, and a load that does not read it finds no method.
        it "reads the files once, then answers from the cache without them" $
            withExtractedArchive $ \archive extraction ->
                withStamps $ \this other _ -> do
                    first <- loadMethodCollectionFromPath (Just this) Nothing archive
                    fmap (null . mcMethods . mrCollection) first `shouldBe` Right False
                    removeFile (extraction </> "method.csv")
                    loadMethodCollectionFromPath (Just this) Nothing archive `shouldReturn` first
                    loadMethodCollectionFromPath (Just other) Nothing archive >>= (`shouldSatisfy` isLeft)
                    loadMethodCollectionFromPath Nothing Nothing archive >>= (`shouldSatisfy` isLeft)

        it "keeps no cache for a binary without a stamp, nor for a run told to keep none" $
            withExtractedArchive $ \archive extraction -> do
                _ <- loadMethodCollectionFromPath Nothing Nothing archive
                _ <- loadMethodCollectionFromConfig NoCache (configOf archive)
                doesFileExist (methodCacheFile extraction Nothing) `shouldReturn` False

        -- With one file between them, each start would have each of two
        -- collections overwrite the other's reading and never find its own.
        it "keeps the methods of one package in files of their own" $
            methodCacheFile "package.zip.d" (Just "EF 3.1") `shouldNotBe` methodCacheFile "package.zip.d" (Just "ReCiPe 2016")
  where
    reading :: MethodRead
    reading =
        MethodRead
            { mrCollection = MethodCollection [] [] []
            , mrWarnings = ["a file left out"]
            , mrSynonyms = synonymCandidate [("alpha", "beta")]
            }

    withStamps :: (BinaryStamp -> BinaryStamp -> FilePath -> IO ()) -> IO ()
    withStamps action = case (binaryStamp "1.2.3" "abc1234", binaryStamp "1.2.3" "def5678") of
        (Just this, Just other) -> withSystemTempDirectory "volca-method-cache" (action this other)
        _ -> expectationFailure "a commit gave no stamp"

    -- An archive already extracted: the load takes the directory beside it
    -- for its content, as it does at every start after the first.
    withExtractedArchive :: (FilePath -> FilePath -> IO ()) -> IO ()
    withExtractedArchive action =
        withSystemTempDirectory "volca-method-archive" $ \dir -> do
            let archive = dir </> "method.zip"
                extraction = archive <> ".d"
            BS.writeFile archive "stands for the archive"
            createDirectory extraction
            copyFile "test/data/simapro_method.csv" (extraction </> "method.csv")
            action archive extraction

    configOf :: FilePath -> MethodConfig
    configOf path =
        MethodConfig
            { mcName = "cached"
            , mcOrigin = MethodFromFile path
            , mcActive = False
            , mcDefault = False
            , mcHome = Nothing
            , mcAdded = Nothing
            , mcSource = Nothing
            , mcDescription = Nothing
            , mcFormat = Nothing
            , mcScoringSets = []
            , mcGlobalMethods = []
            , mcPatches = []
            , mcPackageMethod = Nothing
            }
