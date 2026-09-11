{-# LANGUAGE OverloadedStrings #-}

{- | Regression tests for the auto-relink-on-Load behavior.

The PR these tests guard introduced two related changes:

  1. 'loadDatabaseRawWithCrossDB' returns @(Database, LoadSource)@, which
     says 'FromCache' iff the result came straight from the matrix cache
     (i.e. cross-DB linking was NOT freshly run against 'otherIndexes').
  2. 'loadDatabaseSingleFromConfig' uses that answer to skip the no-op
     self-relink on fresh parses.
  3. A cache is a hit only when the unit table and the location aliases it
     was built with are the ones in force; with either changed, the source
     is read again, since both shape what the cache holds.

The 'LoadSource' answer is the contract these tests pin down. The end-to-end
dep-set-swap scenario (load consumer with deps A → swap to B → reload
consumer → links repointed) is still covered by the PR's manual test plan;
fully automating it requires multi-DB cross-link fixtures and is left to a
follow-up.
-}
module AutoRelinkSpec (spec) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.Directory (
    copyFile,
    createDirectoryIfMissing,
    doesFileExist,
    listDirectory,
 )
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Database.Loader (LoadOptions (..))
import Database.Manager (CachePolicy (..), LoadSource (..), RawLoad (..), loadDatabaseRawWithCrossDB)
import SynonymDB (emptySynonymDB)
import TestHelpers (unitDef)
import Types (AllocationKey (..), GeographyPolicy (..))
import UnitConversion (UnitConfig, defaultUnitConfig, mkUnitConfig, ucDimensionOrder, ucUnits)

{- | Copy regular files from one directory into another (non-recursive,
which is all the EcoSpold v2 fixtures here need).
-}
copyDirContents :: FilePath -> FilePath -> IO ()
copyDirContents src dst = do
    createDirectoryIfMissing True dst
    files <- listDirectory src
    forM_ files $ \f -> copyFile (src </> f) (dst </> f)

{- | Drive 'loadDatabaseRawWithCrossDB' with the inert defaults that this
test cares about. Cross-DB linking is not exercised; we only need the
cache-hit detection.
-}
runRaw :: FilePath -> IO (Either T.Text LoadSource)
runRaw = runRawWith defaultUnitConfig M.empty

-- | The same, under a chosen unit table and location aliases.
runRawWith :: UnitConfig -> M.Map T.Text T.Text -> FilePath -> IO (Either T.Text LoadSource)
runRawWith unitConfig locationAliases dstDir = do
    result <-
        loadDatabaseRawWithCrossDB
            RawLoad
                { rlDbName = "test"
                , rlLoadOptions =
                    LoadOptions
                        { loUnitConfig = unitConfig
                        , loLocationAliases = locationAliases
                        , loAllocation = Declared
                        }
                , rlSourcePath = dstDir
                , rlCachePolicy = UseCache -- cache must be written/read
                , rlSynonymDB = emptySynonymDB
                , rlOtherIndexes = []
                , rlLocationHierarchy = M.empty
                , rlGeographyPolicy = GeoGlobal
                }
    return (fmap snd result)

-- | The default unit table plus one unit, so the table differs in content.
withGram :: UnitConfig
withGram =
    mkUnitConfig
        (ucDimensionOrder defaultUnitConfig)
        (M.insert "g" (unitDef "mass" 0.001) (ucUnits defaultUnitConfig))

spec :: Spec
spec = do
    describe "loadDatabaseRawWithCrossDB cache-hit answer" $ do
        it "answers FromSource on a fresh parse and writes the cache" $
            withSystemTempDirectory "volca-relink" $ \tmp -> do
                let dstDir = tmp </> "sample"
                copyDirContents "test-data/SAMPLE.min1" dstDir

                -- Cache file lives next to sourcePath (in 'tmp', because
                -- takeDirectory dstDir == tmp).
                let cacheFile = tmp </> "volca.cache.test.bin.zst"
                doesFileExist cacheFile `shouldReturn` False

                r1 <- runRaw dstDir
                r1 `shouldBe` Right FromSource

                doesFileExist cacheFile `shouldReturn` True

        it "answers FromCache on a second load against the same path" $
            withSystemTempDirectory "volca-relink" $ \tmp -> do
                let dstDir = tmp </> "sample"
                copyDirContents "test-data/SAMPLE.min1" dstDir

                -- Prime the cache.
                _ <- runRaw dstDir

                -- Second call: must come back from the cache.
                r2 <- runRaw dstDir
                r2 `shouldBe` Right FromCache

        it "reports an unextractable archive rather than an unrecognised format" $
            withSystemTempDirectory "volca-relink" $ \tmp -> do
                -- Binary garbage under a .zip name: no known archive format,
                -- so extraction fails. The old code handed the .zip path back
                -- and the caller reported "No supported database files found",
                -- which names the wrong problem.
                let path = tmp </> "broken.zip"
                BS.writeFile path (BS.pack [0, 1, 2, 3, 4, 5, 6, 7])
                result <- runRaw path
                case result of
                    Left err -> err `shouldSatisfy` T.isInfixOf "Archive could not be extracted"
                    Right _ -> expectationFailure "expected a Left for an unextractable .zip"

        it "reads the source again when the cache was built with another unit table" $
            withSystemTempDirectory "volca-relink" $ \tmp -> do
                let dstDir = tmp </> "sample"
                copyDirContents "test-data/SAMPLE.min1" dstDir

                _ <- runRaw dstDir
                runRawWith withGram M.empty dstDir `shouldReturn` Right FromSource
                -- The rebuilt cache records the new table and is trusted again.
                runRawWith withGram M.empty dstDir `shouldReturn` Right FromCache

        it "reads the source again when the cache was built with other location aliases" $
            withSystemTempDirectory "volca-relink" $ \tmp -> do
                let dstDir = tmp </> "sample"
                copyDirContents "test-data/SAMPLE.min1" dstDir

                _ <- runRaw dstDir
                runRawWith defaultUnitConfig (M.fromList [("CH", "GLO")]) dstDir `shouldReturn` Right FromSource
