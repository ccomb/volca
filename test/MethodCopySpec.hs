{-# LANGUAGE OverloadedStrings #-}

module MethodCopySpec (spec) where

import Control.Monad (void)
import Data.List (find)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Config (Config (..), MethodConfig (..), MethodOrigin (..), MethodPatch (..), MethodPatchMatch (..), ScoringSetConfig (..), defaultConfig)
import Data.JournalFile (Entry (..), readEntries)
import Database.Manager (CachePolicy (..), getMethodCollection, initDatabaseManager, loadMethodCollection, removeMethodCollection, unloadMethodCollection)
import Database.UploadedDatabase (getMethodUploadsDir)
import Method.Edit (MethodEditRefusal (..), copyMethodCollection, editMethodCategories, undoMethodEdit)
import Method.EditPlan (CategoryEdit (..))
import Method.Journal (MethodLine (..), opName)
import Method.Types (MethodCollection, ScoringSet (..), ScoringSetOrigin (..))
import qualified Method.Types
import TestHelpers (withScratchDataDir)
import Types (PatchOp (..))

-- | A columnar method with one category and one flow written twice at one place.
methodCsv :: T.Text
methodCsv =
    T.unlines
        [ ";;;;Ecotoxicity"
        , ";;;;Ecotoxicity"
        , ";;;;CTUe"
        , "substance;compartment;cas;unit;"
        , "turpentine;soil;;kg;8.399"
        , "turpentine;soil;;kg;1.1619"
        , "Ammonia;air;7664-41-7;kg;3.0"
        ]

-- | An uploaded collection's home, as an upload leaves it.
writeUpload :: FilePath -> IO ()
writeUpload home = do
    createDirectoryIfMissing True home
    TIO.writeFile (home </> "method.csv") methodCsv
    TIO.writeFile (home </> "meta.toml") "version = 5\ndisplayName = \"ecotox\"\nformat = \"simapro\"\ndataPath = \".\"\ndepends = []\nallocation = \"declared\"\n"

-- | A collection the configuration declares, adding a set, two patches and an unregionalized category.
configured :: FilePath -> MethodConfig
configured dir =
    MethodConfig
        { mcName = "ecotox"
        , mcOrigin = MethodFromFile (dir </> "ecotox.csv")
        , mcActive = True
        , mcHome = Nothing
        , mcSource = Nothing
        , mcDescription = Nothing
        , mcFormat = Nothing
        , mcScoringSets = [ScoringSetConfig "Single" "Pt" (M.singleton "eco" "Ecotoxicity") M.empty M.empty M.empty (M.singleton "eco" 1) M.empty Nothing]
        , mcGlobalMethods = ["Ecotoxicity"]
        , mcPatches = [selector "Ammonia" (ScaleBy 2), selector "Neon" (SetValueTo 0)]
        }
  where
    selector :: T.Text -> PatchOp -> MethodPatch
    selector flow = MethodPatch Nothing (MethodPatchMatch Nothing (Just flow) Nothing Nothing Nothing)

-- | A copy's sets come from its journal, which is their only difference.
withoutOrigins :: MethodCollection -> MethodCollection
withoutOrigins c = c{Method.Types.mcScoringSets = map (\s -> s{ssOrigin = DeclaredInConfig}) (Method.Types.mcScoringSets c)}

spec :: Spec
spec = describe "copying a method collection" $ do
    it "copies the built-in collection, which then scores like its source" $
        withScratchDataDir $ do
            manager <- initDatabaseManager defaultConfig NoCache
            copyMethodCollection manager "plain-indicators" "My indicators" `shouldReturn` Right "my-indicators"
            source <- getMethodCollection manager "plain-indicators"
            copy <- getMethodCollection manager "my-indicators"
            copy `shouldBe` source

    it "starts a copy of a configured collection from what its configuration adds" $
        withScratchDataDir $
            withSystemTempDirectory "method" $ \dir -> do
                TIO.writeFile (dir </> "ecotox.csv") methodCsv
                manager <- initDatabaseManager defaultConfig{cfgMethods = [configured dir]} NoCache
                copyMethodCollection manager "ecotox" "ecotox-copy" `shouldReturn` Right "ecotox-copy"
                source <- getMethodCollection manager "ecotox"
                copy <- getMethodCollection manager "ecotox-copy"
                fmap withoutOrigins copy `shouldBe` fmap withoutOrigins source
                home <- (</> "ecotox-copy") <$> getMethodUploadsDir
                fmap (map (opName . mlOp . jeOp)) <$> readEntries home
                    `shouldReturn` Right ["create-scoring-set", "scale-factors", "set-factors", "set-global-methods"]

    it "refuses a name another collection already slugs to, writing nothing" $
        withScratchDataDir $ do
            manager <- initDatabaseManager defaultConfig NoCache
            _ <- copyMethodCollection manager "plain-indicators" "copy one"
            refused <- copyMethodCollection manager "plain-indicators" "Copy One"
            void refused `shouldBe` Left (NameTaken "copy-one")

    it "refuses to copy a collection that does not exist" $
        withScratchDataDir $ do
            manager <- initDatabaseManager defaultConfig NoCache
            copyMethodCollection manager "nothing" "x" `shouldReturn` Left (CollectionNotFound "nothing")

    it "deletes a copy of the built-in collection" $
        withScratchDataDir $ do
            manager <- initDatabaseManager defaultConfig NoCache
            _ <- copyMethodCollection manager "plain-indicators" "mine"
            _ <- unloadMethodCollection manager "mine"
            removeMethodCollection manager "mine" `shouldReturn` Right ()

    it "deletes a copy without touching its source, and refuses to delete a source a copy reads" $
        withScratchDataDir $ do
            home <- (</> "ecotox") <$> getMethodUploadsDir
            writeUpload home
            manager <- initDatabaseManager defaultConfig NoCache
            _ <- loadMethodCollection manager "ecotox"
            copyMethodCollection manager "ecotox" "ecotox-copy" `shouldReturn` Right "ecotox-copy"
            refused <- removeMethodCollection manager "ecotox"
            either T.unpack (const "deleted") refused `shouldContain` "ecotox-copy"
            _ <- unloadMethodCollection manager "ecotox-copy"
            removeMethodCollection manager "ecotox-copy" `shouldReturn` Right ()
            doesFileExist (home </> "method.csv") `shouldReturn` True

    it "deletes a copy a copy was made from, and keeps the files both read" $
        withScratchDataDir $ do
            home <- (</> "ecotox") <$> getMethodUploadsDir
            writeUpload home
            manager <- initDatabaseManager defaultConfig NoCache
            _ <- loadMethodCollection manager "ecotox"
            copyMethodCollection manager "ecotox" "first" `shouldReturn` Right "first"
            copyMethodCollection manager "first" "second" `shouldReturn` Right "second"
            mapM_ (unloadMethodCollection manager) ["first", "second"]
            removeMethodCollection manager "first" `shouldReturn` Right ()
            refused <- removeMethodCollection manager "ecotox"
            either T.unpack (const "deleted") refused `shouldContain` "second"
            loadMethodCollection manager "second" `shouldReturn` Right ()

    it "renames a category a scoring set weighs, the set following, and gives the name back on undo" $
        withScratchDataDir $
            withSystemTempDirectory "method" $ \dir -> do
                TIO.writeFile (dir </> "ecotox.csv") methodCsv
                manager <- initDatabaseManager defaultConfig{cfgMethods = [configured dir]} NoCache
                _ <- copyMethodCollection manager "ecotox" "mine"
                collection <- getMethodCollection manager "mine"
                category <-
                    maybe (fail "no Ecotoxicity category") (pure . Method.Types.methodId) $
                        find ((== "Ecotoxicity") . Method.Types.methodName) (maybe [] Method.Types.mcMethods collection)
                _ <- editMethodCategories manager "mine" (Rename category "Ecotoxicity, total")
                renamed <- getMethodCollection manager "mine"
                fmap (concatMap (M.elems . ssVariables) . Method.Types.mcScoringSets) renamed `shouldBe` Just ["Ecotoxicity, total"]
                fmap Method.Types.mcUnregionalized renamed `shouldBe` Just ["Ecotoxicity, total"]
                _ <- undoMethodEdit manager "mine" Nothing
                getMethodCollection manager "mine" `shouldReturn` collection
