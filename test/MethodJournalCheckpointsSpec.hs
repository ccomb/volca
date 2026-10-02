{-# LANGUAGE OverloadedStrings #-}

-- | What a method collection's journal promises, each promise pinned once.
module MethodJournalCheckpointsSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.UUID as UUID
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Hspec

import API.Types (CategoryComparison (..), CategorySide (..), ChangedFactor (..), FactorSide (..), MethodCollectionComparison (..))
import Config (MethodPatch (..), MethodPatchMatch (..), defaultConfig)
import Database.Manager (CachePolicy (..), DatabaseManager, getMethodCollection, initDatabaseManager, loadMethodCollection)
import Database.UploadedDatabase (getMethodUploadsDir)
import Method.Edit
import Method.EditPlan (FactorEdit (..), FactorTarget (..))
import Method.Mapping (MatchStrategy (..), buildMethodTables, contestedFactors)
import Method.Types (Compartment (..), CompartmentMap (..), Method (..), MethodCF (..), MethodCollection (..))
import Service.Compare (Sides (..))
import Service.CompareMethods (CompareMethodsContext (..), Scope (..), compareCollections)
import SynonymDB (buildFromPairs)
import TestHelpers (withScratchDataDir)
import Types (BiosphereFlow (..), PatchOp (..))
import UnitConversion (defaultUnitConfig)

-- | An uploaded columnar method, one category, the rows given.
writeUpload :: [Text] -> IO ()
writeUpload rows = do
    home <- (</> "ecotox") <$> getMethodUploadsDir
    createDirectoryIfMissing True home
    TIO.writeFile (home </> "method.csv") (T.unlines ([";;;;Ecotoxicity", ";;;;Ecotoxicity", ";;;;CTUe", "substance;compartment;cas;unit;"] <> rows))
    TIO.writeFile (home </> "meta.toml") "version = 5\ndisplayName = \"ecotox\"\nformat = \"simapro\"\ndataPath = \".\"\ndepends = []\nallocation = \"declared\"\n"

-- | A fresh manager with the uploaded collection loaded, and its one category.
loadEcotox :: IO (DatabaseManager, Either Text Method)
loadEcotox = do
    manager <- initDatabaseManager defaultConfig NoCache
    loaded <- loadMethodCollection manager "ecotox"
    collection <- getMethodCollection manager "ecotox"
    pure (manager, loaded >> maybe (Left "not loaded") (oneMethod . mcMethods) collection)
  where
    oneMethod :: [Method] -> Either Text Method
    oneMethod [m] = Right m
    oneMethod ms = Left ("expected one category, found " <> T.pack (show (length ms)))

loadedCategory :: IO (DatabaseManager, Method)
loadedCategory = loadEcotox >>= \(manager, found) -> either (fail . T.unpack) (pure . (,) manager) found

named :: Text -> Method -> [MethodCF]
named name = filter ((== name) . mcfFlowName) . methodFactors

-- | The one factor of a category under a name.
theFactor :: Text -> Method -> IO MethodCF
theFactor name method = case named name method of
    [f] -> pure f
    found -> fail ("expected one " <> T.unpack name <> ", found " <> show (length found))

edited :: DatabaseManager -> Text -> FactorEdit -> IO ()
edited manager collection edit = editMethodFactors manager collection edit >>= either (expectationFailure . T.unpack . refusalText) (const (pure ()))

ammoniaRows :: [Text]
ammoniaRows = ["turpentine;soil;;kg;8.399", "turpentine;soil;;kg;1.1619", "Ammonia;air;7664-41-7;kg;3.0"]

byPrefix :: Text -> MethodPatchMatch
byPrefix prefix = MethodPatchMatch Nothing Nothing (Just prefix) Nothing Nothing

-- | A comparison that knows no synonym, no compartment crossing and no location: a copy is compared with its source.
plainContext :: CompareMethodsContext
plainContext = CompareMethodsContext (buildFromPairs []) (CompartmentMap M.empty M.empty) defaultUnitConfig M.empty

-- | The plain indicators collection, a copy of it, and the copy's categories by name.
copied :: IO (DatabaseManager, M.Map Text Method)
copied = do
    manager <- initDatabaseManager defaultConfig NoCache
    copyMethodCollection manager "plain-indicators" "copy" `shouldReturn` Right "copy"
    collection <- getMethodCollection manager "copy"
    pure (manager, M.fromList [(methodName m, m) | m <- maybe [] mcMethods collection])

category :: Text -> M.Map Text Method -> IO Method
category name = maybe (fail ("no category " <> T.unpack name)) pure . M.lookup name

at :: Method -> MethodCF -> FactorTarget
at method factor = FactorTarget (methodId method) (mcfFlowRef factor) Nothing (Just (mcfValue factor))

spec :: Spec
spec = describe "what a method collection's journal promises" $ do
    it "replays to the very collection the changes left" $
        withScratchDataDir $ do
            (manager, categories) <- copied
            methane <- category "Methane" categories
            cadmium <- category "Cadmium" categories
            fossil <- theFactor "Methane, fossil" methane
            biogenic <- theFactor "Methane, biogenic" methane
            edited manager "copy" (SetValue (at methane fossil) 27)
            edited manager "copy" (Remove (at methane biogenic))
            edited manager "copy" (Add (methodId cadmium) fossil{mcfFlowRef = UUID.fromWords 0 0 0 7, mcfFlowName = "Cadmium, ion"})
            edited manager "copy" (Patch (MethodPatch Nothing (byPrefix "Cadmium") (ScaleBy 2)))
            edited manager "copy" (Patch (MethodPatch Nothing (byPrefix "Methane") (SetValueTo 3)))
            _ <- undoMethodEdit manager "copy" Nothing
            _ <- undoMethodEdit manager "copy" Nothing
            inMemory <- getMethodCollection manager "copy"
            fresh <- initDatabaseManager defaultConfig NoCache
            _ <- loadMethodCollection fresh "copy"
            getMethodCollection fresh "copy" `shouldReturn` inMemory

    it "stops loading, naming the line, when the file no longer holds the value a line changed" $
        withScratchDataDir $ do
            writeUpload ammoniaRows
            (manager, ecotox) <- loadedCategory
            ammonia <- theFactor "Ammonia" ecotox
            edited manager "ecotox" (SetValue (at ecotox ammonia) 27)
            writeUpload ["turpentine;soil;;kg;8.399", "turpentine;soil;;kg;1.1619", "Ammonia;air;7664-41-7;kg;4.0"]
            (_, reloaded) <- loadEcotox
            either T.unpack (const "loaded") reloaded `shouldContain` "journal line 1 (set-factor)"

    it "stops loading, naming the line, when a selector reaches another number of factors" $
        withScratchDataDir $ do
            writeUpload ammoniaRows
            (manager, _) <- loadedCategory
            edited manager "ecotox" (Patch (MethodPatch Nothing (byPrefix "Ammonia") (ScaleBy 2)))
            writeUpload (ammoniaRows <> ["Ammonia;water;7664-41-7;kg;1.0"])
            (_, reloaded) <- loadEcotox
            either T.unpack (const "loaded") reloaded `shouldContain` "journal line 1 (scale-factors)"

    it "changes the one of two factors at one place whose value is named" $
        withScratchDataDir $ do
            writeUpload ammoniaRows
            (manager, ecotox) <- loadedCategory
            small <- maybe (fail "no turpentine at 1.1619") pure (lookup 1.1619 [(mcfValue f, f) | f <- named "turpentine" ecotox])
            edited manager "ecotox" (SetValue (at ecotox small) 2)
            (_, after) <- loadEcotox
            fmap (map mcfValue . named "turpentine") after `shouldBe` Right [8.399, 2]

    it "refuses to change one of two identical factors alone, naming it" $
        withScratchDataDir $ do
            writeUpload ["turpentine;soil;;kg;8.399", "turpentine;soil;;kg;8.399"]
            (manager, ecotox) <- loadedCategory
            twin <- case named "turpentine" ecotox of
                f : _ -> pure f
                [] -> fail "no turpentine"
            refused <- editMethodFactors manager "ecotox" (SetValue (at ecotox twin) 2)
            either (T.unpack . refusalText) (const "changed") refused `shouldContain` "turpentine"
            either (T.unpack . refusalText) (const "changed") refused `shouldContain` "identically"

    it "compares a changed copy with its source as exactly the factors changed" $
        withScratchDataDir $ do
            (manager, categories) <- copied
            methane <- category "Methane" categories
            cadmium <- category "Cadmium" categories
            fossil <- theFactor "Methane, fossil" methane
            biogenic <- theFactor "Methane, biogenic" methane
            let ion = fossil{mcfFlowRef = UUID.fromWords 0 0 0 7, mcfFlowName = "Cadmium, ion", mcfCompartment = Just (Compartment "air" "" "")}
            edited manager "copy" (SetValue (at methane fossil) 27)
            edited manager "copy" (Remove (at methane biogenic))
            edited manager "copy" (Add (methodId cadmium) ion)
            source <- getMethodCollection manager "plain-indicators"
            copy <- getMethodCollection manager "copy"
            case compareCollections plainContext [] EveryCategory <$> (Sides <$> source <*> copy) of
                Just (Right comparison) -> do
                    let touched =
                            [ (csdName (ccpBase c), map facFlowName (ccpAdded c), map facFlowName (ccpRemoved c), map (facFlowName . cfxOther) (ccpChanged c))
                            | c <- mccCategories comparison
                            , not (null (ccpAdded c) && null (ccpRemoved c) && null (ccpChanged c))
                            ]
                    touched `shouldBe` [("Methane", [], ["Methane, biogenic"], ["Methane, fossil"]), ("Cadmium", ["Cadmium, ion"], [], [])]
                other -> expectationFailure ("expected a comparison, got " <> show other)

    it "no longer warns of two factors at one place once one of them is removed" $
        withScratchDataDir $ do
            writeUpload ammoniaRows
            (manager, ecotox) <- loadedCategory
            let contested method = length (contestedFactors mempty (buildMethodTables mempty mempty M.empty (matched method)) (matched method))
            contested ecotox `shouldBe` 1
            small <- maybe (fail "no turpentine at 1.1619") pure (lookup 1.1619 [(mcfValue f, f) | f <- named "turpentine" ecotox])
            edited manager "ecotox" (Remove (at ecotox small))
            (_, after) <- loadEcotox
            fmap contested after `shouldBe` Right 0
  where
    -- Every turpentine line matched to one flow, as a database holding it once would.
    matched :: Method -> [(MethodCF, Maybe (BiosphereFlow, MatchStrategy))]
    matched method = [(f, Just (turpentine f, ByUUID)) | f <- named "turpentine" method]

    turpentine :: MethodCF -> BiosphereFlow
    turpentine f = BiosphereFlow (mcfFlowRef f) "turpentine" UUID.nil M.empty Nothing Nothing Nothing
