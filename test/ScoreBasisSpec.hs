{-# LANGUAGE OverloadedStrings #-}

-- | What a score says it rests on: 'basisOf', the part read from the configurations.
module ScoreBasisSpec (spec) where

import API.Types (BasisDatabase (..), BasisMethods (..))
import Config (DatabaseConfig (..), MethodConfig (..), MethodOrigin (..), MethodPatch (..), MethodPatchMatch (..))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import Database.Manager (basisOf)
import Method.Journal (MethodOp (..), changesScores)
import Method.Types (ScoringSet (..), ScoringSetOrigin (..))
import Test.Hspec
import Types (AllocationProperty (..), ExchangePatch (..), ExchangePatchMatch (..), FileReading (..), GeographyPolicy (..), Licence (..), PatchOp (..), Reading (..), Release (..), SystemModel (..))

database :: Text -> DatabaseConfig
database name =
    DatabaseConfig
        { dcName = name
        , dcDisplayName = name
        , dcPath = ""
        , dcDescription = Nothing
        , dcLoad = True
        , dcDefault = False
        , dcDepends = []
        , dcLocationAliases = M.empty
        , dcFormat = Nothing
        , dcIsUploaded = False
        , dcDeletable = False
        , dcGeographyPolicy = GeoGlobal
        , dcReading = FromFiles ReadDeclared
        , dcPatches = []
        , dcSource = Nothing
        , dcLicence = LicenceUnstated
        , dcRelease = Nothing
        , dcRequires = []
        , dcAdded = Nothing
        }

collection :: Text -> MethodConfig
collection name =
    MethodConfig
        { mcName = name
        , mcOrigin = MethodFromFile "methods.csv"
        , mcActive = True
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

release :: Release
release = Release{releaseName = "Farm data", releaseVersion = "3.2", releaseSystemModel = Nothing}

model :: SystemModel
model = SystemModel{smName = "mass", smVersion = "1", smDescription = Nothing, smDefault = DryMass, smExceptions = [], smSubstitutions = [], smParameters = [], smText = ""}

declared, copy, child, parent :: DatabaseConfig
declared = (database "farm"){dcRelease = Just release}
copy = (database "farm-copy"){dcSource = Just "farm"}
child = (database "farm-child"){dcSource = Just "farm", dcReading = ByModel model}
parent = (database "farm-parent"){dcSource = Just "farm", dcReading = FromFiles ReadWhole}

configs :: M.Map Text DatabaseConfig
configs = M.fromList [(dcName c, c) | c <- [declared, copy, child, parent, database "power"]]

-- | The databases of a basis, read with the farm database's journal recording a change or not.
databasesOf :: Bool -> [Text] -> [BasisDatabase]
databasesOf farmEdited names = fst (basisOf configs (S.fromList ["farm" | farmEdited]) M.empty False names "methods")

methodsOf :: M.Map Text MethodConfig -> Bool -> Text -> BasisMethods
methodsOf methodConfigs edited name = snd (basisOf M.empty S.empty methodConfigs edited [] name)

scoringSet :: ScoringSet
scoringSet =
    ScoringSet
        { ssName = "Single"
        , ssUnit = "Pts"
        , ssVariables = M.empty
        , ssComputed = M.empty
        , ssLabels = M.empty
        , ssNormalization = M.empty
        , ssWeighting = M.empty
        , ssScores = M.empty
        , ssDisplayMultiplier = Nothing
        , ssUnits = M.empty
        , ssOrigin = DeclaredInConfig
        }

spec :: Spec
spec = describe "what a score rests on" $ do
    it "names a copy's release from the database it was copied from, and whose files it reads" $
        map (\d -> (bdName d, bdRelease d, bdReadsFilesOf d)) (databasesOf False ["farm-copy"])
            `shouldBe` [("farm-copy", Just release, Just "farm")]

    it "lists the database asked first, each one once, and leaves out a name nothing configures" $
        map bdName (databasesOf False ["farm-copy", "power", "farm-copy", "gone"]) `shouldBe` ["farm-copy", "power"]

    it "says a child is edited when the parent it was divided from is, and divided otherwise" $
        map (\d -> (bdEdited d, bdReallocated d)) (databasesOf True ["farm-child"]) `shouldBe` [(True, True)]

    it "says a parent read again from the files is not edited, its source's journal left behind" $
        map bdEdited (databasesOf True ["farm-parent"]) `shouldBe` [False]

    it "says a database whose configuration adjusts its amounts is patched" $ do
        let zeroed = ExchangePatch{xpDescription = Nothing, xpMatch = ExchangePatchMatch Nothing Nothing Nothing (Just "Diesel") Nothing, xpOp = SetValueTo 0}
            patched = M.insert "farm" declared{dcPatches = [zeroed]} configs
        map bdPatched (fst (basisOf patched S.empty M.empty False ["farm"] "methods")) `shouldBe` [True]

    it "names the collection at the start of a chain of copies" $ do
        let methods = M.fromList [("ef", collection "ef"), ("ef-mine", (collection "ef-mine"){mcSource = Just "ef"}), ("ef-mine-2", (collection "ef-mine-2"){mcSource = Just "ef-mine"})]
        bmCopyOf (methodsOf methods False "ef-mine-2") `shouldBe` Just "ef"
        bmCopyOf (methodsOf methods False "ef") `shouldBe` Nothing

    it "says a collection is modified by its patches, its unregionalized categories, or its journal" $ do
        let patch = MethodPatch Nothing (MethodPatchMatch Nothing (Just "Ammonia") Nothing Nothing Nothing) (ScaleBy 2)
            methods = M.fromList [("p", (collection "p"){mcPatches = [patch]}), ("g", (collection "g"){mcGlobalMethods = ["Land use"]}), ("plain", collection "plain")]
        map (bmModified . methodsOf methods False) ["p", "g", "plain"] `shouldBe` [True, True, False]
        bmModified (methodsOf methods True "plain") `shouldBe` True

    it "does not count the scoring sets a copy is seeded with as a change of its scores" $ do
        changesScores (CreateScoringSet scoringSet) `shouldBe` False
        changesScores (SetGlobalMethods [] ["Land use"]) `shouldBe` True
