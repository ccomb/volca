{-# LANGUAGE OverloadedStrings #-}

{- | A database written as an openLCA package: read back, it computes the same
inventories; written again, the same bytes; and what the package cannot carry
refuses the export, naming the line.
-}
module OlcaSchemaWriterSpec (spec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), decodeStrict)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Database (buildDatabaseWithMatrices)
import ILCD.Writer (ilcdProcessUUID, sharedActivityUUIDs)
import Matrix (computeInventoryMatrix)
import OlcaPackageFixture
import OlcaSchema.Package (readPackage)
import OlcaSchema.Parser (Built (..), buildDatabase)
import OlcaSchema.Writer (locationId, serializeOlcaPackage)
import Types
import UnitConversion (buildFromCSV, defaultUnitConfig)

-- | The fixture package as the reader computes it.
fixture :: IO SimpleDatabase
fixture = withSystemTempDirectory "olca-package" $ \dir -> do
    writePackage dir
    pkg <- readPackage dir >>= either (fail . T.unpack) pure
    builtDatabase <$> either (fail . T.unpack) pure (buildDatabase defaultUnitConfig Declared pkg)

matrices :: SimpleDatabase -> IO Database
matrices sdb = buildDatabaseWithMatrices (BuildInputs defaultUnitConfig mempty Declared []) sdb >>= either (fail . T.unpack) pure

inventoryOf :: Database -> (UUID, UUID) -> IO (M.Map UUID Double)
inventoryOf db key = case M.lookup key (dbProcessIdLookup db) of
    Nothing -> fail ("no process " <> show key)
    Just pid -> M.filter (/= 0) <$> (computeInventoryMatrix db pid >>= either (fail . T.unpack) pure)

near :: Double -> Double -> Bool
near a b = abs (a - b) <= 1e-9 * max 1 (max (abs a) (abs b))

written :: SimpleDatabase -> [(FilePath, Value)]
written sdb = case serializeOlcaPackage defaultUnitConfig sdb of
    Left why -> error (T.unpack why)
    Right (files, _) -> [(path, fromMaybe Null (decodeStrict bytes)) | (path, bytes) <- files]

-- | The exchanges of the one process document written.
processExchanges :: SimpleDatabase -> [KM.KeyMap Value]
processExchanges sdb =
    [ line
    | (path, Object doc) <- written sdb
    , take 10 path == "processes/"
    , Just (Array xs) <- [KM.lookup "exchanges" doc]
    , Object line <- V.toList xs
    ]

kg, made, co2, ore, act :: UUID
kg = UUID.fromWords 9 0 0 1
made = UUID.fromWords 9 0 0 2
co2 = UUID.fromWords 9 0 0 3
ore = UUID.fromWords 9 0 0 4
act = UUID.fromWords 9 0 0 5

-- | One process making a product, with the lines given after its reference.
oneProcess :: [Exchange] -> SimpleDatabase
oneProcess lines' =
    SimpleDatabase
        { sdbActivities = M.singleton (act, made) activity
        , sdbTechFlows = M.singleton made (TechnosphereFlow made "product" kg M.empty Nothing Nothing)
        , sdbBioFlows =
            M.fromList
                [ (co2, BiosphereFlow co2 "carbon dioxide" kg M.empty Nothing Nothing (Just (Compartment Air (Just "urban"))))
                , (ore, BiosphereFlow ore "ore" kg M.empty Nothing Nothing (Just (Compartment NaturalResource (Just "in ground"))))
                ]
        , sdbWasteFlows = M.empty
        , sdbUnits = M.singleton kg (Unit kg "kg" "kg" "")
        , sdbDocumentation = noDocumentation
        }
  where
    activity =
        Activity
            { activityName = "maker"
            , activityDescription = []
            , activityDocumentation = []
            , activitySynonyms = M.empty
            , activityClassification = M.empty
            , activityLocation = "US"
            , activityLocationSource = LocationDeclared
            , activityUnit = "kg"
            , exchanges = TechnosphereExchange made 1 kg ReferenceProduct Nothing ClaimByProduct "" Nothing Nothing Nothing M.empty noProperties : lines'
            , activityParams = M.empty
            , activityParamExprs = M.empty
            , activityNativeType = Nothing
            , activityNativeId = Nothing
            , activityFormulaCheck = Nothing
            , activityDates = noDates
            }

bio :: UUID -> Double -> BioDirection -> Exchange
bio flow amount dir = BiosphereExchange flow amount kg dir "" Nothing Nothing

warned :: SimpleDatabase -> [T.Text]
warned sdb = either (error . T.unpack) snd (serializeOlcaPackage defaultUnitConfig sdb)

-- | The elementary amounts of a database, in the order its lines are given.
bioAmounts :: SimpleDatabase -> [Double]
bioAmounts sdb = [exchangeAmount ex | a <- M.elems (sdbActivities sdb), ex <- exchanges a, isBiosphereExchange ex]

refused :: SimpleDatabase -> T.Text -> Expectation
refused sdb because = case serializeOlcaPackage defaultUnitConfig sdb of
    Left why -> T.unpack why `shouldContain` T.unpack because
    Right _ -> expectationFailure "the export was not refused"

spec :: Spec
spec = describe "an openLCA package written from a database" $ do
    it "reads back into the same inventories" $ do
        original <- fixture
        back <- throughPackage original
        before <- matrices original
        after <- matrices back
        let shared = sharedActivityUUIDs original
        forM_ (M.keys (sdbActivities original)) $ \key@(_, output) -> do
            expected <- inventoryOf before key
            got <- inventoryOf after (ilcdProcessUUID shared key, output)
            M.keys got `shouldBe` M.keys expected
            forM_ (M.toList expected) $ \(flow, amount) ->
                (show key, flow, M.findWithDefault 0 flow got) `shouldSatisfy` (\(_, _, g) -> near g amount)

    it "writes the same bytes once read back" $ do
        once <- throughPackage =<< fixture
        twice <- throughPackage once
        serializeOlcaPackage defaultUnitConfig twice `shouldBe` serializeOlcaPackage defaultUnitConfig once

    it "identifies a location as openLCA does, from its code in lower case" $ do
        locationId "us" `shouldBe` read "0b3b97fa-6688-3c56-88ee-4ae80ec0c3c2"
        locationId "US" `shouldBe` locationId "us"

    it "signs an elementary line by its side, as openLCA does" $ do
        let sdb = oneProcess [bio co2 2 Emission, bio co2 (-3) Emission, bio ore 5 Resource, bio ore (-7) Resource]
            sides = [(KM.lookup "isInput" x, KM.lookup "amount" x) | x <- drop 1 (processExchanges sdb)]
        sides
            `shouldBe` [ (Nothing, Just (Number 2))
                       , (Just (Bool True), Just (Number 3))
                       , (Just (Bool True), Just (Number 5))
                       , (Nothing, Just (Number 7))
                       ]
        back <- throughPackage sdb
        bioAmounts back `shouldBe` [2, -3, 5, -7]

    it "writes a line against its compartment on the compartment's side, with the same amount" $ do
        let sdb = oneProcess [bio co2 4 Resource]
        [(KM.lookup "isInput" x, KM.lookup "amount" x) | x <- drop 1 (processExchanges sdb)] `shouldBe` [(Nothing, Just (Number 4))]
        bioAmounts <$> throughPackage sdb `shouldReturn` [4]
        T.concat (warned sdb) `shouldSatisfy` T.isInfixOf "maker · carbon dioxide"

    it "converts a line to its flow's unit" $ do
        let g = UUID.fromWords 9 0 0 9
            sdb = (oneProcess [BiosphereExchange co2 1500 g Emission "" Nothing Nothing]){sdbUnits = M.fromList [(kg, Unit kg "kg" "kg" ""), (g, Unit g "g" "g" "")]}
            grams = either (error . T.unpack) id (buildFromCSV "name,dimension,factor\nkg,mass,1.0\ng,mass,0.001\n")
        case serializeOlcaPackage grams sdb of
            Left why -> expectationFailure (T.unpack why)
            Right (files, _) ->
                [ KM.lookup "amount" line
                | (path, bytes) <- files
                , take 10 path == "processes/"
                , Just (Object doc) <- [decodeStrict bytes]
                , Just (Array xs) <- [KM.lookup "exchanges" doc]
                , Object line <- drop 1 (V.toList xs)
                ]
                    `shouldBe` [Just (Number 1.5)]

    it "writes location codes differing only by case as one, the most used" $ do
        let sdb = oneProcess [BiosphereExchange co2 1 kg Emission "us" Nothing Nothing, BiosphereExchange ore 1 kg Resource "us" Nothing Nothing]
        [path | (path, _) <- written sdb, take 10 path == "locations/"] `shouldBe` ["locations/" <> UUID.toString (locationId "us") <> ".json"]
        back <- throughPackage sdb
        [activityLocation a | a <- M.elems (sdbActivities back)] `shouldBe` ["us"]
        T.concat (warned sdb) `shouldSatisfy` T.isInfixOf "US as us"

    describe "refuses what it would carry wrong" $ do
        it "a line in a unit that does not convert to its flow's" $ do
            let m = UUID.fromWords 9 0 0 9
                sdb = (oneProcess [BiosphereExchange co2 1 m Emission "" Nothing Nothing]){sdbUnits = M.fromList [(kg, Unit kg "kg" "kg" ""), (m, Unit m "m" "m" "")]}
            refused sdb "maker · carbon dioxide"
        it "a sub-compartment holding '/'" $
            refused
                (oneProcess [bio co2 1 Emission]){sdbBioFlows = M.singleton co2 (BiosphereFlow co2 "carbon dioxide" kg M.empty Nothing Nothing (Just (Compartment Air (Just "a/b"))))}
                "containing '/'"
        it "a waste input to no treatment of the database" $
            refused
                (oneProcess [WasteExchange co2 1 kg True Nothing ClaimByProduct "" Nothing Nothing]){sdbBioFlows = M.empty, sdbWasteFlows = M.singleton co2 (WasteFlow co2 "slag" kg M.empty Nothing Nothing)}
                "maker · slag"
        it "an amount that is not finite" $
            refused (oneProcess [bio co2 (1 / 0) Emission]) "not finite"
