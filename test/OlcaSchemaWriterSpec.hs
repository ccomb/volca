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
import qualified Data.Set as S
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
import OlcaSchema.Parser (Built (..), buildDatabase, shippedScenarios)
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
written sdb = case serializeOlcaPackage defaultUnitConfig M.empty sdb of
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
        , sdbParameters = M.empty
        , sdbProductSystems = []
        , sdbReferenceConvention = SignedReferences
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
            , exchanges = TechnosphereExchange made 1 kg ReferenceProduct Nothing ClaimByProduct "" Nothing Nothing Nothing M.empty noProperties Nothing : lines'
            , activityParameters = M.empty
            , activityNativeType = Nothing
            , activityNativeId = Nothing
            , activityFormulaCheck = Nothing
            , activityDates = noDates
            }

bio :: UUID -> Double -> BioDirection -> Exchange
bio flow amount dir = BiosphereExchange flow amount kg dir "" Nothing Nothing Nothing

warned :: SimpleDatabase -> [T.Text]
warned sdb = either (error . T.unpack) snd (serializeOlcaPackage defaultUnitConfig M.empty sdb)

-- | The elementary amounts of a database, in the order its lines are given.
bioAmounts :: SimpleDatabase -> [Double]
bioAmounts sdb = [exchangeAmount ex | a <- M.elems (sdbActivities sdb), ex <- exchanges a, isBiosphereExchange ex]

-- | The product system documents written with these saved scenarios, by id, and the warnings: a refusal is one, with no document.
systemDocs :: M.Map T.Text [ScenarioValue] -> SimpleDatabase -> (M.Map UUID (KM.KeyMap Value), [T.Text])
systemDocs saved sdb = case serializeOlcaPackage defaultUnitConfig saved sdb of
    Left why -> (M.empty, [why])
    Right (files, warnings) ->
        ( M.fromList [(i, doc) | (path, bytes) <- files, take 16 path == "product_systems/", Just (Object doc) <- [decodeStrict bytes], Just (String t) <- [KM.lookup "@id" doc], Just i <- [UUID.fromText t]]
        , warnings
        )

-- | The written ids a field of a document lists, each under @\@id@ or under one more key.
idsAt :: [KM.Key] -> KM.KeyMap Value -> [UUID]
idsAt path doc = [i | v <- go path (Object doc), String t <- [v], Just i <- [UUID.fromText t]]
  where
    go :: [KM.Key] -> Value -> [Value]
    go keys value = case (keys, value) of
        ([], _) -> [value]
        (_, Array xs) -> concatMap (go keys) (V.toList xs)
        (k : rest, Object o) -> maybe [] (go rest) (KM.lookup k o)
        (_ : _, _) -> []

sid :: Word -> UUID
sid = UUID.fromWords 0 0 7 . fromIntegral

-- | A product system with no set, around the row given.
system :: Word -> T.Text -> (UUID, UUID) -> ProductSystem
system n name (activity, product) = ProductSystem{systemId = sid n, systemName = name, systemReference = Just (ProcessRef activity product), systemTarget = 1, systemSets = []}

refused :: SimpleDatabase -> T.Text -> Expectation
refused sdb because = case serializeOlcaPackage defaultUnitConfig M.empty sdb of
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
        serializeOlcaPackage defaultUnitConfig M.empty twice `shouldBe` serializeOlcaPackage defaultUnitConfig M.empty once

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

    it "keeps every compartment medium through a read" $ do
        let media = [InventoryIndicator, Economic, Waste, Social, Air, Water, Soil, NaturalResource]
            flowOf i m = let u = UUID.fromWords 9 1 0 i in (u, BiosphereFlow u (mediumText m) kg M.empty Nothing Nothing (Just (Compartment m (Just "sub"))))
            flows = zipWith flowOf [1 ..] media
            sdb = (oneProcess [bio u 1 (if m == NaturalResource then Resource else Emission) | ((u, _), m) <- zip flows media]){sdbBioFlows = M.fromList flows}
        back <- throughPackage sdb
        [bfCompartment f | f <- M.elems (sdbBioFlows back)] `shouldBe` [Just (Compartment m (Just "sub")) | m <- media]

    it "converts a line to its flow's unit" $ do
        let g = UUID.fromWords 9 0 0 9
            sdb = (oneProcess [BiosphereExchange co2 1500 g Emission "" Nothing Nothing Nothing]){sdbUnits = M.fromList [(kg, Unit kg "kg" "kg" ""), (g, Unit g "g" "g" "")]}
            grams = either (error . T.unpack) id (buildFromCSV "name,dimension,factor\nkg,mass,1.0\ng,mass,0.001\n")
        case serializeOlcaPackage grams M.empty sdb of
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
        let sdb = oneProcess [BiosphereExchange co2 1 kg Emission "us" Nothing Nothing Nothing, BiosphereExchange ore 1 kg Resource "us" Nothing Nothing Nothing]
        [path | (path, _) <- written sdb, take 10 path == "locations/"] `shouldBe` ["locations/" <> UUID.toString (locationId "us") <> ".json"]
        back <- throughPackage sdb
        [activityLocation a | a <- M.elems (sdbActivities back)] `shouldBe` ["us"]
        T.concat (warned sdb) `shouldSatisfy` T.isInfixOf "US as us"

    describe "product systems" $ do
        let recycling =
                (system 1 "Recycling" (recyclingI, recycledF))
                    { systemTarget = 3
                    , systemSets = [ParameterSet "Base" [Redefinition "leak_method" 2 OfDatabase, Redefinition "cf" 1 (OfImpactCategory (sid 601))], ParameterSet "Empty" []]
                    }
            withSystems systems sdb = sdb{sdbProductSystems = systems}
            setsOf db = [(systemName s, [(parameterSetName set, [(redefinedName r, redefinedValue r) | r <- parameterSetValues set]) | set <- systemSets s]) | s <- sdbProductSystems db]

        it "come back with their id, reference, target and sets, those the engine does not read among them" $ do
            db <- withSystems [recycling] <$> fixture
            back <- throughPackage db
            sdbProductSystems back `shouldBe` [recycling]

        it "list the processes their reference reaches through its suppliers and the treatment of its waste, each line linked to the line's own" $ do
            db <- withSystems [recycling] <$> fixture
            let (docs, _) = systemDocs M.empty db
                doc = M.findWithDefault KM.empty (sid 1) docs
                processDocs = M.fromList [(i, d) | (path, Object d) <- written db, take 10 path == "processes/", Just (String t) <- [KM.lookup "@id" d], Just i <- [UUID.fromText t]]
                lineAt process n = [x | Just (Array xs) <- [KM.lookup "exchanges" =<< M.lookup process processDocs], Object x <- V.toList xs, KM.lookup "internalId" x == Just (Number n)]
            -- Steel sends its slag to its treatment, which buys electricity of the aggregated process.
            S.fromList (idsAt ["processes", "@id"] doc) `shouldBe` S.fromList [recyclingI, elecB, elecB2, steelA, gasP, slagD]
            idsAt ["referenceProcess", "@id"] doc `shouldBe` [recyclingI]
            [KM.lookup "isQuantitativeReference" x | Just (Object r) <- [KM.lookup "referenceExchange" doc], Just (Number n) <- [KM.lookup "internalId" r], x <- lineAt recyclingI n] `shouldBe` [Just (Bool True)]
            let links = [l | Just (Array ls) <- [KM.lookup "processLinks" doc], Object l <- V.toList ls]
            length links `shouldBe` 6
            forM_ links $ \l -> do
                let lines' = [x | Just (Object e) <- [KM.lookup "exchange" l], Just (Number n) <- [KM.lookup "internalId" e], process <- idsAt ["process", "@id"] l, x <- lineAt process n]
                [(idsAt ["flow", "@id"] x, idsAt ["defaultProvider", "@id"] x) | x <- lines'] `shouldBe` [(idsAt ["flow", "@id"] l, idsAt ["provider", "@id"] l)]

        it "keep a waste treatment's target positive, and count the lines linked to no process" $ do
            db <- withSystems [(system 2 "Slag" (slagD, slagF)){systemTarget = 4}] <$> fixture
            back <- throughPackage db
            map systemTarget (sdbProductSystems back) `shouldBe` [4]
            let unlinked ex = case ex of
                    TechnosphereExchange{techRole = Input} -> ex{techActivityLinkId = Nothing}
                    _ -> ex
                unsupplied = M.adjust (\a -> a{exchanges = map unlinked (exchanges a)}) (slagD, slagF) (sdbActivities db)
            T.unlines (snd (systemDocs M.empty db)) `shouldNotSatisfy` T.isInfixOf "linked to no process"
            T.unlines (snd (systemDocs M.empty db{sdbActivities = unsupplied})) `shouldSatisfy` T.isInfixOf "Slag: 1 lines"

        it "give a process's value to every row of its block" $ do
            db <- withSystems [recycling{systemSets = [ParameterSet "Split" [Redefinition "share" 1 (OfProcess cogenF)]]}] <$> fixture
            let shared = sharedActivityUUIDs db
            S.fromList (idsAt ["parameterSets", "parameters", "context", "@id"] (M.findWithDefault KM.empty (sid 1) (fst (systemDocs M.empty db))))
                `shouldBe` S.fromList [ilcdProcessUUID shared (cogenF, heatF), ilcdProcessUUID shared (cogenF, powerF)]

        it "carry a saved scenario in place of the set it is named after, and any other on the first system named once" $ do
            db <- withSystems [recycling, system 2 "Slag" (slagD, slagF)] <$> fixture
            back <-
                throughPackageSaving
                    (M.fromList [("Recycling: Base", [ScenarioValue Nothing "leak_method" 5]), ("Hot", [ScenarioValue (Just formulasJ) "a" 7])])
                    db
            setsOf back `shouldBe` [("Recycling", [("Base", [("leak_method", 5)]), ("Empty", []), ("Hot", [("a", 7)])]), ("Slag", [])]

        it "replace a set only with the saved scenario named after it, any other going where no set has its name" $ do
            db <- withSystems [recycling, system 2 "Slag" (slagD, slagF)] <$> fixture
            back <-
                throughPackageSaving
                    (M.fromList [("Base", [ScenarioValue Nothing "leak_method" 3]), ("Recycling: Hot", [ScenarioValue Nothing "leak_method" 4]), ("Hot", [ScenarioValue Nothing "leak_method" 6])])
                    db
            setsOf back
                `shouldBe` [ ("Recycling", [("Base", [("leak_method", 2), ("cf", 1)]), ("Empty", []), ("Hot", [("leak_method", 4)])])
                           , ("Slag", [("Base", [("leak_method", 3)]), ("Hot", [("leak_method", 6)])])
                           ]

        it "say which saved scenario would read back as none, another system of its name holding a set of its name" $ do
            db <- withSystems [system 4 "Twin" (slagD, slagF), (system 5 "Twin" (recyclingI, recycledF)){systemSets = [ParameterSet "High" [Redefinition "leak_method" 1 OfDatabase]]}] <$> fixture
            T.unlines (snd (systemDocs (M.singleton "Twin: High" [ScenarioValue Nothing "leak_method" 5]) db))
                `shouldSatisfy` T.isInfixOf "1 saved scenarios are written but read back as none"
            T.unlines (snd (systemDocs (M.singleton "Twin: Low" [ScenarioValue Nothing "leak_method" 5]) db))
                `shouldNotSatisfy` T.isInfixOf "read back as none"

        it "write a saved scenario once beside two systems of one name, so it reads back once" $ do
            db <- withSystems [system 4 "Twin" (slagD, slagF), system 5 "Twin" (recyclingI, recycledF), system 2 "Slag" (slagD, slagF)] <$> fixture
            back <- throughPackageSaving (M.fromList [("Hot", [ScenarioValue Nothing "leak_method" 5]), ("Twin: Cold", [ScenarioValue Nothing "leak_method" 0])]) db
            M.keys (fst (shippedScenarios (sdbProductSystems back))) `shouldBe` ["Slag: Hot", "Twin: Cold"]

        it "leave out, and say so, a system whose reference the database does not hold, a value of a process it does not hold, and a saved scenario with no system" $ do
            db <- withSystems [system 6 "Gone" (sid 999, sid 998), recycling{systemSets = [ParameterSet "Base" [Redefinition "a" 1 (OfProcess (sid 997))]]}] <$> fixture
            let (docs, warnings) = systemDocs M.empty db
            M.keys docs `shouldBe` [sid 1]
            T.unlines warnings `shouldSatisfy` T.isInfixOf "1 product systems reference a process the database does not hold"
            T.unlines warnings `shouldSatisfy` T.isInfixOf "Recycling: Base · a"
            T.unlines (snd (systemDocs (M.singleton "Hot" [ScenarioValue Nothing "leak_method" 5]) (withSystems [] db))) `shouldSatisfy` T.isInfixOf "1 saved scenarios are not in the export"

    describe "refuses what it would carry wrong" $ do
        it "a line in a unit that does not convert to its flow's" $ do
            let m = UUID.fromWords 9 0 0 9
                sdb = (oneProcess [BiosphereExchange co2 1 m Emission "" Nothing Nothing Nothing]){sdbUnits = M.fromList [(kg, Unit kg "kg" "kg" ""), (m, Unit m "m" "m" "")]}
            refused sdb "maker · carbon dioxide"
        it "a sub-compartment holding '/'" $
            refused
                (oneProcess [bio co2 1 Emission]){sdbBioFlows = M.singleton co2 (BiosphereFlow co2 "carbon dioxide" kg M.empty Nothing Nothing (Just (Compartment Air (Just "a/b"))))}
                "containing '/'"
        it "a waste input to no treatment of the database" $
            refused
                (oneProcess [WasteExchange co2 1 kg True Nothing ClaimByProduct "" Nothing Nothing Nothing]){sdbBioFlows = M.empty, sdbWasteFlows = M.singleton co2 (WasteFlow co2 "slag" kg M.empty Nothing Nothing)}
                "maker · slag"
        it "an amount that is not finite" $
            refused (oneProcess [bio co2 (1 / 0) Emission]) "not finite"
        it "a coproduct of a process a parent database keeps whole, its shares turned into prices" $ do
            let shared ex = case ex of
                    TechnosphereExchange{} -> ex{techShare = Just (DeclaredShare 50 Nothing), techProperties = noProperties{epPrice = Just (DeducedFromShare 25)}}
                    _ -> ex
                parent = oneProcess [TechnosphereExchange made 2 kg Coproduct Nothing ClaimByProduct "" Nothing Nothing Nothing M.empty noProperties Nothing]
            refused parent{sdbActivities = M.map (\a -> a{exchanges = map shared (exchanges a)}) (sdbActivities parent)} "a parent database keeps whole"
