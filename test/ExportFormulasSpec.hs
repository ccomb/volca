{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Parameters and formulas written to SimaPro, openLCA and EcoSpold 2 and
read back: the file the writer gives is the database again, at its defaults
and at other values.
-}
module ExportFormulasSpec (spec) where

import Control.Monad (forM_, (<=<))
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Database (buildDatabaseWithMatrices)
import Database.Loader (defaultLoadOptions, loadDatabase, loadSimaProCSV)
import EcoSpold.Writer2 (noVolatileMeta, writeEcoSpold2WithWarnings)
import OlcaPackageFixture (throughPackage)
import OlcaSchema.Writer (serializeOlcaPackage)
import Parametric (Setting (..), describeParameterRefusal, instantiate)
import SimaPro.Writer (defaultWriterConfig, serializeSimaProCSV, serializeSimaProCSVWithWarnings)
import System.FilePath ((</>))
import System.IO (hClose)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import Test.Hspec
import Test.QuickCheck (Property, choose, counterexample, forAll, ioProperty, withNumTests)
import Types
import UnitConversion (defaultUnitConfig)

spec :: Spec
spec = do
    simaProSpec
    olcaSpec
    ecoSpold2Spec

ecoSpold2Spec :: Spec
ecoSpold2Spec = describe "EcoSpold 2 export of parameters and formulas" $ do
    it "gives the same amounts at other values, each dataset carrying the database's parameters it reads" $
        sameAmountsAtWith throughEcoSpold2 eachProcess (loadBytes bakeryCSV)

    it "gives a process the database formula it reads at its value there, with a name it redefines" $
        sameAmountsAtWith throughEcoSpold2 eachProcess (ownWasteRate <$> loadBytes bakeryCSV)

    it "gives the readers of an avoided product's name its amount, which the dataset writes negated" $ do
        back <- throughEcoSpold2 . avoidedLineRead =<< loadBytes bakeryCSV
        db <- built back
        -- Read back, the avoided flour is the negative input ecoinvent writes, and the emission still 1 + av.
        moved <- either (fail . T.unpack . describeParameterRefusal) (pure . amountsOf db) (instantiate db (eachProcess db 0.7))
        fmap (map (\x -> fromIntegral (round (x * 1e9) :: Integer) / 1e9 :: Double)) (M.lookup ("Bread", "Bread") moved) `shouldBe` Just [-1.4, 1, 1.428571429, 2.4]

    it "refuses a parameter value it cannot write, and says which spread it leaves out" $ do
        source <- loadBytes bakeryCSV
        let withParameter name definition = onBread (\a -> a{activityParameters = M.insert name (Parameter definition Nothing "") (activityParameters a)}) source
        writeEcoSpold2WithWarnings noVolatileMeta (withParameter "k" (InputParameter (1 / 0) Nothing)) `shouldSatisfy` isLeft
        warnings <- either (fail . T.unpack) (pure . snd) (writeEcoSpold2WithWarnings noVolatileMeta (withParameter "zero" (InputParameter 0 (Just (Lognormal 0 1.2)))))
        warnings `shouldSatisfy` any ("1 parameters are written without their spread" `T.isInfixOf`)

    it "writes a copied database formula at the value it takes in the process" $ do
        back <- throughEcoSpold2 . ownWasteRate =<< loadBytes bakeryCSV
        [v | a <- M.elems (sdbActivities back), activityName a == "Bread", Just Parameter{parameterDefinition = CalculatedParameter _ (Just v)} <- [M.lookup "loss" (activityParameters a)]]
            `shouldSatisfy` \vs -> length vs == 1 && all (close 0.6) vs

    it "gives a process the database formula that reads one of its lines by the line's name" $
        sameAmountsAtWith throughEcoSpold2 eachProcess (lossReadsLine <$> loadBytes bakeryCSV)

    it "keeps a line's name rather than making it a parameter" $ do
        source <- loadBytes bakeryCSV
        back <- throughEcoSpold2 source{sdbActivities = M.map nameFlour (sdbActivities source)}
        [(efName f, M.member "flour_kg" (activityParameters a)) | a <- M.elems (sdbActivities back), activityName a == "Bread", Just f <- map exchangeFormula (exchanges a), efName f == Just "flour_kg"]
            `shouldBe` [(Just "flour_kg", False)]

    it "copies into a dataset only the database's parameters it reads" $ do
        back <- throughEcoSpold2 =<< loadBytes bakeryCSV
        sdbParameters back `shouldBe` M.empty
        M.fromList [(activityName a, M.keys (activityParameters a)) | a <- M.elems (sdbActivities back), activityName a /= "Mill"]
            `shouldBe` M.fromList [("Bread", ["flour_per_loaf", "loss", "waste_rate", "water", "water_ratio", "yield"]), ("Grain", [])]

    it "writes each kind of spread so it reads back the same" $ do
        source <- loadBytes bakeryCSV
        let spreads =
                M.fromList
                    [ ("n", Parameter (InputParameter 2 (Just (Normal 2 0.3))) (Just "kg") "")
                    , ("t", Parameter (InputParameter 2 (Just (Triangular 1 2 4))) Nothing "")
                    , ("u", Parameter (InputParameter 1 (Just (Uniform 0.5 1.5))) Nothing "")
                    , ("g", Parameter (InputParameter 1.6 (Just (Lognormal 1.6 1.1))) Nothing "an area")
                    ]
            spreadOf p = case parameterDefinition p of
                InputParameter v u -> (v, u, parameterUnit p, parameterDescription p)
                CalculatedParameter _ v -> (sum v, Nothing, parameterUnit p, parameterDescription p)
            numbers (v, u, _, _) =
                v : case u of
                    Just (Normal m d) -> [m, d]
                    Just (Triangular a b c) -> [a, b, c]
                    Just (Uniform a b) -> [a, b]
                    Just (Lognormal m d) -> [m, d]
                    Nothing -> []
            kind (_, u, unit, about) = (fmap kindOf u, unit, about)
            kindOf :: Uncertainty -> Text
            kindOf = \case
                Normal{} -> "normal"
                Triangular{} -> "triangular"
                Uniform{} -> "uniform"
                Lognormal{} -> "lognormal"
        back <- throughEcoSpold2 (onBread (\a -> a{activityParameters = M.union spreads (activityParameters a)}) source)
        let read' = M.unions [M.map spreadOf (M.restrictKeys (activityParameters a) (M.keysSet spreads)) | a <- M.elems (sdbActivities back), activityName a == "Bread"]
        M.map kind read' `shouldBe` M.map (kind . spreadOf) spreads
        and (M.intersectionWith (\a b -> and (zipWith (\x y -> abs (x - y) <= 1e-12) (numbers a) (numbers b))) read' (M.map spreadOf spreads)) `shouldBe` True

    it "says how many of the database's parameters no process reads" $ do
        source <- loadBytes bakeryCSV
        let unread = source{sdbParameters = M.insert "salt" (Parameter (InputParameter 1 Nothing) Nothing "") (sdbParameters source)}
        warnings <- either (fail . T.unpack) (pure . snd) (writeEcoSpold2WithWarnings noVolatileMeta unread)
        warnings `shouldSatisfy` any ("1 parameters of the database are read by no process" `T.isInfixOf`)

    it "writes a formula EcoSpold 2 cannot read as its value, and says so" $ do
        source <- loadBytes bakeryCSV
        let rounded = source{sdbActivities = M.map (onFlour "round(flour_per_loaf)") (sdbActivities source)}
        warnings <- either (fail . T.unpack) (pure . snd) (writeEcoSpold2WithWarnings noVolatileMeta rounded)
        warnings `shouldSatisfy` any ("Bread · Flour: round(flour_per_loaf)" `T.isInfixOf`)
        back <- throughEcoSpold2 rounded
        [exchangeFormula ex | a <- M.elems (sdbActivities back), activityName a == "Bread", ex@TechnosphereExchange{techRole = Input} <- exchanges a] `shouldBe` [Nothing]
  where
    -- A database-wide value, set on every dataset carrying a copy of it, once per block: a value set on one product of a block is set on all.
    eachProcess :: Database -> Double -> [Setting]
    eachProcess db v = M.elems (M.fromListWith (\_ first' -> first') [(fst key, Setting (Just pid) "yield" v) | (pid, key, a) <- zip3 [0 ..] (V.toList (dbProcessIdTable db)) (V.toList (dbActivities db)), M.member "yield" (activityParameters a)])

simaProSpec :: Spec
simaProSpec = describe "SimaPro export of parameters and formulas" $ do
    it "reads back the same parameters and formulas" $ do
        source <- loadBytes bakeryCSV
        written <- loadBytes =<< written' source
        sdbParameters written `shouldBe` sdbParameters source
        formulasOf written `shouldBe` formulasOf source

    it "writes the same file from what it reads back" $ do
        once <- written' =<< loadBytes bakeryCSV
        twice <- written' =<< loadBytes once
        twice `shouldBe` once

    it "writes a shared line without its share" $ do
        out <- written' =<< loadBytes bakeryCSV
        rowsUnder out "Materials/fuels" `shouldContain` ["Grain;kg;2 * yield;Undefined;0;0;0;"]

    it "writes a share's formula in the allocation cell" $ do
        out <- written' =<< loadBytes bakeryCSV
        rowsUnder out "Products" `shouldContain` ["Flour;kg;1;flour_share;not defined;material;"]

    it "writes a share's formula SimaPro cannot read as its percentage, and says so" $ do
        source <- loadBytes bakeryCSV
        let reserved = source{sdbParameters = M.insert "or" (Parameter (InputParameter 1 Nothing) Nothing "") (sdbParameters source), sdbActivities = M.map orShare (sdbActivities source)}
        out <- written' reserved
        rowsUnder out "Products" `shouldContain` ["Flour;kg;1;60;not defined;material;"]
        warningsOf reserved >>= (`shouldSatisfy` any ("Mill · share of Flour: flour_share * or" `T.isInfixOf`))

    it "says which parameters spread around another value than their own" $ do
        source <- loadBytes bakeryCSV
        let offCentre = source{sdbParameters = M.insert "salt" (Parameter (InputParameter 1 (Just (Normal 2 0.1))) Nothing "") (sdbParameters source)}
        warningsOf offCentre >>= (`shouldSatisfy` any ("the database · salt" `T.isInfixOf`))

    it "writes arguments apart with a comma, as SimaPro reads them" $ do
        out <- written' =<< loadBytes bakeryCSV
        rowsUnder out "Input parameters" `shouldContain` ["water_ratio;0.6;Lognormal;1.21;0;0;No;"]
        rowsUnder out "Calculated parameters" `shouldContain` ["water;max(water_ratio, 0.5) * flour_per_loaf;"]

    it "gives the same amounts at other values" $
        sameAmountsAt (loadBytes <=< written') (loadBytes bakeryCSV)

    it "writes a formula SimaPro reads otherwise as its value, and says so" $ do
        source <- loadBytes bakeryCSV
        let rounded = source{sdbActivities = M.map roundFlour (sdbActivities source)}
        out <- written' rounded
        rowsUnder out "Materials/fuels" `shouldContain` ["Flour;kg;1.25;Undefined;0;0;0;"]
        warningsOf rounded >>= (`shouldSatisfy` any ("Bread · Flour: round(flour_per_loaf)" `T.isInfixOf`))

    it "writes as its value a formula reading a parameter left out through another" $ do
        source <- loadBytes bakeryCSV
        let calculated f = Parameter (CalculatedParameter f Nothing) Nothing ""
            chained = source{sdbParameters = M.union (M.fromList [("p1", calculated "round(zz)"), ("p2", calculated "p1 * 2")]) (sdbParameters source), sdbActivities = M.map (onFlour "p2 * 3") (sdbActivities source)}
        out <- written' chained
        rowsUnder out "Materials/fuels" `shouldContain` ["Flour;kg;1.25;Undefined;0;0;0;"]
        warningsOf chained >>= (`shouldSatisfy` any ("Bread · Flour: p2 * 3" `T.isInfixOf`))

    it "writes a line another formula reads by its name as a parameter of that name" $ do
        source <- loadBytes bakeryCSV
        let named' = source{sdbActivities = M.map nameFlour (sdbActivities source)}
        out <- written' named'
        rowsUnder out "Calculated parameters" `shouldContain` ["flour_kg;flour_per_loaf;"]
        rowsUnder out "Materials/fuels" `shouldContain` ["Flour;kg;flour_kg;Undefined;0;0;0;"]
        rowsUnder out "Emissions to air" `shouldContain` ["Carbon dioxide, fossil;high. pop.;kg;0.5 * flour_kg;Undefined;;;;;;"]
  where
    orShare act = act{exchanges = map orIt (exchanges act)}
    orIt ex = case ex of
        TechnosphereExchange{techShare = Just share@DeclaredShare{dsFormula = Just "flour_share"}} -> ex{techShare = Just share{dsFormula = Just "flour_share * or"}}
        _ -> ex
    roundFlour = onFlour "round(flour_per_loaf)"

olcaSpec :: Spec
olcaSpec = describe "openLCA export of parameters and formulas" $ do
    it "gives the same amounts at other values" $
        sameAmountsAt throughPackage (loadBytes bakeryCSV)

    it "gives a process the database formula it reads with a name it redefines" $
        sameAmountsAt throughPackage (ownWasteRate <$> loadBytes bakeryCSV)

    it "gives a process the database formula that reads one of its lines" $
        sameAmountsAt throughPackage (lossReadsLine <$> loadBytes bakeryCSV)

    it "writes a line's formula times the factor its amount is written with" $ do
        source <- loadBytes bakeryCSV
        files <- packaged source
        [f | (path, doc) <- files, take 10 path == "processes/", f <- amountFormulas doc] `shouldContain` ["flour_per_loaf"]

    it "writes the database's parameters as global ones" $ do
        files <- packaged =<< loadBytes bakeryCSV
        length [() | (path, _) <- files, take 11 path == "parameters/"] `shouldBe` 4

    it "writes a formula openLCA reads otherwise as its value, and says so" $ do
        source <- loadBytes bakeryCSV
        let compared = source{sdbActivities = M.map comparing (sdbActivities source)}
        either (fail . T.unpack) (pure . snd) (serializeOlcaPackage defaultUnitConfig M.empty compared)
            >>= (`shouldSatisfy` any ("Bread · Flour: (yield < 1) * flour_per_loaf" `T.isInfixOf`))
  where
    comparing act = act{exchanges = map compareIt (exchanges act)}
    compareIt ex = case exchangeFormula ex of
        Just ExchangeFormula{efText = "flour_per_loaf"} -> withFormula (Just (ExchangeFormula Nothing "(yield < 1) * flour_per_loaf")) ex
        _ -> ex
    packaged sdb = either (fail . T.unpack) (pure . fst) (serializeOlcaPackage defaultUnitConfig M.empty sdb)
    amountFormulas bytes = case A.decodeStrict bytes of
        Just (A.Object doc) | Just (A.Array xs) <- KM.lookup "exchanges" doc -> [f | A.Object line <- V.toList xs, Just (A.String f) <- [KM.lookup "amountFormula" line]]
        _ -> []

-- | The bread's flour line named, and its emission reading the line by that name.
nameFlour :: Activity -> Activity
nameFlour act = act{exchanges = map nameIt (exchanges act)}
  where
    nameIt :: Exchange -> Exchange
    nameIt ex = case exchangeFormula ex of
        Just ExchangeFormula{efText = "flour_per_loaf"} -> withFormula (Just (ExchangeFormula (Just "flour_kg") "flour_per_loaf")) ex
        Just ExchangeFormula{efText = "0.5 * water + loss"} -> withFormula (Just (ExchangeFormula Nothing "0.5 * flour_kg")) ex
        _ -> ex

-- | The bread's flour line given another formula.
onFlour :: Text -> Activity -> Activity
onFlour formula act = act{exchanges = map flourAs (exchanges act)}
  where
    flourAs :: Exchange -> Exchange
    flourAs ex = case exchangeFormula ex of
        Just ExchangeFormula{efText = "flour_per_loaf"} -> withFormula (Just (ExchangeFormula Nothing formula)) ex
        _ -> ex

-- | The bread redefines the waste rate its database's loss reads, so its emission is 0.5 * 0.75 + (1 - 0.8 * 0.5).
ownWasteRate :: SimpleDatabase -> SimpleDatabase
ownWasteRate = onBread (\a -> a{activityParameters = M.insert "waste_rate" (Parameter (InputParameter 0.5 Nothing) Nothing "") (activityParameters a), exchanges = map (restateLoss 0.975) (exchanges a)})

-- | The database's loss reads the bread's flour line by its name, so the bread's emission is 0.5 * 0.75 + (1 - 1.25 / 2).
lossReadsLine :: SimpleDatabase -> SimpleDatabase
lossReadsLine sdb = (onBread (\a -> a{exchanges = map (nameFlourIn . restateLoss 0.75) (exchanges a)}) sdb){sdbParameters = M.insert "loss" (Parameter (CalculatedParameter "1 - flour_in / 2" Nothing) Nothing "") (sdbParameters sdb)}
  where
    nameFlourIn :: Exchange -> Exchange
    nameFlourIn ex = case exchangeFormula ex of
        Just ExchangeFormula{efText = "flour_per_loaf"} -> withFormula (Just (ExchangeFormula (Just "flour_in") "flour_per_loaf")) ex
        _ -> ex

-- | The bread avoids flour by a line named @av@, which its emission reads, so the emission is 1 + 2 * 0.8.
avoidedLineRead :: SimpleDatabase -> SimpleDatabase
avoidedLineRead = onBread (\a -> a{exchanges = concatMap avoid (exchanges a)})
  where
    avoid :: Exchange -> [Exchange]
    avoid ex = case (ex, exchangeFormula ex) of
        (TechnosphereExchange{}, Just ExchangeFormula{efText = "flour_per_loaf"}) -> [ex, withFormula (Just (ExchangeFormula (Just "av") "2 * yield")) (withAmount 1.6 ex{techRole = AvoidedProduct})]
        (_, Just ExchangeFormula{efText = "0.5 * water + loss"}) -> [withFormula (Just (ExchangeFormula Nothing "1 + av")) (withAmount 2.6 ex)]
        _ -> [ex]

onBread :: (Activity -> Activity) -> SimpleDatabase -> SimpleDatabase
onBread change sdb = sdb{sdbActivities = M.map (\a -> if activityName a == "Bread" then change a else a) (sdbActivities sdb)}

-- | The bread's emission restated at the amount its formula gives once the database is changed.
restateLoss :: Double -> Exchange -> Exchange
restateLoss amount ex = case exchangeFormula ex of
    Just ExchangeFormula{efText = "0.5 * water + loss"} -> withFormula (exchangeFormula ex) (withAmount amount ex)
    _ -> ex

-- | The database written as EcoSpold 2 datasets, and read back.
throughEcoSpold2 :: SimpleDatabase -> IO SimpleDatabase
throughEcoSpold2 sdb = withSystemTempDirectory "es2-formulas" $ \dir -> do
    files <- either (fail . T.unpack) (pure . fst) (writeEcoSpold2WithWarnings noVolatileMeta sdb)
    forM_ files $ \(name, doc) -> BS.writeFile (dir </> name) (TE.encodeUtf8 doc)
    either (fail . T.unpack) pure =<< loadDatabase defaultUnitConfig dir

-- | The database read back holds the source's amounts, and for random values of the yield moves every process as the source does.
sameAmountsAt :: (SimpleDatabase -> IO SimpleDatabase) -> IO SimpleDatabase -> Property
sameAmountsAt readBack = sameAmountsAtWith readBack (\_ v -> [Setting Nothing "yield" v])

-- | As 'sameAmountsAt', the database read back set to the yield as given.
sameAmountsAtWith :: (SimpleDatabase -> IO SimpleDatabase) -> (Database -> Double -> [Setting]) -> IO SimpleDatabase -> Property
sameAmountsAtWith readBack settingsBack loaded = withNumTests 20 $ forAll (choose (0.6, 0.9)) $ \v -> ioProperty $ do
    source <- loaded
    back <- readBack source
    let at settings sdb = do
            db <- built sdb
            either (fail . T.unpack . describeParameterRefusal) (pure . amountsOf db) (instantiate db (settings db v))
        stored sdb = M.fromList [((activityName a, referenceName sdb a), sort (map exchangeAmount (exchanges a))) | a <- M.elems (sdbActivities sdb)]
        same expected got = M.keys got == M.keys expected && and (M.intersectionWith (\a b -> length a == length b && and (zipWith close a b)) got expected)
    expected <- at (\_ value -> [Setting Nothing "yield" value]) source
    got <- at settingsBack back
    pure (counterexample (show ((stored source, stored back), (expected, got))) (not (M.null expected) && same (stored source) (stored back) && same expected got))

close :: Double -> Double -> Bool
close a b = abs (a - b) <= 1e-9 * max 1 (max (abs a) (abs b))

-- | Each activity's parameters and line formulas, by its name and reference product.
formulasOf :: SimpleDatabase -> M.Map (Text, Text) (M.Map Text Parameter, [(Maybe Text, Text)])
formulasOf db = M.fromList [((activityName a, referenceName db a), (activityParameters a, sort [(efName f, efText f) | Just f <- map exchangeFormula (exchanges a)])) | a <- M.elems (sdbActivities db)]

-- | The amounts of each activity a setting moves, by its name and reference product.
amountsOf :: Database -> M.Map ProcessId Activity -> M.Map (Text, Text) [Double]
amountsOf db moved = M.fromList [((activityName a, referenceName (simpleOf db) a), sort (map exchangeAmount (exchanges a))) | a <- M.elems moved]

simpleOf :: Database -> SimpleDatabase
simpleOf db = SimpleDatabase M.empty (dbTechFlows db) M.empty M.empty M.empty noDocumentation M.empty []

referenceName :: SimpleDatabase -> Activity -> Text
referenceName db a = T.intercalate "," [maybe "?" tfName (M.lookup (exchangeFlowId ex) (sdbTechFlows db)) | ex <- exchanges a, exchangeIsReference ex]

written' :: SimpleDatabase -> IO BS.ByteString
written' = either (fail . T.unpack) pure . serializeSimaProCSV defaultWriterConfig

warningsOf :: SimpleDatabase -> IO [T.Text]
warningsOf = either (fail . T.unpack) (pure . snd) . serializeSimaProCSVWithWarnings defaultWriterConfig

built :: SimpleDatabase -> IO Database
built sdb = either (fail . T.unpack) pure =<< buildDatabaseWithMatrices (BuildInputs defaultUnitConfig mempty Declared []) sdb

loadBytes :: BS.ByteString -> IO SimpleDatabase
loadBytes bytes = withSystemTempFile "formulas.csv" $ \path h -> do
    BS.hPut h bytes
    hClose h
    either (fail . T.unpack) pure =<< loadSimaProCSV (defaultLoadOptions defaultUnitConfig) path

-- | The rows of every section of that name, in file order.
rowsUnder :: BS.ByteString -> Text -> [Text]
rowsUnder out header = concatMap (takeWhile (not . T.null) . drop 1) (sectionsAt (T.lines (T.filter (/= '\r') (TE.decodeUtf8 out))))
  where
    sectionsAt ls = case break (== header) ls of
        (_, []) -> []
        (_, rest) -> rest : sectionsAt (drop 1 rest)

{- | A bakery: a database parameter with its spread and one computed from it, a
process with its own parameters reading both, and a mill whose shared line
reads the database parameter.
-}
bakeryCSV :: BS.ByteString
bakeryCSV =
    BS.intercalate
        "\r\n"
        [ "{SimaPro 9.6.0.1}"
        , "{CSV separator: Semicolon}"
        , "{Decimal separator: .}"
        , ""
        , "Process"
        , ""
        , "Category type"
        , "material"
        , ""
        , "Process name"
        , "Bread"
        , ""
        , "Type"
        , "Unit process"
        , ""
        , "Geography"
        , "FR"
        , ""
        , "Products"
        , "Bread;kg;1;100;not defined;material;"
        , ""
        , "Materials/fuels"
        , "Flour;kg;flour_per_loaf;Undefined;0;0;0;"
        , ""
        , "Emissions to air"
        , "Carbon dioxide, fossil;high. pop.;kg;0.5 * water + loss;Undefined;;;;;;"
        , ""
        , "Input parameters"
        , "water_ratio;0.6;Lognormal;1.21;0;0;No;"
        , ""
        , "Calculated parameters"
        , "flour_per_loaf;1 / yield;"
        , "water;max(water_ratio, 0.5) * flour_per_loaf;"
        , ""
        , "End"
        , ""
        , "Process"
        , ""
        , "Category type"
        , "material"
        , ""
        , "Process name"
        , "Mill"
        , ""
        , "Type"
        , "Unit process"
        , ""
        , "Geography"
        , "FR"
        , ""
        , "Products"
        , "Flour;kg;1;flour_share;not defined;material;"
        , "Bran;kg;0.5;100 - flour_share;not defined;material;"
        , ""
        , "Materials/fuels"
        , "Grain;kg;2 * yield;Undefined;0;0;0;"
        , ""
        , "End"
        , ""
        , "Process"
        , ""
        , "Category type"
        , "material"
        , ""
        , "Process name"
        , "Grain"
        , ""
        , "Type"
        , "Unit process"
        , ""
        , "Geography"
        , "FR"
        , ""
        , "Products"
        , "Grain;kg;1;100;not defined;material;"
        , ""
        , "Emissions to air"
        , "Carbon dioxide, fossil;high. pop.;kg;0.1;Undefined;;;;;;"
        , ""
        , "End"
        , ""
        , "Database Input parameters"
        , "yield;0.8;Uniform;0;0.6;0.9;No;share of flour kept"
        , "flour_share;60;Undefined;0;0;0;No;"
        , "waste_rate;1;Undefined;0;0;0;No;"
        , ""
        , "End"
        , ""
        , "Database Calculated parameters"
        , "loss;1 - yield * waste_rate;"
        , ""
        , "End"
        ]
