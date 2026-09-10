{-# LANGUAGE OverloadedStrings #-}

module UnitConversionSpec (spec) where

import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import UnitConversion

-- Helper for testing Left results
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

-- | The table took the reading and named the spelling it took it under.
respeltAs :: T.Text -> UnitReading -> Bool
respeltAs spelling (ReadRespelt found _) = found == spelling
respeltAs _ (ReadExact _) = False
respeltAs _ ReadAmbiguous{} = False
respeltAs _ ReadUnknown = False

-- | The table spells it exactly this way, so there is nothing to report.
exact :: UnitReading -> Bool
exact (ReadExact _) = True
exact (ReadRespelt _ _) = False
exact ReadAmbiguous{} = False
exact ReadUnknown = False

-- | Load the full unit config from data/units.csv
loadFullUnitConfig :: IO UnitConfig
loadFullUnitConfig = do
    csv <- BL.readFile "data/units.csv"
    case buildFromCSV csv of
        Right cfg -> return cfg
        Left err -> fail $ "Failed to load data/units.csv: " ++ T.unpack err

spec :: Spec
spec = do
    describe "Dimension Parsing" $ do
        let dimOrder = ["mass", "length", "time", "energy", "area", "volume", "count", "currency"]

        it "parses single dimension" $ do
            parseDimension dimOrder "mass" `shouldBe` Right [1, 0, 0, 0, 0, 0, 0, 0]

        it "parses product of dimensions (mass*length)" $ do
            parseDimension dimOrder "mass*length" `shouldBe` Right [1, 1, 0, 0, 0, 0, 0, 0]

        it "parses division (length/time)" $ do
            parseDimension dimOrder "length/time" `shouldBe` Right [0, 1, -1, 0, 0, 0, 0, 0]

        it "parses repeated division (length/time/time)" $ do
            parseDimension dimOrder "length/time/time" `shouldBe` Right [0, 1, -2, 0, 0, 0, 0, 0]

        it "parses complex expression (mass*length/time)" $ do
            parseDimension dimOrder "mass*length/time" `shouldBe` Right [1, 1, -1, 0, 0, 0, 0, 0]

        it "rejects empty expression" $
            parseDimension dimOrder "" `shouldSatisfy` isLeft

        it "rejects whitespace-only expression" $
            parseDimension dimOrder "   " `shouldSatisfy` isLeft

        it "rejects unknown dimension" $
            parseDimension dimOrder "velocity" `shouldSatisfy` isLeft

        it "rejects unknown dimension in denominator" $
            parseDimension dimOrder "mass/velocity" `shouldSatisfy` isLeft

    describe "Unit Compatibility" $ do
        it "tkm and kgkm are compatible (both mass*length)" $ do
            cfg <- loadFullUnitConfig
            unitsCompatible cfg "tkm" "kgkm" `shouldBe` True

        it "tkm and kg are NOT compatible (mass*length vs mass)" $ do
            cfg <- loadFullUnitConfig
            unitsCompatible cfg "tkm" "kg" `shouldBe` False

        it "kg and t are compatible (both mass)" $ do
            cfg <- loadFullUnitConfig
            unitsCompatible cfg "kg" "t" `shouldBe` True

        it "kg and g are compatible (both mass)" $ do
            cfg <- loadFullUnitConfig
            unitsCompatible cfg "kg" "g" `shouldBe` True

        it "MJ and kWh are compatible (both energy)" $ do
            cfg <- loadFullUnitConfig
            unitsCompatible cfg "MJ" "kWh" `shouldBe` True

        it "m/s and km/h are compatible (both velocity)" $ do
            cfg <- loadFullUnitConfig
            unitsCompatible cfg "m/s" "km/h" `shouldBe` True

        it "pkm and person*km are compatible (passenger transport)" $ do
            cfg <- loadFullUnitConfig
            unitsCompatible cfg "pkm" "person*km" `shouldBe` True

        it "unknown units are not compatible" $ do
            cfg <- loadFullUnitConfig
            unitsCompatible cfg "unknown_unit" "kg" `shouldBe` False

        it "l*day is a known unit" $ do
            cfg <- loadFullUnitConfig
            isKnownUnit cfg "l*day" `shouldBe` True

        it "l*day and m3*year are compatible (both volume x time)" $ do
            cfg <- loadFullUnitConfig
            unitsCompatible cfg "l*day" "m3*year" `shouldBe` True

    describe "Unit Conversion" $ do
        it "converts 1 tkm to 1000 kgkm" $ do
            cfg <- loadFullUnitConfig
            convertUnit cfg "tkm" "kgkm" 1.0 `shouldBe` Just 1000.0

        it "converts 1000 kgkm to 1 tkm" $ do
            cfg <- loadFullUnitConfig
            convertUnit cfg "kgkm" "tkm" 1000.0 `shouldBe` Just 1.0

        it "converts 1 t to 1000 kg" $ do
            cfg <- loadFullUnitConfig
            convertUnit cfg "t" "kg" 1.0 `shouldBe` Just 1000.0

        it "converts 1 kg to 1000 g" $ do
            cfg <- loadFullUnitConfig
            convertUnit cfg "kg" "g" 1.0 `shouldBe` Just 1000.0

        it "converts 1 kWh to 3.6 MJ" $ do
            cfg <- loadFullUnitConfig
            case convertUnit cfg "kWh" "MJ" 1.0 of
                Just v -> v `shouldSatisfy` (\x -> abs (x - 3.6) < 0.001)
                Nothing -> expectationFailure "conversion failed"

        -- A year is written four ways in the wild, and a source that writes it
        -- "y" was reading as a unit nobody knew: every conversion through it
        -- failed rather than being off, which is why it went unnoticed.
        it "reads every spelling of a year as the same unit" $ do
            cfg <- loadFullUnitConfig
            mapM_
                (\spelling -> convertUnit cfg spelling "day" 1.0 `shouldBe` Just 365.0)
                ["year", "a", "yr", "y"]

        it "converts 1 kBq to 1000 Bq" $ do
            cfg <- loadFullUnitConfig
            convertUnit cfg "kBq" "Bq" 1.0 `shouldBe` Just 1000.0

        -- Radioactivity's reference unit is kBq, not the SI Bq: EF/ILCD
        -- ionising-radiation CFs are authored per kBq, and a result-expression
        -- CF unit ("kBq U235 equivalents") is unknown to the config, so a Bq
        -- inventory flow must normalize to kBq (÷1000) before the CF applies.
        -- Guards data/units.csv against reverting to a Bq-canonical /time.
        it "normalizes Bq to canonical kBq (ionising-radiation CFs are per kBq)" $ do
            cfg <- loadFullUnitConfig
            fmap (first T.toLower) (normalizeToCanonical cfg "Bq" 1000.0) `shouldBe` Just ("kbq", 1.0)

        -- Energy's reference unit is MJ, not the SI joule: that is the unit
        -- energy CFs and cumulative-energy-demand results are authored in, and
        -- it is what an LCA practitioner reads. A joule-canonical table records
        -- a 1 kWh reference product as 3600000, which is arithmetically right
        -- and unreadable.
        it "normalizes kWh to canonical MJ (energy CFs are per MJ)" $ do
            cfg <- loadFullUnitConfig
            fmap (first T.toLower) (normalizeToCanonical cfg "kWh" 1.0) `shouldBe` Just ("mj", 3.6)

        -- What a dimension's reference unit is decides the unit a reference
        -- product is recorded in and the basis a result-expression CF is read
        -- against. It is a policy choice, not an accident of which spelling
        -- sorts first, so every dimension that declares one is pinned here:
        -- moving one moves recorded amounts, and has to be deliberate. Which
        -- unit is pinned here, not the case the lookup answers in.
        it "pins the canonical unit of every dimension" $ do
            cfg <- loadFullUnitConfig
            let canonicals =
                    [ ("kilogram", Just "kg")
                    , ("meter", Just "m")
                    , ("second", Just "s")
                    , ("joule", Just "mj")
                    , ("square meter", Just "m2")
                    , ("cubic meter", Just "m3")
                    , ("unit", Just "p")
                    , ("EUR2005", Just "eur")
                    , ("Bq", Just "kbq")
                    , ("km/h", Just "m/s")
                    , ("kg/l", Just "kg/m3")
                    , ("kgkm", Just "tkm")
                    , ("m2*year", Just "m2a")
                    , ("m3*year", Just "m3a")
                    , ("kg*day", Just "kgy")
                    , ("km*year", Just "my")
                    , ("passenger-km", Just "pkm")
                    ]
            map (fmap T.toLower . canonicalUnitFor cfg . fst) canonicals `shouldBe` map snd canonicals

        -- A dimension with no row at 1.0 has no reference unit, so
        -- 'normalizeToCanonical' answers Nothing and the amount is recorded in
        -- whatever the source wrote. Two amounts of the same dimension then sit
        -- in one column in two units. Every dimension the table declares carries
        -- one, and this is what says so for the ones no case above names.
        it "leaves no unit without a reference to normalize to" $ do
            cfg <- loadFullUnitConfig
            let orphans = [u | u <- M.keys (ucUnits cfg), isNothing (canonicalUnitFor cfg u)]
            orphans `shouldBe` []

        -- A composed unit's factor is the product of its parts', and the table
        -- writes it by hand. Where the hand slipped, the load was silently out
        -- by that much: a hectare year read as 3.1536e11 square metre years
        -- rather than 10 000, which is the year counted twice.
        it "agrees with the parts every composed unit is made of" $ do
            cfg <- loadFullUnitConfig
            let mile = 1609.344
                composed =
                    [ ("ha a", "m2a", 10000) -- a hectare is 10 000 square metres
                    , ("l*day", "m3a", 1.0e-3 / 365) -- a litre is a thousandth of a cubic metre, a day a 365th of a year
                    , ("kg*day", "kgy", 1 / 365)
                    , ("km*year", "my", 1000)
                    , ("mile*year", "my", mile)
                    , ("person*mile", "pkm", mile / 1000)
                    , ("t*mile", "kgm", 1000 * mile)
                    , ("tkm", "kgm", 1.0e6)
                    , ("kgkm", "tkm", 1.0e-3)
                    , ("km/h", "m/s", 1000 / 3600)
                    ]
            mapM_
                ( \(from, to, expected) -> case convertUnit cfg from to 1.0 of
                    Just got -> got `shouldSatisfy` (\x -> abs (x - expected) <= abs expected * 1.0e-12)
                    Nothing -> expectationFailure (T.unpack (from <> " does not convert to " <> to))
                )
                composed

        -- The table read this as a kilogray, in the dimensionless bucket where
        -- a count lives, while a source writing it means a kilogram year: it is
        -- the reference unit of mass over time in one published unit list.
        it "reads kgy as a mass over time, not as a count" $ do
            cfg <- loadFullUnitConfig
            unitsCompatible cfg "kgy" "kg*year" `shouldBe` True
            unitsCompatible cfg "kgy" "unit" `shouldBe` False

        it "returns Nothing for incompatible units" $ do
            cfg <- loadFullUnitConfig
            convertUnit cfg "kg" "m" 1.0 `shouldBe` Nothing

        it "returns Nothing for unknown units" $ do
            cfg <- loadFullUnitConfig
            convertUnit cfg "unknown" "kg" 1.0 `shouldBe` Nothing

        it "converts 1 m3*year to 365000 l*day" $ do
            cfg <- loadFullUnitConfig
            case convertUnit cfg "m3*year" "l*day" 1.0 of
                Just v -> v `shouldSatisfy` (\x -> abs (x - 365000.0) < 1.0)
                Nothing -> expectationFailure "conversion failed"

    describe "Backward Compatibility" $ do
        it "convertExchangeAmount converts tkm to kgkm" $ do
            cfg <- loadFullUnitConfig
            convertExchangeAmount cfg "tkm" "kgkm" 1.0 `shouldBe` 1000.0

        it "convertExchangeAmount returns original for incompatible units" $ do
            cfg <- loadFullUnitConfig
            convertExchangeAmount cfg "kg" "m" 5.0 `shouldBe` 5.0

    describe "Reading a spelling against the table" $ do
        it "files a spelling under a case-blind key" $ do
            foldedUnit "KG" `shouldBe` "kg"

        it "trims whitespace" $ do
            foldedUnit "  kg  " `shouldBe` "kg"

        it "takes the one reading a case variant leaves, and says which" $ do
            cfg <- loadFullUnitConfig
            readUnit cfg "KG" `shouldSatisfy` respeltAs "kg"
            readUnit cfg "Kg" `shouldSatisfy` respeltAs "kg"
            isKnownUnit cfg "kG" `shouldBe` True

        it "takes the exact spelling without a word about it" $ do
            cfg <- loadFullUnitConfig
            readUnit cfg "kg" `shouldSatisfy` exact
            readUnit cfg " kg " `shouldSatisfy` exact

        it "refuses when two spellings differ only by case" $ do
            let Right cfg = buildFromCSV "name,dimension,factor\nMJ,energy,1.0\nmJ,energy,1.0e-9\n"
            readUnit cfg "mj" `shouldBe` ReadAmbiguous "MJ" "mJ" []
            lookupUnitDef cfg "mj" `shouldBe` Nothing
            isKnownUnit cfg "mj" `shouldBe` False
            readUnit cfg "MJ" `shouldBe` ReadExact (UnitDef [0, 0, 0, 1, 0, 0, 0, 0] 1.0)

        it "refuses a table that spells one unit twice" $ do
            buildFromCSV "name,dimension,factor\nkg,mass,1.0\nkg,mass,2.0\n"
                `shouldBe` (Left "unit spelled more than once: kg" :: Either Text UnitConfig)

        it "refuses when the source tells apart two units the table has one row for" $ do
            -- A published ILCD unit group writes Mg beside mg. A table holding
            -- only mg reads each of them as that one row, and each reading on
            -- its own looks settled; the pair is what says a megagram would be
            -- carried through as a milligram.
            let Right cfg = buildFromCSV "name,dimension,factor\nmg,mass,1.0e-6\n"
                verdict = judgeUnits cfg ["Mg", "mg"]
            uvCollapsed verdict `shouldBe` [("mg", ["Mg", "mg"])]
            uvRespelt verdict `shouldBe` []

        it "says nothing once the table holds both readings" $ do
            let Right cfg = buildFromCSV "name,dimension,factor\nmg,mass,1.0e-6\nMg,mass,1000.0\n"
                verdict = judgeUnits cfg ["Mg", "mg"]
            uvCollapsed verdict `shouldBe` []
            uvRespelt verdict `shouldBe` []
            uvAmbiguous verdict `shouldBe` []
            uvUnknown verdict `shouldBe` []

        it "reports a lone case variant without calling it a collapse" $ do
            let Right cfg = buildFromCSV "name,dimension,factor\nkWh,energy,3.6\n"
                verdict = judgeUnits cfg ["KWH"]
            uvRespelt verdict `shouldBe` [("KWH", "kWh")]
            uvCollapsed verdict `shouldBe` []

    describe "Config Building (buildFromCSV)" $ do
        it "builds config from CSV" $ do
            let csv = "name,dimension,factor\nkg,mass,1.0\ng,mass,0.001\ntkm,mass*length,1e6\n"
            case buildFromCSV csv of
                Left err -> expectationFailure $ "Parse failed: " ++ T.unpack err
                Right cfg -> do
                    isKnownUnit cfg "kg" `shouldBe` True
                    isKnownUnit cfg "tkm" `shouldBe` True

        it "adds custom unit" $ do
            let csv = "name,dimension,factor\ncustomunit,mass,42.0\n"
            case buildFromCSV csv of
                Left err -> expectationFailure $ "Parse failed: " ++ T.unpack err
                Right cfg -> do
                    isKnownUnit cfg "customunit" `shouldBe` True
                    case lookupUnitDef cfg "customunit" of
                        Just def -> udFactor def `shouldBe` 42.0
                        Nothing -> expectationFailure "customunit should exist"

        it "parses compound dimension expressions" $ do
            let csv = "name,dimension,factor\nmyvelocity,length/time,1.0\nmytransport,mass*length,500.0\nm/s,length/time,1.0\ntkm,mass*length,1e6\n"
            case buildFromCSV csv of
                Left err -> expectationFailure $ "Parse failed: " ++ T.unpack err
                Right cfg -> do
                    unitsCompatible cfg "myvelocity" "m/s" `shouldBe` True
                    unitsCompatible cfg "mytransport" "tkm" `shouldBe` True

        it "fails on invalid dimension" $ do
            let csv = "name,dimension,factor\nbadunit,invalid_dimension,1.0\n"
            buildFromCSV csv `shouldSatisfy` isLeftT

        it "merges multiple configs (later overrides)" $ do
            let csv1 = "name,dimension,factor\nkg,mass,1.0\n"
                csv2 = "name,dimension,factor\nkg,mass,999.0\n"
            case (buildFromCSV csv1, buildFromCSV csv2) of
                (Right cfg1, Right cfg2) -> do
                    let merged = mergeUnitConfigs [cfg1, cfg2]
                    case lookupUnitDef merged "kg" of
                        Just def -> udFactor def `shouldBe` 999.0
                        Nothing -> expectationFailure "kg should exist"
                _ -> expectationFailure "both parses should succeed"

        it "mergeUnitConfigs [] returns defaultUnitConfig" $
            unitCount (mergeUnitConfigs []) `shouldBe` unitCount defaultUnitConfig

        it "fails on malformed CSV (wrong column count)" $ do
            let csv = "name,dimension,factor\nkg,mass\n"
            buildFromCSV csv `shouldSatisfy` isLeftT

    describe "unitCount" $ do
        it "is 0 for empty config" $ do
            let Right cfg = buildFromCSV "name,dimension,factor\n"
            unitCount cfg `shouldBe` 0

        it "counts the number of units in defaultUnitConfig" $
            unitCount defaultUnitConfig `shouldSatisfy` (> 0)

isLeftT :: Either T.Text b -> Bool
isLeftT (Left _) = True
isLeftT _ = False
