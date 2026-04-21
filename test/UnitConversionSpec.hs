{-# LANGUAGE OverloadedStrings #-}

module UnitConversionSpec (spec) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Test.Hspec
import UnitConversion

-- Helper for testing Left results
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

-- Helper for testing Right results
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

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

    describe "Unit Normalization" $ do
        it "normalizes to lowercase" $ do
            normalizeUnit "KG" `shouldBe` "kg"

        it "trims whitespace" $ do
            normalizeUnit "  kg  " `shouldBe` "kg"

        it "case-insensitive lookup works" $ do
            cfg <- loadFullUnitConfig
            isKnownUnit cfg "KG" `shouldBe` True
            isKnownUnit cfg "Kg" `shouldBe` True
            isKnownUnit cfg "kG" `shouldBe` True

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
