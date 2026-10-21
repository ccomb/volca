{-# LANGUAGE OverloadedStrings #-}

module ActivityParametersSpec (spec) where

import Data.Aeson (decode, encode, object, toJSON, (.=))
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.UUID as UUID
import Test.Hspec

import API.Types (ActivityForAPI (..), ParameterAPI (..))
import Database (buildDatabaseWithMatrices)
import Service (convertActivityForAPI, resolveActivityAndProcessId)
import TestHelpers (mkActivity, mkTechFlow, reference, units)
import Types
import UnitConversion (defaultUnitConfig)

spec :: Spec
spec = describe "an activity's parameters" $ do
    it "lists the process's own, then those of its database it reads and does not define, whatever the case" $ do
        detail <- butter
        map prmName (pfaParameters detail) `shouldBe` ["Qm", "share"]
        map prmName (pfaDatabaseParameters detail) `shouldBe` ["ratio", "yield"]

    it "shows an input's value and spread, a calculated one's formula and value" $ do
        detail <- butter
        [(prmValue p, prmFormula p, prmUnit p, prmDescription p, prmUncertainty p) | p <- pfaParameters detail]
            `shouldBe` [ (Just 20.53, Nothing, Nothing, "Milk collected", Just (Lognormal 20.53 1.07))
                       , (Just 10.265, Just "Qm * ratio", Just "kg", "", Nothing)
                       ]

    it "writes a distribution with its own numbers, and reads it back" $ do
        toJSON (Lognormal 20 1.5) `shouldBe` object ["distribution" .= ("lognormal" :: Text), "geometricMean" .= (20 :: Double), "geometricSd" .= (1.5 :: Double)]
        toJSON (Triangular 1 4 6) `shouldBe` object ["distribution" .= ("triangular" :: Text), "minimum" .= (1 :: Double), "mode" .= (4 :: Double), "maximum" .= (6 :: Double)]
        let spreads = [Lognormal 20 1.5, Normal 1 2, Triangular 1 4 6, Uniform 0 3]
        decode (encode spreads) `shouldBe` Just spreads

    it "refuses to read a distribution whose numbers spread nothing" $
        (decode "{\"distribution\": \"uniform\", \"minimum\": 3, \"maximum\": 3}" :: Maybe Uncertainty) `shouldBe` Nothing

{- | A process defining "Qm" and "share", in a database defining "QM", which
"Qm" hides, "ratio", which "share" reads, "yield", which "ratio" reads, and
"unused", which nothing reads.
-}
butter :: IO ActivityForAPI
butter = do
    let product' = UUID.fromWords 1 0 0 1
        process = UUID.fromWords 2 0 0 2
        parameter definition = Parameter{parameterDefinition = definition, parameterUnit = Nothing, parameterDescription = ""}
        made =
            (mkActivity "butter" [reference product'])
                { activityParameters =
                    M.fromList
                        [ ("Qm", (parameter (InputParameter 20.53 (Just (Lognormal 20.53 1.07)))){parameterDescription = "Milk collected"})
                        , ("share", (parameter (CalculatedParameter "Qm * ratio" (Just 10.265))){parameterUnit = Just "kg"})
                        ]
                }
    db <-
        either (fail . show) pure
            =<< buildDatabaseWithMatrices
                (BuildInputs defaultUnitConfig M.empty Declared [])
                SimpleDatabase
                    { sdbActivities = M.singleton (process, product') made
                    , sdbTechFlows = M.singleton product' (mkTechFlow product' "butter")
                    , sdbBioFlows = M.empty
                    , sdbWasteFlows = M.empty
                    , sdbUnits = units
                    , sdbDocumentation = noDocumentation
                    , sdbParameters =
                        M.fromList
                            [ ("QM", parameter (InputParameter 3 Nothing))
                            , ("ratio", parameter (CalculatedParameter "yield / 8" (Just 0.5)))
                            , ("yield", parameter (InputParameter 4 Nothing))
                            , ("unused", parameter (InputParameter 9 Nothing))
                            ]
                    , sdbProductSystems = []
                    , sdbReferenceConvention = SignedReferences
                    }
    either (fail . show) (pure . uncurry (convertActivityForAPI db)) (resolveActivityAndProcessId db (UUID.toText process <> "_" <> UUID.toText product'))
