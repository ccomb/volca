{-# LANGUAGE OverloadedStrings #-}

module SimaProScoringSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Test.Hspec

import Method.SimaProScoring
import Method.Types hiding (DamageCategory (..), NormWeightSet (..))

method :: Text -> Text -> Method
method name unit =
    Method
        { methodId = UUID.nil
        , methodName = name
        , methodDescription = Nothing
        , methodUnit = unit
        , methodCategory = name
        , methodMethodology = Nothing
        , methodFactors = []
        }

methods :: [Method]
methods =
    [ method "Climate change" "kg CO2 eq"
    , method "Ecotoxicity, freshwater - part 1" "CTUe"
    , method "Ecotoxicity, freshwater - part 2" "CTUe"
    , method "Water use" "m3 depriv."
    ]

damages :: [DamageCategory]
damages =
    [ DamageCategory "Climate change" "kg CO2 eq" [("Climate change", 1)]
    , DamageCategory "Ecotoxicity, freshwater" "CTUe" [("Ecotoxicity, freshwater - part 1", 1), ("Ecotoxicity, freshwater - part 2", 1)]
    ]

nwSet :: NormWeightSet
nwSet =
    NormWeightSet
        "EF 3.1"
        (M.fromList [("Climate change", 1.3e-4), ("Ecotoxicity, freshwater", 1.76e-5), ("Water use", 8.7e-5)])
        (M.fromList [("Climate change", 0.2106), ("Ecotoxicity, freshwater", 0.0192)])

raw :: M.Map Text Double
raw = M.fromList [("Climate change", 10), ("Ecotoxicity, freshwater - part 1", 100), ("Ecotoxicity, freshwater - part 2", 50), ("Water use", 3)]

close :: Double -> Double -> Bool
close a b = abs (a - b) <= 1e-12 * max (abs a) (abs b)

-- | Run a check on the one set a translation gives, failing on any other count.
withSole :: ([ScoringSet], [Text]) -> (ScoringSet -> [Text] -> Expectation) -> Expectation
withSole (sets, warnings) check = case sets of
    [set] -> check set warnings
    _ -> expectationFailure ("expected one set, got " <> show (map ssName sets))

spec :: Spec
spec = do
    describe "shortNames" $ do
        it "lowers, replaces every other character by one underscore, trims the edges" $
            shortNames ["Ecotoxicity, freshwater - part 1", "Climate change"]
                `shouldBe` ["ecotoxicity_freshwater_part_1", "climate_change"]

        it "numbers names that would read alike, in the order given" $
            shortNames ["Water use", "Water Use", "Ecotoxicity, freshwater - organics", "Ecotoxicity, freshwater_organics"]
                `shouldBe` ["water_use", "water_use_2", "ecotoxicity_freshwater_organics", "ecotoxicity_freshwater_organics_2"]

        it "never hands out a suffixed name that another one already has" $
            shortNames ["a 2", "a", "A"] `shouldBe` ["a_2", "a", "a_3"]

        it "gives an identifier to a name that starts with a digit or has no letter" $
            shortNames ["2-butene", "%"] `shouldBe` ["v_2_butene", "v"]

    describe "translateScoring" $ do
        let translated = translateScoring methods damages [nwSet]

        it "reads one variable per impact category and one computed variable per damage" $
            withSole translated $ \set warnings -> do
                ssName set `shouldBe` "EF 3.1"
                ssUnit set `shouldBe` "Pt"
                ssOrigin set `shouldBe` ReadFromSimaProFile
                ssVariables set
                    `shouldBe` M.fromList
                        [ ("climate_change", "Climate change")
                        , ("ecotoxicity_freshwater_part_1", "Ecotoxicity, freshwater - part 1")
                        , ("ecotoxicity_freshwater_part_2", "Ecotoxicity, freshwater - part 2")
                        , ("water_use", "Water use")
                        ]
                ssComputed set
                    `shouldBe` M.fromList
                        [ ("climate_change_2", "climate_change")
                        , ("ecotoxicity_freshwater", "ecotoxicity_freshwater_part_1 + ecotoxicity_freshwater_part_2")
                        ]
                ssLabels set `shouldBe` M.fromList [("climate_change_2", "Climate change"), ("ecotoxicity_freshwater", "Ecotoxicity, freshwater")]
                ssUnits set `shouldBe` M.fromList [("climate_change_2", "kg CO2 eq"), ("ecotoxicity_freshwater", "CTUe")]
                ssDisplayMultiplier set `shouldBe` Nothing
                warnings `shouldBe` []

        it "divides by the inverse of the factor SimaPro multiplies by" $
            withSole translated $ \set _ -> do
                M.keys (ssNormalization set) `shouldBe` ["climate_change_2", "ecotoxicity_freshwater", "water_use"]
                M.lookup "climate_change_2" (ssNormalization set) `shouldBe` Just (1 / 1.3e-4)
                ssWeighting set `shouldBe` M.fromList [("climate_change_2", 0.2106), ("ecotoxicity_freshwater", 0.0192)]

        it "sums the damages that have both a normalization and a weight into the single score" $
            withSole translated $ \set _ -> do
                ssScores set `shouldBe` M.singleton singleScoreName "climate_change_2 + ecotoxicity_freshwater"
                let expected = 10 * 1.3e-4 * 0.2106 + 150 * 1.76e-5 * 0.0192
                fmap (M.lookup singleScoreName . seScores) (computeFormulaScores set raw)
                    `shouldSatisfy` either (const False) (maybe False (close expected))

        it "leaves a category with a normalization and no weight out of every score" $
            withSole translated $ \set _ ->
                ssScores set `shouldSatisfy` (not . any (T.isInfixOf "water_use"))

        it "writes a coefficient other than one into the grouping" $
            withSole (translateScoring [method "Climate change" "kg CO2 eq"] [DamageCategory "Climate change" "kg CO2 eq" [("Climate change", 0.5)]] [nwSet]) $ \set _ ->
                M.lookup "climate_change_2" (ssComputed set) `shouldBe` Just "0.5 * climate_change"

        it "keeps a normalization factor of zero, which normalizes to zero as before" $ do
            let zero = nwSet{nwNormalization = M.insert "Climate change" 0 (nwNormalization nwSet)}
            withSole (translateScoring methods damages [zero]) $ \set _ ->
                fmap (M.lookup "climate_change_2" . seNwEnv) (computeFormulaScores set raw) `shouldBe` Right (Just 0)

        it "leaves out, and names, a damage grouping a category the collection does not have" $ do
            let ghost = damages <> [DamageCategory "Ghost" "x" [("Missing category", 1)]]
            withSole (translateScoring methods ghost [nwSet]) $ \set warnings -> do
                M.member "ghost" (ssComputed set) `shouldBe` False
                warnings `shouldBe` ["Damage category 'Ghost' groups 'Missing category', which no impact category of this collection carries; it is left out of the scoring sets."]

        it "names a normalization or a weight that reaches nothing" $ do
            let stray = nwSet{nwWeighting = M.insert "Land use" 0.08 (nwWeighting nwSet)}
            snd (translateScoring methods damages [stray])
                `shouldBe` ["Normalization-weighting set 'EF 3.1' names 'Land use', which is neither a damage category nor an ungrouped impact category; it is not read."]

        it "names a category that two damages group" $ do
            let twice = damages <> [DamageCategory "Toxicity" "CTU" [("Ecotoxicity, freshwater - part 1", 1)]]
            snd (translateScoring methods twice [nwSet])
                `shouldBe` ["Impact category 'Ecotoxicity, freshwater - part 1' is grouped by two damage categories (Ecotoxicity, freshwater, Toxicity); both read it, and the fields kept until 0.16.0 stay empty for it."]

        it "gives a file with damages and no normalization-weighting set one set with no score" $
            withSole (translateScoring methods damages []) $ \set warnings -> do
                ssName set `shouldBe` damageOnlySetName
                ssScores set `shouldBe` M.empty
                M.keys (ssComputed set) `shouldBe` ["climate_change_2", "ecotoxicity_freshwater"]
                warnings `shouldBe` []

        it "gives nothing to a file with neither damages nor sets" $
            translateScoring methods [] [] `shouldBe` ([], [])

        it "gives one set per normalization-weighting set, in the order of the file" $
            map ssName (fst (translateScoring methods damages [nwSet, nwSet{nwName = "Other"}])) `shouldBe` ["EF 3.1", "Other"]
