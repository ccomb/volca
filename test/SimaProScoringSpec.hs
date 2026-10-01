{-# LANGUAGE OverloadedStrings #-}

module SimaProScoringSpec (spec) where

import Data.List (sortOn)
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

    describe "toSimaProBlocks" $ do
        let translated = translateScoring methods damages [nwSet]
            refused name reason = ([], [], ["Scoring set '" <> name <> "' is not exported: " <> reason])

        it "gives back the damages and the normalization-weighting set it was read from" $ do
            let (ds, nws, ws) = toSimaProBlocks (fst translated)
            sortOn dcName ds `shouldBe` sortOn dcName damages
            map nwName nws `shouldBe` ["EF 3.1"]
            map nwWeighting nws `shouldBe` [nwWeighting nwSet]
            ws `shouldBe` []
            map (M.keys . nwNormalization) nws `shouldBe` [M.keys (nwNormalization nwSet)]
            map (and . M.intersectionWith close (nwNormalization nwSet) . nwNormalization) nws `shouldBe` [True]

        it "writes a set that only groups, and no normalization-weighting block for it" $ do
            let (ds, nws, ws) = toSimaProBlocks (fst (translateScoring methods damages []))
            (sortOn dcName ds, nws, ws) `shouldBe` (sortOn dcName damages, [], [])

        it "leaves out a set with a display multiplier, and says why" $
            withSole translated $ \set _ ->
                toSimaProBlocks [set{ssName = "ECS", ssDisplayMultiplier = Just 1e6, ssOrigin = DeclaredInConfig}]
                    `shouldBe` refused "ECS" "SimaPro has no place for a display multiplier."

        it "leaves out a set whose score is not the sum of its weighted damages" $
            withSole translated $ \set _ ->
                toSimaProBlocks [set{ssName = "Doubled", ssScores = M.singleton singleScoreName "2 * climate_change_2 + ecotoxicity_freshwater"}]
                    `shouldBe` refused "Doubled" "SimaPro writes one score, the sum of the damages that have both a normalization and a weight."

        it "leaves out a set with two scores" $
            withSole translated $ \set _ ->
                toSimaProBlocks [set{ssName = "Two", ssScores = M.insert "Other" "climate_change_2" (ssScores set)}]
                    `shouldBe` refused "Two" "SimaPro writes one score, the sum of the damages that have both a normalization and a weight."

        it "leaves out a set whose grouping is no sum of categories times coefficients" $
            withSole translated $ \set _ ->
                toSimaProBlocks [set{ssName = "Square", ssComputed = M.insert "climate_change_2" "climate_change * climate_change" (ssComputed set)}]
                    `shouldBe` refused "Square" "damage 'Climate change' is not a sum of impact categories times coefficients."

        it "leaves out a set that groups otherwise than the first one written" $
            withSole translated $ \set _ -> do
                let other = set{ssName = "Other", ssComputed = M.insert "climate_change_2" "2 * climate_change" (ssComputed set)}
                    (_, nws, ws) = toSimaProBlocks [set, other]
                map nwName nws `shouldBe` ["EF 3.1"]
                ws `shouldBe` ["Scoring set 'Other' is not exported: a SimaPro file has one set of damage categories, and this set groups the impact categories otherwise than 'EF 3.1'."]

        it "writes back a coefficient other than one" $ do
            let halved = [DamageCategory "Climate change" "kg CO2 eq" [("Climate change", 0.5)]]
                (ds, _, _) = toSimaProBlocks (fst (translateScoring [method "Climate change" "kg CO2 eq"] halved [nwSet]))
            ds `shouldBe` halved

    describe "legacyReading" $ do
        let translated = translateScoring methods damages [nwSet]

        it "reads a sub-category on the damage it feeds, as SimaPro multiplies" $
            withSole translated $ \set _ ->
                case legacyReading (legacyReadings set) "Ecotoxicity, freshwater - part 1" 100 of
                    Just (LegacyReading d (Just n) (Just w)) -> do
                        d `shouldBe` "Ecotoxicity, freshwater"
                        n `shouldSatisfy` close (100 * 1.76e-5)
                        w `shouldSatisfy` close (100 * 1.76e-5 * 0.0192)
                    other -> expectationFailure (show other)

        it "names the category itself when no damage groups it, and stays empty without a weight" $
            withSole translated $ \set _ ->
                legacyReading (legacyReadings set) "Water use" 3 `shouldBe` Just (LegacyReading "Water use" Nothing Nothing)

        it "reads nothing for a category the set does not know" $
            withSole translated $ \set _ ->
                legacyReading (legacyReadings set) "Land use" 1 `shouldBe` Nothing

        it "keeps a normalization factor of zero at zero" $ do
            let zero = nwSet{nwNormalization = M.insert "Climate change" 0 (nwNormalization nwSet)}
            withSole (translateScoring methods damages [zero]) $ \set _ ->
                legacyReading (legacyReadings set) "Climate change" 10 `shouldBe` Just (LegacyReading "Climate change" (Just 0) (Just 0))

        it "stays empty for a category two damages group" $ do
            let twice = damages <> [DamageCategory "Toxicity" "CTU" [("Ecotoxicity, freshwater - part 1", 1)]]
            withSole (translateScoring methods twice [nwSet]) $ \set _ ->
                legacyReading (legacyReadings set) "Ecotoxicity, freshwater - part 1" 100
                    `shouldBe` Just (LegacyReading "Ecotoxicity, freshwater - part 1" Nothing Nothing)

        it "reads on the first set read from the file, never on a configured one" $
            withSole translated $ \set _ -> do
                let configured = set{ssName = "PEF", ssOrigin = DeclaredInConfig}
                fmap ssName (legacySet [configured, set]) `shouldBe` Just "EF 3.1"
                fmap ssName (legacySet [configured]) `shouldBe` Nothing
                legacySetNames [configured, set] `shouldBe` ["EF 3.1"]
                legacySetNames (fst (translateScoring methods damages [])) `shouldBe` []
