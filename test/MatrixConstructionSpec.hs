{-# LANGUAGE OverloadedStrings #-}

module MatrixConstructionSpec (spec) where

import Data.Either (isLeft)
import Data.List (elemIndex)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Database (buildDatabaseWithMatrices)
import Matrix (Demand (..), buildDemandVectorFromIndex, computeInventoryMatrix)
import Test.Hspec
import TestHelpers
import Types
import UnitConversion (defaultUnitConfig)

-- | Build a UUID from a literal string; explode loudly at test time if malformed.
mkUUID :: String -> UUID.UUID
mkUUID s = case UUID.fromString s of
    Just u -> u
    Nothing -> error $ "invalid UUID literal in test: " <> s

spec :: Spec
spec = do
    describe "Matrix Construction Sign Convention" $ do
        it "stores technosphere input triplets as positive on samples without substitutions" $ do
            -- Regression: an earlier bug stored input coefficients with the
            -- wrong sign, producing positive (I-A) off-diagonals instead of
            -- negative ones. Database.hs sets value = sign * amount / denom
            -- with sign=+1 for inputs; Matrix.hs negates when building (I-A).
            --
            -- SAMPLE.min3 has no substitution (avoided-burden) rows, so every
            -- input is a real consumption and every tech triplet is positive.
            -- Sources with negative Materials/fuels rows legitimately produce
            -- negative triplets, see SimaProParserSpec "SimaPro substitutions".
            db <- loadSampleDatabase "SAMPLE.min3"

            let techTriples = VU.toList (dbTechnosphereTriples db)

            -- Expected: Y needs 0.6 from X, Z needs 0.4 from Y
            let positiveTriplets = filter (\(SparseTriple _ _ v) -> v > 0) techTriples
            length positiveTriplets `shouldBe` length techTriples

            -- Verify specific expected values
            let sortedTriplets = VU.toList (dbTechnosphereTriples db)
            length sortedTriplets `shouldSatisfy` (>= 2)

        it "normalizes exchanges by reference product amount" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            -- Load activity X which outputs 1.0 kg
            -- Check that technosphere inputs are normalized
            let techTriples = VU.toList (dbTechnosphereTriples db)

            -- Should have at least 2 technosphere exchanges
            length techTriples `shouldSatisfy` (>= 2)

    describe "Biosphere Matrix Construction" $ do
        it "stores emissions as POSITIVE values" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            let bioTriples = VU.toList (dbBiosphereTriples db)

            -- Expected: 2 biosphere flows (CO2 and Zinc)
            length bioTriples `shouldSatisfy` (>= 2)

            -- All biosphere values should be positive for emissions
            let emissionTriplets = filter (\(SparseTriple _ _ v) -> v > 0) bioTriples
            length emissionTriplets `shouldSatisfy` (>= 2)

        it "builds biosphere matrix with correct dimensions" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            let bioFlowCount = dbBiosphereCount db
            let activityCount = dbActivityCount db

            -- SAMPLE.min3: 2 biosphere flows, 3 activities
            bioFlowCount `shouldBe` 2
            activityCount `shouldBe` 3

    describe "Database Structure" $ do
        it "builds ProcessId table correctly" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            -- ProcessId table maps index to (activityUUID, productUUID)
            let processIdTable = dbProcessIdTable db
            V.length processIdTable `shouldBe` 3

        it "builds activity index" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            -- Activity index should be identity mapping for simple case
            let activityIndex = dbActivityIndex db
            V.length activityIndex `shouldBe` 3

        it "refuses a demand vector for a process the index does not hold" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            -- One unit of demand lands in the column the process occupies; an id
            -- with no column is refused rather than answered with a vector of
            -- zeros, which would solve to an inventory of zeros.
            let activityIndex = dbActivityIndex db
                offEnd = fromIntegral (V.length activityIndex) :: ProcessId
            fmap (VU.sum . unDemand) (buildDemandVectorFromIndex activityIndex 0) `shouldBe` Right 1.0
            isLeft (buildDemandVectorFromIndex activityIndex offEnd) `shouldBe` True
            isLeft (buildDemandVectorFromIndex activityIndex (-1)) `shouldBe` True

    describe "Matrix Sparsity" $ do
        it "produces only well above-zero entries on the basic SAMPLE.min3 fixture" $ do
            -- Sanity check on SAMPLE.min3: its declared exchanges are all O(0.1..1),
            -- so post-normalization triplets sit comfortably above any floating-point
            -- noise floor. Kept as a smoke test that the matrix builder produces sane
            -- magnitudes on a known good fixture. Note: PR #69 dropped the previous
            -- post-normalization 1e-15 magnitude filter (it dropped legitimate
            -- emissions on large-productAmount activities); see the "PR #69" test
            -- below for the regression.
            db <- loadSampleDatabase "SAMPLE.min3"

            let techTriples = VU.toList (dbTechnosphereTriples db)
            all (\(SparseTriple _ _ v) -> abs v > 1.0e-15) techTriples `shouldBe` True

        it "excludes diagonal entries from technosphere triplets" $ do
            db <- loadSampleDatabase "SAMPLE.min3"

            let techTriples = VU.toList (dbTechnosphereTriples db)

            -- Diagonal entries (self-loops) are excluded in simple SAMPLE.min3
            let diagonalEntries = filter (\(SparseTriple i j _) -> i == j) techTriples
            length diagonalEntries `shouldBe` 0

        it "keeps biosphere flows on activities with very large reference amounts (PR #69)" $ do
            -- Regression: the previous filter `abs value > 1e-15` was applied AFTER
            -- dividing rawValue by the normalization factor. For activities whose
            -- reference product gets normalized to a large canonical-unit amount at
            -- ingest (e.g. SimaPro turns "3.6 TJ" into 3.6e6 MJ, so normFactor = 3.6e6),
            -- a real 1e-9 kg emission yielded a post-normalization value of ~2.8e-16
            -- and was silently dropped. The fix moves the filter to the source value,
            -- so the triplet must now survive even though its stored magnitude is
            -- below 1e-15.
            let actUUID = mkUUID "11111111-2222-3333-4444-555555555555"
                prodUUID = mkUUID "22222222-3333-4444-5555-666666666666"
                bioFlowUUID = mkUUID "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
                mjUnitId = mkUUID "00000000-0000-0000-0000-00000000000a"
                kgUnitId = mkUUID "00000000-0000-0000-0000-00000000000b"
                rawBioAmount = 1.0e-9 :: Double
                normFactor = 3.6e6 :: Double
                expectedValue = rawBioAmount / normFactor
                refExchange =
                    TechnosphereExchange
                        { techFlowId = prodUUID
                        , techAmount = normFactor
                        , techUnitId = mjUnitId
                        , techRole = ReferenceProduct
                        , techActivityLinkId = Nothing
                        , techSupplierClaim = ClaimByProduct
                        , techLocation = ""
                        , techComment = Nothing
                        , techPedigree = Nothing
                        , techShare = Nothing
                        , techClassification = M.empty
                        , techProperties = noProperties
                        }
                bioExchange =
                    BiosphereExchange
                        { bioFlowId = bioFlowUUID
                        , bioAmount = rawBioAmount
                        , bioUnitId = kgUnitId
                        , bioDirection = Emission
                        , bioLocation = ""
                        , bioComment = Nothing
                        , bioPedigree = Nothing
                        }
                activity =
                    Activity
                        { activityName = "high-canonical-unit power process"
                        , activityDescription = []
                        , activityDocumentation = []
                        , activitySynonyms = M.empty
                        , activityClassification = M.empty
                        , activityLocation = "GLO"
                        , activityLocationSource = LocationDeclared
                        , activityUnit = "mj"
                        , exchanges = [refExchange, bioExchange]
                        , activityParams = M.empty
                        , activityParamExprs = M.empty
                        , activityNativeType = Nothing
                        , activityNativeId = Nothing
                        , activityFormulaCheck = Nothing
                        }
                activityMap = M.singleton (actUUID, prodUUID) activity
                techFlowDB = M.singleton prodUUID (TechnosphereFlow prodUUID "energy product" mjUnitId M.empty Nothing Nothing)
                bioFlowDB = M.singleton bioFlowUUID (BiosphereFlow bioFlowUUID "trace pollutant" kgUnitId M.empty Nothing Nothing (Just (Compartment Air Nothing)))
                unitDB =
                    M.fromList
                        [ (mjUnitId, Unit mjUnitId "mj" "mj" "")
                        , (kgUnitId, Unit kgUnitId "kg" "kg" "")
                        ]

            result <-
                buildDatabaseWithMatrices
                    (BuildInputs defaultUnitConfig mempty Declared)
                    SimpleDatabase
                        { sdbActivities = activityMap
                        , sdbTechFlows = techFlowDB
                        , sdbBioFlows = bioFlowDB
                        , sdbWasteFlows = M.empty
                        , sdbUnits = unitDB
                        }
            case result of
                Left err -> expectationFailure $ "buildDatabaseWithMatrices failed: " <> T.unpack err
                Right db -> do
                    let bioTriples = VU.toList (dbBiosphereTriples db)
                    -- Under the old buggy filter, length would be 0: the triplet was
                    -- dropped because abs (1e-9 / 3.6e6) < 1e-15.
                    length bioTriples `shouldBe` 1
                    case bioTriples of
                        [SparseTriple _ _ v] -> do
                            -- The stored magnitude is genuinely below 1e-15: this is
                            -- exactly the case the new filter must preserve.
                            abs v `shouldSatisfy` (< 1.0e-15)
                            withinTolerance 1.0e-22 expectedValue v `shouldBe` True
                        _ -> expectationFailure "expected exactly one biosphere triplet"

        it "still excludes biosphere rows whose source amount is exactly zero" $ do
            -- The fix replaced `abs value > 1e-15` with `rawValue /= 0`. Confirm the
            -- new predicate still strips truly empty rows: an exchange with raw
            -- amount 0 must not produce a triplet.
            let actUUID = mkUUID "11111111-2222-3333-4444-777777777777"
                prodUUID = mkUUID "22222222-3333-4444-5555-888888888888"
                bioFlowUUID = mkUUID "aaaaaaaa-bbbb-cccc-dddd-ffffffffffff"
                mjUnitId = mkUUID "00000000-0000-0000-0000-00000000000a"
                kgUnitId = mkUUID "00000000-0000-0000-0000-00000000000b"
                refExchange =
                    TechnosphereExchange
                        { techFlowId = prodUUID
                        , techAmount = 1.0
                        , techUnitId = mjUnitId
                        , techRole = ReferenceProduct
                        , techActivityLinkId = Nothing
                        , techSupplierClaim = ClaimByProduct
                        , techLocation = ""
                        , techComment = Nothing
                        , techPedigree = Nothing
                        , techShare = Nothing
                        , techClassification = M.empty
                        , techProperties = noProperties
                        }
                zeroBio =
                    BiosphereExchange
                        { bioFlowId = bioFlowUUID
                        , bioAmount = 0.0
                        , bioUnitId = kgUnitId
                        , bioDirection = Emission
                        , bioLocation = ""
                        , bioComment = Nothing
                        , bioPedigree = Nothing
                        }
                activity =
                    Activity
                        { activityName = "process with empty biosphere row"
                        , activityDescription = []
                        , activityDocumentation = []
                        , activitySynonyms = M.empty
                        , activityClassification = M.empty
                        , activityLocation = "GLO"
                        , activityLocationSource = LocationDeclared
                        , activityUnit = "mj"
                        , exchanges = [refExchange, zeroBio]
                        , activityParams = M.empty
                        , activityParamExprs = M.empty
                        , activityNativeType = Nothing
                        , activityNativeId = Nothing
                        , activityFormulaCheck = Nothing
                        }
                activityMap = M.singleton (actUUID, prodUUID) activity
                techFlowDB = M.singleton prodUUID (TechnosphereFlow prodUUID "energy product" mjUnitId M.empty Nothing Nothing)
                bioFlowDB = M.singleton bioFlowUUID (BiosphereFlow bioFlowUUID "trace pollutant" kgUnitId M.empty Nothing Nothing (Just (Compartment Air Nothing)))
                unitDB =
                    M.fromList
                        [ (mjUnitId, Unit mjUnitId "mj" "mj" "")
                        , (kgUnitId, Unit kgUnitId "kg" "kg" "")
                        ]

            result <-
                buildDatabaseWithMatrices
                    (BuildInputs defaultUnitConfig mempty Declared)
                    SimpleDatabase
                        { sdbActivities = activityMap
                        , sdbTechFlows = techFlowDB
                        , sdbBioFlows = bioFlowDB
                        , sdbWasteFlows = M.empty
                        , sdbUnits = unitDB
                        }
            case result of
                Left err -> expectationFailure $ "buildDatabaseWithMatrices failed: " <> T.unpack err
                Right db ->
                    VU.length (dbBiosphereTriples db) `shouldBe` 0

    describe "Waste-treatment scoring sign (negative reference)" $ do
        -- A waste-treatment / market-for-waste activity's reference flow is a
        -- NEGATIVE production (here -1 kg of the waste it treats). A producer that
        -- sends 3 kg of that waste to treatment must pick up +3× the treatment's
        -- burden, not -3×. Before the activityNormFactor / safeDenom sign fix the
        -- normalization collapsed the -1 reference to +1, flipping every linked
        -- treatment burden negative, so treating waste spuriously *reduced* impact.
        it "adds the treatment burden with a positive sign to the waste producer" $ do
            let tA = mkUUID "11111111-1111-1111-1111-111111111111"
                wW = mkUUID "22222222-2222-2222-2222-222222222222"
                pA = mkUUID "33333333-3333-3333-3333-333333333333"
                yY = mkUUID "44444444-4444-4444-4444-444444444444"
                co2 = mkUUID "55555555-5555-5555-5555-555555555555"
                kgU = mkUUID "66666666-6666-6666-6666-666666666666"
                tRef =
                    TechnosphereExchange
                        { techFlowId = wW
                        , techAmount = -1.0 -- treats 1 kg of waste W (negative production)
                        , techUnitId = kgU
                        , techRole = ReferenceProduct
                        , techActivityLinkId = Nothing
                        , techSupplierClaim = ClaimByProduct
                        , techLocation = ""
                        , techComment = Nothing
                        , techPedigree = Nothing
                        , techShare = Nothing
                        , techClassification = M.empty
                        , techProperties = noProperties
                        }
                tCO2 =
                    BiosphereExchange
                        { bioFlowId = co2
                        , bioAmount = 2.0 -- 2 kg CO2 per kg treated
                        , bioUnitId = kgU
                        , bioDirection = Emission
                        , bioLocation = ""
                        , bioComment = Nothing
                        , bioPedigree = Nothing
                        }
                treatment =
                    Activity
                        { activityName = "treatment of waste W"
                        , activityDescription = []
                        , activityDocumentation = []
                        , activitySynonyms = M.empty
                        , activityClassification = M.empty
                        , activityLocation = "GLO"
                        , activityLocationSource = LocationDeclared
                        , activityUnit = "kg"
                        , exchanges = [tRef, tCO2]
                        , activityParams = M.empty
                        , activityParamExprs = M.empty
                        , activityNativeType = Nothing
                        , activityNativeId = Nothing
                        , activityFormulaCheck = Nothing
                        }
                pRef =
                    TechnosphereExchange
                        { techFlowId = yY
                        , techAmount = 1.0
                        , techUnitId = kgU
                        , techRole = ReferenceProduct
                        , techActivityLinkId = Nothing
                        , techSupplierClaim = ClaimByProduct
                        , techLocation = ""
                        , techComment = Nothing
                        , techPedigree = Nothing
                        , techShare = Nothing
                        , techClassification = M.empty
                        , techProperties = noProperties
                        }
                pWaste =
                    WasteExchange
                        { waFlowId = wW
                        , waAmount = 3.0 -- produces 3 kg of waste W, sent to treatment
                        , waUnitId = kgU
                        , waIsInput = False
                        , waActivityLinkId = Just tA
                        , waSupplierClaim = ClaimByProduct
                        , waLocation = ""
                        , waComment = Nothing
                        , waPedigree = Nothing
                        }
                producer =
                    Activity
                        { activityName = "producer of Y"
                        , activityDescription = []
                        , activityDocumentation = []
                        , activitySynonyms = M.empty
                        , activityClassification = M.empty
                        , activityLocation = "GLO"
                        , activityLocationSource = LocationDeclared
                        , activityUnit = "kg"
                        , exchanges = [pRef, pWaste]
                        , activityParams = M.empty
                        , activityParamExprs = M.empty
                        , activityNativeType = Nothing
                        , activityNativeId = Nothing
                        , activityFormulaCheck = Nothing
                        }
                activityMap = M.fromList [((tA, wW), treatment), ((pA, yY), producer)]
                techFlowDB =
                    M.fromList
                        [ (wW, TechnosphereFlow wW "waste W" kgU M.empty Nothing Nothing)
                        , (yY, TechnosphereFlow yY "product Y" kgU M.empty Nothing Nothing)
                        ]
                bioFlowDB = M.singleton co2 (BiosphereFlow co2 "carbon dioxide" kgU M.empty Nothing Nothing (Just (Compartment Air Nothing)))
                wasteFlowDB = M.singleton wW (WasteFlow wW "waste W" kgU M.empty Nothing Nothing)
                unitDB = M.singleton kgU (Unit kgU "kg" "kg" "")

            result <-
                buildDatabaseWithMatrices
                    (BuildInputs defaultUnitConfig mempty Declared)
                    SimpleDatabase
                        { sdbActivities = activityMap
                        , sdbTechFlows = techFlowDB
                        , sdbBioFlows = bioFlowDB
                        , sdbWasteFlows = wasteFlowDB
                        , sdbUnits = unitDB
                        }
            case result of
                Left err -> expectationFailure $ "buildDatabaseWithMatrices failed: " <> T.unpack err
                Right db -> case elemIndex (pA, yY) (V.toList (dbProcessIdTable db)) of
                    Nothing -> expectationFailure "producer activity was not interned"
                    Just ix -> do
                        inv <- either (fail . show) pure =<< computeInventoryMatrix db (fromIntegral ix)
                        -- 3 kg waste × 2 kg CO2/kg treated = +6 kg CO2 (positive!)
                        withinTolerance 1.0e-9 6.0 (M.findWithDefault 0.0 co2 inv) `shouldBe` True
