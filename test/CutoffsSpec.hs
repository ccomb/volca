{-# LANGUAGE OverloadedStrings #-}

{- | Which unsupplied product inputs a solution meets.

Each example solves a hand-built database and reads the cut-offs off the
solution. The expected amounts are worked out by hand from the matrix: a
process run at @x@ whose input is @a@ per reference amount @r@ asks @x * a / r@.
-}
module CutoffsSpec (spec) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import Test.Hspec

import API.Types (CutoffInput (..), WithheldCutoffs (..))
import Config (defaultConfig)
import Database (buildDatabaseWithMatrices)
import Database.Cutoffs (Cutoffs (..), GapIndex (..), gapIndexOf, noCutoffs)
import Database.Manager (CachePolicy (..), clearMethodMappingCacheForDb, getGapIndex, initDatabaseManager)
import Impact (LicencedSolution (..), cutoffsOf, partitionByLicence)
import qualified SharedSolver as SS
import TestHelpers (linkDatabases, mkActivity, mkDepLookupFromMap, mkSolverFromDb, mkTechFlow, reference, techInput, units)
import Types
import UnitConversion (defaultUnitConfig)

u :: String -> UUID
u suffix = read ("00000000-0000-0000-0000-0000000001" <> suffix)

rFlow, pFlow, mFlow, qFlow, nFlow, kFlow, wFlow :: UUID
rFlow = u "01"
pFlow = u "02"
mFlow = u "03"
qFlow = u "04"
nFlow = u "05"
kFlow = u "06"
wFlow = u "07"

actR, actP, actQ, actS, actW, absentAct :: UUID
actR = u "a1"
actP = u "a2"
actQ = u "a3"
actS = u "a4"
actW = u "a5"
absentAct = u "ff" -- named by a source identity, held by no database

-- | An input linked to the activity that produces it in the same database.
linkedTo :: UUID -> UUID -> Double -> Exchange
linkedTo act fid amount = (techInput fid amount){techActivityLinkId = Just act, techSupplierClaim = ClaimById act}

simple :: [((UUID, UUID), Activity)] -> SimpleDatabase
simple acts =
    SimpleDatabase
        { sdbActivities = M.fromList acts
        , sdbTechFlows =
            M.fromList
                [ (f, mkTechFlow f n)
                | (f, n) <- [(rFlow, "R"), (pFlow, "P"), (mFlow, "M"), (qFlow, "Q"), (nFlow, "N"), (kFlow, "K"), (wFlow, "W")]
                ]
        , sdbBioFlows = M.empty
        , sdbWasteFlows = M.empty
        , sdbUnits = units
        , sdbDocumentation = noDocumentation
        }

build :: SimpleDatabase -> IO Database
build sdb =
    buildDatabaseWithMatrices (BuildInputs defaultUnitConfig M.empty Declared []) sdb
        >>= either (fail . show) pure

{- | R takes 2 kg of P (internal) and 0.5 kg of M, P takes 3 kg of M, nobody
makes M. Q, off R's chain, takes 1 kg of N, which nobody makes either.
-}
mainDb :: IO Database
mainDb =
    build $
        simple
            [ ((actR, rFlow), mkActivity "R" [reference rFlow, linkedTo actP pFlow 2, techInput mFlow 0.5])
            , ((actP, pFlow), mkActivity "P" [reference pFlow, techInput mFlow 3])
            , ((actQ, qFlow), mkActivity "Q" [reference qFlow, techInput nFlow 1])
            ]

-- | Solve one unit of a process; the databases listed are its dependencies.
solve :: [(Text, Database)] -> Text -> Database -> (UUID, UUID) -> IO SS.CrossDBSolution
solve deps name db key = do
    depSolvers <- traverse (\(n, d) -> (,) n . (,) d <$> mkSolverFromDb d n) deps
    solver <- mkSolverFromDb db name
    pid <- maybe (fail "no such process") pure (M.lookup key (dbProcessIdLookup db))
    SS.computeInventoryMatrixWithDepsCached defaultUnitConfig (mkDepLookupFromMap (M.fromList depSolvers)) db name solver pid
        >>= either (fail . show) pure

shown :: SS.CrossDBSolution -> LicencedSolution
shown sol = LicencedSolution{lsWhole = sol, lsShown = sol, lsWithheld = []}

-- | The index of every database of the example, scanned as the cache would.
indexIn :: [(Text, Database)] -> Text -> GapIndex
indexIn dbs name = maybe (GapIndex M.empty) gapIndexOf (lookup name dbs)

noNameMatch :: NE.NonEmpty BlockerReason
noNameMatch = NE.singleton (BlockerReason "no_name_match" Nothing)

cutoff :: Text -> Text -> Double -> Int -> NE.NonEmpty BlockerReason -> CutoffInput
cutoff db product amount consumers reasons =
    CutoffInput
        { ciDatabase = db
        , ciProduct = product
        , ciLocation = "FR"
        , ciUnit = "kg"
        , ciAmount = amount
        , ciConsumers = consumers
        , ciReasons = reasons
        }

-- | A link from R's input of M to the producer of M in another database.
linkM :: Database -> Database -> Database
linkM consumer supplier =
    let linked = linkDatabases consumer supplier "supplier" 0.5
     in linked{dbCrossDBLinks = map (\l -> l{cdlConsumerFlowId = mFlow, cdlExchangeUnit = "kg"}) (dbCrossDBLinks linked)}

{- | 'cutoffsOf' with its amounts rounded to a billionth, so a sum the solver
carries to the last bit compares with the one worked out by hand.
-}
metBy :: (Text -> GapIndex) -> LicencedSolution -> Cutoffs
metBy indexOf sol =
    let met = cutoffsOf indexOf sol
     in met{cutoffShown = map (\c -> c{ciAmount = fromIntegral (round (ciAmount c * 1e9) :: Integer) / 1e9}) (cutoffShown met)}

spec :: Spec
spec = do
    describe "cutoffsOf" $ do
        it "sums an unsupplied product over every process of the chain that asks for it" $ do
            -- x_R = 1, x_P = 2: R asks 1 × 0.5 of M, P asks 2 × 3, together 6.5 by two processes.
            db <- mainDb
            sol <- solve [] "main" db (actR, rFlow)
            metBy (indexIn [("main", db)]) (shown sol)
                `shouldBe` Cutoffs [cutoff "main" "M" 6.5 2 noNameMatch] []

        it "leaves out a process the chain does not run" $ do
            -- Solving R scales Q by 0, so N is absent; solving Q gives x_Q = 1 and N at 1 × 1.
            db <- mainDb
            fromR <- solve [] "main" db (actR, rFlow)
            fromQ <- solve [] "main" db (actQ, qFlow)
            map ciProduct (cutoffShown (metBy (indexIn [("main", db)]) (shown fromR))) `shouldBe` ["M"]
            cutoffShown (metBy (indexIn [("main", db)]) (shown fromQ)) `shouldBe` [cutoff "main" "N" 1 1 noNameMatch]

        it "does not count an input another database supplies" $ do
            -- R's 0.5 kg of M goes to S in the dependency, which needs nothing.
            supplier <- build (simple [((actS, mFlow), mkActivity "S" [reference mFlow])])
            consumer <- build (simple [((actR, rFlow), mkActivity "R" [reference rFlow, techInput mFlow 0.5])])
            let linked = linkM consumer supplier
                dbs = [("consumer", linked), ("supplier", supplier)]
            sol <- solve [("supplier", supplier)] "consumer" linked (actR, rFlow)
            metBy (indexIn dbs) (shown sol) `shouldBe` noCutoffs

        it "counts, without naming them, the cut-offs inside a dependency whose licence keeps its detail" $ do
            -- S runs at 0.5 and takes 1 kg of K, which nobody makes: one entry inside the supplier.
            supplier <- build (simple [((actS, mFlow), mkActivity "S" [reference mFlow, techInput kFlow 1])])
            consumer <- build (simple [((actR, rFlow), mkActivity "R" [reference rFlow, techInput mFlow 0.5])])
            let linked = linkM consumer supplier
                dbs = [("consumer", linked), ("supplier", supplier)]
            sol <- solve [("supplier", supplier)] "consumer" linked (actR, rFlow)
            metBy (indexIn dbs) (shown sol)
                `shouldBe` Cutoffs [cutoff "supplier" "K" 0.5 1 noNameMatch] []
            metBy (indexIn dbs) (partitionByLicence M.empty (S.singleton "supplier") sol)
                `shouldBe` Cutoffs [] [WithheldCutoffs "supplier" 1]

        it "lists an input whose source identity no database holds" $ do
            db <- build (simple [((actR, rFlow), mkActivity "R" [reference rFlow, linkedTo absentAct mFlow 0.5])])
            sol <- solve [] "main" db (actR, rFlow)
            cutoffShown (metBy (indexIn [("main", db)]) (shown sol))
                `shouldBe` [cutoff "main" "M" 0.5 1 (NE.singleton (BlockerReason "dangling_source_identity" Nothing))]

        it "asks a positive amount of a waste treatment run as the root" $ do
            -- The reference is -1 kg, so x_W = -1 and the input asks -1 × 0.5 / -1 = +0.5.
            db <- build (simple [((actW, wFlow), mkActivity "W" [(reference wFlow){techAmount = -1}, techInput mFlow 0.5])])
            sol <- solve [] "main" db (actW, wFlow)
            map ciAmount (cutoffShown (metBy (indexIn [("main", db)]) (shown sol))) `shouldBe` [0.5]

        it "puts the largest demand first, whatever its sign" $ do
            -- x_R = 1: K at -3, N at 2, M at 0.5, ranked by size.
            db <- build (simple [((actR, rFlow), mkActivity "R" [reference rFlow, techInput mFlow 0.5, techInput nFlow 2, techInput kFlow (-3)])])
            sol <- solve [] "main" db (actR, rFlow)
            map (\c -> (ciProduct c, ciAmount c)) (cutoffShown (metBy (indexIn [("main", db)]) (shown sol)))
                `shouldBe` [("K", -3), ("N", 2), ("M", 0.5)]

        it "adds up a database the solution lists twice, counting its processes once" $ do
            -- The same entry twice: R and P each ask 6.5 in all, twice over, still two processes.
            db <- mainDb
            sol <- solve [] "main" db (actR, rFlow)
            let twice = sol{SS.csScalings = NE.head (SS.csScalings sol) NE.:| NE.toList (SS.csScalings sol)}
            metBy (indexIn [("main", db)]) (shown twice)
                `shouldBe` Cutoffs [cutoff "main" "M" 13 2 noNameMatch] []

        it "says nothing when every input is supplied" $ do
            db <- build (simple [((actR, rFlow), mkActivity "R" [reference rFlow, linkedTo actP pFlow 2]), ((actP, pFlow), mkActivity "P" [reference pFlow])])
            sol <- solve [] "main" db (actR, rFlow)
            metBy (indexIn [("main", db)]) (shown sol) `shouldBe` noCutoffs

    describe "getGapIndex" $
        it "never serves one version's index to another version of the same database" $ do
            manager <- initDatabaseManager defaultConfig NoCache
            before <- mainDb
            edited <- build (simple [((actR, rFlow), mkActivity "R" [reference rFlow, techInput mFlow 0.5, techInput kFlow 1])])
            gapIndexOf edited `shouldNotBe` gapIndexOf before
            getGapIndex manager "main" before `shouldReturn` gapIndexOf before
            -- A request still holding the old version writes its index after the edit's clear.
            clearMethodMappingCacheForDb manager "main"
            getGapIndex manager "main" before `shouldReturn` gapIndexOf before
            getGapIndex manager "main" edited `shouldReturn` gapIndexOf edited
            getGapIndex manager "main" edited `shouldReturn` gapIndexOf edited
            getGapIndex manager "main" before `shouldReturn` gapIndexOf before
