{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{- | A database read at other values of its parameters.

The fixture is a bakery whose bread takes flour by a formula of the
database's @yield@ and electricity by two lines, France and Germany, whose
formulas read the bread's own @mix_fr@ and @mix_de@. The electricity comes
from a second database, where the German kilowatt-hour emits ten times the
French one.

A loaf, read by its reduced model, takes the bread and a dough that takes the
loaf back, both by formulas of the database's @hydration@, which also sets how
much loaf the loaf makes, and gives a waste to a bin that treats it. A sandwich
takes bread in grams.
-}
module ParametricSpec (spec) where

import API.MCP (callTool, noRequestId)
import API.Routes (ScoreLines (..), activityLCIABatchH, batchImpactsH, postReducedModel)
import API.Types (BatchImpactsEntry (..), BatchImpactsRequest (..), BatchImpactsResponse (..), ExchangeContribution (..), FreeParameter (..), LCIABatchResult (..), LCIAResult (..), ModelBinding (..), ModelEmission (..), ModelInput (..), ModelLine (..), ModelProcess (..), ModelReference (..), ModelScoringSet (..), ModelSupplier (..), ParameterAPI (..), ParameterName (..), ParameterValue (..), ReducedModel (..), ReducedModelRequest (..), Substitution (..), SubstitutionRequest (..), SubstitutionScope (..))
import App.Env (AppEnv (..), AppM, runApp)
import Config (defaultConfig)
import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import Control.Exception (evaluate)
import Control.Monad (foldM, forM_)
import Data.Aeson (Value (..), decode, toJSON)
import qualified Data.Aeson.KeyMap as KM
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Either (isLeft)
import Data.List (nub, sort, sortOn, transpose)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Database (buildDatabaseWithMatrices)
import Database.Allocation (Allocating (..), allocate)
import Database.CrossLinking (LinkingContext (..), buildIndexedDatabaseFromDB, defaultLinkingThreshold, emptyAliasMap)
import Database.Cutoffs (gapIndexOf)
import Database.Loader (findAllCrossDBLinks)
import Database.Manager (CachePolicy (..), CollectionName (..), DatabaseManager (..), getGapIndex, initDatabaseManager, withSettings)
import Database.MatrixBuild (technosphereChange, withActivities)
import Database.Rebuild (rebuildFromActivities)
import qualified Expr
import FlowContributionSpec (payloadOf)
import Method.Mapping (LongTermMode (..))
import Method.Types (Method (..), MethodCF (..), MethodCollection (..), ScoringSet (..), ScoringSetOrigin (..))
import qualified Method.Types as MT
import Parametric (Check (..), ParameterRefusal (..), Setting (..), checkFormula, describeParameterRefusal, instantiate, parameterised)
import Servant (ServerError (..), runHandler)
import Service (withholdBatch)
import SharedSolver (CrossDBSolution (..), SharedSolver (solverDbName), computeInventoryMatrixWithDepsCached, computeScalingVectorCached)
import SynonymDB (emptySynonymDB)
import System.Mem.StableName (makeStableName)
import Test.Hspec
import TestHelpers (install, mkDepLookupFromMap, mkSolverFromDb, withinTolerance)
import Types
import UnitConversion (UnitConfig, UnitDef (..), defaultDimensionOrder, defaultUnitConfig, mergeUnitConfigs, mkUnitConfig)

spec :: Spec
spec = describe "a database at other values of its parameters" $ do
    it "is the database itself when nothing is set, or nothing set moves an amount" $ do
        (bakery, _) <- databases
        M.null <$> instantiate bakery [] `shouldBe` Right True
        M.keys <$> instantiate bakery [Setting Nothing "yield" 1] `shouldBe` Right []

    it "builds the columns a rebuild from the same processes builds" $ do
        (bakery, _) <- databases
        bread <- pidOf bakery (aBread, pBread)
        let settings = [Setting Nothing "yield" 2, Setting (Just bread) "mix_fr" 0, Setting (Just bread) "mix_de" 1]
        changed <- either (fail . show) pure (instantiate bakery settings)
        M.keys changed `shouldMatchList` [bread, flour bakery]
        fast <- either (fail . T.unpack) pure (withActivities bakeryUnits bakery changed)
        let byKey = M.fromList (zip (V.toList (dbProcessIdTable bakery)) (V.toList (dbActivities fast)))
        slow <- either (fail . T.unpack) pure (rebuildFromActivities bakeryUnits bakery byKey)
        triples (dbTechnosphereTriples fast) `shouldBe` triples (dbTechnosphereTriples slow)
        triples (dbBiosphereTriples fast) `shouldBe` triples (dbBiosphereTriples slow)

    it "keeps the technosphere as it was when only emissions move" $ do
        (bakery, _) <- databases
        bread <- pidOf bakery (aBread, pBread)
        changed <- either (fail . show) pure (instantiate bakery [Setting (Just bread) "smoke" 2])
        fast <- either (fail . T.unpack) pure (withActivities bakeryUnits bakery changed)
        dbTechnosphereTriples fast `shouldBe` dbTechnosphereTriples bakery
        dbBiosphereTriples fast `shouldNotBe` dbBiosphereTriples bakery

    it "names, on each link to the other database, the line it supplies" $ do
        (bakery, _) <- databases
        sort [(cdlLocation link, aePosition <$> cdlAnswers link) | link <- dbCrossDBLinks bakery, cdlConsumerActUUID link == aBread]
            `shouldBe` [("DE", Just 3), ("FR", Just 2)]

    it "scores the supplier the settings move the electricity to, in the other database" $ do
        (bakery, grid) <- databases
        bread <- pidOf bakery (aBread, pBread)
        co2Of bakery grid "bakery" `shouldReturn'` 2.5
        atSettings <- either (fail . T.unpack) (pure . snd) (parameterised bakeryUnits bakery [Setting Nothing "yield" 2, Setting (Just bread) "mix_fr" 0, Setting (Just bread) "mix_de" 1])
        -- Flour 4 kg at 0.125, the bread's own 1, and a German kilowatt-hour.
        co2Of atSettings grid "bakery at settings" `shouldReturn'` 11.5

    it "solves values that move inputs on the loaded factorisation, as a new one would" $ do
        (bakery, _) <- databases
        bread <- pidOf bakery (aBread, pBread)
        manager <- bakeryManager
        let settings = [Setting Nothing "yield" 2, Setting (Just bread) "mix_fr" 0, Setting (Just bread) "mix_de" 1]
        loaded <- mkSolverFromDb bakery "bakery-loaded"
        (solvedBy, corrected) <- either (fail . T.unpack) pure =<< withSettings manager "bakery-loaded" (bakery, loaded) settings (\db solver -> (solverDbName solver,) <$> computeScalingVectorCached db solver bread)
        solvedBy `shouldBe` "bakery-loaded"
        atSettings <- either (fail . T.unpack) (pure . snd) (parameterised bakeryUnits bakery settings)
        fresh <- mkSolverFromDb atSettings "bakery-fresh"
        expected <- either (fail . T.unpack) pure =<< computeScalingVectorCached atSettings fresh bread
        moved <- either (fail . show) pure (instantiate bakery settings)
        technosphereChange (M.keysSet moved) bakery atSettings `shouldSatisfy` (not . null)
        (U.toList . U.zipWith (withinTolerance 1e-9) expected <$> corrected) `shouldSatisfy` either (const False) and

    it "factorises again for values that move more inputs than a correction holds" $ do
        -- Each of 130 processes buys the one before it by the same formula.
        chain <- build "chain" chainFlows chainActivities (M.singleton "yield" (input 1))
        top <- pidOf chain (link 129)
        manager <- bakeryManager
        let settings = [Setting Nothing "yield" 1.01]
        loaded <- mkSolverFromDb chain "chain-loaded"
        (solvedBy, got) <- either (fail . T.unpack) pure =<< withSettings manager "chain-loaded" (chain, loaded) settings (\db solver -> (solverDbName solver,) <$> computeScalingVectorCached db solver top)
        solvedBy `shouldNotBe` "chain-loaded"
        atSettings <- either (fail . T.unpack) (pure . snd) (parameterised bakeryUnits chain settings)
        fresh <- mkSolverFromDb atSettings "chain-fresh"
        expected <- either (fail . T.unpack) pure =<< computeScalingVectorCached atSettings fresh top
        (U.toList . U.zipWith (withinTolerance 1e-9) expected <$> got) `shouldSatisfy` either (const False) and

    it "lets a process's own value of a name win over its database's, in the database's formulas too" $ do
        (bakery, _) <- databases
        cake <- pidOf bakery (aCake, pCake)
        M.keys <$> instantiate bakery [Setting Nothing "yield" 5] `shouldBe` Right (sort [bread' bakery, flour bakery])
        Right changed <- pure (instantiate bakery [Setting (Just cake) "yield" 1])
        map exchangeAmount . exchanges <$> M.lookup cake changed `shouldBe` Just [1, 4]

    it "sets a process's value on every product of its block" $ do
        (bakery, _) <- databases
        meal <- pidOf bakery (aMill, pMeal)
        bran <- pidOf bakery (aMill, pBran)
        M.keys <$> instantiate bakery [Setting (Just bran) "grind" 2] `shouldBe` Right (sort [meal, bran])

    it "refuses what it cannot compute, naming it" $ do
        (bakery, _) <- databases
        bread <- pidOf bakery (aBread, pBread)
        refusal (instantiate bakery [Setting Nothing "nope" 1]) `shouldBe` Just (UnknownParameter Nothing "nope")
        refusal (instantiate bakery [Setting Nothing "ratio" 1]) `shouldBe` Just (FormulaDefined Nothing "ratio")
        refusal (instantiate bakery [Setting (Just bread) "yield" 1]) `shouldBe` Just (DatabaseWide "bread" "yield")
        refusal (instantiate bakery [Setting Nothing "yield" 1, Setting Nothing "YIELD" 2]) `shouldBe` Just (SetTwice Nothing "YIELD")
        case instantiate bakery [Setting Nothing "yield" 0] of
            Left (NoAmount "flour" "carbon dioxide" _) -> pure ()
            other -> expectationFailure ("expected the flour's emission refused, got " <> show (refusal other))
        refusal (instantiate bakery [Setting Nothing "share_pct" 50]) `shouldBe` Just (ShareMoves "jam" "share_pct")

    it "reads the gap index at other values from the loaded database's, and keeps each apart" $ do
        -- Without its links to the grid, the bakery's electricity is a cut-off input.
        bakery <- evaluate . (\(linked, _) -> linked{dbCrossDBLinks = []}) =<< databases
        manager <- bakeryManager
        install manager "bakery" bakery
        -- The German electricity, asked for nothing by default, becomes one; the French one stops being one.
        bread <- Just <$> pidOf bakery (aBread, pBread)
        atMix <- either (fail . T.unpack) (pure . snd) (parameterised bakeryUnits bakery [Setting bread "mix_fr" 0, Setting bread "mix_de" 1])
        gapIndexOf atMix `shouldNotBe` gapIndexOf bakery
        loaded <- makeStableName =<< evaluate bakery
        mixed <- makeStableName =<< evaluate atMix
        let cachedFrom cache identity = fmap ((== identity) . fst) . M.lookup "bakery" <$> readTVarIO (cache manager)
        getGapIndex manager "bakery" atMix `shouldReturn` gapIndexOf atMix
        cachedFrom dmGapIndexCache loaded `shouldReturn` Just True
        cachedFrom dmGapIndexAtValues mixed `shouldReturn` Just True
        getGapIndex manager "bakery" bakery `shouldReturn` gapIndexOf bakery
        getGapIndex manager "bakery" atMix `shouldReturn` gapIndexOf atMix
        cachedFrom dmGapIndexCache loaded `shouldReturn` Just True

    it "scores each process of a batch at each set, as the request at that set alone would" $ do
        manager <- scoringManager
        let processes = [processIdText (aBread, pBread), processIdText (aFlour, pFlour)]
            sets =
                [ [ParameterValue Nothing "yield" 2]
                , [ParameterValue (Just (processIdText (aBread, pBread))) "mix_fr" 0, ParameterValue (Just (processIdText (aBread, pBread))) "mix_de" 1]
                , []
                ]
        batch <- answer manager (batchImpactsH "bakery" (CollectionName "co2") Nothing IncludeLongTerm (BatchImpactsRequest processes sets))
        alone <-
            sequence
                [ answer manager (activityLCIABatchH "bakery" process (CollectionName "co2") (Just (SubstitutionRequest [] values)) IncludeLongTerm WithoutLines)
                | values <- sets
                , process <- processes
                ]
        [(bieParameterSet e, bieProcessId e) | e <- birResults batch] `shouldBe` [(Just i, p) | i <- [0, 1, 2], p <- processes]
        map (scoresOf . bieImpacts) (birResults batch) `shouldBe` map scoresOf alone
        -- The bread's score moves with each set, so the rows are not all one number.
        length (nub (map (scoresOf . bieImpacts) (birResults batch))) `shouldSatisfy` (> 2)
        atDefaults <- answer manager (batchImpactsH "bakery" (CollectionName "co2") Nothing IncludeLongTerm (BatchImpactsRequest processes []))
        [(bieParameterSet e, scoresOf (bieImpacts e)) | e <- birResults atDefaults] `shouldBe` [(Nothing, scoresOf (bieImpacts e)) | e <- drop 4 (birResults batch)]

    it "refuses a batch one of whose sets it cannot compute, naming the set" $ do
        manager <- scoringManager
        refused <- runHandler . runApp (envOf manager) $ batchImpactsH "bakery" (CollectionName "co2") Nothing IncludeLongTerm (BatchImpactsRequest [processIdText (aBread, pBread)] [[ParameterValue Nothing "yield" 2], [ParameterValue Nothing "nope" 1]])
        either (\e -> (errHTTPCode e, take 16 (BSL.unpack (errBody e)))) (const (200, "")) refused `shouldBe` (400, "parameter set 1:")

    it "reads a request that only sets parameters, and refuses a misspelt field of one or of a batch" $ do
        let request = decode "{\"parameters\": [{\"process\": \"p\", \"name\": \"yield\", \"value\": 2}]}" :: Maybe SubstitutionRequest
        fmap (null . srSubstitutions) request `shouldBe` Just True
        fmap (map (\v -> (pvProcess v, pvName v, pvValue v)) . srParameters) request `shouldBe` Just [(Just "p", "yield", 2)]
        isNothing (decode "{\"parameters\": [{\"proces\": \"p\", \"name\": \"yield\", \"value\": 2}]}" :: Maybe SubstitutionRequest) `shouldBe` True
        -- A batch whose sets are misspelt would be scored at the database's own values.
        isNothing (decode "{\"processIds\": [\"p\"], \"parameter_sets\": [[]]}" :: Maybe BatchImpactsRequest) `shouldBe` True
        fmap (null . birParameterSets) (decode "{\"processIds\": [\"p\"]}") `shouldBe` Just True

    it "gives at other values the scores the engine gives there, through a loop, a moving reference and another database" $ do
        manager <- scoringManager
        (bakery, _) <- databases
        let bread = Just (processIdText (aBread, pBread))
            loaf = processIdText (aLoaf, pLoaf)
            names = [ParameterName bread "mix_fr", ParameterName bread "mix_de", ParameterName Nothing "yield", ParameterName Nothing "hydration"]
            -- The first set is the defaults; the second moves the electricity to the German line, at zero by default.
            sets = [[1, 0, 1, 1], [0, 1, 2, 1.5], [0.3, 0.7, 0.5, 0.8], [1, 0, 1, 0.6]]
        forM_ [IncludeLongTerm, ExcludeLongTerm] $ \policy -> do
            model <- answer manager (postReducedModel "bakery" loaf (CollectionName "model") (Just (policy == ExcludeLongTerm)) (ReducedModelRequest names))
            map mprName (rdmProcesses model) `shouldMatchList` ["bread", "flour", "loaf", "dough"]
            forM_ sets $ \values -> do
                engine <- answer manager (activityLCIABatchH "bakery" loaf (CollectionName "model") (Just (SubstitutionRequest [] (zipWith (\n v -> ParameterValue (pnProcess n) (pnName n) v) names values))) policy WithoutLines)
                (scores, scoring) <- either (fail . T.unpack) pure (modelAt model values)
                (values, zipWith (withinTolerance 1e-9) (scoresOf engine) scores) `shouldBe` (values, map (const True) scores)
                M.keys scoring `shouldBe` M.keys (lbrScoringResults engine)
                and (M.intersectionWith (\a b -> and (M.intersectionWith (withinTolerance 1e-9) a b) && M.keys a == M.keys b) (lbrScoringResults engine) scoring) `shouldBe` True
        -- Its differences in the bakery's own rows are those the engine's columns take at the same values.
        model <- answer manager (postReducedModel "bakery" loaf (CollectionName "model") Nothing (ReducedModelRequest names))
        breadPid <- pidOf bakery (aBread, pBread)
        let settings = [Setting (Just breadPid) "mix_fr" 0, Setting (Just breadPid) "mix_de" 1, Setting Nothing "yield" 2, Setting Nothing "hydration" 1.5]
        moved <- either (fail . show) pure (instantiate bakery settings)
        atSettings <- either (fail . T.unpack) (pure . snd) (parameterised bakeryUnits bakery settings)
        differences <- either (fail . T.unpack) pure (modelDifferences model [0, 1, 2, 1.5])
        let expected = M.fromList [((processIdToText bakery (fromIntegral row), processIdToText bakery (fromIntegral column)), negate v) | (column, entries) <- technosphereChange (M.keysSet moved) bakery atSettings, (row, v) <- entries]
            within = M.filterWithKey (\(supplier, _) _ -> any (\s -> msuProcessId s == supplier && msuDatabase s == "bakery") (rdmSuppliers model)) differences
        M.keys (M.filter (/= 0) within) `shouldBe` M.keys expected
        and (M.intersectionWith (withinTolerance 1e-12) expected within) `shouldBe` True

    it "splits each score by the activity's lines, which add up to it" $ do
        manager <- scoringManager
        let scored process values = answer manager (activityLCIABatchH "bakery" (processIdText process) (CollectionName "model") (Just (SubstitutionRequest [] values)) IncludeLongTerm WithLines)
            climate batch = [byPosition r | r <- lbrResults batch, lrMethodName r == "Climate change"]
        -- The bread: 2 kg of flour at 0.25, a French kilowatt-hour at 1, a German one at zero, and its own 1.
        (climate <$> scored (aBread, pBread) []) `shouldReturnNear` [M.fromList [(1, 0.5), (2, 1), (3, 0), (4, 1)]]
        -- The sandwich: 500 g of that bread, 2 kg of French electricity, and its own 0.1.
        (climate <$> scored (aSandwich, pSandwich) []) `shouldReturnNear` [M.fromList [(1, 1.25), (2, 2), (3, 0.1)]]
        -- A treatment's reference is a negative output: one unit of its service still emits its 2 kg.
        (climate <$> scored (aBin, pWaste) []) `shouldReturnNear` [M.fromList [(1, 2)]]
        let bread = Just (processIdText (aBread, pBread))
            atValues = [ParameterValue bread "mix_fr" 0.3, ParameterValue bread "mix_de" 0.7, ParameterValue Nothing "yield" 2, ParameterValue Nothing "hydration" 1.5]
        forM_ [((aLoaf, pLoaf), []), ((aLoaf, pLoaf), atValues), ((aBread, pBread), atValues), ((aSandwich, pSandwich), []), ((aBin, pWaste), [])] $ \(process, values) -> do
            batch <- scored process values
            [(lrMethodName r, withinTolerance 1e-9 (lrScore r) (sum (byPosition r))) | r <- lbrResults batch] `shouldBe` [(lrMethodName r, True) | r <- lbrResults batch]
            [lrScore r | r <- lbrResults batch] `shouldSatisfy` any (/= 0)
        -- The loaf's dough line comes back to the loaf, and its waste goes to the bin.
        loaf <- scored (aLoaf, pLoaf) []
        [M.keys (byPosition r) | r <- lbrResults loaf] `shouldBe` [[1, 2, 3, 4], [1, 2, 3, 4]]

    it "gives the lines only when asked, without substitutions, and under a licence that shows them" $ do
        manager <- scoringManager
        let loaf = processIdText (aLoaf, pLoaf)
            scored request lines' = runHandler (runApp (envOf manager) (activityLCIABatchH "bakery" loaf (CollectionName "model") request IncludeLongTerm lines'))
        plain <- answer manager (activityLCIABatchH "bakery" loaf (CollectionName "model") Nothing IncludeLongTerm WithoutLines)
        map (null . lrExchangeContributions) (lbrResults plain) `shouldBe` [True, True]
        substituted <- scored (Just (SubstitutionRequest [Substitution (processIdText (aBread, pBread)) (processIdText (aCake, pCake)) AllConsumers] [])) WithLines
        either errHTTPCode (const 200) substituted `shouldBe` 400
        split <- answer manager (activityLCIABatchH "bakery" loaf (CollectionName "model") Nothing IncludeLongTerm WithLines)
        let kept refused = map (null . lrExchangeContributions) (lbrResults (withholdBatch "bakery" (LicenceOwn OwnLicence{ownText = "Ours", ownRefused = S.fromList refused, ownAttribution = AttributionRequired}) split))
        kept [] `shouldBe` [False, False]
        kept [ReadInventory] `shouldBe` [True, True]
        kept [SeeDetailedScores] `shouldBe` [True, True]
        reply <-
            callTool manager [] Nothing Nothing noRequestId "score_activity" . KM.fromList $
                [("database", String "bakery"), ("process_id", String loaf), ("collection", String "model"), ("exchange_contributions", Bool True)]
        let split' = \case
                Object r | Just (Array a) <- KM.lookup "exchangeContributions" r -> not (null a)
                _ -> False
        (KM.lookup "results" =<< payloadOf reply) `shouldSatisfy` \case
            Just (Array rs) -> length rs == 2 && all split' rs
            _ -> False

    it "gives through its tool the model the route gives" $ do
        manager <- scoringManager
        let loaf = processIdText (aLoaf, pLoaf)
            names = [ParameterName (Just (processIdText (aBread, pBread))) "mix_de", ParameterName Nothing "hydration"]
        model <- answer manager (postReducedModel "bakery" loaf (CollectionName "model") Nothing (ReducedModelRequest names))
        reply <-
            callTool manager [] Nothing Nothing noRequestId "get_reduced_model" . KM.fromList $
                [("database", String "bakery"), ("process_id", String loaf), ("collection", String "model"), ("parameters", toJSON names)]
        (Object <$> payloadOf reply) `shouldBe` Just (toJSON model)

    describe "a block split by its shares" $
        it "reads a named line as its source wrote it, so every formula still gives its amount" $ do
            -- 1.25 kg of flour feeds 0.625 kg of emission; the meal takes 60 %, the bran 40 %.
            -- The emission reads the bran line too, which the meal's process does not hold.
            let block =
                    (activity "mill" [declared 60 (reference pMeal), declared 40 (withFormula (named "bran_kg" "1") (line pBran 1 Coproduct)), withFormula (named "Flour_kg" "1 / yield") (line pFlour 1.25 Input), emission "0.5 * flour_kg * bran_kg" 0.625])
                        { activityParameters = M.singleton "yield" (input 0.8)
                        }
                split = NE.toList (allocate Allocating{alKey = Declared, alUnitConfig = bakeryUnits, alUnitDB = units} block)
            mill <- build "mill" techFlows (M.fromList (zip [(aMill, pMeal), (aMill, pBran)] split)) M.empty
            forM_ split $ \act ->
                forM_ [(exchangeAmount ex, efText f) | ex <- exchanges act, Just f <- [exchangeFormula ex]] $ \(stored, formula) ->
                    (checkValue <$> checkFormula bakeryUnits mill (Just (act, Nothing)) formula) `shouldSatisfy` either (const False) (Expr.agrees stored)

    describe "a formula checked where it would be written" $ do
        let checked at formula = do
                (bakery, _) <- databases
                bread <- maybe (fail "no bread") pure (getActivity bakery =<< findProcessId bakery aBread pBread)
                pure (checkFormula bakeryUnits (withCheckParameters bakery) (at bread) formula)
            inBread act = Just (act, Nothing)
            onLine i act = Just (act, Just (exchanges act !! i))
            -- The flour line, named, as EcoSpold 2 names a line other formulas read.
            onNamedFlour i act = onLine i act{exchanges = [if j == 1 then withFormula (Just ExchangeFormula{efName = Just "Flour_kg", efText = "2 * yield"}) ex else ex | (j, ex) <- zip [0 :: Int ..] (exchanges act)]}
            valueOf = fmap checkValue
            problemOf = either Just (const Nothing)

        it "gives the value at the defaults, and the names the formula reads" $ do
            fmap (\c -> (checkValue c, checkReads c)) <$> checked inBread "2 * Yield + mix_fr" `shouldReturn` Right (3, ["yield", "mix_fr"])
            valueOf <$> checked (const Nothing) "ratio / 2" `shouldReturn` Right 2

        it "reads a database's parameter the process does not read yet" $
            valueOf <$> checked inBread "distance" `shouldReturn` Right 3

        it "names the name that has no value" $ do
            problemOf <$> checked inBread "nope * 2" `shouldReturn` Just "no parameter or named line 'nope'"
            -- The bread's own names are not the database's.
            problemOf <$> checked (const Nothing) "mix_fr" `shouldReturn` Just "no parameter or named line 'mix_fr'"

        it "says a cycle is a cycle, not an unknown name" $
            checked inBread "loop_a * 2" >>= (`shouldSatisfy` either (\p -> all (`T.isInfixOf` p) ["loop_a", "loop_b", "need each other's values"]) (const False))

        it "puts a line's new formula in place of its old one, so a name that reads it back is a cycle" $ do
            valueOf <$> checked (onNamedFlour 2) "flour_kg * 2" `shouldReturn` Right 4
            problemOf <$> checked (onNamedFlour 1) "flour_kg + 1" `shouldReturn` Just "flour_kg needs its own value"

        it "says what does not read, and refuses a value that is not a number" $ do
            checked inBread "2 +" >>= (`shouldSatisfy` isLeft)
            problemOf <$> checked inBread "1 / (yield - 1)" `shouldReturn` Just "the formula gives no finite number: Infinity"

        it "warns of a parameter whose unit does not convert to the line's, and judges nothing else" $ do
            fmap checkWarnings <$> checked (onLine 1) "distance" `shouldReturn` Right ["parameter 'distance' is in m, which does not convert to kg, the line's unit"]
            forM_ ["2 * distance", "distance / 2", "-distance"] $ \f ->
                fmap (length . checkWarnings) <$> checked (onLine 1) f `shouldReturn` Right 1
            fmap checkWarnings <$> checked (onLine 1) "odd_unit" `shouldReturn` Right ["unit 'furlongs' is not in the unit table, so parameter 'odd_unit' was not compared with the line's unit"]
            fmap checkWarnings <$> checked (onLine 1) "distance * yield" `shouldReturn` Right []
            fmap checkWarnings <$> checked (onLine 1) "dough_mass" `shouldReturn` Right []
            fmap checkWarnings <$> checked inBread "distance" `shouldReturn` Right []

    it "refuses a model it cannot give, naming why" $ do
        manager <- scoringManager
        let refusedWith request process = either (\e -> (errHTTPCode e, BSL.unpack (errBody e))) (const (200, "")) <$> runHandler (runApp (envOf manager) (postReducedModel "bakery" process (CollectionName "model") Nothing request))
        fst <$> refusedWith (ReducedModelRequest []) (processIdText (aLoaf, pLoaf)) `shouldReturn` 400
        refusedWith (ReducedModelRequest [ParameterName Nothing "share_pct"]) (processIdText (aJam, pJam)) `shouldReturn` (400, T.unpack (describeParameterRefusal (ShareMoves "jam" "share_pct")))
        fst <$> refusedWith (ReducedModelRequest [ParameterName Nothing "nope"]) (processIdText (aLoaf, pLoaf)) `shouldReturn` 400
  where
    shouldReturn' :: IO Double -> Double -> Expectation
    shouldReturn' action expected = action >>= \got -> (got, withinTolerance 1e-9 expected got) `shouldBe` (got, True)

    shouldReturnNear :: IO [M.Map Int Double] -> [M.Map Int Double] -> Expectation
    shouldReturnNear action expected = action >>= \got -> (got, map M.keys got == map M.keys expected && and (zipWith (\a b -> and (M.intersectionWith (withinTolerance 1e-9) a b)) expected got)) `shouldBe` (got, True)

    byPosition :: LCIAResult -> M.Map Int Double
    byPosition r = M.fromListWith (+) [(ecPosition c, ecContribution c) | c <- lrExchangeContributions r]

    flour db = fromMaybe (-1) (findProcessId db aFlour pFlour)
    bread' db = fromMaybe (-1) (findProcessId db aBread pBread)

refusal :: Either ParameterRefusal a -> Maybe ParameterRefusal
refusal = either Just (const Nothing)

{- | The scores a reduced model gives at these values of its parameters, by
method and by scoring set, computed as a program reading it computes them.
-}
modelAt :: ReducedModel -> [Double] -> Either Text ([Double], M.Map Text (M.Map Text Double))
modelAt model values = do
    at <- traverse (columnAt model values) (rdmProcesses model)
    at0 <- traverse (columnAt model (defaultsOf model)) (rdmProcesses model)
    let suppliers = rdmSuppliers model
        k = length at
        d = [[M.findWithDefault 0 i c - M.findWithDefault 0 i c0 | ((c, _), (c0, _)) <- zip at at0] | i <- [0 .. length suppliers - 1]]
        m = transpose (map msuScalings suppliers)
        system = [[(if p == q then 1 else 0) - sum (zipWith (*) (m !! p) (map (!! q) d)) | q <- [0 .. k - 1]] | p <- [0 .. k - 1]]
        w = solve system (map mprScaling (rdmProcesses model))
        dw = [sum (zipWith (*) row w) | row <- d]
        scores =
            [ base + sum [msuScores s !! j * x | (s, x) <- zip suppliers dw] + sum [(g !! j - g0 !! j) * wp | ((_, g), (_, g0), wp) <- zip3 at at0 w]
            | (j, base) <- zip [0 ..] (rdmBaseline model)
            ]
    sets <- traverse (setAt scores) (rdmScoringSets model)
    pure (scores, M.fromList sets)
  where
    setAt :: [Double] -> ModelScoringSet -> Either Text (Text, M.Map Text Double)
    setAt scores set = do
        raw <- bound (M.map (scores !!) (mssVariables set)) (mssComputed set)
        let weighted = M.mapWithKey (\v x -> x / M.findWithDefault 1 v (mssNormalization set) * M.findWithDefault 0 v (mssWeighting set)) raw
        (mssName set,) <$> traverse (fmap (* mssDisplayMultiplier set) . evaluated weighted) (mssScores set)

{- | Each supplier's row of A at these values minus at the defaults, keyed by
supplier and moving process, as a program reading the model computes it.
-}
modelDifferences :: ReducedModel -> [Double] -> Either Text (M.Map (Text, Text) Double)
modelDifferences model values = do
    at <- traverse (columnAt model values) (rdmProcesses model)
    at0 <- traverse (columnAt model (defaultsOf model)) (rdmProcesses model)
    pure $
        M.fromList
            [ ((msuProcessId s, mprProcessId process), M.findWithDefault 0 i c - M.findWithDefault 0 i c0)
            | (process, (c, _), (c0, _)) <- zip3 (rdmProcesses model) at at0
            , (i, s) <- zip [0 ..] (rdmSuppliers model)
            ]

defaultsOf :: ReducedModel -> [Double]
defaultsOf model = [fromMaybe 0 (prmValue (fpParameter p)) | p <- rdmParameters model]

-- | A moving process's column at these values: its suppliers' rows, and what one unit of it emits, scored by method.
columnAt :: ReducedModel -> [Double] -> ModelProcess -> Either Text (M.Map Int Double, [Double])
columnAt model values process = do
    atDefaults <- scope (defaultsOf model)
    atValues <- scope values
    let amount (ModelLine stored formula) = case formula of
            Nothing -> Right stored
            Just f -> (\v v0 -> if v == v0 then stored else v) <$> evaluated atValues f <*> evaluated atDefaults f
        ModelReference outputs selfLoops inputs = mprReference process
    o <- sum <$> traverse amount outputs
    s <- sum <$> traverse amount selfLoops
    i <- sum . map abs <$> traverse amount inputs
    let reference
            | abs o > 1e-15 && abs (o - s) > 1e-15 = o - s
            | i > 1e-15 = i
            | otherwise = 1
    tech <- M.fromListWith (+) <$> traverse (\(ModelInput supplier factor line) -> (\a -> (supplier, factor * a / reference)) <$> amount line) (mprTechnosphere process)
    bio <- foldr (zipWith (+)) (map (const 0) (rdmMethods model)) <$> traverse (\(ModelEmission factors line) -> (\a -> map (\f -> f * a / reference) factors) <$> amount line) (mprBiosphere process)
    pure (tech, bio)
  where
    scope :: [Double] -> Either Text (M.Map Text Double)
    scope at = bound (M.union (mprValues process) ((at !!) <$> mprParameters process)) (mprBindings process)

-- | Names given their values, then each binding computed in turn.
bound :: M.Map Text Double -> [ModelBinding] -> Either Text (M.Map Text Double)
bound = foldM (\env (ModelBinding name formula) -> (\v -> M.insert name v env) <$> evaluated env formula)

evaluated :: M.Map Text Double -> Expr.Formula -> Either Text Double
evaluated env = first Expr.describeRefusal . Expr.evaluate Expr.Volca env . Expr.render

-- | x such that a x = b, eliminating on the largest pivot first.
solve :: [[Double]] -> [Double] -> [Double]
solve a b = backward (forward (zipWith (\row x -> row <> [x]) a b))
  where
    forward :: [[Double]] -> [[Double]]
    forward rows = case sortOn (negate . abs . leading) rows of
        [] -> []
        pivot : others -> pivot : forward [drop 1 (zipWith (\q r -> r - q * leading row / leading pivot) pivot row) | row <- others]

    leading :: [Double] -> Double
    leading = \case
        x : _ -> x
        [] -> 0

    backward :: [[Double]] -> [Double]
    backward = foldr (\row xs -> case row of c : rest -> (last rest - sum (zipWith (*) rest xs)) / c : xs; [] -> xs) []

-- | A manager holding the bakery and its grid, and a collection scoring their carbon dioxide.
scoringManager :: IO DatabaseManager
scoringManager = do
    (bakery, grid) <- databases
    manager <- bakeryManager
    install manager "grid" grid
    install manager "bakery" bakery
    atomically $ modifyTVar' (dmLoadedMethods manager) (M.insert "co2" (MethodCollection [climate] [] []))
    atomically $ modifyTVar' (dmLoadedMethods manager) (M.insert "model" (MethodCollection [located, climate] [single, circular] []))
    pure manager
  where
    -- Only the grid has German electricity, so only the grid's tables find a located factor.
    -- Declared before 'climate', whose identifier sorts first, and weighing carbon dioxide otherwise.
    located :: Method
    located =
        climate
            { methodId = UUID.fromWords 7 0 0 2
            , methodName = "Climate change, located"
            , methodFactors = [MethodCF co2 "carbon dioxide" MT.Output 3 (Just (MT.Compartment "air" "unspecified" "")) Nothing "kg" Nothing, MethodCF co2 "carbon dioxide" MT.Output 5 (Just (MT.Compartment "air" "unspecified" "")) Nothing "kg" (Just "DE")]
            }

    single :: ScoringSet
    single =
        ScoringSet
            { ssName = "Single"
            , ssUnit = "mPt"
            , ssVariables = M.fromList [("CC", "Climate change"), ("cl", "Climate change, located")]
            , ssComputed = M.fromList [("Both", "CC + 2 * cl")]
            , ssLabels = M.empty
            , ssNormalization = M.fromList [("Both", 4)]
            , ssWeighting = M.fromList [("Both", 1), ("CC", 0.5)]
            , ssScores = M.fromList [("Total", "both + cc")]
            , ssDisplayMultiplier = Just 1000
            , ssUnits = M.empty
            , ssOrigin = DeclaredInConfig
            }

    -- The engine leaves out a set it cannot compute; so must the model.
    circular :: ScoringSet
    circular = single{ssName = "Circular", ssComputed = M.fromList [("A", "B + 1"), ("B", "A + 1")], ssScores = M.fromList [("Total", "a")]}

    climate :: Method
    climate =
        Method
            { methodId = UUID.fromWords 7 0 0 1
            , methodName = "Climate change"
            , methodDescription = Nothing
            , methodUnit = "kg CO2 eq"
            , methodCategory = "Climate change"
            , methodMethodology = Nothing
            , methodFactors = [MethodCF co2 "carbon dioxide" MT.Output 1 (Just (MT.Compartment "air" "unspecified" "")) Nothing "kg" Nothing]
            }

-- | The units the fixture states amounts in: the kilogram and the gram.
bakeryUnits :: UnitConfig
bakeryUnits = mergeUnitConfigs [defaultUnitConfig, mkUnitConfig defaultDimensionOrder (M.singleton "g" (UnitDef [if d == "mass" then 1 else 0 | d <- defaultDimensionOrder] 0.001))]

-- | A manager that reads those units.
bakeryManager :: IO DatabaseManager
bakeryManager = do
    manager <- initDatabaseManager defaultConfig NoCache
    atomically $ modifyTVar' (dmLoadedUnitDefs manager) (M.insert "bakery" bakeryUnits)
    pure manager

envOf :: DatabaseManager -> AppEnv
envOf manager =
    AppEnv
        { aeDbManager = manager
        , aeMaxTreeDepth = 5
        , aePassword = Nothing
        , aeHostingConfig = Nothing
        , aeClassificationPresets = []
        , aeDataVersion = Nothing
        , aeUsageLog = Nothing
        , aeReader = Nothing
        }

-- | Run a handler, failing the test on an error status.
answer :: DatabaseManager -> AppM a -> IO a
answer manager handler = runHandler (runApp (envOf manager) handler) >>= either (\e -> fail (show (errHTTPCode e) <> ": " <> BSL.unpack (errBody e))) pure

scoresOf :: LCIABatchResult -> [Double]
scoresOf = map lrScore . lbrResults

processIdText :: (UUID, UUID) -> Text
processIdText (act, prod) = UUID.toText act <> "_" <> UUID.toText prod

triples :: U.Vector SparseTriple -> [(Int, Int, Double)]
triples = sort . map (\(SparseTriple i j v) -> (fromIntegral i, fromIntegral j, v)) . U.toList

pidOf :: Database -> (UUID, UUID) -> IO ProcessId
pidOf db (act, prod) = maybe (fail "process not interned") pure (findProcessId db act prod)

-- | The bread's carbon dioxide, its electricity supplied by the grid.
co2Of :: Database -> Database -> Text -> IO Double
co2Of bakery grid name = do
    solver <- mkSolverFromDb bakery name
    gridSolver <- mkSolverFromDb grid "grid"
    bread <- pidOf bakery (aBread, pBread)
    result <- computeInventoryMatrixWithDepsCached bakeryUnits (mkDepLookupFromMap (M.singleton "grid" (grid, gridSolver))) bakery name solver bread
    either (fail . T.unpack) (pure . M.findWithDefault 0 co2 . csInventory) result

-- | The bakery, linked to the grid the way a load links it, and the grid.
databases :: IO (Database, Database)
databases = do
    grid <- build "grid" techFlows (M.fromList [((aFR, pElectricity), electricity "FR" 1), ((aDE, pElectricity), electricity "DE" 10)]) M.empty
    bakery <- build "bakery" techFlows bakeryActivities bakeryParameters
    let context =
            LinkingContext
                { lcIndexedDatabases = [buildIndexedDatabaseFromDB "grid" emptySynonymDB grid]
                , lcSynonymDB = emptySynonymDB
                , lcUnitConfig = bakeryUnits
                , lcThreshold = defaultLinkingThreshold
                , lcLocationHierarchy = M.empty
                , lcGeographyPolicy = GeoExact
                , lcSupplierAliases = emptyAliasMap
                }
        links = cdlLinks (findAllCrossDBLinks context techFlows M.empty units bakeryActivities)
    pure (bakery{dbCrossDBLinks = links}, grid)

build :: Text -> M.Map UUID TechnosphereFlow -> M.Map (UUID, UUID) Activity -> M.Map Text Parameter -> IO Database
build name flows acts parameters =
    either (\e -> fail (T.unpack name <> ": " <> T.unpack e)) pure
        =<< buildDatabaseWithMatrices
            (BuildInputs bakeryUnits M.empty Declared [])
            SimpleDatabase
                { sdbActivities = acts
                , sdbTechFlows = flows
                , sdbBioFlows = M.singleton co2 (BiosphereFlow co2 "carbon dioxide" kg M.empty Nothing Nothing (Just (Compartment Air Nothing)))
                , sdbWasteFlows = M.empty
                , sdbUnits = units
                , sdbDocumentation = noDocumentation
                , sdbParameters = parameters
                }

-- | The activity and product of the chain's link @i@.
link :: Int -> (UUID, UUID)
link i = (UUID.fromWords 9 (fromIntegral i) 0 1, UUID.fromWords 9 (fromIntegral i) 0 2)

chainFlows :: M.Map UUID TechnosphereFlow
chainFlows = M.fromList [(product', TechnosphereFlow product' "link" kg M.empty Nothing Nothing) | i <- [0 .. 129], let product' = snd (link i)]

chainActivities :: M.Map (UUID, UUID) Activity
chainActivities =
    M.fromList
        [ (link i, activity "link" (reference (snd (link i)) : [withFormula (formulaOf "yield") (line (snd (link (i - 1))) 1 Input){techActivityLinkId = Just (fst (link (i - 1)))} | i > 0]))
        | i <- [0 .. 129]
        ]

-- | The bakery's parameters with a few more for the formula check: units, and two that read each other.
withCheckParameters :: Database -> Database
withCheckParameters db =
    db
        { dbParameters =
            M.union
                ( M.fromList
                    [ ("distance", (input 3){parameterUnit = Just "m"})
                    , ("dough_mass", (input 500){parameterUnit = Just "g"})
                    , ("odd_unit", (input 1){parameterUnit = Just "furlongs"})
                    , ("loop_a", parameter (CalculatedParameter "loop_b + 1" Nothing))
                    , ("loop_b", parameter (CalculatedParameter "loop_a * 2" Nothing))
                    ]
                )
                (dbParameters db)
        }

bakeryParameters :: M.Map Text Parameter
bakeryParameters =
    M.fromList
        [ ("yield", input 1)
        , ("ratio", parameter (CalculatedParameter "yield * 4" (Just 4)))
        , ("share_pct", input 100)
        , ("hydration", input 1)
        ]

bakeryActivities :: M.Map (UUID, UUID) Activity
bakeryActivities =
    M.fromList
        [ ((aBread, pBread), (activity "bread" [reference pBread, flourLine, power "FR" "mix_fr" 1, power "DE" "mix_de" 0, emission "smoke" 1]){activityParameters = M.fromList [("mix_fr", input 1), ("mix_de", input 0), ("smoke", input 1)]})
        , ((aFlour, pFlour), activity "flour" [reference pFlour, emission "0.25 / yield" 0.25])
        , ((aCake, pCake), (activity "cake" [reference pCake, emission "ratio" 12]){activityParameters = M.singleton "yield" (input 3)})
        , ((aMill, pMeal), milled pMeal)
        , ((aMill, pBran), milled pBran)
        , ((aJam, pJam), activity "jam" [(reference pJam){techShare = Just DeclaredShare{dsPercent = 100, dsFormula = Just "share_pct"}}])
        , ((aLoaf, pLoaf), activity "loaf" [withFormula (formulaOf "hydration") (reference pLoaf), from aDough (withFormula (formulaOf "1 + hydration") (line pDough 2 Input)), from aBread (line pBread 1 Input), from aBin (withFormula (formulaOf "-0.2 - 0.3 * hydration") (line pWaste (-0.5) Input)), emission "hydration * hydration" 1])
        , ((aDough, pDough), activity "dough" [reference pDough, from aLoaf (withFormula (formulaOf "0.1 * hydration") (line pLoaf 0.1 Input)), (emission "" 0.3){bioFormula = Nothing}])
        , -- A treatment, its reference a negative output.
          ((aBin, pWaste), activity "bin" [line pWaste (-1) ReferenceProduct, (emission "" 2){bioFormula = Nothing}])
        , ((aSandwich, pSandwich), activity "sandwich" [reference pSandwich, (from aBread (line pBread 500 Input)){techUnitId = g}, (line pElectricity 2000 Input){techLocation = "FR", techUnitId = g}, (emission "" 0.1){bioFormula = Nothing}])
        ]
  where
    from supplier ex = ex{techActivityLinkId = Just supplier}
    flourLine = withFormula (formulaOf "2 * yield") (line pFlour 2 Input){techActivityLinkId = Just aFlour}
    power location formula amount = withFormula (formulaOf formula) (line pElectricity amount Input){techLocation = location}
    milled product' = (activity "mill" [reference product', emission "grind" 1]){activityParameters = M.singleton "grind" (input 1)}

electricity :: Text -> Double -> Activity
electricity location amount = (activity "electricity" [reference pElectricity, (emission "" amount){bioFormula = Nothing}]){activityLocation = location}

activity :: Text -> [Exchange] -> Activity
activity name exs =
    Activity
        { activityName = name
        , activityDescription = []
        , activityDocumentation = []
        , activitySynonyms = M.empty
        , activityClassification = M.empty
        , activityLocation = "FR"
        , activityLocationSource = LocationDeclared
        , activityUnit = "kg"
        , exchanges = exs
        , activityParameters = M.empty
        , activityNativeType = Nothing
        , activityNativeId = Nothing
        , activityFormulaCheck = Nothing
        , activityDates = noDates
        }

reference :: UUID -> Exchange
reference product' = line product' 1 ReferenceProduct

line :: UUID -> Double -> TechRole -> Exchange
line product' amount role =
    TechnosphereExchange
        { techFlowId = product'
        , techAmount = amount
        , techUnitId = kg
        , techRole = role
        , techActivityLinkId = Nothing
        , techSupplierClaim = ClaimByProduct
        , techLocation = ""
        , techComment = Nothing
        , techPedigree = Nothing
        , techFormula = Nothing
        , techShare = Nothing
        , techClassification = M.empty
        , techProperties = noProperties
        }

emission :: Text -> Double -> Exchange
emission formula amount =
    BiosphereExchange
        { bioFlowId = co2
        , bioAmount = amount
        , bioUnitId = kg
        , bioDirection = Emission
        , bioLocation = ""
        , bioComment = Nothing
        , bioPedigree = Nothing
        , bioFormula = formulaOf formula
        }

named :: Text -> Text -> Maybe ExchangeFormula
named name formula = Just ExchangeFormula{efName = Just name, efText = formula}

declared :: Double -> Exchange -> Exchange
declared percent ex = ex{techShare = Just DeclaredShare{dsPercent = percent, dsFormula = Nothing}}

formulaOf :: Text -> Maybe ExchangeFormula
formulaOf formula = Just ExchangeFormula{efName = Nothing, efText = formula}

input :: Double -> Parameter
input value = parameter (InputParameter value Nothing)

parameter :: ParameterDefinition -> Parameter
parameter definition = Parameter{parameterDefinition = definition, parameterUnit = Nothing, parameterDescription = ""}

techFlows :: M.Map UUID TechnosphereFlow
techFlows =
    M.fromList
        [ (product', TechnosphereFlow product' name kg M.empty Nothing Nothing)
        | (product', name) <- [(pBread, "bread"), (pFlour, "flour"), (pCake, "cake"), (pMeal, "meal"), (pBran, "bran"), (pJam, "jam"), (pElectricity, "electricity"), (pLoaf, "loaf"), (pDough, "dough"), (pWaste, "waste"), (pSandwich, "sandwich")]
        ]

units :: M.Map UUID Unit
units = M.fromList [(kg, Unit kg "kg" "kg" ""), (g, Unit g "g" "g" "")]

kg, g, co2, aBread, pBread, aFlour, pFlour, aCake, pCake, aMill, pMeal, pBran, aJam, pJam, aFR, aDE, pElectricity, aLoaf, pLoaf, aDough, pDough, aBin, pWaste, aSandwich, pSandwich :: UUID
kg = UUID.fromWords 0 0 0 1
g = UUID.fromWords 0 0 0 3
co2 = UUID.fromWords 0 0 0 2
aBread = UUID.fromWords 1 0 0 1
pBread = UUID.fromWords 1 0 0 2
aFlour = UUID.fromWords 2 0 0 1
pFlour = UUID.fromWords 2 0 0 2
aCake = UUID.fromWords 3 0 0 1
pCake = UUID.fromWords 3 0 0 2
aMill = UUID.fromWords 4 0 0 1
pMeal = UUID.fromWords 4 0 0 2
pBran = UUID.fromWords 4 0 0 3
aJam = UUID.fromWords 5 0 0 1
pJam = UUID.fromWords 5 0 0 2
aFR = UUID.fromWords 6 0 0 1
aDE = UUID.fromWords 6 0 0 2
pElectricity = UUID.fromWords 6 0 0 3
aLoaf = UUID.fromWords 8 0 0 1
pLoaf = UUID.fromWords 8 0 0 2
aDough = UUID.fromWords 8 0 0 3
pDough = UUID.fromWords 8 0 0 4
aBin = UUID.fromWords 8 0 0 5
pWaste = UUID.fromWords 8 0 0 6
aSandwich = UUID.fromWords 8 0 0 7
pSandwich = UUID.fromWords 8 0 0 8
