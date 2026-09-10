{-# LANGUAGE OverloadedStrings #-}

{- | The allocation gate: how an activity as its source wrote it becomes the
single-output processes the matrix holds, and what happens to one that
cannot.
-}
module AllocationSpec (spec) where

import Control.Monad ((<=<))
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import API.Types (ActivitySummary (..))
import Database (buildDatabaseWithMatrices)
import Database.Allocation
import Database.Loader (defaultLoadOptions, loadDatabaseWithLocationAliases)
import Database.MatrixBuild (InterningTables (..), buildInterningTables, buildSupplierRefUnits, buildTechTriples)
import Database.Quality (QualityCheck (..), QualityOffender (..), QualityReport (..), qualityReport)
import qualified Service
import Types
import UnitConversion (UnitConfig, UnitDef (..), defaultUnitConfig, mkUnitConfig, ucDimensionOrder, ucUnits)

-- | Amounts of the five products of the Abondance cheese block, in kilograms.
abondance :: NE.NonEmpty StatedAmount
abondance = NE.fromList [StatedAmount{saUnit = "kg", saAmount = q} | q <- [1.0, 5.58318, 1.12527, 0.775791, 0.0686462]]

-- | 'defaultUnitConfig' plus a gram and a megajoule, to have a second mass and a non-mass.
massUnits :: UnitConfig
massUnits =
    mkUnitConfig
        (ucDimensionOrder defaultUnitConfig)
        (M.union (M.fromList [("g", UnitDef mass 0.001), ("MJ", UnitDef energy 1.0)]) (ucUnits defaultUnitConfig))
  where
    mass, energy :: [Int]
    mass = [1, 0, 0, 0, 0, 0, 0, 0]
    energy = [0, 0, 0, 1, 0, 0, 0, 0]

{- | A product of a block as the products table reports it: the summary the
reader sees, and the row it was split from, which is where a mass is read.
-}
product_ :: Text -> Double -> Maybe Double -> (ActivitySummary, Maybe Exchange)
product_ unitName amount declared = (summary, Just row)
  where
    summary :: ActivitySummary
    summary =
        ActivitySummary
            { prsProcessId = ""
            , prsActivityName = ""
            , prsLocation = ""
            , prsProductName = ""
            , prsProductAmount = amount
            , prsProductUnit = unitName
            , prsAllocationPercent = declared
            , prsAllocationFormula = Nothing
            , prsMassAllocationPercent = Nothing
            , prsNativeType = Nothing
            , prsBlock = ""
            , prsBlockProducts = 2
            }

    row :: Exchange
    row = (tech cheeseId amount ReferenceProduct){techUnitId = if unitName == "kg" then kgId else mjId}

kg :: Double -> StatedAmount
kg amount = StatedAmount{saUnit = "kg", saAmount = amount}

-- | Splitting on what the source declares, over the fixture's units.
declaredOn :: Allocating
declaredOn = Allocating{alKey = Declared, alUnitConfig = massUnits, alUnitDB = units}

-- | Splitting on a physical property instead.
byPropertyOn :: AllocationProperty -> Allocating
byPropertyOn prop = declaredOn{alKey = ByProperty prop}

-- | The share the reference exchange of a split process records.
appliedShare :: Activity -> Maybe Double
appliedShare act = listToMaybe [dsPercent d | ex <- exchanges act, exchangeIsReference ex, Just d <- [exchangeDeclaredShare ex]]

-- | The five Abondance quantities as the product rows of one block.
abondanceRows :: NE.NonEmpty Exchange
abondanceRows = NE.map (plainRow . saAmount) abondance

-- | A product row stating nothing but its amount, in kilograms.
plainRow :: Double -> Exchange
plainRow amount = tech cheeseId amount Coproduct

-- | A product row in megajoules, which no mass can be read from.
energyRow :: Double -> Exchange
energyRow amount = (tech cheeseId amount Coproduct){techUnitId = mjId}

-- | A product row declaring one property, stated per unit of the row.
declaring :: AllocationProperty -> Text -> Double -> Double -> Exchange
declaring prop unit perUnit amount = (tech cheeseId amount Coproduct){techProperties = stated}
  where
    stated :: ExchangeProperties
    stated = case prop of
        DryMass -> noProperties{epDryMass = Just (StatedAmount unit perUnit)}
        WetMass -> noProperties{epWetMass = Just (StatedAmount unit perUnit)}

-- | What the products table shows in the mass column for a block.
column :: [(ActivitySummary, Maybe Exchange)] -> [Maybe Double]
column = map prsMassAllocationPercent . Service.withMassAllocationPercent massUnits units

-- | A product whose row declares a wet mass per unit of itself.
declaringWetMass :: Double -> Double -> Maybe Double -> (ActivitySummary, Maybe Exchange)
declaringWetMass perUnit amount declared =
    (fst (product_ "kg" amount declared), Just (declaring WetMass "kg" perUnit amount))

round1 :: Double -> Double
round1 x = fromIntegral (round (x * 10) :: Int) / 10

spec :: Spec
spec = do
    describe "allocate Declared" $ do
        it "splits a block into one process per product, scaled by each declared share" $ do
            let processes = NE.toList (allocate declaredOn block)
            length processes `shouldBe` 3
            -- Each process keeps its own product as the reference, and only it.
            [exchangeFlowId ex | p <- processes, ex <- exchanges p, exchangeIsReference ex] `shouldBe` [cheeseId, wheyId, creamId]
            [length (filter exchangeIsProductOutput (exchanges p)) | p <- processes] `shouldBe` [1, 1, 1]
            -- The shared exchanges follow, scaled by share / 100, in source order.
            [inputAmounts p | p <- processes] `shouldBe` [[5.0], [3.0], [2.0]]
            [bioAmounts p | p <- processes] `shouldBe` [[2.0], [1.2], [0.8]]
            [avoidedAmounts p | p <- processes] `shouldBe` [[0.5], [0.3], [0.2]]

        it "keeps the declared share on each process's reference, for the writer and the wire" $ do
            let processes = NE.toList (allocate declaredOn block)
            map (fmap dsPercent . activityReferenceShare) processes `shouldBe` [Just 50, Just 30, Just 20]
            map (dsFormula <=< activityReferenceShare) processes `shouldBe` [Nothing, Just "Qw*DMw/total*100", Nothing]

        it "names each process's unit after its product, and its category after the product row" $ do
            let processes = NE.toList (allocate declaredOn block)
            map activityUnit processes `shouldBe` ["kg", "MJ", "kg"]
            map (M.lookup "Category" . activityClassification) processes
                `shouldBe` [Just "Food\\Transformation", Just "Animal feed\\Others", Just "Food\\Transformation"]
            -- What the block said beyond the category survives on every process.
            map (M.lookup "Category type" . activityClassification) processes `shouldBe` replicate 3 (Just "material")

        it "applies a single product's share as declared, 0 % included" $ do
            let zero = activity [productRow cheeseId 1.0 ReferenceProduct (Just 0) M.empty, input 10.0]
                half = activity [productRow cheeseId 1.0 ReferenceProduct (Just 51) M.empty, input 10.0]
            concatMap inputAmounts (allocate declaredOn zero) `shouldBe` [0.0]
            concatMap inputAmounts (allocate declaredOn half) `shouldBe` [5.1]

        it "leaves a single product with no share as it is" $ do
            let plain = activity [productRow cheeseId 1.0 ReferenceProduct Nothing M.empty, input 10.0]
            map shape (NE.toList (allocate declaredOn plain)) `shouldBe` [shape plain]

        it "leaves an activity whole when a product output carries no share" $ do
            let noShares = activity [productRow cheeseId 1.0 ReferenceProduct Nothing M.empty, productRow wheyId 2.0 Coproduct Nothing M.empty, input 10.0]
                oneShare = activity [productRow cheeseId 1.0 ReferenceProduct (Just 60) M.empty, productRow wheyId 2.0 Coproduct Nothing M.empty, input 10.0]
            map shape (NE.toList (allocate declaredOn noShares)) `shouldBe` [shape noShares]
            map shape (NE.toList (allocate declaredOn oneShare)) `shouldBe` [shape oneShare]

        it "lets an avoided product through unsplit, scaled with the other shared lines" $ do
            let substituting = activity [productRow cheeseId 1.0 ReferenceProduct (Just 50) M.empty, avoided 2.0]
            concatMap avoidedAmounts (allocate declaredOn substituting) `shouldBe` [1.0]

    {- The question a user of a multi-output block asks first, and the one the
    functional unit used to answer wrongly: a coproduct row states 3 kg where
    its block states 1 kg of cheese, and the matrix column is divided by that
    3, so the score beside it is per kilogram. -}
    describe "the functional unit of a split process" $ do
        it "names one unit of the product, not the amount the block states" $ do
            let processes = NE.toList (allocate declaredOn block)
            map (Service.functionalUnitOf flows units) processes
                `shouldBe` ["1.00 kg of cheese", "1.00 MJ of whey", "1.00 kg of cream"]

    describe "allocate normalises first, as the EcoSpold parsers used to" $ do
        it "drops a zero-amount coproduct the source states no share for" $ do
            let act = activity [productRow cheeseId 1.0 ReferenceProduct Nothing M.empty, productRow wheyId 0.0 Coproduct Nothing M.empty]
            length (concatMap exchanges (allocate declaredOn act)) `shouldBe` 1

        it "keeps a zero-amount product row that declares a share: it is a process of its own" $ do
            let act = activity [productRow cheeseId 1.0 ReferenceProduct (Just 100) M.empty, productRow wheyId 0.0 Coproduct (Just 0) M.empty]
            length (allocate declaredOn act) `shouldBe` 2

        it "promotes the only non-zero output of an activity with no reference" $ do
            let act = activity [productRow wheyId 2.0 Coproduct Nothing M.empty, productRow creamId 0.0 Coproduct Nothing M.empty]
                result = NE.toList (allocate declaredOn act)
            [exchangeFlowId ex | p <- result, ex <- exchanges p, exchangeIsReference ex] `shouldBe` [wheyId]

        it "does not choose between two non-zero outputs" $ do
            let act = activity [productRow wheyId 2.0 Coproduct Nothing M.empty, productRow creamId 1.0 Coproduct Nothing M.empty]
            any exchangeIsReference (concatMap exchanges (allocate declaredOn act)) `shouldBe` False

    describe "asAllocated" $ do
        it "accepts a split process and a single-output activity alike" $ do
            let processes = NE.toList (allocate declaredOn block)
            map (either (const False) (const True) . asAllocated) processes `shouldBe` [True, True, True]
            either (const False) (const True) (asAllocated (activity [productRow cheeseId 1.0 ReferenceProduct Nothing M.empty])) `shouldBe` True

        it "accepts an activity that displaces a product" $
            either (const False) (const True) (asAllocated (activity [productRow cheeseId 1.0 ReferenceProduct Nothing M.empty, avoided 2.0]))
                `shouldBe` True

        it "refuses an activity still carrying a coproduct, counting the outputs without a share" $ do
            refusalOf (activity [productRow cheeseId 1.0 ReferenceProduct Nothing M.empty, productRow wheyId 2.0 Coproduct Nothing M.empty])
                `shouldBe` Just (UnallocatedOutputs 2 2)
            refusalOf (activity [productRow cheeseId 1.0 ReferenceProduct (Just 60) M.empty, productRow wheyId 2.0 Coproduct Nothing M.empty])
                `shouldBe` Just (UnallocatedOutputs 2 1)

        it "refuses an activity with no reference, or with two" $ do
            refusalOf (activity [input 1.0]) `shouldBe` Just (NoSingleReference 0)
            refusalOf (activity [productRow cheeseId 1.0 ReferenceProduct Nothing M.empty, productRow wheyId 1.0 ReferenceProduct Nothing M.empty])
                `shouldBe` Just (NoSingleReference 2)

        it "says what is missing and what would repair it" $ do
            describeRefusal (UnallocatedOutputs 3 3) `shouldSatisfy` T.isInfixOf "3 product outputs, 3 without a declared share"
            describeRefusal (UnallocatedOutputs 3 3) `shouldSatisfy` T.isInfixOf "state a share on every product row"
            describeRefusal (NoSingleReference 0) `shouldSatisfy` T.isInfixOf "no reference exchange"
            describeRefusal (NoSingleReference 2) `shouldSatisfy` T.isInfixOf "2 reference exchanges"

    describe "massShares" $ do
        it "reads the Abondance block the way the mass would, against its declared key" $
            -- Quantities of AGRIBALU000000003100165, whose declared key is dry matter:
            -- cheese 51.4 %, permeate 24.3 %, concentrated whey 17.6 %, whey 4.4 %, cream 2.3 %.
            fmap (map round1 . NE.toList) (massShares massUnits abondance)
                `shouldBe` Right [11.7, 65.3, 13.2, 9.1, 0.8]

        it "converts before summing, so a half-kilo is a third and not almost everything" $
            fmap (map round1 . NE.toList) (massShares massUnits (NE.fromList [kg 1.0, StatedAmount{saUnit = "g", saAmount = 500.0}]))
                `shouldBe` Right [66.7, 33.3]

        it "refuses a block whose product is not stated in a mass" $
            massShares massUnits (NE.fromList [kg 1.0, StatedAmount{saUnit = "MJ", saAmount = 4.0}])
                `shouldBe` Left (NotAMass "MJ")

        it "refuses an amount no share can be read from, rather than dropping it to zero" $ do
            massShares massUnits (NE.fromList [kg 1.0, kg 0.0]) `shouldBe` Left (NonPositiveMass 0.0)
            massShares massUnits (NE.fromList [kg (-1.0)]) `shouldBe` Left (NonPositiveMass (-1.0))

    describe "allocate under a property key" $ do
        it "divides on the property rather than on what the source declares" $ do
            -- Two products of 1 kg and 3 kg, declared 60/40 by their source.
            let block = activity [productRow cheeseId 1.0 ReferenceProduct (Just 60) M.empty, productRow creamId 3.0 Coproduct (Just 40) M.empty, input 10.0]
            concatMap inputAmounts (allocate (byPropertyOn WetMass) block) `shouldBe` [2.5, 7.5]

        it "keeps a product its source cut out of the block at zero" $ do
            -- The residue row declares 0 %: its author took it out of the
            -- block on purpose, and the mass divides between the other two.
            let block =
                    activity
                        [ productRow cheeseId 1.0 ReferenceProduct (Just 60) M.empty
                        , productRow creamId 3.0 Coproduct (Just 40) M.empty
                        , productRow feedId 4.0 Coproduct (Just 0) M.empty
                        , input 10.0
                        ]
            concatMap inputAmounts (allocate (byPropertyOn WetMass) block) `shouldBe` [2.5, 7.5, 0.0]

        it "records on each process the share that was applied to it" $ do
            -- The SimaPro writer divides an exchange by this field to rebuild
            -- the block, so a row still claiming its declared share would
            -- export an inventory divided by the wrong factor.
            let block = activity [productRow cheeseId 1.0 ReferenceProduct (Just 60) M.empty, productRow creamId 3.0 Coproduct (Just 40) M.empty]
            map appliedShare (NE.toList (allocate (byPropertyOn WetMass) block)) `shouldBe` [Just 25.0, Just 75.0]

        it "leaves a process of one product as its source wrote it" $ do
            -- 51 % of a block whose other outputs the file does not carry. A
            -- key answering 100 % would hand this process the whole inventory
            -- its author had cut in half.
            let lone = activity [productRow cheeseId 1.0 ReferenceProduct (Just 51) M.empty, input 10.0]
            concatMap inputAmounts (allocate (byPropertyOn WetMass) lone) `shouldBe` [5.1]
            map appliedShare (NE.toList (allocate (byPropertyOn WetMass) lone)) `shouldBe` [Just 51.0]

        it "keeps a lone product's declared zero at zero" $ do
            -- The same reading from the other end: nothing to divide, and the
            -- source says this process carries none of the block.
            let lone = activity [productRow cheeseId 1.0 ReferenceProduct (Just 0) M.empty, input 10.0]
            concatMap inputAmounts (allocate (byPropertyOn WetMass) lone) `shouldBe` [0.0]

        it "never weighs a row it holds at zero" $ do
            -- The residue is stated in megajoules, which no mass reads. Its
            -- share is zero before its mass is looked at, so demanding one
            -- would refuse the block on the very row the zero rule protects.
            let block =
                    activity
                        [ productRow cheeseId 1.0 ReferenceProduct (Just 60) M.empty
                        , productRow creamId 3.0 Coproduct (Just 40) M.empty
                        , (productRow feedId 4.0 Coproduct (Just 0) M.empty){techUnitId = mjId}
                        , input 10.0
                        ]
            concatMap inputAmounts (allocate (byPropertyOn WetMass) block) `shouldBe` [2.5, 7.5, 0.0]

        it "hands back a block the property cannot divide, for the gate to refuse" $ do
            -- Neither row declares a dry mass, and no amount stands in for one.
            let block = activity [productRow cheeseId 1.0 ReferenceProduct (Just 60) M.empty, productRow creamId 3.0 Coproduct (Just 40) M.empty]
            map (length . exchanges) (NE.toList (allocate (byPropertyOn DryMass) block)) `shouldBe` [2]

    describe "propertyShares" $ do
        it "reads the Abondance block the way its wet mass would, against its declared key" $
            -- The same five quantities and the same answer as massShares gives
            -- them, reached from the exchanges rather than from a summary: the
            -- rows state no property, so their own amounts are their wet mass.
            fmap (map round1 . NE.toList) (propertyShares WetMass units massUnits abondanceRows)
                `shouldBe` Right [11.7, 65.3, 13.2, 9.1, 0.8]

        it "believes a declared mass over the one the amount implies" $
            -- Two lines of 2 kg and 1 kg, the first declaring half a kilo of
            -- wet mass per kilo. On the amounts alone this block would read
            -- 66.7 / 33.3.
            fmap
                (map round1 . NE.toList)
                (propertyShares WetMass units massUnits (NE.fromList [declaring WetMass "kg" 0.5 2.0, plainRow 1.0]))
                `shouldBe` Right [50.0, 50.0]

        it "reads a property stated per unit against the length of its line" $
            -- 614.4 kg of dry matter per m3, on 1 m3 and on 3 m3.
            fmap
                (map round1 . NE.toList)
                (propertyShares DryMass units massUnits (NE.fromList [declaring DryMass "kg" 614.4 1.0, declaring DryMass "kg" 614.4 3.0]))
                `shouldBe` Right [25.0, 75.0]

        it "refuses a dry mass no product states, rather than reading the amount as one" $
            -- A kilo of cheese says nothing about how much of it is water.
            propertyShares DryMass units massUnits (NE.fromList [plainRow 1.0])
                `shouldBe` Left NotStated

        it "refuses a line that neither declares a mass nor is stated in one" $
            propertyShares WetMass units massUnits (NE.fromList [plainRow 1.0, energyRow 4.0])
                `shouldBe` Left (NotAMass "MJ")

        it "refuses a declared mass stated in something that is not a mass" $
            propertyShares WetMass units massUnits (NE.fromList [declaring WetMass "dimensionless" 1.0 2.0])
                `shouldBe` Left (NotAMass "dimensionless")

    describe "withMassAllocationPercent" $ do
        it "fills a block whose source states a share on every product" $
            column [product_ "kg" 1 (Just 60), product_ "kg" 3 (Just 40)]
                `shouldBe` [Just 25, Just 75]

        it "leaves a lone product alone, there being nothing to compare it against" $
            column [product_ "kg" 1 (Just 100)] `shouldBe` [Nothing]

        it "leaves a block whose datasets arrived already allocated alone" $
            -- Each is normalised to one of its own product and states no
            -- share, so the amounts are not one run's joint outputs.
            column [product_ "kg" 1 Nothing, product_ "kg" 1 Nothing]
                `shouldBe` [Nothing, Nothing]

        it "leaves a block whose products are not all a mass alone" $
            column [product_ "kg" 1 (Just 60), product_ "MJ" 3 (Just 40)]
                `shouldBe` [Nothing, Nothing]

        it "reads a declared mass, so the column and an allocation key agree" $
            -- 2 kg declaring half a kilo of wet mass per kilo, beside 1 kg.
            -- On the amounts alone this block would read 66.7 / 33.3, and a
            -- database loaded under `wet mass` would show 50 beside it.
            column [declaringWetMass 0.5 2 (Just 60), product_ "kg" 1 (Just 40)]
                `shouldBe` [Just 50, Just 50]

    describe "the matrix" $ do
        it "gives a refused activity no column, and says why" $ do
            let refused = activity [productRow cheeseId 1.0 ReferenceProduct Nothing M.empty, productRow wheyId 2.0 Coproduct Nothing M.empty, input 10.0]
                consumer = (activity [productRow creamId 1.0 ReferenceProduct Nothing M.empty, (tech cheeseId 3.0 Input){techActivityLinkId = Just refusedActId}]){activityName = "consumer"}
                tables = buildInterningTables (M.fromList [((refusedActId, cheeseId), refused), ((consumerActId, creamId), consumer)])
                refUnits = buildSupplierRefUnits units (itActivities tables)
            case buildTechTriples defaultUnitConfig units tables refUnits of
                Left err -> expectationFailure (T.unpack err)
                Right (triples, warnings) -> do
                    -- Column 0 is the refused activity: nothing in it. Column 1 is
                    -- the consumer, whose input reaches the refused row (0) as usual.
                    [(i, j) | SparseTriple i j _ <- VU.toList triples] `shouldBe` [(0, 1)]
                    length warnings `shouldBe` 1
                    concat warnings `shouldSatisfy` isInfixOfS "is not allocated: 2 product outputs, 2 without a declared share"
            V.length (itActivities tables) `shouldBe` 2

    endToEnd

-- ---------------------------------------------------------------------------
-- Fixture: a cheese block with three products and one of each shared line.
-- ---------------------------------------------------------------------------

block :: Activity
block =
    ( activity
        [ productRow cheeseId 1.0 ReferenceProduct (Just 50) M.empty
        , productRow wheyId 2.0 Coproduct (Just 30) (M.singleton "Category" "Animal feed\\Others")
        , productRow creamId 3.0 Coproduct (Just 20) M.empty
        , input 10.0
        , bio 4.0
        , avoided 1.0
        ]
    )
        { activityClassification = M.fromList [("Category type", "material"), ("Category", "Food\\Transformation")]
        }

productRow :: UUID -> Double -> TechRole -> Maybe Double -> M.Map Text Text -> Exchange
productRow fid amount role share cls =
    (tech fid amount role)
        { techUnitId = if fid == wheyId then mjId else kgId
        , techShare = (\p -> DeclaredShare p (if fid == wheyId then Just "Qw*DMw/total*100" else Nothing)) <$> share
        , techClassification = cls
        }

input :: Double -> Exchange
input amount = tech feedId amount Input

avoided :: Double -> Exchange
avoided amount = tech avoidedId amount AvoidedProduct

tech :: UUID -> Double -> TechRole -> Exchange
tech fid amount role =
    TechnosphereExchange
        { techFlowId = fid
        , techAmount = amount
        , techUnitId = kgId
        , techRole = role
        , techActivityLinkId = Nothing
        , techSupplierClaim = ClaimByProduct
        , techLocation = ""
        , techComment = Nothing
        , techPedigree = Nothing
        , techShare = Nothing
        , techClassification = M.empty
        , techProperties = noProperties
        }

bio :: Double -> Exchange
bio amount =
    BiosphereExchange
        { bioFlowId = co2Id
        , bioAmount = amount
        , bioUnitId = kgId
        , bioDirection = Emission
        , bioLocation = ""
        , bioComment = Nothing
        , bioPedigree = Nothing
        }

activity :: [Exchange] -> Activity
activity exs =
    Activity
        { activityName = "cheese production"
        , activityDescription = []
        , activityDocumentation = []
        , activitySynonyms = M.empty
        , activityClassification = M.empty
        , activityLocation = "FR"
        , activityLocationSource = LocationDeclared
        , activityUnit = "kg"
        , exchanges = exs
        , activityParams = M.empty
        , activityParamExprs = M.empty
        , activityNativeType = Nothing
        , activityNativeId = Just (NativeProcessId "block-1")
        , activityFormulaCheck = Nothing
        }

units :: UnitDB
units = M.fromList [(kgId, Unit kgId "kg" "kg" ""), (mjId, Unit mjId "MJ" "MJ" "")]

flows :: TechFlowDB
flows =
    M.fromList
        [ (fid, TechnosphereFlow fid name kgId M.empty Nothing Nothing)
        | (fid, name) <- [(cheeseId, "cheese"), (wheyId, "whey"), (creamId, "cream")]
        ]

inputAmounts, bioAmounts, avoidedAmounts :: Activity -> [Double]
inputAmounts act = [techAmount ex | ex@TechnosphereExchange{techRole = Input} <- exchanges act]
bioAmounts act = [bioAmount ex | ex@BiosphereExchange{} <- exchanges act]
avoidedAmounts act = [techAmount ex | ex@TechnosphereExchange{techRole = AvoidedProduct} <- exchanges act]

-- | What a test compares two activities on: their exchanges' flows, roles and amounts.
shape :: Activity -> [(UUID, TechRole, Double)]
shape act = [(techFlowId ex, techRole ex, techAmount ex) | ex@TechnosphereExchange{} <- exchanges act]

refusalOf :: Activity -> Maybe AllocationRefusal
refusalOf = either Just (const Nothing) . asAllocated

isInfixOfS :: String -> String -> Bool
isInfixOfS needle haystack = T.pack needle `T.isInfixOf` T.pack haystack

u :: String -> UUID
u suffix = read ("00000000-0000-0000-0000-0000000000" <> suffix)

cheeseId, wheyId, creamId, feedId, avoidedId, co2Id, kgId, mjId, refusedActId, consumerActId :: UUID
cheeseId = u "01"
wheyId = u "02"
creamId = u "03"
feedId = u "04"
avoidedId = u "05"
co2Id = u "06"
kgId = u "07"
mjId = u "08"
refusedActId = u "a1"
consumerActId = u "a2"

-- ---------------------------------------------------------------------------
-- End to end: an unlinked EcoSpold 2 dataset with a reference product and
-- a coproduct the source declares no share for.
-- ---------------------------------------------------------------------------

endToEnd :: Spec
endToEnd =
    describe "an EcoSpold 2 dataset with an unallocated coproduct, loaded" $ do
        it "loads, reads, is refused a score and is named by the quality report" $
            withTwoOutputDataset $ \(simpleDb, db) -> do
                V.length (dbActivities db) `shouldBe` 1
                -- No column for it: the only tech triples would be its own inputs.
                VU.length (dbTechnosphereTriples db) `shouldBe` 0
                let pidText = processIdToText db 0
                either (const Nothing) (Just . activityName . snd) (Service.resolveActivityAndProcessId db pidText)
                    `shouldBe` Just "Two outputs"
                case Service.resolveScorable db pidText of
                    Left (Service.NotScorable msg) -> T.unpack msg `shouldContain` "2 product outputs, 2 without a declared share"
                    Left other -> expectationFailure ("expected NotScorable, got " ++ show other)
                    Right _ -> expectationFailure "expected a refusal, the activity resolved for scoring"
                map qoDetail (qcOffenders (qrUnallocated (qualityReport "two" simpleDb)))
                    `shouldBe` ["2 product outputs, 2 without a declared share: state a share on every product row, or load the dataset already allocated"]

withTwoOutputDataset :: ((SimpleDatabase, Database) -> IO ()) -> IO ()
withTwoOutputDataset k = withSystemTempDirectory "es2-two-outputs" $ \dir -> do
    BS.writeFile (dir </> "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa_bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb.spold") twoOutputsXml
    loaded <- loadDatabaseWithLocationAliases (defaultLoadOptions defaultUnitConfig) dir
    simpleDb <- either (fail . T.unpack) pure loaded
    built <- buildDatabaseWithMatrices (BuildInputs defaultUnitConfig mempty Declared) simpleDb
    db <- either (fail . T.unpack) pure built
    k (simpleDb, db)

twoOutputsXml :: BS.ByteString
twoOutputsXml =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
    \<ecoSpold xmlns=\"http://www.EcoInvent.org/EcoSpold02\">\n\
    \  <activityDataset>\n\
    \    <activityDescription>\n\
    \      <activity id=\"aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa\" activityNameId=\"two-outputs\">\n\
    \        <activityName xml:lang=\"en\">Two outputs</activityName>\n\
    \      </activity>\n\
    \      <geography geographyId=\"TEST\"><shortname xml:lang=\"en\">TEST</shortname></geography>\n\
    \    </activityDescription>\n\
    \    <flowData>\n\
    \      <intermediateExchange id=\"ref\" unitId=\"unit-kg\" amount=\"1.0\"\n\
    \                           intermediateExchangeId=\"bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb\">\n\
    \        <name xml:lang=\"en\">Cheese</name>\n\
    \        <unitName xml:lang=\"en\">kg</unitName>\n\
    \        <outputGroup>0</outputGroup>\n\
    \      </intermediateExchange>\n\
    \      <intermediateExchange id=\"co\" unitId=\"unit-kg\" amount=\"2.0\"\n\
    \                           intermediateExchangeId=\"cccccccc-cccc-cccc-cccc-cccccccccccc\">\n\
    \        <name xml:lang=\"en\">Whey</name>\n\
    \        <unitName xml:lang=\"en\">kg</unitName>\n\
    \        <outputGroup>2</outputGroup>\n\
    \      </intermediateExchange>\n\
    \      <intermediateExchange id=\"in\" unitId=\"unit-kg\" amount=\"10.0\"\n\
    \                           intermediateExchangeId=\"dddddddd-dddd-dddd-dddd-dddddddddddd\">\n\
    \        <name xml:lang=\"en\">Milk</name>\n\
    \        <unitName xml:lang=\"en\">kg</unitName>\n\
    \        <inputGroup>5</inputGroup>\n\
    \      </intermediateExchange>\n\
    \    </flowData>\n\
    \  </activityDataset>\n\
    \</ecoSpold>\n"
