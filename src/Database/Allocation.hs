{-# LANGUAGE OverloadedStrings #-}

{- | From an activity as its source wrote it to the single-output processes
the matrix can hold.

An activity may produce several products. A matrix column carries one
product's share of the inventory, so every activity passes through here
between the parser and the index:

* 'allocate' splits an activity whose product outputs all carry a declared
  share into one process per product, the shared exchanges scaled by that
  share. It never refuses: what it cannot split it hands back unchanged.
* 'asAllocated' is the gate. An activity still carrying a 'Coproduct', or
  not carrying exactly one reference exchange, has no column the matrix could
  fill honestly. The matrix builder, the compute path and the quality report
  all read this one verdict, so a refused activity loads, is inspectable, and
  is refused with the same words everywhere.

The split reproduces what the SimaPro parser used to do per block, with one
rule the parser never had to state: a product row's share is applied as
declared, whether or not the block's shares sum to 100. A single product at
51 % or at 0 % is what SimaPro itself reads from such a block, and what the
SimaPro writer emits for every process split here.
-}
module Database.Allocation (
    Allocating (..),
    AllocationRefusal (..),
    AllocatedActivity,
    ExchangeShares,
    SharedExchange (..),
    noExchangeShares,
    exchangeShares,
    allocatedActivity,
    allocate,
    allocateWith,
    allocateAll,
    asAllocated,
    isCoproduct,
    keptWhole,
    describeRefusal,
    describePropertyRefusal,
    propertyKeyRefusals,
    propertyKeyShares,
    PropertyRefusal (..),
    propertyName,
    productProperty,
    propertyShares,
    massShares,
    revenueShares,
) where

import Control.Applicative ((<|>))
import Data.Containers.ListUtils (nubOrd)
import Data.List (find, partition)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Types
import UnitConversion (UnitConfig, convertUnit)

-- | Why an activity has no honest column in the matrix.
data AllocationRefusal
    = -- | Product outputs of the activity, and how many of them carry no declared share.
      UnallocatedOutputs
        { refusedOutputs :: !Int
        , refusedWithoutShare :: !Int
        }
    | -- | How many reference exchanges the activity carries, where exactly one is needed.
      NoSingleReference !Int
    deriving (Eq, Show)

{- | An activity 'asAllocated' accepted. The constructor is not exported: the
only way to hold one is to have passed the gate.
-}
newtype AllocatedActivity = AllocatedActivity Activity

allocatedActivity :: AllocatedActivity -> Activity
allocatedActivity (AllocatedActivity act) = act

{- | What deciding the shares of one block needs besides the block: the key,
and the two unit tables reading a key needs -- the conversion table a property
is weighed through, and the names this database gives its units.
-}
data Allocating = Allocating
    { alKey :: !AllocationKey
    , alUnitConfig :: !UnitConfig
    , alUnitDB :: !UnitDB
    }

{- | A share for one exchange of one product's process, overriding the
product's own share for that exchange alone: causal allocation, where a
source divides each input and emission between the products on its own
terms. Keyed by the positions of the product and of the exchange in the
activity as its reader built it, because those are what every reader has;
the pairs describe the block, not any one process cut from it, so nothing is
stored on the exchanges. Percentages, like 'DeclaredShare'.
-}
newtype ExchangeShares = ExchangeShares (M.Map SharedExchange Double)

-- | One exchange of one product's process, both by position in the activity.
data SharedExchange = SharedExchange
    { seProduct :: !Int
    , seExchange :: !Int
    }
    deriving (Eq, Ord, Show)

-- | No pair: every exchange follows its product's share.
noExchangeShares :: ExchangeShares
noExchangeShares = ExchangeShares M.empty

exchangeShares :: M.Map SharedExchange Double -> ExchangeShares
exchangeShares = ExchangeShares

{- | Split an activity into one process per product output when every product
carries a declared share; otherwise return it as it is, normalised.

Each process keeps its own product as the reference, in the position the
source gave it, followed by the shared exchanges (inputs, avoided products,
biosphere, waste) scaled by @share / 100@. Its unit is the product's, and its
classification is the activity's with whatever the product row states on top.
A named line becomes a calculated parameter of each process, its formula as
the source wrote it: other formulas read the name as the block's amount, so
the share they are scaled by is not applied to it a second time. The line
itself loses the name, so a new amount set on it leaves those formulas as they
were: they follow the parameter.
'allocateWith' gives single exchanges a share of their own.
-}
allocate :: Allocating -> Activity -> NonEmpty Activity
allocate alloc = allocateWith alloc noExchangeShares

{- | 'allocate', with a share per (product, exchange) pair on top of each
product's share. A pair applies only when the shares come from the source: a
key on a property replaces every declared share, the pairs included.
-}
allocateWith :: Allocating -> ExchangeShares -> Activity -> NonEmpty Activity
allocateWith Allocating{alKey = key, alUnitConfig = unitCfg, alUnitDB = unitDB} (ExchangeShares pairs) act =
    fromMaybe (pure normalised) $ do
        (from, shares) <- plan
        NE.nonEmpty (zipWith (process from) products shares)
  where
    placed :: [(Int, Exchange)]
    placed = case key of
        Unallocated -> deducePrices (normalise act)
        _ -> normalise act

    normalised :: Activity
    normalised = act{exchanges = map snd placed}

    products, shared :: [(Int, Exchange)]
    (products, shared) = partition (exchangeIsProductOutput . snd) placed

    {- Which shares to apply, and whether they replace what the source wrote.

    A key on a property divides a block between its products, so a block of
    one product is not something it can answer. Whatever the source declared
    there -- 51 % of a block whose other outputs the file does not carry -- is
    the only statement in existence, and a key answering 100 % would hand the
    process the whole inventory its author had cut in half. -}
    plan :: Maybe (SharesFrom, [Double])
    plan = case key of
        Declared -> (,) FromSource <$> declaredShares
        ByProperty prop
            | length products < 2 -> (,) FromSource <$> declaredShares
            | otherwise -> (,) (FromProperty prop) <$> propertyKeyShares prop unitCfg unitDB (map snd products)
        -- A lone product's declared share is the same statement as under a
        -- property key: nothing else in the file can stand for the half cut away.
        Unallocated
            | length products < 2 -> (,) FromSource <$> declaredShares
            | otherwise -> Nothing

    declaredShares :: Maybe [Double]
    declaredShares = map dsPercent <$> traverse (exchangeDeclaredShare . snd) products

    process :: SharesFrom -> (Int, Exchange) -> Double -> Activity
    process from (productAt, productEx) share =
        normalised
            { exchanges = map unnamed (applied from share (asReference productEx) : map (scaled from productAt share) shared)
            , activityParameters = M.union (activityParameters normalised) namedLines
            , activityUnit = getUnitNameForExchange unitDB productEx
            , activityClassification = M.union (exchangeClassification productEx) (activityClassification normalised)
            }

    namedLines :: M.Map Text Parameter
    namedLines =
        M.fromList
            [ (name, Parameter{parameterDefinition = CalculatedParameter text (Just (exchangeAmount ex)), parameterUnit = Just (getUnitNameForExchange unitDB ex), parameterDescription = ""})
            | (_, ex) <- placed
            , Just ExchangeFormula{efName = Just name, efText = text} <- [exchangeFormula ex]
            ]

    unnamed :: Exchange -> Exchange
    unnamed ex = withFormula ((\f -> f{efName = Nothing}) <$> exchangeFormula ex) ex

    scaled :: SharesFrom -> Int -> Double -> (Int, Exchange) -> Exchange
    scaled from productAt share (at, ex) = scaleExchange (exchangeShare / 100) ex
      where
        exchangeShare :: Double
        exchangeShare = case from of
            FromSource -> M.findWithDefault share SharedExchange{seProduct = productAt, seExchange = at} pairs
            FromProperty _ -> share

    {- The share a split process records is the one that was applied to it.
    Where that is what the row already says, the row is left alone, formula
    included. Where a property replaced it, the row's percentage and its
    formula both describe the key the source chose and neither is true any
    more: a database whose inventory was divided one way while its rows claim
    another exports divided by the wrong factor. -}
    applied :: SharesFrom -> Double -> Exchange -> Exchange
    applied from share ex = case from of
        FromSource -> ex
        FromProperty prop -> withShare prop share ex

    withShare :: AllocationProperty -> Double -> Exchange -> Exchange
    withShare prop share ex = case ex of
        TechnosphereExchange{} -> ex{techShare = Just (DeclaredShare{dsPercent = share, dsFormula = Nothing, dsBy = Just prop})}
        BiosphereExchange{} -> ex
        WasteExchange{} -> ex

{- | Give each product row of a block a price deduced from its declared share,
so that dividing the block by price gives the declared shares back: a row of
51 % on 3 kg is priced 17 per kilo, and brings in the 51 it was declared.

Only a block whose every product row declares a share and states a non-zero
amount gets any: one row left out would make the others' prices compare with
nothing, and the price key then refuses the block by name instead. A row that
already states a price keeps it, and the price key refuses to compare it with
the deduced ones beside it.
-}
deducePrices :: [(Int, Exchange)] -> [(Int, Exchange)]
deducePrices placed
    | all deducible products = map (fmap priced) placed
    | otherwise = placed
  where
    products :: [Exchange]
    products = filter exchangeIsProductOutput (map snd placed)

    deducible :: Exchange -> Bool
    deducible ex = isJust (exchangeDeclaredShare ex) && exchangeAmount ex /= 0

    priced :: Exchange -> Exchange
    priced ex = case (ex, exchangeDeclaredShare ex) of
        (TechnosphereExchange{techProperties = props}, Just share)
            | exchangeIsProductOutput ex
            , isNothing (epPrice props) ->
                ex{techProperties = props{epPrice = Just (DeducedFromShare (dsPercent share / exchangeAmount ex))}}
        _ -> ex

-- | Whether the shares a block was split on replace what its source declared.
data SharesFrom
    = FromSource
    | FromProperty !AllocationProperty
    deriving (Eq, Show)

{- | The shares a property key gives the products of one block, keeping a
declared zero at zero.

A product whose source declares exactly 0 % carries none of the load, and the
key divides what is left among the others. That zero is a modelling decision --
an author who cut a residue out of the block on purpose -- and recomputing it
into a share would undo the decision without saying so. Every other declared
share is replaced, which is the whole point of naming another key.

A row held at zero is also never weighed. Its mass decides nothing, so
demanding one would refuse the block on exactly the rows the rule exists to
protect: a residue stated in cubic metres beside two products in kilograms
would cost the whole block its column.

'Nothing' where the property cannot decide, because 'allocate' never refuses:
the block is handed back whole and the gate says why it has no column.
-}
propertyKeyShares :: AllocationProperty -> UnitConfig -> UnitDB -> [Exchange] -> Maybe [Double]
propertyKeyShares prop cfg unitDB products = do
    weighed <- NE.nonEmpty [numbered | numbered <- zip [0 :: Int ..] products, not (declaresZero (snd numbered))]
    shares <- either (const Nothing) Just (propertyShares prop unitDB cfg (NE.map snd weighed))
    let placed = M.fromList (zip (map fst (NE.toList weighed)) (NE.toList shares))
    -- The rows absent from 'placed' are the ones held at zero, so the default
    -- is the rule rather than a stand-in for a lookup that failed.
    pure [M.findWithDefault 0 i placed | i <- [0 .. length products - 1]]
  where
    declaresZero :: Exchange -> Bool
    declaresZero ex = (dsPercent <$> exchangeDeclaredShare ex) == Just 0

-- | Why a property could not decide the shares of a block, in one sentence.
describePropertyRefusal :: AllocationProperty -> PropertyRefusal -> Text
describePropertyRefusal prop refusal = propertyName prop <> ": " <> because
  where
    because :: Text
    because = case refusal of
        NotStated -> "no product of the block states one, and no amount stands in for it"
        NotAMass unit -> unit <> " is not a mass"
        NonPositiveMass amount -> "a product weighs " <> T.pack (show amount)
        PricesNotComparable units -> "prices in " <> T.intercalate " and " units <> " cannot be compared"
        NegativeRevenue amount -> "a product brings in " <> T.pack (show amount)
        NothingSold -> "every product sells for nothing"

{- | The blocks a property key could not divide, named, with the reason.

'allocate' hands such a block back whole, and the gate then refuses it in the
words it keeps for a block missing declared shares -- advice its author cannot
follow, because every share is stated and none of them is the problem. The
reason lives here instead, to be said once by the load that applied the key.
-}
propertyKeyRefusals :: Allocating -> [Activity] -> [(Text, PropertyRefusal)]
propertyKeyRefusals Allocating{alKey = key, alUnitConfig = cfg, alUnitDB = unitDB} activities = case key of
    Declared -> []
    Unallocated -> []
    ByProperty prop -> [(activityName act, refusal) | act <- activities, Left refusal <- [weigh prop act]]
  where
    -- A block of one product is not something a property key answers, so it
    -- is not something it can refuse either.
    weigh :: AllocationProperty -> Activity -> Either PropertyRefusal (NonEmpty Double)
    weigh prop act = case NE.nonEmpty (filter exchangeIsProductOutput (exchanges act)) of
        Just block | NE.length block > 1 -> propertyShares prop unitDB cfg block
        _ -> Right (pure 100)

-- | 'allocate' over a parsed list, in order.
allocateAll :: Allocating -> [Activity] -> [Activity]
allocateAll alloc = concatMap (NE.toList . allocate alloc)

{- | The gate: the activity has no 'Coproduct' left and exactly one reference
exchange.
-}
asAllocated :: Activity -> Either AllocationRefusal AllocatedActivity
asAllocated act
    | any isCoproduct products =
        Left (UnallocatedOutputs (length products) (length (filter (isNothing . exchangeDeclaredShare) products)))
    | references /= 1 = Left (NoSingleReference references)
    | otherwise = Right (AllocatedActivity act)
  where
    products :: [Exchange]
    products = filter exchangeIsProductOutput (exchanges act)

    references :: Int
    references = length (filter exchangeIsReference (exchanges act))

-- | What is missing, and what would repair it.
describeRefusal :: AllocationRefusal -> Text
describeRefusal refusal = case refusal of
    -- Every product declares a share and the block is still whole: the key the
    -- database was read under kept it so, which a missing share cannot repair.
    UnallocatedOutputs outputs 0 ->
        T.pack (show outputs)
            <> " product outputs kept whole by the allocation key this database was read under:"
            <> " a database derived from it under another key divides them"
    UnallocatedOutputs outputs withoutShare ->
        T.pack (show outputs)
            <> " product outputs, "
            <> T.pack (show withoutShare)
            <> " without a declared share: state a share on every product row, or load the dataset already allocated"
    NoSingleReference 0 -> "no reference exchange: one product output must be the reference"
    NoSingleReference k -> T.pack (show k) <> " reference exchanges where exactly one is needed"

-- | Why a property cannot serve as the key of a block.
data PropertyRefusal
    = -- | A product of the block states nothing of the property, and nothing stands in for it.
      NotStated
    | -- | The unit a product is stated in, which is not a mass.
      NotAMass !Text
    | -- | The amount a product states, which no share can be read from.
      NonPositiveMass !Double
    | {- | The units the products' prices are in, when they are not all one:
      two currencies, two years of one, or a price deduced from a share beside
      one in money.
      -}
      PricesNotComparable ![Text]
    | -- | What a product brings in, below zero.
      NegativeRevenue !Double
    | -- | Every product of the block brings in nothing.
      NothingSold
    deriving (Eq, Show)

{- | The unit a price deduced from a share is counted in, where a stated price
names its currency. It is a sentence rather than a code so that a refusal
naming it beside @EUR2005@ says what the two are, and so that no currency a
source writes can be mistaken for it.
-}
deducedUnit :: Text
deducedUnit = "deduced from a declared share"

{- | The quantity one product line carries of a property.

For a price, that quantity is what the line brings in: its amount times its
price, in the unit of the price.

The source's own statement wins, in the unit the source writes it in. Where
the source states none, a line already written in a mass unit is its own wet
mass: that is the same fact read from the amount column instead of from a
property table, and it is what makes a wet-mass key computable on the formats
that have no property table at all.

A dry mass is never read from an amount. Nothing in a wet kilogram says how
much of it is water, so a block whose products declare none is refused rather
than divided on a number that measures something else.
-}
productProperty :: AllocationProperty -> UnitDB -> Exchange -> Maybe StatedAmount
productProperty prop unitDB ex = case prop of
    DryMass -> perUnit (epDryMass (exchangeProperties ex))
    WetMass -> perUnit (epWetMass (exchangeProperties ex)) <|> ownAmount
    Price -> revenue <$> epPrice (exchangeProperties ex)
  where
    -- What the line brings in, in the unit its price is in.
    revenue :: Price -> StatedAmount
    revenue price = case price of
        StatedPrice stated -> stated{saAmount = saAmount stated * exchangeAmount ex}
        DeducedFromShare amount -> StatedAmount{saUnit = deducedUnit, saAmount = amount * exchangeAmount ex}

    -- A property is stated per unit of the line, so the line's own amount is
    -- what turns it into a quantity.
    perUnit :: Maybe StatedAmount -> Maybe StatedAmount
    perUnit stated = (\s -> s{saAmount = saAmount s * exchangeAmount ex}) <$> stated

    ownAmount :: Maybe StatedAmount
    ownAmount = Just StatedAmount{saUnit = getUnitNameForExchange unitDB ex, saAmount = exchangeAmount ex}

{- | The share each product of one block carries under a property key, as
percentages in the order given.

A mass is converted to kilograms first, for the reason 'massShares' gives, and
the same refusals apply on top of the property not being stated at all. A
price is divided as 'revenueShares' says. Nothing here knows about the shares
the source declares: this is what the property says, and combining the two is
the caller's business.
-}
propertyShares ::
    AllocationProperty ->
    UnitDB ->
    UnitConfig ->
    NonEmpty Exchange ->
    Either PropertyRefusal (NonEmpty Double)
propertyShares prop unitDB cfg products =
    divide =<< maybe (Left NotStated) Right (traverse (productProperty prop unitDB) products)
  where
    divide :: NonEmpty StatedAmount -> Either PropertyRefusal (NonEmpty Double)
    divide = case prop of
        DryMass -> massShares cfg
        WetMass -> massShares cfg
        Price -> revenueShares

{- | The share each product of one block carries of what the block brings in,
as percentages in the order given.

No currency is converted into another, so every line has to be in the same
unit, and a price deduced from a share is a unit of its own. A product that
brings in nothing carries nothing, which is how a block whose author put a
residue at 0 % keeps it there once its prices are deduced; but at least one
product has to bring in something, or there is nothing to divide.
-}
revenueShares :: NonEmpty StatedAmount -> Either PropertyRefusal (NonEmpty Double)
revenueShares revenues
    | length units > 1 = Left (PricesNotComparable units)
    | Just negative <- find (< 0) amounts = Left (NegativeRevenue negative)
    | total <= 0 = Left NothingSold
    | otherwise = Right ((* (100 / total)) <$> amounts)
  where
    units :: [Text]
    units = nubOrd (map saUnit (NE.toList revenues))

    amounts :: NonEmpty Double
    amounts = saAmount <$> revenues

    total :: Double
    total = sum amounts

{- | The share each product of one source block would carry if the key were
its mass, as percentages in the order given.

This is not an 'AllocationKey'. Nothing is split and nothing is scored: it
answers what the mass would say beside what the source declared, and the
reader draws the comparison. An impact per kilo is the quotient of the two
fractions, so the whole comparison follows from these numbers alone.

Every amount is converted to kilograms first, because summing amounts as
written would hand the half-kilo of a 1 kg / 500 g pair 99.8 % of the load.
A unit that is not a mass, or an amount at or below zero, refuses the whole
block rather than dropping one product to a silent zero.
-}
massShares :: UnitConfig -> NonEmpty StatedAmount -> Either PropertyRefusal (NonEmpty Double)
massShares cfg products = share <$> traverse mass products
  where
    mass :: StatedAmount -> Either PropertyRefusal Double
    mass stated
        | saAmount stated <= 0 = Left (NonPositiveMass (saAmount stated))
        | otherwise =
            maybe (Left (NotAMass (saUnit stated))) Right $
                convertUnit cfg (saUnit stated) "kg" (saAmount stated)

    -- Every mass is strictly positive, so their total is too.
    share :: NonEmpty Double -> NonEmpty Double
    share masses = (* (100 / sum masses)) <$> masses

{- | Read a dataset for what it makes, before it is allocated.

A zero-amount product row the source states no share for is not an output:
a coproduct, and a reference beside another reference that states an amount.
An EcoSpold 2 file written for one product of a multi-output activity lists
every product of that activity as a reference and zeroes all but its own.
Kept, those zeros would leave the process with several references, which the
gate refuses, and hand its consumers the unit of whichever came first. An
activity with no reference but one non-zero product output is that product's
process.

Each exchange keeps the position its reader gave it, which is what a
per-exchange share is keyed by.
-}
normalise :: Activity -> [(Int, Exchange)]
normalise act = promote (filter (keep . snd) (zip [0 ..] (exchanges act)))
  where
    keep :: Exchange -> Bool
    keep ex = case ex of
        TechnosphereExchange{techRole = Coproduct, techAmount = amount, techShare = Nothing} -> amount /= 0
        TechnosphereExchange{techRole = ReferenceProduct, techAmount = amount, techShare = Nothing} -> amount /= 0 || not makesSomething
        TechnosphereExchange{} -> True
        BiosphereExchange{} -> True
        WasteExchange{} -> True

    -- Only beside a reference that states an amount is one at zero listed rather than made.
    makesSomething :: Bool
    makesSomething = any (\ex -> exchangeIsReference ex && exchangeAmount ex /= 0) (exchanges act)

    promote :: [(Int, Exchange)] -> [(Int, Exchange)]
    promote placed
        | any (exchangeIsReference . snd) placed = placed
        | otherwise = case filter ((/= 0) . exchangeAmount) (filter exchangeIsProductOutput (map snd placed)) of
            [single] -> map (fmap (promoteTo single)) placed
            _ -> placed

    promoteTo :: Exchange -> Exchange -> Exchange
    promoteTo single ex
        | exchangeFlowId ex == exchangeFlowId single && exchangeIsProductOutput ex = asReference ex
        | otherwise = ex

{- | Whether a block was kept whole although every product declares a share:
the key it was read under kept it so, since a missing share is the only thing
that leaves a block whole under @declared@.
-}
keptWhole :: Activity -> Bool
keptWhole act = any isCoproduct products && all (isJust . exchangeDeclaredShare) products
  where
    products :: [Exchange]
    products = filter exchangeIsProductOutput (exchanges act)

isCoproduct :: Exchange -> Bool
isCoproduct ex = case ex of
    TechnosphereExchange{techRole = role} -> role == Coproduct
    BiosphereExchange{} -> False
    WasteExchange{} -> False

asReference :: Exchange -> Exchange
asReference ex = case ex of
    TechnosphereExchange{} -> ex{techRole = ReferenceProduct}
    BiosphereExchange{} -> ex
    WasteExchange{} -> ex
