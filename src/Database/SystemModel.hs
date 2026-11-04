{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The file that says how a child is divided from its parent: a property
for every block of several products, exceptions giving some of them another,
coproducts left out of the division for what they displace, and parameters of
the parent's processes taken at another value, each with its reason.

@
name = "Agribalyse 4.0, economic allocation"
version = "1.0.0"
description = "Price everywhere, dry matter for cheese making."

[allocation]
default = "price"

[[allocation.exceptions]]
key = "dry mass"
reason = "Dry matter follows the milk, not the price."
classification = { system = "Category", value = "Agricultural\\\\Food\\\\Cheese production" }

[[substitutions]]
coproduct = "\<activity\>_\<product\>"
replaces = "\<activity\>_\<product\>"
ratio = 23.3
reason = "The biogas displaces natural gas of the same heating value."

[[method_parameters]]
name = "A"
value = 1
reason = "Cut-off: recycled material is credited neither on input nor at end of life."
@

A substitution names the coproduct by its block and its product, and the
process it displaces the same way, since that process must be in the parent:
a child is made from the parent and its model alone.

A method parameter is an input parameter of the parent's processes: every
process that declares it takes the value, and its amounts follow, before any
block is divided. It is how a model makes a choice the parent wrote as data,
such as the share of a recycling credit a process claims.

The file is read strictly: a key it does not know is refused, since a rule
written for a later release and skipped here would be a child its author did
not ask for.
-}
module Database.SystemModel (
    parseSystemModel,
    resolveModel,
    onParent,
    OnParent (..),
    Resolution (..),
    Substituted (..),
    NamedSubstitution (..),
    ModelOnParent (..),
    takenProcesses,
    Traced (..),
    Rule (..),
    traceOf,
    ruleText,
    tracedRule,
    severalProducts,
    buys,
) where

import Control.Monad (mfilter, unless, when)
import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import Data.Indexing (groupInOrder)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import TOML (Decoder, TOMLError, Table, Value (..), decode, decodeWith, getArrayOf, getField, getFieldWith, renderTOMLError, tomlDecoder)

import Config (KeySpec (..), getFieldOpt, getFieldOptWith, unknownKeys)
import Parametric (Before (..), databaseParametersRead, describeParameterRefusal, restated)
import Types (
    Activity (..),
    AllocationProperty,
    ClassificationFilter (..),
    ClassificationMatch (..),
    Database (..),
    DeclaredShare (..),
    Exchange (..),
    ModelException (..),
    ModelParameter (..),
    ModelSubstitution (..),
    Parameter (..),
    ParameterDefinition (..),
    ProcessRef (..),
    Ratio (..),
    Reason (..),
    Selector (..),
    SimpleDatabase (..),
    SystemModel (..),
    TechnosphereFlow (..),
    classificationMatches,
    exchangeAmount,
    exchangeDeclaredShare,
    exchangeFlowId,
    exchangeIsInput,
    exchangeIsProductOutput,
    exchangeIsReference,
    parseProcessRef,
    processRefText,
    propertyName,
    reasonText,
    toSimpleDatabase,
 )
import qualified Types

{- | A system model from the text of its file, or every reason it is not one.
The text is kept as written.
-}
parseSystemModel :: Text -> Either Text SystemModel
parseSystemModel text = do
    table <- either (Left . renderTOMLError) Right (decode text :: Either TOMLError Table)
    case unknownKeys modelKeys table of
        [] -> pure ()
        unknown -> Left ("the system model names keys nothing reads: " <> T.intercalate ", " unknown)
    model <- either (Left . renderTOMLError) Right (decodeWith (modelDecoder text) text)
    case [exReason ex | ex <- smExceptions model, exKey ex == smDefault model] of
        [] -> pure ()
        Reason why : _ -> Left ("an exception divides under the default key, so it changes nothing: " <> why)
    -- A formula reads a name without case, so A and a are one parameter.
    case [name | (name, n) <- M.toList (M.fromListWith (+) [(T.toLower (mpName mp), 1 :: Int) | mp <- smParameters model]), n > 1] of
        [] -> pure model
        name : _ -> Left ("the method parameter " <> name <> " is set twice")

modelKeys :: KeySpec
modelKeys =
    keys
        [ plain "name"
        , plain "version"
        , plain "description"
        ,
            ( "allocation"
            , keys
                [ plain "default"
                ,
                    ( "exceptions"
                    , keys [plain "key", plain "reason", plain "processes", ("classification", keys [plain "system", plain "value"])]
                    )
                ]
            )
        , ("substitutions", keys [plain "coproduct", plain "replaces", plain "ratio", plain "reason"])
        , ("method_parameters", keys [plain "name", plain "value", plain "reason"])
        ]
  where
    keys = Accepts . M.fromList
    plain k = (k, Accepts M.empty)

modelDecoder :: Text -> Decoder SystemModel
modelDecoder text = do
    name <- getField "name"
    version <- getField "version"
    description <- getFieldOpt "description"
    (deflt, exceptions) <- getFieldWith allocation "allocation"
    substitutions <- fromMaybe [] <$> getFieldOptWith (getArrayOf substitution) "substitutions"
    parameters <- fromMaybe [] <$> getFieldOptWith (getArrayOf parameter) "method_parameters"
    pure
        SystemModel
            { smName = name
            , smVersion = version
            , smDescription = description
            , smDefault = deflt
            , smExceptions = exceptions
            , smSubstitutions = substitutions
            , smParameters = parameters
            , smText = text
            }
  where
    substitution :: Decoder ModelSubstitution
    substitution = do
        why <- T.strip <$> getField "reason"
        reason <- if T.null why then fail "a substitution needs its reason" else pure (Reason why)
        coproduct <- getFieldWith processRef "coproduct"
        replaces <- getFieldWith processRef "replaces"
        ratio <- getFieldWith number "ratio"
        unless (ratio > 0 && not (isInfinite ratio)) $
            fail ("the substitution \"" <> T.unpack why <> "\" needs a ratio above zero, not " <> show ratio)
        pure ModelSubstitution{subCoproduct = coproduct, subReplaces = replaces, subRatio = Ratio ratio, subReason = reason}

    parameter :: Decoder ModelParameter
    parameter = do
        name <- T.strip <$> getField "name"
        when (T.null name) (fail "a method parameter needs its name")
        why <- T.strip <$> getField "reason"
        reason <- if T.null why then fail ("the method parameter " <> T.unpack name <> " needs its reason") else pure (Reason why)
        value <- getFieldWith number "value"
        when (isNaN value || isInfinite value) $
            fail ("the method parameter " <> T.unpack name <> " needs a finite value, not " <> show value)
        pure ModelParameter{mpName = name, mpValue = value, mpReason = reason}

    -- TOML tells @2@ from @2.0@; a ratio or a value is either.
    number :: Decoder Double
    number =
        tomlDecoder >>= \case
            Integer n -> pure (fromInteger n)
            Float d -> pure d
            other -> fail ("not a number: " <> show other)

    processRef :: Decoder ProcessRef
    processRef = do
        raw <- tomlDecoder :: Decoder Text
        maybe (fail ("not a process identifier, written <activity>_<product>: " <> T.unpack raw)) pure (parseProcessRef (T.strip raw))

    allocation :: Decoder (AllocationProperty, [ModelException])
    allocation = (,) <$> getFieldWith property "default" <*> (fromMaybe [] <$> getFieldOptWith (getArrayOf exception) "exceptions")

    exception :: Decoder ModelException
    exception = do
        key <- getFieldWith property "key"
        why <- T.strip <$> getField "reason"
        selector <- (,) <$> getFieldOptWith classification "classification" <*> getFieldOpt "processes"
        reason <- if T.null why then fail "an exception needs its reason" else pure (Reason why)
        selects <- case selector of
            (Just c, Nothing) -> pure (ByClassification c)
            (Nothing, Just ids) -> ByProcesses <$> processIds ids
            (Nothing, Nothing) -> fail ("the exception \"" <> T.unpack why <> "\" takes no process: write classification or processes")
            (Just _, Just _) -> fail ("the exception \"" <> T.unpack why <> "\" writes both classification and processes: write one")
        pure ModelException{exKey = key, exReason = reason, exSelects = selects}

    classification :: Decoder ClassificationFilter
    classification = do
        system <- getField "system"
        value <- getField "value"
        pure ClassificationFilter{clfSystem = system, clfValue = value, clfMatch = MatchExact}

    processIds :: [Text] -> Decoder (NonEmpty UUID)
    processIds raw = case partitionEithers [maybe (Left t) Right (UUID.fromText (T.strip t)) | t <- raw] of
        ([], u : us) -> pure (u :| us)
        ([], []) -> fail "processes is empty"
        (bad, _) -> fail ("not process identifiers: " <> T.unpack (T.intercalate ", " bad))

    property :: Decoder AllocationProperty
    property = do
        raw <- tomlDecoder :: Decoder Text
        case Types.parseAllocationKey raw of
            Right (Types.ByProperty prop) -> pure prop
            Right _ -> fail ("a system model divides by price, dry mass or wet mass, not by " <> T.unpack raw)
            Left err -> fail (T.unpack err)

{- | A model read against its parent: the parent's processes with the model's
parameters set, keyed by activity and product as a load keys them, what the
model then does to their blocks, and the processes each parameter moved. A
model's page and its child read this one reading, so a rule one accepts the
other does too.
-}
data OnParent = OnParent
    { opProcesses :: !(M.Map (UUID, UUID) Activity)
    , opResolution :: !Resolution
    , opMoved :: ![(ModelParameter, [(UUID, UUID)])]
    }

onParent :: SystemModel -> Database -> Either (NonEmpty Text) OnParent
onParent model parent = do
    (processes, moved) <- setParameters model parent (sdbActivities (toSimpleDatabase parent))
    resolution <- resolveModel model processes
    pure OnParent{opProcesses = processes, opResolution = resolution, opMoved = moved}

{- | The parent's processes with the model's parameters set, each restated as
an edit of the parameter restates it, and the processes each parameter moved.
A block kept whole is one process, a process of one product under the same
activity another, each with its own parameters.

Refused, naming the process, where a parameter would be skipped in silence: a
process that computes it by a formula, or reads it from its database without
declaring it. Refused too where a process gives no amount at the new value or
would move the share it was cut to, and for a parameter that moves no process.
-}
setParameters :: SystemModel -> Database -> M.Map (UUID, UUID) Activity -> Either (NonEmpty Text) (M.Map (UUID, UUID) Activity, [(ModelParameter, [(UUID, UUID)])])
setParameters model parent processes
    | null (smParameters model) = Right (processes, [])
    | otherwise = case NE.nonEmpty (concat refusals <> idle) of
        Just problems -> Left problems
        Nothing -> Right (M.union (M.fromList [(psKey p, psProcess p) | p <- set]) processes, moved)
  where
    (refusals, set) = partitionEithers (mapMaybe (uncurry setOn) (M.toList processes))

    moved :: [(ModelParameter, [(UUID, UUID)])]
    moved = [(mp, [psKey p | p <- set, mp `elem` psMoving p]) | mp <- smParameters model]

    idle :: [Text]
    idle = [said mp <> " moves no process of the parent: " <> why mp | (mp, []) <- moved]

    why :: ModelParameter -> Text
    why mp
        | any (\p -> mp `elem` psDeclaring p) set = "every process that declares it already has it at that value, or reads it in no amount"
        | otherwise = "none declares it as an input parameter"

    -- A process with every parameter it declares set, and those that moved one of its amounts.
    setOn :: (UUID, UUID) -> Activity -> Maybe (Either [Text] ParameterSet)
    setOn k act = case partitionEithers (mapMaybe (declared act) (smParameters model)) of
        ([], []) -> Nothing
        ([], found) -> Just $ do
            asStated <- restate act []
            stated <- restate act found
            let moving = [mp | f@(mp, _) <- found, either (const True) (moves asStated) (restate act [f])]
            pure ParameterSet{psKey = k, psProcess = stated, psDeclaring = map fst found, psMoving = if moves asStated stated then moving else []}
        (problems, _) -> Just (Left problems)

    -- Measured against the process restated as it is, so an amount its source rounded is not counted as moved.
    moves :: Activity -> Activity -> Bool
    moves before after = map exchangeAmount (exchanges before) /= map exchangeAmount (exchanges after)

    restate :: Activity -> [(ModelParameter, Text)] -> Either [Text] Activity
    restate act found = first (pure . describeParameterRefusal) (restated (Before parent act) parent act{activityParameters = foldr valued (activityParameters act) found})

    -- Under the key as the process spells it; a process spelling it twice is refused by 'declared'.
    valued :: (ModelParameter, Text) -> M.Map Text Parameter -> M.Map Text Parameter
    valued (mp, key) = M.adjust (\p -> p{parameterDefinition = InputParameter (mpValue mp) Nothing}) key

    declared :: Activity -> ModelParameter -> Maybe (Either Text (ModelParameter, Text))
    declared act mp = case spellings of
        [(key, InputParameter{})] -> Just (Right (mp, key))
        [(_, CalculatedParameter{})] -> Just (Left (activityName act <> " computes " <> mpName mp <> " by a formula, which " <> said mp <> " would skip"))
        _ : _ : _ -> Just (Left (activityName act <> " spells " <> mpName mp <> " twice, as " <> T.intercalate " and " [key | (key, _) <- spellings] <> ", which " <> said mp <> " cannot tell apart"))
        []
            -- Only a parent that defines the name can lend it: parse the formulas then, not for every process.
            | any ((== name) . T.toLower) (M.keys (dbParameters parent))
            , any ((== name) . T.toLower) (M.keys (databaseParametersRead act (dbParameters parent))) ->
                Just (Left (activityName act <> " reads " <> mpName mp <> " from its database without declaring it, which " <> said mp <> " would skip"))
            | otherwise -> Nothing
      where
        name = T.toLower (mpName mp)
        spellings = [(key, parameterDefinition p) | (key, p) <- M.toList (activityParameters act), T.toLower key == name]

    said :: ModelParameter -> Text
    said mp = "the method parameter " <> parameterText mp

-- | A parameter as a model sets it: "A = 1 (why)".
parameterText :: ModelParameter -> Text
parameterText mp = mpName mp <> " = " <> valueText (mpValue mp) <> " \"" <> reasonText (mpReason mp) <> "\""
  where
    valueText :: Double -> Text
    valueText v
        | v == fromInteger (round v) = T.pack (show (round v :: Integer))
        | otherwise = T.pack (show v)

{- | What a model does to one parent: the property each block of several
products is divided under, keyed by the block's activity, the blocks each
exception took, and each substitution with the processes it names.
-}

-- | A process of the parent with the model's parameters set: those it declares, and those that moved one of its amounts.
data ParameterSet = ParameterSet
    { psKey :: !(UUID, UUID)
    , psProcess :: !Activity
    , psDeclaring :: ![ModelParameter]
    , psMoving :: ![ModelParameter]
    }

data Resolution = Resolution
    { rsKeys :: !(M.Map UUID AllocationProperty)
    , rsTaken :: ![(ModelException, [UUID])]
    , rsSubstitutions :: ![Substituted]
    }

-- | A substitution as the parent holds it: the block and its coproduct's row, the process displaced and its product's row.
data Substituted = Substituted
    { sdRule :: !ModelSubstitution
    , sdBlock :: !Activity
    , sdCoproduct :: !Exchange
    , sdReplaced :: !Activity
    , sdReplacedProduct :: !Exchange
    }

{- | The model against the parent's blocks, or every way it does not fit:
a process it names that the parent lacks or that makes one product, an
exception that takes no block, a block two exceptions take, a substitution
that would leave a block nothing to carry or credit a process the child lacks.

The parent is given as its processes keyed by activity and product, as a load
keys them.
-}
resolveModel :: SystemModel -> M.Map (UUID, UUID) Activity -> Either (NonEmpty Text) Resolution
resolveModel model parent = case NE.nonEmpty (concatMap problemsOf taken <> overlaps <> concat subProblems <> twice <> emptied) of
    Just problems -> Left problems
    Nothing ->
        Right
            Resolution
                { rsKeys = M.union (M.fromList [(u, exKey ex) | (ex, us) <- taken, u <- us]) (smDefault model <$ blocks)
                , rsTaken = taken
                , rsSubstitutions = substituted
                }
  where
    allActs :: M.Map UUID Activity
    allActs = M.fromList [(activityId, act) | ((activityId, _), act) <- M.toList parent]

    blocks :: M.Map UUID Activity
    blocks = M.filter severalProducts allActs

    taken :: [(ModelException, [UUID])]
    taken = [(ex, takenBy (exSelects ex)) | ex <- smExceptions model]

    takenBy :: Selector -> [UUID]
    takenBy selector = case selector of
        ByClassification f -> M.keys (M.filter (classificationMatches f . activityClassification) blocks)
        ByProcesses ids -> filter (`M.member` blocks) (S.toList (S.fromList (NE.toList ids)))

    problemsOf :: (ModelException, [UUID]) -> [Text]
    problemsOf (ex, us) =
        [quoted ex <> " names " <> UUID.toText u <> ", which the parent " <> lacking u | ByProcesses ids <- [exSelects ex], u <- NE.toList ids, not (M.member u blocks)]
            <> [quoted ex <> " takes no process of several products" | null us]

    lacking :: UUID -> Text
    lacking u = maybe "does not hold" (const "holds with one product only") (M.lookup u allActs)

    overlaps :: [Text]
    overlaps =
        [ activityName (blocks M.! u) <> " is taken by two exceptions: " <> T.intercalate " and " (map quoted exs)
        | (u, exs) <- M.toList (NE.toList <$> groupInOrder [(u, ex) | (ex, us) <- taken, u <- S.toList (S.fromList us)])
        , length exs > 1
        ]

    quoted :: ModelException -> Text
    quoted ex = "the " <> propertyName (exKey ex) <> " exception \"" <> reasonText (exReason ex) <> "\""

    (subProblems, substituted) = partitionEithers (map resolveOne (smSubstitutions model))

    resolveOne :: ModelSubstitution -> Either [Text] Substituted
    resolveOne s = case (producing (subCoproduct s), producing (subReplaces s)) of
        (Just (block, coproduct), Just (replaced, replacedProduct)) -> case problems of
            [] -> Right Substituted{sdRule = s, sdBlock = block, sdCoproduct = coproduct, sdReplaced = replaced, sdReplacedProduct = replacedProduct}
            _ -> Left problems
          where
            problems =
                [said s <> " takes " <> activityName block <> ", a process of one product: nothing would be left to carry its burden" | not (severalProducts block)]
                    <> [said s <> " takes a coproduct of " <> activityName block <> " whose amount is " <> T.pack (show (exchangeAmount coproduct)) <> ": it displaces nothing" | exchangeAmount coproduct <= 0]
                    <> [said s <> " credits " <> activityName replaced <> ", a product of the same block" | prActivity (subReplaces s) == prActivity (subCoproduct s)]
                    <> [said s <> " credits " <> activityName replaced <> ", whose product another substitution takes away" | subReplaces s `S.member` coproducts]
                    <> [said s <> " credits " <> activityName replaced <> ", which buys that coproduct itself: the line would loop" | any (buys (subCoproduct s)) (exchanges replaced)]
        (co, rep) ->
            Left $
                [said s <> " names " <> processRefText (subCoproduct s) <> " as its coproduct, which the parent does not make" | Nothing <- [co]]
                    <> [said s <> " names " <> processRefText (subReplaces s) <> " as the process it displaces, which the parent does not make" | Nothing <- [rep]]

    -- The product row of a process, by activity and product: a block is one entry, a sibling of one product under its activity another.
    producing :: ProcessRef -> Maybe (Activity, Exchange)
    producing (ProcessRef a p) =
        listToMaybe
            [ (act, ex)
            | act <- M.elems (M.takeWhileAntitone ((== a) . fst) (M.dropWhileAntitone ((< a) . fst) parent))
            , ex <- exchanges act
            , exchangeIsProductOutput ex
            , exchangeFlowId ex == p
            ]

    coproducts :: S.Set ProcessRef
    coproducts = S.fromList (map subCoproduct (smSubstitutions model))

    twice :: [Text]
    twice =
        [ processRefText ref <> " is taken by two substitutions: " <> T.intercalate " and " (map said ss)
        | (ref, ss) <- M.toList (NE.toList <$> groupInOrder [(subCoproduct s, s) | s <- smSubstitutions model])
        , length ss > 1
        ]

    emptied :: [Text]
    emptied =
        [ activityName block <> " has every product substituted: nothing is left to carry its burden"
        | (block, flows) <- M.elems (M.fromListWith (\(b, fs) (_, gs) -> (b, S.union fs gs)) [(prActivity (subCoproduct (sdRule sd)), (sdBlock sd, S.singleton (exchangeFlowId (sdCoproduct sd)))) | sd <- substituted])
        , S.size flows >= length (filter exchangeIsProductOutput (exchanges block))
        ]

    said :: ModelSubstitution -> Text
    said s = "the substitution \"" <> reasonText (subReason s) <> "\""

-- | A line of a process buying this product from this process, as the parent linked it.
buys :: ProcessRef -> Exchange -> Bool
buys (ProcessRef a p) ex = case ex of
    TechnosphereExchange{techActivityLinkId = Just link} -> exchangeIsInput ex && exchangeFlowId ex == p && link == a
    _ -> False

-- | A block: the only kind of process a model divides.
severalProducts :: Activity -> Bool
severalProducts act = length (filter exchangeIsProductOutput (exchanges act)) > 1

{- | A substitution with the names a reader checks it by: the block, its
coproduct, the process displaced and its product.
-}
data NamedSubstitution = NamedSubstitution
    { nsRule :: !ModelSubstitution
    , nsBlock :: !Text
    , nsCoproduct :: !Text
    , nsReplaced :: !Text
    , nsReplacedProduct :: !Text
    }

{- | A model read against its parent: each exception with the parent's
processes it takes, by activity and name, each substitution named, and each
method parameter with the parent's processes it moved, by activity and
product and name. What a reader checks a rule against its reason with.
-}
data ModelOnParent = ModelOnParent
    { mopModel :: !SystemModel
    , mopExceptions :: ![(ModelException, [(UUID, Text)])]
    , mopSubstitutions :: ![NamedSubstitution]
    , mopParameters :: ![(ModelParameter, [((UUID, UUID), Text)])]
    }

takenProcesses :: SystemModel -> Database -> Either (NonEmpty Text) ModelOnParent
takenProcesses model parent = named <$> onParent model parent
  where
    simple :: SimpleDatabase
    simple = toSimpleDatabase parent

    named :: OnParent -> ModelOnParent
    named reading =
        ModelOnParent
            { mopModel = model
            , mopExceptions = [(ex, [(u, name) | u <- us, Just name <- [M.lookup u names]]) | (ex, us) <- rsTaken resolution]
            , mopParameters = [(mp, [(k, maybe "" activityName (M.lookup k (sdbActivities simple))) | k <- ks]) | (mp, ks) <- opMoved reading]
            , mopSubstitutions =
                [ NamedSubstitution
                    { nsRule = sdRule sd
                    , nsBlock = activityName (sdBlock sd)
                    , nsCoproduct = flowName (sdCoproduct sd)
                    , nsReplaced = activityName (sdReplaced sd)
                    , nsReplacedProduct = flowName (sdReplacedProduct sd)
                    }
                | sd <- rsSubstitutions resolution
                ]
            }
      where
        resolution = opResolution reading

    names :: M.Map UUID Text
    names = M.fromList [(activityId, activityName act) | ((activityId, _), act) <- M.toList (sdbActivities simple)]

    flowName :: Exchange -> Text
    flowName ex = maybe (UUID.toText (exchangeFlowId ex)) tfName (M.lookup (exchangeFlowId ex) (sdbTechFlows simple))

{- | A process of a child, as its delivery sheet traces it back to the
parent: the activity is the parent's, the product the row it keeps, the
parent's process it was cut from, and the method parameters that moved it.
-}
data Traced = Traced
    { trProcess :: !(UUID, UUID)
    , trParent :: !(UUID, UUID)
    , trActivity :: !Text
    , trProduct :: !Text
    , trShare :: !(Maybe Double)
    , trRule :: !Rule
    , trParameters :: ![ModelParameter]
    }

-- | Why a process of a child carries the share it does.
data Rule
    = -- | A block its key could not weigh, left with all its products.
      KeptWhole !AllocationProperty
    | -- | A piece of a divided block: its key, the exception that chose that key, the substitutions its block also made.
      Divided !AllocationProperty !(Maybe Reason) ![Reason]
    | -- | All a block has left once its other products are substituted: the whole block.
      Credited !(NonEmpty Reason)
    | -- | A share below 100 the source declared on a process of one product.
      DeclaredBySource
    | -- | A process of one product the parent holds whole, which only method parameters moved.
      AsTheParent

ruleText :: Rule -> Text
ruleText = \case
    KeptWhole prop -> "kept whole: its products cannot be weighed by " <> propertyName prop
    Divided prop exception substitutions ->
        propertyName prop
            <> maybe ", default key" ((", exception: " <>) . reasonText) exception
            <> foldMap (("; substitution: " <>) . reasonText) substitutions
    Credited reasons -> T.intercalate "; " (map (("substitution: " <>) . reasonText) (NE.toList reasons))
    DeclaredBySource -> "share declared by the source"
    AsTheParent -> "as the parent has it"

-- | The rule of a traced process, and each method parameter that moved it.
tracedRule :: Traced -> Text
tracedRule t = ruleText (trRule t) <> foldMap (("; parameter " <>) . parameterText) (trParameters t)

{- | Each process of a child that comes from a block of its parent, each
share below 100 the source declared on a process of one product, and each
process a method parameter moved. Any other process is the parent's as it
stands. A block left whole is listed once, under its reference product.

A process comes from a block when its activity is the block's and its product
one of the block's: a process of one product under the same activity is not
the block's.
-}
traceOf :: ModelOnParent -> Database -> Database -> [Traced]
traceOf mop parent child =
    [ Traced{trProcess = k, trParent = parentKey k, trActivity = activityName act, trProduct = productName ref, trShare = share, trRule = rule, trParameters = moving}
    | (k, act) <- M.toList (sdbActivities simple)
    , let moving = M.findWithDefault [] (parentKey k) parametered
    , ref <- take 1 (filter exchangeIsReference (exchanges act))
    , Just (share, rule) <- [orParametered moving (ruled k act (exchangeDeclaredShare ref))]
    ]
  where
    orParametered :: [ModelParameter] -> Maybe (Maybe Double, Rule) -> Maybe (Maybe Double, Rule)
    orParametered moving traced = case (traced, moving) of
        (Nothing, _ : _) -> Just (Just 100, AsTheParent)
        _ -> traced

    parametered :: M.Map (UUID, UUID) [ModelParameter]
    parametered = NE.toList <$> groupInOrder [(k, mp) | (mp, ks) <- mopParameters mop, (k, _) <- ks]

    simple :: SimpleDatabase
    simple = toSimpleDatabase child

    ruled :: (UUID, UUID) -> Activity -> Maybe DeclaredShare -> Maybe (Maybe Double, Rule)
    ruled (activityId, productId) act declared
        | not (fromBlock activityId productId) = (\d -> (Just (dsPercent d), DeclaredBySource)) <$> mfilter ((< 100) . dsPercent) declared
        | severalProducts act = Just (Nothing, KeptWhole (maybe (smDefault (mopModel mop)) exKey exception))
        | Just DeclaredShare{dsPercent = share, dsBy = Just prop} <- declared = Just (Just share, Divided prop (exReason <$> exception) substitutions)
        | otherwise = (\reasons -> (Just 100, Credited reasons)) <$> NE.nonEmpty substitutions
      where
        exception :: Maybe ModelException
        exception = M.lookup activityId exceptions

        substitutions :: [Reason]
        substitutions = M.findWithDefault [] activityId substituted

    -- A process outside every block is the parent's own, kept as it was.
    parentKey :: (UUID, UUID) -> (UUID, UUID)
    parentKey k@(activityId, productId) = case M.lookup activityId blockProducts of
        Just (block, products) | S.member productId products -> block
        _ -> k

    fromBlock :: UUID -> UUID -> Bool
    fromBlock activityId productId = maybe False (S.member productId . snd) (M.lookup activityId blockProducts)

    -- Each block of the parent by its activity, with its own key: a process of
    -- one product may sit under the same activity beside it.
    blockProducts :: M.Map UUID ((UUID, UUID), S.Set UUID)
    blockProducts =
        M.fromList
            [ (activityId, (k, S.fromList (map exchangeFlowId (filter exchangeIsProductOutput (exchanges act)))))
            | (k@(activityId, _), act) <- M.toList (sdbActivities (toSimpleDatabase parent))
            , severalProducts act
            ]

    -- Two exceptions taking one block are refused when the model is read.
    exceptions :: M.Map UUID ModelException
    exceptions = M.fromList [(u, ex) | (ex, taken) <- mopExceptions mop, (u, _) <- taken]

    substituted :: M.Map UUID [Reason]
    substituted = NE.toList <$> groupInOrder [(prActivity (subCoproduct (nsRule ns)), subReason (nsRule ns)) | ns <- mopSubstitutions mop]

    productName :: Exchange -> Text
    productName ex = maybe (UUID.toText (exchangeFlowId ex)) tfName (M.lookup (exchangeFlowId ex) (sdbTechFlows simple))
