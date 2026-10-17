{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{- | A database at other values of its parameters.

A process's formulas read its own names first and its database's second, case
aside: the finest level wins, and a database formula a process reads sees the
process's own value of the names it reads. A setting replaces the value of an
input parameter at its level; calculated parameters follow from it.
-}
module Parametric (
    Setting (..),
    ParameterRefusal (..),
    describeParameterRefusal,
    instantiate,
    parameterised,
    databaseParametersRead,
    Moving (..),
    Input (..),
    moving,
    Check (..),
    checkFormula,
) where

import Control.Monad (when)
import Data.Bifunctor (first)
import Data.Foldable (foldlM, traverse_)
import Data.List (nub)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Database.MatrixBuild (findProducer, withActivities)
import qualified Expr
import Types
import UnitConversion (UnitConfig, UnitDef (..), lookupUnitDef)

-- | A value for an input parameter: the process's own, or its database's when no process is named.
data Setting = Setting
    { settingProcess :: !(Maybe ProcessId)
    , settingName :: !Text
    , settingValue :: !Double
    }
    deriving (Eq, Show)

-- | Why a set of values gives no database. The processes are named by their activity name.
data ParameterRefusal
    = -- | No parameter of that name, at the level the setting names.
      UnknownParameter !(Maybe Text) !Text
    | -- | The parameter is a formula; what it reads is what can be set.
      FormulaDefined !(Maybe Text) !Text
    | -- | A process setting for a parameter only the database defines.
      DatabaseWide !Text !Text
    | -- | Two settings for one name at one level.
      SetTwice !(Maybe Text) !Text
    | -- | A process, the flow of one of its exchanges, and why that exchange's formula gives no amount.
      NoAmount !Text !Text !Text
    | -- | A process whose allocation share is a formula the settings move, and that formula.
      ShareMoves !Text !Text
    deriving (Eq, Show)

describeParameterRefusal :: ParameterRefusal -> Text
describeParameterRefusal = \case
    UnknownParameter at name -> "no parameter '" <> name <> "' " <> level at
    FormulaDefined at name -> "parameter '" <> name <> "' " <> level at <> " is a formula: set the parameters it reads instead"
    DatabaseWide process name -> "process '" <> process <> "' does not define '" <> name <> "', its database does: set it without naming the process"
    SetTwice at name -> "parameter '" <> name <> "' " <> level at <> " is set twice"
    NoAmount process flow why -> "process '" <> process <> "', exchange '" <> flow <> "': " <> why
    ShareMoves process formula -> "process '" <> process <> "' is allocated by the formula '" <> formula <> "', which these values change: a share that follows parameters is not computed yet"
  where
    level :: Maybe Text -> Text
    level = maybe "in the database" (\process -> "in process '" <> process <> "'")

{- | The processes whose amounts the settings change, with their new amounts.
Empty when no amount moves. A formula is evaluated at the defaults and with the
settings, and the stored amount is kept where both agree exactly, so an
amount the settings do not reach never moves by the rounding between a formula
and the number its source printed.
-}
instantiate :: Database -> [Setting] -> Either ParameterRefusal (M.Map ProcessId Activity)
instantiate db settings = do
    (databaseWide, ownByProcess) <- foldlM (accept db) (M.empty, M.empty) [(process, name, value) | Setting process name value <- settings]
    changed <- traverse (\pid -> fmap (pid,) <$> restate db databaseWide (M.findWithDefault M.empty pid ownByProcess) pid) (candidates db databaseWide ownByProcess)
    pure (M.fromList (catMaybes changed))

-- | The processes whose formulas may read what is set: those set at their own level, and those that read a database parameter set.
candidates :: Database -> M.Map Text a -> M.Map ProcessId (M.Map Text a) -> [ProcessId]
candidates db databaseWide ownByProcess =
    S.toList . S.fromList $
        M.keys ownByProcess
            <> [ pid
               | not (M.null databaseWide)
               , (pid, act) <- zip [0 ..] (V.toList (dbActivities db))
               , any (`M.member` databaseWide) (M.keys (lowered (databaseParametersRead act (dbParameters db))))
               ]

-- | The database at the settings with the activities that moved in it, or why it has none.
parameterised :: UnitConfig -> Database -> [Setting] -> Either Text (M.Map ProcessId Activity, Database)
parameterised unitConfig db settings =
    first describeParameterRefusal (instantiate db settings) >>= \changed -> (changed,) <$> withActivities unitConfig db changed

type Values = M.Map Text Double

-- | One setting checked and filed, by lowered name, under its database or its process.
accept :: Database -> (M.Map Text a, M.Map ProcessId (M.Map Text a)) -> (Maybe ProcessId, Text, a) -> Either ParameterRefusal (M.Map Text a, M.Map ProcessId (M.Map Text a))
accept db (databaseWide, own) (mPid, name, value) =
    case mPid of
        Nothing -> do
            settable Nothing (dbParameters db)
            fresh Nothing databaseWide
            pure (M.insert key value databaseWide, own)
        Just pid -> do
            let process = maybe "" activityName (getActivity db pid)
                ownParameters = maybe M.empty activityParameters (getActivity db pid)
            case M.lookup key (lowered ownParameters) of
                Nothing
                    | M.member key (lowered (dbParameters db)) -> Left (DatabaseWide process name)
                    | otherwise -> Left (UnknownParameter (Just process) name)
                Just _ -> settable (Just process) ownParameters
            fresh (Just process) (M.findWithDefault M.empty pid own)
            -- The products of one block carry the same parameters, so a value for one is a value for all.
            pure (databaseWide, foldr (\row -> M.insertWith M.union row (M.singleton key value)) own (block pid))
  where
    key :: Text
    key = T.toLower name

    settable :: Maybe Text -> M.Map Text Parameter -> Either ParameterRefusal ()
    settable at parameters = case parameterDefinition <$> M.lookup key (lowered parameters) of
        Nothing -> Left (UnknownParameter at name)
        Just CalculatedParameter{} -> Left (FormulaDefined at name)
        Just InputParameter{} -> Right ()

    block :: ProcessId -> [ProcessId]
    block pid = maybe [pid] NE.toList $ do
        (activityUUID, _) <- dbProcessIdTable db V.!? fromIntegral pid
        M.lookup activityUUID (dbActivityUUIDIndex db)

    fresh :: Maybe Text -> M.Map Text a -> Either ParameterRefusal ()
    fresh at already
        | M.member key already = Left (SetTwice at name)
        | otherwise = Right ()

lowered :: M.Map Text a -> M.Map Text a
lowered = M.mapKeys T.toLower

{- | The process at the settings, when one of its amounts moves. Its own names
shadow the database's it reads; the settings replace input values at their
level.
-}
restate :: Database -> Values -> Values -> ProcessId -> Either ParameterRefusal (Maybe Activity)
restate db databaseWide own pid = case getActivity db pid of
    Nothing -> Right Nothing
    Just act -> do
        let scope = scopeOf db act
            atDefaults = valuesAt scope M.empty M.empty
            atSettings = valuesAt scope databaseWide own
        traverse_ (shareFixed act atDefaults atSettings) (shareFormulas act)
        amounts <- traverse (amountOf db act atDefaults atSettings) (exchanges act)
        pure $
            if any snd amounts
                then Just act{exchanges = map fst amounts}
                else Nothing
  where
    -- The amounts were split by the share at the defaults, so a share the settings move would leave them wrong.
    shareFixed :: Activity -> Values -> Values -> Text -> Either ParameterRefusal ()
    shareFixed act atDefaults atSettings formula
        | Expr.evaluate Expr.SimaPro atDefaults formula == Expr.evaluate Expr.SimaPro atSettings formula = Right ()
        | otherwise = Left (ShareMoves (activityName act) formula)

{- | What a process's formulas read, by lowered name: its own input values, its
database's it reads, and the formulas computed from them, its own and its
database's parameters and its named exchanges.
-}
data Scope = Scope
    { scopeOwn :: !Values
    , scopeDatabase :: !Values
    , scopeCalculated :: ![(Text, Text)]
    }

scopeOf :: Database -> Activity -> Scope
scopeOf db act = scopeWith (activityParameters act) (databaseParametersRead act (dbParameters db)) (namedLines act)

-- | A scope from a process's own parameters, its database's it may read, and its named lines' formulas.
scopeWith :: M.Map Text Parameter -> M.Map Text Parameter -> [(Text, Text)] -> Scope
scopeWith own fromDatabase named =
    Scope
        { scopeOwn = inputs ownParameters
        , scopeDatabase = inputs readFromDatabase
        , scopeCalculated = formulas ownParameters <> formulas readFromDatabase <> named
        }
  where
    ownParameters, readFromDatabase :: M.Map Text Parameter
    ownParameters = lowered own
    readFromDatabase = lowered fromDatabase

    inputs :: M.Map Text Parameter -> Values
    inputs = M.mapMaybe $ \parameter -> case parameterDefinition parameter of
        InputParameter value _ -> Just value
        CalculatedParameter{} -> Nothing

    formulas :: M.Map Text Parameter -> [(Text, Text)]
    formulas parameters = [(key, formula) | (key, CalculatedParameter formula _) <- M.toList (parameterDefinition <$> parameters)]

-- | The formulas of a process's named lines, by lowered name.
namedLines :: Activity -> [(Text, Text)]
namedLines act = [(T.toLower name, text) | ExchangeFormula{efName = Just name, efText = text} <- mapMaybe exchangeFormula (exchanges act)]

-- | Every name of a scope with its value, each input replaced by the setting of its level when there is one.
valuesAt :: Scope -> Values -> Values -> Values
valuesAt scope forDatabase forProcess =
    fst (Expr.settle Expr.Volca (M.union (setBy forProcess (scopeOwn scope)) (setBy forDatabase (scopeDatabase scope))) (scopeCalculated scope))
  where
    setBy :: Values -> Values -> Values
    setBy set = M.mapWithKey (\key value -> M.findWithDefault value key set)

-- | An exchange at the values of its scope, and whether its amount moved from the defaults.
amountOf :: Database -> Activity -> Values -> Values -> Exchange -> Either ParameterRefusal (Exchange, Bool)
amountOf db act atDefaults atSettings ex = case exchangeFormula ex of
    Nothing -> Right (ex, False)
    Just ExchangeFormula{efText = formula} -> do
        value <- first (refusal . Expr.describeRefusal) (Expr.evaluate Expr.Volca atSettings formula)
        if isNaN value || isInfinite value
            then Left (refusal ("the formula gives no finite number: " <> T.pack (show value)))
            else
                pure $
                    if Expr.evaluate Expr.Volca atDefaults formula == Right value
                        then (ex, False)
                        else (withFormula (exchangeFormula ex) (withAmount value ex), True)
  where
    refusal :: Text -> ParameterRefusal
    refusal = NoAmount (activityName act) (maybe "" flowKindName (lookupExchangeFlow db ex))

-- | Where an input of a reduced model's process starts from: its value, or the position of the free parameter it is.
data Input = Fixed !Double | Free !Int
    deriving (Eq, Show)

{- | A process whose amounts move with the free parameters of a reduced model,
as a client computes them again: each input name of its scope, lowered, and
the formulas computed from them, in the order they read each other.
-}
data Moving = Moving
    { movingProcess :: !ProcessId
    , movingInputs :: !(M.Map Text Input)
    , movingBindings :: ![(Text, Text)]
    }
    deriving (Eq, Show)

{- | The processes a root reaches, through any line whose supplier its database
holds whatever the amount, whose amounts these parameters move; each parameter
named at its process's level, or at the database's with no process. Refused as
'instantiate' would refuse some value of them: a formula that gives no amount
at the defaults gives none anywhere, and a share the parameters reach cannot be
told fixed without the values.
-}
moving :: Database -> ProcessId -> [(Maybe ProcessId, Text)] -> Either ParameterRefusal [Moving]
moving db root free = do
    (databaseWide, ownByProcess) <- foldlM (accept db) (M.empty, M.empty) [(process, name, position) | (position, (process, name)) <- zip [0 ..] free]
    catMaybes <$> traverse (movingAt databaseWide ownByProcess) (filter (`S.member` reachable db root) (candidates db databaseWide ownByProcess))
  where
    movingAt :: M.Map Text Int -> M.Map ProcessId (M.Map Text Int) -> ProcessId -> Either ParameterRefusal (Maybe Moving)
    movingAt databaseWide ownByProcess pid = case getActivity db pid of
        Nothing -> Right Nothing
        Just act -> do
            let scope = scopeOf db act
                inputs = M.union (positions (M.findWithDefault M.empty pid ownByProcess) (scopeOwn scope)) (positions databaseWide (scopeDatabase scope))
                freeNames = M.keysSet (M.filter isFree inputs)
                reaches dialect = any (`S.member` freeNames) . namesReached dialect (M.fromList (scopeCalculated scope))
                atDefaults = valuesAt scope M.empty M.empty
            traverse_ (\formula -> when (reaches Expr.SimaPro formula) (Left (ShareMoves (activityName act) formula))) (shareFormulas act)
            traverse_ (amountOf db act atDefaults atDefaults) (exchanges act)
            pure $
                if any (reaches Expr.Volca . efText) (mapMaybe exchangeFormula (exchanges act))
                    then Just (Moving pid inputs (Expr.settledInOrder Expr.Volca (M.union (scopeOwn scope) (scopeDatabase scope)) (scopeCalculated scope)))
                    else Nothing

    positions :: M.Map Text Int -> Values -> M.Map Text Input
    positions set = M.mapWithKey (\key value -> maybe (Fixed value) Free (M.lookup key set))

    isFree :: Input -> Bool
    isFree = \case
        Free _ -> True
        Fixed _ -> False

{- | Every name a formula reads, and those the formulas it reads read in turn,
lowered. A calculated name shadows an input of that name, as in 'Expr.settle'.
-}
namesReached :: Expr.Dialect -> M.Map Text Text -> Text -> S.Set Text
namesReached dialect calculated = go S.empty . names
  where
    names :: Text -> [Text]
    names = map T.toLower . Expr.collectIdentifiers dialect

    go :: S.Set Text -> [Text] -> S.Set Text
    go seen = \case
        [] -> seen
        name : rest
            | S.member name seen -> go seen rest
            | Just formula <- M.lookup name calculated -> go (S.insert name seen) (names formula <> rest)
            | otherwise -> go (S.insert name seen) rest

-- | The processes a root reaches through lines whose supplier its database holds, the root among them.
reachable :: Database -> ProcessId -> S.Set ProcessId
reachable db root = go S.empty [root]
  where
    go :: S.Set ProcessId -> [ProcessId] -> S.Set ProcessId
    go seen = \case
        [] -> seen
        pid : rest
            | S.member pid seen -> go seen rest
            | otherwise -> go (S.insert pid seen) (suppliers pid <> rest)

    suppliers :: ProcessId -> [ProcessId]
    suppliers pid = maybe [] (mapMaybe (findProducer (dbProcessIdLookup db)) . filter (not . isBiosphereExchange) . exchanges) (getActivity db pid)

{- | The database's parameters a process reads and does not define itself:
those its exchange and parameter formulas name, and those the database's own
calculated parameters name in turn. A process's definition of a name replaces
its database's, whatever the case it is spelt in.
-}
databaseParametersRead :: Activity -> M.Map Text Parameter -> M.Map Text Parameter
databaseParametersRead activity databaseWide =
    M.fromList (M.elems (M.restrictKeys byName (reach S.empty (concatMap namesRead formulas <> concatMap shareNamesRead (shareFormulas activity)))))
  where
    own :: M.Map Text Parameter
    own = activityParameters activity
    ownNames :: S.Set Text
    ownNames = S.map T.toLower (M.keysSet own)
    byName :: M.Map Text (Text, Parameter)
    byName = M.fromList [(T.toLower name, (name, parameter)) | (name, parameter) <- M.toList databaseWide]
    formulas :: [Text]
    formulas = map efText (mapMaybe exchangeFormula (exchanges activity)) <> mapMaybe formulaOf (M.elems own)
    formulaOf :: Parameter -> Maybe Text
    formulaOf parameter = case parameterDefinition parameter of
        CalculatedParameter formula _ -> Just formula
        InputParameter{} -> Nothing
    namesRead, shareNamesRead :: Text -> [Text]
    namesRead = map T.toLower . Expr.collectIdentifiers Expr.Volca
    shareNamesRead = map T.toLower . Expr.collectIdentifiers Expr.SimaPro
    reach :: S.Set Text -> [Text] -> S.Set Text
    reach found = \case
        [] -> found
        name : rest
            | S.member name found || S.member name ownNames -> reach found rest
            | Just (_, parameter) <- M.lookup name byName -> reach (S.insert name found) (maybe [] namesRead (formulaOf parameter) <> rest)
            | otherwise -> reach found rest

-- | The allocation shares a process was declared with as formulas, kept as the source wrote them.
shareFormulas :: Activity -> [Text]
shareFormulas act = mapMaybe (dsFormula =<<) (activityDeclaredShares act)

-- | What a formula gives where it would be written, at the defaults.
data Check = Check
    { checkValue :: !Double
    , checkReads :: ![Text] -- Each name once, lowered, in the order the formula uses them
    , checkWarnings :: ![Text]
    }
    deriving (Eq, Show)

{- | A formula read in the scope of a process, or of its database when none is
given, at the defaults: its value and the names it reads, or why it gives
none. Every parameter of the database is in reach, as a process's formula may
start reading one it did not. Written for a line, the formula takes that
line's place, so a name that reads the line back is a cycle; and it is warned
when it is one parameter, or a number times one, whose unit does not convert
to the line's. Nothing is said of products and quotients of units.
-}
checkFormula :: UnitConfig -> Database -> Maybe (Activity, Maybe Exchange) -> Text -> Either Text Check
checkFormula unitCfg db at formula = do
    tree <- first Expr.describeRefusal (Expr.readFormula Expr.Volca formula)
    let (values, unsettled) = Expr.settle Expr.Volca (M.union (scopeOwn scope) (scopeDatabase scope)) (scopeCalculated scope)
    value <- first (explain unsettled) (Expr.evaluate Expr.Volca values formula)
    when (isNaN value || isInfinite value) (Left ("the formula gives no finite number: " <> T.pack (show value)))
    pure Check{checkValue = value, checkReads = nub (map T.toLower (Expr.collectIdentifiers Expr.Volca formula)), checkWarnings = maybe [] (unitWarnings tree) line}
  where
    own :: M.Map Text Parameter
    own = maybe M.empty (activityParameters . fst) at

    -- A process's own names shadow its database's, whatever their case.
    fromDatabase :: M.Map Text Parameter
    fromDatabase = M.withoutKeys (lowered (dbParameters db)) (M.keysSet (lowered own))

    line :: Maybe Exchange
    line = snd =<< at

    scope :: Scope
    scope = scopeWith own fromDatabase (filter ((/= lineName) . Just . fst) (maybe [] (namedLines . fst) at) <> maybe [] (\name -> [(name, formula)]) lineName)

    lineName :: Maybe Text
    lineName = T.toLower <$> (efName =<< exchangeFormula =<< line)

    explain :: [(Text, Expr.Unsettled)] -> Expr.Refusal -> Text
    explain unsettled = \case
        Expr.Unresolved names -> T.intercalate "; " (map (why unsettled) (NE.toList names))
        refusal -> Expr.describeRefusal refusal

    why :: [(Text, Expr.Unsettled)] -> Text -> Text
    why unsettled name = case lookup (T.toLower name) [(T.toLower n, u) | (n, u) <- unsettled] of
        Just cycle'@Expr.Circular{} -> Expr.describeUnsettled cycle'
        Just other -> name <> ": " <> Expr.describeUnsettled other
        Nothing -> "no parameter or named line '" <> name <> "'"

    unitWarnings :: Expr.Formula -> Exchange -> [Text]
    unitWarnings tree ex = case (scaled tree, unitName <$> getUnitForExchange (dbUnits db) ex) of
        (Just name, Just lineUnit)
            | Just unit <- parameterUnit =<< M.lookup (T.toLower name) (M.union (lowered own) fromDatabase)
            , Just (UnitDef{udDimension = a}, UnitDef{udDimension = b}) <- (,) <$> lookupUnitDef unitCfg unit <*> lookupUnitDef unitCfg lineUnit
            , a /= b ->
                ["parameter '" <> name <> "' is in " <> unit <> ", which does not convert to " <> lineUnit <> ", the line's unit"]
        _ -> []

    -- The parameter a formula is, or is a number times.
    scaled :: Expr.Formula -> Maybe Text
    scaled = \case
        Expr.Name name -> Just name
        Expr.Binary Expr.Multiply (Expr.Literal _) (Expr.Name name) -> Just name
        Expr.Binary Expr.Multiply (Expr.Name name) (Expr.Literal _) -> Just name
        _ -> Nothing
