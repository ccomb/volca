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
    restated,
    shareFormulas,
    editedParameters,
    parameterProblems,
    formulaProblems,
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
    | -- | A process, and why one of its parameters no longer has a value.
      NoValue !Text !Text
    deriving (Eq, Show)

describeParameterRefusal :: ParameterRefusal -> Text
describeParameterRefusal = \case
    UnknownParameter at name -> "no parameter '" <> name <> "' " <> level at
    FormulaDefined at name -> "parameter '" <> name <> "' " <> level at <> " is a formula: set the parameters it reads instead"
    DatabaseWide process name -> "process '" <> process <> "' does not define '" <> name <> "', its database does: set it without naming the process"
    SetTwice at name -> "parameter '" <> name <> "' " <> level at <> " is set twice"
    NoAmount process flow why -> "process '" <> process <> "', exchange '" <> flow <> "': " <> why
    ShareMoves process formula -> "process '" <> process <> "' is allocated by the formula '" <> formula <> "', which these values change: a share that follows parameters is not computed yet"
    NoValue process why -> "process '" <> process <> "': " <> why
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

{- | A process after an edit, at the defaults: each line whose formula no
longer gives its amount takes the formula's value, by the same comparison an
import makes, and each calculated parameter its value. Refused when a formula
gives no amount, when a parameter that had a finite value before the edit has none,
and when the edit moves an allocation share, since the amounts were split by
the old one. The process before the edit, in its database then, is read for
those two alone.
-}
restated :: (Database, Activity) -> (Database, Activity) -> Either ParameterRefusal Activity
restated before (db, act) = do
    traverse_ shareKept (shareFormulas act)
    case [(name, unsettled) | (name, unsettled) <- unsettledNow, name `notElem` map fst unsettledBefore] of
        (name, _) : _ -> Left (NoValue (activityName act) (unsettledWhy unsettledNow (Expr.Unresolved (name NE.:| []))))
        [] -> pure ()
    case [name | (name, Parameter{parameterDefinition = CalculatedParameter{}}) <- M.toList (activityParameters act), notFinite values name, not (notFinite valuesBefore name)] of
        name : _ -> Left (NoValue (activityName act) (name <> ": the formula gives no finite number"))
        [] -> pure ()
    restatedLines <- traverse line (exchanges act)
    pure act{exchanges = restatedLines, activityParameters = M.mapWithKey refreshed (activityParameters act)}
  where
    settled :: (Database, Activity) -> (Values, [(Text, Expr.Unsettled)])
    settled (atDb, at) = let scope = scopeOf atDb at in Expr.settle Expr.Volca (M.union (scopeOwn scope) (scopeDatabase scope)) (scopeCalculated scope)

    values, valuesBefore :: Values
    (values, unsettledNow) = settled (db, act)
    (valuesBefore, unsettledBefore) = settled before

    unsettledNow, unsettledBefore :: [(Text, Expr.Unsettled)]

    notFinite :: Values -> Text -> Bool
    notFinite at name = maybe False (\value -> isNaN value || isInfinite value) (M.lookup (T.toLower name) at)

    line :: Exchange -> Either ParameterRefusal Exchange
    line ex = case exchangeFormula ex of
        Nothing -> Right ex
        Just ExchangeFormula{efText = formula} -> do
            value <- first (refusal ex . unsettledWhy unsettledNow) (Expr.evaluate Expr.Volca values formula)
            when (isNaN value || isInfinite value) (Left (refusal ex ("the formula gives no finite number: " <> T.pack (show value))))
            pure (if Expr.agrees (exchangeAmount ex) value then ex else withFormula (exchangeFormula ex) (withAmount value ex))

    refusal :: Exchange -> Text -> ParameterRefusal
    refusal ex = NoAmount (activityName act) (maybe "" flowKindName (lookupExchangeFlow db ex))

    refreshed :: Text -> Parameter -> Parameter
    refreshed name parameter = case parameterDefinition parameter of
        CalculatedParameter formula _ -> parameter{parameterDefinition = CalculatedParameter formula (M.lookup (T.toLower name) values)}
        InputParameter{} -> parameter

    shareKept :: Text -> Either ParameterRefusal ()
    shareKept formula
        | Expr.evaluate Expr.SimaPro valuesBefore formula == Expr.evaluate Expr.SimaPro values formula = Right ()
        | otherwise = Left (ShareMoves (activityName act) formula)

{- | A database's parameters after edits, each one set or removed (Nothing),
and the processes they move, restated as an edit of the process restates it.
A calculated parameter of the database takes the value it settles to among
the database's parameters, and keeps the one it had when it reads a name only
processes define. Refused, naming why: a parameter no activity could carry
either, a removal of a name the database does not define or that a formula
still reads, a process the edits leave without a value, and a calculated
parameter no process reads that settles to none.
-}
editedParameters :: Database -> [(Text, Maybe Parameter)] -> Either [Text] (M.Map Text Parameter, M.Map ProcessId Activity)
editedParameters db edits = case foldl' step (dbParameters db, []) edits of
    (parameters, []) -> do
        let edited = db{dbParameters = M.mapWithKey (refreshed (fst (settledAlone parameters))) parameters}
            restatedReaders = [(pid, act, restated (db, act) (edited, act)) | (pid, act) <- readers edited]
            moved = [(pid, stated) | (pid, act, Right stated) <- restatedReaders, changed act stated]
            readByNoProcess = S.difference editedNames (S.fromList [key | (_, act) <- readers edited, key <- M.keys (lowered (databaseParametersRead act (dbParameters edited)))])
            (_, unsettledAfter) = settledAlone (dbParameters edited)
        case [describeParameterRefusal refusal | (_, _, Left refusal) <- restatedReaders]
            <> [ "the database: " <> unsettledWhy unsettledAfter (Expr.Unresolved (name NE.:| []))
               | (name, _) <- unsettledAfter
               , S.member name readByNoProcess
               , name `notElem` map fst (snd (settledAlone (dbParameters db)))
               ] of
            [] -> Right (dbParameters edited, M.fromList moved)
            errs -> Left errs
    (_, errs) -> Left errs
  where
    editedNames :: S.Set Text
    editedNames = S.fromList (map (T.toLower . fst) edits)

    step :: (M.Map Text Parameter, [Text]) -> (Text, Maybe Parameter) -> (M.Map Text Parameter, [Text])
    step (parameters, errs) = \case
        (name, Just parameter) -> case parameterProblems "the database" parameters [] name parameter of
            [] -> (M.insert name parameter parameters, errs)
            problems -> (parameters, errs <> problems)
        (name, Nothing) -> case [spelt | spelt <- M.keys parameters, T.toLower spelt == T.toLower name] of
            [] -> (parameters, errs <> ["the database has no parameter '" <> name <> "'"])
            spelt : _ -> case readersOf (T.toLower name) parameters of
                [] -> (M.delete spelt parameters, errs)
                some -> (parameters, errs <> ["the parameter '" <> name <> "' is still read by " <> listed some])

    -- What reads a database parameter: the database's other formulas, and the processes that do not define it themselves.
    readersOf :: Text -> M.Map Text Parameter -> [Text]
    readersOf key parameters =
        [ "parameter '" <> other <> "'"
        | (other, Parameter{parameterDefinition = CalculatedParameter formula _}) <- M.toList parameters
        , T.toLower other /= key
        , key `elem` map T.toLower (Expr.collectIdentifiers Expr.Volca formula)
        ]
            <> nub ["process '" <> activityName act <> "'" | act <- V.toList (dbActivities db), M.member key (lowered (databaseParametersRead act parameters))]

    listed :: [Text] -> Text
    listed some = case splitAt 3 some of
        (shown, []) -> T.intercalate ", " shown
        (shown, rest) -> T.intercalate ", " shown <> " and " <> T.pack (show (length rest)) <> " more"

    -- The processes that read an edited name, before the edits or after them.
    readers :: Database -> [(ProcessId, Activity)]
    readers edited =
        [ (pid, act)
        | (pid, act) <- zip [0 ..] (V.toList (dbActivities db))
        , any (readsEdited act) [dbParameters db, dbParameters edited]
        ]

    readsEdited :: Activity -> M.Map Text Parameter -> Bool
    readsEdited act parameters = not (S.disjoint editedNames (M.keysSet (lowered (databaseParametersRead act parameters))))

    settledAlone :: M.Map Text Parameter -> (Values, [(Text, Expr.Unsettled)])
    settledAlone parameters = let scope = scopeWith M.empty parameters [] in Expr.settle Expr.Volca (scopeDatabase scope) (scopeCalculated scope)

    refreshed :: Values -> Text -> Parameter -> Parameter
    refreshed values name parameter = case (parameterDefinition parameter, M.lookup (T.toLower name) values) of
        (CalculatedParameter formula _, Just value) -> parameter{parameterDefinition = CalculatedParameter formula (Just value)}
        _ -> parameter

    changed :: Activity -> Activity -> Bool
    changed act stated = map exchangeAmount (exchanges act) /= map exchangeAmount (exchanges stated) || activityParameters act /= activityParameters stated

{- | A parameter that can be set among others, in a place a sentence names
("this activity", "the database"): one name of the language that no function
takes, the only spelling of it there and not a line's name, since a formula
reads names without case; with a finite value, or a formula that reads.
-}
parameterProblems :: Text -> M.Map Text Parameter -> [Text] -> Text -> Parameter -> [Text]
parameterProblems place existing lineNames name parameter =
    nameCheck
        <> [ "the parameter '" <> name <> "' is already spelt '" <> other <> "' in " <> place
           | other <- M.keys existing
           , other /= name
           , T.toLower other == key
           ]
        <> ["'" <> name <> "' already names a line of " <> place | key `elem` lineNames]
        <> case parameterDefinition parameter of
            InputParameter value _ ->
                ["the value of '" <> name <> "' is " <> T.pack (show value) <> "; it must be a finite number" | isNaN value || isInfinite value]
            CalculatedParameter formula _ -> formulaProblems formula
  where
    key :: Text
    key = T.toLower name
    nameCheck :: [Text]
    nameCheck = case Expr.readFormula Expr.Volca name of
        Right (Expr.Name read')
            | read' == name
            , key `notElem` map T.toLower Expr.functionNames ->
                []
        _ -> ["'" <> name <> "' is not a parameter name: one word of letters, digits and underscores, starting with a letter, and not a function's name"]

formulaProblems :: Text -> [Text]
formulaProblems formula = case Expr.readFormula Expr.Volca formula of
    Left refusal -> ["the formula '" <> formula <> "' does not read: " <> Expr.describeRefusal refusal]
    Right _ -> []

{- | Why a formula gives no value, its unknown names explained by what a scope
could not settle: a cycle said as a cycle, a name settled nowhere as unknown.
-}
unsettledWhy :: [(Text, Expr.Unsettled)] -> Expr.Refusal -> Text
unsettledWhy unsettled = \case
    Expr.Unresolved names -> T.intercalate "; " (map why (NE.toList names))
    refusal -> Expr.describeRefusal refusal
  where
    why :: Text -> Text
    why name = case lookup (T.toLower name) [(T.toLower n, u) | (n, u) <- unsettled] of
        Just cycle'@Expr.Circular{} -> Expr.describeUnsettled cycle'
        Just other -> name <> ": " <> Expr.describeUnsettled other
        Nothing -> "no parameter or named line '" <> name <> "'"

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
    value <- first (unsettledWhy unsettled) (Expr.evaluate Expr.Volca values formula)
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

    unitWarnings :: Expr.Formula -> Exchange -> [Text]
    unitWarnings tree ex = case (scaled tree, unitName <$> getUnitForExchange (dbUnits db) ex) of
        (Just name, Just lineUnit)
            | Just unit <- parameterUnit =<< M.lookup (T.toLower name) (M.union (lowered own) fromDatabase) ->
                case (lookupUnitDef unitCfg unit, lookupUnitDef unitCfg lineUnit) of
                    (Just UnitDef{udDimension = a}, Just UnitDef{udDimension = b})
                        | a /= b -> ["parameter '" <> name <> "' is in " <> unit <> ", which does not convert to " <> lineUnit <> ", the line's unit"]
                        | otherwise -> []
                    (Nothing, _) -> [unknown name unit]
                    (_, Nothing) -> [unknown name lineUnit]
        _ -> []

    unknown :: Text -> Text -> Text
    unknown name unit = "unit '" <> unit <> "' is not in the unit table, so parameter '" <> name <> "' was not compared with the line's unit"

    -- The parameter a formula is, or is a number times.
    scaled :: Expr.Formula -> Maybe Text
    scaled = \case
        Expr.Name name -> Just name
        Expr.Binary Expr.Multiply (Expr.Literal _) (Expr.Name name) -> Just name
        Expr.Binary Expr.Multiply (Expr.Name name) (Expr.Literal _) -> Just name
        Expr.Binary Expr.Divide (Expr.Name name) (Expr.Literal _) -> Just name
        Expr.Unary Expr.Negate inner -> scaled inner
        _ -> Nothing
