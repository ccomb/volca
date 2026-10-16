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
) where

import Data.Bifunctor (first)
import Data.Foldable (foldlM, traverse_)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Database.MatrixBuild (withActivities)
import qualified Expr
import Types
import UnitConversion (UnitConfig)

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
    (databaseWide, ownByProcess) <- foldlM (accept db) (M.empty, M.empty) settings
    let candidates =
            S.toList . S.fromList $
                M.keys ownByProcess
                    <> [ pid
                       | not (M.null databaseWide)
                       , (pid, act) <- zip [0 ..] (V.toList (dbActivities db))
                       , any (`M.member` databaseWide) (M.keys (lowered (databaseParametersRead act (dbParameters db))))
                       ]
    changed <- traverse (\pid -> fmap (pid,) <$> restate db databaseWide (M.findWithDefault M.empty pid ownByProcess) pid) candidates
    pure (M.fromList (catMaybes changed))

-- | The database at the settings, or why it has none.
parameterised :: UnitConfig -> Database -> [Setting] -> Either Text Database
parameterised unitConfig db settings =
    first describeParameterRefusal (instantiate db settings) >>= withActivities unitConfig db

type Values = M.Map Text Double

-- | One setting checked and filed, by lowered name, under its database or its process.
accept :: Database -> (Values, M.Map ProcessId Values) -> Setting -> Either ParameterRefusal (Values, M.Map ProcessId Values)
accept db (databaseWide, own) Setting{settingProcess = mPid, settingName = name, settingValue = value} =
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

    fresh :: Maybe Text -> Values -> Either ParameterRefusal ()
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
        let atDefaults = valuesOf act M.empty M.empty
            atSettings = valuesOf act databaseWide own
        traverse_ (shareFixed act atDefaults atSettings) (shareFormulas act)
        amounts <- traverse (amountOf act atDefaults atSettings) (exchanges act)
        pure $
            if any snd amounts
                then Just act{exchanges = map fst amounts}
                else Nothing
  where
    valuesOf :: Activity -> Values -> Values -> Values
    valuesOf act forDatabase forProcess = fst (Expr.settle Expr.Volca known calculated)
      where
        ownParameters, readFromDatabase :: M.Map Text Parameter
        ownParameters = lowered (activityParameters act)
        readFromDatabase = lowered (databaseParametersRead act (dbParameters db))
        known :: Values
        known = M.union (inputs forProcess ownParameters) (inputs forDatabase readFromDatabase)
        calculated :: [(Text, Text)]
        calculated =
            formulas ownParameters
                <> formulas readFromDatabase
                <> [(T.toLower name, text) | ExchangeFormula{efName = Just name, efText = text} <- mapMaybe exchangeFormula (exchanges act)]

    -- The input values, each replaced by its setting when there is one.
    inputs :: Values -> M.Map Text Parameter -> Values
    inputs set = M.mapMaybeWithKey $ \key parameter -> case parameterDefinition parameter of
        InputParameter value _ -> Just (M.findWithDefault value key set)
        CalculatedParameter{} -> Nothing

    formulas :: M.Map Text Parameter -> [(Text, Text)]
    formulas parameters = [(key, formula) | (key, CalculatedParameter formula _) <- M.toList (parameterDefinition <$> parameters)]

    -- The amounts were split by the share at the defaults, so a share the settings move would leave them wrong.
    shareFixed :: Activity -> Values -> Values -> Text -> Either ParameterRefusal ()
    shareFixed act atDefaults atSettings formula
        | Expr.evaluate Expr.SimaPro atDefaults formula == Expr.evaluate Expr.SimaPro atSettings formula = Right ()
        | otherwise = Left (ShareMoves (activityName act) formula)

    amountOf :: Activity -> Values -> Values -> Exchange -> Either ParameterRefusal (Exchange, Bool)
    amountOf act atDefaults atSettings ex = case exchangeFormula ex of
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
