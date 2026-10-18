{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The parameters and formulas of a database as a format writes them.

A formula is written when the format reads the text back to the very formula
the engine holds ('Expr.writtenIn'), and reads no parameter the format cannot
name. Otherwise it is frozen: a line keeps its amount, a calculated parameter
becomes an input parameter at its value, and the warnings name it.

A format that names no line (SimaPro, openLCA) gets each line a formula
reads by its name as a calculated parameter of its process under that name,
its formula the line's, and the line reads the parameter. EcoSpold 2 names its
lines, so there a line keeps its name, unless its amount is written otherwise
than the engine holds it (an avoided product, negated): the name would then
give its readers that other amount. A share's formula, kept in SimaPro's
language, follows the same rule.

EcoSpold 2 states an amount beside every formula and has no database-wide
parameter: a calculated parameter with no value cannot be written there, and
each dataset carries the database's parameters it reads ('databaseCopies').

openLCA and Brightway compute the database's parameters among themselves,
before any process's: a database formula reading a name a process defines is
also written among that process's parameters, at the value it settles to there
('ownCopies'). Brightway also leaves out at the database's level one that
settles to no value there.

Each name a formula reads is written as the parameter it resolves to spells
it, the process's own first: Python reads a name in its case.
-}
module Database.ExportFormulas (
    Exported (..),
    Written (..),
    ProcessExport (..),
    processExport,
    Scaled (..),
    exportFormulas,
    databaseCopies,
    formulaWarnings,
    frozenWarnings,
    valuesOnly,
) where

import Data.Either (lefts, rights)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Maybe (isJust, isNothing, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import qualified Expr
import Parametric (databaseParametersIn, namesReached)
import Types

-- | A parameter as a format writes it: a value and its spread, or a formula in the format's language.
data Written
    = WrittenValue !Double !(Maybe Uncertainty)
    | WrittenFormula !Text
    deriving (Eq, Show)

-- | What a writer does to a line's amount, which the line's formula follows.
data Scaled = Times !Double | DividedBy !Double

-- | What a format writes of a database's parameters and formulas.
data Exported = Exported
    { exDatabase :: !(M.Map Text (Parameter, Written))
    -- ^ The database's parameters.
    , exProcesses :: !(M.Map (UUID, UUID) ProcessExport)
    , exFrozen :: ![Text]
    -- ^ Each formula written as its value, or parameter left out, and why.
    , exUnits :: !Int
    -- ^ How many parameters state a unit.
    }

-- | What a format writes of one process's parameters and formulas.
data ProcessExport = ProcessExport
    { peParameters :: !(M.Map Text (Parameter, Written))
    -- ^ The process's own parameters, its named lines among them.
    , peLines :: !(IM.IntMap Text)
    -- ^ The formula of each line written with one, by its position in the activity.
    , peShares :: !(IM.IntMap Text)
    -- ^ The formula of each product's share written with one, by its position.
    , peLineNames :: !(IM.IntMap Text)
    -- ^ The name each line is written with, by its position, in a format that names its lines.
    }

-- | Whether a format names a line other formulas read, or reads it through a parameter of that name.
data LineNames = NamedLines | LinesAsParameters

lineNames :: Expr.Dialect -> LineNames
lineNames = \case
    Expr.EcoSpold2 _ -> NamedLines
    Expr.SimaPro -> LinesAsParameters
    Expr.OpenLca -> LinesAsParameters
    Expr.Volca -> LinesAsParameters
    Expr.Python -> LinesAsParameters

{- | Whether a format cannot write a calculated parameter with no value: it
states an amount beside every formula (EcoSpold 2), or computes the
database's parameters before any process's, where a formula with no value
reads a name no parameter of that level has (Brightway).
-}
leavesOutValueless :: Expr.Dialect -> Bool
leavesOutValueless = \case
    Expr.EcoSpold2 _ -> True
    Expr.Python -> True
    Expr.SimaPro -> False
    Expr.OpenLca -> False
    Expr.Volca -> False

-- | Whether a format computes the database's parameters once, before any process's.
computesDatabaseFirst :: Expr.Dialect -> Bool
computesDatabaseFirst = \case
    Expr.Python -> True
    Expr.OpenLca -> True
    Expr.EcoSpold2 _ -> False
    Expr.SimaPro -> False
    Expr.Volca -> False

-- | What is written of a process, nothing for one the database does not hold.
processExport :: Exported -> (UUID, UUID) -> ProcessExport
processExport exported key = M.findWithDefault (ProcessExport M.empty IM.empty IM.empty IM.empty) key (exProcesses exported)

exportFormulas :: Expr.Dialect -> ((UUID, UUID) -> Exchange -> Scaled) -> SimpleDatabase -> Exported
exportFormulas dialect scaledBy db =
    Exported
        { exDatabase = M.fromList database
        , exProcesses = M.map fst processes
        , exFrozen = databaseFrozen <> concatMap snd (M.elems processes)
        , exUnits = length [() | p <- M.elems (sdbParameters db) <> concatMap (M.elems . activityParameters) (M.elems (sdbActivities db)), isJust (parameterUnit p)]
        }
  where
    databaseUnwritten :: S.Set Text
    databaseUnwritten = unwritable dialect S.empty (sdbParameters db)

    databaseSpellings :: M.Map Text Text
    databaseSpellings = spellings (sdbParameters db)

    database :: [(Text, (Parameter, Written))]
    databaseFrozen :: [Text]
    (database, databaseFrozen) = parametersIn (writtenWith dialect databaseUnwritten . Expr.respelt databaseSpellings) databaseUnwritten "the database" (sdbParameters db)

    processes :: M.Map (UUID, UUID) (ProcessExport, [Text])
    processes = M.mapWithKey process (sdbActivities db)

    -- A process's parameters and formulas as written, and those frozen.
    process :: (UUID, UUID) -> Activity -> (ProcessExport, [Text])
    process key source =
        ( ProcessExport{peParameters = M.fromList parameters, peLines = IM.fromList (rights lines'), peShares = IM.fromList (rights shares), peLineNames = named}
        , parametersFrozen <> lefts lines' <> lefts shares
        )
      where
        -- The process's names: its own parameters, the lines formulas read by name, and the database's it must compute itself.
        viewed :: Activity
        viewed = withOwnCopies (namedLinesAsParameters (const True) (namesReadByDatabase db) source)
        act :: Activity
        act = case lineNames dialect of
            NamedLines -> namedLinesAsParameters (not . asHeld . scaledBy key) (namesReadByDatabase db) source
            LinesAsParameters -> viewed
        withOwnCopies :: Activity -> Activity
        withOwnCopies a
            | computesDatabaseFirst dialect = a{activityParameters = M.union (activityParameters a) (ownCopies db a)}
            | otherwise = a
        unwritten :: S.Set Text
        unwritten = unwritable dialect databaseUnwritten (activityParameters viewed)
        spelt :: M.Map Text Text
        spelt = M.union (spellings (activityParameters viewed)) databaseSpellings
        written :: Text -> Maybe Text
        written = writtenWith dialect unwritten . Expr.respelt spelt
        (parameters, parametersFrozen) = parametersIn written unwritten (activityName act) (activityParameters act)
        taken = S.map T.toLower (M.keysSet (activityParameters source))
        named = case lineNames dialect of
            NamedLines ->
                IM.fromList
                    [ (i, name)
                    | (i, ex) <- zip [0 ..] (exchanges act)
                    , Just ExchangeFormula{efName = Just name} <- [exchangeFormula ex]
                    , S.notMember (T.toLower name) taken
                    , S.notMember (T.toLower name) unwritten
                    ]
            LinesAsParameters -> IM.empty
        lines' =
            [ maybe (Left (activityName act <> " · " <> flowName db ex <> ": " <> text)) (Right . (,) i) (written text)
            | (i, ex) <- zip [0 ..] (exchanges act)
            , Just f <- [exchangeFormula ex]
            , let text = scaled (scaledBy key ex) (efText f)
            ]
        -- A share's formula is kept in SimaPro's language, as it was read.
        shares =
            [ maybe (Left (activityName act <> " · share of " <> flowName db ex <> ": " <> f)) (Right . (,) i) (written =<< either (const Nothing) Just (Expr.translate Expr.SimaPro f))
            | (i, ex) <- zip [0 ..] (exchanges act)
            , Just DeclaredShare{dsFormula = Just f} <- [exchangeDeclaredShare ex]
            ]

-- | The names the database's formulas read, which a process's formulas see too.
namesReadByDatabase :: SimpleDatabase -> [Text]
namesReadByDatabase db = concat [Expr.collectIdentifiers Expr.Volca f | CalculatedParameter f _ <- map parameterDefinition (M.elems (sdbParameters db))]

{- | The database's parameters each process reads, as a format with no
database-wide parameter copies them into it: at the value they settle to
there, a line read by a name shadowing a database parameter of that name.
-}
databaseCopies :: SimpleDatabase -> M.Map (UUID, UUID) (M.Map Text Parameter)
databaseCopies db = M.map (\act -> databaseParametersIn (namedLinesAsParameters (const True) (namesReadByDatabase db) act) (sdbParameters db)) (sdbActivities db)

{- | The database's calculated parameters a process reads whose formula
reads, itself or through others of the database, a name the process defines,
at the value each settles to there: in a format that computes the database's
parameters first, the database's level would compute them without the
process's names.
-}
ownCopies :: SimpleDatabase -> Activity -> M.Map Text Parameter
ownCopies db act = M.filter reachesOwn (databaseParametersIn act (sdbParameters db))
  where
    own :: S.Set Text
    own = S.map T.toLower (M.keysSet (activityParameters act))

    calculated :: M.Map Text Text
    calculated = M.fromList [(T.toLower name, f) | (name, Parameter{parameterDefinition = CalculatedParameter f _}) <- M.toList (sdbParameters db)]

    reachesOwn :: Parameter -> Bool
    reachesOwn p = case parameterDefinition p of
        CalculatedParameter f _ -> not (S.disjoint own (namesReached Expr.Volca calculated f))
        InputParameter{} -> False

-- | Each name of a scope by its lowercase form, as its definition spells it.
spellings :: M.Map Text Parameter -> M.Map Text Text
spellings params = M.fromList [(T.toLower name, name) | name <- M.keys params]

-- | A formula as the format writes it, unless it reads a name the format does not.
writtenWith :: Expr.Dialect -> S.Set Text -> Text -> Maybe Text
writtenWith dialect unwritten formula
    | any ((`S.member` unwritten) . T.toLower) (Expr.collectIdentifiers Expr.Volca formula) = Nothing
    | otherwise = Expr.writtenIn dialect formula

scaled :: Scaled -> Text -> Text
scaled = \case
    Times k -> Expr.times k
    DividedBy k -> Expr.dividedBy k

-- | Whether a line's amount is written as the engine holds it.
asHeld :: Scaled -> Bool
asHeld = \case
    Times k -> k == 1
    DividedBy k -> k == 1

{- | The names, lowercased, a scope cannot write, given those its enclosing
scope cannot and the scope's own parameters, which shadow them: one the format
does not read as a name, and a calculated parameter with no value whose
formula reads such a name or cannot be written, which could be written
neither way and so leaves its own readers without it. In a format stating an
amount beside every formula, a calculated parameter with no value is one.
-}
unwritable :: Expr.Dialect -> S.Set Text -> M.Map Text Parameter -> S.Set Text
unwritable dialect enclosing params = grown (S.union outer (S.fromList (map T.toLower unnamed)))
  where
    outer = S.difference enclosing (S.map T.toLower (M.keysSet params))
    unnamed = [name | name <- M.keys params, Expr.writtenIn dialect name /= Just name]
    valueless = [(T.toLower name, f) | (name, Parameter{parameterDefinition = CalculatedParameter f Nothing}) <- M.toList params]
    grown out
        | S.null more = out
        | otherwise = grown (S.union out more)
      where
        more = S.fromList [name | (name, f) <- valueless, S.notMember name out, leavesOutValueless dialect || isNothing (writtenWith dialect out f)]

-- | Parameters as the format writes them, given how it writes a formula of their scope, and those frozen or left out.
parametersIn :: (Text -> Maybe Text) -> S.Set Text -> Text -> M.Map Text Parameter -> ([(Text, (Parameter, Written))], [Text])
parametersIn written unwritten place params = (concatMap fst each, concatMap snd each)
  where
    each = map one (M.toList params)
    one (name, p)
        | S.member (T.toLower name) unwritten = ([], [place <> " · " <> name <> ": left out, the format cannot write it"])
        | otherwise = case parameterDefinition p of
            InputParameter v u -> ([(name, (p, WrittenValue v u))], [])
            CalculatedParameter f value -> case written f of
                Just text -> ([(name, (p, WrittenFormula text))], [])
                Nothing -> case value of
                    Just v -> ([(name, (p, WrittenValue v Nothing))], [place <> " · " <> name <> ": " <> f])
                    Nothing -> ([], [place <> " · " <> name <> ": " <> f <> " (no value, left out)"])

{- | The activity with each named line some formula reads, among those
given, made a calculated parameter of that name, the line reading it. A line
not given keeps its name. A name a parameter already has
stays the parameter's: the engine refuses a line of that name at edit, and an
import keeps no formula reading a name given twice.
-}
namedLinesAsParameters :: (Exchange -> Bool) -> [Text] -> Activity -> Activity
namedLinesAsParameters given databaseReads act =
    act
        { exchanges = map unnamed (exchanges act)
        , activityParameters = M.union (activityParameters act) (M.fromList (mapMaybe asParameter (exchanges act)))
        }
  where
    readNames :: S.Set Text
    readNames =
        S.fromList . map T.toLower $
            databaseReads
                <> concat [Expr.collectIdentifiers Expr.Volca (efText f) | Just f <- map exchangeFormula (exchanges act)]
                <> concat [Expr.collectIdentifiers Expr.Volca f | CalculatedParameter f _ <- map parameterDefinition (M.elems (activityParameters act))]

    taken :: S.Set Text
    taken = S.map T.toLower (M.keysSet (activityParameters act))

    read' :: Text -> Bool
    read' name = S.member (T.toLower name) readNames && S.notMember (T.toLower name) taken

    asParameter :: Exchange -> Maybe (Text, Parameter)
    asParameter ex = case exchangeFormula ex of
        Just ExchangeFormula{efName = Just name, efText = text}
            | given ex && read' name -> Just (name, Parameter{parameterDefinition = CalculatedParameter text (Just (exchangeAmount ex)), parameterUnit = Nothing, parameterDescription = ""})
        _ -> Nothing

    unnamed :: Exchange -> Exchange
    unnamed ex = case exchangeFormula ex of
        Just ExchangeFormula{efName = Just name}
            | given ex && read' name -> withFormula (Just (ExchangeFormula Nothing name)) ex
        Just f
            | given ex -> withFormula (Just f{efName = Nothing}) ex
            | otherwise -> ex
        Nothing -> ex

-- | The name of a line's flow, as the warnings name it.
flowName :: SimpleDatabase -> Exchange -> Text
flowName db ex = case ex of
    TechnosphereExchange{} -> maybe fallback tfName (M.lookup fid (sdbTechFlows db))
    BiosphereExchange{} -> maybe fallback bfName (M.lookup fid (sdbBioFlows db))
    WasteExchange{} -> maybe fallback wfName (M.lookup fid (sdbWasteFlows db))
  where
    fid = exchangeFlowId ex
    fallback = T.pack (show fid)

-- | Whether an activity computes any of its amounts or parameters.
hasFormulas :: Activity -> Bool
hasFormulas a = any isCalculated (activityParameters a) || any (isJust . exchangeFormula) (exchanges a)
  where
    isCalculated p = case parameterDefinition p of
        CalculatedParameter{} -> True
        InputParameter{} -> False

-- | What a format writing no parameter says: the processes whose formulas it writes as values, and the database's parameters it leaves out.
valuesOnly :: SimpleDatabase -> [Text]
valuesOnly db =
    [ T.pack (show (length named)) <> " processes have formulas, written as the values they computed:" <> foldMap ("\n  " <>) (take 10 named)
    | let named = [activityName a | a <- M.elems (sdbActivities db), hasFormulas a]
    , not (null named)
    ]
        <> [T.pack (show (M.size (sdbParameters db))) <> " parameters of the database are not written" | not (M.null (sdbParameters db))]

-- | What the export says of the formulas it could not write, and of the parameters' units it has no place for.
formulaWarnings :: Exported -> [Text]
formulaWarnings ex = frozenWarnings ex <> [tshow (exUnits ex) <> " parameters state a unit, which the format has no place for" | exUnits ex > 0]
  where
    tshow :: Int -> Text
    tshow = T.pack . show

-- | What the export says of the formulas it could not write.
frozenWarnings :: Exported -> [Text]
frozenWarnings ex =
    [ T.intercalate "\n  " ((tshow (length frozen) <> " formulas are written as the values they compute, the format reading them otherwise or not at all:") : take 10 frozen <> ["and " <> tshow (length frozen - 10) <> " more" | length frozen > 10])
    | let frozen = exFrozen ex
    , not (null frozen)
    ]
  where
    tshow :: Int -> Text
    tshow = T.pack . show
