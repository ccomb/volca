{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The parameters and formulas of a database as a format writes them.

A formula is written when the format reads the text back to the very formula
the engine holds ('Expr.writtenIn'), and reads no parameter the format cannot
name. Otherwise it is frozen: a line keeps its amount, a calculated parameter
becomes an input parameter at its value, and the warnings name it.

No format written here names its lines, so a line that a formula reads by its
name becomes a calculated parameter of its process under that name, its
formula the line's, and the line reads the parameter. A share's formula,
kept in SimaPro's language, follows the same rule.
-}
module Database.ExportFormulas (
    Exported (..),
    Written (..),
    Scaled (..),
    exportFormulas,
    formulaWarnings,
    valuesOnly,
    hasFormulas,
) where

import Data.Either (lefts, rights)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Maybe (isJust, isNothing, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import qualified Expr
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
    , exProcesses :: !(M.Map (UUID, UUID) (M.Map Text (Parameter, Written)))
    -- ^ Each process's own parameters, its named lines among them.
    , exLines :: !(M.Map (UUID, UUID) (IM.IntMap Text))
    -- ^ The formula of each line written with one, by its position in the activity.
    , exShares :: !(M.Map (UUID, UUID) (IM.IntMap Text))
    -- ^ The formula of each product's share written with one, by its position.
    , exFrozen :: ![Text]
    -- ^ Each formula written as its value, or parameter left out, and why.
    , exUnits :: !Int
    -- ^ How many parameters state a unit.
    }

exportFormulas :: Expr.Dialect -> ((UUID, UUID) -> Exchange -> Scaled) -> SimpleDatabase -> Exported
exportFormulas dialect scaledBy db =
    Exported
        { exDatabase = M.fromList database
        , exProcesses = M.map (\(ps, _, _, _) -> M.fromList ps) processes
        , exLines = M.map (\(_, ls, _, _) -> IM.fromList ls) processes
        , exShares = M.map (\(_, _, ss, _) -> IM.fromList ss) processes
        , exFrozen = databaseFrozen <> concatMap (\(_, _, _, fs) -> fs) (M.elems processes)
        , exUnits = length [() | p <- M.elems (sdbParameters db) <> concatMap (M.elems . activityParameters) (M.elems (sdbActivities db)), isJust (parameterUnit p)]
        }
  where
    databaseUnwritten :: S.Set Text
    databaseUnwritten = unwritable dialect (sdbParameters db)

    database :: [(Text, (Parameter, Written))]
    databaseFrozen :: [Text]
    (database, databaseFrozen) = parametersIn dialect "the database" databaseUnwritten (sdbParameters db)

    -- The names the database's formulas read, which a process's formulas see too.
    databaseReads :: [Text]
    databaseReads = concat [Expr.collectIdentifiers Expr.Volca f | CalculatedParameter f _ <- map parameterDefinition (M.elems (sdbParameters db))]

    processes :: M.Map (UUID, UUID) ([(Text, (Parameter, Written))], [(Int, Text)], [(Int, Text)], [Text])
    processes = M.mapWithKey process (sdbActivities db)

    process :: (UUID, UUID) -> Activity -> ([(Text, (Parameter, Written))], [(Int, Text)], [(Int, Text)], [Text])
    process key source = (parameters, rights lines', rights shares, parametersFrozen <> lefts lines' <> lefts shares)
      where
        act = namedLinesAsParameters databaseReads source
        own = activityParameters act
        unwritten = unwritable dialect own <> S.difference databaseUnwritten (S.map T.toLower (M.keysSet own))
        (parameters, parametersFrozen) = parametersIn dialect (activityName act) unwritten own
        lines' =
            [ maybe (Left (activityName act <> " · " <> flowName db ex <> ": " <> efText f)) (Right . (,) i) (writtenWith dialect unwritten (scaled (scaledBy key ex) (efText f)))
            | (i, ex) <- zip [0 ..] (exchanges act)
            , Just f <- [exchangeFormula ex]
            ]
        -- A share's formula is kept in SimaPro's language, as it was read.
        shares =
            [ maybe (Left (activityName act <> " · share of " <> flowName db ex <> ": " <> f)) (Right . (,) i) (writtenWith dialect unwritten =<< either (const Nothing) Just (Expr.translate Expr.SimaPro f))
            | (i, ex) <- zip [0 ..] (exchanges act)
            , Just DeclaredShare{dsFormula = Just f} <- [exchangeDeclaredShare ex]
            ]

-- | A formula as the format writes it, unless it reads a name the format does not.
writtenWith :: Expr.Dialect -> S.Set Text -> Text -> Maybe Text
writtenWith dialect unwritten formula
    | any ((`S.member` unwritten) . T.toLower) (Expr.collectIdentifiers Expr.Volca formula) = Nothing
    | otherwise = Expr.writtenIn dialect formula

scaled :: Scaled -> Text -> Text
scaled = \case
    Times k -> Expr.times k
    DividedBy k -> Expr.dividedBy k

{- | The names, lowercased, the format cannot give its parameters: one it does
not read as a name, and a calculated parameter with no value whose formula it
cannot write, which could be written neither way.
-}
unwritable :: Expr.Dialect -> M.Map Text Parameter -> S.Set Text
unwritable dialect params = S.fromList (map T.toLower (unnamed <> valueless))
  where
    unnamed = [name | name <- M.keys params, Expr.writtenIn dialect name /= Just name]
    unnamedSet = S.fromList (map T.toLower unnamed)
    valueless = [name | (name, Parameter{parameterDefinition = CalculatedParameter f Nothing}) <- M.toList params, isNothing (writtenWith dialect unnamedSet f)]

-- | Parameters as the format writes them, and those frozen or left out.
parametersIn :: Expr.Dialect -> Text -> S.Set Text -> M.Map Text Parameter -> ([(Text, (Parameter, Written))], [Text])
parametersIn dialect place unwritten params = (concatMap fst each, concatMap snd each)
  where
    each = map one (M.toList params)
    one (name, p)
        | S.member (T.toLower name) unwritten = ([], [place <> " · " <> name <> ": left out, the format cannot write it"])
        | otherwise = case parameterDefinition p of
            InputParameter v u -> ([(name, (p, WrittenValue v u))], [])
            CalculatedParameter f value -> case writtenWith dialect unwritten f of
                Just text -> ([(name, (p, WrittenFormula text))], [])
                Nothing -> case value of
                    Just v -> ([(name, (p, WrittenValue v Nothing))], [place <> " · " <> name <> ": " <> f])
                    Nothing -> ([], [place <> " · " <> name <> ": " <> f <> " (no value, left out)"])

{- | The activity with each named line some formula reads made a calculated
parameter of that name, the line reading it. A name a parameter already has
stays the parameter's: the engine refuses a line of that name at edit, and an
import keeps no formula reading a name given twice.
-}
namedLinesAsParameters :: [Text] -> Activity -> Activity
namedLinesAsParameters databaseReads act =
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
            | read' name -> Just (name, Parameter{parameterDefinition = CalculatedParameter text (Just (exchangeAmount ex)), parameterUnit = Nothing, parameterDescription = ""})
        _ -> Nothing

    unnamed :: Exchange -> Exchange
    unnamed ex = case exchangeFormula ex of
        Just ExchangeFormula{efName = Just name}
            | read' name -> withFormula (Just (ExchangeFormula Nothing name)) ex
        Just f -> withFormula (Just f{efName = Nothing}) ex
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

-- | What the export says of the formulas it could not write.
formulaWarnings :: Exported -> [Text]
formulaWarnings ex =
    [ T.intercalate "\n  " ((tshow (length frozen) <> " formulas are written as the values they compute, the format reading them otherwise or not at all:") : take 10 frozen <> ["and " <> tshow (length frozen - 10) <> " more" | length frozen > 10])
    | let frozen = exFrozen ex
    , not (null frozen)
    ]
        <> [tshow (exUnits ex) <> " parameters state a unit, which the format has no place for" | exUnits ex > 0]
  where
    tshow :: Int -> Text
    tshow = T.pack . show
