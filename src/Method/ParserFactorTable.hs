{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Methods published as a workbook that lists one characterization factor per
row: a @CFs@ sheet labelled @Method, Category, Indicator, Name, Compartment,
Subcompartment, CF@, and an @Indicators@ sheet giving each indicator its unit.
One workbook holds many method families, each read to a collection of its own,
since two families both name a category "climate change".

The table names a flow by name and compartment only. The unit a factor is
written per, and the flow's CAS number, come from the master data of the
EcoSpold 2 release the table was made for ('MasterFlow'). The unit cannot be
left to the indicator's: a factor whose indicator unit the engine knows (an
energy in MJ, a land use "dimensionless") would then convert the flow onto it,
which fails across dimensions and scores the flow zero.
-}
module Method.ParserFactorTable (
    FactorTable,
    Dropped (..),
    isFactorTable,
    readFactorTable,
    familyCollections,
) where

import Data.Containers.ListUtils (nubOrd)
import Data.Indexing (groupInOrder, repeated, uniqueIndex)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID

import EcoSpold.Parser2 (MasterFlow (..))
import Method.Types
import SubstanceRegistry (nonEmptyCAS)
import Xlsx (CellValue (..), Workbook, cellText, foldSheet, sheetNames)

-- | One row of the @CFs@ sheet.
data Factor = Factor
    { fFamily :: !Text
    , fCategory :: !Text
    , fIndicator :: !Text
    , fFlow :: !FlowKey
    , fValue :: !Double
    }

-- | A flow as the table and the master data both name it: name, compartment, subcompartment.
type FlowKey = (Text, Text, Text)

data FactorTable = FactorTable
    { ftFactors :: ![Factor]
    -- ^ In sheet order, which is the order the families and their columns are kept in.
    , ftUnits :: !(M.Map (Text, Text, Text) Text)
    -- ^ (family, category, indicator) to the indicator's unit.
    }

-- | The factors that name a flow the master data does not define, so carry no unit.
data Dropped = Dropped
    { dropCount :: !Int
    , dropExamples :: ![Text]
    }
    deriving (Eq, Show)

isFactorTable :: Workbook -> Bool
isFactorTable wb = all (`elem` sheetNames wb) ["CFs", "Indicators"]

-- | The factors read so far, newest first, and the rows that gave none.
data Rows = Rows ![Factor] !Int

{- | Read both sheets. A row that is not empty yet gives no factor (no family,
no flow name, a CF that is not a number) refuses the table rather than vanish
from it, and so does an indicator the @Indicators@ sheet gives two units.
-}
readFactorTable :: Workbook -> Either Text FactorTable
readFactorTable wb = do
    Rows factors unread <- foldLabelled wb "CFs" (map pure ["Method", "Category", "Indicator", "Name", "Compartment", "Subcompartment", "CF"]) addFactor (Rows [] 0)
    case (unread, factors) of
        (0, []) -> Left "the CFs sheet gives no factor"
        (0, _) -> Right ()
        _ -> Left (T.pack (show unread) <> " rows of the CFs sheet give no factor: each needs a method, a flow name and a numeric CF")
    unitSets <- foldLabelled wb "Indicators" [["Method"], ["Category"], ["Indicator"], ["Indicator Unit", "Unit"]] addUnit M.empty
    units <- case M.toList (M.filter ((> 1) . S.size) unitSets) of
        [] -> Right (M.mapMaybe S.lookupMin unitSets)
        ((family, category, ind), given) : _ -> Left ("the Indicators sheet gives " <> family <> " / " <> category <> " / " <> ind <> " several units: " <> T.intercalate ", " (S.toList given))
    Right FactorTable{ftFactors = reverse factors, ftUnits = units}
  where
    addFactor acc@(Rows done unread) cells = case cells of
        [Just family, Just category, Just indicator, Just name, compartment, subcompartment, Just (CellNumber value)]
            | Just f <- cellText family
            , Just n <- cellText name ->
                -- Built now: a pending row would keep its cells alive until the end of the sheet.
                let !row = Factor f (text category) (text indicator) (n, textOr compartment, textOr subcompartment) value in Rows (row : done) unread
        _
            | all null cells -> acc
            | otherwise -> Rows done (unread + 1)
    -- The sheet can repeat an indicator once per factor, so its rows collapse to the units they give.
    addUnit acc cells = case cells of
        [Just family, category, indicator, Just unit]
            | Just f <- cellText family
            , Just u <- cellText unit ->
                M.insertWith S.union (f, textOr category, textOr indicator) (S.singleton u) acc
        _ -> acc
    text = fromMaybe "" . cellText
    textOr = maybe "" text

-- | Where a sheet's fold stands: before its label row, reading under it, or refused.
data Labelled a = Seeking | Reading ![Int] !a | Refused !Text

{- | Fold over the rows under a sheet's label row, each read at the columns its
labels name, whatever their order. A label given several spellings takes the
first the sheet uses; a label the sheet lacks, or gives two columns, refuses
the sheet, naming it.
-}
foldLabelled :: Workbook -> Text -> [[Text]] -> (a -> [Maybe CellValue] -> a) -> a -> Either Text a
foldLabelled wb sheet labels step start = do
    scan <- foldSheet wb sheet go Seeking
    case scan of
        Reading _ acc -> Right acc
        Seeking -> Left ("the " <> sheet <> " sheet is empty")
        Refused why -> Left ("the " <> sheet <> " sheet " <> why)
  where
    go Seeking row
        | M.null row = Seeking
        | otherwise =
            let wanted = concat labels
                given = [(label, col) | (col, cell) <- M.toList row, Just label <- [cellText cell], label `elem` wanted]
             in case uniqueIndex given of
                    Left twice -> Refused ("has two columns labelled " <> T.intercalate ", " (NE.toList twice))
                    Right columns ->
                        let found = [mapMaybe (`M.lookup` columns) spellings | spellings <- labels]
                         in case [T.intercalate " or " spellings | (spellings, []) <- zip labels found] of
                                [] -> Reading [col | col : _ <- found] start
                                missing -> Refused ("has no column labelled " <> T.intercalate ", " missing)
    go (Reading columns acc) row = Reading columns (step acc [M.lookup col row | col <- columns])
    go refused _ = refused

{- | One collection per family, each paired with its family's name, and the
factors left out because the master data defines no flow for them.

A column is a (category, indicator) pair, and the engine identifies a method
by its name alone, so the name is the category, followed by the indicator
only where the category heads several columns: one indicator serves many
categories (a GWP100 for every climate change category), and one category
can be measured by several indicators.
-}
familyCollections :: [MasterFlow] -> FactorTable -> Either Text ([(Text, MethodCollection)], Dropped)
familyCollections masterFlows table = do
    facts <- flowFacts masterFlows
    let known = filter ((`M.member` facts) . fFlow) (ftFactors table)
        unknown = filter (not . (`M.member` facts) . fFlow) (ftFactors table)
        -- A family none of whose flows the master data defines has no factor left: its factors are all in 'Dropped'.
        families = nubOrd (map fFamily known)
        byFamily = NE.toList <$> groupInOrder [(fFamily f, f) | f <- known]
    collections <- traverse (collection facts byFamily) families
    Right (collections, Dropped (length unknown) (take 10 (nubOrd [describe (fFlow f) | f <- unknown])))
  where
    describe (name, top, sub) = name <> " (" <> T.intercalate "/" (filter (not . T.null) [top, sub]) <> ")"

    collection facts byFamily family = do
        let rows = M.findWithDefault [] family byFamily
            columns = nubOrd [(fCategory f, fIndicator f) | f <- rows]
            byColumn = NE.toList <$> groupInOrder [((fCategory f, fIndicator f), f) | f <- rows]
            names = columnNames columns
        case repeated names of
            [] -> Right ()
            twice -> Left (family <> ": two columns would both be named " <> T.intercalate ", " twice)
        units <- traverse (unitOf family) columns
        Right
            ( family
            , MethodCollection
                { mcMethods =
                    [ method facts family column name unit (M.findWithDefault [] column byColumn)
                    | (column, name, unit) <- zip3 columns names units
                    ]
                , mcScoringSets = []
                , mcUnregionalized = []
                }
            )

    unitOf family (category, indicator) =
        maybe (Left ("the Indicators sheet gives no unit for " <> family <> " / " <> category <> " / " <> indicator)) Right (M.lookup (family, category, indicator) (ftUnits table))

    method facts family (category, _) name unit rows =
        Method
            { -- Written to CSV and read back, which derives its own identifiers.
              methodId = UUID.nil
            , methodName = name
            , methodDescription = Nothing
            , methodUnit = unit
            , methodCategory = category
            , methodMethodology = Just family
            , methodFactors = mapMaybe (factor facts) rows
            }

columnNames :: [(Text, Text)] -> [Text]
columnNames columns = [if heads category == 1 then category else category <> " / " <> indicator | (category, indicator) <- columns]
  where
    counts = M.fromListWith (+) [(category, 1 :: Int) | (category, _) <- columns]
    heads category = M.findWithDefault 0 category counts

-- | What the master data adds to a factor: the flow's unit, and its CAS number.
data FlowFacts = FlowFacts !Text !(Maybe Text)
    deriving (Eq, Ord)

{- | The facts of every flow the master data defines. A CAS number it gives to
flows of several names is dropped: "Nitrogen" and "Nitrogen, organic bound"
share one, and kept it would hand the factor of either to the other. The
comparison is on the normalized number, so a padded @007440-43-9@ and a plain
@7440-43-9@ are one.

A flow listed twice alike is one flow; listed twice with another unit or CAS
number, the table cannot say which its factor is written per, and is refused.
-}
flowFacts :: [MasterFlow] -> Either Text (M.Map FlowKey FlowFacts)
flowFacts flows = case uniqueIndex (nubOrd [(key f, FlowFacts (mfUnit f) (mfilterShared (cas f))) | f <- flows]) of
    Right facts -> Right facts
    Left twice -> Left ("the master data gives these flows two units or CAS numbers: " <> T.intercalate ", " [n <> " (" <> top <> "/" <> sub <> ")" | (n, top, sub) <- NE.toList twice])
  where
    key f = (mfName f, mfCompartment f, mfSubcompartment f)
    cas f = nonEmptyCAS =<< mfCAS f
    owners = M.fromListWith S.union [(c, S.singleton (mfName f)) | f <- flows, Just c <- [cas f]]
    mfilterShared c = c >>= \n -> if maybe 0 S.size (M.lookup n owners) == 1 then Just n else Nothing

factor :: M.Map FlowKey FlowFacts -> Factor -> Maybe MethodCF
factor facts f = do
    FlowFacts unit cas <- M.lookup (fFlow f) facts
    let (name, top, sub) = fFlow f
        medium = T.toCaseFold top
    Just
        MethodCF
            { mcfFlowRef = UUID.nil
            , mcfFlowName = name
            , mcfDirection = if medium == "natural resource" then Input else Output
            , mcfValue = fValue f
            , mcfCompartment = Just (Compartment medium sub "")
            , mcfCAS = cas
            , mcfUnit = unit
            , mcfConsumerLocation = Nothing
            }
