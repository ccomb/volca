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

readFactorTable :: Workbook -> Either Text FactorTable
readFactorTable wb = do
    factors <- foldLabelled wb "CFs" (map pure ["Method", "Category", "Indicator", "Name", "Compartment", "Subcompartment", "CF"]) addFactor []
    units <- foldLabelled wb "Indicators" [["Method"], ["Category"], ["Indicator"], ["Indicator Unit", "Unit"]] addUnit M.empty
    Right FactorTable{ftFactors = reverse factors, ftUnits = units}
  where
    addFactor acc cells = case cells of
        [Just family, Just category, Just indicator, Just name, compartment, subcompartment, Just (CellNumber value)]
            | Just f <- cellText family
            , Just n <- cellText name ->
                -- Built now: a pending row would keep its cells alive until the end of the sheet.
                let !row = Factor f (text category) (text indicator) (n, textOr compartment, textOr subcompartment) value in row : acc
        _ -> acc
    addUnit acc cells = case cells of
        [Just family, category, indicator, unit] | Just f <- cellText family -> M.insert (f, textOr category, textOr indicator) (textOr unit) acc
        _ -> acc
    text = fromMaybe "" . cellText
    textOr = maybe "" text

-- | Where a sheet's fold stands: before its label row, or reading under it.
data Labelled a = Seeking | Reading ![Int] !a | Unlabelled ![Text]

{- | Fold over the rows under a sheet's label row, each read at the columns its
labels name, whatever their order. A label given several spellings takes the
first the sheet uses; a label the sheet lacks refuses the sheet, naming it.
-}
foldLabelled :: Workbook -> Text -> [[Text]] -> (a -> [Maybe CellValue] -> a) -> a -> Either Text a
foldLabelled wb sheet labels step start = do
    scan <- foldSheet wb sheet go Seeking
    case scan of
        Reading _ acc -> Right acc
        Seeking -> Left ("the " <> sheet <> " sheet is empty")
        Unlabelled missing -> Left ("the " <> sheet <> " sheet has no column labelled " <> T.intercalate ", " missing)
  where
    go Seeking row
        | M.null row = Seeking
        | otherwise =
            let columns = M.fromList [(label, col) | (col, cell) <- M.toList row, Just label <- [cellText cell]]
                found = [mapMaybe (`M.lookup` columns) spellings | spellings <- labels]
             in case [T.intercalate " or " spellings | (spellings, []) <- zip labels found] of
                    [] -> Reading [col | col : _ <- found] start
                    missing -> Unlabelled missing
    go (Reading columns acc) row = Reading columns (step acc [M.lookup col row | col <- columns])
    go unlabelled _ = unlabelled

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
    collections <- traverse collection families
    Right (collections, dropped)
  where
    facts = flowFacts masterFlows
    known = filter ((`M.member` facts) . fFlow) (ftFactors table)
    unknown = filter (not . (`M.member` facts) . fFlow) (ftFactors table)
    dropped = Dropped (length unknown) (take 10 (nubOrd [describe (fFlow f) | f <- unknown]))
    describe (name, top, sub) = name <> " (" <> T.intercalate "/" (filter (not . T.null) [top, sub]) <> ")"
    families = nubOrd (map fFamily (ftFactors table))
    byFamily = M.fromListWith (flip (<>)) [(fFamily f, [f]) | f <- known]

    collection family = do
        let rows = M.findWithDefault [] family byFamily
            columns = nubOrd [(fCategory f, fIndicator f) | f <- rows]
            byColumn = M.fromListWith (flip (<>)) [((fCategory f, fIndicator f), [f]) | f <- rows]
            names = columnNames columns
        case repeatedNames names of
            [] -> Right ()
            twice -> Left (family <> ": two columns would both be named " <> T.intercalate ", " twice)
        Right
            ( family
            , MethodCollection
                { mcMethods =
                    [ method family column name (M.findWithDefault [] column byColumn)
                    | (column, name) <- zip columns names
                    ]
                , mcScoringSets = []
                , mcUnregionalized = []
                }
            )

    method family (category, indicator) name rows =
        Method
            { -- Written to CSV and read back, which derives its own identifiers.
              methodId = UUID.nil
            , methodName = name
            , methodDescription = Nothing
            , methodUnit = M.findWithDefault "" (family, category, indicator) (ftUnits table)
            , methodCategory = category
            , methodMethodology = Just family
            , methodFactors = mapMaybe (factor facts) rows
            }

columnNames :: [(Text, Text)] -> [Text]
columnNames columns = [if heads category == 1 then category else category <> " / " <> indicator | (category, indicator) <- columns]
  where
    counts = M.fromListWith (+) [(category, 1 :: Int) | (category, _) <- columns]
    heads category = M.findWithDefault 0 category counts

repeatedNames :: [Text] -> [Text]
repeatedNames names = M.keys (M.filter (> (1 :: Int)) (M.fromListWith (+) [(n, 1) | n <- names]))

-- | What the master data adds to a factor: the flow's unit, and its CAS number.
data FlowFacts = FlowFacts !Text !(Maybe Text)

{- | The facts of every flow the master data defines. A CAS number it gives to
flows of several names is dropped: "Nitrogen" and "Nitrogen, organic bound"
share one, and kept it would hand the factor of either to the other. The
comparison is on the normalized number, so a padded @007440-43-9@ and a plain
@7440-43-9@ are one.
-}
flowFacts :: [MasterFlow] -> M.Map FlowKey FlowFacts
flowFacts flows = M.fromList [(key f, FlowFacts (mfUnit f) (mfilterShared (cas f))) | f <- flows]
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
