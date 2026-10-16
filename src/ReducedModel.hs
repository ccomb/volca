{- | The columns of the processes some parameters move, as entries a program
computes again at other values: where each line goes, what one unit of its
amount puts there, its stored amount and its formula.

The rows and factors come from the builders that load the database, read for
an amount of one, so the columns rebuilt from these entries are the matrix's
own; a line whose amount is zero at the defaults is kept, since other values
make it an entry of the matrix.
-}
module ReducedModel (
    Supplier (..),
    Entry (..),
    Column (..),
    column,
    columns,
    maxProcesses,
    maxEntries,
) where

import Data.Int (Int32)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Vector as V
import Database.MatrixBuild (LineRow (..), LineRows (..), lineRows)
import Parametric (Moving (..), ParameterRefusal, moving)
import Types
import UnitConversion (UnitConfig)

-- | Where a line of a moving process takes its product from.
data Supplier
    = -- | A process of the same database.
      Within !ProcessId
    | -- | A process of another database, by name, and the unit the line states its amount in.
      Elsewhere !Text !(UUID, UUID) !Text
    deriving (Eq, Ord, Show)

{- | A line: its position among the activity's exchanges, none for a fixed
amount from another database that answers no line, what one unit of its amount
adds before the column is divided by its reference, its stored amount, its
formula.
-}
data Entry = Entry
    { entryLine :: !(Maybe Int)
    , entryFactor :: !Double
    , entryStored :: !Double
    , entryFormula :: !(Maybe Text)
    }
    deriving (Eq, Show)

data Column = Column
    { columnProcess :: !ProcessId
    , columnReference :: !NormLines
    , columnTechnosphere :: ![(Supplier, Entry)]
    , columnBiosphere :: ![(Int32, Entry)]
    }

{- | The processes a root reaches whose amounts these parameters move, each with
its column. A process the allocation gate refuses has none, and is refused with
its reason; so is a parameter 'moving' refuses.
-}
columns :: UnitConfig -> Database -> ProcessId -> [(Maybe ProcessId, Text)] -> Either (Either ParameterRefusal Text) [(Moving, Column)]
columns unitConfig db root free = either (Left . Left) (traverse (\process -> either (Left . Right) (Right . (,) process) (columnOf (movingProcess process)))) (moving db root free)
  where
    columnOf = column unitConfig db

{- | A process's column, or why the allocation gate refuses it one. Applied to
its database once, then to each process.
-}
column :: UnitConfig -> Database -> ProcessId -> Either Text Column
column unitConfig db = columnOf
  where
    rowsOf = lineRows unitConfig db

    columnOf :: ProcessId -> Either Text Column
    columnOf pid = do
        let act = dbActivities db V.! fromIntegral pid
            key = dbProcessIdTable db V.! fromIntegral pid
        LineRows tech bio <- rowsOf pid act
        pure
            Column
                { columnProcess = pid
                , columnReference = normLines act key
                , columnTechnosphere = [(Within (lineRow line), entry line) | line <- tech] <> map (linked act) (M.findWithDefault [] key linksOf)
                , columnBiosphere = [(lineRow line, entry line) | line <- bio]
                }

    linksOf :: M.Map (UUID, UUID) [CrossDBLink]
    linksOf = M.fromListWith (flip (<>)) [((cdlConsumerActUUID link, cdlConsumerProdUUID link), [link]) | link <- dbCrossDBLinks db]

    entry :: LineRow row -> Entry
    entry (LineRow n ex _ factor) = Entry (Just n) factor (exchangeAmount ex) (efText <$> exchangeFormula ex)

    -- A link supplies the line it answers, or, answering none, a fixed amount.
    linked :: Activity -> CrossDBLink -> (Supplier, Entry)
    linked act link =
        ( Elsewhere (cdlSourceDatabase link) (cdlSupplierActUUID link, cdlSupplierProdUUID link) (cdlExchangeUnit link)
        , fromMaybe (Entry Nothing 1 (cdlCoefficient link) Nothing) $ do
            AnsweredExchange{aePosition = position, aeFactor = factor} <- cdlAnswers link
            (\ex -> entry (LineRow position ex () factor)) <$> listToMaybe (drop position (exchanges act))
        )

-- | The most processes a model moves: the client solves a system of that size for each set of values.
maxProcesses :: Int
maxProcesses = 500

-- | The most moving processes times suppliers: the scalings a model carries, about 8 MB of numbers.
maxEntries :: Int
maxEntries = 1000000
