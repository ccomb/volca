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
    columns,
) where

import Data.Int (Int32)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Vector as V
import Database.MatrixBuild (lineRows)
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

-- | A line: what one unit of its amount adds before the column is divided by its reference, its stored amount, its formula.
data Entry = Entry
    { entryFactor :: !Double
    , entryStored :: !Double
    , entryFormula :: !(Maybe Text)
    }
    deriving (Eq, Show)

data Column = Column
    { columnMoving :: !Moving
    , columnReference :: !NormLines
    , columnTechnosphere :: ![(Supplier, Entry)]
    , columnBiosphere :: ![(Int32, Entry)]
    }

{- | The processes a root reaches whose amounts these parameters move, each with
its column. A process the allocation gate refuses has none, and is refused with
its reason; so is a parameter 'moving' refuses.
-}
columns :: UnitConfig -> Database -> ProcessId -> [(Maybe ProcessId, Text)] -> Either (Either ParameterRefusal Text) [Column]
columns unitConfig db root free = either (Left . Left) (traverse (either (Left . Right) Right . column)) (moving db root free)
  where
    rowsOf = lineRows unitConfig db

    column :: Moving -> Either Text Column
    column process = do
        let pid = movingProcess process
            act = dbActivities db V.! fromIntegral pid
            key = dbProcessIdTable db V.! fromIntegral pid
        (tech, bio) <- rowsOf pid act
        pure
            Column
                { columnMoving = process
                , columnReference = normLines act key
                , columnTechnosphere = [(Within i, entry factor ex) | (ex, i, factor) <- tech] <> [linked act link | link <- dbCrossDBLinks db, (cdlConsumerActUUID link, cdlConsumerProdUUID link) == key]
                , columnBiosphere = [(row, entry 1 ex) | (ex, row) <- bio]
                }

    entry :: Double -> Exchange -> Entry
    entry factor ex = Entry factor (exchangeAmount ex) (efText <$> exchangeFormula ex)

    -- A link supplies the line it answers, or, answering none, a fixed amount.
    linked :: Activity -> CrossDBLink -> (Supplier, Entry)
    linked act link =
        ( Elsewhere (cdlSourceDatabase link) (cdlSupplierActUUID link, cdlSupplierProdUUID link) (cdlExchangeUnit link)
        , fromMaybe (Entry 1 (cdlCoefficient link) Nothing) $ do
            AnsweredExchange{aePosition = position, aeFactor = factor} <- cdlAnswers link
            entry factor <$> listToMaybe (drop position (exchanges act))
        )
