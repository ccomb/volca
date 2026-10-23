{-# LANGUAGE OverloadedStrings #-}

{- | A database read under an allocation key, built from a parent held in
memory rather than from the files.

A parent keeps its processes of several products whole, every product row
priced. Its child divides them under a key. Building the child from the parent
as it stands, rather than from the files, is what carries the parent's edits
over: a price changed on the parent changes the child's shares.
-}
module Database.Child (
    childOf,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.UUID (UUID)

import Database.Allocation (Allocating (..), allocate)
import Database.Loader (getReferenceProductUUID, linkWithinDatabaseByName)
import Database.Rebuild (rebuildFromActivities)
import Types (
    Activity (..),
    AllocationKey,
    Database (..),
    SimpleDatabase (..),
    exchangeIsProductOutput,
    toSimpleDatabase,
 )
import UnitConversion (UnitConfig)

{- | The parent's processes of several products divided under the key, each
piece keyed as a load keys it: the block's activity, the piece's product.

Only those processes are divided again. A process of one product was already
cut to its declared share when the parent was read, and dividing it a second
time would apply that share twice.

The pieces are then linked by name among themselves: an input that consumed a
coproduct found no supplier in the parent, whose name index holds reference
products only, and finds the piece that now produces it. An input the parent
already linked keeps its supplier.
-}
childOf :: UnitConfig -> AllocationKey -> Database -> IO (Either Text Database)
childOf unitConfig key parent = case keyed of
    Left clash -> pure (Left clash)
    Right pieces -> do
        linked <- linkWithinDatabaseByName unitConfig simple{sdbActivities = pieces}
        pure (rebuildFromActivities unitConfig parent (sdbActivities linked))
  where
    simple :: SimpleDatabase
    simple = toSimpleDatabase parent

    alloc :: Allocating
    alloc = Allocating{alKey = key, alUnitConfig = unitConfig, alUnitDB = sdbUnits simple}

    divided :: [((UUID, UUID), Activity)]
    divided =
        [ ((activityId, getReferenceProductUUID piece), piece)
        | ((activityId, _), act) <- M.toList (sdbActivities simple)
        , piece <- if severalProducts act then NE.toList (allocate alloc act) else [act]
        ]

    severalProducts :: Activity -> Bool
    severalProducts act = length (filter exchangeIsProductOutput (exchanges act)) > 1

    -- Two product rows of one block naming the same flow would key two pieces
    -- alike, and a map would keep one inventory in silence.
    keyed :: Either Text (M.Map (UUID, UUID) Activity)
    keyed = case M.elems (M.filter ((> 1) . NE.length) (M.fromListWith (<>) [(k, pure act) | (k, act) <- divided])) of
        [] -> Right (M.fromList divided)
        clash : _ -> Left (activityName (NE.head clash) <> " names one product on two rows: its pieces cannot be told apart")
