{-# LANGUAGE OverloadedStrings #-}

{- | A database divided by a system model, built from a parent held in memory
rather than from the files.

A parent keeps its processes of several products whole, every product row
priced. Its child divides them under its model. Building the child from the
parent as it stands, rather than from the files, is what carries the parent's
edits over: a price changed on the parent changes the child's shares.
-}
module Database.Child (
    childOf,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)

import Database.Allocation (Allocating (..), allocate, describePropertyRefusal, propertyKeyRefusals)
import Database.Loader (getReferenceProductUUID, linkWithinDatabaseByName)
import Database.Rebuild (rebuildFromActivities)
import Database.SystemModel (Resolution (..), parentProcesses, resolveModel)
import Progress (ProgressLevel (..), reportProgress)
import Types (
    Activity (..),
    AllocationKey (..),
    AllocationProperty,
    Database (..),
    ModelException (..),
    SimpleDatabase (..),
    SystemModel,
    propertyName,
    reasonText,
    toSimpleDatabase,
 )
import UnitConversion (UnitConfig)

{- | The parent's processes of several products divided by the model, each
block under its own property, each piece keyed as a load keys it: the block's
activity, the piece's product.

Only those processes are divided again. A process of one product was already
cut to its declared share when the parent was read, and dividing it a second
time would apply that share twice.

A block its property cannot weigh stays whole and loses its column, and is
named. An exception that can weigh none of the blocks it takes is refused: the
child would carry its reason and nothing it did.

The pieces are then linked by name among themselves: an input that consumed a
coproduct found no supplier in the parent, whose name index holds reference
products only, and finds the piece that now produces it. An input the parent
already linked keeps its supplier.
-}
childOf :: UnitConfig -> SystemModel -> Database -> IO (Either Text Database)
childOf unitConfig model parent = case resolveModel model activities of
    Left problems -> pure (Left (T.intercalate "; " (NE.toList problems)))
    Right resolution -> case divide resolution of
        Left refusal -> pure (Left refusal)
        Right pieces -> do
            reportRefusals (refused resolution)
            linked <- linkWithinDatabaseByName unitConfig simple{sdbActivities = pieces}
            pure (rebuildFromActivities unitConfig parent (sdbActivities linked))
  where
    simple :: SimpleDatabase
    simple = toSimpleDatabase parent

    activities :: M.Map UUID Activity
    activities = parentProcesses parent

    allocating :: AllocationProperty -> Allocating
    allocating prop = Allocating{alKey = ByProperty prop, alUnitConfig = unitConfig, alUnitDB = sdbUnits simple}

    divide :: Resolution -> Either Text (M.Map (UUID, UUID) Activity)
    divide resolution = do
        mapM_ (refuseIdle (M.fromList (refused resolution))) (rsTaken resolution)
        keyed
            [ keyedPiece
            | (k@(activityId, _), act) <- M.toList (sdbActivities simple)
            , keyedPiece <- case M.lookup activityId (rsKeys resolution) of
                Just prop -> [((activityId, getReferenceProductUUID piece), piece) | piece <- NE.toList (allocate (allocating prop) act)]
                Nothing -> [(k, act)]
            ]

    -- Each block its property could not weigh, and why.
    refused :: Resolution -> [(UUID, Text)]
    refused resolution =
        [ (activityId, name <> " has no column -- " <> describePropertyRefusal prop reason)
        | (activityId, prop) <- M.toList (rsKeys resolution)
        , Just act <- [M.lookup activityId activities]
        , (name, reason) <- propertyKeyRefusals (allocating prop) [act]
        ]

    refuseIdle :: M.Map UUID Text -> (ModelException, [UUID]) -> Either Text ()
    refuseIdle whys (ex, taken)
        | all (`M.member` whys) taken =
            Left $
                "the "
                    <> propertyName (exKey ex)
                    <> " exception \""
                    <> reasonText (exReason ex)
                    <> "\" divides none of the "
                    <> T.pack (show (S.size (S.fromList taken)))
                    <> " blocks it takes: "
                    <> T.intercalate "; " (take 3 (M.elems (M.restrictKeys whys (S.fromList taken))))
        | otherwise = Right ()

    -- Loud rather than silent: such a block keeps every share its source
    -- declared and still loses its column.
    reportRefusals :: [(UUID, Text)] -> IO ()
    reportRefusals whys =
        if null whys
            then pure ()
            else do
                reportProgress Warning (show (length whys) <> " blocks were not divided: their key names a property they do not carry")
                mapM_ (reportProgress Warning . T.unpack . snd) (take 10 whys)

    -- Two product rows of one block naming the same flow would key two pieces
    -- alike, and a map would keep one inventory in silence.
    keyed :: [((UUID, UUID), Activity)] -> Either Text (M.Map (UUID, UUID) Activity)
    keyed divided = case M.elems (M.filter ((> 1) . NE.length) (M.fromListWith (<>) [(k, pure act) | (k, act) <- divided])) of
        [] -> Right (M.fromList divided)
        clash : _ -> Left (activityName (NE.head clash) <> " names one product on two rows: its pieces cannot be told apart")
