{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Method.EditPlan
Description : What a requested change to a method collection records, and its undo

Everything here is decided against the collection in use, before anything is
written: a change a replay could not apply again is refused now, naming what
it could not choose between, rather than written and found wrong at the next
load.
-}
module Method.EditPlan (
    -- * A change asked for
    FactorTarget (..),
    FactorEdit (..),
    EditEffect (..),
    planEdit,

    -- * Undoing
    inEffect,
    undoTarget,
    Undo (..),
    inverseOf,
    restoreOf,
    blockedUndo,
    undoEffect,

    -- * A copy's first lines
    seedLines,
) where

import Data.List (elemIndices)
import Data.Maybe (listToMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)

import Config (MethodPatch (..))
import Method.Journal (
    LineKind (..),
    MethodLine (..),
    MethodOp (..),
    Restore (..),
    describeFactor,
    oneCategory,
    sameAddress,
 )
import Method.Patch (applyMethodPatches, cfMatches, describePatch)
import Method.Types (Method (..), MethodCF (..), MethodCollection (..), ScoringSet (..), ScoringSetOrigin (..))
import Types (applyPatchOp)

{- | One factor, as a caller names it: its category, its flow and its place,
and its value when several factors answer at that place.
-}
data FactorTarget = FactorTarget
    { ftCategory :: UUID
    , ftFlow :: UUID
    , ftLocation :: Maybe Text
    , ftValue :: Maybe Double
    }
    deriving (Eq, Show)

-- | A change asked of a collection's factors.
data FactorEdit
    = SetValue FactorTarget Double
    | Remove FactorTarget
    | Add UUID MethodCF
    | Patch MethodPatch
    deriving (Eq, Show)

-- | What a change did, as its caller reads it.
data EditEffect = EditEffect
    { eeTouched :: Int
    , eeBefore :: Maybe Double
    , eeAfter :: Maybe Double
    }
    deriving (Eq, Show)

-- | The line a change records, and what it does; or why it cannot be recorded.
planEdit :: MethodCollection -> FactorEdit -> Either Text (MethodOp, EditEffect)
planEdit collection = \case
    -- Two identical factors can only be changed together, by a selector:
    -- a value that makes them so leaves no change of value to undo it with.
    SetValue target value -> do
        (category, position, factor) <- locate collection target
        method <- oneCategory collection category
        let changed = factor{mcfValue = value}
        case [other | (i, other) <- zip [0 ..] (methodFactors method), i /= position, other == changed] of
            [] -> pure (SetFactor category factor value, EditEffect 1 (Just (mcfValue factor)) (Just value))
            _ ->
                Left $
                    describeFactor changed
                        <> " is already in "
                        <> methodName method
                        <> ": the two would be identical and could no longer be changed one by one; remove this one instead"
    Remove target -> do
        (category, position, factor) <- locate collection target
        pure (RemoveFactor category position factor, EditEffect 1 (Just (mcfValue factor)) Nothing)
    -- Only here, never at replay: undoing the removal of one of two factors at
    -- one address has to be able to put it back beside the other.
    Add category factor -> do
        method <- oneCategory collection category
        case filter (sameAddress factor) (methodFactors method) of
            [] -> pure (AddFactor category Nothing factor, EditEffect 1 Nothing (Just (mcfValue factor)))
            held ->
                Left $
                    "a factor of "
                        <> methodName method
                        <> " is already written for this flow at this place ("
                        <> T.intercalate "; " (map describeFactor held)
                        <> "): change its value instead"
    -- A selector replayed from a configuration may touch nothing; one asked
    -- for now and touching nothing is a mistake its author wants to hear about.
    Patch patch -> case snd (applyMethodPatches [patch] collection) of
        [(_, touched)] | touched > 0 -> pure (PatchFactors patch touched, EditEffect touched Nothing Nothing)
        _ -> Left ("\"" <> describePatch patch <> "\" touches no factor of this collection")

-- | The one factor a target names, with its category and position.
locate :: MethodCollection -> FactorTarget -> Either Text (UUID, Int, MethodCF)
locate collection target = do
    method <- oneCategory collection (ftCategory target)
    let addressed =
            [ (i, factor)
            | (i, factor) <- zip [0 ..] (methodFactors method)
            , mcfFlowRef factor == ftFlow target
            , mcfConsumerLocation factor == ftLocation target
            ]
    case maybe addressed (\v -> filter ((== v) . mcfValue . snd) addressed) (ftValue target) of
        [(i, factor)] -> Right (methodId method, i, factor)
        [] -> Left (absent method addressed)
        several@((_, first) : _)
            | all ((== first) . snd) several ->
                Left $
                    "the factor "
                        <> describeFactor first
                        <> " is written "
                        <> T.pack (show (length several))
                        <> " times, identically, in "
                        <> methodName method
                        <> ": none can be changed alone, only a selector reaches them together"
            | Just _ <- ftValue target ->
                Left $
                    "several factors of "
                        <> methodName method
                        <> " at this place have this value: "
                        <> T.intercalate "; " (map (describeFactor . snd) several)
                        <> ". None can be changed alone, only a selector reaches them together"
            | otherwise ->
                Left $
                    "several factors of "
                        <> methodName method
                        <> " answer at this place: "
                        <> T.intercalate "; " (map (describeFactor . snd) several)
                        <> ". Name the value of the one to change."
  where
    absent :: Method -> [(Int, MethodCF)] -> Text
    absent method [] = "no factor of " <> methodName method <> " for this flow at this place"
    absent method found =
        "no factor of " <> methodName method <> " at this place has that value; it holds " <> T.intercalate "; " (map (describeFactor . snd) found)

{- | Which lines are in effect: all but those a later line in effect undoes.
Read from the end, since only a line in effect can take another out.
-}
inEffect :: [MethodLine] -> [Bool]
inEffect lines' = reverse (go S.empty (reverse (zip [1 ..] lines')))
  where
    go :: S.Set Int -> [(Int, MethodLine)] -> [Bool]
    go _ [] = []
    go undone ((i, l) : rest)
        | i `S.member` undone = False : go undone rest
        | otherwise = True : go (undoing (mlKind l) undone) rest
    undoing :: LineKind -> S.Set Int -> S.Set Int
    undoing = \case
        Undoing k -> S.insert k
        Change -> id
        TakenFromConfiguration -> id

{- | The line an undo takes out. Without a number, the latest change in effect:
never an undo (so repeated undos walk back rather than toggle) and never what
a copy took from its source's configuration (so a fresh copy of a configured
collection keeps scoring like it). A copy of an uploaded collection or of
another copy takes its source's journal as it is, changes included, and those
changes can be undone like its own. With a number, that line, whatever it is,
if it is in effect: undoing an undo is how a change is redone.
-}
undoTarget :: [MethodLine] -> Maybe Int -> Either Text Int
undoTarget lines' = \case
    Nothing ->
        maybe (Left "there is no change left to undo") Right . listToMaybe . reverse $
            [i | (i, l, True) <- zip3 [1 ..] lines' (inEffect lines'), mlKind l == Change]
    Just k -> case drop (k - 1) (zip lines' (inEffect lines')) of
        (_, True) : _ | k >= 1 -> Right k
        (_, False) : _ | k >= 1 -> Left ("line " <> T.pack (show k) <> " is already undone")
        _ -> Left ("the journal has no line " <> T.pack (show k))

{- | How a line is undone. Most are undone by a line computed from the
collection as it is now; a selector by the values it replaced, which only the
collection just before it knows ('restoreOf').
-}
data Undo = UndoWith MethodOp | UndoSelector MethodPatch
    deriving (Eq, Show)

inverseOf :: MethodCollection -> MethodOp -> Either Text Undo
inverseOf collection = \case
    SetFactor category factor value -> Right (UndoWith (SetFactor category factor{mcfValue = value} (mcfValue factor)))
    PatchFactors patch _ -> Right (UndoSelector patch)
    RestoreFactors restores ->
        Right (UndoWith (RestoreFactors [r{rsFactor = (rsFactor r){mcfValue = rsValue r}, rsValue = mcfValue (rsFactor r)} | r <- restores]))
    SetGlobalMethods before after -> Right (UndoWith (SetGlobalMethods after before))
    RemoveFactor category position factor -> Right (UndoWith (AddFactor category (Just position) factor))
    AddFactor category _ factor -> do
        method <- oneCategory collection category
        case elemIndices factor (methodFactors method) of
            [position] -> Right (UndoWith (RemoveFactor category position factor))
            _ -> Left (describeFactor factor <> " is no longer in " <> methodName method <> " as it was added")
    -- A set comes back at the end of the list. Only a copy's first lines
    -- create one in this version, and they are at the end already.
    CreateScoringSet set -> Right (UndoWith (RemoveScoringSet set))
    RemoveScoringSet set -> Right (UndoWith (CreateScoringSet set))

{- | The values a selector replaced, read from the collection just before it.

The positions are those before the selector, which are also those after it: a
selector moves nothing. Undoing in reverse order always finds them again; an
undo out of order, after an addition or a removal in the same category, is
refused by the restore, which names the factor, rather than written to the
wrong place.
-}
restoreOf :: MethodPatch -> MethodCollection -> [Restore]
restoreOf patch before =
    [ Restore (methodId method) i factor{mcfValue = applyPatchOp (mpOp patch) (mcfValue factor)} (mcfValue factor)
    | method <- mcMethods before
    , (i, factor) <- zip [0 ..] (methodFactors method)
    , cfMatches (mpMatch patch) (methodName method) factor
    ]

{- | Why line @target@ cannot be undone, when a later line in effect changed
one of its factors since: that line, named, so its author knows which undo to
ask for first. Any other reason is left as the replay gave it.
-}
blockedUndo :: MethodCollection -> [MethodLine] -> Int -> Text -> Text
blockedUndo collection lines' target reason =
    maybe reason refusal . listToMaybe . reverse $
        [ (i, factor)
        | (i, l, True) <- zip3 [1 ..] lines' (inEffect lines')
        , i > target
        , (category, factor) <- touchedBy collection (mlOp l)
        , any (\(c, f) -> c == category && sameAddress f factor) undone
        ]
  where
    undone :: [(UUID, MethodCF)]
    undone = foldMap (touchedBy collection . mlOp) (take 1 (drop (target - 1) lines'))
    refusal :: (Int, MethodCF) -> Text
    refusal (i, factor) =
        "line "
            <> T.pack (show target)
            <> " cannot be undone alone: line "
            <> T.pack (show i)
            <> " has changed "
            <> describeFactor factor
            <> " since; undo line "
            <> T.pack (show i)
            <> " first"

-- | The factors a line touches, with their category; a selector's are read from the collection.
touchedBy :: MethodCollection -> MethodOp -> [(UUID, MethodCF)]
touchedBy collection = \case
    SetFactor category factor _ -> [(category, factor)]
    RemoveFactor category _ factor -> [(category, factor)]
    AddFactor category _ factor -> [(category, factor)]
    RestoreFactors restores -> [(rsMethod r, rsFactor r) | r <- restores]
    PatchFactors patch _ ->
        [(methodId method, factor) | method <- mcMethods collection, factor <- methodFactors method, cfMatches (mpMatch patch) (methodName method) factor]
    SetGlobalMethods _ _ -> []
    CreateScoringSet _ -> []
    RemoveScoringSet _ -> []

-- | What writing a line does, in the terms a change reports.
undoEffect :: MethodOp -> EditEffect
undoEffect = \case
    SetFactor _ factor value -> EditEffect 1 (Just (mcfValue factor)) (Just value)
    RemoveFactor _ _ factor -> EditEffect 1 (Just (mcfValue factor)) Nothing
    AddFactor _ _ factor -> EditEffect 1 Nothing (Just (mcfValue factor))
    RestoreFactors restores -> EditEffect (length restores) Nothing Nothing
    PatchFactors _ touched -> EditEffect touched Nothing Nothing
    SetGlobalMethods _ _ -> EditEffect 0 Nothing Nothing
    CreateScoringSet _ -> EditEffect 0 Nothing Nothing
    RemoveScoringSet _ -> EditEffect 0 Nothing Nothing

{- | The first lines of a copy of a collection the configuration declares: what
the configuration adds to the files, in the order it applies them (the
scoring sets after the file's, then the patches, each with the number of
factors it touches on the files, then the unregionalized categories). A copy
replayed from these scores exactly like its source.
-}
seedLines :: [ScoringSet] -> [MethodPatch] -> [Text] -> MethodCollection -> [MethodOp]
seedLines sets patches unregionalized parsed =
    map (\s -> CreateScoringSet s{ssOrigin = CreatedInJournal}) sets
        <> [PatchFactors patch touched | (patch, touched) <- snd (applyMethodPatches patches parsed)]
        <> [SetGlobalMethods [] unregionalized | not (null unregionalized)]
