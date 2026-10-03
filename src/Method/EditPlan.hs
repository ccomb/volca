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
    CategoryDraft (..),
    CategoryEdit (..),
    planCategoryEdit,

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

import Control.Monad (when)
import Data.List (elemIndex, elemIndices, find, findIndex)
import Data.Maybe (listToMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID

import Config (MethodPatch (..))
import Method.Journal (
    LineKind (..),
    MethodLine (..),
    MethodOp (..),
    Regionalization (..),
    Restore (..),
    describeFactor,
    nameIsFree,
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

-- | A category to create: it starts with no factor.
data CategoryDraft = CategoryDraft
    { cdName :: Text
    , cdUnit :: Text
    , cdImpactCategory :: Maybe Text
    -- ^ The impact category it belongs to; its name when absent, as most files write it.
    , cdMethodology :: Maybe Text
    }
    deriving (Eq, Show)

-- | A change asked of a collection's categories.
data CategoryEdit
    = NewCategory CategoryDraft
    | Rename UUID Text
    | ChangeUnit UUID Text
    | Delete UUID
    deriving (Eq, Show)

{- | The line a change to a category records, or why it cannot be recorded. A
new category takes the identifier it is given, drawn by the caller, so that
planning stays pure. A name or a unit is read with its edges trimmed; a change
that would change nothing is refused, as it would only crowd the history.
-}
planCategoryEdit :: UUID -> MethodCollection -> CategoryEdit -> Either Text (MethodOp, EditEffect)
planCategoryEdit fresh collection = \case
    NewCategory draft -> do
        name <- nonBlank "name" (cdName draft)
        unit <- nonBlank "unit" (cdUnit draft)
        nameIsFree collection name
        let method = Method fresh name Nothing unit (maybe name T.strip (cdImpactCategory draft)) (cdMethodology draft) []
        pure (AddCategory Nothing method Regionalized, EditEffect 0 Nothing Nothing)
    Rename category asked -> do
        method <- oneCategory collection category
        name <- nonBlank "name" asked
        when (name == methodName method) $ Left (methodName method <> " is already named " <> name)
        nameIsFree collection name
        pure (RenameCategory category (methodName method) name, EditEffect 0 Nothing Nothing)
    ChangeUnit category asked -> do
        method <- oneCategory collection category
        unit <- nonBlank "unit" asked
        when (unit == methodUnit method) $ Left (methodName method <> " is already in " <> unit)
        pure (SetCategoryUnit category (methodUnit method) unit, EditEffect 0 Nothing Nothing)
    Delete category -> do
        method <- oneCategory collection category
        position <- positionOf collection method
        pure (RemoveCategory position method (rankIn collection (methodName method)), EditEffect (length (methodFactors method)) Nothing Nothing)
  where
    nonBlank :: Text -> Text -> Either Text Text
    nonBlank field raw
        | T.null (T.strip raw) = Left ("a category needs a " <> field)
        | otherwise = Right (T.strip raw)

-- | Where a category stands among the collection's categories.
positionOf :: MethodCollection -> Method -> Either Text Int
positionOf collection method =
    maybe (Left (methodName method <> " is no longer in the collection")) Right (findIndex ((== methodId method) . methodId) (mcMethods collection))

-- | Where a category stands among the unregionalized ones now.
rankIn :: MethodCollection -> Text -> Regionalization
rankIn collection name = maybe Regionalized UnregionalizedAt (elemIndex name (mcUnregionalized collection))

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
    -- The category as it was added: factors added since make the removal
    -- refuse, and the refusal names the line that added them.
    AddCategory _ method _ -> do
        position <- positionOf collection method
        Right (UndoWith (RemoveCategory position method (rankIn collection (methodName method))))
    RenameCategory category before after -> Right (UndoWith (RenameCategory category after before))
    SetCategoryUnit category before after -> Right (UndoWith (SetCategoryUnit category after before))
    RemoveCategory position method regionalization -> Right (UndoWith (AddCategory (Just position) method regionalization))

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
a factor or a category it changed: that line, named, so its author knows which
undo to ask for first. Any other reason is left as the replay gave it.
-}
blockedUndo :: MethodCollection -> [MethodLine] -> Int -> Text -> Text
blockedUndo collection lines' target reason =
    maybe reason refusal . listToMaybe . reverse $
        [ (i, reach)
        | (i, l, True) <- zip3 [1 ..] lines' (inEffect lines')
        , i > target
        , reach <- reachOf collection (mlOp l)
        , any (`blocks` reach) undone
        ]
  where
    undone :: [Reach]
    undone = foldMap (reachOf collection . mlOp) (take 1 (drop (target - 1) lines'))
    refusal :: (Int, Reach) -> Text
    refusal (i, reach) =
        "line "
            <> T.pack (show target)
            <> " cannot be undone alone: line "
            <> T.pack (show i)
            <> " has changed "
            <> describeReach reach
            <> " since; undo line "
            <> T.pack (show i)
            <> " first"

{- | What a line reaches: a factor of a category, a category's name or unit,
or a category's existence.
-}
data Reach
    = FactorOf UUID MethodCF
    | DefinitionOf UUID Text
    | ExistenceOf UUID Text

-- | What a line reaches; a selector's factors are read from the collection.
reachOf :: MethodCollection -> MethodOp -> [Reach]
reachOf collection = \case
    SetFactor category factor _ -> [FactorOf category factor]
    RemoveFactor category _ factor -> [FactorOf category factor]
    AddFactor category _ factor -> [FactorOf category factor]
    RestoreFactors restores -> [FactorOf (rsMethod r) (rsFactor r) | r <- restores]
    PatchFactors patch _ ->
        [FactorOf (methodId method) factor | method <- mcMethods collection, factor <- methodFactors method, cfMatches (mpMatch patch) (methodName method) factor]
    AddCategory _ method _ -> [ExistenceOf (methodId method) (methodName method)]
    RemoveCategory _ method _ -> [ExistenceOf (methodId method) (methodName method)]
    RenameCategory category _ after -> [DefinitionOf category after]
    SetCategoryUnit category _ _ -> [DefinitionOf category (nameOf category)]
    SetGlobalMethods _ _ -> []
    CreateScoringSet _ -> []
    RemoveScoringSet _ -> []
  where
    nameOf :: UUID -> Text
    nameOf category = maybe (UUID.toText category) methodName (find ((== category) . methodId) (mcMethods collection))

{- | Whether a later line reaching the second blocks undoing a line reaching
the first. A factor is named by its category's identifier, so a rename or a
change of unit never blocks a factor's undo, nor a factor a rename's; the
category's removal or addition blocks everything in it.
-}
blocks :: Reach -> Reach -> Bool
blocks undone later = case (undone, later) of
    (FactorOf c f, FactorOf c' g) -> c == c' && sameAddress f g
    (FactorOf c _, ExistenceOf c' _) -> c == c'
    (FactorOf _ _, DefinitionOf _ _) -> False
    (DefinitionOf _ _, FactorOf _ _) -> False
    (DefinitionOf c _, DefinitionOf c' _) -> c == c'
    (DefinitionOf c _, ExistenceOf c' _) -> c == c'
    (ExistenceOf c _, other) -> c == categoryOf other
  where
    categoryOf :: Reach -> UUID
    categoryOf = \case
        FactorOf c' _ -> c'
        DefinitionOf c' _ -> c'
        ExistenceOf c' _ -> c'

describeReach :: Reach -> Text
describeReach = \case
    FactorOf _ factor -> describeFactor factor
    DefinitionOf _ name -> name
    ExistenceOf _ name -> name

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
    AddCategory _ method _ -> EditEffect (length (methodFactors method)) Nothing Nothing
    RemoveCategory _ method _ -> EditEffect (length (methodFactors method)) Nothing Nothing
    RenameCategory{} -> EditEffect 0 Nothing Nothing
    SetCategoryUnit{} -> EditEffect 0 Nothing Nothing

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
