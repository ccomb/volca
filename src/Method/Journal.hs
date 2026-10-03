{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Method.Journal
Description : The changes made to a method collection, and how to apply them again

A method collection under the uploads directory is its files plus a journal of
the changes made to it, never a rewritten copy of those files. Loading it
parses the files, then replays the journal over them, one line at a time.

A line records a gesture and what it found: a factor in full, as it stood, and
the value written over it; a selector with the number of factors it touched; a
factor removed, in full, and the position it held. That is what makes a replay
checkable. When the files no longer hold what a line recorded (a factor
missing, or written twice identically, or a selector touching another number)
the load stops and names the line, rather than apply the change to something
else.

A factor is addressed by its whole value, not by its flow alone: a SimaPro
file can write one flow twice at one place with two values, and only the value
tells the two apart. Two factors identical in every field cannot be addressed
one by one at all, and a line never tries.

The file itself, its version check and its torn last line are
'Data.JournalFile'; the version of these words is 1.
-}
module Method.Journal (
    -- * What a line records
    MethodOp (..),
    Restore (..),
    Regionalization (..),
    LineKind (..),
    MethodLine (..),

    -- * Applying it
    applyMethodOp,
    replayMethodJournal,

    -- * Shared with the planning of a change
    opName,
    opCategory,
    describeFactor,
    oneCategory,
    sameAddress,
) where

import Control.Monad (foldM, unless, when, zipWithM)
import Data.Aeson (FromJSON, Key, Object, ToJSON, Value, object, withObject, (.:), (.:?), (.=))
import Data.Aeson.Types (Pair, Parser)
import Data.Bifunctor (first)
import Data.Containers.ListUtils (nubOrd)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID

import Config (MethodPatch (..), MethodPatchMatch (..))
import Data.JournalFile (Entry (..), JournalVocabulary (..))
import Method.Patch (applyMethodPatches)
import Method.Scoring (
    NumberEntry (..),
    ScoringChange (..),
    ScoringGesture (..),
    ScoringKey,
    TextEntry (..),
    applyChange,
    checkSet,
    keyOf,
 )
import Method.Types (
    Compartment (..),
    FlowDirection (..),
    Method (..),
    MethodCF (..),
    MethodCollection (..),
    ScoringSet (..),
    ScoringSetOrigin (..),
 )
import Types (PatchOp (..))

-- ---------------------------------------------------------------------------
-- What a line records
-- ---------------------------------------------------------------------------

-- | The ways a method collection changes, each with what it found.
data MethodOp
    = -- | The category, the factor as it stood (its value is the old one), and the value written.
      SetFactor UUID MethodCF Double
    | -- | A selector and what it does, with the number of factors it touched.
      PatchFactors MethodPatch Int
    | {- | Values given back, each at its position in its category, with the
      factor as it stands there. The undo of a selector: dividing by a scale
      does not always land on the value it multiplied, and a set value erased
      what it replaced.
      -}
      RestoreFactors [Restore]
    | -- | The unregionalized categories found, and the ones written.
      SetGlobalMethods [Text] [Text]
    | -- | The category, the position the factor held, and the factor, in full.
      RemoveFactor UUID Int MethodCF
    | -- | The category, the position to insert at (the end when absent), and the factor.
      AddFactor UUID (Maybe Int) MethodCF
    | -- | A scoring set added at the end of the list.
      CreateScoringSet ScoringSet
    | -- | A scoring set taken out, in full.
      RemoveScoringSet ScoringSet
    | -- | A category at a position (the end when absent), in full, factors included, with its place among the unregionalized categories.
      AddCategory (Maybe Int) Method Regionalization
    | {- | The category, the name it had and the one written. The scoring sets'
      variables and the unregionalized categories that named it follow, in the
      same line: left behind, a set would stop scoring without a word.
      -}
      RenameCategory UUID Text Text
    | -- | The category, the unit it had and the one written.
      SetCategoryUnit UUID Text Text
    | -- | The position the category held, the category in full, and its place among the unregionalized categories.
      RemoveCategory Int Method Regionalization
    | {- | One gesture on one scoring set, named as it was before the line: the
      changes apply in order, all or none, each to an entry of its own, and
      the set must still score after.
      -}
      ChangeScoringSet Text ScoringGesture (NonEmpty ScoringChange)
    deriving (Eq, Show)

-- | One value given back by 'RestoreFactors'.
data Restore = Restore
    { rsMethod :: UUID
    , rsPosition :: Int
    , rsFactor :: MethodCF
    -- ^ The factor as it stands at that position before the restore.
    , rsValue :: Double
    -- ^ The value it gets back.
    }
    deriving (Eq, Show)

{- | Whether a category is scored without regionalization, and if so its rank
in that list: a category put back returns to the same rank, so the list reads
the same as before it left.
-}
data Regionalization = Regionalized | UnregionalizedAt Int
    deriving (Eq, Show)

-- | Why a line is in the journal.
data LineKind
    = -- | A change someone asked for.
      Change
    | -- | The inverse of the line with this number (from 1), written by an undo.
      Undoing Int
    | {- | Written by a copy, carrying what its source's configuration added to
      the files. Undoing the latest change never reaches it; naming its line does.
      -}
      TakenFromConfiguration
    deriving (Eq, Show)

data MethodLine = MethodLine
    { mlOp :: MethodOp
    , mlKind :: LineKind
    }
    deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Applying it
-- ---------------------------------------------------------------------------

{- | Replay a journal over the collection its files parse to, each line seeing
the result of the ones before it. Any line that does not apply stops the whole
load, naming the line: a half-replayed collection would score something no one
asked for.
-}
replayMethodJournal :: MethodCollection -> [Entry MethodLine] -> Either Text MethodCollection
replayMethodJournal start = foldM step start . zip [1 :: Int ..]
  where
    step :: MethodCollection -> (Int, Entry MethodLine) -> Either Text MethodCollection
    step collection (i, entry) =
        first (\msg -> "journal line " <> showT i <> " (" <> opName (mlOp (jeOp entry)) <> "): " <> msg) $
            applyMethodOp collection (mlOp (jeOp entry))

-- | Apply one line, or say why the collection does not hold what it recorded.
applyMethodOp :: MethodCollection -> MethodOp -> Either Text MethodCollection
applyMethodOp collection = \case
    SetFactor category factor value ->
        inCategory category (replaceFactor factor (\found -> found{mcfValue = value})) collection
    PatchFactors patch touched -> case applyMethodPatches [patch] collection of
        (patched, stats)
            | map snd stats == [touched] -> Right patched
            | otherwise -> Left (touchDrift touched (sum (map snd stats)))
    RestoreFactors restores -> foldM restoreCategory collection (M.toList (byCategory restores))
    SetGlobalMethods before after
        | mcUnregionalized collection == before -> Right collection{mcUnregionalized = after}
        | otherwise ->
            Left $
                "recorded with the unregionalized categories "
                    <> listed before
                    <> " but the collection has "
                    <> listed (mcUnregionalized collection)
    RemoveFactor category position factor -> inCategory category (removeAt position factor) collection
    AddFactor category position factor -> inCategory category (addAt position factor) collection
    CreateScoringSet set
        | ssName set `elem` map ssName (mcScoringSets collection) ->
            Left ("a scoring set named '" <> ssName set <> "' is already there")
        | otherwise -> Right collection{mcScoringSets = mcScoringSets collection <> [set]}
    RemoveScoringSet set
        | any (sameSet set) (mcScoringSets collection) ->
            Right collection{mcScoringSets = filter (not . sameSet set) (mcScoringSets collection)}
        | otherwise -> Left ("the scoring set '" <> ssName set <> "' is not there as recorded")
    AddCategory position method regionalization -> addCategory position method regionalization collection
    RenameCategory category before after -> renameCategory category before after collection
    SetCategoryUnit category before after -> inCategory category (setUnit before after) collection
    RemoveCategory position method regionalization -> removeCategory position method regionalization collection
    ChangeScoringSet name _ changes -> changeScoringSet name changes collection

{- | Two scoring sets alike but for their origin, which a journal does not
keep: a set read from a file, once written in a line, reads back as one the
journal created.
-}
sameSet :: ScoringSet -> ScoringSet -> Bool
sameSet a b = a{ssOrigin = CreatedInJournal} == b{ssOrigin = CreatedInJournal}

{- | Apply a line's changes to the one set it names, then check the set still
scores. What the line itself writes is held to more than what the set already
held: a category it names must be one of the collection, a number it sets
must be one a score can use, and a new name must be free. A set read from a
file may already hold a normalization of zero, and keeps it.
-}
changeScoringSet :: Text -> NonEmpty ScoringChange -> MethodCollection -> Either Text MethodCollection
changeScoringSet name changes collection = do
    unless (length (nubOrd keys) == length keys) $
        Left ("the line changes one entry of the scoring set '" <> name <> "' twice")
    set <- case filter ((== name) . ssName) (mcScoringSets collection) of
        [one] -> Right one
        [] -> Left ("there is no scoring set named '" <> name <> "'; the collection has " <> listed (map ssName (mcScoringSets collection)))
        _ -> Left ("several scoring sets are named '" <> name <> "'")
    changed <- foldM (flip applyChange) set changes
    mapM_ written changes
    checkSet changed
    pure collection{mcScoringSets = map (\s -> if ssName s == name then changed else s) (mcScoringSets collection)}
  where
    keys :: [ScoringKey]
    keys = map keyOf (NE.toList changes)

    written :: ScoringChange -> Either Text ()
    written = \case
        SetText entry variable _ after -> textWritten entry variable after
        SetNumber entry variable _ after -> numberWritten entry variable after
        SetDisplayMultiplier _ after -> maybe (Right ()) multiplier after
        SetUnitOfSet _ _ -> Right ()
        RenameSet _ after ->
            when (after `elem` map ssName (mcScoringSets collection)) $
                Left ("a scoring set named '" <> after <> "' is already there")

    textWritten :: TextEntry -> Text -> Maybe Text -> Either Text ()
    textWritten CategoryOf variable (Just category) =
        unless (category `elem` map methodName (mcMethods collection)) $
            Left ("the variable '" <> variable <> "' reads '" <> category <> "', which is no impact category of this collection")
    textWritten CategoryOf _ Nothing = Right ()
    textWritten FormulaOf _ _ = Right ()
    textWritten LabelOf _ _ = Right ()
    textWritten VariableUnitOf _ _ = Right ()
    textWritten ScoreOf _ _ = Right ()

    numberWritten :: NumberEntry -> Text -> Maybe Double -> Either Text ()
    numberWritten NormalizationOf variable (Just n) =
        when (n == 0 || isNaN n || isInfinite n) $
            Left ("the normalization of '" <> variable <> "' is " <> showT n <> ", which no score can divide by")
    numberWritten WeightOf variable (Just w) =
        when (isNaN w || isInfinite w) $
            Left ("the weight of '" <> variable <> "' is " <> showT w <> ", which is not a number a score can use")
    numberWritten NormalizationOf _ Nothing = Right ()
    numberWritten WeightOf _ Nothing = Right ()

    multiplier :: Double -> Either Text ()
    multiplier m =
        when (m == 0 || isNaN m || isInfinite m) $
            Left ("the display multiplier is " <> showT m <> ", which shows no score")

{- | The one category an identifier names. Two categories under one identifier
would make a line mean two things, so that is a refusal too.
-}
oneCategory :: MethodCollection -> UUID -> Either Text Method
oneCategory collection category = case filter ((== category) . methodId) (mcMethods collection) of
    [method] -> Right method
    [] -> Left ("no impact category " <> UUID.toText category <> " in this collection")
    several ->
        Left ("impact categories " <> T.intercalate ", " (map methodName several) <> " share the identifier " <> UUID.toText category)

-- | Change the one category a line names.
inCategory :: UUID -> (Method -> Either Text Method) -> MethodCollection -> Either Text MethodCollection
inCategory category change collection = do
    changed <- change =<< oneCategory collection category
    pure collection{mcMethods = map (\m -> if methodId m == category then changed else m) (mcMethods collection)}

-- | Insert a category, refusing an identifier or a name the collection already holds.
addCategory :: Maybe Int -> Method -> Regionalization -> MethodCollection -> Either Text MethodCollection
addCategory position method regionalization collection = do
    unless (all ((/= methodId method) . methodId) (mcMethods collection)) $
        Left ("an impact category already has the identifier " <> UUID.toText (methodId method))
    nameIsFree collection (methodName method)
    at <- maybe (Right (length (mcMethods collection))) within position
    unregionalized <- enterUnregionalized regionalization (methodName method) (mcUnregionalized collection)
    pure collection{mcMethods = insertAt at method (mcMethods collection), mcUnregionalized = unregionalized}
  where
    within :: Int -> Either Text Int
    within p
        | p >= 0 && p <= length (mcMethods collection) = Right p
        | otherwise = Left ("position " <> showT p <> " is past the end of the impact categories")

{- | A name nothing in the collection uses yet: no category, and no scoring
set or unregionalized list naming a category the collection does not have.
A name such a set already holds would catch the new category without a
word, and a rename's undo would then take that name away from the set.
-}
nameIsFree :: MethodCollection -> Text -> Either Text ()
nameIsFree collection name
    | any ((== name) . methodName) (mcMethods collection) = Left ("an impact category is already named " <> name)
    | (s : _) <- filter (elem name . M.elems . ssVariables) (mcScoringSets collection) =
        Left ("the scoring set " <> ssName s <> " already names " <> name <> ", which no impact category has")
    | name `elem` mcUnregionalized collection =
        Left ("the unregionalized categories already name " <> name <> ", which no impact category has")
    | otherwise = Right ()

{- | Rename the one category a line names, and every name that pointed at it.
The name has to be its own: two categories sharing it would leave the sets
naming it unable to say which one.
-}
renameCategory :: UUID -> Text -> Text -> MethodCollection -> Either Text MethodCollection
renameCategory category before after collection = do
    method <- oneCategory collection category
    unless (methodName method == before) $
        Left ("the impact category " <> UUID.toText category <> " is named " <> methodName method <> ", not " <> before <> " as recorded")
    unless (length (filter ((== before) . methodName) (mcMethods collection)) == 1) $
        Left ("several impact categories are named " <> before <> ": the scoring sets naming it cannot tell which one")
    nameIsFree collection after
    pure
        collection
            { mcMethods = map (\m -> if methodId m == category then m{methodName = after} else m) (mcMethods collection)
            , mcScoringSets = map (\s -> s{ssVariables = M.map follow (ssVariables s)}) (mcScoringSets collection)
            , mcUnregionalized = map follow (mcUnregionalized collection)
            }
  where
    follow :: Text -> Text
    follow name = if name == before then after else name

setUnit :: Text -> Text -> Method -> Either Text Method
setUnit before after method
    | methodUnit method == before = Right method{methodUnit = after}
    | otherwise = Left (methodName method <> " is in " <> methodUnit method <> ", not " <> before <> " as recorded")

{- | Take out the category at a position, when it is the one recorded, with
its place among the unregionalized ones. A category a scoring set weighs
stays: the set would stop scoring.
-}
removeCategory :: Int -> Method -> Regionalization -> MethodCollection -> Either Text MethodCollection
removeCategory position method regionalization collection = case splitAt position (mcMethods collection) of
    (front, found : back)
        | position >= 0
        , found == method -> do
            unlessWeighed
            unregionalized <- leaveUnregionalized regionalization (methodName method) (mcUnregionalized collection)
            pure collection{mcMethods = front <> back, mcUnregionalized = unregionalized}
    _ -> Left (methodName method <> " is not at position " <> showT position <> " of the impact categories as recorded")
  where
    unlessWeighed :: Either Text ()
    unlessWeighed = case [ssName s | s <- mcScoringSets collection, methodName method `elem` M.elems (ssVariables s)] of
        [] -> Right ()
        sets -> Left (methodName method <> " is weighed by the scoring set " <> T.intercalate ", " sets <> ": take it out of the set first")

enterUnregionalized :: Regionalization -> Text -> [Text] -> Either Text [Text]
enterUnregionalized regionalization name names = case regionalization of
    Regionalized -> Right names
    UnregionalizedAt i
        | i >= 0 && i <= length names -> Right (insertAt i name names)
        | otherwise -> Left ("rank " <> showT i <> " is past the end of the unregionalized categories")

leaveUnregionalized :: Regionalization -> Text -> [Text] -> Either Text [Text]
leaveUnregionalized regionalization name names = case regionalization of
    Regionalized
        | name `notElem` names -> Right names
        | otherwise -> Left (name <> " is scored without regionalization, which the line did not record")
    UnregionalizedAt i -> case splitAt i names of
        (front, found : back) | i >= 0, found == name -> Right (front <> back)
        _ -> Left (name <> " is not at rank " <> showT i <> " of the unregionalized categories as recorded")

insertAt :: Int -> a -> [a] -> [a]
insertAt i x xs = take i xs <> (x : drop i xs)

-- | Replace the one factor equal to the recorded one.
replaceFactor :: MethodCF -> (MethodCF -> MethodCF) -> Method -> Either Text Method
replaceFactor factor change method = case break (== factor) (methodFactors method) of
    (before, found : after)
        | factor `notElem` after -> Right method{methodFactors = before <> (change found : after)}
        | otherwise ->
            Left (describeFactor factor <> " is written twice, identically, in " <> methodName method <> ": nothing tells the two apart")
    (_, []) ->
        Left (describeFactor factor <> " is not in " <> methodName method <> " as recorded: the source has changed under the journal")

-- | Take out the factor at a position, when it is the one recorded.
removeAt :: Int -> MethodCF -> Method -> Either Text Method
removeAt position factor method = case splitAt position (methodFactors method) of
    (before, found : after) | position >= 0, found == factor -> Right method{methodFactors = before <> after}
    _ -> Left (describeFactor factor <> " is not at position " <> showT position <> " of " <> methodName method <> " as recorded")

{- | Insert a factor where a line put it. A replay judges the position only:
undoing the removal of one of two factors at one address puts it back beside
the other, which is the collection as it was. Adding a second factor at an
address is refused when the change is asked ('Method.EditPlan.planEdit').

The position is the rank the factor had; a later addition or removal in the
same category shifts what stands before it, and the factor comes back one
rank off. The values scored are the same; only the order an export writes
moves. Undoing in reverse order never meets this.
-}
addAt :: Maybe Int -> MethodCF -> Method -> Either Text Method
addAt position factor method = case position of
    Nothing -> Right method{methodFactors = methodFactors method <> [factor]}
    Just i
        | i >= 0
        , i <= length (methodFactors method) ->
            Right method{methodFactors = insertAt i factor (methodFactors method)}
        | otherwise -> Left ("position " <> showT i <> " is past the end of " <> methodName method)

-- | One flow at one place: the address a factor is written under.
sameAddress :: MethodCF -> MethodCF -> Bool
sameAddress a b = mcfFlowRef a == mcfFlowRef b && mcfConsumerLocation a == mcfConsumerLocation b

byCategory :: [Restore] -> M.Map UUID (M.Map Int Restore)
byCategory restores = M.fromListWith M.union [(rsMethod r, M.singleton (rsPosition r) r) | r <- restores]

-- | Give back one category's values in one pass, whatever the number restored.
restoreCategory :: MethodCollection -> (UUID, M.Map Int Restore) -> Either Text MethodCollection
restoreCategory collection (category, byPosition) = inCategory category restoreIn collection
  where
    restoreIn :: Method -> Either Text Method
    restoreIn method = do
        let factors = methodFactors method
        unless (all (\i -> i >= 0 && i < length factors) (M.keys byPosition)) $
            Left ("a restored position is past the end of " <> methodName method)
        restored <- zipWithM (restoreOne method) [0 ..] factors
        pure method{methodFactors = restored}
    restoreOne :: Method -> Int -> MethodCF -> Either Text MethodCF
    restoreOne method i found = case M.lookup i byPosition of
        Nothing -> Right found
        Just r
            | found == rsFactor r -> Right found{mcfValue = rsValue r}
            | otherwise ->
                Left (describeFactor (rsFactor r) <> " is no longer at position " <> showT i <> " of " <> methodName method)

touchDrift :: Int -> Int -> Text
touchDrift recorded touched =
    "recorded as touching "
        <> showT recorded
        <> " factors but the same selector now touches "
        <> showT touched
        <> ". The source has changed since the line was written, so replaying it would not be the change that was made."

listed :: [Text] -> Text
listed [] = "none"
listed names = T.intercalate ", " names

-- | A factor in words, for a refusal: its flow, its place and its value.
describeFactor :: MethodCF -> Text
describeFactor factor =
    mcfFlowName factor
        <> maybe "" ((", " <>) . place) (mcfCompartment factor)
        <> maybe "" (", " <>) (mcfConsumerLocation factor)
        <> " at "
        <> showT (mcfValue factor)
  where
    place :: Compartment -> Text
    place (Compartment medium sub qualifier) = T.intercalate "/" (filter (not . T.null) [medium, sub, qualifier])

showT :: (Show a) => a -> Text
showT = T.pack . show

-- | The verb a line is written with.
opName :: MethodOp -> Text
opName = \case
    SetFactor{} -> "set-factor"
    PatchFactors patch _ -> case mpOp patch of
        ScaleBy _ -> "scale-factors"
        SetValueTo _ -> "set-factors"
    RestoreFactors _ -> "restore-factors"
    SetGlobalMethods _ _ -> "set-global-methods"
    RemoveFactor{} -> "remove-factor"
    AddFactor{} -> "add-factor"
    CreateScoringSet _ -> "create-scoring-set"
    RemoveScoringSet _ -> "remove-scoring-set"
    AddCategory{} -> "add-category"
    RenameCategory{} -> "rename-category"
    SetCategoryUnit{} -> "set-category-unit"
    RemoveCategory{} -> "remove-category"
    ChangeScoringSet{} -> "change-scoring-set"

-- | The one category a line names, when it names one: what a change answers with.
opCategory :: MethodOp -> Maybe UUID
opCategory = \case
    SetFactor category _ _ -> Just category
    RemoveFactor category _ _ -> Just category
    AddFactor category _ _ -> Just category
    AddCategory _ method _ -> Just (methodId method)
    RenameCategory category _ _ -> Just category
    SetCategoryUnit category _ _ -> Just category
    RemoveCategory _ method _ -> Just (methodId method)
    PatchFactors _ _ -> Nothing
    RestoreFactors _ -> Nothing
    SetGlobalMethods _ _ -> Nothing
    CreateScoringSet _ -> Nothing
    RemoveScoringSet _ -> Nothing
    ChangeScoringSet{} -> Nothing

-- ---------------------------------------------------------------------------
-- Codec
--
-- Hand-written, and the journal's own: the wire types describing the same
-- changes are free to change shape with the API, while what is already on
-- disk has to keep reading.
-- ---------------------------------------------------------------------------

instance JournalVocabulary MethodLine where
    vocabularyVersion _ = 1
    opFields (MethodLine op kind) = ("op" .= opName op) : kindFields kind <> verbFields op
    parseOp o = do
        verb <- o .: "op"
        MethodLine <$> parseVerb o verb <*> parseKind o

kindFields :: LineKind -> [Pair]
kindFields = \case
    Change -> []
    Undoing k -> ["undoes" .= k]
    TakenFromConfiguration -> ["seed" .= True]

parseKind :: Object -> Parser LineKind
parseKind o = do
    undoes <- o .:? "undoes"
    seed <- o .:? "seed"
    case (undoes, seed) of
        (Nothing, Nothing) -> pure Change
        (Just k, Nothing) -> pure (Undoing k)
        (Nothing, Just True) -> pure TakenFromConfiguration
        (Nothing, Just False) -> fail "\"seed\" is only ever written true"
        (Just _, Just _) -> fail "a line undoes another or comes from the configuration, not both"

verbFields :: MethodOp -> [Pair]
verbFields = \case
    SetFactor category factor value ->
        ["category" .= UUID.toText category, "factor" .= factorJSON factor, "value" .= value]
    PatchFactors patch touched ->
        ["match" .= matchJSON (mpMatch patch), "touched" .= touched]
            <> opValue (mpOp patch)
            <> maybe [] (\d -> ["description" .= d]) (mpDescription patch)
    RestoreFactors restores -> ["factors" .= map restoreJSON restores]
    SetGlobalMethods before after -> ["before" .= before, "after" .= after]
    RemoveFactor category position factor ->
        ["category" .= UUID.toText category, "position" .= position, "factor" .= factorJSON factor]
    AddFactor category position factor ->
        ["category" .= UUID.toText category, "factor" .= factorJSON factor] <> maybe [] (\p -> ["position" .= p]) position
    CreateScoringSet set -> ["set" .= scoringSetJSON set]
    RemoveScoringSet set -> ["set" .= scoringSetJSON set]
    AddCategory position method regionalization ->
        ["category" .= categoryJSON method] <> maybe [] (\p -> ["position" .= p]) position <> regionalizationFields regionalization
    RenameCategory category before after -> ["category" .= UUID.toText category, "before" .= before, "after" .= after]
    SetCategoryUnit category before after -> ["category" .= UUID.toText category, "before" .= before, "after" .= after]
    RemoveCategory position method regionalization ->
        ["category" .= categoryJSON method, "position" .= position] <> regionalizationFields regionalization
    -- "set" names the set here, where a creation or a removal carries it whole.
    ChangeScoringSet name gesture changes ->
        ["set" .= name, "gesture" .= gestureJSON gesture, "changes" .= map changeJSON (NE.toList changes)]
  where
    opValue :: PatchOp -> [Pair]
    opValue = \case
        ScaleBy s -> ["scale" .= s]
        SetValueTo v -> ["value" .= v]

parseVerb :: Object -> Text -> Parser MethodOp
parseVerb o = \case
    "set-factor" -> SetFactor <$> category <*> (o .: "factor" >>= parseFactor) <*> o .: "value"
    "scale-factors" -> patchWith . ScaleBy =<< o .: "scale"
    "set-factors" -> patchWith . SetValueTo =<< o .: "value"
    "restore-factors" -> RestoreFactors <$> (o .: "factors" >>= traverse parseRestore)
    "set-global-methods" -> SetGlobalMethods <$> o .: "before" <*> o .: "after"
    "remove-factor" -> RemoveFactor <$> category <*> o .: "position" <*> (o .: "factor" >>= parseFactor)
    "add-factor" -> AddFactor <$> category <*> o .:? "position" <*> (o .: "factor" >>= parseFactor)
    "create-scoring-set" -> CreateScoringSet <$> (o .: "set" >>= parseScoringSet)
    "remove-scoring-set" -> RemoveScoringSet <$> (o .: "set" >>= parseScoringSet)
    "add-category" -> AddCategory <$> o .:? "position" <*> (o .: "category" >>= parseCategory) <*> regionalization
    "rename-category" -> RenameCategory <$> category <*> o .: "before" <*> o .: "after"
    "set-category-unit" -> SetCategoryUnit <$> category <*> o .: "before" <*> o .: "after"
    "remove-category" -> RemoveCategory <$> o .: "position" <*> (o .: "category" >>= parseCategory) <*> regionalization
    "change-scoring-set" -> do
        changes <- o .: "changes" >>= traverse parseChange
        ChangeScoringSet
            <$> o .: "set"
            <*> (o .: "gesture" >>= parseGesture)
            <*> maybe (fail "a change to a scoring set changes something") pure (NE.nonEmpty changes)
    other -> fail ("unknown method journal operation: " <> T.unpack other)
  where
    category :: Parser UUID
    category = o .: "category" >>= parseUUID
    patchWith :: PatchOp -> Parser MethodOp
    patchWith op = do
        match <- o .: "match" >>= parseMatch
        description <- o .:? "description"
        PatchFactors (MethodPatch description match op) <$> o .: "touched"
    regionalization :: Parser Regionalization
    regionalization = maybe Regionalized UnregionalizedAt <$> o .:? "unregionalized-at"

regionalizationFields :: Regionalization -> [Pair]
regionalizationFields = \case
    Regionalized -> []
    UnregionalizedAt i -> ["unregionalized-at" .= i]

-- | A category in full, its factors included: what an addition puts and a removal takes.
categoryJSON :: Method -> Value
categoryJSON m =
    object $
        [ "id" .= UUID.toText (methodId m)
        , "name" .= methodName m
        , "unit" .= methodUnit m
        , "impact-category" .= methodCategory m
        , "factors" .= map factorJSON (methodFactors m)
        ]
            <> maybe [] (\d -> ["description" .= d]) (methodDescription m)
            <> maybe [] (\d -> ["methodology" .= d]) (methodMethodology m)

parseCategory :: Value -> Parser Method
parseCategory = withObject "impact category" $ \o ->
    Method
        <$> (o .: "id" >>= parseUUID)
        <*> o .: "name"
        <*> o .:? "description"
        <*> o .: "unit"
        <*> o .: "impact-category"
        <*> o .:? "methodology"
        <*> (o .: "factors" >>= traverse parseFactor)

parseUUID :: Text -> Parser UUID
parseUUID raw = maybe (fail ("not an identifier: " <> T.unpack raw)) pure (UUID.fromText raw)

factorJSON :: MethodCF -> Value
factorJSON f =
    object $
        [ "flow" .= UUID.toText (mcfFlowRef f)
        , "name" .= mcfFlowName f
        , "direction" .= directionText (mcfDirection f)
        , "value" .= mcfValue f
        , "unit" .= mcfUnit f
        ]
            <> maybe [] (\c -> ["compartment" .= compartmentJSON c]) (mcfCompartment f)
            <> maybe [] (\cas -> ["cas" .= cas]) (mcfCAS f)
            <> maybe [] (\loc -> ["location" .= loc]) (mcfConsumerLocation f)

parseFactor :: Value -> Parser MethodCF
parseFactor = withObject "factor" $ \o ->
    MethodCF
        <$> (o .: "flow" >>= parseUUID)
        <*> o .: "name"
        <*> (o .: "direction" >>= parseDirection)
        <*> o .: "value"
        <*> (o .:? "compartment" >>= traverse parseCompartment)
        <*> o .:? "cas"
        <*> o .: "unit"
        <*> o .:? "location"

compartmentJSON :: Compartment -> Value
compartmentJSON (Compartment medium sub qualifier) =
    object ["medium" .= medium, "subcompartment" .= sub, "qualifier" .= qualifier]

parseCompartment :: Value -> Parser Compartment
parseCompartment = withObject "compartment" $ \o ->
    Compartment <$> o .: "medium" <*> o .: "subcompartment" <*> o .: "qualifier"

directionText :: FlowDirection -> Text
directionText = \case
    Input -> "input"
    Output -> "output"

parseDirection :: Text -> Parser FlowDirection
parseDirection = \case
    "input" -> pure Input
    "output" -> pure Output
    other -> fail ("unknown direction: " <> T.unpack other <> " (expected input|output)")

-- | A selector in the words the configuration uses for it.
matchJSON :: MethodPatchMatch -> Value
matchJSON m =
    object . catMaybes $
        [ ("category" .=) <$> mpmCategory m
        , ("flow-name" .=) <$> mpmFlowName m
        , ("flow-name-prefix" .=) <$> mpmFlowNamePrefix m
        , ("cas" .=) <$> mpmCAS m
        , ("subcompartment-contains" .=) <$> mpmSubcompartmentContains m
        ]

parseMatch :: Value -> Parser MethodPatchMatch
parseMatch = withObject "selector" $ \o ->
    MethodPatchMatch
        <$> o .:? "category"
        <*> o .:? "flow-name"
        <*> o .:? "flow-name-prefix"
        <*> o .:? "cas"
        <*> o .:? "subcompartment-contains"

restoreJSON :: Restore -> Value
restoreJSON r =
    object
        [ "category" .= UUID.toText (rsMethod r)
        , "position" .= rsPosition r
        , "factor" .= factorJSON (rsFactor r)
        , "value" .= rsValue r
        ]

parseRestore :: Value -> Parser Restore
parseRestore = withObject "restored factor" $ \o ->
    Restore
        <$> (o .: "category" >>= parseUUID)
        <*> o .: "position"
        <*> (o .: "factor" >>= parseFactor)
        <*> o .: "value"

{- | The words of one change: a set or remove verb for each entry, as the
change writes a value or takes one away. A removal says what it removed; a
setting says what it writes, but for the display multiplier, whose absence is
one.
-}
changeJSON :: ScoringChange -> Value
changeJSON = \case
    SetText entry key before after ->
        object (verbOf (textVerbs entry) after <> [textKeyField entry .= key] <> values before after)
    SetNumber entry key before after ->
        object (verbOf (numberVerbs entry) after <> ["variable" .= key] <> values before after)
    RenameSet before after -> object ["verb" .= ("rename-scoring-set" :: Text), "before" .= before, "after" .= after]
    SetUnitOfSet before after -> object ["verb" .= ("set-scoring-unit" :: Text), "before" .= before, "after" .= after]
    SetDisplayMultiplier before after -> object (("verb" .= ("set-display-multiplier" :: Text)) : values before after)
  where
    verbOf :: (Text, Text) -> Maybe a -> [Pair]
    verbOf (set, remove) after = ["verb" .= maybe remove (const set) after]

    values :: (ToJSON a) => Maybe a -> Maybe a -> [Pair]
    values before after = maybe [] (\b -> ["before" .= b]) before <> maybe [] (\a -> ["after" .= a]) after

parseChange :: Value -> Parser ScoringChange
parseChange = withObject "scoring set change" $ \o -> do
    verb <- o .: "verb"
    let texts = [(entry, settingOf (textVerbs entry) verb) | entry <- [minBound .. maxBound]]
        numbers = [(entry, settingOf (numberVerbs entry) verb) | entry <- [minBound .. maxBound]]
    case (verb :: Text, [(e, w) | (e, Just w) <- texts], [(e, w) | (e, Just w) <- numbers]) of
        ("rename-scoring-set", _, _) -> RenameSet <$> o .: "before" <*> o .: "after"
        ("set-scoring-unit", _, _) -> SetUnitOfSet <$> o .: "before" <*> o .: "after"
        ("set-display-multiplier", _, _) -> SetDisplayMultiplier <$> o .:? "before" <*> o .:? "after"
        (_, [(entry, setting)], []) -> do
            (before, after) <- valued o setting
            key <- o .: textKeyField entry
            pure (SetText entry key before after)
        (_, [], [(entry, setting)]) -> do
            (before, after) <- valued o setting
            key <- o .: "variable"
            pure (SetNumber entry key before after)
        _ -> fail ("unknown scoring set change: " <> T.unpack verb)
  where
    settingOf :: (Text, Text) -> Text -> Maybe Setting
    settingOf (set, remove) verb
        | verb == set = Just Setting
        | verb == remove = Just Removing
        | otherwise = Nothing

    valued :: (FromJSON a) => Object -> Setting -> Parser (Maybe a, Maybe a)
    valued o setting = do
        before <- o .:? "before"
        after <- o .:? "after"
        case (setting, before, after) of
            (Setting, _, Just _) -> pure (before, after)
            (Setting, _, Nothing) -> fail "a setting writes a value"
            (Removing, Just _, Nothing) -> pure (before, Nothing)
            (Removing, Nothing, _) -> fail "a removal says what it removed"
            (Removing, Just _, Just _) -> fail "a removal writes no value"

-- | Whether a change's verb writes a value or takes one away.
data Setting = Setting | Removing

textVerbs :: TextEntry -> (Text, Text)
textVerbs = \case
    CategoryOf -> ("set-variable", "remove-variable")
    FormulaOf -> ("set-formula", "remove-computed")
    LabelOf -> ("set-label", "remove-label")
    VariableUnitOf -> ("set-variable-unit", "remove-variable-unit")
    ScoreOf -> ("set-score", "remove-score")

textKeyField :: TextEntry -> Key
textKeyField = \case
    ScoreOf -> "score"
    CategoryOf -> "variable"
    FormulaOf -> "variable"
    LabelOf -> "variable"
    VariableUnitOf -> "variable"

numberVerbs :: NumberEntry -> (Text, Text)
numberVerbs = \case
    NormalizationOf -> ("set-normalization", "remove-normalization")
    WeightOf -> ("set-weight", "remove-weight")

gestureJSON :: ScoringGesture -> Value
gestureJSON = \case
    AddedRow label -> object ["kind" .= ("added-row" :: Text), "label" .= label]
    ChangedRow label -> object ["kind" .= ("changed-row" :: Text), "label" .= label]
    RemovedRow label -> object ["kind" .= ("removed-row" :: Text), "label" .= label]
    RenamedSet before after -> object ["kind" .= ("renamed-set" :: Text), "before" .= before, "after" .= after]
    SetUnitTo before after -> object ["kind" .= ("set-unit" :: Text), "before" .= before, "after" .= after]
    SetMultiplierTo before after ->
        object (("kind" .= ("set-multiplier" :: Text)) : maybe [] (\b -> ["before" .= b]) before <> maybe [] (\a -> ["after" .= a]) after)
    WroteFormula name -> object ["kind" .= ("wrote-formula" :: Text), "name" .= name]
    AddedScore name -> object ["kind" .= ("added-score" :: Text), "name" .= name]
    ChangedScore name -> object ["kind" .= ("changed-score" :: Text), "name" .= name]
    RemovedScore name -> object ["kind" .= ("removed-score" :: Text), "name" .= name]

parseGesture :: Value -> Parser ScoringGesture
parseGesture = withObject "gesture" $ \o ->
    o .: "kind" >>= \case
        "added-row" -> AddedRow <$> o .: "label"
        "changed-row" -> ChangedRow <$> o .: "label"
        "removed-row" -> RemovedRow <$> o .: "label"
        "renamed-set" -> RenamedSet <$> o .: "before" <*> o .: "after"
        "set-unit" -> SetUnitTo <$> o .: "before" <*> o .: "after"
        "set-multiplier" -> SetMultiplierTo <$> o .:? "before" <*> o .:? "after"
        "wrote-formula" -> WroteFormula <$> o .: "name"
        "added-score" -> AddedScore <$> o .: "name"
        "changed-score" -> ChangedScore <$> o .: "name"
        "removed-score" -> RemovedScore <$> o .: "name"
        other -> fail ("unknown scoring set gesture: " <> T.unpack (other :: Text))

{- | A scoring set, without its origin: a set read back from a journal is one
the journal created, whatever wrote the line.
-}
scoringSetJSON :: ScoringSet -> Value
scoringSetJSON s =
    object $
        [ "name" .= ssName s
        , "unit" .= ssUnit s
        , "variables" .= ssVariables s
        , "computed" .= ssComputed s
        , "labels" .= ssLabels s
        , "normalization" .= ssNormalization s
        , "weighting" .= ssWeighting s
        , "scores" .= ssScores s
        , "units" .= ssUnits s
        ]
            <> maybe [] (\m -> ["display-multiplier" .= m]) (ssDisplayMultiplier s)

parseScoringSet :: Value -> Parser ScoringSet
parseScoringSet = withObject "scoring set" $ \o ->
    ScoringSet
        <$> o .: "name"
        <*> o .: "unit"
        <*> o .: "variables"
        <*> o .: "computed"
        <*> o .: "labels"
        <*> o .: "normalization"
        <*> o .: "weighting"
        <*> o .: "scores"
        <*> o .:? "display-multiplier"
        <*> o .: "units"
        <*> pure CreatedInJournal
