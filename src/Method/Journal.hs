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
    LineKind (..),
    MethodLine (..),

    -- * Applying it
    applyMethodOp,
    replayMethodJournal,

    -- * Shared with the planning of a change
    opName,
    describeFactor,
    oneCategory,
    sameAddress,
) where

import Control.Monad (foldM, unless, zipWithM)
import Data.Aeson (Object, Value, object, withObject, (.:), (.:?), (.=))
import Data.Aeson.Types (Pair, Parser)
import Data.Bifunctor (first)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID

import Config (MethodPatch (..), MethodPatchMatch (..))
import Data.JournalFile (Entry (..), JournalVocabulary (..))
import Method.Patch (applyMethodPatches)
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
        | set `elem` mcScoringSets collection ->
            Right collection{mcScoringSets = filter (/= set) (mcScoringSets collection)}
        | otherwise -> Left ("the scoring set '" <> ssName set <> "' is not there as recorded")

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
            Right method{methodFactors = take i (methodFactors method) <> (factor : drop i (methodFactors method))}
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
    other -> fail ("unknown method journal operation: " <> T.unpack other)
  where
    category :: Parser UUID
    category = o .: "category" >>= parseUUID
    patchWith :: PatchOp -> Parser MethodOp
    patchWith op = do
        match <- o .: "match" >>= parseMatch
        description <- o .:? "description"
        PatchFactors (MethodPatch description match op) <$> o .: "touched"

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
