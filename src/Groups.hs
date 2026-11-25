{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

{- | A score split between groups of processes, wherever they sit in the
supply chain: transport, electricity, fertilisers. Each process goes to the
first group of the list whose filter it answers, the root to none, and what
no group takes is the rest. The parts add up to the score, the rest computed
as the others are and never as the total less the groups.

Under the own-emissions reading a group may also, or instead, keep
substances: all the nitrous oxide, or the nitrous oxide of the fields. It then
takes emissions, a process and a flow, rather than whole processes: each
emission goes to the first group that accepts both its process and its flow,
and a field gives its nitrous oxide to one group and its carbon dioxide to
the rest.

Two readings of what a group's part is:

* its own emissions: the biosphere exchanges of its processes, at the
  scaling the score solves them to;
* its whole chain, below the first process of a group met: once a process
  of a group is met, what it buys, and what that buys in turn, counts for
  that group, unless the walk from the root meets a process of another
  group first.

The whole-chain reading cuts the supply chain at the processes of a group.
With M = I − A, emptying their columns gives M′, and s′ = M′⁻¹ f. The rest is
s′ on the processes no group takes, w; what reaches a process j of a group is
y_j = s′_j. Then M w = f − y, so the solution is s = w + M⁻¹ y: the rest is
the score of w, and a group's part the score of M⁻¹ y restricted to its
processes, solved on the loaded matrix.
-}
module Groups (
    -- * What is asked
    Attribution (..),
    readAttribution,
    attributionWord,
    Criteria (..),
    ProcessFilter,
    processFilter,
    criteriaOf,
    SubstanceMatch (..),
    readSubstanceMatch,
    substanceMatchWord,
    EmissionCriteria (..),
    EmissionFilter,
    emissionFilter,
    emissionCriteriaOf,
    GroupFilter (..),
    groupFilter,
    processCriteriaOf,
    emissionCriteriaOfGroup,
    Group (..),
    Groups,
    groups,
    groupList,
    onlyProcesses,
    maxGroups,
    GroupIndex (..),
    groupNamed,
    candidateCriteria,
    distinctTakers,

    -- * Which process goes where
    Assignment,
    Assigned (..),
    processAnswers,
    firstGroup,
    assignment,
    unassigned,
    groupOfProcess,
    processesOfGroup,

    -- * Which emission goes where
    EmissionBasis (..),
    Reach,
    reachOf,
    Masked,
    masked,
    emissionTest,

    -- * The parts
    Split (..),
    Scalings,
    Parts (..),
    takenCount,
    summedByName,
    ownEmissionParts,
    emissionPart,
    emissionParts,
    emissionsTaken,
    Solving (..),
    cutDatabase,
    databaseCycle,
    firstMatchParts,
    entryParts,
    everyProcessOnce,
    perUnitByColumn,
    assembled,
) where

import Control.Monad (forM, guard, join)
import Data.Bits (bit, countTrailingZeros, (.&.), (.|.))
import Data.Char (isDigit)
import Data.Containers.ListUtils (nubOrd)
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.List (findIndex, nubBy, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import Database (Geographies, flowFieldsOf, flowQueryIn, isNamedSubstance, locationAnswers, referenceProductNames)
import Matrix (Demand (..), Vector, applyBiosphereMatrix, linkConsumer)
import Method.ChemSynonyms (ChemSynonyms)
import Method.Mapping (applyLongTermMode)
import qualified Search.Normalize as Normalize
import Service (matchClassifications)
import SharedSolver (CrossDBSolution (..), DepSolverLookup, Reached (..), Weights, weightAt)
import qualified SharedSolver
import Types hiding (Attribution)
import UnitConversion (UnitConfig)

-- | What a group's part of the score is.
data Attribution
    = -- | Its processes' whole chain, below the first process of a group met
      WholeChainBelowFirstMatch
    | -- | Its processes' own emissions
      OwnEmissions
    deriving (Eq, Show)

-- | The word a request names an attribution with: @first-match@ or @own@.
readAttribution :: Text -> Either Text Attribution
readAttribution = \case
    "first-match" -> Right WholeChainBelowFirstMatch
    "own" -> Right OwnEmissions
    other -> Left ("Unknown attribution '" <> other <> "': use first-match or own.")

attributionWord :: Attribution -> Text
attributionWord = \case
    WholeChainBelowFirstMatch -> "first-match"
    OwnEmissions -> "own"

{- | What a group asks of a process: a name and a product it contains, a place
that answers for its own, classifications as a search reads them (alternatives
within a system, every system), and classifications that leave it out.
-}
data Criteria = Criteria
    { crName :: Maybe Text
    , crProduct :: Maybe Text
    , crLocation :: Maybe Text
    , crClassifications :: ClassificationFilters
    }
    deriving (Eq, Show)

{- | Criteria that keep something: built only by 'processFilter', which refuses
those that would take every process, or every process but some.
-}
newtype ProcessFilter = ProcessFilter Criteria
    deriving (Eq, Show)

criteriaOf :: ProcessFilter -> Criteria
criteriaOf (ProcessFilter c) = c

-- | The filter of these criteria, blanks dropped, or why there is none.
processFilter :: Criteria -> Either Text ProcessFilter
processFilter c
    | any (T.null . T.strip . clfValue) (cfKept classes ++ cfLeftOut classes) = Left "A classification of a group has no value."
    | isNothing name && isNothing productText && isNothing place && null (cfKept classes) && not (null (cfLeftOut classes)) =
        Left "A group's classifications only leave processes out: name what its processes are too, a name, a product, a place or a classification to keep."
    | isNothing name && isNothing productText && isNothing place && null (cfKept classes) =
        Left "A group needs a name, a product, a place, a classification or a substance to keep: without one it would take every process."
    | otherwise = Right (ProcessFilter c{crName = name, crProduct = productText, crLocation = place})
  where
    classes :: ClassificationFilters
    classes = crClassifications c

    name, productText, place :: Maybe Text
    name = unblank (crName c)
    productText = unblank (crProduct c)
    place = unblank (crLocation c)

unblank :: Maybe Text -> Maybe Text
unblank = (>>= \t -> let s = T.strip t in if T.null s then Nothing else Just s)

-- | How a group reads the name of a substance.
data SubstanceMatch
    = -- | The flow is the substance named: by its own name, a name the registry gives the same substance, a formula or a CAS ('isNamedSubstance')
      IsTheSubstance
    | -- | The flow's own name contains the text, for a family such as HFC
      NameContains
    deriving (Eq, Show)

-- | The word a request names a reading of a substance with: @substance@ or @contains@.
readSubstanceMatch :: Text -> Either Text SubstanceMatch
readSubstanceMatch = \case
    "substance" -> Right IsTheSubstance
    "contains" -> Right NameContains
    other -> Left ("Unknown way to read a substance '" <> other <> "': use substance or contains.")

substanceMatchWord :: SubstanceMatch -> Text
substanceMatchWord = \case
    IsTheSubstance -> "substance"
    NameContains -> "contains"

{- | What a group asks of an emission: the substance, read one of two ways,
and the compartment it goes to or is taken from.
-}
data EmissionCriteria = EmissionCriteria
    { ecSubstance :: Maybe (SubstanceMatch, Text)
    , ecMedium :: Maybe Medium
    }
    deriving (Eq, Show)

{- | Criteria on emissions that keep something: built only by 'emissionFilter',
which refuses those that would take every emission.
-}
newtype EmissionFilter = EmissionFilter EmissionCriteria
    deriving (Eq, Show)

emissionCriteriaOf :: EmissionFilter -> EmissionCriteria
emissionCriteriaOf (EmissionFilter c) = c

-- | The filter of these criteria, a blank name dropped, or why there is none.
emissionFilter :: EmissionCriteria -> Either Text EmissionFilter
emissionFilter c = case (substance, ecMedium c) of
    (Nothing, Nothing) -> Left "A group's substance needs a name or a compartment: without one it would take every emission."
    _ -> Right (EmissionFilter c{ecSubstance = substance})
  where
    substance :: Maybe (SubstanceMatch, Text)
    substance = ecSubstance c >>= \(match, name) -> (match,) <$> unblank (Just name)

-- | What a group keeps: processes, emissions, or the emissions of some processes. Never nothing, never everything.
data GroupFilter
    = Processes ProcessFilter
    | Emissions EmissionFilter
    | ProcessesAndEmissions ProcessFilter EmissionFilter
    deriving (Eq, Show)

{- | The filter of criteria on processes and, if asked, on emissions. Criteria
on processes that name nothing at all beside criteria on emissions leave the
processes free.
-}
groupFilter :: Criteria -> Maybe EmissionCriteria -> Either Text GroupFilter
groupFilter c = \case
    Nothing -> Processes <$> processFilter c
    Just e
        | namesNothing -> Emissions <$> emissionFilter e
        | otherwise -> ProcessesAndEmissions <$> processFilter c <*> emissionFilter e
  where
    namesNothing :: Bool
    namesNothing =
        all (isNothing . unblank) [crName c, crProduct c, crLocation c]
            && null (cfKept (crClassifications c))
            && null (cfLeftOut (crClassifications c))

processCriteriaOf :: GroupFilter -> Maybe Criteria
processCriteriaOf = \case
    Processes p -> Just (criteriaOf p)
    Emissions _ -> Nothing
    ProcessesAndEmissions p _ -> Just (criteriaOf p)

emissionCriteriaOfGroup :: GroupFilter -> Maybe EmissionCriteria
emissionCriteriaOfGroup = \case
    Processes _ -> Nothing
    Emissions e -> Just (emissionCriteriaOf e)
    ProcessesAndEmissions _ e -> Just (emissionCriteriaOf e)

-- | A group: what its part is called, and what it takes.
data Group f = Group
    { groupName :: Text
    , groupFilterOf :: f
    }
    deriving (Eq, Show, Functor, Foldable, Traversable)

{- | The groups of a request, in the order a process or an emission is offered
to them: at least one, at most 'maxGroups', no two of a name.
-}
newtype Groups f = Groups (NonEmpty (Group f))
    deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Twenty groups: each has a bit in the 'Word32' masks of 'Masked'.
maxGroups :: Int
maxGroups = 20

-- | The groups as groups of processes, when none of them keeps a substance.
onlyProcesses :: Groups GroupFilter -> Maybe (Groups ProcessFilter)
onlyProcesses = traverse $ \case
    Processes p -> Just p
    Emissions _ -> Nothing
    ProcessesAndEmissions _ _ -> Nothing

groups :: [Group f] -> Either Text (Groups f)
groups listed = case listed of
    [] -> Left "Name at least one group."
    g : gs
        | length listed > maxGroups -> Left ("At most " <> T.pack (show maxGroups) <> " groups.")
        | any (T.null . T.strip . groupName) listed -> Left "A group has no name."
        | dup : _ <- duplicates -> Left ("Two groups are named '" <> dup <> "'.")
        | otherwise -> Right (Groups (g :| gs))
  where
    duplicates :: [Text]
    duplicates = M.keys (M.filter (> (1 :: Int)) (M.fromListWith (+) [(groupName g, 1) | g <- listed]))

groupList :: Groups f -> [Group f]
groupList (Groups gs) = NE.toList gs

-- | A group by its position in the list.
newtype GroupIndex = GroupIndex Int
    deriving (Eq, Ord, Show)

-- | The position of the group of that name, or why there is none.
groupNamed :: Groups f -> Text -> Either Text GroupIndex
groupNamed gs name =
    maybe
        (Left ("No group is named '" <> name <> "': the groups are " <> T.intercalate ", " (map groupName (groupList gs)) <> "."))
        (Right . GroupIndex)
        (findIndex ((== name) . groupName) (groupList gs))

{- | What a process could be grouped by, each with the name its group would
take, the narrowest first: its product; a code by each of its prefixes down
to two digits (@6511: Road transport@ by 6511, 651 and 65); a value that is
neither code nor path as it is (a SimaPro category type); a path by each of
its levels, before the path a source files whole databases under.
-}
candidateCriteria :: [Text] -> M.Map Text Text -> [(Text, Criteria)]
candidateCriteria products classes =
    nubBy ((==) `on` snd) $
        [(p, (classified []){crProduct = Just p}) | p <- take 1 (filter (not . T.null) (map T.strip products))]
            ++ concatMap snd (sortOn fst [reading system (T.strip value) | (system, value) <- M.toList classes, not (T.null (T.strip value))])
  where
    reading :: Text -> Text -> (ValueShape, [(Text, Criteria)])
    reading system value
        | (digits, rest) <- T.span isDigit value
        , not (T.null digits)
        , ":" `T.isPrefixOf` T.stripStart rest =
            (Coded, [(system <> " " <> code, classified [ClassificationFilter system code MatchPrefix]) | n <- [T.length digits, T.length digits - 1 .. min 2 (T.length digits)], let code = T.take n digits])
        -- A backslash first: a SimaPro path may hold a slash inside one of its levels.
        | sep : _ <- filter (`T.isInfixOf` value) ["\\", "/"] =
            let parts = T.splitOn sep value
             in ( Path
                , [ (level, classified [ClassificationFilter system level MatchExact, ClassificationFilter system (level <> sep) MatchPrefix])
                  | n <- [length parts, length parts - 1 .. 1]
                  , let level = T.intercalate sep (take n parts)
                  , not (T.null level)
                  ]
                )
        | otherwise = (Plain, [(value, classified [ClassificationFilter system value MatchExact])])

    classified :: [ClassificationFilter] -> Criteria
    classified kept = Criteria{crName = Nothing, crProduct = Nothing, crLocation = Nothing, crClassifications = ClassificationFilters{cfKept = kept, cfLeftOut = []}}

{- | Of criteria tried in the order offered, each with the processes it would
take, those taking something, one per set of processes taken. A set shows
at the place of its first criterion, under the last criterion taking it: the
broadest says the same and holds better when the chain changes, and keeping
the place means a cut on the count never drops the set with its twin.
-}
distinctTakers :: (Ord k) => [(t, [k])] -> [t]
distinctTakers tried = mapMaybe broadest (nubOrd [taken | (_, taken) <- tried, not (null taken)])
  where
    broadest taken = NE.last <$> NE.nonEmpty [t | (t, same) <- tried, same == taken]

-- | How a classification value reads, in the order its levels are offered.
data ValueShape = Coded | Plain | Path
    deriving (Eq, Ord)

-- | The first group whose filter an activity answers.
firstGroup :: Geographies -> Database -> Groups ProcessFilter -> Activity -> Maybe GroupIndex
firstGroup geographies db gs activity = GroupIndex <$> findIndex (\g -> processAnswers geographies db (groupFilterOf g) activity) (groupList gs)

-- | Whether an activity answers a filter on processes.
processAnswers :: Geographies -> Database -> ProcessFilter -> Activity -> Bool
processAnswers geographies db (ProcessFilter c) activity =
    maybe True (`Normalize.caseInsensitiveInfixOf` activityName activity) (crName c)
        && maybe True (\pat -> any (Normalize.caseInsensitiveInfixOf pat) (referenceProductNames db activity)) (crProduct c)
        && maybe True (\pat -> locationAnswers geographies pat (activityLocation activity)) (crLocation c)
        && matchClassifications activity (cfKept (crClassifications c))
        && not (any (`classificationMatches` activityClassification activity) (cfLeftOut (crClassifications c)))

-- | The group of each column of a database's matrix, if it has one.
type Assignment = V.Vector (Maybe GroupIndex)

{- | The group of each column, the process given, the root, left with none:
were it a group's, that group would take the whole score.
-}
assignment :: (Activity -> Maybe GroupIndex) -> Maybe ProcessId -> Database -> Assignment
assignment groupOf root db =
    unassigned db
        V.// [ (fromIntegral column, Just g)
             | (pid, column) <- zip [0 :: ProcessId ..] (V.toList (dbActivityIndex db))
             , Just pid /= root
             , Just g <- [groupOf (dbActivities db V.! fromIntegral pid)]
             ]

-- | The group a process of the solution goes to, if any.
groupOfProcess :: Assigned -> Scalings -> (Text, ProcessId) -> Maybe GroupIndex
groupOfProcess assigned scalings (name, p) = do
    (d, _) <- M.lookup name scalings
    asg <- M.lookup name (assignedColumns assigned)
    column <- dbActivityIndex d V.!? fromIntegral p
    join (asg V.!? fromIntegral column)

-- | The processes the solution reaches that go to a group, by database and column.
processesOfGroup :: Assigned -> Scalings -> GroupIndex -> [(Text, Int)]
processesOfGroup assigned scalings g =
    [ (name, column)
    | (name, (_, v)) <- M.toList scalings
    , Just asg <- [M.lookup name (assignedColumns assigned)]
    , (column, x) <- zip [0 ..] (U.toList v)
    , x /= 0
    , asg V.!? column == Just (Just g)
    ]

-- | No process of the database in any group, as in a database whose detail is withheld.
unassigned :: Database -> Assignment
unassigned db = V.replicate (fromIntegral (dbActivityCount db)) Nothing

{- | A vector of scalings per database, by the name it is loaded under, with
its data.
-}
type Scalings = M.Map Text (Database, Vector)

{- | The rest and each group's part, in the order of the groups, with what
each group takes: the scaling of its processes for their own emissions, what
reaches them for their whole chain. A database a part does not reach may be
missing from it.
-}
data Parts = Parts
    { partsRest :: Scalings
    , partsGroups :: [Scalings]
    , partsTaken :: [Scalings]
    }

-- | How many processes a group takes something of.
takenCount :: Scalings -> Int
takenCount = sum . map (U.length . U.filter (/= 0) . snd) . M.elems

-- | A database reached twice, by two links, moves as one: its vectors summed.
summedByName :: NonEmpty Reached -> Scalings
summedByName reached = M.fromListWith plus [(name, (db, v)) | Reached name db v <- NE.toList reached]

plus :: (Database, Vector) -> (Database, Vector) -> (Database, Vector)
plus (db, a) (_, b) = (db, U.zipWith (+) a b)

{- | The groups and where each process of each database reached goes among
them, by database name.
-}
data Assigned = Assigned
    { assignedGroups :: Groups ProcessFilter
    , assignedColumns :: M.Map Text Assignment
    }

{- | How a split goes: under the whole-chain reading, by process; under the
own-emissions reading, by process when no group keeps a substance, by
emission when one does. The whole chain is never split by emission.
-}
data Split
    = WholeChain Assigned
    | OwnByProcess Assigned
    | OwnByEmission Masked

{- | What a split by emission reads each group's reach from: the places, the
chemical names, the flows (merged across databases), the root, the databases
whose licence withholds their detail, and the databases reached.
-}
data EmissionBasis = EmissionBasis
    { ebGeographies :: Geographies
    , ebChemicals :: ChemSynonyms
    , ebFlows :: BioFlowDB
    , ebRoot :: (Text, ProcessId)
    , ebWithheld :: [Text]
    , ebReached :: Scalings
    }

{- | Where one group reaches in one database: the columns whose process its
criteria on processes accept, and the rows whose flow its criteria on
substances accept. A criterion the group does not have accepts all.
-}
data Reach = Reach
    { reachColumns :: !(U.Vector Bool)
    , reachRows :: !(U.Vector Bool)
    }

{- | Where a group reaches in each database reached. A group of processes
never takes the root, which would give it the whole score; a group that keeps
substances whatever their process takes the root's own emissions, without
which the nitrous oxide of a field scored alone would stay out of a group of
all the nitrous oxide. A database whose licence withholds its detail is in
no group. A row the flows do not describe is in no group that keeps
substances.
-}
reachOf :: EmissionBasis -> GroupFilter -> M.Map Text Reach
reachOf EmissionBasis{ebGeographies = geographies, ebChemicals = chemicals, ebFlows = flows, ebRoot = (rootName, rootProcess), ebWithheld = withheld, ebReached = reached} gf =
    M.mapWithKey reachIn reached
  where
    reachIn :: Text -> (Database, Vector) -> Reach
    reachIn name (db, _) = Reach{reachColumns = columns, reachRows = rows}
      where
        processes :: V.Vector ProcessId
        processes = processesByColumn db

        rootColumn :: Maybe Int
        rootColumn
            | name == rootName = fromIntegral <$> dbActivityIndex db V.!? fromIntegral rootProcess
            | otherwise = Nothing

        acceptedBy :: ProcessFilter -> Int -> Bool
        acceptedBy p c = Just c /= rootColumn && processAnswers geographies db p (dbActivities db V.! fromIntegral (processes V.! c))

        columns :: U.Vector Bool
        columns
            | name `elem` withheld = U.replicate (V.length processes) False
            | otherwise = U.generate (V.length processes) $ \c -> case gf of
                Processes p -> acceptedBy p c
                ProcessesAndEmissions p _ -> acceptedBy p c
                Emissions _ -> True

        rowCount :: Int
        rowCount = fromIntegral (dbBiosphereCount db)

        rows :: U.Vector Bool
        rows = case emissionOf gf of
            Nothing -> U.replicate rowCount True
            Just e ->
                let takes = emissionTest chemicals db e
                 in U.generate rowCount (\i -> maybe False takes (flip M.lookup flows =<< dbBiosphereOrder db V.!? i))

    emissionOf :: GroupFilter -> Maybe EmissionFilter
    emissionOf = \case
        Processes _ -> Nothing
        Emissions e -> Just e
        ProcessesAndEmissions _ e -> Just e

{- | Whether an emission filter takes a flow of one database: the substance
read against that database's registry and the chemical names, once for every
flow it is asked of; the compartment by its medium, which a flow without one
never answers.
-}
emissionTest :: ChemSynonyms -> Database -> EmissionFilter -> BiosphereFlow -> Bool
emissionTest chemicals db (EmissionFilter EmissionCriteria{ecSubstance = substance, ecMedium = medium}) = \flow -> inMedium flow && named flow
  where
    inMedium :: BiosphereFlow -> Bool
    inMedium flow = maybe True (\m -> fmap compartmentName (bfCompartment flow) == Just m) medium

    named :: BiosphereFlow -> Bool
    named = case substance of
        Nothing -> const True
        Just (IsTheSubstance, text) -> let query = flowQueryIn chemicals db text in isNamedSubstance query . flowFieldsOf . BioKind
        Just (NameContains, text) -> Normalize.caseInsensitiveInfixOf text . bfName

{- | Each group's reach combined, by database: a bit per group on each column
and each row, the first group the lowest bit. An emission goes to the lowest
group whose bit both its column and its row carry.
-}
data Masked = Masked
    { maskedCount :: !Int
    , maskedColumns :: !(M.Map Text (U.Vector Word32))
    , maskedRows :: !(M.Map Text (U.Vector Word32))
    }

-- | The masks of groups' reaches, in the order of the groups.
masked :: [M.Map Text Reach] -> Masked
masked reaches = Masked{maskedCount = length reaches, maskedColumns = bits reachColumns, maskedRows = bits reachRows}
  where
    bits :: (Reach -> U.Vector Bool) -> M.Map Text (U.Vector Word32)
    bits field = M.unionsWith (U.zipWith (.|.)) [M.map (U.map (\b -> if b then bit g else 0) . field) r | (g, r) <- zip [0 ..] reaches]

-- | The group an entry goes to, at a row and a column of a database, if any.
destination :: U.Vector Word32 -> U.Vector Word32 -> SparseTriple -> Maybe GroupIndex
destination columns rows (SparseTriple i j _) = case (columns U.! fromIntegral j) .&. (rows U.! fromIntegral i) of
    0 -> Nothing
    common -> Just (GroupIndex (countTrailingZeros common))

-- | The masks of one database, none where the split has none for it: all its emissions then go to the rest.
masksOf :: Masked -> Text -> Database -> (U.Vector Word32, U.Vector Word32)
masksOf Masked{maskedColumns = columns, maskedRows = rows} name db =
    ( M.findWithDefault (U.replicate (fromIntegral (dbActivityCount db)) 0) name columns
    , M.findWithDefault (U.replicate (fromIntegral (dbBiosphereCount db)) 0) name rows
    )

{- | One part of a split by emission, a group's or the rest: each database
with only the entries of its biosphere matrix that go there, and the scaling
of the processes that keep one. A part that keeps every entry of a database
keeps the database itself, whose regionalized weights are then those already
summed; a database a part keeps nothing of is left out of it. Rows, columns
and flows are never numbered again.
-}
emissionPart :: Masked -> Maybe GroupIndex -> Scalings -> Scalings
emissionPart m wanted = M.mapMaybeWithKey keep
  where
    keep :: Text -> (Database, Vector) -> Maybe (Database, Vector)
    keep name (db, v) = do
        let (columns, rows) = masksOf m name db
            entries = dbBiosphereTriples db
            kept = U.filter ((== wanted) . destination columns rows) entries
            keeps = U.update (U.replicate (U.length v) False) (U.map (\(SparseTriple _ j _) -> (fromIntegral j, True)) kept)
        guard (not (U.null kept))
        pure
            ( if U.length kept == U.length entries then db else db{dbBiosphere = byColumn kept}
            , U.zipWith (\x k -> if k then x else 0) v keeps
            )

-- | The rest and each group's part of a split by emission.
emissionParts :: Masked -> Scalings -> Parts
emissionParts m scalings =
    Parts
        { partsRest = emissionPart m Nothing scalings
        , partsGroups = taken
        , partsTaken = taken
        }
  where
    taken :: [Scalings]
    taken = [emissionPart m (Just (GroupIndex g)) scalings | g <- [0 .. maskedCount m - 1]]

{- | The emissions a group takes, by database: where its entries sit in each
database's biosphere matrix, of processes the solution reaches. Two groups
taking the same take the same part.
-}
emissionsTaken :: Masked -> GroupIndex -> Scalings -> [(Text, U.Vector Int)]
emissionsTaken m g scalings =
    [ (name, at)
    | (name, (db, v)) <- M.toList scalings
    , let (columns, rows) = masksOf m name db
          at = U.findIndices (\e@(SparseTriple _ j _) -> v U.! fromIntegral j /= 0 && destination columns rows e == Just g) (dbBiosphereTriples db)
    , not (U.null at)
    ]

-- | The scalings of each database split by the group of each process.
splitBy :: Assigned -> Scalings -> Parts
splitBy Assigned{assignedGroups = gs, assignedColumns = assignments} scalings =
    Parts
        { partsRest = keeping isNothing
        , partsGroups = taken
        , partsTaken = taken
        }
  where
    taken :: [Scalings]
    taken = [keeping (== Just (GroupIndex g)) | g <- [0 .. length (groupList gs) - 1]]

    keeping :: (Maybe GroupIndex -> Bool) -> Scalings
    keeping keep = M.mapWithKey (\name (db, v) -> (db, kept keep (M.findWithDefault (unassigned db) name assignments) v)) scalings

    kept :: (Maybe GroupIndex -> Bool) -> Assignment -> Vector -> Vector
    kept keep asg = U.imap (\c x -> if keep (asg V.! c) then x else 0)

-- | Each group's own emissions: the solution's scalings split by group.
ownEmissionParts :: Assigned -> CrossDBSolution -> Parts
ownEmissionParts assigned sol = splitBy assigned (summedByName (csScalings sol))

-- | How the parts are solved: the units, the other databases, and the hold on factorising.
data Solving = Solving
    { solvingUnits :: UnitConfig
    , solvingDeps :: DepSolverLookup
    , solvingHold :: forall a. IO a -> IO a
    -- ^ Holds the factorisation of the cut matrices
    }

{- | The database with the columns of its processes in a group emptied, and
with no link to another database from them: whatever such a process buys is
counted for its group, not followed from here. Every triple of the column
goes, one on the diagonal included, so an emptied column is that of the
identity.
-}
cutDatabase :: Assignment -> Database -> Database
cutDatabase asg db =
    db
        { dbTechnosphereTriples = U.filter (\(SparseTriple _ column _) -> isNothing (asg V.! fromIntegral column)) (dbTechnosphereTriples db)
        , dbCrossDBLinks = filter (not . fromAssigned) (dbCrossDBLinks db)
        }
  where
    fromAssigned :: CrossDBLink -> Bool
    fromAssigned link = maybe False (\(pid, _) -> isJust (asg V.! fromIntegral (dbActivityIndex db V.! fromIntegral pid))) (linkConsumer db link)

{- | A loop of links between the databases a solution reaches, from its root,
if there is one, as the names along it. Two links reaching one database are
not a loop.
-}
databaseCycle :: NonEmpty Reached -> Maybe [Text]
databaseCycle reached = walk [] (reachedName (NE.head reached))
  where
    suppliers :: M.Map Text [Text]
    suppliers = M.fromList [(name, S.toList (S.fromList (map cdlSourceDatabase (dbCrossDBLinks db)))) | Reached name db _ <- NE.toList reached]

    walk :: [Text] -> Text -> Maybe [Text]
    walk path name
        | name `elem` path = Just (reverse (name : path))
        | otherwise = listToMaybe (mapMaybe (walk (name : path)) (M.findWithDefault [] name suppliers))

{- | Each group's whole chain below the first process of a group met, and the
rest, from the root's demand. The databases with a process in a group are cut
and factorised again, all of them under the one hold the caller gives, since
a solver factorises on first use, inside the solve; each group's part is then
solved on the loaded matrices, all groups at once for each database.

Refused when the databases reached loop into each other: a part solved again
from a dependency would go deeper than the depth the score stopped at.
-}
firstMatchParts ::
    Solving ->
    -- | The score's solution
    CrossDBSolution ->
    -- | The root's demand
    Demand ->
    Assigned ->
    IO (Either Text Parts)
firstMatchParts Solving{solvingUnits = unitCfg, solvingDeps = depLookup, solvingHold = holding} sol rootDemand assigned = case databaseCycle (csScalings sol) of
    Just loop -> pure (Left ("The databases this chain reaches loop into each other (" <> T.intercalate " → " loop <> "): only the own-emissions reading splits its score."))
    Nothing -> do
        solvers <- M.traverseWithKey withSolver (M.intersectionWith (\asg (db, _) -> cutDatabase asg db) (M.filter (V.any isJust) (assignedColumns assigned)) reached)
        let cutLookup name = maybe (depLookup name) (pure . Just) (M.lookup name solvers)
        if M.null solvers
            then pure (Right (splitBy assigned reached))
            else
                cutLookup rootName >>= \case
                    Nothing -> pure (Left ("The database '" <> rootName <> "' is no longer loaded."))
                    Just (cutRoot, cutSolver) ->
                        holding (SharedSolver.goWithDeps unitCfg cutLookup cutRoot rootName cutSolver [Right rootDemand] 0) >>= \case
                            [Right solvedCut] -> belowFirstMatch (splitBy assigned (loaded (summedByName (csScalings solvedCut))))
                            [Left why] -> pure (Left why)
                            _ -> pure (Left "The chain cut at its groups gave no single answer to its one demand.")
  where
    reached :: Scalings
    reached = summedByName (csScalings sol)

    rootName :: Text
    rootName = reachedName (NE.head (csScalings sol))

    withSolver :: Text -> Database -> IO (Database, SharedSolver.SharedSolver)
    withSolver name cutDb = (cutDb,) <$> SharedSolver.createSharedSolver (name <> " cut at its groups") (dbTechnosphereTriples cutDb) (fromIntegral (dbActivityCount cutDb))

    -- The loaded data of each database cut, in place of the cut one.
    loaded :: Scalings -> Scalings
    loaded = M.mapWithKey (\name (db, v) -> (maybe db fst (M.lookup name reached), v))

    -- The rest is w as it is; each group's part is its y solved again.
    belowFirstMatch :: Parts -> IO (Either Text Parts)
    belowFirstMatch (Parts rest _ ys) = do
        let asked = M.keys (M.unions (map (M.filter (U.any (/= 0) . snd)) ys))
        solved <- forM asked $ \name -> solvedFrom name [snd <$> M.lookup name y | y <- ys]
        pure $ case partitionEithers solved of
            (why : _, _) -> Left why
            ([], perDatabase) -> Right (Parts rest (foldl' (zipWith (M.unionWith plus)) (map (const M.empty) ys) perDatabase) ys)

    -- Each group's y on one database, solved on its loaded matrix with what it buys from the others.
    solvedFrom :: Text -> [Maybe Vector] -> IO (Either Text [Scalings])
    solvedFrom name ys =
        depLookup name >>= \case
            Nothing -> pure (Left ("The database '" <> name <> "' is no longer loaded."))
            Just (db, solver) -> do
                let demand = Right . Demand . fromMaybe (U.replicate (fromIntegral (dbActivityCount db)) 0)
                answers <- SharedSolver.goWithDeps unitCfg depLookup db name solver (map demand ys) 0
                pure (map (summedByName . csScalings) <$> sequence answers)

{- | What each process that receives something of a group brings into its part
under the whole-chain reading: what reaches it times what one unit of it weighs
with its whole chain, solved on the loaded matrices. They add up to the part.
Refused, naming the process, for one that has no weight, which the score
having an answer rules out: its chain is part of the score's.
-}
entryParts :: M.Map Text (Database, Weights) -> Scalings -> Either Text [((Text, ProcessId), Double)]
entryParts weights taken = concat <$> traverse entriesIn (M.toList taken)
  where
    entriesIn :: (Text, (Database, Vector)) -> Either Text [((Text, ProcessId), Double)]
    entriesIn (name, (db, y)) = case M.lookup name weights of
        Nothing -> Left ("The database '" <> name <> "' is no longer loaded.")
        Just (_, w) -> traverse (entry w) [(column, v) | (column, v) <- zip [0 ..] (U.toList y), v /= 0]
      where
        processes :: V.Vector ProcessId
        processes = processesByColumn db

        entry :: Weights -> (Int, Double) -> Either Text ((Text, ProcessId), Double)
        entry w (column, v) = case weightAt w column of
            Just weight -> Right ((name, processes V.! column), v * weight)
            Nothing ->
                Left
                    ( "The process "
                        <> processIdToText db (processes V.! column)
                        <> " of '"
                        <> name
                        <> "' has no score per unit: its upstream processes reach one that cannot be solved for, or a link to another database whose unit cannot be converted."
                    )

{- | Every process of each database at one unit: each process's own part of
the score of this is that process's score per unit of it.
-}
everyProcessOnce :: Scalings -> Scalings
everyProcessOnce = M.map (\(db, v) -> (db, U.replicate (U.length v) 1))

{- | Each process's score per unit of it, by database and column, from each
process's own part of 'everyProcessOnce'. A process with no part scores zero.
-}
perUnitByColumn :: Scalings -> M.Map (Text, ProcessId) Double -> M.Map Text Vector
perUnitByColumn reached perUnit = M.mapWithKey columns reached
  where
    columns :: Text -> (Database, Vector) -> Vector
    columns name (db, _) = let processes = processesByColumn db in U.generate (V.length processes) (\c -> M.findWithDefault 0 (name, processes V.! c) perUnit)

-- | The process of each column of a database's matrix.
processesByColumn :: Database -> V.Vector ProcessId
processesByColumn db = V.update (V.replicate (V.length (dbActivityIndex db)) 0) (V.imap (\pid column -> (fromIntegral column, fromIntegral pid)) (dbActivityIndex db))

{- | A part as a solution scored the way the score is: every database of the
score listed in its order, each with the part's data and at its scaling (zero where the part
reaches none of it, and a second listing of a database at zero), any other
database the part reaches after them, then the inventory built again and the
long-term policy applied. Listing every database of the score keeps the
choice between regionalized and flat scoring the score's, so the parts add up
to it.
-}
assembled :: BioFlowDB -> CrossDBSolution -> Scalings -> CrossDBSolution
assembled flowDB sol part =
    sol
        { csScalings = listed
        , csInventory = applyLongTermMode flowDB (csLongTerm sol) (M.unionsWith (+) [applyBiosphereMatrix db v | Reached _ db v <- NE.toList listed])
        }
  where
    shape :: NonEmpty Reached
    shape = csScalings sol

    listed :: NonEmpty Reached
    listed = NE.appendList (placed S.empty shape) [Reached name db v | (name, (db, v)) <- M.toList part, name `notElem` fmap reachedName shape]

    placed :: S.Set Text -> NonEmpty Reached -> NonEmpty Reached
    placed seen (r :| rs) = here :| maybe [] (NE.toList . placed (S.insert (reachedName r) seen)) (NE.nonEmpty rs)
      where
        here :: Reached
        here
            | reachedName r `S.member` seen = SharedSolver.atZero r
            | otherwise = maybe (SharedSolver.atZero r) (\(db, v) -> r{reachedDatabase = db, reachedScaling = v}) (M.lookup (reachedName r) part)
