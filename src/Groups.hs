{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

{- | A score split between groups of processes, wherever they sit in the
supply chain: transport, electricity, fertilisers. Each process goes to the
first group of the list whose filter it answers, the root to none, and what
no group takes is the rest. The parts add up to the score, the rest computed
as the others are and never as the total less the groups.

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
    GroupFilter,
    groupFilter,
    criteriaOf,
    Group (..),
    Groups,
    groups,
    groupList,
    maxGroups,
    GroupIndex (..),
    groupNamed,

    -- * Which process goes where
    Assignment,
    Assigned (..),
    firstGroup,
    assignment,
    unassigned,

    -- * The parts
    Scalings,
    Parts (..),
    takenCount,
    summedByName,
    ownEmissionParts,
    Solving (..),
    cutDatabase,
    databaseCycle,
    firstMatchParts,
    entryParts,
    everyProcessOnce,
    perUnitByColumn,
    scoreFromPerUnit,
    assembled,
) where

import Control.Monad (forM)
import Data.Either (partitionEithers)
import Data.List (findIndex)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Database (Geographies, locationAnswers, referenceProductNames)
import Matrix (Demand (..), Vector, applyBiosphereMatrix, linkConsumer)
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

{- | Criteria that keep something: built only by 'groupFilter', which refuses
those that would take every process, or every process but some.
-}
newtype GroupFilter = GroupFilter Criteria
    deriving (Eq, Show)

criteriaOf :: GroupFilter -> Criteria
criteriaOf (GroupFilter c) = c

-- | The filter of these criteria, blanks dropped, or why there is none.
groupFilter :: Criteria -> Either Text GroupFilter
groupFilter c
    | any (T.null . T.strip . clfValue) (cfKept classes ++ cfLeftOut classes) = Left "A classification of a group has no value."
    | isNothing name && isNothing productText && isNothing place && null (cfKept classes) =
        Left "A group needs a name, a product, a place or a classification to keep: without one it would take every process."
    | otherwise = Right (GroupFilter c{crName = name, crProduct = productText, crLocation = place})
  where
    classes :: ClassificationFilters
    classes = crClassifications c

    name, productText, place :: Maybe Text
    name = unblank (crName c)
    productText = unblank (crProduct c)
    place = unblank (crLocation c)

    unblank :: Maybe Text -> Maybe Text
    unblank = (>>= \t -> let s = T.strip t in if T.null s then Nothing else Just s)

-- | A group: what its part is called, and the processes it takes.
data Group = Group
    { groupName :: Text
    , groupFilterOf :: GroupFilter
    }
    deriving (Eq, Show)

{- | The groups of a request, in the order a process is offered to them: at
least one, at most 'maxGroups', no two of a name.
-}
newtype Groups = Groups (NonEmpty Group)
    deriving (Eq, Show)

maxGroups :: Int
maxGroups = 20

groups :: [Group] -> Either Text Groups
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

groupList :: Groups -> [Group]
groupList (Groups gs) = NE.toList gs

-- | A group by its position in the list.
newtype GroupIndex = GroupIndex Int
    deriving (Eq, Ord, Show)

-- | The position of the group of that name, or why there is none.
groupNamed :: Groups -> Text -> Either Text GroupIndex
groupNamed gs name =
    maybe
        (Left ("No group is named '" <> name <> "': the groups are " <> T.intercalate ", " (map groupName (groupList gs)) <> "."))
        (Right . GroupIndex)
        (findIndex ((== name) . groupName) (groupList gs))

-- | The first group whose filter an activity answers.
firstGroup :: Geographies -> Database -> Groups -> Activity -> Maybe GroupIndex
firstGroup geographies db gs activity = GroupIndex <$> findIndex (answers . criteriaOf . groupFilterOf) (groupList gs)
  where
    answers :: Criteria -> Bool
    answers c =
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
    { assignedGroups :: Groups
    , assignedColumns :: M.Map Text Assignment
    }

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
    keeping keep = M.mapWithKey (\name (db, v) -> (db, masked keep (M.findWithDefault (unassigned db) name assignments) v)) scalings

    masked :: (Maybe GroupIndex -> Bool) -> Assignment -> Vector -> Vector
    masked keep asg = U.imap (\c x -> if keep (asg V.! c) then x else 0)

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
    entriesIn (name, (db, y)) = traverse entry [(column, v) | (column, v) <- zip [0 ..] (U.toList y), v /= 0]
      where
        processes = processesByColumn db
        entry (column, v) = case M.lookup name weights >>= \(_, w) -> weightAt w column of
            Just weight -> Right ((name, processes V.! column), v * weight)
            Nothing ->
                Left
                    ( "The process "
                        <> processIdToText db (processes V.! column)
                        <> " of '"
                        <> name
                        <> "' has no score per unit: its supply chain reaches a process that cannot be solved for, or a link to another database whose unit cannot be converted."
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

{- | The score of a part, each process's scaling times its score per unit:
the score is linear in the scalings, so this is the score of the part without
building its inventory.
-}
scoreFromPerUnit :: M.Map Text Vector -> Scalings -> Double
scoreFromPerUnit perUnit part = sum (M.intersectionWith (\unitScores (_, v) -> U.sum (U.zipWith (*) unitScores v)) perUnit part)

-- | The process of each column of a database's matrix.
processesByColumn :: Database -> V.Vector ProcessId
processesByColumn db = V.update (V.replicate (V.length (dbActivityIndex db)) 0) (V.imap (\pid column -> (fromIntegral column, fromIntegral pid)) (dbActivityIndex db))

{- | A part as a solution scored the way the score is: every database of the
score listed in its order, each at the part's scaling (zero where the part
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
            | otherwise = maybe (SharedSolver.atZero r) (\(_, v) -> r{reachedScaling = v}) (M.lookup (reachedName r) part)
