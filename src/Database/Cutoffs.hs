{-# LANGUAGE OverloadedStrings #-}

{- | The product inputs a calculation met that no loaded database supplies.

The supplier-gap report lists them for a whole database; a result names only
the ones its own chain reaches, those of a process the solution scales by
something other than zero.
-}
module Database.Cutoffs (
    GapIndex (..),
    gapIndexOf,
    gapIndexIn,
    gapIndexMoved,
    refuses,
    refusesSomewhere,
    linkCheckIn,
    Cutoffs (..),
    noCutoffs,
    cutoffInputsIn,
    cutoffsReached,
    cutoffsOf,
    ChainRefusal (..),
    chainRefusalOf,
    chainRefusedPrefix,
    chainRefusalSentence,
    CheckedSolution,
    checkedSolution,
    checkedCutoffs,
    checked,
    checkedOf,
    reshaped,
) where

import API.Types (CutoffInput (..), WithheldCutoffs (..))
import Data.Indexing (groupInOrder)
import Data.List (partition, sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (isNothing)
import Data.Ord (Down (..))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Database.Loader (GapEdge (..), GapReason (..), gapEdgesForLoaded, gapEdgesOfProcesses, gapReasons)
import Matrix (activityNormalizationFactor, linkConsumer, processScaling)
import SharedSolver (CrossDBSolution (..), Reached (..))
import Types (BlockerReason (..), CrossDBLink (..), Database (..), ProcessId, findProcessId)

{- | A database's unsupplied edges, by the process that asks for them, and
whether the database takes from another one, which decides what an identity
nobody ships means ('refuses'). Its links to other databases that the
databases loaded do not answer are kept by consumer too ('LinkCheck'): a solve
counts them as zero, as it does an unsupplied edge.
-}
data GapIndex = GapIndex
    { giTakesFromAnother :: Bool
    , giByConsumer :: M.Map ProcessId [GapEdge]
    , giUnanswered :: M.Map ProcessId [(CrossDBLink, BlockerReason)]
    -- ^ The links carrying an amount that the loaded databases do not answer, and why
    }
    deriving (Show, Eq)

{- | One scan of a database read alone: no dependency declared, its links taken
as answered, as by a walk of the database that follows none.
-}
gapIndexOf :: Database -> GapIndex
gapIndexOf = gapIndexIn (const Nothing) []

{- | One scan of the database, so a result only looks up its own processes:
its links checked against the databases loaded, and the dependencies its
configuration declares. A database takes from another when it declares one,
even when none is loaded and so none supplied a link: an identity nobody
ships then names a process of the missing dependency.
-}
gapIndexIn :: LinkCheck -> [Text] -> Database -> GapIndex
gapIndexIn answered declared db =
    (indexedByConsumer db (gapEdgesForLoaded db))
        { giTakesFromAnother = not (null (dbDependsOn db)) || not (null declared)
        , giUnanswered =
            NE.toList
                <$> groupInOrder
                    [ (pid, (l, why))
                    | l <- dbCrossDBLinks db
                    , cdlCoefficient l /= 0
                    , Just why <- [answered l]
                    , Just (pid, _) <- [linkConsumer db l]
                    ]
        }

{- | The index of a database whose processes in @moved@ changed, from the index
of the database before: theirs scanned again, the others' kept.
-}
gapIndexMoved :: Database -> [ProcessId] -> GapIndex -> GapIndex
gapIndexMoved db moved before =
    before{giByConsumer = M.union (giByConsumer fresh) (M.withoutKeys (giByConsumer before) (S.fromList moved))}
  where
    fresh = indexedByConsumer db (gapEdgesOfProcesses db moved)

indexedByConsumer :: Database -> [GapEdge] -> GapIndex
indexedByConsumer db edges =
    GapIndex
        { giTakesFromAnother = not (null (dbDependsOn db))
        , giUnanswered = M.empty
        , giByConsumer =
            NE.toList
                <$> groupInOrder
                    [ (pid, e)
                    | e <- edges
                    , Just pid <- [M.lookup (gapConsumerAct e, gapConsumerProd e) (dbProcessIdLookup db)]
                    ]
        }

{- | Whether an unsupplied input stops a calculation that sums the chain. It
does when a database that could be loaded would supply it, since the result
would then count as zero what that database holds.

It does not for the cut-offs the publisher or a curator made: an identity of
the database's own release that the release does not ship (an identity names
one dataset, so in a database that takes from no other it can only be one of
its own), a link the relink mapping declined, a waste left without a
treatment. In a database that does take from another one, an identity nobody
ships may be one of that other database's, missing from the version loaded.
-}
refuses :: Bool -> GapReason -> Bool
refuses takesFromAnother reason = case reason of
    GapBlocked reasons -> any ((/= "alias_declined") . brReason) reasons
    GapDanglingIdentity -> takesFromAnother
    GapWasteInput -> False

{- | Whether some input of a database refuses a calculation whose chain reaches
it: an unsupplied edge 'refuses' holds for, or a link the loaded databases do
not answer. What a database's status says, so the status and the refusals
agree.
-}
refusesSomewhere :: GapIndex -> Bool
refusesSomewhere index =
    not (M.null (giUnanswered index))
        || any (refuses (giTakesFromAnother index) . gapReason) (concat (M.elems (giByConsumer index)))

{- | Whether the loaded databases answer a link to another database: 'Nothing'
when they do, or why not. A link whose database is not loaded, or whose
database lacks its supplier, counts as zero in a solve: such a link refuses
a calculation that sums the chain, as any input a loadable database would
supply does.
-}
type LinkCheck = CrossDBLink -> Maybe BlockerReason

-- | The links the databases loaded under these names answer.
linkCheckIn :: (Text -> Maybe Database) -> LinkCheck
linkCheckIn loaded link = case loaded (cdlSourceDatabase link) of
    Nothing -> Just (BlockerReason "dependency_not_loaded" (Just (cdlSourceDatabase link)))
    Just dep
        | isNothing (findProcessId dep (cdlSupplierActUUID link) (cdlSupplierProdUUID link)) ->
            Just (BlockerReason "supplier_missing" (Just (cdlSourceDatabase link)))
        | otherwise -> Nothing

{- | The cut-offs of a result: named where the licence shows them, counted where
it does not. A result that sums the chain holds no refusing one ('checked').
-}
data Cutoffs = Cutoffs
    { cutoffShown :: [CutoffInput]
    , cutoffWithheld :: [WithheldCutoffs]
    }
    deriving (Show, Eq)

noCutoffs :: Cutoffs
noCutoffs = Cutoffs{cutoffShown = [], cutoffWithheld = []}

{- | What one cut-off input is grouped by: the same product asked for the same
way, of the same named supplier, as the gap report groups its entries.
-}
data CutoffKey = CutoffKey
    { ckDatabase :: Text
    , ckProduct :: Text
    , ckSupplier :: Maybe Text
    , ckLocation :: Text
    , ckUnit :: Text
    }
    deriving (Eq, Ord)

-- | What the processes asking for one cut-off product add up to.
data Met = Met
    { metAmount :: Double
    , metConsumers :: S.Set ProcessId
    , metReasons :: NE.NonEmpty BlockerReason
    , metRefusing :: Bool
    }

instance Semigroup Met where
    a <> b =
        Met
            { metAmount = metAmount a + metAmount b
            , metConsumers = metConsumers a <> metConsumers b
            , metReasons = NE.nub (metReasons a <> metReasons b)
            , metRefusing = metRefusing a || metRefusing b
            }

{- | The cut-off inputs a solution meets, largest demand first: its unsupplied
edges, and its links the loaded databases do not answer.

A process's input of @a@ per its reference amount @r@ asks @x * a / r@ of the
chain when the process runs at @x@: the matrix column holds @a / r@. A
database listed twice in the solution (two links reaching it, or a cycle)
adds its demands up.

The index must be the one of the database each entry of the solution carries,
and is read at that database's process identifiers without a bound check: an
index taken from another version of the database stops the request rather
than answering with the cut-offs of a chain that is not this one.
-}
cutoffInputsIn :: (Text -> GapIndex) -> [Reached] -> [CutoffInput]
cutoffInputsIn indexOf scalings =
    sortOn (Down . abs . ciAmount) (map input (M.toList grouped))
  where
    grouped :: M.Map CutoffKey Met
    grouped = M.fromListWith (flip (<>)) (edgesMet <> linksMet)

    edgesMet, linksMet :: [(CutoffKey, Met)]
    edgesMet =
        [ (keyOf name e, Met (x * gapAmount e / activityNormalizationFactor db pid) (S.singleton pid) (gapReasons (gapReason e)) (refuses (giTakesFromAnother index) (gapReason e)))
        | Reached name db scaling <- scalings
        , let index = indexOf name
        , (pid, edges) <- M.toList (giByConsumer index)
        , let x = processScaling db scaling pid
        , x /= 0
        , e <- edges
        ]
    linksMet =
        [ (linkKeyOf name l, Met (x * cdlCoefficient l / activityNormalizationFactor db pid) (S.singleton pid) (NE.singleton why) True)
        | Reached name db scaling <- scalings
        , (pid, links) <- M.toList (giUnanswered (indexOf name))
        , let x = processScaling db scaling pid
        , x /= 0
        , (l, why) <- links
        ]

    keyOf :: Text -> GapEdge -> CutoffKey
    keyOf name e = CutoffKey name (gapFlowName e) (gapSupplierActivity e) (gapLocation e) (gapUnit e)

    linkKeyOf :: Text -> CrossDBLink -> CutoffKey
    linkKeyOf name l = CutoffKey name (cdlFlowName l) Nothing (cdlLocation l) (cdlExchangeUnit l)

    input :: (CutoffKey, Met) -> CutoffInput
    input (k, m) =
        CutoffInput
            { ciDatabase = ckDatabase k
            , ciProduct = ckProduct k
            , ciSupplier = ckSupplier k
            , ciLocation = ckLocation k
            , ciUnit = ckUnit k
            , ciAmount = metAmount m
            , ciConsumers = S.size (metConsumers m)
            , ciReasons = metReasons m
            , ciRefusing = metRefusing m
            }

{- | The cut-offs of the databases a calculation ran: named in those it shows,
one count for each database in @withheld@, whose licence keeps the products it
misses to itself. A database listed twice counts once, its entries summed.
-}
cutoffsReached :: (Text -> GapIndex) -> S.Set Text -> [Reached] -> Cutoffs
cutoffsReached indexOf withheld scalings =
    Cutoffs
        { cutoffShown = cutoffInputsIn indexOf shown
        , cutoffWithheld =
            [ WithheldCutoffs{wcDatabase = name, wcCount = length inputs, wcRefusing = length (filter ciRefusing inputs)}
            | (name, part) <- M.toList (NE.toList <$> groupInOrder [(reachedName s, s) | s <- hidden])
            , let inputs = cutoffInputsIn indexOf part
            , not (null inputs)
            ]
        }
  where
    hidden, shown :: [Reached]
    (hidden, shown) = partition ((`S.member` withheld) . reachedName) scalings

{- | 'cutoffsReached' with the index of every database the calculation ran,
each taken from the very database the calculation carries, as
'cutoffInputsIn' requires.

The lookup answers only the names it was built from; any other is answered
with no gap, and none is asked, every name coming from the same list.
-}
cutoffsOf :: (Text -> Database -> IO GapIndex) -> S.Set Text -> [Reached] -> IO Cutoffs
cutoffsOf indexFor withheld scalings = do
    built <- M.fromList <$> traverse (\(Reached name db _) -> (,) name <$> indexFor name db) scalings
    pure (cutoffsReached (\name -> M.findWithDefault (GapIndex False M.empty M.empty) name built) withheld scalings)

{- | Why a calculation that sums the chain is refused: how many refusing inputs
the chain reaches, and the largest of those the licences let it name.
-}
data ChainRefusal = ChainRefusal
    { crCount :: Int
    , crProducts :: [Text]
    }
    deriving (Show, Eq)

-- | The refusal the refusing cut-offs amount to, if any.
chainRefusalOf :: Cutoffs -> Maybe ChainRefusal
chainRefusalOf cutoffs
    | count == 0 = Nothing
    | otherwise = Just ChainRefusal{crCount = count, crProducts = map ciProduct named}
  where
    named = filter ciRefusing (cutoffShown cutoffs)
    count = length named + sum (map wcRefusing (cutoffWithheld cutoffs))

{- | How the body of every refusal starts, so a client tells it from another
unprocessable request without parsing the rest.
-}
chainRefusedPrefix :: Text
chainRefusedPrefix = "Refused: the chain of "

{- | The sentence a refusal is answered with, about the process @what@: the
count, the first products by what the chain asks of them, and where to read
them all.
-}
chainRefusalSentence :: Text -> ChainRefusal -> Text
chainRefusalSentence what refusal =
    chainRefusedPrefix
        <> what
        <> " reaches "
        <> T.pack (show (crCount refusal))
        <> (if crCount refusal == 1 then " input" else " inputs")
        <> " that only a database not loaded would supply"
        <> named (crProducts refusal)
        <> "; its supply chain names them."
  where
    named [] = ""
    named ps = " (" <> T.intercalate ", " (take 3 ps) <> (if length ps > 3 then ", \8230" else "") <> ")"

{- | A solution whose chain reaches no input that a database not loaded would
supply, with the cut-offs it does reach. Only 'checked' and 'checkedOf' make
one, so a calculation that sums the chain cannot be handed a solution nobody
checked.
-}
data CheckedSolution = CheckedSolution
    { checkedSolution :: CrossDBSolution
    , checkedCutoffs :: Cutoffs
    -- ^ The cut-offs the chain reaches, none of them refusing
    }

-- | Check a solution against the indexes of the databases it reached.
checked :: (Text -> GapIndex) -> S.Set Text -> CrossDBSolution -> Either ChainRefusal CheckedSolution
checked indexOf withheld sol = verdictOn sol (cutoffsReached indexOf withheld (NE.toList (csScalings sol)))

-- | 'checked' with the index of every database the solution reached, taken as 'cutoffsOf' takes them.
checkedOf :: (Text -> Database -> IO GapIndex) -> S.Set Text -> CrossDBSolution -> IO (Either ChainRefusal CheckedSolution)
checkedOf indexFor withheld sol = verdictOn sol <$> cutoffsOf indexFor withheld (NE.toList (csScalings sol))

verdictOn :: CrossDBSolution -> Cutoffs -> Either ChainRefusal CheckedSolution
verdictOn sol cutoffs = maybe (Right (CheckedSolution sol cutoffs)) Left (chainRefusalOf cutoffs)

{- | A checked solution changed in a way that reaches no new process: an
emission policy applied, the part of some databases taken alone, the vectors
laid out as another solution's. What it was checked for still holds.
-}
reshaped :: (CrossDBSolution -> CrossDBSolution) -> CheckedSolution -> CheckedSolution
reshaped f c = c{checkedSolution = f (checkedSolution c)}
