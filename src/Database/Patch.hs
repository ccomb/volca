{-# LANGUAGE OverloadedStrings #-}

{- | Declarative, idempotent adjustments to the amounts a database states,
applied to the just-parsed activities before the matrices are built - the
equivalent of an import script that rewrites the inventory, expressed as data
('Types.ExchangePatch') instead of an imperative function.

A patch is a pure transform of the parsed activities: applying the same
patches to the same source files always yields the same result, so rebuilding
a database never compounds an adjustment. The patch list is part of
'Types.BuildInputs', so a matrix cache built under another list is rebuilt
rather than reused.
-}
module Database.Patch (
    ProcessNames (..),
    applyExchangePatches,
    exchangeMatches,
    describeExchangePatch,
) where

import Data.List (mapAccumL)
import qualified Data.Map.Strict as M
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Types (
    Activity (..),
    BiosphereFlow (..),
    Exchange (..),
    ExchangePatch (..),
    ExchangePatchMatch (..),
    SimpleDatabase (..),
    TechnosphereFlow (..),
    UUID,
    WasteFlow (..),
    applyPatchOp,
    describePatchOp,
    exchangeAmount,
    exchangeFlowId,
    exchangeIsProductOutput,
    exchangeIsReference,
    withAmount,
 )

-- | What a selector reads of the process an exchange belongs to.
data ProcessNames = ProcessNames
    { pnActivity :: !Text
    -- ^ The name of the activity doing the exchanging.
    , pnProduct :: !Text
    -- ^ The name of the product that process is addressed by.
    , pnLocation :: !Text
    -- ^ Where the activity was documented.
    }
    deriving (Eq, Show)

{- | Apply every patch, in order, to the exchanges of a database. Each patch
scans every process, and every exchange of it that the selector matches has
its amount replaced by 'applyPatchOp'.

The rows saying what a process makes (its reference and its coproducts) are
left alone: they define the unit every other amount is stated per, so
rescaling one would restate the whole process rather than adjust a line of it.

Returns the database alongside, for each patch, how many exchanges it touched
- a patch that touches none is very likely a selector typo, and the caller
(which has a logging effect) is expected to surface that.
-}
applyExchangePatches :: [ExchangePatch] -> SimpleDatabase -> (SimpleDatabase, [(ExchangePatch, Int)])
applyExchangePatches [] sdb = (sdb, [])
applyExchangePatches patches sdb =
    let names = flowNames sdb
        (activities, stats) = mapAccumL (step names) (sdbActivities sdb) patches
     in (sdb{sdbActivities = activities}, stats)
  where
    step :: M.Map UUID Text -> M.Map (UUID, UUID) Activity -> ExchangePatch -> (M.Map (UUID, UUID) Activity, (ExchangePatch, Int))
    step names activities patch =
        let (touched, patched) = M.mapAccumWithKey (patchProcess names patch) 0 activities
         in (patched, (patch, touched))

-- | The name of every flow the database states, technosphere, biosphere and waste alike.
flowNames :: SimpleDatabase -> M.Map UUID Text
flowNames sdb =
    M.unions
        [ M.map tfName (sdbTechFlows sdb)
        , M.map bfName (sdbBioFlows sdb)
        , M.map wfName (sdbWasteFlows sdb)
        ]

{- | One process against one patch: its exchanges with the matched amounts
replaced, and how many were.
-}
patchProcess :: M.Map UUID Text -> ExchangePatch -> Int -> (UUID, UUID) -> Activity -> (Int, Activity)
patchProcess names patch touched (_, productId) activity =
    let process =
            ProcessNames
                { pnActivity = activityName activity
                , pnProduct = M.findWithDefault "" productId names
                , pnLocation = activityLocation activity
                }
        patched = map (patchExchange names patch process) (exchanges activity)
     in (touched + length (filter snd patched), activity{exchanges = map fst patched})

-- | One exchange against one patch: its amount as the patch leaves it, and whether it matched.
patchExchange :: M.Map UUID Text -> ExchangePatch -> ProcessNames -> Exchange -> (Exchange, Bool)
patchExchange names patch process exchange
    | exchangeIsReference exchange || exchangeIsProductOutput exchange = (exchange, False)
    | exchangeMatches (xpMatch patch) process (M.lookup (exchangeFlowId exchange) names) =
        (withAmount (applyPatchOp (xpOp patch) (exchangeAmount exchange)) exchange, True)
    | otherwise = (exchange, False)

{- | Does this exchange match the selector? Every field the selector sets must
match (conjunction); an unset field imposes no constraint. The flow name is
the one the database gives the flow exchanged, and an exchange whose flow the
database does not name matches no selector that names one.
-}
exchangeMatches :: ExchangePatchMatch -> ProcessNames -> Maybe Text -> Bool
exchangeMatches selector process flow =
    maybe True (`containedIn` pnActivity process) (xpmActivityNameContains selector)
        && maybe True (`containedIn` pnProduct process) (xpmProductNameContains selector)
        && maybe True (== pnLocation process) (xpmLocation selector)
        && maybe True (\wanted -> flow == Just wanted) (xpmFlowName selector)
        && maybe True (\wanted -> any (containedIn wanted) flow) (xpmFlowNameContains selector)

-- | Is the first text part of the second, either one's case being the source's business?
containedIn :: Text -> Text -> Bool
containedIn needle haystack = T.toLower needle `T.isInfixOf` T.toLower haystack

{- | Human-readable label for a patch, for log lines - its description when
given, else a rendering of the selector and operation.
-}
describeExchangePatch :: ExchangePatch -> Text
describeExchangePatch patch = case xpDescription patch of
    Just description -> description
    Nothing -> describeMatch (xpMatch patch) <> " " <> describePatchOp (xpOp patch)

describeMatch :: ExchangePatchMatch -> Text
describeMatch selector =
    T.intercalate ", " $
        concat
            [ ["activity-name-contains=" <> value | value <- maybeToList (xpmActivityNameContains selector)]
            , ["product-name-contains=" <> value | value <- maybeToList (xpmProductNameContains selector)]
            , ["location=" <> value | value <- maybeToList (xpmLocation selector)]
            , ["flow-name=" <> value | value <- maybeToList (xpmFlowName selector)]
            , ["flow-name-contains=" <> value | value <- maybeToList (xpmFlowNameContains selector)]
            ]
