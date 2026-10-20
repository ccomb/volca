{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Who supplies each input of a database being exported, for the formats
that name a supplier rather than point at it.

EcoSpold 2 and an openLCA package write the supplier's identifier, which any
reader resolves. SimaPro CSV and a Brightway workbook name a product, with the
supplier's activity and location beside it; ILCD names a product and a
location only. What they name has to be worked out: from the database's own
links, and from the links the engine made into the databases it depends on,
which are kept beside the database ('dbCrossDBLinks') rather than on its
exchanges.
-}
module Database.ExportSupplier (
    Elsewhere (..),
    Dependencies (..),
    noDependencies,
    dependenciesOf,
    Maker (..),
    makerOf,
    madeAt,
    sharedProducts,
    boughtShared,
    clashing,
    unnamedWarnings,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import Database.CrossLinking (IndexedDatabase, SupplierEntry (..), findSupplierByActivityProduct)
import Types

-- | A supplier in another database, as a writer names it.
data Elsewhere = Elsewhere
    { elDatabase :: !Text
    , elActivity :: !Text
    , elLocation :: !Text
    }
    deriving (Eq, Show)

-- | The supplier in another database of each input, by consumer and by the input's place among its exchanges.
newtype Dependencies = Dependencies (M.Map (UUID, UUID, Int) Elsewhere)
    deriving (Eq, Show)

-- | What an export of a database alone knows of its dependencies: nothing.
noDependencies :: Dependencies
noDependencies = Dependencies M.empty

{- | The suppliers a database's links into its dependencies name.

An input several suppliers answer, each for a share of it, has no single row to
write in a format that names one supplier per row, so it is refused here rather
than written as one of them. So is a link to a supplier no loaded dependency
holds any more.
-}
dependenciesOf :: M.Map Text IndexedDatabase -> Database -> Either Text Dependencies
dependenciesOf indexed db = do
    named <- traverse elsewhere answered
    case [key | (key, n) <- M.toList (M.fromListWith (+) [(key, 1 :: Int) | (key, _) <- named]), n > 1] of
        [] -> Right (Dependencies (M.fromList named))
        ((act, prod, _) : _) -> Left (refusal act prod "several activities of its dependencies supply it, each a share")
  where
    answered :: [((UUID, UUID, Int), CrossDBLink)]
    answered =
        [ ((cdlConsumerActUUID l, cdlConsumerProdUUID l, aePosition a), l)
        | l <- dbCrossDBLinks db
        , Just a <- [cdlAnswers l]
        ]

    elsewhere :: ((UUID, UUID, Int), CrossDBLink) -> Either Text ((UUID, UUID, Int), Elsewhere)
    elsewhere (key@(act, prod, _), l) =
        case findSupplierByActivityProduct (maybe [] pure (M.lookup source indexed)) supplier of
            ((e, _) : _) -> Right (key, Elsewhere source (seActivityName e) (seLocation e))
            [] -> Left (refusal act prod ("its supplier in " <> source <> " is not loaded"))
      where
        source = cdlSourceDatabase l
        supplier = ProcessRef (cdlSupplierActUUID l) (cdlSupplierProdUUID l)

    refusal :: UUID -> UUID -> Text -> Text
    refusal act prod why =
        "This format names each input's supplier, and cannot for an input of "
            <> maybe (processRefText (ProcessRef act prod)) activityName (getActivity db =<< findProcessId db act prod)
            <> ": "
            <> why
            <> ". EcoSpold 2 and openLCA point at suppliers instead, and can."

-- | Who supplies one input, as far as the export can tell.
data Maker
    = -- | an activity of the exported database
      InFile !Activity
    | -- | an activity of a database it depends on
      InDependency !Elsewhere
    | -- | a link to nothing exported, nor to anything its dependencies were found to hold
      Dangling !UUID
    | -- | no link: the reader is left to find a supplier by the product alone
      NoLink

-- | The supplier of the exchange at this place of this consumer.
makerOf :: Dependencies -> SimpleDatabase -> (UUID, UUID) -> Int -> Exchange -> Maker
makerOf (Dependencies deps) db (act, prod) position = \case
    TechnosphereExchange{techRole = Input, techActivityLinkId = link, techFlowId = flow}
        | Just supplier <- link, Just maker <- M.lookup (supplier, flow) (sdbActivities db) -> InFile maker
        | Just e <- M.lookup (act, prod, position) deps -> InDependency e
        | Just supplier <- link -> Dangling supplier
        | otherwise -> NoLink
    TechnosphereExchange{} -> NoLink
    BiosphereExchange{} -> NoLink
    WasteExchange{} -> NoLink

{- | Where a maker of the file offers its product: the location its reference
states, else its own. It is what a name-linking reader compares an input's
location with.
-}
madeAt :: Activity -> Text
madeAt act =
    fromMaybe (activityLocation act) $
        listToMaybe [code | ex <- exchanges act, exchangeIsReference ex, Just code <- [statedCode (exchangeStatedLocation ex)]]

-- | The products two or more activities of the file make.
sharedProducts :: SimpleDatabase -> S.Set UUID
sharedProducts db = M.keysSet (M.filter ((> 1) . S.size) (makersByProduct db))

makersByProduct :: SimpleDatabase -> M.Map UUID (S.Set UUID)
makersByProduct db =
    M.fromListWith
        S.union
        [ (exchangeFlowId ex, S.singleton act)
        | ((act, _), activity) <- M.toList (sdbActivities db)
        , ex <- exchanges activity
        , exchangeIsReference ex
        ]

{- | The shared products some input of the file buys, each with its makers. A
format that tells makers apart by what it can write of them has to find
something different to write for each.
-}
boughtShared :: SimpleDatabase -> [(UUID, NE.NonEmpty Activity)]
boughtShared db =
    [ (flow, makers)
    | flow <- S.toList (S.intersection shared bought)
    , Just makers <- [NE.nonEmpty [a | ((_, p), a) <- M.toList (sdbActivities db), p == flow]]
    ]
  where
    shared = sharedProducts db
    bought =
        S.fromList
            [ techFlowId ex
            | activity <- M.elems (sdbActivities db)
            , ex@TechnosphereExchange{techRole = Input} <- exchanges activity
            ]

-- | The makers that write the same thing under a key, in groups of two or more.
clashing :: (Ord k) => (Activity -> k) -> NE.NonEmpty Activity -> [NE.NonEmpty Activity]
clashing key makers = filter ((> 1) . NE.length) (NE.groupAllWith key (NE.toList makers))

{- | What a format naming suppliers says of the inputs it could name none for:
those linked to a supplier nothing loaded holds, and those of a shared product
linked to no maker, which no longer meet a product row named by its maker.
-}
unnamedWarnings :: Dependencies -> SimpleDatabase -> [Text]
unnamedWarnings deps db =
    [ T.pack (show (length dangling)) <> " inputs link to a supplier neither the database nor its dependencies hold, so they are written by their product alone:" <> foldMap ("\n  " <>) (take 10 dangling)
    | not (null dangling)
    ]
        <> [ T.pack (show (length unlinked)) <> " inputs buy a product several activities make and link to none of them, so a re-import links them to none:" <> foldMap ("\n  " <>) (take 10 unlinked)
           | not (null unlinked)
           ]
  where
    inputs =
        [ (activityName act <> " · " <> maybe "?" tfName (M.lookup (techFlowId ex) (sdbTechFlows db)), makerOf deps db key i ex, techFlowId ex)
        | (key, act) <- M.toList (sdbActivities db)
        , (i, ex@TechnosphereExchange{techRole = Input}) <- zip [0 ..] (exchanges act)
        ]
    dangling = [line | (line, Dangling _, _) <- inputs]
    unlinked = [line | (line, NoLink, flow) <- inputs, S.member flow shared]
    shared = sharedProducts db
