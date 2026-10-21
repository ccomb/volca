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
    inDependency,
    madeAt,
    namedProducts,
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
    , elReferenceSign :: !Double
    -- ^ Sign of its reference amount: -1 for a treatment written as giving out its waste
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
            ((e, _) : _) -> Right (key, Elsewhere source (seActivityName e) (seLocation e) (seRefSign e))
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
    = -- | the one activity of the file making the product: rows name the product alone
      SoleMaker !Activity
    | -- | one of the activities of the file making a product it buys: rows name the maker
      OneOfMakers !Activity
    | -- | an activity of a database it depends on
      InDependency !Elsewhere
    | -- | a link to nothing exported, nor to anything its dependencies were found to hold
      Dangling !UUID
    | -- | no link: the reader is left to find a supplier by the product alone
      NoLink

{- | The supplier of the exchange at this place of this consumer. An avoided
product names what it stands in for the same way an input names what it buys,
and a waste sent to a treatment of a dependency names that treatment.
Applied to a database once, it works out the products named by their maker
once.
-}
makerOf :: Dependencies -> SimpleDatabase -> (UUID, UUID) -> (Int, Exchange) -> Maker
makerOf (Dependencies deps) db = \(act, prod) (position, ex) -> case ex of
    TechnosphereExchange{techRole = role, techActivityLinkId = link, techFlowId = flow}
        | buys role
        , Just supplier <- link
        , Just maker <- M.lookup (supplier, flow) (sdbActivities db) ->
            if S.member flow named then OneOfMakers maker else SoleMaker maker
        | buys role, Just e <- M.lookup (act, prod, position) deps -> InDependency e
        | buys role, Just supplier <- link -> Dangling supplier
        | otherwise -> NoLink
    BiosphereExchange{} -> NoLink
    WasteExchange{waIsInput = False}
        | Just e <- M.lookup (act, prod, position) deps -> InDependency e
    WasteExchange{} -> NoLink
  where
    named = namedProducts db

-- | Whether a dependency supplies the exchange.
inDependency :: Maker -> Bool
inDependency = \case
    InDependency _ -> True
    SoleMaker _ -> False
    OneOfMakers _ -> False
    Dangling _ -> False
    NoLink -> False

-- | The roles that stand for a product some supplier makes.
buys :: TechRole -> Bool
buys = \case
    Input -> True
    AvoidedProduct -> True
    ReferenceProduct -> False
    ReferenceInput -> False
    Coproduct -> False

{- | Where a maker of the file offers its product: the location its reference
states, else its own. It is what a name-linking reader compares an input's
location with.
-}
madeAt :: Activity -> Text
madeAt act =
    fromMaybe (activityLocation act) $
        listToMaybe [code | ex <- exchanges act, exchangeIsReference ex, Just code <- [statedCode (exchangeStatedLocation ex)]]

{- | The products two or more activities of the file make and some row buys or
avoids: the rows naming them have to say which maker they mean. A product no
row asks for keeps its plain name.
-}
namedProducts :: SimpleDatabase -> S.Set UUID
namedProducts db = S.intersection shared asked
  where
    shared = M.keysSet (M.filter ((> 1) . S.size) (makersByProduct db))
    asked =
        S.fromList
            [ techFlowId ex
            | activity <- M.elems (sdbActivities db)
            , ex@TechnosphereExchange{} <- exchanges activity
            , buys (techRole ex)
            ]

makersByProduct :: SimpleDatabase -> M.Map UUID (S.Set UUID)
makersByProduct db =
    M.fromListWith
        S.union
        [ (exchangeFlowId ex, S.singleton act)
        | ((act, _), activity) <- M.toList (sdbActivities db)
        , ex <- exchanges activity
        , exchangeIsReference ex
        ]

{- | The products named by their maker, each with its makers. A format that
tells makers apart by what it can write of them has to find something
different to write for each.
-}
boughtShared :: SimpleDatabase -> [(UUID, NE.NonEmpty Activity)]
boughtShared db =
    [ (flow, makers)
    | flow <- S.toList (namedProducts db)
    , Just makers <- [NE.nonEmpty [a | ((_, p), a) <- M.toList (sdbActivities db), p == flow]]
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
    [ T.pack (show (length dangling)) <> " inputs link to a supplier neither the database nor its dependencies hold, so a re-import looks for one by the product alone:" <> foldMap ("\n  " <>) (take 10 dangling)
    | not (null dangling)
    ]
        <> [ T.pack (show (length unlinked)) <> " inputs buy a product several activities make and link to none of them, so a re-import links them to none:" <> foldMap ("\n  " <>) (take 10 unlinked)
           | not (null unlinked)
           ]
  where
    inputs =
        [ (activityName act <> " · " <> maybe "?" tfName (M.lookup (techFlowId ex) (sdbTechFlows db)), maker key (i, ex), techFlowId ex)
        | (key, act) <- M.toList (sdbActivities db)
        , (i, ex@TechnosphereExchange{techRole = Input}) <- zip [0 ..] (exchanges act)
        ]
    maker = makerOf deps db
    dangling = [line | (line, Dangling _, _) <- inputs]
    unlinked = [line | (line, NoLink, flow) <- inputs, S.member flow named]
    named = namedProducts db
