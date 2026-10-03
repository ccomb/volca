{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : API.MethodEditHandlers
Description : The REST handlers that copy a method collection, change it and read its journal

Each one maps the refusal of 'Method.Edit' to a status a client can branch on
without reading the message, and the journal's lines to what a reader of the
collection calls them.
-}
module API.MethodEditHandlers (
    copyMethodCollectionHandler,
    methodCollectionStatusAPI,
    editMethodFactorsHandler,
    editMethodCategoriesHandler,
    editScoringSetsHandler,
    scoringSetsHandler,
    scoringSetAPI,
    undoMethodEditHandler,
    methodHistoryHandler,
    methodFlowsHandler,
    historyToAPI,
    collectionFlows,
    refusalStatus,
    outcomeToAPI,
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Servant (ServerError, err400, err404, err409, err500, errBody, throwError)

import API.DatabaseHandlers (guardMutation)
import API.Types (
    CategoryEditRequest,
    CompartmentAPI (..),
    FactorEditRequest,
    HistoryKindAPI (..),
    MethodChangeAPI (..),
    MethodCollectionStatusAPI (..),
    MethodEditResponse (..),
    MethodFlowAPI (..),
    MethodHistoryEntry (..),
    RowTermAPI (..),
    RowTermsAPI (..),
    ScoreAPI (..),
    ScoringEditRequest,
    ScoringGestureAPI (..),
    ScoringRowAPI (..),
    ScoringSetAPI (..),
    toCategoryEdit,
    toFactorEdit,
    toScoringEdit,
 )
import App.Env (AppEnv (..), AppM)
import Database.Manager (DatabaseLoadStatus (..), MethodCollectionStatus (..), getMethodCollection, listMethodCollections)
import Method.Edit (EditOutcome (..), HistoryLine (..), MethodEditRefusal (..), copyMethodCollection, editMethodCategories, editMethodFactors, editScoringSets, methodHistory, refusalText, undoMethodEdit)
import Method.EditPlan (EditEffect (..))
import Method.Journal (LineKind (..), MethodOp (..))
import Method.Patch (describePatch)
import Method.Scoring (RowTerms (..), ScoringGesture (..), ScoringRow (..), rowsOf, sumOfRows)
import Method.Types (Compartment (..), Method (..), MethodCF (..), MethodCollection (..), ScoringSet (..))
import Service.CompareMethods (factorSide)

{- | The copy answers with the collection it made, because the name it is
known by is the slug of the one asked for, and the next request needs it.
-}
copyMethodCollectionHandler :: Text -> Text -> AppM MethodCollectionStatusAPI
copyMethodCollectionHandler collection newName = do
    guardMutation
    manager <- asks aeDbManager
    copied <- liftIO (copyMethodCollection manager collection newName) >>= either refuse pure
    statuses <- liftIO (listMethodCollections manager)
    case [s | s <- statuses, mcsName s == copied] of
        [s] -> pure (methodCollectionStatusAPI s)
        _ -> throwError err500{errBody = BSL.fromStrict (TE.encodeUtf8 ("the copy " <> copied <> " was made but is not listed"))}

methodCollectionStatusAPI :: MethodCollectionStatus -> MethodCollectionStatusAPI
methodCollectionStatusAPI s =
    MethodCollectionStatusAPI
        { mcaName = mcsName s
        , mcaDisplayName = mcsDisplayName s
        , mcaDescription = mcsDescription s
        , mcaStatus = case mcsStatus s of
            Loaded -> "loaded"
            PartiallyLinked -> "unloaded"
            Unloaded -> "unloaded"
        , mcaIsUploaded = mcsIsUploaded s
        , mcaPath = mcsPath s
        , mcaMethodCount = mcsMethodCount s
        , mcaFormat = Just (mcsFormat s)
        , mcaSource = mcsSource s
        }

editMethodFactorsHandler :: Text -> FactorEditRequest -> AppM MethodEditResponse
editMethodFactorsHandler collection req = do
    guardMutation
    manager <- asks aeDbManager
    edit <- either (refuse . EditRefused) pure (toFactorEdit req)
    outcomeToAPI <$> (liftIO (editMethodFactors manager collection edit) >>= either refuse pure)

editMethodCategoriesHandler :: Text -> CategoryEditRequest -> AppM MethodEditResponse
editMethodCategoriesHandler collection req = do
    guardMutation
    manager <- asks aeDbManager
    edit <- either (refuse . EditRefused) pure (toCategoryEdit req)
    outcomeToAPI <$> (liftIO (editMethodCategories manager collection edit) >>= either refuse pure)

editScoringSetsHandler :: Text -> ScoringEditRequest -> AppM MethodEditResponse
editScoringSetsHandler collection req = do
    guardMutation
    manager <- asks aeDbManager
    edit <- either (refuse . EditRefused) pure (toScoringEdit req)
    outcomeToAPI <$> (liftIO (editScoringSets manager collection edit) >>= either refuse pure)

scoringSetsHandler :: Text -> AppM [ScoringSetAPI]
scoringSetsHandler collection = do
    manager <- asks aeDbManager
    loaded <- liftIO (getMethodCollection manager collection)
    maybe (refuse (CollectionNotLoaded collection)) (\c -> pure (map (scoringSetAPI c) (mcScoringSets c))) loaded

{- | A set as the rows of a guided grouping. A category a row reads carries
its identifier when the collection holds exactly one category of that name.
-}
scoringSetAPI :: MethodCollection -> ScoringSet -> ScoringSetAPI
scoringSetAPI collection set =
    ScoringSetAPI
        { ssaName = ssName set
        , ssaUnit = ssUnit set
        , ssaDisplayMultiplier = ssDisplayMultiplier set
        , ssaRows = map row (rowsOf set)
        , ssaScores = [ScoreAPI name formula (sumOfRows set name) | (name, formula) <- M.toList (ssScores set)]
        , ssaVariables = ssVariables set <> M.mapWithKey (\v _ -> M.findWithDefault v v (ssLabels set)) (ssComputed set)
        }
  where
    row :: ScoringRow -> ScoringRowAPI
    row r = ScoringRowAPI (srVariable r) (srLabel r) (srUnit r) (terms (srTerms r)) (srNormalization r) (srWeight r)
    terms :: RowTerms -> RowTermsAPI
    terms = \case
        Grouped ts -> RowGrouped [RowTermAPI category (M.lookup category ids) coef | (category, coef) <- ts]
        Written formula -> RowWritten formula
    ids :: M.Map Text UUID
    ids = M.mapMaybe sole (M.fromListWith (<>) [(methodName m, [methodId m]) | m <- mcMethods collection])
    sole :: [UUID] -> Maybe UUID
    sole = \case
        [one] -> Just one
        _ -> Nothing

undoMethodEditHandler :: Text -> Maybe Int -> AppM MethodEditResponse
undoMethodEditHandler collection line = do
    guardMutation
    manager <- asks aeDbManager
    outcomeToAPI <$> (liftIO (undoMethodEdit manager collection line) >>= either refuse pure)

methodHistoryHandler :: Text -> AppM [MethodHistoryEntry]
methodHistoryHandler collection = do
    manager <- asks aeDbManager
    loaded <- liftIO (getMethodCollection manager collection)
    historyToAPI loaded <$> (liftIO (methodHistory manager collection) >>= either refuse pure)

methodFlowsHandler :: Text -> Maybe Text -> Maybe Int -> AppM [MethodFlowAPI]
methodFlowsHandler collection q limit = do
    manager <- asks aeDbManager
    loaded <- liftIO (getMethodCollection manager collection)
    maybe (refuse (CollectionNotLoaded collection)) (pure . collectionFlows (fromMaybe "" q) limit) loaded

refuse :: MethodEditRefusal -> AppM a
refuse refusal = throwError (refusalStatus refusal){errBody = BSL.fromStrict (TE.encodeUtf8 (refusalText refusal))}

-- | One status per refusal, so a client never has to read the message to branch.
refusalStatus :: MethodEditRefusal -> ServerError
refusalStatus = \case
    CollectionNotFound _ -> err404
    CollectionNotLoaded _ -> err404
    NotEditable _ -> err409
    NameTaken _ -> err409
    EditRefused _ -> err400

outcomeToAPI :: EditOutcome -> MethodEditResponse
outcomeToAPI (EditOutcome line effect category) = MethodEditResponse line (eeTouched effect) (eeBefore effect) (eeAfter effect) category

{- | A collection's journal as a reader of the collection names it. A category
is named after the collection in use; one it no longer holds, or a collection
not loaded, is named by its identifier.
-}
historyToAPI :: Maybe MethodCollection -> [HistoryLine] -> [MethodHistoryEntry]
historyToAPI loaded history = map entry history
  where
    entry :: HistoryLine -> MethodHistoryEntry
    entry h =
        MethodHistoryEntry
            { mheLine = hlLine h
            , mheAt = hlAt h
            , mheKind = kindOf (hlKind h)
            , mheUndoes = undoes (hlKind h)
            , mheInEffect = hlInEffect h
            , mheChange = changeOf (hlOp h)
            }
    kindOf :: LineKind -> HistoryKindAPI
    kindOf = \case
        Change -> ChangeLine
        Undoing _ -> UndoLine
        TakenFromConfiguration -> ConfigurationLine
    undoes :: LineKind -> Maybe Int
    undoes = \case
        Undoing k -> Just k
        Change -> Nothing
        TakenFromConfiguration -> Nothing
    changeOf :: MethodOp -> MethodChangeAPI
    changeOf = \case
        SetFactor c f v -> FactorSet (categoryName c) (factorSide f) (mcfValue f) v
        RemoveFactor c _ f -> FactorRemoved (categoryName c) (factorSide f)
        AddFactor c _ f -> FactorAdded (categoryName c) (factorSide f)
        PatchFactors p n -> FactorsPatched (describePatch p) n
        RestoreFactors rs -> FactorsRestored (length rs)
        SetGlobalMethods before after -> UnregionalizedSet before after
        CreateScoringSet s -> ScoringSetCreated (ssName s)
        RemoveScoringSet s -> ScoringSetRemoved (ssName s)
        AddCategory _ m _ -> CategoryAdded (methodName m) (length (methodFactors m))
        RenameCategory _ before after -> CategoryRenamed before after
        SetCategoryUnit c before after -> CategoryUnitSet (categoryName c) before after
        RemoveCategory _ m _ -> CategoryRemoved (methodName m) (length (methodFactors m))
        ChangeScoringSet set gesture _ -> ScoringSetChanged set (gestureAPI gesture)
    gestureAPI :: ScoringGesture -> ScoringGestureAPI
    gestureAPI = \case
        AddedRow label -> RowAdded label
        ChangedRow label -> RowChanged label
        RemovedRow label -> RowRemoved label
        RenamedSet before after -> SetRenamed before after
        SetUnitTo before after -> SetUnitChanged before after
        SetMultiplierTo before after -> MultiplierSet before after
        WroteFormula variable -> FormulaWritten variable
        AddedScore name -> ScoreAdded name
        ChangedScore name -> ScoreChanged name
        RemovedScore name -> ScoreRemoved name
    categoryName :: UUID -> Text
    categoryName c = fromMaybe (UUID.toText c) (M.lookup c names)
    -- The collection's own names first; a category it no longer has keeps
    -- the last name the journal gave it.
    names :: M.Map UUID Text
    names =
        M.fromList [(methodId m, methodName m) | m <- maybe [] mcMethods loaded]
            `M.union` M.fromList (concatMap (namesIn . hlOp) history)
    namesIn :: MethodOp -> [(UUID, Text)]
    namesIn = \case
        AddCategory _ m _ -> [(methodId m, methodName m)]
        RemoveCategory _ m _ -> [(methodId m, methodName m)]
        RenameCategory c _ after -> [(c, after)]
        SetCategoryUnit{} -> []
        SetFactor{} -> []
        RemoveFactor{} -> []
        AddFactor{} -> []
        PatchFactors{} -> []
        RestoreFactors{} -> []
        SetGlobalMethods{} -> []
        CreateScoringSet{} -> []
        RemoveScoringSet{} -> []
        ChangeScoringSet{} -> []

{- | The flows a collection characterizes, once each, whose name holds every
word of the query, case aside; sorted by name, then compartment. Two rows that
differ only in their CAS number or unit are both listed: which one a new
factor should copy is the caller's choice.
-}
collectionFlows :: Text -> Maybe Int -> MethodCollection -> [MethodFlowAPI]
collectionFlows q limit collection =
    take (fromMaybe 50 limit) . S.toAscList $
        S.fromList [flowOf f | m <- mcMethods collection, f <- methodFactors m, matches (mcfFlowName f)]
  where
    needles :: [Text]
    needles = T.words (T.toLower q)
    matches :: Text -> Bool
    matches name = all (`T.isInfixOf` T.toLower name) needles
    flowOf :: MethodCF -> MethodFlowAPI
    flowOf f =
        MethodFlowAPI
            { mflName = mcfFlowName f
            , mflCompartment = (\(Compartment medium sub qualifier) -> CompartmentAPI medium sub qualifier) <$> mcfCompartment f
            , mflFlowId = mcfFlowRef f
            , mflDirection = mcfDirection f
            , mflCas = mcfCAS f
            , mflUnit = mcfUnit f
            }
