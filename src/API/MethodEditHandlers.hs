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
    editMethodFactorsHandler,
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
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Servant (ServerError, err400, err404, err409, errBody, throwError)

import API.DatabaseHandlers (guardMutation)
import API.Types (
    ActivateResponse (..),
    CompartmentAPI (..),
    FactorEditRequest,
    HistoryKindAPI (..),
    MethodChangeAPI (..),
    MethodEditResponse (..),
    MethodFlowAPI (..),
    MethodHistoryEntry (..),
    toFactorEdit,
 )
import App.Env (AppEnv (..), AppM)
import Database.Manager (getMethodCollection)
import Method.Edit (EditOutcome (..), HistoryLine (..), MethodEditRefusal (..), copyMethodCollection, editMethodFactors, methodHistory, refusalText, undoMethodEdit)
import Method.EditPlan (EditEffect (..))
import Method.Journal (LineKind (..), MethodOp (..))
import Method.Patch (describePatch)
import Method.Types (Compartment (..), Method (..), MethodCF (..), MethodCollection (..), ScoringSet (..))
import Service.CompareMethods (factorSide)

copyMethodCollectionHandler :: Text -> Text -> AppM ActivateResponse
copyMethodCollectionHandler collection newName = do
    guardMutation
    manager <- asks aeDbManager
    copied <- liftIO (copyMethodCollection manager collection newName) >>= either refuse pure
    pure (ActivateResponse True ("Copied " <> collection <> " as " <> copied) Nothing)

editMethodFactorsHandler :: Text -> FactorEditRequest -> AppM MethodEditResponse
editMethodFactorsHandler collection req = do
    guardMutation
    manager <- asks aeDbManager
    edit <- either (refuse . EditRefused) pure (toFactorEdit req)
    outcomeToAPI <$> (liftIO (editMethodFactors manager collection edit) >>= either refuse pure)

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
outcomeToAPI (EditOutcome line effect) = MethodEditResponse line (eeTouched effect) (eeBefore effect) (eeAfter effect)

{- | A collection's journal as a reader of the collection names it. A category
is named after the collection in use; one it no longer holds, or a collection
not loaded, is named by its identifier.
-}
historyToAPI :: Maybe MethodCollection -> [HistoryLine] -> [MethodHistoryEntry]
historyToAPI loaded = map entry
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
    categoryName :: UUID -> Text
    categoryName c = case [methodName m | m <- maybe [] mcMethods loaded, methodId m == c] of
        name : _ -> name
        [] -> UUID.toText c

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
