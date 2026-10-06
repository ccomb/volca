{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- | 'AppM' is @'ReaderT' 'AppEnv' 'Handler'@; 'runApp' is the @AppM ~> Handler@
mapping passed to Servant's 'hoistServer'.
-}
module App.Env (
    AppEnv (..),
    AppM (..),
    runApp,
    counted,
    countedEach,
    usageLookup,
) where

import qualified Config
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT (..), asks)
import Data.Foldable (for_)
import Data.Text (Text)
import Database.Manager (DatabaseManager, LoadedDatabase (..), getDatabase, releasesRead)
import Servant (Handler, ServerError)
import qualified Service
import Types (Activity (..), Database (..))
import Usage (Lookup (..), ProcessKey (..), ProcessNaming (..), UsageKind, UsageLog, Use (..), recordUse)

-- | Read-only application environment threaded through every request.
data AppEnv = AppEnv
    { aeDbManager :: !DatabaseManager
    , aeMaxTreeDepth :: !Int
    , aePassword :: !(Maybe String)
    , aeHostingConfig :: !(Maybe Config.HostingConfig)
    , aeClassificationPresets :: ![Config.ClassificationPreset]
    , aeDataVersion :: !(Maybe Config.DataVersion)
    , aeUsageLog :: !(Maybe UsageLog)
    -- ^ Where computations are noted, when the configuration keeps a usage log
    , aeReader :: !(Maybe Text)
    -- ^ Whoever the request in hand was made for, as the server in front of the engine named them
    }

newtype AppM a = AppM {unAppM :: ReaderT AppEnv Handler a}
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv, MonadError ServerError, MonadThrow, MonadCatch, MonadMask)

runApp :: AppEnv -> AppM a -> Handler a
runApp env (AppM m) = runReaderT m env

-- | What a usage line is told of the database and the process it counts.
usageLookup :: DatabaseManager -> Lookup
usageLookup manager = Lookup{releasesOf = releasesRead manager, namingOf = naming}
  where
    naming :: Text -> ProcessKey -> IO (Maybe ProcessNaming)
    naming dbName (ProcessKey pid) = (>>= namedIn . ldDatabase) <$> getDatabase manager dbName
      where
        namedIn :: Database -> Maybe ProcessNaming
        namedIn db = either (const Nothing) (Just . describe) (Service.resolveActivityByProcessId db pid)
          where
            describe :: Activity -> ProcessNaming
            describe a = ProcessNaming (activityName a) (Service.getReferenceProductName (dbTechFlows db) a) (activityLocation a)

{- | Run a computation on a process, and note it in the usage log once it has
answered: a refused or failed one read nothing.
-}
counted :: UsageKind -> Text -> ProcessKey -> AppM a -> AppM a
counted kind dbName processId = countedEach kind dbName (const [processId])

-- | 'counted' for a computation on several processes, read from its answer: only those it answered for.
countedEach :: UsageKind -> Text -> (a -> [ProcessKey]) -> AppM a -> AppM a
countedEach kind dbName answered action = do
    result <- action
    env <- asks id
    for_ (aeUsageLog env) $ \lg ->
        for_ (answered result) $ \processId ->
            liftIO (recordUse lg (usageLookup (aeDbManager env)) (aeReader env) (Use kind dbName processId))
    pure result
