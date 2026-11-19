{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The path from a process to one of its suppliers, when the supplier lives
in another database.

On the two databases of 'DependencyLicenceSpec' the root buys one unit of the
dependency's process. Both processes are named "act-FR", so only the process
id tells the root's namesake from the supplier.
-}
module PathToSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Foldable (toList)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Servant (ServerError (..))
import Test.Hspec

import API.Routes (getActivityPathTo)
import Config (defaultConfig)
import qualified Database.Manager as DM
import Types

import CrossDBRegionalLCIAFixture (linkAt, mkDB)
import DependencyFlowClosureSpec (install)
import DependencyLicenceSpec (configFor, dependency, inventoryKept, managerOn, producing, referenceOf, root, rootPid, runIn, runInError)

-- | The id and the local ratio of each step of a path, or the HTTP code it was refused with.
pathOn :: Database -> Licence -> Maybe Text -> Maybe Text -> IO (Either Int [(Value, Value)])
pathOn depDb licence target targetId = do
    manager <- managerOn root depDb licence
    fmap steps <$> runIn manager (getActivityPathTo "root" rootPid target targetId)
  where
    steps :: Value -> [(Value, Value)]
    steps (Object o)
        | Just (Array path) <- KM.lookup "path" o =
            [ (fromMaybe Null (KM.lookup "processId" s), fromMaybe Null (KM.lookup "localStepRatio" s))
            | Object s <- toList path
            ]
    steps _ = []

depPid :: Database -> Int -> Text
depPid db i = "dep::" <> processIdToText db (fromIntegral i)

-- | A dependency whose first process, the one the root buys, buys half a unit of a second one.
twoDeep :: Database
twoDeep =
    base
        { dbActivities = V.imap (\i act -> act{activityLines = packExchanges [referenceOf (1 + i)]}) (dbActivities base)
        , dbTechnosphereTriples = U.singleton (SparseTriple 1 0 0.5)
        }
  where
    base :: Database
    base = mkDB 1 ["FR", "FR"] [(0, 2.0)]

{- | Three databases in a row: the root buys from @dep@, whose licence keeps
its amounts, and @dep@ buys from @far@, open. The supply chain lists @far@'s
process; the only route to it crosses @dep@.
-}
behindWithheld :: IO DM.DatabaseManager
behindWithheld = do
    manager <- DM.initDatabaseManager defaultConfig DM.NoCache
    DM.addDatabase manager (configFor "root" LicenceUnstated)
    DM.addDatabase manager (configFor "dep" inventoryKept)
    DM.addDatabase manager (configFor "far" LicenceUnstated)
    install manager "far" far
    install manager "dep" (linkAt dependency far "far" 0 1.0){dbDependsOn = ["far"]}
    install manager "root" root
    pure manager
  where
    far :: Database
    far = producing 200 [(0, 1.0)]

spec :: Spec
spec = do
    it "reaches a supplier of another database by its name" $
        pathOn dependency LicenceUnstated (Just "act-FR") Nothing
            `shouldReturn` Right [(String rootPid, Null), (String (depPid dependency 0), Number 1)]

    it "reaches the same supplier by its process id" $
        pathOn dependency LicenceUnstated Nothing (Just (depPid dependency 0))
            `shouldReturn` Right [(String rootPid, Null), (String (depPid dependency 0), Number 1)]

    it "follows the edges inside the other database" $
        pathOn twoDeep LicenceUnstated Nothing (Just (depPid twoDeep 1))
            `shouldReturn` Right [(String rootPid, Null), (String (depPid twoDeep 0), Number 1), (String (depPid twoDeep 1), Number 0.5)]

    it "finds no path into a database whose licence keeps its amounts" $
        pathOn dependency inventoryKept Nothing (Just (depPid dependency 0)) `shouldReturn` Left 404

    it "refuses a target named both by name and by process id" $
        pathOn dependency LicenceUnstated (Just "act-FR") (Just (depPid dependency 0)) `shouldReturn` Left 400

    it "refuses a request with no target" $
        pathOn dependency LicenceUnstated Nothing Nothing `shouldReturn` Left 400

    it "says the route crosses a withheld database when it does" $ do
        manager <- behindWithheld
        answer <- runInError manager (getActivityPathTo "root" rootPid Nothing (Just ("far::" <> processIdToText (producing 200 [(0, 1.0)]) 0)))
        fmap (\e -> (errHTTPCode e, BSL.unpack (errBody e))) (either Just (const Nothing) answer)
            `shouldSatisfy` \case
                Just (404, body) -> "dep" `isInfixOf` body && "licence" `isInfixOf` body
                _ -> False
