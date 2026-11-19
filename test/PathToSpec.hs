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
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Test.Hspec

import API.Routes (getActivityPathTo)
import Types

import CrossDBRegionalLCIAFixture (mkDB)
import DependencyLicenceSpec (dependency, inventoryKept, managerOn, referenceOf, root, rootPid, runIn)

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
