{-# LANGUAGE OverloadedStrings #-}

module TestHelpers (
    loadSampleDatabase,
    loadSampleDatabaseWithPath,
    withinTolerance,
    assertVectorNear,
    findFlowByName,
    findActivityByUUID,
) where

import Types
import Database (buildDatabaseWithMatrices)
import UnitConversion (defaultUnitConfig)
import Control.Monad (zipWithM_)
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T
import Database.Loader (loadDatabase)
import Test.Hspec
import qualified Data.Vector.Unboxed as U

-- | Load a SAMPLE database for testing
loadSampleDatabase :: String -> IO Database
loadSampleDatabase sampleName = loadSampleDatabaseWithPath sampleName

-- | Load database from a specific path
loadSampleDatabaseWithPath :: String -> IO Database
loadSampleDatabaseWithPath path = do
    loadResult <- loadDatabase ("test-data/" ++ path)
    case loadResult of
        Left err -> error $ "Failed to load test database: " ++ show err
        Right simpleDb ->
            buildDatabaseWithMatrices defaultUnitConfig (sdbActivities simpleDb) (sdbFlows simpleDb) (sdbUnits simpleDb)

-- | Check if two floating point numbers are within tolerance
withinTolerance :: Double -> Double -> Double -> Bool
withinTolerance tolerance expected actual = abs (expected - actual) < tolerance

-- | Assert that a vector is near expected values
assertVectorNear :: String -> Double -> U.Vector Double -> [Double] -> Expectation
assertVectorNear label tolerance actualVec expectedList = do
    let actual = U.toList actualVec
    length actual `shouldBe` length expectedList
    zipWithM_ (\a e -> withinTolerance tolerance e a `shouldBe` True) actual expectedList

-- | Find a flow by name in the database
findFlowByName :: Database -> Text -> Maybe Flow
findFlowByName db name =
    let flows = M.elems (dbFlows db)
     in case filter (\f -> T.toLower (flowName f) == T.toLower name) flows of
            (f:_) -> Just f
            [] -> Nothing

-- | Find an activity by UUID in the database
-- Search in ProcessId table for matching activity UUID
findActivityByUUID :: Database -> UUID -> Maybe Activity
findActivityByUUID db uuid =
    let processIdTable = dbProcessIdTable db
        matchingIndices = [i | (i, (actUUID, _)) <- zip [0..] (V.toList processIdTable), actUUID == uuid]
     in case matchingIndices of
            (idx:_) -> Just $ dbActivities db V.! idx
            [] -> Nothing
