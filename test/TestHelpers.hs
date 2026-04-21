{-# LANGUAGE OverloadedStrings #-}

module TestHelpers (
    loadSampleDatabase,
    loadSampleDatabaseWithPath,
    withinTolerance,
    assertVectorNear,
    findFlowByName,
    findActivityByUUID,
    mkDepLookupFromMap,
    linkDatabases,
    mkSolverFromDb,
) where

import Control.Monad (zipWithM_)
import qualified Data.Map as M
import qualified Data.Map.Strict as MS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Database (buildDatabaseWithMatrices)
import Database.Loader (loadDatabase)
import qualified SharedSolver as SS
import Test.Hspec
import Types
import UnitConversion (defaultUnitConfig)

-- | Load a SAMPLE database for testing
loadSampleDatabase :: String -> IO Database
loadSampleDatabase sampleName = loadSampleDatabaseWithPath sampleName

-- | Load database from a specific path
loadSampleDatabaseWithPath :: String -> IO Database
loadSampleDatabaseWithPath path = do
    loadResult <- loadDatabase ("test-data/" ++ path)
    case loadResult of
        Left err -> error $ "Failed to load test database: " ++ show err
        Right simpleDb -> do
            dbResult <- buildDatabaseWithMatrices defaultUnitConfig (sdbActivities simpleDb) (sdbFlows simpleDb) (sdbUnits simpleDb)
            case dbResult of
                Left err -> error $ "Failed to build matrix: " ++ show err
                Right db -> return db

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
            (f : _) -> Just f
            [] -> Nothing

{- | Find an activity by UUID in the database
Search in ProcessId table for matching activity UUID
-}
findActivityByUUID :: Database -> UUID -> Maybe Activity
findActivityByUUID db uuid =
    let processIdTable = dbProcessIdTable db
        matchingIndices = [i | (i, (actUUID, _)) <- zip [0 ..] (V.toList processIdTable), actUUID == uuid]
     in case matchingIndices of
            (idx : _) -> Just $ dbActivities db V.! idx
            [] -> Nothing

{- | Turn a map of loaded (Database, SharedSolver) pairs into the lookup
that the cross-DB solver expects. Replaces the 'noDeps _ = pure Nothing'
stub sprinkled across the test suite.
-}
mkDepLookupFromMap :: MS.Map Text (Database, SS.SharedSolver) -> SS.DepSolverLookup
mkDepLookupFromMap m name = pure (MS.lookup name m)

{- | Build a fresh 'SharedSolver' from a database's technosphere triples.
Lifted from the ad-hoc @mkSolver@ bodies in 'CrossDBSubstitutionSpec' and
'NestedSubstitutionSpec'.
-}
mkSolverFromDb :: Database -> Text -> IO SS.SharedSolver
mkSolverFromDb db name =
    let tech =
            [ (fromIntegral i, fromIntegral j, v)
            | SparseTriple i j v <- U.toList (dbTechnosphereTriples db)
            ]
        n = fromIntegral (dbActivityCount db)
     in SS.createSharedSolver name tech n

{- | Inject a single synthetic cross-DB link from the first activity of
@consumerDb@ to the first activity of @supplierDb@. Required for tests
that need to observe the dep-level substitution recursion without
depending on external fixture data. The coefficient is picked arbitrarily
small (0.1) so the induced dep demand is strictly positive but does not
dominate; unit is inherited from the first activity's refUnit.
-}
linkDatabases ::
    -- | consumer DB (gets the link attached)
    Database ->
    -- | supplier DB
    Database ->
    -- | supplier DB name (stored in cdlSourceDatabase)
    Text ->
    -- | coefficient
    Double ->
    -- | consumer DB with the new link in dbCrossDBLinks
    Database
linkDatabases consumerDb supplierDb supplierName coeff =
    let (consumerAct, consumerProd) = dbProcessIdTable consumerDb V.! 0
        (supplierAct, supplierProd) = dbProcessIdTable supplierDb V.! 0
        link =
            CrossDBLink
                { cdlConsumerActUUID = consumerAct
                , cdlConsumerProdUUID = consumerProd
                , cdlSupplierActUUID = supplierAct
                , cdlSupplierProdUUID = supplierProd
                , cdlCoefficient = coeff
                , cdlExchangeUnit = "kg"
                , cdlFlowName = "synthetic-test-link"
                , cdlLocation = "GLO"
                , cdlSourceDatabase = supplierName
                }
     in consumerDb{dbCrossDBLinks = link : dbCrossDBLinks consumerDb}
