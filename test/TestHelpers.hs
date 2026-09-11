{-# LANGUAGE OverloadedStrings #-}

module TestHelpers (
    unitDef,
    withScratchDataDir,
    loadSampleDatabase,
    loadSampleDatabaseWithPath,
    withinTolerance,
    assertVectorNear,
    findFlowByName,
    mkDepLookupFromMap,
    linkDatabases,
    mkSolverFromDb,
    shippedGeographies,
) where

import Builtin (builtinGeographies)
import Control.Exception (bracket_)
import Control.Monad (zipWithM_)
import qualified Data.ByteString.Lazy as BL
import Data.Either (fromRight)
import qualified Data.Map as M
import qualified Data.Map.Strict as MS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Database (Geographies, buildDatabaseWithMatrices, readGeographies)
import Database.Loader (loadDatabase)
import Database.Manager (hierarchyFromGeographies, parseGeographies)
import qualified SharedSolver as SS
import System.Environment (setEnv, unsetEnv)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Types
import UnitConversion (UnitDef (..), defaultDimensionOrder, defaultUnitConfig, parseDimension)

{- | A unit definition written the way the shipped table writes one: a dimension
expression and a factor.

A hand-written exponent vector is a copy of 'defaultDimensionOrder' with no
author. It keeps parsing the day a slot is added or removed, so an example goes
on comparing a kilogram of its own against a kilogram of the table's and finding
them different dimensions - and only the examples that mix the two notice. Going
through 'parseDimension' leaves the slot list one author.

An expression the parser refuses yields an empty vector, which no unit has, so
it converts with nothing well formed beside it and the example that asked for
it fails. Two refused expressions in one fixture are the hole in that: they
share the empty vector, so they convert into one another at the ratio of their
factors. Only a typo repeated across every row of a fixture reaches it.
-}
unitDef :: Text -> Double -> UnitDef
unitDef dimExpr = UnitDef (fromRight [] (parseDimension defaultDimensionOrder dimExpr))

{- | Run an example with the engine's data directory pointed at a scratch
tree. Anything that writes there - an upload, a copy, an edit journal - then
belongs to the example rather than to the working tree it was run from.
-}
withScratchDataDir :: IO () -> IO ()
withScratchDataDir act =
    withSystemTempDirectory "volca-scratch" $ \dir ->
        bracket_ (setEnv "VOLCA_DATA_DIR" dir) (unsetEnv "VOLCA_DATA_DIR") act

-- | Load a SAMPLE database for testing
loadSampleDatabase :: String -> IO Database
loadSampleDatabase = loadSampleDatabaseWithPath

-- | Load database from a specific path
loadSampleDatabaseWithPath :: String -> IO Database
loadSampleDatabaseWithPath path = do
    loadResult <- loadDatabase defaultUnitConfig ("test-data/" ++ path)
    case loadResult of
        Left err -> error $ "Failed to load test database: " ++ show err
        Right simpleDb -> do
            dbResult <- buildDatabaseWithMatrices (BuildInputs defaultUnitConfig mempty Declared) simpleDb
            case dbResult of
                Left err -> error $ "Failed to build matrix: " ++ show err
                Right db -> return db

-- | Check if two floating point numbers are within tolerance
withinTolerance :: Double -> Double -> Double -> Bool
withinTolerance tolerance expected actual = abs (expected - actual) < tolerance

-- | Assert that a vector is near expected values
assertVectorNear :: String -> Double -> U.Vector Double -> [Double] -> Expectation
assertVectorNear _label tolerance actualVec expectedList = do
    let actual = U.toList actualVec
    length actual `shouldBe` length expectedList
    zipWithM_ (\a e -> withinTolerance tolerance e a `shouldBe` True) actual expectedList

-- | Find a biosphere flow by name in the database (test helper).
findFlowByName :: Database -> Text -> Maybe BiosphereFlow
findFlowByName db name =
    let flows = M.elems (dbBioFlows db)
     in case filter (\f -> T.toLower (bfName f) == T.toLower name) flows of
            (f : _) -> Just f
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
                , cdlConsumerFlowId = UUID.nil
                , cdlSupplierActUUID = supplierAct
                , cdlSupplierProdUUID = supplierProd
                , cdlCoefficient = coeff
                , cdlExchangeUnit = "kg"
                , cdlFlowName = "synthetic-test-link"
                , cdlLocation = "GLO"
                , cdlSourceDatabase = supplierName
                , cdlTiedAlternatives = []
                }
     in consumerDb{dbCrossDBLinks = link : dbCrossDBLinks consumerDb}

{- | The location table the engine ships, read the way a geography filter reads
it. A fixture would pin a test to itself; this pins it to the answer a user
gets. 'StructuredFiltersSpec' checks the table is actually there, so an empty
one cannot pass unnoticed.
-}
shippedGeographies :: Geographies
shippedGeographies =
    readGeographies $
        either (const mempty) hierarchyFromGeographies $
            parseGeographies "the built-in geographies" (BL.toStrict builtinGeographies)
