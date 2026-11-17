{- | The index of the rows using each flow is laid out in one array, and says
exactly what collecting the rows line by line says.
-}
module FlowIndexSpec (spec) where

import Data.Indexing (groupInOrder)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Test.Hspec
import TestHelpers (loadSampleDatabase)

import Database (flowIndex)
import Types (Database (..), exchangeFlowId, exchanges)

spec :: Spec
spec = describe "the index of the rows using each flow" $ do
    it "names, for each flow, the row of every line that uses it" $ do
        db <- loadSampleDatabase "SAMPLE.min3"
        let activities = dbActivities db
            lineByLine = groupInOrder [(exchangeFlowId ex, row) | (row, activity) <- zip [0 ..] (V.toList activities), ex <- exchanges activity]
        M.map VU.toList (flowIndex activities) `shouldBe` M.map NE.toList lineByLine
        M.size lineByLine `shouldSatisfy` (> 1)

    it "keeps one entry per line, so a flow used twice by a row counts twice" $ do
        db <- loadSampleDatabase "SAMPLE.min3"
        let entries = sum (VU.length <$> flowIndex (dbActivities db))
        entries `shouldBe` sum (length . exchanges <$> dbActivities db)
