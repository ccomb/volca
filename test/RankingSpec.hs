{- | The contributions to a score come largest first in absolute value, with
their share of the absolute sum, and stop at a number of rows or once they
cover a percentage of it.
-}
module RankingSpec (spec) where

import Data.Either (isLeft)
import Test.Hspec

import Impact.Ranking (Ranked (..), RowCut (..), rankRows, rowCut)

-- | The rows and their two percentages, rounded to a tenth.
listed :: [Ranked Double] -> [(Double, Double, Double)]
listed = map (\r -> (rkRow r, tenth (rkAbsolutePercent r), tenth (rkCumulativePercent r)))
  where
    tenth :: Double -> Double
    tenth x = fromIntegral (round (x * 10) :: Integer) / 10

spec :: Spec
spec = describe "Impact.Ranking" $ do
    it "lists a credit by its size, its share taken on the absolute sum" $
        listed (rankRows id [] (FirstRows 10) [10, -30, 60])
            `shouldBe` [(60, 60, 60), (-30, 30, 90), (10, 10, 100)]

    it "keeps rows until they cover more than the percentage, the row crossing it included" $
        map rkRow (rankRows id [] (Covering 80 100) [50, 30, 15, 5]) `shouldBe` [50, 30, 15]

    it "stops at the cap even when the percentage is not reached" $
        map rkRow (rankRows id [] (Covering 99 2) [50, 30, 15, 5]) `shouldBe` [50, 30]

    it "counts the unlisted parts in the sum, so the rows cover less of it" $
        listed (rankRows id [100] (Covering 80 100) [50, 50])
            `shouldBe` [(50, 25, 25), (50, 25, 50)]

    it "gives no share when every contribution is zero" $
        listed (rankRows id [] (FirstRows 2) [0, 0]) `shouldBe` [(0, 0, 0), (0, 0, 0)]

    it "reads a number of rows, or a percentage capped by it, and refuses a percentage out of range" $ do
        rowCut 20 Nothing Nothing `shouldBe` Right (FirstRows 20)
        rowCut 20 (Just 5) Nothing `shouldBe` Right (FirstRows 5)
        rowCut 20 (Just 40) (Just 80) `shouldBe` Right (Covering 80 40)
        rowCut 20 Nothing (Just 80) `shouldBe` Right (Covering 80 100)
        rowCut 20 Nothing (Just 0) `shouldSatisfy` isLeft
        rowCut 20 Nothing (Just 120) `shouldSatisfy` isLeft
