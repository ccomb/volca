{- | The sparse solver answers as a dense elimination of the same system does,
and names the process that stops it where the system has no answer.
-}
module SparseSolverSpec (spec) where

import Control.Monad (void)
import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Primitive.PrimArray (primArrayFromList, primArrayToList)
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as U
import Matrix.Dense (factorLU, solveLU)
import Matrix.Sparse (Extent (..), Unsolvable (..), extent, factorize, solve)
import Matrix.Sparse.Arrays (Lists (..))
import Matrix.Sparse.Order (Elimination (..), eliminate, loops)
import Test.Hspec
import Test.QuickCheck
import Types (SparseTriple (..))

spec :: Spec
spec = do
    describe "the elimination order" $ do
        it "takes each time the node that can create the fewest entries, and keeps the pattern to the entry" $
            forAll shape $ \(n, columns) ->
                let found = eliminate (listsOf columns)
                    expected = slowly n (S.fromList [(row, column) | (column, rows) <- zip [0 ..] columns, row <- rows, row >= 0, row < n, row /= column])
                    order = [node | Step node _ _ <- expected]
                    place = (M.!) (M.fromList (zip order [0 ..]))
                 in primArrayToList (pivotOrder found)
                        === order
                        .&&. map sort (linesOf (below found))
                            === [sort (map place under) | Step _ under _ <- expected]
                        .&&. linesOf (above found)
                            === [[earlier | (earlier, Step _ _ right) <- zip [0 ..] expected, node `elem` right] | node <- order]
        it "fills nothing in a matrix where one process trades with all the others" $
            -- Eliminating the hub first would link every other process to every other.
            let n = 30
                hub = eliminate (listsOf ([1 .. n - 1] : replicate (n - 1) [0]))
             in (length (concat (linesOf (below hub))), length (concat (linesOf (above hub)))) `shouldBe` (n - 1, n - 1)
        it "orders nothing when there is nothing" $
            primArrayToList (pivotOrder (eliminate (listsOf []))) `shouldBe` []

    describe "the loops of a pattern" $ do
        it "put together the nodes that lead to each other and no others, suppliers first" $
            forAll shape $ \(n, columns) ->
                let sets = linesOf (loops (listsOf columns))
                    setOf = M.fromList [(node, nth) | (nth, set) <- zip [0 :: Int ..] sets, node <- set]
                    entries = [(row, column) | (column, rows) <- zip [0 ..] columns, row <- rows, row >= 0, row < n]
                    reach = M.fromList [(node, leadsTo entries node) | node <- [0 .. n - 1]]
                    together a b = (S.member b <$> M.lookup a reach) == Just True && (S.member a <$> M.lookup b reach) == Just True
                 in sort (concat sets)
                        === [0 .. n - 1]
                        .&&. map sort sets
                            === sets
                        .&&. [(a, b) | a <- [0 .. n - 1], b <- [0 .. n - 1], (M.lookup a setOf == M.lookup b setOf) /= together a b]
                            === []
                        .&&. [(row, column) | (row, column) <- entries, M.lookup row setOf > M.lookup column setOf]
                            === []
        it "find no loop where there is no entry" $
            linesOf (loops (listsOf [[], [], []])) `shouldBe` [[0], [1], [2]]
        it "follow a chain far deeper than a recursion could go" $
            -- Each process takes from the next, and the last from the first.
            let n = 200000
             in linesOf (loops (listsOf ([[column + 1] | column <- [0 .. n - 2]] <> [[0]]))) `shouldBe` [[0 .. n - 1]]

    describe "a technosphere factorised" $ do
        it "answers as a dense elimination of the same system" $
            forAll asked $ \(n, triples, demand) ->
                case (factorLU n (dense n triples), solve (factorize n (U.fromList triples)) (U.fromList demand)) of
                    (Right lu, Right supplied) -> counterexample (show supplied) (close supplied (solveLU lu (U.fromList demand)))
                    (expected, found) -> counterexample (show (void expected, found)) False
        it "leaves nothing of the demand unanswered" $
            forAll asked $ \(n, triples, demand) ->
                case solve (factorize n (U.fromList triples)) (U.fromList demand) of
                    Right supplied ->
                        let used = U.accum (+) (U.replicate n 0) [(fromIntegral row, value * supplied U.! fromIntegral column) | SparseTriple row column value <- triples]
                         in property (close (U.zipWith (-) supplied used) (U.fromList demand))
                    Left why -> counterexample (show why) False
        it "answers the same whatever numbers the processes carry" $
            forAll (asked >>= \system@(n, _, _) -> (,) system <$> shuffle [0 .. n - 1]) $ \((n, triples, demand), names) ->
                let named = U.fromList names
                    rename = fromIntegral . (named U.!) . fromIntegral
                    renamed = [SparseTriple (rename row) (rename column) value | SparseTriple row column value <- triples]
                 in case (solve (factorize n (U.fromList triples)) (U.fromList demand), solve (factorize n (U.fromList renamed)) (U.replicate n 0 U.// zip names demand)) of
                        (Right supplied, Right shuffled) ->
                            let largest = U.foldl' max 0 (U.map abs supplied)
                             in counterexample (show (supplied, shuffled)) (U.and (U.zipWith (\x y -> abs (x - y) <= 1e-12 * (1 + largest)) supplied (U.backpermute shuffled named)))
                        refused -> counterexample (show refused) False
        it "adds up the coefficients that name the same entry" $
            solve (factorize 2 (U.fromList [SparseTriple 0 1 0.2, SparseTriple 0 1 0.3])) (U.fromList [0, 1])
                `shouldBe` solve (factorize 2 (U.fromList [SparseTriple 0 1 0.5])) (U.fromList [0, 1])
        it "hands back the demand of processes that take nothing" $
            solve (factorize 3 U.empty) (U.fromList [1, 2, 3]) `shouldBe` Right (U.fromList [1, 2, 3])
        it "solves a single process that feeds itself" $
            -- Half of each unit goes back in: two units made for one delivered.
            solve (factorize 1 (U.fromList [SparseTriple 0 0 0.5])) (U.fromList [1]) `shouldBe` Right (U.fromList [2])
        it "solves a technosphere of no process" $
            solve (factorize 0 U.empty) U.empty `shouldBe` Right U.empty

    describe "a technosphere that cannot be solved" $ do
        it "refuses two processes that feed each other whole, naming the one left with nothing" $
            solve (factorize 2 (U.fromList [SparseTriple 0 1 1, SparseTriple 1 0 1])) (U.fromList [1, 0]) `shouldBe` Left (Singular 1)
        it "refuses a process that takes back all it makes" $
            solve (factorize 1 (U.fromList [SparseTriple 0 0 1])) (U.fromList [1]) `shouldBe` Left (Singular 0)
        it "refuses a coefficient that is not a number, naming its process" $
            solve (factorize 2 (U.fromList [SparseTriple 0 1 (0 / 0)])) (U.fromList [0, 1]) `shouldBe` Left (NotFinite 1)
        it "refuses an infinite coefficient" $
            solve (factorize 2 (U.fromList [SparseTriple 0 1 (1 / 0)])) (U.fromList [0, 1]) `shouldBe` Left (NotFinite 1)
        it "refuses a coefficient that names a process outside the matrix, by its place in the list" $
            solve (factorize 2 (U.fromList [SparseTriple 0 1 0.5, SparseTriple 2 0 0.5])) (U.fromList [1, 0]) `shouldBe` Left (OutOfRange 1)
        it "refuses a demand that has not one entry per process" $
            solve (factorize 2 U.empty) (U.fromList [1, 0, 0]) `shouldBe` Left (WrongLength 3)
        it "answers a demand of nothing with nothing, whatever the matrix" $
            solve (factorize 1 (U.fromList [SparseTriple 0 0 1])) (U.fromList [0]) `shouldBe` Right (U.fromList [0])

    describe "a technosphere with one loop that cannot be solved" $ do
        -- Processes 0 and 1 feed each other whole. Process 0 also takes from
        -- 5, and 2 takes from 0; 3 takes from 4 and has nothing to do with them.
        let stuck = factorize 6 (U.fromList [SparseTriple 0 1 1, SparseTriple 1 0 1, SparseTriple 5 0 0.5, SparseTriple 0 2 0.5, SparseTriple 4 3 0.5])
            only at = U.generate 6 (\i -> if i == at then 1 else 0)
        it "answers a demand that stays clear of the loop" $
            solve stuck (only 3) `shouldBe` Right (U.fromList [0, 0, 0, 1, 0.5, 0])
        it "answers a demand for what the loop takes from, when nothing is asked of the loop" $
            solve stuck (only 5) `shouldBe` Right (only 5)
        it "refuses a demand for a process of the loop" $
            solve stuck (only 0) `shouldBe` Left (Singular 1)
        it "refuses a demand that reaches the loop through its supply chain" $
            solve stuck (only 2) `shouldBe` Left (Singular 1)
        it "refuses a demand that reaches a single process that cannot be solved for, and no other" $
            -- Process 0 takes back all it makes, 1 takes from it, 2 from nobody.
            let lone = factorize 3 (U.fromList [SparseTriple 0 0 1, SparseTriple 0 1 0.5])
             in (solve lone (U.fromList [0, 1, 0]), solve lone (U.fromList [0, 0, 1])) `shouldBe` (Left (Singular 0), Right (U.fromList [0, 0, 1]))

    describe "what a factorisation holds" $ do
        it "counts one loop for processes that feed each other, and the entries left between the others" $
            -- 0 and 1 feed each other, 2 takes from both.
            extent (factorize 3 (U.fromList [SparseTriple 0 1 0.5, SparseTriple 1 0 0.5, SparseTriple 0 2 0.1, SparseTriple 1 2 0.1]))
                `shouldBe` Extent{loopCount = 1, largestLoop = 2, factorEntries = 2, linkEntries = 2, refusedProcesses = 0, smallestPivot = 0.75}

    describe "the same answer on every machine" $
        it "solves a fixed system to the last bit" $
            -- Every sum is taken in one order, whatever the processor: an answer
            -- that moves in its last digit here moved because the order did.
            let n = 12
                fixed = U.fromList [SparseTriple (fromIntegral row) (fromIntegral column) (1 / fromIntegral (7 + 3 * row + 5 * column)) | row <- [0 .. n - 1], column <- [0 .. n - 1], row /= column, (row + 2 * column) `mod` 3 /= 0]
             in (U.toList <$> solve (factorize n fixed) (U.generate n (\i -> fromIntegral (i `mod` 4))))
                    `shouldBe` Right [0.49579634680026435, 1.4776871521607156, 2.429059433469893, 3.354520803607369, 0.352736845331329, 1.3273682290636128, 2.2820944402782373, 3.2837988226807213, 0.2679518525491004, 1.2362573567723893, 2.2387044538119447, 3.227906667278556]

{- | A technosphere of up to sixty processes and a demand. Each process takes
less than one unit in all for each unit it makes, so the system has an answer
and no pivot comes near zero; rows are drawn with repeats, so some entries
come in several parts and some sit on the diagonal.
-}
asked :: Gen (Int, [SparseTriple], [Double])
asked = do
    n <- chooseInt (1, 60)
    triples <- concat <$> traverse (inputs n) [0 .. n - 1]
    demand <- oneof [vectorOf n (choose (-10, 10)), (\at -> [if i == at then 1 else 0 | i <- [0 .. n - 1]]) <$> chooseInt (0, n - 1)]
    pure (n, triples, demand)
  where
    inputs :: Int -> Int -> Gen [SparseTriple]
    inputs n column = do
        -- Few inputs leave many processes outside any loop, many tie them all into one.
        count <- oneof [chooseInt (0, 1), chooseInt (0, min n 8)]
        rows <- vectorOf count (chooseInt (0, n - 1))
        weights <- vectorOf count (choose (-1, 1))
        let share = 0.9 / max 1e-9 (sum (map abs weights))
        pure [SparseTriple (fromIntegral row) (fromIntegral column) (weight * share) | (row, weight) <- zip rows weights]

-- | @I − A@ row by row.
dense :: Int -> [SparseTriple] -> U.Vector Double
dense n triples =
    U.accum
        (+)
        (U.generate (n * n) (\at -> if at `mod` (n + 1) == 0 then 1 else 0))
        [(fromIntegral row * n + fromIntegral column, negate value) | SparseTriple row column value <- triples]

close :: U.Vector Double -> U.Vector Double -> Bool
close found expected = U.length found == U.length expected && U.and (U.zipWith (\x y -> abs (x - y) <= 1e-9 * (1 + abs y)) found expected)

{- | A square pattern by columns, written carelessly: rows repeated, on the
diagonal, and outside the square.
-}
shape :: Gen (Int, [[Int]])
shape = do
    n <- chooseInt (1, 25)
    columns <- vectorOf n (chooseInt (0, min n 5) >>= \count -> vectorOf count (chooseInt (-1, n)))
    pure (n, columns)

listsOf :: [[Int]] -> Lists
listsOf ls = Lists (primArrayFromList (scanl (+) 0 (map length ls))) (primArrayFromList (concat ls))

linesOf :: Lists -> [[Int]]
linesOf (Lists starts items) = zipWith (\from to -> take (to - from) (drop from (primArrayToList items))) bounds (drop 1 bounds)
  where
    bounds :: [Int]
    bounds = primArrayToList starts

-- | Every node a node leads to, itself included, a column leading to the rows it holds.
leadsTo :: [(Int, Int)] -> Int -> S.Set Int
leadsTo entries = go . S.singleton
  where
    go :: S.Set Int -> S.Set Int
    go seen
        | wider == seen = seen
        | otherwise = go wider
      where
        wider :: S.Set Int
        wider = seen <> S.fromList [row | (row, column) <- entries, column `S.member` seen]

-- | A node eliminated, the rows under it and the columns right of it then.
data Step = Step Int [Int] [Int]

{- | The elimination by its definition, on a set of entries off the diagonal:
the cheapest node each time, the lowest number among equals.
-}
slowly :: Int -> S.Set (Int, Int) -> [Step]
slowly n = go (S.fromList [0 .. n - 1])
  where
    go :: S.Set Int -> S.Set (Int, Int) -> [Step]
    go live entries = case S.lookupMin (S.map (\node -> (length (rowOf node) * length (columnOf node), node)) live) of
        Nothing -> []
        Just (_, node) ->
            let under = filter (/= node) (columnOf node)
                right = filter (/= node) (rowOf node)
             in Step node under right : go (S.delete node live) (entries <> S.fromList [(row, column) | row <- under, column <- right, row /= column])
      where
        rowOf, columnOf :: Int -> [Int]
        rowOf node = [column | (row, column) <- S.toList entries, row == node, column `S.member` live]
        columnOf node = [row | (row, column) <- S.toList entries, column == node, row `S.member` live]
