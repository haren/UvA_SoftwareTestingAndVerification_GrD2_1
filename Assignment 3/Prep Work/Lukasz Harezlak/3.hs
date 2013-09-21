import Week3
import Techniques

import System.Random
import Data.List


---- 3.

genIntList :: IO [Int]
genIntList = getIntsInRange 10 0 100

getIntInRange :: Int -> Int -> IO Int
getIntInRange x y = getStdRandom(randomR(x,y))

getIntsInRange :: Int -> Int -> Int -> IO [Int]
getIntsInRange 0 _ _ = return []
getIntsInRange n x y = do
	f <- (getIntInRange x y)
	fs <- (getIntsInRange (n-1) x y)
	return (f:fs)

-- 4.
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation a b = (elem (a) (permutations b))

--found an in-built function doing the same - permutations
--permutation :: Eq a => [a] -> [[a]]
--permutation [] = [[]]
--permutation xs = [x:ys | x <- xs, ys <- permutation(delete x xs)]

-- 5.

testPermutations :: IO Bool
testPermutations = do
	randomList <- genIntList -- generate random list
	let permutationsOfRandomList = permutations randomList -- generate its permutation	
	let notARandomListPermutation = permutations (map (*2) randomList)
	return (elem randomList permutationsOfRandomList 
		&& not (elem randomList notARandomListPermutation))
	
-- 6.

-- TRY TO FIGURE THIS OUT!

-- 7.
