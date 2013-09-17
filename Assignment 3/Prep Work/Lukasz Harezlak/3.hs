import Week3
import Techniques

import System.Random
import Data.List


---- 3.

getIntInRange :: Int -> Int -> IO Int
getIntInRange x y = getStdRandom(randomR(x,y))

getIntsInRange :: Int -> Int -> Int -> IO [Int]
getIntsInRange 0 _ _ = return []
getIntsInRange n x y = do
	f <- (getIntInRange x y)
	fs <- (getIntsInRange (n-1) x y)
	return (f:fs)

--justAtest :: [IO Int] -> IO ()
--justAtest [] = print ("oops")
--justAtest (x:xs) = do
--	print (show x)
--	justAtest xs


-- 4.
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation a b = (elem (a) (permutations b))

--found an in-built function doing the same - permutations
--permutation :: Eq a => [a] -> [[a]]
--permutation [] = [[]]
--permutation xs = [x:ys | x <- xs, ys <- permutation(delete x xs)]

-- 5.

-- 6.

-- 7.
