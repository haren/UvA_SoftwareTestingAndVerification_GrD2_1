module Week3 where

import Data.List
import System.Random

-- Assignment 3
getRandomInt :: IO Int
getRandomInt = getStdRandom(random)

getRandomIntInRange :: Int -> Int -> IO Int
getRandomIntInRange x y = getStdRandom(randomR(x,y))

getRandomInts :: Int -> IO [Int]
getRandomInts 0 = return []
getRandomInts n = do
    f <- getRandomInt
    fs <- getRandomInts (n-1)
    return (f : fs)

getRandomIntsInRange :: Int -> Int -> Int -> IO [Int]
getRandomIntsInRange 0 _ _ = return []
getRandomIntsInRange n x y = do
    f <- getRandomIntInRange x y
    fs <- getRandomIntsInRange (n-1) x y
    return (f : fs)
    
-- Assignment 4
-- Fastest solution
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = (length xs == length ys) && (null (deleteFirstsBy (==) xs ys))

-- Slow solution
isPermutation' :: Eq a => [a] -> [a] -> Bool
isPermutation' a b = (elem (a) (permutations b))

-- tests
testPermutations = inputPermutations 10000 -- amount of variations to test
inputPermutations :: Int -> IO Bool
inputPermutations 0 = return testPermutations' -- combining a recursive and a non-recursive test together
inputPermutations x = do
  i <- getRandomIntsInRange 10 1 10000 -- generate a random list of 10 ints within boundaries
  r <- getRandomInt
  o <- inputPermutations (x-1)
  let s = isPermutation (sort i) i -- check if the sorted random list is a permutation of the unaltered random list
  let s' = (not) (isPermutation (drop 1 i) i) -- drop 1 from the list to compare it to the random list
  let s'' = (not) (isPermutation i (r : i)) -- add a random number to the list to compare to, to make the permutation check fail
  let selfPermutation = isPermutation i i -- should be a permutation of itself
  return (selfPermutation && s && s' && s'' && o) -- combine all permutation checks

-- really slow - generates all permutations
testPermutations' :: IO Bool
testPermutations' = do
	randomList <- genIntList -- generate random list
	let permutationsOfRandomList = permutations randomList -- generate its permutation	
	let notARandomListPermutation = permutations (map (*2) randomList)	
	randomIndex <- getStdRandom(randomR(0, length permutationsOfRandomList))
	randomIndex2 <- getStdRandom(randomR(0, length notARandomListPermutation))
	return (isPermutation randomList (permutationsOfRandomList !! randomIndex) 
		&& not (isPermutation randomList (notARandomListPermutation !! randomIndex2)))
  
