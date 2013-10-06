module Lab5_1

where

import Data.List
import Week5

-- Assignment 1
mergeSrt :: Ord a => [a] -> [a]
mergeSrt [] = []
mergeSrt (x:xs) = merge [x] (mergeSrt xs)

mergeAssert :: Ord a => [a] -> [a] -> [a]
mergeAssert = assert2 sortedProp $ assert2 sublistProp merge


-- Assignment 2
split :: [a] -> ([a],[a])
split xs = let n = (length xs) `div` 2 in (take n xs, drop n xs)

mergeSplit :: Ord a => [a] -> [a]
mergeSplit [] = []
mergeSplit xs = let n = (split xs) in (merge (sort (fst n) ) (sort (snd n)))


test1 = split [4,3,5,1,7,9]
test2 = mergeSplit [4,3,5,1,7,9]


-- Assignment 5
testSubGrid :: [Value] -> Bool   -- Unique pattern of 1 to 9
testSubGrid [] = True
testSubGrid (xs) = (length xs) == 9 && (xs == nub xs) && null (deleteFirstsBy (==) xs [1..9]) 

testRow :: [[Int]] -> Bool
testRow [] = True
testRow (x:xs) = testSubGrid x