module Lab5_1

where

import Data.List
import Week5


mergeSrt :: Ord a => [a] -> [a]
mergeSrt [] = []
mergeSrt (x:xs) = merge [x] (mergeSrt xs)

mergeAssert :: Ord a => [a] -> [a] -> [a]
mergeAssert = assert2 sortedProp $ assert2 sublistProp merge