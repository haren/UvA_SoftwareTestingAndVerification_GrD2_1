module Week3_4 where

import Data.List
import Data.Ord 

--quicksort :: (Ord a) => [a] -> [a]
--quicksort a = sort a

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation a b = (a ==  b)
				--where a = (quicksort a)



test = isPermutation [3,4,2] [3,4,2] -- True
test2 = isPermutation [3,4,2] [3,2,4]  -- False, has to be true after sorting

test3 = sort([4,2,1])
test4 = quicksort([4,2,1])