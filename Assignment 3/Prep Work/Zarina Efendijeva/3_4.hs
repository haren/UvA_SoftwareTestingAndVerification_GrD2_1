module Week3_4 where

import Data.List

-- HELL YEAH!!!!!

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation a b =
  let equal x = (b == x)
  in null(filter equal(permutations(a))) /= True


test = isPermutation [3,4,2] [3,4,2] -- True
test2 = isPermutation [3,4,2] [3,2,4]  -- 
test3 = isPermutation [3,4,2,9] [9,2,4,3]

test4 = isPermutation [3,4,2,9] [9,2,4,9]  --False

--test3 = sort(permutations[4,2,1])
