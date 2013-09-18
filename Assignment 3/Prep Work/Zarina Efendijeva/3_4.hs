module Week3_4 where

import Data.List

-- HELL YEAH!!!!!

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True -- if boss lists empty / then true, since they are equal? Can be change to false if you think so guys.
isPermutation [] _ = False -- 
isPermutation _ [] = False
isPermutation a b =
  let equal x = (b == x)   -- see comments below
  in null(filter equal(permutations(a))) /= True


test = isPermutation [3,4,2] [3,4,2] -- True
test2 = isPermutation [3,4,2] [3,2,4]  -- 
test3 = isPermutation [3,4,2,9] [9,2,4,3]

test4 = isPermutation [3,4,2,9] [9,2,4,9]  --False


-- The result is filtered with a filter that equals it with b. 
-- If this list is null then b is not a permutation of a
-- the null /= True is to get the right boolean statement for that case

