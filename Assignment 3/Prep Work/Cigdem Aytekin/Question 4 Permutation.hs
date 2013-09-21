module Module34 where
import Week3
import Techniques
import Data.List

-- Cigdem Aytekin, UvA student nr.: 104 63 135


-- Question 4: Write a function that returns true if the  
-- arguments are permutations of each other
-- for example [0,0,2] is permutation of [0,2,0], but not of [2,2,0]


isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] []			= True
isPermutation [] xs			= False
isPermutation ys []			= False
isPermutation (x:xs) list2 	
							| length (x:xs) /= length list2		= False
							| not (x `elem` list2)				= False
							| otherwise 						= (isPermutation xs (delete x list2) ) 




