module Week5Ans

where

import Data.List
import Week5 


-- Question 1: Find a suitable assertion for the function
-- mergeSrt
 
mergeSrt :: Ord a => [a] -> [a]
mergeSrt [] = []
mergeSrt (x:xs) = merge [x] (mergeSrt xs)

mergeSrtA :: Ord a => [a] -> [a]
mergeSrtA = assert1 (\xs ys -> sorted xs) mergeSrt 
    
mergeSrtA2 :: Ord a => [a] -> [a]
mergeSrtA2 = assert1 (equalLength) mergeSrt
    
equalLength :: Ord a => [a] -> [a] -> Bool
equalLength xs ys = (length xs == length ys)
 
 
 
-- Question 2: Another approach to merge sort is to start by splitting 
-- the list to be sorted in equal parts, recursively sort the parts, next merge.

split       :: [a] -> ([a],[a])
split xs    = let
        n = (length xs) `div` 2
       in
        (take n xs, drop n xs)
        
-- mergeSort2 takes one list and returns the sorted list as result
-- it first splits the list into two, recursively sorts two parts 
-- and merges the two parts.        
mergeSort2  :: Ord a => [a] -> [a]
mergeSort2 []       = []
mergeSort2 [x]      = [x]
mergeSort2 [x, y]   = if ( x <= y) then [x,y]
                      else [y,x] 
mergeSort2 xs       = merge (mergeSort2 list1) (mergeSort2 list2)
    where (list1, list2) = split xs
                    

isPermutation :: Ord a => [a] -> [a] -> Bool
isPermutation [] []         = True
isPermutation [] xs         = False
isPermutation ys []         = False
isPermutation (x:xs) list2  
                            | length (x:xs) /= length list2     = False
                            | not (x `elem` list2)              = False
                            | otherwise                         = (isPermutation xs (delete x list2) ) 
    
mergeSort2Assert :: Ord a => [a] -> [a]
mergeSort2Assert = assert1 (isPermutation) mergeSort2
                    
   
                 
-- Question 3: 
                    
                    
                    






        
