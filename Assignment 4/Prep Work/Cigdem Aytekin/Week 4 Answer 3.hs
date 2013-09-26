
module Week43

where 
  
import Data.List
import SetOrd

-- Cigdem Aytekin
-- Week 4, assignment 3


intersectionSet :: (Ord a) => Set a -> Set a -> Set a
intersectionSet (Set []) set2 = (Set [])
intersectionSet (Set (x:xs)) set2  
    | (inSet x set2 ) = insertSet x (intersectionSet (Set xs) set2)
    | otherwise       = (intersectionSet (Set xs) set2)
    
    
setDifference :: (Ord a) => Set a -> Set a -> Set a
setDifference (Set[]) set2 = (Set [])
setDifference (Set (x:xs)) set2
    | (inSet x set2)  = (setDifference (Set xs) set2)
    | otherwise       = insertSet x (setDifference (Set xs) set2)
    
    
    
    
