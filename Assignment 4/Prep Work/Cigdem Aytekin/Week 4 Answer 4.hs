module Week44

where 
  
import Data.List
import SetOrd

-- Cigdem Aytekin
-- Week 4, assignment 4

--  
-- 
--  
-- 

type Rel a = [(a,a)]

-- infixr 5 @@ : define a new infix operator @@, 
-- which has priority 5

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- Use this to implement a function
-- trClos :: Ord a => Rel a -> Rel a
-- that gives the transitive closure of a relation, 
-- where the relation is represented as a list of pairs.
-- E.g., trClos [(1,2),(2,3),(3,4)] should give
-- [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)].