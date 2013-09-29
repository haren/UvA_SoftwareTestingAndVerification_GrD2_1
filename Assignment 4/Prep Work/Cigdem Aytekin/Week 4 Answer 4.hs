module Week44

where 
  
import Data.List
import SetOrd

-- Cigdem Aytekin
-- Week 4, assignment 4

type Rel a = [(a,a)]

-- infixr 5 @@ : define a new infix operator @@, 
-- which has priority 5

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]


-- isEmptyRel returns true if a given relation is 
-- empty and otherwise false
isEmptyRel :: Eq a => Rel a -> Bool
isEmptyRel []   = True
isEmptyRel r    = False


-- isTransitive checks if a given relation 
-- is transitive. It uses the auxiliary function auxTuples
isTransitive :: Eq a => Rel a -> Bool
isTransitive [] = True
isTransitive r  = isEmptyRel (auxTuples r)  


-- auxTuples gives the list of tuples which are not
-- in the relation but which should be in a relation
-- for a relation to be transitive
-- an empty list, a list with one element and a
-- list with two elements are by definition transitive
auxTuples :: Eq a => Rel a -> Rel a
auxTuples []    = []
auxTuples r     = [(x,z) | (x,y) <-r, (w,z) <- r, (y == w) && not ((x,z) `elem` r)]


-- returns the union of two given relations
unionRel :: Eq a => Rel a -> Rel a -> Rel a
unionRel [] r   = r
unionRel r []   = r
unionRel r s    = nub (r ++ s)


-- trClos gives the transitive closure relation
-- for a given relation
-- E.g., trClos [(1,2),(2,3),(3,4)] should give
-- [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)].
trClos :: Ord a => Rel a -> Rel a
trClos []   = []
trClos r    
        |   (isTransitive r)    = r
        |   otherwise           = trClos ( (unionRel (r @@ r) r) ) 



