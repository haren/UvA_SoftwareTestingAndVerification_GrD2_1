module Week44

where 
  
import Data.List
import SetOrd
import System.Random

-- Cigdem Aytekin
-- Week 4, assignment 4 and 5

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


-- testTrClosure takes two parameters, the first 
-- parameter is a relation, and the second parameter is
-- the transitive closure of the first parameter.
-- It finds the transitive closure of the first parameter by using 
-- the function trClos from Module44, and compares the two 
-- results.

testTrClosure :: Ord a => Rel a -> Rel a-> IO Bool
testTrClosure r givenClosure = do
                let calculatedClosure = trClos r
                let result = areTwoSetsEqual (list2set calculatedClosure) (list2set givenClosure)
                return result 
                    where 
                        areTwoSetsEqual :: Ord a =>  Set a -> Set a -> Bool
                        areTwoSetsEqual fstSet sndSet = (subSet fstSet sndSet) && (subSet sndSet fstSet)  
             
             
staticTest1 = testTrClosure [(1,2), (2,3), (3,4)] [(1,2), (1,3), (1,4), (2,3), (2, 4), (3,4)]
staticTest2 = testTrClosure [(1,2), (2,3), (3,4)] [(1,2), (1,3), (1,4), (2,3), (2, 4)]
staticTest3 = testTrClosure [(1,2), (2,3)] [(1,2), (2,3), (1,3)]
staticTest4 = testTrClosure [(1,2)] [(1,2), (2,3)]


-- testPropClosure takes a relation as parameter,
-- it calculates the transitive closure of this relation 
-- using trClos function and checks two properties:
-- the transitive closure should be transitive
-- and the given relation should be subset of 
-- the transitive closure

testPropClosure :: Ord a => Rel a -> IO Bool
testPropClosure givenRel = do
                let calculatedClosure = trClos givenRel
                let result = (isTransitive calculatedClosure) && (subSet (list2set givenRel) (list2set calculatedClosure))
                return result
                
                
staticPropTest1 = testPropClosure [(1,2), (2,3), (3, 4)]
staticPropTest2 = testPropClosure [(1,2), (1, 3)]
staticPropTest3 = testPropClosure [(1,2)]
staticPropTest4 = testPropClosure [(1,2), (2, 3)]
               
getRandomIntInRange :: Int -> Int -> IO Int
getRandomIntInRange x y = getStdRandom(randomR(x,y))

getARandomTuple     :: Int -> Int -> IO (Int, Int)
getARandomTuple lowBound upBound = do
                                randInt1 <- getRandomIntInRange lowBound upBound
                                randInt2 <- getRandomIntInRange lowBound upBound
                                return (randInt1, randInt2)
                             
                
getARandomListOfTuples ::  Int -> Int -> Int -> IO [(Int, Int)]
getARandomListOfTuples n lowBound upBound 
                        | (lowBound >= upBound)  = error "Upper boundary should be greater than lower boundary"
                        | (n < 0)                = error "Number of set elements should be greater than zero."
                        | (n == 0)               = return []
                        | otherwise              = do
                                                    f <- getARandomTuple lowBound upBound                 
                                                    fs <- getARandomListOfTuples (n-1) lowBound upBound
                                                    return (f:fs)
                
                
list2rel      :: [(Int, Int)] -> Rel Int
list2rel  []      = []
list2rel [(x,y)]  = [(x,y)]
list2rel ((x,y):fs)   = (x,y) : (list2rel fs)

                

getARandomRel :: Int -> Int -> Int -> IO (Rel Int)
getARandomRel n lowBound upBound = do
                               listOfTuples <- getARandomListOfTuples n lowBound upBound 
                               let randomRel = list2rel (nub listOfTuples)
                               return randomRel
                               
                     
dynamicTest1 :: IO Bool          
dynamicTest1 = do 
                    rel <- getARandomRel 5 1 10
                    let propertiesOK = testPropClosure rel
                    return propertiesOK 



                                    





 
