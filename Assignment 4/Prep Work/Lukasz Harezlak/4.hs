import Week4
import SetOrd
import System.Random
import Data.List

-- 4.1


-- 4.2 
-- 2 hours
getRandomInt :: IO Int
getRandomInt = getStdRandom(random)

getRandomIntInRange :: Int -> Int -> IO Int
getRandomIntInRange x y = getStdRandom(randomR(x,y))

getRandomList :: Int -> IO [Int]
getRandomList 0 = return []
getRandomList x = do
	a <- getRandomInt
	as <- getRandomList (x-1)
	return (a:as)

getRandomOrderedList :: Int -> Int -> Int -> IO [Int]
getRandomOrderedList 0 _  _= return []
getRandomOrderedList x min max = do
	a <- getRandomIntInRange min max --max integer 536870911
	as <- getRandomOrderedList (x-1) (a+1) (max+1)
	return (a:as)

getRandomSet :: Int -> Int -> Int -> IO (Set Int)
getRandomSet a min max = do
	list <- (getRandomOrderedList a min max)
	return (list2set list)

-- 4.3
-- 2 hours

setIntersection :: (Ord a) => Set a -> Set a -> Set a 
setIntersection (Set[]) b = Set[]
setIntersection a (Set[]) = Set[]
setIntersection (Set(xs))(Set(ys)) = list2set ([x | x <- xs, y <- ys, x==y])

setUnion :: (Ord a) => Set a -> Set a -> Set a 
setUnion (Set [])     set2  =  set2
setUnion (Set (x:xs)) set2  = 
   insertSet x (setUnion (Set xs) set2)

setDifference :: (Ord a) => Set a -> Set a -> Set a
setDifference (Set(xs)) (Set(ys)) = list2set(filter (\x -> not (elem x ys)) xs)

-- testing

testSetOperations :: IO Bool
testSetOperations = do
	set1 <- getRandomSet 10 0 100
	set2 <- getRandomSet 10 0 100
	let intersection = setIntersection set1 set2
	let correctSubsets = subSet intersection set1 && subSet intersection set2
	list1 <- getRandomOrderedList 5 0 1000
	list2 <- getRandomOrderedList 5 0 1000
	let set3 = list2set list1
	let set4 = list2set list2
	let listIntersect = [x | x <- list1, y <- list2, x==y]
	let setIntersect = setIntersection set3 set4
	let setIntersectfromListIntersect = list2set listIntersect
	let listUnion = list1 ++ list2
	let setUnion_ = setUnion set3 set4
	let setUnionfromListUnion = list2set listUnion
	let listDifference = filter (\x -> not (elem x list2)) list1
	let setDifference_ = setDifference set3 set4
	let setDifferenceFromListDifference = list2set listDifference
	-- check if operation performed on lists			
	-- is equal to operation performed on sets 
	-- A == B when A is subset of B and B is subset of A	
	return (correctSubsets 
		&& (subSet setIntersect setIntersectfromListIntersect) 
		&& (subSet setIntersectfromListIntersect setIntersect)
		&& (subSet setUnion_ setUnionfromListUnion)
		&& (subSet setUnionfromListUnion setUnion_)
		&& (subSet setDifference_ setDifferenceFromListDifference)
		&& (subSet setDifferenceFromListDifference setDifference_))
		

-- 4.4

type Rel a = [(a,a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
	nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- transitive closure of a relation
--trClos :: Ord a => Rel a -> Rel a
--trClos [] = []
--trClos (xs) = getElements xs

-- return a list of all elements
-- build tuples from this list


--getElements :: Ord a => Rel a -> [Int]
--getElements [] = []
--getElements (x:xs) = (fst x) ++ (snd x) ++ (getElements xs)

testRel = [(1,2), (1,3)]
testRel2 = [(2,4), (3,7)]

-- 4.5
