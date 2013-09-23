import Week4
import SetOrd
import System.Random
import Data.List

-- 4.1


-- 4.2 
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

getListOfRandomOrderedLists :: Int -> IO [[Int]]
getListOfRandomOrderedLists 0 = return []
getListOfRandomOrderedLists x = do
	numberOfListMembers <- getRandomIntInRange 0 10
	a <- getRandomOrderedList (numberOfListMembers) 0 100
	as <- getListOfRandomOrderedLists (x-1)
	return (a:as)
	

getRandomSet :: Set a
getRandomSet = do
	list <- (getListOfRandomOrderedLists 2)
	return Set (list)
	
{-
-- getRandomListOfLists = [[]]
--getRandomListOfLists :: [[a]]
--getRandomListOfLists = do
--	numberOfSublists <- getRandomIntInRange 0 10	
--	return [[numberOfSublists]]
--	--return (getRandomLists numberOfSublists)

----getRandomLists :: Int -> [[a]]
----getRandomLists x = []
-}
-- 4.3


-- 4.4


-- 4.5
