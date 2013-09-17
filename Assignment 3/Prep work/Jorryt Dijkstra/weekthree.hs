import System.Random
import Data.List
import Week3

getRandomInt :: IO Int
getRandomInt = getStdRandom(random)

getRandomIntInRange :: Int -> Int -> IO Int
getRandomIntInRange x y = getStdRandom(randomR(x,y))

getRandomInts :: Int -> IO [Int]
getRandomInts 0 = return []
getRandomInts n = do
	f <- getRandomInt
	fs <- getRandomInts (n-1)
	return (f : fs)

getRandomIntsWithRange :: Int -> Int -> Int -> IO [Int]
getRandomIntsWithRange 0 _ _ = return []
getRandomIntsWithRange n x y = do
	f <- getRandomIntInRange x y
	fs <- getRandomIntsWithRange (n-1) x y
	return (f : fs)

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = (length xs == length ys) && (length (deleteFirstsBy (\x y -> x == y) xs ys) == 0)


testPermutations = inputPermutations 10000 -- amount of variations to test
inputPermutations :: Int -> IO Bool
inputPermutations 0 = return True
inputPermutations x = do
			i <- getRandomIntsWithRange 10 1 10000 -- generate a random list of 10 ints within boundaries
			o <- inputPermutations (x-1)
			let s = isPermutation (sort i) i -- check if the sorted random list is a permutation of the unaltered random list
			return (s && o)