import System.Random
import Data.List

getRandomInt :: IO Int
getRandomInt = getStdRandom(random)

{--
getRandomInts :: IO [Int]
getRandomInts = do
	r <- getRandomInt
	f <- getRandomInts
	return (r : f)

takeRandoms :: Int -> IO [Int] 
takeRandoms n = do
	o <- getRandomInts
	return ((take n) o)
--}

getRandomInts :: Int -> IO [Int]
getRandomInts 0 = return []
getRandomInts n = do
	f <- getRandomInt
	fs <- getRandomInts (n-1)
	return (f : fs)