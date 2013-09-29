module Week42

where 
    
import System.Random
import Data.List
import SetOrd

-- newtype Set a = Set [a] deriving (Eq,Ord)

getRandomInt :: IO Int
getRandomInt = getStdRandom(random)


getRandomIntInRange :: Int -> Int -> IO Int
getRandomIntInRange x y = getStdRandom(randomR(x,y))


getListOfRandoms :: Int -> IO [Int]
getListOfRandoms 0 = return []
getListOfRandoms n = do
    f <- getRandomInt
    fs <- getListOfRandoms (n-1)
    return (f : fs)


test = getListOfRandoms 6