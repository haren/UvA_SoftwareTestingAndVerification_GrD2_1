module Week42

where 
    
import Data.List
import System.Random
import SetOrd

-- Cigdem Aytekin
-- Week 4, assignment 2

-- Implement a random data generator for the datatype Set Int

getRandomInt :: IO Int
getRandomInt = getStdRandom(random)

getRandomIntInRange :: Int -> Int -> IO Int
getRandomIntInRange x y = getStdRandom(randomR(x,y))

getRandomIntsInRange :: Int -> Int -> Int -> IO [Int]
getRandomIntsInRange 0 _ _ = return []
getRandomIntsInRange n x y = do
    f <- getRandomIntInRange x y
    fs <- getRandomIntsInRange (n-1) x y
    return (f : fs)
    
    
    
getARandomSet :: Int -> Int -> Int -> IO (Set Int)
getARandomSet n lowBound upBound 
                        | (lowBound >= upBound) = error "Upper boundary should be greater than lower boundary"
                        | (n <= 0)              = error "Number of set elements should be greater than 0"
                        | otherwise             = do                 
                                                    randIntList <- getRandomIntsInRange n lowBound upBound
                                                    let auxSet = list2set (nub randIntList)
                                                    return auxSet 
                                    
    
    
    
    