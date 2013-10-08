module Main

where
import Data.List
import Data.Bits
import System.Random
import System.TimeIt
import Week6

-- cabal install timeit

getRandomIntInRange :: Int -> Int -> IO Int
getRandomIntInRange x y = getStdRandom(randomR(x,y))

getRandomIntsInRange :: Int -> Int -> Int -> IO [Int]
getRandomIntsInRange 0 _ _ = return []
getRandomIntsInRange n x y = do
    f <- getRandomIntInRange x y
    fs <- getRandomIntsInRange (n-1) x y
    return (f : fs)

exM' :: Integer -> Integer -> Integer -> Integer
exM' base expo modulus  = rightToLeftBinary 1 base expo modulus
            where rightToLeftBinary :: Integer -> Integer -> Integer -> Integer -> Integer
                  rightToLeftBinary result base expo modulus
                    | (expo > 0) = rightToLeftBinary newResult newBase newExponent modulus -- recurse until expo is 0
                    | otherwise = result
                    where newResult = if ((.&.) expo 1 == 1) -- Check for odd number
                                      then mod (result * base) modulus -- Intermediate result
                                      else result
                          newExponent = shiftR expo 1 -- Bit shift (dev by 2 basically)
                          newBase = mod (base * base) modulus

-- Test for modular exponentiation
-- ghc -o weeksix weeksix.hs for preventing interpreter out of memory
main = do
  amountToTest <- getRandomIntInRange 10 100
  bases <- getRandomIntsInRange amountToTest 10000000 1000000000
  exponents <- getRandomIntsInRange amountToTest 10000000 1000000000
  moduluses <- getRandomIntsInRange amountToTest 10 1000000000
  
  putStrLn ("Starting " ++ show amountToTest ++ " tests of different modulo exponentiations")
  
  testExponentialModulus' bases exponents moduluses
  where
        testExponentialModulus' :: [Int] -> [Int] -> [Int] -> IO()
        testExponentialModulus' [] _ _ = putStrLn "End"
        testExponentialModulus' (x:xs) (y:ys) (z:zs) = do
          let x' = toInteger x
              y' = toInteger y
              z' = toInteger z
              
          putStrLn("-- Test for input: " ++ show x' ++ "^" ++ show y' ++ " modulo " ++ show z)
          
          timeIt $ putStrLn ("ExpM: " ++ (show ((expM x' y' z'))))
          timeIt $ putStrLn ("ExM': " ++ (show ((exM' x' y' z'))))
          putChar '\n'
          testExponentialModulus' xs ys zs




