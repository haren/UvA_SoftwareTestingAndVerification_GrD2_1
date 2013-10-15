module Main

where
import Week6
import Data.Bits
import Text.Printf
import System.CPUTime
import System.Random

getRandomIntInRange :: Int -> Int -> IO Int
getRandomIntInRange x y = getStdRandom(randomR(x,y))

getRandomIntsInRange :: Int -> Int -> Int -> IO [Int]
getRandomIntsInRange 0 _ _ = return []
getRandomIntsInRange n x y = do
    f <- getRandomIntInRange x y
    fs <- getRandomIntsInRange (n-1) x y
    return (f : fs)

-- Improved exM function using right to left binary method
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
                          

main :: IO ()
main = do    
    let amountToTest = 50000
    bases <- getRandomIntsInRange amountToTest 100000000000000000000000 1000000000000000000000000
    exponents <- getRandomIntsInRange amountToTest 100000000000000000000000 1000000000000000000000000
    moduluses <- getRandomIntsInRange amountToTest 1 100

    -- recurse
    performanceTestExponentialModulus' bases exponents moduluses 0 0
    where
        performanceTestExponentialModulus' :: [Int] -> [Int] -> [Int] -> Integer -> Integer -> IO()
        performanceTestExponentialModulus' [] _ _ totalComputationTimeOriginal totalComputationTimeBinaryMethod = do
            
            -- The time only represents the computation time (this means excluding output etc)
            printf "Total Computation Time (Original): %.10f sec\n" ((fromIntegral totalComputationTimeOriginal) / 10^12 :: Double)
            printf "Total Computation Time (Binary Method): %.10f sec\n" ((fromIntegral totalComputationTimeBinaryMethod) / 10^12 :: Double)

        performanceTestExponentialModulus' (x:xs) (y:ys) (z:zs) totalComputationTimeOriginal totalComputationTimeBinaryMethod = do
          let x' = toInteger x
              y' = toInteger y
              z' = toInteger z
              
          putStrLn("Test for input: " ++ show x' ++ "^" ++ show y' ++ " modulo " ++ show z')
          
          -- Execute original function + measure the time it took
          startOriginal <- getCPUTime
          let originalResult = exM x' y' z'
          endOriginal <- getCPUTime
          let computingTimeOriginal = endOriginal - startOriginal

          -- Execute new function + measure the time it took to compute
          startBinaryMethod <- getCPUTime
          let binaryMethodResult = exM' x' y' z'
          endBinaryMethod <- getCPUTime
          let computingTimeBinaryMethod = endBinaryMethod - startBinaryMethod

          -- This is a testing course
          if (originalResult == binaryMethodResult)
          then do
              printf "Computation time ExM (Original): %0.10f sec\n" ((fromIntegral computingTimeOriginal) / 10^12 :: Double)
              printf "Computation time ExM (Binary Method): %0.10f sec\n\n" ((fromIntegral computingTimeBinaryMethod) / 10^12 :: Double)

              performanceTestExponentialModulus' xs ys zs (totalComputationTimeOriginal + computingTimeOriginal) (totalComputationTimeBinaryMethod + computingTimeBinaryMethod) 
          else error "ExM outputs are not equal"