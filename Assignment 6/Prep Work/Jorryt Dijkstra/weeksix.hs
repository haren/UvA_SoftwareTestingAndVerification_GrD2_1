module Main

where
import Data.List
import Data.Bits
import Control.Monad
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
  bases <- getRandomIntsInRange amountToTest 10000000 100000000
  exponents <- getRandomIntsInRange amountToTest 10000000 100000000
  moduluses <- getRandomIntsInRange amountToTest 10 100000000
  
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

composites :: [Integer]
composites = [ n | n <- [4..], not $ isPrime n ]

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | k <- [2..], isPrime (6*k+1), isPrime (12*k+1), isPrime (18*k+1) ]

-- The first composite integer to be found prime was 4
testFermat :: IO ()
testFermat = do
  accuracy <- getRandomIntInRange 10 100 -- amount of tests to do for each primality check (determines the accuracy obviously)
  amount <- getRandomIntInRange 1000 10000
  staticTest <- prime_test_F 1
  staticTest2 <- prime_test_F 2
  staticTest3 <- prime_test_F 3
  
  putStrLn "Static tests:"
  putStrLn ("1: " ++ show staticTest)
  putStrLn ("2: " ++ show staticTest2)
  putStrLn ("3: " ++ show staticTest3)
  putStrLn ("Testing the numbers from the first " ++ show amount ++ " composite and the carmichael numbers for primality")
  dynamicTest <- repeatFermatCheck accuracy amount composites
  -- carmichaelTest <- repeatFermatCheck accuracy amount carmichael
  
  putStrLn ("Composite numbers found as false positives: " ++ show dynamicTest)
  -- putStrLn ("Carmichael numbers found as false positives: " ++ show carmichaelTest)
  putStrLn "End of test"

  where
        repeatFermatCheck 0 n (x:xs) = return []
        repeatFermatCheck a n xs = do
          result <- testFermat' a n xs
          testTail <- repeatFermatCheck (a-1) n xs
          return ((a, result) : testTail)

        testFermat' _ 0 _ = return []
        testFermat' a n (x:xs) = do
          result <- primeF a x

          putStrLn(show x ++ ": " ++ show result)

          testTail <- testFermat' a (n-1) xs
          
          if result
          then return (x : testTail)
          else return testTail

-- More accurate for carmichael numbers
testMillerRabin :: IO ()
testMillerRabin = do
  accuracy <- getRandomIntInRange 10 100 -- amount of tests to do for each primality check (determines the accuracy obviously)
  amount <- getRandomIntInRange 10 20
  putStrLn ("Testing MR test with " ++ show amount ++ " carmichael numbers, with initial accuracy " ++ show accuracy ++ " and loosening it over time")
  results <- repeatMillerCheck accuracy (take amount carmichael)
  putStrLn("Results of false positives (format: (accuracy, [prime numbers]):\n " ++ show results)
  where 
        repeatMillerCheck 0 (x:xs) = return []
        repeatMillerCheck a xs = do
          result <- testMillerRabin' a xs
          testTail <- repeatMillerCheck (a-1) xs
          return ((a, result) : testTail)

        testMillerRabin' 0 _ = return []
        testMillerRabin' a xs = do
          results <- mapM (\x -> primeMR a x) xs
          let zippedResults = zip xs results
              outputResults = map (fst) (filter (\(_,y) -> y) zippedResults)
          mapM_ (\(x,y) -> putStrLn $ show x ++ ": " ++ show y) zippedResults

          testTail <- testMillerRabin' (a-1) xs
          return (outputResults ++ testTail)

-- Parameter is the amount of primes to consider
generateMersennePrimes :: Int -> IO [Integer]
generateMersennePrimes primeThreshold = do
  let mersennes = [ ((2^p)-1) | p <- take primeThreshold primes ]
  results <- filterM (\x -> primeMR 1 x) mersennes
  return results
  
generatePublicKeyPair :: IO (Int, Int)
generatePublicKeyPair = do
  p <- generateRandomPrime
  p2 <- generateRandomPrime
  return (p, p2)

generateRandomPrime :: IO Int
generateRandomPrime = do
  p <- getRandomIntInRange 100000000000000 900000000000000
  result <- primeMR 3 (fromIntegral p)
  if ((not) result)
  then generateRandomPrime
  else return p