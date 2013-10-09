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

getRandomIntegerInRange :: Integer -> Integer -> IO Integer
getRandomIntegerInRange x y = getStdRandom(randomR(x,y))

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

-- Test for modular exponentiation
-- ghc -o weeksix weeksix.hs for preventing interpreter out of memory
main = testModularExponentiation

testModularExponentiation = do
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
  carmichaelTest <- repeatFermatCheck accuracy amount carmichael
  
  putStrLn ("Composite numbers found as false positives: " ++ show dynamicTest)
  -- putStrLn ("Carmichael numbers found as false positives: " ++ show carmichaelTest)
  putStrLn "End of test"

  where
        -- Loop/repeat the fermat test for different accuracies (which is a parameter that gets decreased by 1)
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
        -- Loop for the miller check with decreasing accuracies
        repeatMillerCheck 0 (x:xs) = return []
        repeatMillerCheck a xs = do
          result <- testMillerRabin' a xs
          testTail <- repeatMillerCheck (a-1) xs
          return ((a, result) : testTail)

        -- TODO: refactor to above function
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

-- Generate a co prime of z within the range of x and y
generateCoPrimeInRange :: Integer -> Integer -> Integer -> IO Integer
generateCoPrimeInRange x y z = do
  i <- getRandomIntegerInRange x y
  if ((gcd i z) /= 1)
  then generateCoPrimeInRange x y z
  else return i

-- Stolen from Week6.hs, but changed the parameter sequence (which is more logical to me) plus using the faster/improved exM' function
rsa_encode' :: Integer -> Integer -> Integer -> Integer 
rsa_encode' n e =  \m -> exM' m e n

rsa_decode' :: Integer -> Integer -> Integer -> Integer 
rsa_decode' n d = \c -> exM' c d n

generatePublicKeyPair :: IO ((Integer, Integer), Integer)
generatePublicKeyPair = do
  p <- generateUniqueLargeRandomPrime 0 -- 0 will be ignored
  q <- generateUniqueLargeRandomPrime p
  let n = p * q
      phi = (p - 1) * (q - 1)
  e <- generateCoPrimeInRange 2 (n - 1) phi  
  return ((n, e), phi)

generatePrivateKey :: Integer -> Integer -> Integer
generatePrivateKey e phi = invM e phi

setupRSA :: IO ((Integer, Integer), Integer)
setupRSA = do
  publicPair <- generatePublicKeyPair
  let privateKey = generatePrivateKey (snd (fst publicPair)) (snd publicPair) -- generate a private key based on e and phi
  
  -- Verbose
  putStrLn("Public key modulus:  " ++ (show $ fst (fst publicPair)))
  putStrLn("Public key exponent: " ++ (show $ snd (fst publicPair)))
  putStrLn("Private key exponent:" ++ (show privateKey) ++ "\n\n")
  
  return (fst publicPair, privateKey) -- first from the public pair as we do not need phi anymore

generateUniqueLargeRandomPrime :: Integer -> IO Integer
generateUniqueLargeRandomPrime otherPrime = do
  p <- getRandomIntegerInRange 10000000000000000000000000000000000000000000000 90000000000000000000000000000000000000000000000 -- large enough? :)
  result <- primeMR 10 (fromIntegral p) -- 10 tests is considered acceptable by me
  if ((not) result || p == otherPrime) -- make sure the result is a prime and NOT equal to the other prime provided
  then generateUniqueLargeRandomPrime otherPrime  -- repeat the process for a unique prime
  else return p

testRSA :: IO Bool
testRSA = do
  rsa <- setupRSA -- generates rsa public key pair and a private key
  amountToTest <- getRandomIntInRange 100 1000 -- random amount to test for le fun

  let 
      publicPair = fst rsa -- returns a tuple with (n, e)
      privateKey = snd rsa -- returns d
  
      -- Static test
      msg = 133333333337
      encodeOutput = rsa_encode' (fst publicPair) (snd publicPair) msg
      decodeOutput = rsa_decode' (fst publicPair) privateKey encodeOutput

  -- Dynamic random test
  testResults <- testRSA' amountToTest publicPair privateKey
  return (msg == decodeOutput && testResults)

  -- Tests whether random input which gets encoded is equal to the encoded version being decoded, using the public exponent for encryption and the private exponent for decryption
  where
      testRSA' :: Int -> (Integer, Integer) -> Integer -> IO Bool
      testRSA' 0 _ _ = return True
      testRSA' n publicPair privateKey = do
        msg <- getRandomIntegerInRange 0 ((fst publicPair) - 1)
      
        let encodeOutput = rsa_encode' (fst publicPair) (snd publicPair) msg
            decodeOutput = rsa_decode' (fst publicPair) privateKey encodeOutput
            
        testTail <- testRSA' (n-1) publicPair privateKey

        return (msg == decodeOutput && testTail)