module Lab6_7
where

import Week6	
import System.Random
import Control.Monad 

-- to generate random prime between x and y
randomInt :: Integer -> Integer -> IO Integer
randomInt x y = getStdRandom(randomR(x,y))

test = randomInt 1000 9000

-- take a large prime p, and use the Miller-Rabin algorithm
-- to check whether 2^p-1 is also prime

mersennePrime :: Int -> IO [Integer]
mersennePrime x = do
  let merNr = [ ((2^p)-1) | p <- take x primes ]
  results <- filterM (\x -> primeMR 1 x) merNr
  return results

test2 = mersennePrime 31


