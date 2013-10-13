module Week6

where

import Data.List
import System.Random

factors_naive :: Integer -> [Integer]
factors_naive n = factors' n 2 where 
  factors' 1 _ = []
  factors' n m 
    | n `mod` m == 0 = m : factors' (n `div` m) m
    | otherwise      =     factors' n (m+1)

factors :: Integer -> [Integer]
factors n = let 
   ps = takeWhile (\m -> m^2 <= n) primes
 in factors' n ps where 
   factors' 1 _  = []
   factors' n [] = [n]
   factors' n (p:ps) 
    | n `mod` p == 0 = p: factors' (n `div` p) (p:ps)
    | otherwise      =    factors' n ps

primes = sieve [2..]
sieve (n:ns) = n : sieve 
   (filter (\ m -> rem m n /= 0) ns)

isPrime :: Integer -> Bool
isPrime n = factors n == [n]

m1  = 2^2-1;    m2  = 2^3-1;     m3  = 2^5-1
m4  = 2^7-1;    m5  = 2^13-1;    m6  = 2^17-1 
m7  = 2^19-1;   m8  = 2^31-1;    m9  = 2^61-1
m10 = 2^89-1;   m11 = 2^107-1;   m12 = 2^127-1
m13 = 2^521-1;  m14 = 2^607-1;   m15 = 2^1279-1
m16 = 2^2203-1; m17 = 2^2281-1;  m18 = 2^3217-1
m19 = 2^4253-1; m20 = 2^4423-1;  m21 = 2^9689-1
m22 = 2^9941-1; m23 = 2^11213-1; m24 = 2^19937-1
m25 = 2^21701-1

addM :: Integer -> Integer -> Integer -> Integer
addM x y = rem (x+y)

multM :: Integer -> Integer -> Integer -> Integer
multM x y = rem (x*y) 

invM :: Integer -> Integer -> Integer
invM x n = let 
   (u,v) = fct_gcd x n
   copr  = x*u + v*n == 1
   i     = if signum u == 1 then u else u + n  
 in 
   if copr then i else error "no inverse"

fct_gcd :: Integer -> Integer -> (Integer,Integer) 
fct_gcd a b = 
  if b == 0 
  then (1,0) 
  else 
     let 
       (q,r) = quotRem a b
       (s,t) = fct_gcd b r 
     in (t, s - q*t)

expM ::  Integer -> Integer -> Integer -> Integer
expM x y = rem (x^y)

prime_test_F :: Integer -> IO Bool
prime_test_F n = do 
   a <- randomRIO (1, n-1) :: IO Integer
   return (exM a (n-1) n == 1)

primeF :: Int -> Integer -> IO Bool
primeF _ 2 = return True
primeF 0 _ = return True
primeF k n = do
   a <- randomRIO (1, n-1) :: IO Integer
   if (exM a (n-1) n /= 1) 
      then return False 
      else primeF (k-1) n

decomp :: Integer -> (Integer,Integer)
decomp n = decomp' (0,n) where
  decomp' = until (odd.snd) (\ (m,n) -> (m+1,div n 2))

primeMR :: Int -> Integer -> IO Bool
primeMR _ 2 = return True
primeMR 0 _ = return True
primeMR k n = let 
   (r,s) = decomp (n-1) 
   f = \ x -> takeWhile (/= 1) 
       (map (\ j -> exM x (2^j*s) n)  [0..r])
  in 
   do 
    a <- randomRIO (1, n-1) :: IO Integer
    if exM a (n-1) n /= 1 
      then return False 
      else 
        if exM a s n /= 1 && last (f a) /= (n-1) 
          then return False
          else primeMR (k-1) n

encodeDH :: Integer -> Integer -> Integer -> Integer
encodeDH p k m = m*k `mod` p

decodeDH :: Integer -> Integer -> Integer 
         -> Integer -> Integer -> Integer
decodeDH p k ga b c = let 
    gab' = exM ga ((p-1)-b) p 
  in 
    rem (c*gab') p

encode :: Integer -> Integer -> Integer -> Integer
encode p k m = let 
   p' = p-1
   e  = head [ x | x <- [k..], gcd x p' == 1 ]
 in 
   exM m e p

decode :: Integer -> Integer -> Integer -> Integer
decode p k m = let 
   p' = p-1
   e  = head [ x | x <- [k..], gcd x p' == 1 ]
   d  = invM e p' 
 in 
   exM m d p

cipher :: Integer -> Integer
cipher = encode secret bound

decipher :: Integer -> Integer
decipher = decode secret bound

rsa_public :: Integer -> Integer -> (Integer,Integer)
rsa_public p q = let 
   n   = p * q
   phi = (p-1)*(q-1)
   e   = head [ x | x <- [3..], gcd x phi == 1 ]
 in 
   (e,p*q)

rsa_private ::  Integer -> Integer 
                -> (Integer,Integer)
rsa_private p q = let 
   n = p * q
   phi = (p-1)*(q-1)
   e = head [ x | x <- [3..], gcd x phi == 1 ]
   d = invM e phi 
  in 
   (d,p*q)

rsa_encode :: (Integer,Integer) -> Integer -> Integer 
rsa_encode (e,n) =  \ m -> exM m e n

rsa_decode = rsa_encode

trapdoor :: (Integer,Integer) -> Integer -> Integer
trapdoor = rsa_encode 

secret = m18
bound  = 131

exM :: Integer -> Integer -> Integer -> Integer
exM _ 0 _ = 1
exM x y n = let 
              z = exM x (y `div` 2) n
              w = multM z z n
            in 
              if even y then w
              else multM x w n 

