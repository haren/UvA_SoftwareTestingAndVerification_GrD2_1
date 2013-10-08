module Lab6_3
where

import Week6

-- Exercise 3
-- According to Fermat's Little Theorem: a^(p-1) = 1 (mod p), p is prime, 0 < a < p
-- Function that generates the infinite list of composite natural numbers.

-- A composite number n is a positive integer n>1 which is not prime 
composites :: [Integer]
composites = [ n | n <- [4..], not $ isPrime n ]

