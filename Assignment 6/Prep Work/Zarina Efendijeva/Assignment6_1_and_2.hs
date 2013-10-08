module Lab6
where

import Week6
-- Exercise 1. Estimated time around 4 hours. I am slow.
-- To calculate x^p mod n

{-

exM1 :: Integer -> Integer -> Integer -> Integer
exM1 x 0 n = 1
exM1 x y n = if (mod y 2 == 0 && y >0) 
			then exM1 (mod (x*x) n) (div y 2) n 
			else mod ( (exM1 x ( y - 1 ) n) * x ) n 

-}

-- Better version I think, productivity at night is high...
exMZarina :: Integer -> Integer -> Integer -> Integer
exMZarina x y n
    | y <= 0	= 1
    | y == 1	= mod x n
    | otherwise	= if odd y
     then mod ( (exMZarina x ( y - 1 ) n) * x ) n
     else exMZarina ( mod (x*x) n ) ( div y 2 ) n


-- Exercise 2. Testing
testZ0 = exMZarina 23456789 33 5 -- Relative small numbers, so same
testZ1 = exMZarina 98765432123456789 333333 5 -- Mine version is faster
testZ2 = exMZarina 13779988664411225 666666666 5 -- Still fast as previous 2.
testZ3 = exMZarina 99887766554433 12345678 55

testJ0 = expM 23456789 33 5 
testJ1 = expM 98765432123456789 333333 5
testJ2 = expM 13779988664411225 666666666 5  -- quits..
testJ3 = expM 99887766554433 12345678 55


{-
My function is a rocket function:
testZ0 - 0.00 sec
testZ1 - 0.00 sec
testZ2 - 0.00 sec
testZ3 - 0.00 sec

expM function:
testJ0 - 0.00 sec
testJ1 - 0.56 sec
testJ2 - no idea, terminates my GHCI
testJ3 - 24.28 sec

-}


		 


