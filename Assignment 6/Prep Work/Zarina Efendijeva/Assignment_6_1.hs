module Lab6
where

-- Exercise 1
-- To calculate x^p mod n

{-

exM :: Integer -> Integer -> Integer -> Integer
exM x 0 n = 1
exM x y n = if (mod y 2 == 0 && y >0) 
			then exM (mod (x*x) n) (div y 2) n 
			else mod ( (exM x ( y - 1 ) n) * x ) n 

-}

-- Better version I think, productivity at night is high...
exM :: Integer -> Integer -> Integer -> Integer
exM x y n
    | y <= 0	= 1
    | y == 1	= mod x n
    | otherwise	= if odd y
     then mod ( (exM x ( y - 1 ) n) * x ) n
     else exM ( mod (x*x) n ) ( div y 2 ) n


-- Next example 23^25 mod 30 = 23
test0 = exM 23 25 30 

test1 = exM 24 33 5

test2 = exM 1377 666 5

		 


