import Lab6
import Week6
import TimeIt
-- 6.1
-- 1 hour

{- 

After wikipedia:

Like the first method, this requires O(e) multiplications to complete. 
However, since the numbers used in these calculations are much smaller than 
the numbers used in the first algorithm's calculations, the computation time 
decreases by a factor of at least O(e) in this method.

-}

exM_fast :: Integer -> Integer -> Integer -> Integer
exM_fast b e m = exM_fast' 1 0 b e m where 
   exM_fast' c e' b e m
   	| e' >= e 		 = c
    | otherwise      = exM_fast' (mod (b*c) m) (e'+1) b e m

-- 6.2