import Lab6
import Week6
import TimeIt
import System.Random
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
-- 2 hours

-- we assume the input lists are of the same length


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

loop_expM :: [Int] -> [Int] -> [Int] -> [Int]
loop_expM [] _ _ = []
loop_expM _ [] _ = []
loop_expM _ _ [] = []
loop_expM (b:bs) (e:es) (m:ms) = (fromIntegral(expM (toInteger(b)) (toInteger(e)) (toInteger(m)))) : ((loop_expM bs es ms))

loop_exm_fast :: [Int] -> [Int] -> [Int] -> [Int]
loop_exm_fast [] _ _ = []
loop_exm_fast _ [] _ = []
loop_exm_fast _ _ [] = []
loop_exm_fast (b:bs) (e:es) (m:ms) = (fromIntegral(expM (toInteger(b)) (toInteger(e)) (toInteger(m)))) : ((loop_exm_fast bs es ms))

test_exm :: IO()
test_exm = do 
	let times = 100
	bases <- getRandomIntsInRange times 1 1000000 
	exponents <- getRandomIntsInRange times 1 1000000 
	modulus <- getRandomIntsInRange times 1 1000000 
	timeIt $ putStrLn (show(loop_expM bases exponents modulus))
	timeIt $ putStrLn (show(loop_exm_fast bases exponents modulus))
	

{-
A lot of printing and converting distorting the results. Nevertheless, improvement can be seen.

for times = 5
*Main> test_exm
[636163,110161,474676,29153,39460]
CPU time:   4.56s
[636163,110161,474676,29153,39460]
CPU time:   4.25s
*Main> test_exm
[12549,548519,137441,24966,261682]
CPU time:   5.08s
[12549,548519,137441,24966,261682]
CPU time:   4.97s
*Main> test_exm
[471276,137793,187680,800752,18743]
CPU time:   5.06s
[471276,137793,187680,800752,18743]
CPU time:   4.95s

for times = 100
*Main> test_exm
[492665,856873,239379,236765,924,1396,68008,397275,724513,136412,644128,44331,62
909,100231,293419,314120,486562,215720,720178,31302,207929,98920,85570,552302,93
091,252937,334198,443954,69005,136320,333191,128698,791441,20221,87024,81281,851
8,77509,129906,355284,70439,51036,24169,248914,120245,132343,462109,35284,115074
,272245,664192,179611,297341,57356,216655,763410,117884,215965,342877,109448,458
819,413289,546151,180820,140693,366437,33958,47296,126009,534232,594913,41130,72
353,21119,397715,426139,36111,65019,150005,346928,662393,165344,437299,789603,23
8545,118513,318737,137592,9388,638551,344912,3688,125441,479361,73560,13556,9049
,411352,32990,628801]
CPU time: 106.58s
[492665,856873,239379,236765,924,1396,68008,397275,724513,136412,644128,44331,62
909,100231,293419,314120,486562,215720,720178,31302,207929,98920,85570,552302,93
091,252937,334198,443954,69005,136320,333191,128698,791441,20221,87024,81281,851
8,77509,129906,355284,70439,51036,24169,248914,120245,132343,462109,35284,115074
,272245,664192,179611,297341,57356,216655,763410,117884,215965,342877,109448,458
819,413289,546151,180820,140693,366437,33958,47296,126009,534232,594913,41130,72
353,21119,397715,426139,36111,65019,150005,346928,662393,165344,437299,789603,23
8545,118513,318737,137592,9388,638551,344912,3688,125441,479361,73560,13556,9049
,411352,32990,628801]
CPU time: 102.39s
-}	

-- 6.3
-- 20 minutes
composites :: [Integer]
composites = filter (not . isPrime) [4..]

-- 6.4