import Week3
import Week2
import Techniques

import System.Random
import Data.List


---- 3.

genIntList :: IO [Int]
genIntList = getIntsInRange 10 0 100

getIntInRange :: Int -> Int -> IO Int
getIntInRange x y = getStdRandom(randomR(x,y))

getIntsInRange :: Int -> Int -> Int -> IO [Int]
getIntsInRange 0 _ _ = return []
getIntsInRange n x y = do
	f <- (getIntInRange x y)
	fs <- (getIntsInRange (n-1) x y)
	return (f:fs)

-- 4.
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation a b = (elem (a) (permutations b))

--found an in-built function doing the same - permutations
--permutation :: Eq a => [a] -> [[a]]
--permutation [] = [[]]
--permutation xs = [x:ys | x <- xs, ys <- permutation(delete x xs)]

-- 5.

testPermutations :: IO Bool
testPermutations = do
	randomList <- genIntList -- generate random list
	let permutationsOfRandomList = permutations randomList -- generate its permutation	
	let notARandomListPermutation = permutations (map (*2) randomList)	
	randomIndex <- getStdRandom(randomR(0, length permutationsOfRandomList))
	randomIndex2 <- getStdRandom(randomR(0, length notARandomListPermutation))
	return (isPermutation randomList (permutationsOfRandomList !! randomIndex) 
		&& not (isPermutation randomList (notARandomListPermutation !! randomIndex2)))

-- 6.

cnf :: Form -> Form
cnf (Cnj x) = Cnj (map cnf x)
cnf (Dsj x) = distAux (map cnf x)
cnf x = x

distAux :: [Form] -> Form
distAux [] = error "empty list"
distAux [f] = f
distAux (f:fs) = (dist f (distAux fs))

dist :: Form -> Form -> Form
dist p (Cnj fs) = Cnj (map (\x -> dist p x) fs)
dist (Cnj fs) q = Cnj (map (\x -> dist x q) fs) 
dist p q = Dsj [p, q]

printCnf :: IO Form
printCnf = do
	cnf <- getRandomF
	return cnf
