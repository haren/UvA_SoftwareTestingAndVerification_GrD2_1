module Week3_1 where

import Data.List
import System.Random
import Week3


-- Assignment 3
getRandomInt :: IO Int
getRandomInt = getStdRandom(random)

getRandomIntInRange :: Int -> Int -> IO Int
getRandomIntInRange x y = getStdRandom(randomR(x,y))

getRandomInts :: Int -> IO [Int]
getRandomInts 0 = return []
getRandomInts n = do
    f <- getRandomInt
    fs <- getRandomInts (n-1)
    return (f : fs)

getRandomIntsInRange :: Int -> Int -> Int -> IO [Int]
getRandomIntsInRange 0 _ _ = return []
getRandomIntsInRange n x y = do
    f <- getRandomIntInRange x y
    fs <- getRandomIntsInRange (n-1) x y
    return (f : fs)


-- alternative solution

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
    
-- Assignment 4
-- Fastest solution
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = (length xs == length ys) && (null (deleteFirstsBy (==) xs ys))

-- Slow solution
isPermutation' :: Eq a => [a] -> [a] -> Bool
isPermutation' a b = (elem (a) (permutations b))

-- tests
testPermutations = inputPermutations 10000 -- amount of variations to test
inputPermutations :: Int -> IO Bool
inputPermutations 0 = testPermutations' -- combining a recursive and a non-recursive test together
inputPermutations x = do
  i <- getRandomIntsInRange 10 1 10000 -- generate a random list of 10 ints within boundaries
  r <- getRandomInt
  o <- inputPermutations (x-1)
  let s = isPermutation (sort i) i -- check if the sorted random list is a permutation of the unaltered random list
  let s' = (not) (isPermutation (drop 1 i) i) -- drop 1 from the list to compare it to the random list
  let s'' = (not) (isPermutation i (r : i)) -- add a random number to the list to compare to, to make the permutation check fail
  let selfPermutation = isPermutation i i -- should be a permutation of itself
  return (selfPermutation && s && s' && s'' && o) -- combine all permutation checks

-- really slow - generates all permutations
testPermutations' :: IO Bool
testPermutations' = do
	randomList <- genIntList -- generate random list
	let permutationsOfRandomList = permutations randomList -- generate its permutation	
	let notARandomListPermutation = permutations (map (*2) randomList)	
	randomIndex <- getStdRandom(randomR(0, length permutationsOfRandomList))
	randomIndex2 <- getStdRandom(randomR(0, length notARandomListPermutation))
	return (isPermutation randomList (permutationsOfRandomList !! randomIndex) 
		&& not (isPermutation randomList (notARandomListPermutation !! randomIndex2)))


-- Assignment 7

terms = [x, y, z]

getRandomTerm :: IO Term
getRandomTerm = do
		r <- getRandomIntInRange 0 2
		return (terms !! r)

getRandomLowercaseLetter :: IO Char
getRandomLowercaseLetter = do
                    c <- randomRIO('a','z')
                    return c

getRandomUppercaseLetter :: IO Char
getRandomUppercaseLetter = do
                    c <- randomRIO('A','Z')
                    if (c == 'E' || c == 'A') -- These are reserved for printing exists and for all
                    then do x <- getRandomUppercaseLetter
                            return x
                    else return c

getRandomF :: IO Formula
getRandomF = do d <- getRandomIntInRange 0 3
                getRandomForm d

getRandomAtomFormWithBoundLetter :: Int -> Term -> IO Formula
getRandomAtomFormWithBoundLetter n c = do
                                          r <- getRelations (n-1)
                                          f <- getRandomUppercaseLetter
                                          p <- randomRIO(0, (length r) - 1)
                                          -- Randomly place the bound letter
                                          let (xs, ys) = splitAt p r
                                          return (Atom [f] (xs ++ [c] ++ ys))

getRandomAtomUnboundForm :: Int -> IO Formula
getRandomAtomUnboundForm n = do
                                r <- getRelations (n-1)
                                f <- getRandomUppercaseLetter
                                return (Atom [f] r)


getRelations :: Int -> IO [Term]
getRelations 0 = return []
getRelations i = do
                    x <- getRelations (i-1)
                    t <- getRandomTerm
                    return (t : x)

getRandomAtomForm :: Char -> IO Formula
getRandomAtomForm c = do
                        p <- getRandomIntInRange 2 4
                        n <- getRandomIntInRange 1 2
                        -- Generate bound and unbound forms with at least 1 to 3 relations
                        case n of
                          1 -> do f <- getRandomAtomFormWithBoundLetter p (V [c])
                                  return f
                          2 -> do f <- getRandomAtomUnboundForm p
                                  return f

getRandomForm :: Int -> IO Formula
getRandomForm 0 = do c <- getRandomUppercaseLetter
                     f <- getRandomAtomForm c
                     return (f)

getRandomForm d = do n <- getRandomIntInRange 0 5
                     case n of
                       0 -> do f <- getRandomForm 0
                               return f
                       1 -> do f <- getRandomForm (d-1)
                               return (Neg f)
                       2 -> do m <- getRandomIntInRange 2 4
                               fs <- getRandomForms (d-1) m
                               return (Conj fs)
                       3 -> do m  <- getRandomIntInRange 2 4
                               fs <- getRandomForms (d-1) m
                               return (Disj fs)
                       4 -> do c <- getRandomLowercaseLetter
                               f <- getRandomAtomForm c
                               return (Forall [c] f)
                       5 -> do c <- getRandomLowercaseLetter
                               f <- getRandomAtomForm c
                               return (Exists [c] f)

getRandomFs :: Int ->  IO [Formula]
getRandomFs n = do d <- getRandomIntInRange 0 5
                   getRandomForms d n

getRandomForms :: Int -> Int -> IO [Formula]
getRandomForms _ 0 = return []
getRandomForms d n = do
                     f <- getRandomForm d
                     fs <- getRandomForms d (n-1)
                     return (f:fs)
  
