module Week4 where

import SetOrd
import System.Random
import Data.List

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

-- Generate a random list (with unique elements) of a fixed length and convert it to a set. Boundaries etc are checked.
generateRandomSet :: Int -> Int -> Int -> IO (Set Int)
generateRandomSet n x y
    | (x > y) = error "the upper boundary should be higher than the lower boundary"
    | (y > x) = error "the lower boundary should be lower than the upper boundary"
    | (x == y) = error "no boundary given"
    | (y + x < n) = error "boundary is smaller than the amount of elements to generate for the set"
    | otherwise = do
            retval <- generateRandomSet' [] n x y
            return (list2set retval)

-- Used to generate a set of a fixed length that contains unique values
generateRandomSet' :: [Int] -> Int -> Int -> Int -> IO [Int]
generateRandomSet' _ 0 _ _ = return []
generateRandomSet' list n x y = do    
    randomNumber <- getNewRandomIntInRange list x y
    let list' = (randomNumber : list)
    randomTail <- generateRandomSet' list' (n-1) x y
    return (list' ++ randomTail)
    where
        -- Retrieve a Int that is certainly not in the given list, but within range
        getNewRandomIntInRange :: [Int] -> Int -> Int -> IO Int
        getNewRandomIntInRange list x y = do
            r <- getRandomIntInRange x y
            if (elem r list)
            then getNewRandomIntInRange list x y
            else return r

getSetLength (Set(xs)) = length xs

intersectionSet :: (Ord a) => Set a -> Set a -> Set a 
intersectionSet (Set(xs)) (Set(ys)) = list2set $ (filter (\x -> elem x ys) xs)

differenceSet :: (Ord a) => Set a -> Set a -> Set a
differenceSet (Set(xs)) (Set(ys)) = list2set $ (deleteFirstsBy (==) xs ys)

testUnion :: IO Bool
testUnion = do
    result <- (testUnion' 100)
    return (result && testUnionStatic) -- Run the same test an X amount of times
    where
        testUnion' :: Int -> IO Bool
        testUnion' 0 = return True
        testUnion' n = do
            -- Generate a random amount of x's and y's in different ranges
            lengthx <- getRandomIntInRange 10 100
            lengthy <- getRandomIntInRange 10 100
            x <- generateRandomSet lengthx 0 1000
            y <- generateRandomSet lengthy 1001 2000 -- make sure that all numbers generated are unique by using different boundaries
            let u = unionSet x y
            
            -- Test to make sure numbers overlap in the union implementation
            let x' = list2set [1..100]
            let y' = list2set [50..150]
            let u' = unionSet x' y'
            let validateOverlap = getSetLength u' == 150
            let validateSubsets = subSet x' u' && subSet y' u'
            
            recursiveResult <- testUnion' (n - 1)
            
            -- Check the length of the union (should be the same length as both sets together) and validate if each element of both united sets (inputs) are elements of the union (output)
            -- Also check if the overlap implementation holds and combine these conditions
            return (getSetLength u == (lengthx + lengthy) && subSet x u && subSet y u && validateOverlap && validateSubsets && recursiveResult)
        testUnionStatic :: Bool
        testUnionStatic =
            (unionOfEmptyCheck && unifiedNumbersCheck)
            where unionOfEmptyCheck = (getSetLength (unionSet emptySet numbersSet) == 100) && (getSetLength (unionSet numbersSet emptySet) == 100)
                  numbersSet = list2set [1..100]
                  numbersSet' = list2set [101..200]
                  unifiedNumbersSet = unionSet numbersSet numbersSet'
                  unifiedNumbersCheck = subSet numbersSet unifiedNumbersSet && subSet numbersSet' unifiedNumbersSet && ((getSetLength unifiedNumbersSet) == 200)

testDifference :: IO Bool
testDifference = do
    result <- testDifference' 100  -- Run the same test an X amount of times
    -- Also do static testing in combination with the empty set
    return (result && (differenceSet emptySet numberSet) == emptySet && (differenceSet numberSet emptySet == numberSet))
    where
        testDifference' :: Int -> IO Bool
        testDifference' 0 = return True
        testDifference' n = do
            -- Generate a random amount of x's and y's in different ranges
            setALowerBoundary <- getRandomIntInRange 1 10000
            setAUpperBoundaryAddition <- getRandomIntInRange 100 10000
            let setAUpperBoundary = setALowerBoundary + setAUpperBoundaryAddition
            let setA = list2set [setALowerBoundary..setAUpperBoundary]
            
            amountToOverlap <- getRandomIntInRange 1 99
            
            let setBLowerBoundary = setAUpperBoundary - amountToOverlap
            setBUpperBoundaryAddition <- getRandomIntInRange 100 10000
            let setBUpperBoundary = setBLowerBoundary + setBUpperBoundaryAddition
            let setB = list2set [setBLowerBoundary..setBUpperBoundary]
            let difference = differenceSet setA setB
            -- The length of the difference set should be equal to that of setA excluding the overlapping with setB, the minus one is added due to exclusion of the upper boundary            
            let correctDifferenceOfAB = ((getSetLength difference == (getSetLength setA) - amountToOverlap - 1) && (subSet difference setA))
            
            -- Check for difference without overlapping elements
            setCUpperBoundary <- getRandomIntInRange 1 10000
            let setC = list2set [1..setCUpperBoundary]
            let setDLowerBoundary = (setCUpperBoundary+1)
            setDUpperBoundaryAddition <- getRandomIntInRange 1 10000
            let setD = list2set [setDLowerBoundary..(setDLowerBoundary + setDUpperBoundaryAddition)]
            let differenceSetCD = differenceSet setC setD
            let correctDifferenceOfCD = (getSetLength differenceSetCD == getSetLength setC) && (subSet differenceSetCD setC)

            correctTail <- testDifference' (n-1)
            return (correctDifferenceOfAB && correctDifferenceOfCD && correctTail)
        numberSet = list2set [1..100]

testIntersection :: IO Bool
testIntersection = do
    result <- testIntersection' 100 -- Run the same test an X amount of times
    -- Also test intersection with an empty set
    return ((intersectionSet (list2set [1..100]) emptySet == emptySet) && (intersectionSet emptySet (list2set [1..100]) == emptySet) && result)
    where
        testIntersection' :: Int -> IO Bool
        testIntersection' 0 = return True
        testIntersection' n = do
            -- Generate a random amount of x's and y's in different ranges
            setALowerBoundary <- getRandomIntInRange 1 10000
            setAUpperBoundaryAddition <- getRandomIntInRange 100 10000
            let setAUpperBoundary = setALowerBoundary + setAUpperBoundaryAddition
            let setA = list2set [setALowerBoundary..setAUpperBoundary]
            
            amountToOverlap <- getRandomIntInRange 10 99
            
            let setBLowerBoundary = setAUpperBoundary - amountToOverlap
            setBUpperBoundaryAddition <- getRandomIntInRange 100 10000
            let setBUpperBoundary = setBLowerBoundary + setBUpperBoundaryAddition
            let setB = list2set [setBLowerBoundary..setBUpperBoundary]
            let intersection = intersectionSet setA setB

            let correctIntersection = ((getSetLength intersection) == amountToOverlap + 1) -- +1 due to including the boundary itself
            -- putStrLn("Intersection between A and B: " ++ show (getSetLength intersection) ++ " amount to overlap: " ++ show amountToOverlap)
            
            correctTail <- testIntersection' (n-1)
            return (correctIntersection && correctTail)

testSetMethods :: IO Bool
testSetMethods = do
    intersectionResult <- testIntersection
    differenceResult <- testDifference
    unionResult <- testUnion
    return (intersectionResult && differenceResult && unionResult)

-- Assignment 4
type Rel a = [(a,a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- isTransitive :: Rel a -> Bool
--isTransitive (xs) = 

{-- trClos :: Ord a => Rel a -> Rel a
trClos (xs) = trClos' xs (xs !! 1)
    where trClos' (x:xs) (y:ys) = (x @@ y) : trClos' xs ys
          trClos' [] _ = []
          trClos' _ [] = [] --}
