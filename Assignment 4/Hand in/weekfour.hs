module Week4 where

import SetOrd
import System.Random
import Data.List

-- Software verification and testing
-- Assignment for Week 4, Date: 29-9-2013
-- Answers to the questions 2, 3, 4 and 5 
-- Group D2_1
-- Cigdem Aytekin 10463135,
-- Jorryt Jan Dijkstra 10462015,
-- Zarina Efendijeva 10628185,
-- Lukasz Harezlak, 10630171

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



-- Question 2: Implement a random data generator for the datatype Set Int
-- Generate a random list (with unique elements) of a fixed length and convert it to a set. Boundaries etc are checked.
generateRandomSet :: Int -> Int -> Int -> IO (Set Int)
generateRandomSet n x y
    | (x > y) = error "the upper boundary should be higher than the lower boundary"
    | (y < x) = error "the lower boundary should be lower than the upper boundary"
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




-- Question 3: Implement operations for set intersection, set union and set difference,
-- for the datatype Set. Next, use automated random testing to check that your
-- implementation is correct.

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
                y' = list2set [50..150]
                u' = unionSet x' y'
                validateOverlap = getSetLength u' == 150
                validateSubsets = subSet x' u' && subSet y' u'
            
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
                setA = list2set [setALowerBoundary..setAUpperBoundary]
            
            amountToOverlap <- getRandomIntInRange 1 99
            
            let setBLowerBoundary = setAUpperBoundary - amountToOverlap
            setBUpperBoundaryAddition <- getRandomIntInRange 100 10000
            let setBUpperBoundary = setBLowerBoundary + setBUpperBoundaryAddition
                setB = list2set [setBLowerBoundary..setBUpperBoundary]
                difference = differenceSet setA setB
            -- The length of the difference set should be equal to that of setA excluding the overlapping with setB, the minus one is added due to exclusion of the upper boundary            
                correctDifferenceOfAB = ((getSetLength difference == (getSetLength setA) - amountToOverlap - 1) && (subSet difference setA))
            
            -- Check for difference without overlapping elements
            setCUpperBoundary <- getRandomIntInRange 1 10000
            let setC = list2set [1..setCUpperBoundary]
                setDLowerBoundary = (setCUpperBoundary+1)
            setDUpperBoundaryAddition <- getRandomIntInRange 1 10000
            let setD = list2set [setDLowerBoundary..(setDLowerBoundary + setDUpperBoundaryAddition)]
                differenceSetCD = differenceSet setC setD
                correctDifferenceOfCD = (getSetLength differenceSetCD == getSetLength setC) && (subSet differenceSetCD setC)

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
                setA = list2set [setALowerBoundary..setAUpperBoundary]
            
            amountToOverlap <- getRandomIntInRange 10 99
            
            let setBLowerBoundary = setAUpperBoundary - amountToOverlap
            setBUpperBoundaryAddition <- getRandomIntInRange 100 10000
            let setBUpperBoundary = setBLowerBoundary + setBUpperBoundaryAddition
                setB = list2set [setBLowerBoundary..setBUpperBoundary]
                intersection = intersectionSet setA setB

            -- Check for length to be the same and check for the intersection to be a subset of setA as well as B
            let correctIntersection = ((getSetLength intersection) == amountToOverlap + 1) && ((subSet intersection setB) && (subSet intersection setA)) -- +1 due to including the boundary itself
                
                
            -- Check for no overlap between setA and setC
            let setCLowerBoundary = setAUpperBoundary + amountToOverlap + 1000 -- This wont overlap :)
                setCUpperBoundary = setCLowerBoundary + setAUpperBoundaryAddition -- Use a random addition
                setC = list2set [setCLowerBoundary..setCUpperBoundary]
                intersection2 = intersectionSet setA setC

            correctTail <- testIntersection' (n-1)
            return (correctIntersection && correctTail && (intersection2 == emptySet))

testSetMethods :: IO Bool
testSetMethods = do
    intersectionResult <- testIntersection
    differenceResult <- testDifference
    unionResult <- testUnion
    return (intersectionResult && differenceResult && unionResult)



-- Question 4: Implement a function that gives the transitive closure of a 
-- relation, where the relation is represented as a list of pairs.

type Rel a = [(a,a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- Any relations less than 3 is per definition transitive
isTransitive :: Eq a => Rel a -> Bool
isTransitive xs
    | (null xs || length xs < 3) = True -- per definition True (not necessary due to the list comprehension returning an empty list, but for readability this is added)
    | otherwise = null ([x | (x,y) <- xs, (w,z) <- xs, y == w && not (elem (x,z) xs)]) -- create a list of elements that are not living up the transitive standard

trClos :: Ord a => Rel a -> Rel a
trClos [] = []
trClos (xs) = trClos' xs xs
              where
                trClos' xs ys
                    | (isTransitive ys) = sort $ nub ys
                    | otherwise = trClos' xs (ys ++ (xs @@ ys))


-- Question 5: Test the function trClos from the previous exercise.

testTransitiveClosure :: IO Bool
testTransitiveClosure = do
    let staticTest = (trClos [(1,2),(2,3),(3,4)] == [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)])
    
    dynamicTest <- testTransitiveClosure' 1000
    return (dynamicTest && staticTest)
        where
            testTransitiveClosure' :: Int -> IO Bool
            testTransitiveClosure' 0 = return True
            testTransitiveClosure' amount = do
                -- Create an easy closure that does not require relation composition
                n <- getRandomIntInRange 1 100
                let noStepX = n
                    noStepY = n+1
                    noStepZ = n+2
                    noStepClosure = sort [(noStepX,noStepY),(noStepY,noStepZ),(noStepX,noStepZ)]

                -- These should be transitive closures by definition, due to the length not being met
                let closureByDefinitions = ([(noStepX,noStepZ),(noStepX, noStepY)], [(noStepX, noStepZ)])
                    definitionCheck = (trClos (fst closureByDefinitions) == (sort $ fst closureByDefinitions)) && (trClos (snd closureByDefinitions) == (sort $ snd closureByDefinitions))
                    transitiveClosure = sort (trClos noStepClosure)
                    
                -- Any set of random relations should have a transitive closure which should intersect with the original relations
                relations <- generateRandomRelations 100
                let transitiveClosureOfRelations = trClos relations
                    intersectionOfRelationsWithTransitiveClosure = (intersect relations transitiveClosureOfRelations) == relations
                
                -- Recursion
                correctTail <- testTransitiveClosure' (amount-1)


                return (transitiveClosure == noStepClosure && definitionCheck && intersectionOfRelationsWithTransitiveClosure && correctTail)

generateRandomRelations :: Int -> IO [(Int,Int)]
generateRandomRelations 0 = do return []
generateRandomRelations n = do
                    -- Generate random integers with random intervals
                x <- getRandomIntInRange 1 1000000
                y <- getRandomIntInRange 1 1000000
                let randomStepX = x
                    randomStepY = x + y
                
                randomTail <- generateRandomRelations (n-1)
                    
                return ((randomStepX, randomStepY) : randomTail)
            
