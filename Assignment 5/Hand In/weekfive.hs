-- Software verification and testing
-- Assignment for Week 5, Date: 6-10-2013
-- Group D2_1
-- Cigdem Aytekin 10463135,
-- Jorryt Jan Dijkstra 10462015,
-- Zarina Efendijeva 10628185,
-- Lukasz Harezlak, 10630171

module Lab5
where
import Data.List
import Week5
import RandomSudoku

mergeSrt :: Ord a => [a] -> [a]
mergeSrt [] = []
mergeSrt (x:xs) = merge [x] (mergeSrt (xs))

unsortedToSorted :: Ord a => [a] -> [a] -> Bool
unsortedToSorted xs ys = (not $ sorted xs) ==> sorted ys

equalLength :: Ord a => [a] -> [a] -> Bool
equalLength xs ys = length xs == length ys

sublist' :: Ord a => [a] -> [a] -> Bool
sublist' xs ys = null $ deleteFirstsBy (==) xs ys

-- Assignment 1: Time spent: 30m
mergeSrtAssertive :: Ord a => [a] -> [a]
mergeSrtAssertive = assert1 unsortedToSorted $ assert1 sublist' $ assert1 equalLength mergeSrt

split :: [a] -> ([a],[a])
split xs = let n = (length xs) `div` 2 in (take n xs, drop n xs)

-- Assignment 2: Time spent: 30m
splitMerge :: Ord a => [a] -> [a]
splitMerge xs = let s = split xs in merge (sort (fst s)) (sort (snd s))

splitMergeAssertive :: Ord a => [a] -> [a]
splitMergeAssertive = assert1 unsortedToSorted $ assert1 sublist' $ assert1 equalLength splitMerge

-- Assignment 3: See Week5.hs for the changed specification
-- Specification: For each NRC block the same uniqueness constraint (as the blocks) also holds.
--                Namely in each NRC subgrid, there has to be an injection from the subgrids to the natural numbers 1-9 (except for the cells with 0 values, as the 0 values are chosen to be a syntactic placeholder for an empty sudoku cell)
--                Due to generating incomplete sudoku's (ones that are not solved yet), there is no surjection as some values have to be filled in during the manual solving process.
--                Surjection is mandatory when the Sudoku is correctly solved.
-- Time spent: 2h
nrcSolution :: IO [()]
nrcSolution = solveAndShow exampleNrc

-- Assignment 4: See RandomSudoku.hs, Time spent: 30m due to getting to understand the random generation code and converting it to tuples rather than writing the Sudoku to stdout 
    
-- Helper function for testing sudoku's
-- Determines whether there is complete intersection (aka being a subsudoku) with another sudoku, useful for comparing a solvable sudoku to a solved one
subSudoku :: [[Int]] -> [[Int]] -> Bool
subSudoku [] _ = True
subSudoku xs'@(x:xs) ys'@(y:ys)
                | (length xs' /= length ys') = False 
                | otherwise = (length intersectionResult == intersectionLength) && length y == length x && null (deleteFirstsBy (==) intersectionResult y) && subSudoku xs ys
                where
                    intersectionGrid = filter (/= 0) x
                    intersectionLength = length intersectionGrid
                    intersectionResult = intersect x y

-- Helper function for testing sudoku's
getRandomFilledPosition :: Sudoku -> IO (Int, Int)
getRandomFilledPosition s = do
        let filled = filledPositions s
        r <- getRandomInt ((length filled) - 1)
        return (filled !! r)

-- Assignment 5: see the test report for more information
testSudoku :: IO Bool
testSudoku = do
    testSudoku' 10 -- Amount to test, feel free to increase this amount of tests in case you have the time :)
    where
        testSudoku' 0 = return True
        testSudoku' n = do
            -- Generate a NRC sudoku including its solution (results in a tuple of the form: (incomplete sudoku, solved sudoku))
            r <- genSudoku
            
            -- Get random coordinates to remove later to test for minimalism
            coordinates <- getRandomFilledPosition (fst (fst r))
            let incompleteSudoku = fst (incompleteSudokuNode)
                incompleteSudokuNode = fst r
                incompleteSudokuGrid = sud2grid incompleteSudoku
                solvedSudoku = fst (head (snd r))
                solvedSudokuGrid = sud2grid solvedSudoku
                minimalistic = uniqueSol incompleteSudokuNode
                alteredIncomplete = eraseN incompleteSudokuNode coordinates
                alteredMinimalistic = uniqueSol alteredIncomplete
                
            testTail <- testSudoku' (n-1)
            return (consistent incompleteSudoku && consistent solvedSudoku && null(openPositions solvedSudoku) && (not)(null $ openPositions incompleteSudoku) 
                && subSudoku incompleteSudokuGrid solvedSudokuGrid && minimalistic && (not)alteredMinimalistic && testTail)