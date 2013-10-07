module Lab5
where
import Data.List
import Week5
import RandomSudoku

mergeSrt :: Ord a => [a] -> [a]
mergeSrt [] = []
mergeSrt (x:xs) = merge [x] (mergeSrt (xs))

unsorted :: Ord a => [a] -> [a] -> Bool
unsorted xs ys = (not $ sorted xs) ==> sorted ys

len :: Ord a => [a] -> [a] -> Bool
len xs ys = (length xs) == length ys

subl :: Ord a => [a] -> [a] -> Bool
subl xs ys = null $ deleteFirstsBy (==) xs ys

-- Time spent: 1m
mergeA :: Ord a => [a] -> [a] -> [a]
mergeA = assert2 sortedProp $ assert2 sublistProp merge

mergeSrtA :: Ord a => [a] -> [a]
mergeSrtA = assert1 unsorted $ assert1 subl $ assert1 len mergeSrt

split :: [a] -> ([a],[a])
split xs = let n = (length xs) `div` 2 in (take n xs, drop n xs)

splitMerge :: Ord a => [a] -> [a]
splitMerge xs = let s = (split xs) in (merge (sort (fst s)) (sort (snd s)))

splitMergeA :: Ord a => [a] -> [a]
splitMergeA = assert1 unsorted $ assert1 subl $ assert1 len splitMerge



exampleNrcBroken :: Grid
exampleNrcBroken = [ [0,0,0, 3,0,0,  0,0,0],
                [0,0,0, 7,0,0,  3,0,0],
                [2,0,0, 0,0,0,  0,0,8],
                
                [0,0,6, 0,0,5,  0,0,0],
                [0,9,1, 6,0,0,  0,0,0],
                [3,0,0, 0,7,1,  2,0,0],
                
                [0,0,0, 0,0,0,  0,3,1],
                [0,8,0, 0,4,0,  0,0,0],
                [0,0,2, 0,0,0,  0,0,0]]

validateComplete :: [[Int]] -> Bool
validateComplete = not . validateIncomplete

validateIncomplete :: [[Int]] -> Bool
validateIncomplete = or . map (elem 0)

validateNumberPattern :: [Int] -> Bool
validateNumberPattern (rs) = let rs' = filter (/= 0) rs in length rs == 9 && null (deleteFirstsBy (==) rs' [1..9])

validateMatrix :: [[Int]] -> Bool
validateMatrix xs = validateRow xs && validateRow (transpose xs)

validateRow :: [[Int]] -> Bool
validateRow [] = True
validateRow (m:ms) = validateNumberPattern m && validateRow ms

getNRCSubGrids :: [[Int]] -> [[Int]]
getNRCSubGrids xs = getNRCSubGrids' xs ++ getNRCSubGrids' reversedMatrix
    where
        reversedMatrix = reverse xs
        getNRCSubGrids' xs = [getNRCTopSubGrid xs] ++ [getNRCTopSubGrid (map (reverse) xs)]

getNRCTopSubGrid :: [[Int]] -> [Int]
getNRCTopSubGrid = concat . take 3 . tail . map (take 3) . map (tail)

getSubGrids :: [[Int]] -> [[Int]]
getSubGrids [] = []
getSubGrids xs = (getSubGrids (drop 3 (xs))) ++ getRow xs
    where getRow xs' = getBlock xs' ++ getBlock secondCol ++ getBlock thirdCol
          getBlock xs' = [concat (take 3 $ map (take 3) xs')]
          secondCol = map (drop 3) xs
          thirdCol = map (drop 6) xs

getAllSubGrids :: [[Int]] -> [[Int]]
getAllSubGrids xs = getNRCSubGrids xs ++ getSubGrids xs

validateConsistency :: [[Int]] -> Bool
validateConsistency xs = validateRow (getAllSubGrids xs) && validateMatrix xs


getRandomFilledPosition :: Sudoku -> IO (Int, Int)
getRandomFilledPosition s = do
        let filled = filledPositions s
        r <- getRandomInt ((length filled) - 1)
        return (filled !! r)
    

subSudoku :: [[Int]] -> [[Int]] -> Bool
subSudoku [] _ = True
subSudoku xs'@(x:xs) ys'@(y:ys)
                | (length xs' /= length ys') = False 
                | otherwise = (length intersectionResult == intersectionLength) && length y == length x && null (deleteFirstsBy (==) intersectionResult y) && subSudoku xs ys
                where
                    intersectionGrid = filter (/= 0) x
                    intersectionLength = length intersectionGrid
                    intersectionResult = intersect x y

thd (_, _, x) = x

nrcSolution :: IO [()]
nrcSolution = solveAndShow exampleNrc

testSudoku :: IO Bool
testSudoku = do
    testSudoku' 10
    where
        testSudoku' 0 = return True
        testSudoku' n = do
            r <- genSudoku
            coordinates <- getRandomFilledPosition (fst (fst r))
            let incompleteSudoku = fst (incompleteSudokuNode) -- sud2grid (fst (fst r))
                incompleteSudokuNode = fst r
                incompleteSudokuGrid = sud2grid incompleteSudoku
                solvedSudoku = fst (head (snd r))
                solvedSudokuGrid = sud2grid solvedSudoku
                minimalistic = uniqueSol incompleteSudokuNode
                alteredIncomplete = eraseN incompleteSudokuNode coordinates
                alteredMinimalistic = uniqueSol alteredIncomplete
                
            testTail <- testSudoku' (n-1)
            return (consistent incompleteSudoku && consistent solvedSudoku && null(openPositions solvedSudoku) && (not)(null $ openPositions incompleteSudoku) && subSudoku incompleteSudokuGrid solvedSudokuGrid && minimalistic && (not)alteredMinimalistic && testTail)
            -- (consistent incompleteSudoku && consistent solvedSudoku && validateComplete solvedSudokuGrid && validateIncomplete incompleteSudokuGrid && subSudoku incompleteSudokuGrid solvedSudokuGrid && minimalistic && (not)alteredMinimalistic && testTail)
     