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

thd (_, _, x) = x

testSudoku :: IO Bool
testSudoku = do
    testSudoku' 5
    where
        testSudoku' 0 = return True
        testSudoku' n = do
            r <- genSudoku
            let incompleteSudoku = sud2grid (fst (fst r))
                solvedSudoku = snd r
            testTail <- testSudoku' (n-1)
            return (validateConsistency incompleteSudoku && testTail)
            --  && validateConsistency solvedSudoku && validateIncomplete incompleteSudoku && validateComplete solvedSudoku && validateNumberPattern solvedSudoku && testTail
     