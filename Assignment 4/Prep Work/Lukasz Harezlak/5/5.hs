import Week5
-- import RandomSudoku

import Data.List


-- 5.1

-- 3 minutes

mergeSrt :: Ord a => [a] -> [a]
mergeSrt [] = []
mergeSrt (x:xs) = merge [x] (mergeSrt xs)

mergeSrtA :: Ord a => [a] -> [a] -> [a]
mergeSrtA = assert2 sortedProp 
            $ assert2 sublistProp merge

-- 5.2

-- 20 minutes

split :: [a] -> ([a],[a])
split xs = let
	n = (length xs) `div` 2
	in
	(take n xs, drop n xs)

mergeSplitSort :: Ord a => [a] -> [a]
mergeSplitSort [] = []
mergeSplitSort (xs) = mergeSrtA (mergeSrt(fst(split xs))) (mergeSrt(snd(split xs)))

mergeSplitSortA :: Ord a => [a] -> [a] -> [a]
mergeSplitSortA = assert2 sortedProp 
					$ assert2 sublistProp merge


-- 5.3

-- 60 minutes
-- the modified code is in week5.

{-
*Main> showGrid example6
+-------+-------+-------+
|       | 3     |       |
|       | 7     | 3     |
| 2     |       |     8 |
+-------+-------+-------+
|     6 |     5 |       |
|   9 1 | 6     |       |
| 3     |   7 1 | 2     |
+-------+-------+-------+
|       |       |   3 1 |
|   8   |   4   |       |
|     2 |       |       |
+-------+-------+-------+

*Main> solveAndShow example6
+-------+-------+-------+
| 4 7 8 | 3 9 2 | 6 1 5 |
| 6 1 9 | 7 5 8 | 3 2 4 |
| 2 3 5 | 4 1 6 | 9 7 8 |
+-------+-------+-------+
| 7 2 6 | 8 3 5 | 1 4 9 |
| 8 9 1 | 6 2 4 | 7 5 3 |
| 3 5 4 | 9 7 1 | 2 8 6 |
+-------+-------+-------+
| 5 6 7 | 2 8 9 | 4 3 1 |
| 9 8 3 | 1 4 7 | 5 6 2 |
| 1 4 2 | 5 6 3 | 8 9 7 |
+-------+-------+-------+
[()]

-}

-- 5.4

-- 10 minutes

-- Since the code was modified in Week5, no modifications were needed. The solution was received out of the box.

-- 5.5

-- equivalent to consistenf function

testSudokuGrid :: Grid -> Bool
testSudokuGrid [] = True
testSudokuGrid (xs) = 
	testSudokuRows xs -- test rows
	&& testSudokuRows (transpose xs) -- test columns
	&& testSudokuSubGrids xs

testSudokuRows :: Grid -> Bool
testSudokuRows [] = True
testSudokuRows (x:xs) = (testSudokuRow x) && (testSudokuRows xs)

testSudokuSubGrids :: Grid -> Bool
testSudokuSubGrids [] = True
testSudokuSubGrids (xs) = not (elem False (map testSudokuBlock (getSubGrids xs)))
-- map returns array of Booleans indicating if blocks are consistent.
-- if at least one is False - function should return false

{-

Subgrid Layout 
1 4 7
2 5 8
3 6 9

Additional subgrids
10 12
11 13
-}	

getSubGrids :: Grid -> Grid
getSubGrids [] = []
getSubGrids (xs) = [
	concat (take 3 (map (take 3) xs)) -- subgrid 1
	, concat (drop 3 . take 6 $ (map (take 3) xs)) -- subgrid 2
	, concat (drop 6 . take 9 $ (map (take 3) xs)) -- subgrid 3
	, concat (take 3 (map (drop 3 .take 6) xs)) -- subgrid 4
	, concat (drop 3 . take 6 $ (map (drop 3 .take 6) xs)) -- subgrid 5
	, concat (drop 6 . take 9 $ (map (drop 3 .take 6) xs)) -- subgrid 6
	, concat (take 3 (map (drop 6 .take 9) xs)) -- subgrid 7
	, concat (drop 3 . take 6 $ (map (drop 6 .take 9) xs)) -- subgrid 8
	, concat (drop 6 . take 9 $ (map (drop 6 .take 9) xs)) -- subgrid 9
	, concat (drop 1 . take 4 $ (map (drop 1 .take 4) xs)) -- subgrid 10
	, concat (drop 5 . take 8 $ (map (drop 1 .take 4) xs)) -- subgrid 11
	, concat (drop 1 . take 4 $ (map (drop 5 .take 8) xs)) -- subgrid 12
	, concat (drop 5 . take 8 $ (map (drop 5 .take 8) xs)) -- subgrid 13
	]

--testSudokuColumns :: Grid -> Bool
--testSudokuColumns [] = True
--testSudokuColumns (xs) = testSudokuColumn(map head xs) 
--	&& testSudokuColumns (map removeHead xs)

--removeHead :: [Int] -> [Int]
--removeHead [] = []
--removeHead (x:xs) = xs

--testSudokuColumn :: [Value] -> Bool
--testSudokuColumn = testSudokuSet

testSudokuRow :: [Value] -> Bool
testSudokuRow = testSudokuSet

testSudokuBlock :: [Value] -> Bool
testSudokuBlock = testSudokuSet

testSudokuSet :: [Value] -> Bool
testSudokuSet [] = True
testSudokuSet (xs) = (length xs) == 9 -- apropriate number of elements
						&& (xs == nub xs) -- elements unique						
						&& null (deleteFirstsBy (==) xs [1..9]) -- all from 1 to 9



