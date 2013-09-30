module Lab5
where
import Data.List
import Week5

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

