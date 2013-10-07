module TestSudoku where 


import Data.List
import Week5Nrc
import System.Random
import RandomNrcSudoku

-- Cigdem Aytekin, std. nr.: 104 63 135
-- Test your programs from the previous two exercises, and document the 
-- test process. One important property to test is whether the
-- generated sudoku problems are minimal. How can you test this?
-- Deliverables: testing code, test report, indication of time spent.


-- maxTotal is the sum of integers between and including 1 and 9
maxTotal :: Int
maxTotal = 45

-- uniqueValues returns True if a list does not contain
-- duplicate values
uniqueValues :: Eq a => [a] -> Bool
uniqueValues xs = nub xs == xs


-- getRowVals returns the values in r'th row of a 
-- given Sudoku in a list                   
getRowVals :: Sudoku -> Row -> [Value]
getRowVals s r = [ s (r,i) | i <- positions  ]                   

-- getColVals returns the values in c'th column of a 
-- given Sudoku in a list                   
getColumnVals :: Sudoku -> Column -> [Value]
getColumnVals s c = [s (i, c) | i <- positions]
                    
-- getSubGridVals returns the values in a subGrid, the 
-- subGrid is the grid which the element (Row, Column) belongs to                   
getSubGridVals :: Sudoku -> (Row, Column) -> [Value]
getSubGridVals s (r, c) =  
        [ s (r', c') | r' <- bl r, c' <- bl c]
        
-- getSubGridNrcVals returns the values in a subGridNrc, the
-- subGridNrc is the Nrc subgrid which the element (Row, Column)
-- belongs to       
getSubGridNrcVals :: Sudoku -> (Row, Column) -> [Value]
getSubGridNrcVals s (r, c) = 
        [ s (r', c') | r' <- nrcBl r, c' <- nrcBl c]                    


-- allValuesUnique takes a Sudoku as argument and checks values in 
-- every individual row, column, sub grid, and nrc sub grid 
-- for uniqueness 
allValuesUnique :: Sudoku -> Bool
allValuesUnique s = uniqueValues [getRowVals s r | r <- positions] &&
                    uniqueValues [getColumnVals s c | c <- positions] &&
                    uniqueValues [getSubGridVals s (r,c) | r <- [1,4,7], c <- [1,4,7] ] &&
                    uniqueValues [getSubGridNrcVals s (r,c) | r <- [2, 6], c <- [2,6] ]

        
        
makeManyTests :: Int -> IO [Bool]
makeManyTests 0 = do
                    return []
makeManyTests n = do
                    f <- generateAndTest
                    fs <- makeManyTests (n-1)
                    return (f:fs)                    

                    
generateAndTest :: IO Bool
generateAndTest = do  
                        [r] <- rsolveNs [emptyN]
                        s <- genProblem r
                        let auxSudoku = fst s 
                        let myBool = allValuesUnique auxSudoku
                        return myBool





                     
                    