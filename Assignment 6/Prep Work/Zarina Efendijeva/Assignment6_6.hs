module Lab6_6
where

import Week6
import System.Random
import Data.List


 -- Nt working the way it should, missing something
mrTest :: [Integer] -> IO [Integer]
mrTest [] = return []
mrTest (x:xs) = do
                y <- mrTest [x]
                ys <- mrTest xs
                return ( y ++ ys)

