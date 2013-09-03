removeFst :: [Int] -> [Int]
removeFst [] = error "empty list" 
removeFst (x:xs) = xs