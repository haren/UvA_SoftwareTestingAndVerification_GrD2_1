removeFst :: Int -> [Int] -> [Int]
removeFst n [] = []
removeFst n [x] | n == x = []
		| otherwise = [x]
removeFst n (x:xs) | n == x = xs
		   | otherwise = (x : (removeFst n xs))