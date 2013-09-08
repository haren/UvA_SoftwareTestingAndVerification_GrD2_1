count :: Char -> String -> Int
count c [x] | c == x = 1
	    | otherwise = 0
count c (x:xs)  | c == x = 1 + count c xs
		| otherwise = count c xs