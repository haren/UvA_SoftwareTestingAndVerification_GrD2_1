substring :: String -> String -> Bool
substring [] ys = True
substring (x:xs) [] = False
substring xs (y:ys') | (prefix xs (y:ys')) == True = True
		| (substring xs ys' == True) = True
		| otherwise = False

prefix :: String -> String -> Bool
prefix [] ys = True 
prefix (x:xs) [] = False 
prefix (x:xs) (y:ys) = (x==y) && prefix xs ys 