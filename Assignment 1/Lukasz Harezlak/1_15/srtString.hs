srtString :: [String] -> [String]
srtString [] = [] 
srtString xs = m : (srtString (removeFst m xs)) where m = mnmStr xs

mnmStr :: [String] -> String
mnmStr [] = []
mnmStr [x] = x 
mnmStr(x : xs) = min x (mnmStr xs) 

removeFst :: String -> [String] -> [String]
removeFst n [] = []
removeFst n [x] | n == x = []
		| otherwise = [x]
removeFst n (x:xs) | n == x = xs
		   | otherwise = (x : (removeFst n xs))