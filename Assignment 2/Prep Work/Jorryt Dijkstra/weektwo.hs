-- J. Dijkstra
-- Software Testing - Lab Assignment 2

data Shape = NoTriangle | Equilateral | Isosceles  | Rectangular | Other deriving (Eq, Show)
triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z
		| (x <= 0 || y <= 0 || z <= 0) = error "Invalid input on coordinates"
		| exceedLength x y z == False = NoTriangle
		| otherwise = Other

exceedLength :: Integer -> Integer -> Integer -> Bool
exceedLength x y z = (x * y > z) || (y * z > x) || (z * x > y)
