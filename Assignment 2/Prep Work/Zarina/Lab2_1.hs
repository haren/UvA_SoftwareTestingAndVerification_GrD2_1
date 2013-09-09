 module Lab2 where 

data Shape = NoTriangle | Equilateral | Rectangular | Isosceles | Other deriving (Show)

triangle :: Int -> Int -> Int -> Shape

triangle a b c
	| a <= 0 || b <= 0 || c <= 0 = NoTriangle 	-- Checking if sides are negative or equal to 0, if so then it's not a triangle
	| a == b && b == c  = Equilateral			-- Checking if 3 sides are equal to satisfy equilaterality 
	| ((a> b && a>c) || (b>a && b >c) || (c>a && c>b)) && 								-- Searching for hypotenuse
	  ( a/=b || b/=c || a/=c) && 														-- Excluding Isosceles triangle
	  ((a^2 == b^2 + c^2) || (b^2 == a^2 + c^2) || (c^2 == b^2 + a^2 ))  = Rectangular  -- Checking Pythagorean theorem
	| a == b  || b == c || a == c  = Isosceles	-- Checking if any of 2 sides are equal, if so then it's a isosceles triangle  
	| otherwise = Other  -- For any other kind of triangles

	
	

	



 