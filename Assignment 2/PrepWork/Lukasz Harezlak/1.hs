import Week2
import Data.List

-- time spent on excercise: 15 minutes

-------------------------------------------------------------

data Shape = NoTriangle | Equilateral | Isosceles | Recatngular | Other deriving(Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c = testTriangle(sort [a,b,c])

testTriangle :: [Integer] -> Shape
testTriangle [a,b,c] 	| (a <= 0 || b <= 0 || c <= 0) = NoTriangle
						| ((a+b) <= c) = NoTriangle
						| ((a==b) && (b==c)) = Equilateral
						| (a==b || b==c || a==c) = Isosceles						
						| ((a^2)+(b^2) == (c^2)) = Recatngular
						| otherwise = Other


-------------------------------------------------------------

-------------------------------------------------------------
--test variables
-------------------------------------------------------------

-------------------------------------------------------------
--test results
-------------------------------------------------------------						