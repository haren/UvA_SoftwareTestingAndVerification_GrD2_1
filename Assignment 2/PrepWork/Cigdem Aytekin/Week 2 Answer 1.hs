
module Module21 where
import Week2
import Data.List

data Shape = NoTriangle | Equilateral | Isosceles
 | Rectangular | Other deriving (Eq,Show)


triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c = triangleSorted (sort [a, b, c])

triangleSorted :: [Integer] -> Shape
triangleSorted [a, b, c]
		| (a+b) <= c			= NoTriangle
		| ( (a==b) && (b==c) )	= Equilateral
		| ( a^2 + b^2 == c^2 ) = Rectangular
		| ( a == b)				= Isosceles
		| otherwise				= Other
		


