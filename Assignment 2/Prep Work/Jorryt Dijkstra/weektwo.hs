-- J. Dijkstra
-- Software Testing - Lab Assignment 2
import Data.List

data Shape = NoTriangle | Equilateral | Isosceles  | Rectangular | Other deriving (Eq, Show)
triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z
		| (x <= 0 || y <= 0 || z <= 0) = error "Invalid input on coordinates"
		| not (triangleDefinition x y z) = NoTriangle
		| (x == y && y == z) = Equilateral
		| (x == y || y == z || x == z) = Isosceles
		| (pythagoras(sort[x,y,z])) = Rectangular
		| otherwise = Other
		where
			pythagoras [x,y,z] = (x^2 + y^2) == z^2
			triangleDefinition x y z = (x + y) > z && (y + z) > x && (z + x) > y


testPythagoras :: Bool
testPythagoras = inputPythagoras [1..1000000] [1..1000000]

inputPythagoras :: [Integer] -> [Integer] -> Bool
inputPythagoras [] _ = True
inputPythagoras _ [] = True
inputPythagoras (a:as) (b:bs)
		| (isInteger c) = ((triangle a b (ceiling c)) == Rectangular && pythagorasTail)
		| otherwise = pythagorasTail
		where 
			c = sqrt((fromIntegral) (a^2 + b^2))
			pythagorasTail = (inputPythagoras as bs)
			isInteger n = (floor n == ceiling n)
