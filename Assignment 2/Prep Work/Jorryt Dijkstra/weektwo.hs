-- J. Dijkstra
-- Software Testing - Lab Assignment 2
import Data.List
import Data.Array.IO

data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq, Show)
triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z
		| (x <= 0 || y <= 0 || z <= 0) = NoTriangle -- or error "Invalid input on coordinates" would also be fine
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

testEquilateral :: Bool
testEquilateral = inputEquilateral s s s
		where s = [1..1000000]

inputEquilateral :: [Integer] -> [Integer] -> [Integer] -> Bool
inputEquilateral [] _ _ = True
inputEquilateral _ [] _ = True
inputEquilateral _ _ [] = True
inputEquilateral (x:xs) (y:ys) (z:zs) = (triangle x y z == Equilateral) && (inputEquilateral xs ys zs)

testNoTriangle :: Bool
testNoTriangle = inputNoTriangle s s s && (inputNoTriangle [-2,5,7,3,0,20] [1,0,-3,7,1,10] [4,5,6,-1,9,0])
		where s = [-100000..0]
inputNoTriangle [] _ _ = True
inputNoTriangle _ [] _ = True
inputNoTriangle _ _ [] = True
inputNoTriangle (x:xs) (y:ys) (z:zs) = (triangle x y z == NoTriangle) && (inputNoTriangle xs ys zs)

testIscosceles :: Bool
testIscosceles = inputIscosceles x x y && inputIscosceles x y x && inputIscosceles y x x
	where 
		x = [100..100000]
		y = (map (50+) x)

inputIscosceles :: [Integer] -> [Integer] -> [Integer] -> Bool
inputIscosceles [] [] [] = True
inputIscosceles (x:xs) (y:ys) (z:zs) = (triangle x y z == Isosceles) && (inputIscosceles xs ys zs)

testOther :: Bool
testOther = inputOther x y z
	where
		x = [1..100001]
		y = [2..100002]
		z = [3..100003]

inputOther :: [Integer] -> [Integer] -> [Integer] -> Bool
inputOther [] [] [] = True
inputOther (x:xs) (y:ys) (z:zs)
	| (c) = (t == Other) && tail
	| otherwise = tail
	where 
		t = triangle x y z
		c = (t /= NoTriangle && t /= Equilateral && t /= Isosceles && t /= Rectangular)
		tail = inputOther xs ys zs

testTriangle = testEquilateral && testIscosceles && testPythagoras && testNoTriangle && testOther