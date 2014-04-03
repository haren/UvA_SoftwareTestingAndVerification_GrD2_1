-- Software verification and testing
-- Lab 2 Assignment 1
-- Group D2_1
-- Cigdem Aytekin 10463135,
-- Jorryt Jan Dijkstra 10462015,
-- Z.E. 10628185, 
-- Lukasz Harezlak, 10630171 

import Data.List
import Data.Array.IO


data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq, Show)
triangle :: Integer -> Integer -> Integer -> Shape
-- VVZ: one way to shorten the code would be to sort a, b, c and write a check that relies on their order
triangle a b c
	| (a <= 0 || b <= 0 || c <= 0) || (a + b <= c || b + c <= a || a + c <= b) = NoTriangle 	-- Checking if sides are negative or equal to 0, if so then it's not a triangle and checking if sum of 2 sides is bigger than the 3rd one, if not then not a triangle
	| a == b && b == c  = Equilateral				-- Checking if 3 sides are equal to satisfy equilaterality 
	| (pythagoras(sort[a,b,c])) &&					-- Checking Pythagorean theorem
	  ((a> b && a>c) || (b>a && b >c) || (c>a && c>b)) &&  ( a/=b || b/=c || a/=c) = Rectangular	-- Searching for hypotenuse & Excluding Isosceles triangle		
	| a == b  || b == c || a == c  = Isosceles		-- Checking if any of 2 sides are equal, if so then it's a isosceles triangle  
	| otherwise = Other  -- For any other kind of triangles
	where
			pythagoras [a,b,c] = c^2==(a^2 + b^2)  -- Checking Pythagorean theorem



 -- Test Scenarios: 
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

-- VVZ: impressive machinery!
