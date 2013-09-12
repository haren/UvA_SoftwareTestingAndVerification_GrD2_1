-- J. Dijkstra
-- Software Testing - Lab Assignment 2
import Data.List
import Week2

-- 1.5h inclusive tests
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

------
-- Time spent: 1h inclusive tests

contradiction :: Form -> Bool
contradiction f = all (\v -> not $ eval v f) (allVals f) -- not satisfiable would also work here

tautology :: Form -> Bool
tautology f = all (\v -> eval v f) (allVals f)

entails :: Form -> Form -> Bool
entails x y = tautology $ Impl x y

equiv :: Form -> Form -> Bool
equiv x y = tautology $ Equiv x y

-- equivalences in a list in the form of tuples (a, b) where a represents formula a of the equality and b formula b 
equivalences1 = [(Cnj[p,p],p), (p,p), (p, Neg $ Neg $ p), ((Impl p q), Dsj[Neg p, q]), (Neg $ (Impl p q), Cnj[p, Neg q]), ((Impl (Neg p) (Neg q)), (Impl q p)), ((Impl p (Neg q)), (Impl q (Neg p))), ((Impl (Neg p) q), (Impl (Neg q) p))] -- Theorem 2.10
equivalences2 = [((Equiv p q), Cnj[Impl p q, Impl q p]), ((Equiv p q), Dsj[Cnj[p, q], Cnj[Neg p, Neg q]]), (Cnj[p, q], Cnj[q, p]), (Dsj[p, q], Dsj[q, p]), ((Neg $ Cnj[p, q]), Dsj[Neg p, Neg q]), ((Neg $ Dsj[p, q]), Cnj[Neg p, Neg q])] -- Theorem 2.10
equivalences3 = [((Cnj[p, (Cnj[q, r])]), Cnj[(Cnj[p, q]), r]), ((Dsj[p, (Dsj[q, r])]), Dsj[(Dsj[p, q]), r]), ((Cnj[p, (Dsj[q, r])]), Dsj[(Cnj[p, q]), Cnj[p, r]]), ((Dsj[p, (Cnj[q, r])]), Cnj[(Dsj[p, q]), Dsj[p, r]])] -- Theorem 2.10

equivalences :: [(Form, Form)]
equivalences = equivalences1 ++ equivalences2 ++ equivalences3

testEquiv = inputEquivalences equivalences
testEntails = (entails p p) && (entails q q) && (entails r r) && (entails (Dsj[p, q, r]) (Dsj[p, q, r])) && (not $ entails p q)

-- Actually tested with the equiv/entails, but this does rely on those methods, should we (because we probably shouldnt know the implementation of those functions at all and thus test it here separately)?
testTautology = (tautology $ Dsj[p, Neg p]) && (tautology $ Impl p p)

inputEquivalences :: [(Form, Form)] -> Bool
inputEquivalences [] = True;
inputEquivalences (x:xs) = (equiv (fst x) (snd x)) && inputEquivalences xs

testContradiction = contradiction (Cnj[Neg p, p]) -- need more testcases please

--------

-- 3h if not more, took a while to decode the slides to a working form (figured I needed an auxillery method)

cnf :: Form -> Form
cnf (Prop l) = Prop l
cnf (Cnj c) = Cnj (map (\x -> cnf x) c)
cnf (Dsj d) = distlist (map cnf d) -- dist over the complete disjunction list

distlist :: [Form] -> Form
distlist [x] = x
distlist (x:xs) = dist x (distlist xs)

dist :: Form -> Form -> Form
dist (Cnj x) y = Cnj (map (\z -> dist z y) x)
dist x (Cnj y) = Cnj (map (\z -> dist x z) y)
dist x y = Dsj[x, y]

cnftests :: [(Form, Form)] -- [(input of cnf, output of cnf)]
cnftests = [(Dsj[Cnj[p,q],r], Cnj[Dsj[p,r],Dsj[q,r]]),  (Dsj[p, Cnj[q, r]], Cnj[Dsj[p, q], Dsj[p, r]])] -- Taken from the example in the slides :)
testcnf = inputcnf cnftests

inputcnf :: [(Form, Form)] -> Bool
inputcnf (x:xs) = ((cnf (nnf (arrowfree (fst x)))) == (snd x)) && inputcnf xs
inputcnf [] = True

