-- Software verification and testing
-- Lab Assignment 1

-- Group D2_1
-- Cigdem Aytekin 10463135,
-- Jorryt Jan Dijkstra 10462015,
-- Zarina Efendijeva TODO, 
-- Lukasz Harezlak, 10630171


module Sol2 where
import GS
import TAMO

-- Exercise 2.4
testExercise2_4 = ((True <+> False) && ((not)(True <+> True)) && ((not)(False <+> False)) && (False <+> True))

-- Exercise 2.13
exercise2_13a = logEquiv1 (\p -> not True) (\p -> False)
exercise2_13a_2 = logEquiv1 (\p -> not False) (\p -> True)
exercise2_13b = logEquiv1 (\p -> p ==> False) (\p -> not p)
exercise2_13c = logEquiv1 (\p -> p || True) (\p -> True)
exercise2_13c_2 = logEquiv1 (\p -> p && False) (\p -> False)
exercise2_13d = logEquiv1 (\p -> p || False) (\p -> p)
exercise2_13d_2 = logEquiv1 (\p -> p && True) (\p -> p)
exercise2_13e = logEquiv1 (\p -> p || (not) p) (\p -> True)
exercise2_13f = logEquiv1 (\p -> p && (not) p) (\p -> False)

-- Exercise 2.15
contradiction1 ::  (Bool -> Bool) -> Bool
contradiction1 f = (f True <=> False) && (f False <=> False)

contradiction2 ::  (Bool -> Bool -> Bool) -> Bool
contradiction2 f = and [f p q <=> False | p <- [True, False]]

contradiction3 ::  (Bool -> Bool -> Bool -> Bool) -> Bool
contradiction3 f = and [f p q r <=> False | p <- [True, False], q <- [True, False], r <- [True, False]]

testContradiction1 = contradiction1 (\  p -> p && not p  )
testContradiction2 = contradiction2 (\ p q -> p && (q && not q))
testContradiction3 = contradiction3 (\ p q r -> ((p && r) || (q && not r)) <=>((not p && r) || (not q && not r )))

-- Exercise 2.18
exercise2_18a = logEquiv2 (\p q -> p <=> q) (\p q -> ((not) p) <=> ((not) q)) 
exercise2_18b = logEquiv2 (\p q -> ((not) p) <=> q) (\p q -> p <=> ((not) q))

-- Exercise 2.20
exercise2_20a = logEquiv2 (\p q -> (not p) ==> q) (\p q -> p ==> (not q))
exercise2_20b = logEquiv2 (\p q -> (not p) ==> q) (\p q -> q ==> (not p))
exercise2_20c = logEquiv2 (\p q -> (not p) ==> q) (\p q -> (not) q ==> p)
exercise2_20d = logEquiv3 (\p q r -> p ==> (q ==> r)) (\p q r -> q ==> (p ==> r))
exercise2_20e = logEquiv3 (\p q r -> p ==> (q ==> r)) (\p q r -> (p ==> q) ==> r)
exercise2_20f = logEquiv2 (\p q -> (p ==> q) ==> p) (\p q -> p)
exercise2_20g = logEquiv3 (\p q r -> p || q ==> r) (\p q r -> (p ==> r) && (q ==> r))

-- Exercise 2.51
unique :: (a -> Bool) -> [a] -> Bool
unique p xs = length (filter p xs) == 1

-- Exercise 2.52
-- An even number is probably an even element number? the exercise is not well defined
parity :: [Bool] -> Bool
parity xs = any (\x -> even (snd x) && (fst x) == True) (zip xs [1..])

-- Exercise 2.53
evenNR :: (a -> Bool) -> [a] -> Bool
evenNR p xs = parity (map p xs)