import TAMO

-- Exercise 2.11
{--
formula6 p = p <=> not (not p)
formula7 p = p && p <=> p
formula8 p = p || p <=> p
formula9 p q = (p ==> q) <=> not p || q
formula10 p q = not (p =	=> q) <=> p && not q
--}

-- Exercise 2.13
test2_12a = logEquiv1 (\p -> not True) (\p -> False)
test2_12a_2 = logEquiv1 (\p -> not False) (\p -> True)
test2_12b = logEquiv1 (\p -> p ==> False) (\p -> not p)
test2_12c = logEquiv1 (\p -> p || True) (\p -> True)
test2_12c_2 = logEquiv1 (\p -> p && False) (\p -> False)
test2_12d = logEquiv1 (\p -> p || False) (\p -> p)
test2_12d_2 = logEquiv1 (\p -> p && True) (\p -> p)
test2_12e = logEquiv1 (\p -> p || (not) p) (\p -> True)
test2_12f = logEquiv1 (\p -> p && (not) p) (\p -> False)

-- Exercise 2.15
contradiction1 ::  (Bool -> Bool) -> Bool
contradiction1 f = (f True <=> False) && (f False <=> False)

contradiction2 ::  (Bool -> Bool -> Bool) -> Bool
contradiction2 f = and [f p q <=> False | p <- [True, False]]

contradiction3 ::  (Bool -> Bool -> Bool -> Bool) -> Bool
contradiction3 f = and [f p q r <=> False | p <- [True, False], q <- [True, False], r <- [True, False]]


-- Thanks Zarina :P
testContradiction1 = contradiction1 (\  p -> p && not p  )
testContradiction2 = contradiction2 (\ p q -> p && (q && not q))
testContradiction3 = contradiction3 (\ p q r -> ((p && r) || (q && not r)) <=>((not p && r) || (not q && not r )))


exercise2_18a = logEquiv2 (\p q -> p <=> q) (\p q -> ((not) p) <=> ((not) q)) 
exercise2_18b = logEquiv2 (\p q -> ((not) p) <=> q) (\p q -> p <=> ((not) q))


exercise2_19 = (logEquiv1 (\p -> p) (\q -> q)) && (logEquiv2 (\p q -> p ==> q) (\q p -> q ==> p))
exercise2_20a = logEquiv2 (\p q -> (not p) ==> q) (\p q -> p ==> (not q))
exercise2_20b = logEquiv2 (\p q -> (not p) ==> q) (\p q -> q ==> (not p))
exercise2_20c = logEquiv2 (\p q -> (not p) ==> q) (\p q -> (not) q ==> p)
exercise2_20d = logEquiv3 (\p q r -> p ==> (q ==> r)) (\p q r -> q ==> (p ==> r))
exercise2_20e = logEquiv3 (\p q r -> p ==> (q ==> r)) (\p q r -> (p ==> q) ==> r)
exercise2_20f = logEquiv2 (\p q -> (p ==> q) ==> p) (\p q -> p)
exercise2_20g = logEquiv3 (\p q r -> p || q ==> r) (\p q r -> (p ==> r) && (q ==> r))

-- exercise 2.51
unique :: (a -> Bool) -> [a] -> Bool
unique p xs = length (filter p xs) == 1

-- exercise 2.52
-- An even number is probably an even element number? the exercise is not well defined
parity :: [Bool] -> Bool
parity xs = any (\x -> even (snd x) && (fst x) == True) (zip xs [1..])

-- exercise 2.53
evenNR :: (a -> Bool) -> [a] -> Bool
evenNR p xs = parity (map p xs)