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
test2_12a = logEquiv2 (\p q -> not True) (\p q -> False)
test2_12a_2 = logEquiv2 (\p q -> not False) (\p q -> True)
test2_12b = logEquiv2 (\p q -> p ==> False) (\p q -> not p)
test2_12c = logEquiv2 (\p q -> p || True) (\p q -> True)
test2_12c_2 = logEquiv1 (\p -> p && False) (\p -> False)
test2_12d = logEquiv1 (\p -> p || False) (\p -> p)
test2_12d_2 = logEquiv1 (\p -> p && True) (\p -> p)
test2_12e = logEquiv1 (\p -> p || (not) p) (\p -> True)
test2_12f = logEquiv1 (\p -> p && (not) p) (\p -> False)


-- Exercise 2.15
testContradiction1 ::  (Bool -> Bool) -> Bool
testContradiction1 f = (f True <=> False) && (f False <=> False)

testContradiction2 ::  (Bool -> Bool) -> Bool
testContradiction2 f = and [f p <=> False | p <- [True, False]]

testContradiction3 ::  (Bool -> Bool -> Bool) -> Bool
testContradiction3 f = and [f p q <=> False | p <- [True, False], q <- [True, False]]

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
parity xs = (length (filter (\x -> even (snd x) && (fst x) == True) (zip xs [0..]))) == 1

-- exercise 2.53
evenNR :: (a -> Bool) -> [a] -> Bool
evenNR p xs = parity (map p xs)