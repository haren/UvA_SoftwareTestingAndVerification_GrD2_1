{--
Group:
	Cigdem Aytekin
	Jorryt Dijkstra
	Zarina Efendijeva
	Lukasz Harezlak
--}

module Sol2  where
	import GS
	import TAMO

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