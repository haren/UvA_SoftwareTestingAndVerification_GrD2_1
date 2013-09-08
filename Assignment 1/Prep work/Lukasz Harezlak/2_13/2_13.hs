(<=>) :: Bool -> Bool -> Bool 
x <=> y = x==y 

(<+>) :: Bool -> Bool -> Bool
x <+> y = x /= y  

(==>) :: Bool -> Bool -> Bool 
True ==> x = x 
False ==> x = True

logEquiv1 :: ( Bool -> Bool ) -> ( Bool -> Bool ) -> Bool 
logEquiv1 bf1 bf2 = 
	(bf1 True <=> bf2 True) && (bf1 False <=> bf2 False)


logEquiv2 :: ( Bool -> Bool -> Bool ) -> ( Bool -> Bool -> Bool ) -> Bool 
logEquiv2 bf1 bf2 = 
	and [(bf1 p q <=> bf2 p q) | p <- [True, False],
					q <- [True, False]]

formula3 p q = p 
formula4 p q = ( p <+> q ) <+> q

test1a = logEquiv1 (\ p -> not True) (\p -> False)
test1b = logEquiv1 (\ p -> not False) (\p -> True)
test2  = logEquiv1 (\ p -> p ==> False) (\p -> not p)
test3a = logEquiv1 (\ p -> p || True) (\ p -> True)
test3b = logEquiv1 (\ p -> p && True) (\ p -> p)
test4a = logEquiv1 (\ p -> p || False) (\ p -> p)
test4b = logEquiv1 (\ p -> p && False) (\ p -> False)
test5 = logEquiv1 (\ p -> p || not p) (\ p -> True)
test6 = logEquiv1 (\ p -> p && not p) (\ p -> False)