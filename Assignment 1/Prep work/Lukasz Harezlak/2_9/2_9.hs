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

test3a = logEquiv2 (\ p q -> ( p ==> q)) (\ p q -> not p || q)

//copy the rest examples from the book?