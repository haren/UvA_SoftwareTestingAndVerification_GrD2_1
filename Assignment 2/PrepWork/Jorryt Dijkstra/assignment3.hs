module AssignmentCNF where

import Week2
{-
cnf :: Form -> Form
cnf x
	| (x == Cnj x) = error "cnf cnj"
	| (x == Dsj [p, q]) = error "cnf dsj"
	| otherwise = x
-}
cnf2 :: Form -> Form
cnf2 x = cnf3 x where 
	cnf3 (Cnj x) = Cnj[cnf2(head x), cnf2 (tail x)]
	cnf3 (Dsj x) = (dist (cnf2 (head x)) (cnf2 (tail x)))
	cnf3 x = x

dist :: Form -> Form -> Form
dist x y 
	| (x == Cnj [p, q]) = Cnj[dist p y, dist q y]
	| (y == Cnj [p, q]) = Cnj[dist x p, dist x q]
	| otherwise = Dsj[x, y]


testCnj = Cnj[p,q]
testDsj = Dsj[p,q]
testFrm = Dsj[(Cnj[p,q]), r]

{-
	| (x == Cnj [p, q]) = Cnj[cnf p, cnf q]
	| (x == Dsj [p, q]) = (dist (cnf p) (cnf q))
	| otherwise = x


	| (x == Cnj [p, q]) = Cnj[dist p y, dist q y]
	| (y == Cnj [p, q]) = Cnj[dist x p, dist x q]
	| otherwise = Dsj[x, y]

-}