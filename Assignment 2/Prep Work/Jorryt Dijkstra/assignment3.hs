module AssignmentCNF where

import Week2

cnf :: Form -> Form
cnf x
	| (x == Cnj [p, q]) = Cnj[cnf p, cnf q]
	| (x == Dsj [p, q]) = (dist (cnf p) (cnf q))
	| otherwise = x


dist :: Form -> Form -> Form
dist x y 
	| (x == Cnj [p, q]) = Cnj[dist p y, dist q y]
	| (y == Cnj [p, q]) = Cnj[dist x p, dist x q]
	| otherwise = Dsj[x, y]



