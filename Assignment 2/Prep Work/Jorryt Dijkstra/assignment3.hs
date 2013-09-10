module AssignmentCNF where

import Week2

cnf :: Form -> Form
cnf (Cnj x) = Cnj[cnf (head x), subcnf (tail x)]
cnf (Dsj x) = Cnj[cnf p, cnf q]
cnf (Prop x) = Prop x
cnf (x) = error "invalid input"

subcnf :: [Form] -> Form
subcnf (x:xs) = (cnf x : subcnf xs)


dist :: Form -> Form -> Form
dist (Cnj x) y = Cnj[dist p y, dist q y]
dist x (Cnj y) = Cnj[dist x p, dist x q]
dist x y = Dsj[x, y]


testFrm = Dsj[Cnj[p, q], r]