module AssignmentCNF where

import Week2

cnf2 :: Form -> Form
cnf2 x = cnf3 x where 
	cnf3 (Cnj x) = Cnj (map cnf2 x)
	cnf3 (Dsj x) = dist_ (map cnf2 x)
	cnf3 x = x

dist_ :: [Form] -> Form
dist_ [] = error "empty list"
dist_ [f] = f
dist_ (f:fs) = (dist f (dist_ fs))

--dist :: Form -> Form -> Form
--dist x y = Dsj[x,y]

-- precondition: input forms are in cnf
dist :: Form -> Form -> Form
dist p (Cnj fs) = Cnj (map (\x -> dist p x) fs)
dist (Cnj fs) q = Cnj (map (\x -> dist x q) fs) 
dist p q = Dsj [p, q]

{-
cnf :: Form -> Form
cnf x
	| (x == Cnj x) = error "cnf cnj"
	| (x == Dsj [p, q]) = error "cnf dsj"
	| otherwise = x
-}
{-
dist :: Form -> Form -> Form
dist x y 
	| (x == Cnj [p, q]) = Cnj[dist p y, dist q y]
	| (y == Cnj [p, q]) = Cnj[dist x p, dist x q]
	| otherwise  = Dsj[x, y]

-}

{-
cnf3 :: Form -> Form
cnf3 (Prop x) = Prop x
cnf3 (Cnj fs) = Cnj (map cnf3 fs)
cnf3 (Dsj fs) = dist (map cnf3 fs)
-}

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