import Week2

-- time spent on excercise: 3 hours

-------------------------------------------------------------
cnf :: Form -> Form
cnf (Cnj x) = Cnj (map cnf x)
cnf (Dsj x) = distAux (map cnf x)
cnf x = x

distAux :: [Form] -> Form
distAux [] = error "empty list"
distAux [f] = f
distAux (f:fs) = (dist f (distAux fs))

dist :: Form -> Form -> Form
dist p (Cnj fs) = Cnj (map (\x -> dist p x) fs)
dist (Cnj fs) q = Cnj (map (\x -> dist x q) fs) 
dist p q = Dsj [p, q]
-------------------------------------------------------------

-- test variables
testCnj = Cnj[p,q]
testDsj = Dsj[p,q]
testFrm = Dsj[(Cnj[p,q]), r]

-------------------------------------------------------------
-- testing results
{-

-}