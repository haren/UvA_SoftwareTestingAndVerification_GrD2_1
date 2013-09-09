import Week2

--time spent on excercise: 

-- contradiction
contradiction :: Form -> Bool
contradiction f = not (satisfiable f)

-- tautology 
tautology :: Form -> Bool
tautology f = all (\ v -> eval v f) (allVals f)

-- logical entailment
-- entails :: Form -> Form -> Bool

-- logical equivalence
--equiv :: Form -> Form -> Bool

-- test variables
testContradictionTrue = Equiv p (Neg p)
testContradictionFalse = Equiv p (Impl (Neg p) p)
testTautologyTrue = Equiv p (Neg (Neg p))
testTautologyFalse = Equiv p (Impl p q)

-- testing results
{-
*Main> contradiction testContradictionTrue
True
*Main> contradiction testContradictionFalse
False

*Main> tautology testTautologyFalse
False
*Main> tautology testTautologyTrue
True
*Main> tautology testContradictionFalse --which is true - testContradictionFalse is a tautology
True
*Main> tautology testContradictionTrue
False

-}