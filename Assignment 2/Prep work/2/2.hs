import Week2

-- time spent on excercise: 

-------------------------------------------------------------

-- contradiction
contradiction :: Form -> Bool
contradiction f = not (satisfiable f)

-- tautology 
tautology :: Form -> Bool
tautology f = all (\ v -> eval v f) (allVals f)

-- logical entailment
-- Note: 1 has 2 as logical consequence if and only if 1 -> 2 is
-- a tautology.
entails :: Form -> Form -> Bool
entails f1 f2 = tautology (Impl f1 f2)

-- logical equivalence
--equiv :: Form -> Form -> Bool

-------------------------------------------------------------

-- test variables
testContradictionTrue = Equiv p (Neg p)
testContradictionFalse = Equiv p (Impl (Neg p) p)

testTautologyTrue = Equiv p (Neg (Neg p))
testTautologyFalse = Equiv p (Impl p q)

testEntailsTrue_1 = Equiv p q
testEntailsTrue_2 = Equiv p p

testEntailsFalse_1 = Equiv p q
testEntailsFalse_2 = Equiv q (Neg q)

-------------------------------------------------------------
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

*Main> entails testEntailsTrue_1 testEntailsTrue_2
True
*Main> entails testEntailsTrue_1 testEntailsFalse_2
False
*Main> entails testEntailsFalse_1 testEntailsFalse_2
False
-}