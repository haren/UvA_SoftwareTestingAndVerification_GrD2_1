import Week2

-- testing helpful material

-- any (1==) [0,1,2,3,4,5]
-- if any of the 2 argument meets the condition, return true



-- satisfiable :: Form -> Bool
-- satisfiable f = any (\ v -> eval v f) (allVals f)

contradiction :: Form -> Bool
--contradiction f = not any (\ v -> eval v f) (allVals f)
contradiction f = not (satisfiable f)

--tautology :: Form -> Bool

-- logical entailment
--entails :: Form -> Form -> Bool

-- logical equivalence
--equiv :: Form -> Form -> Bool

-- test variables
testContradictionTrue = Equiv p (Neg p)
testContradictionFalse = Equiv p (Impl (Neg p) p)

-- testing results
{-
*Main> contradiction testContradictionTrue
True
*Main> contradiction testContradictionFalse
False
*Main>
-}