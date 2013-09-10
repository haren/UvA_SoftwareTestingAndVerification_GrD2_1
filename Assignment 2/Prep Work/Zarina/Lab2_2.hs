import Week2

 -- satisfiable :: Form -> Bool
 -- satisfiable f = any (\ v -> eval v f) (allVals f)
 
 
  -- Tautology: (instead of any will be all, to make them true)
  
tautology:: Form -> Bool
tautology f = all (\ v -> eval v f) (allVals f)
  
tTest1 = Equiv p (Neg(Neg p))   -- True
tTest2 = Equiv p (Neg p)		-- False


 
  -- Contradiction: (If p is a tautology, then -p is a contradiction)
  
contradiction :: Form -> Bool
contradiction f = not (satisfiable f)
  
cTest1 = Equiv p (Neg p) 	  -- True 
cTest2 = Equiv p ( Impl p q)  -- False 
  
  -- Logical entailment (In logic, entailment (or logical implication) is a relation between sets of sentences and a sentence.)
  -- We can determine logical entailment by determining unsatisfiability.
 
entails :: Form -> Form -> Bool
entails f1 f2 = not(contradiction (Impl f1 (Neg f2)))  -- I suppose, using tautology is compacter :P Idea is to see if p |= -q is unsatisfiable, then p |=q 

	-- Logical equivalence (tautology -> equival)
equiv :: Form -> Form -> Bool
equiv f1 f2 = tautology (Equiv f1 f2)

eTestT_1 = Impl p q
eTestT_2 = Dsj [(Neg p), q]

eTestF_1 = Impl p q
eTestF_2 = Dsj [p, q]



  