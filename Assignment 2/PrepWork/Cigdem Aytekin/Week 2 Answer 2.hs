module Module22 where
import Week2
import Data.List



tautology :: Form -> Bool
tautology f = all (\ v -> eval v f) (allVals f)


contradiction :: Form -> Bool
contradiction f = not (satisfiable f)


-- logical equivalence
equivalent :: Form -> Form -> Bool
equivalent f1 f2 = tautology (Equiv f1 f2) 


-- logical entailment
entailment :: Form -> Form -> Bool
entailment f1 f2 = tautology (Impl f1 f2)


