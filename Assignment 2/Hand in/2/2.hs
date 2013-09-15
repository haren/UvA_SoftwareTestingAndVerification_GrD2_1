-- Software verification and testing
-- Lab Assignment 1

-- Group D2_1
-- Cigdem Aytekin 10463135,
-- Jorryt Jan Dijkstra 10462015,
-- Zarina Efendijeva 10628185, 
-- Lukasz Harezlak, 10630171

-- time spent on excercise: 45 minutes

import Week2

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
equiv :: Form -> Form -> Bool
equiv f1 f2 = tautology (Equiv f1 f2)


-------------------------------------------------------------

-- test variables - group 1
testContradictionTrue = Equiv p (Neg p)
testContradictionFalse = Equiv p (Impl (Neg p) p)

testTautologyTrue = Equiv p (Neg (Neg p))
testTautologyFalse = Equiv p (Impl p q)

testEntailsTrue_1 = Equiv p q
testEntailsTrue_2 = Equiv p p

testEntailsFalse_1 = Equiv p q
testEntailsFalse_2 = Equiv q (Neg q)

testEquivTrue_1 = Equiv p p
testEquivTrue_2 = Equiv q q

testEquivFalse_1 = Equiv p (Neg p)

-- test variables - group 2
-- equivalences in a list in the form of tuples (a, b) where a represents formula a of the equality and b formula b 
equivalences1 = [(Cnj[p,p],p), (p,p), (p, Neg $ Neg $ p), ((Impl p q), Dsj[Neg p, q]), (Neg $ (Impl p q), Cnj[p, Neg q]), ((Impl (Neg p) (Neg q)), (Impl q p)), ((Impl p (Neg q)), (Impl q (Neg p))), ((Impl (Neg p) q), (Impl (Neg q) p))] -- Theorem 2.10
equivalences2 = [((Equiv p q), Cnj[Impl p q, Impl q p]), ((Equiv p q), Dsj[Cnj[p, q], Cnj[Neg p, Neg q]]), (Cnj[p, q], Cnj[q, p]), (Dsj[p, q], Dsj[q, p]), ((Neg $ Cnj[p, q]), Dsj[Neg p, Neg q]), ((Neg $ Dsj[p, q]), Cnj[Neg p, Neg q])] -- Theorem 2.10
equivalences3 = [((Cnj[p, (Cnj[q, r])]), Cnj[(Cnj[p, q]), r]), ((Dsj[p, (Dsj[q, r])]), Dsj[(Dsj[p, q]), r]), ((Cnj[p, (Dsj[q, r])]), Dsj[(Cnj[p, q]), Cnj[p, r]]), ((Dsj[p, (Cnj[q, r])]), Cnj[(Dsj[p, q]), Dsj[p, r]])] -- Theorem 2.10

testEquiv = inputEquivalences equivalences
testEntails = (entails p p) && (entails q q) && (entails r r) && (entails (Dsj[p, q, r]) (Dsj[p, q, r])) && (not $ entails p q)

-- Actually tested with the equiv/entails, but this does rely on those methods, should we (because we probably shouldnt know the implementation of those functions at all and thus test it here separately)?
testTautology = (tautology $ Dsj[p, Neg p]) && (tautology $ Impl p p)

testContradiction = contradiction (Cnj[Neg p, p]) -- need more testcases please

--auxiliary test code
equivalences :: [(Form, Form)]
equivalences = equivalences1 ++ equivalences2 ++ equivalences3

inputEquivalences :: [(Form, Form)] -> Bool
inputEquivalences [] = True;
inputEquivalences (x:xs) = (equiv (fst x) (snd x)) && inputEquivalences xs
-------------------------------------------------------------
{- 

Testing results group 1

We picked this group so as to test the solution with the most obvious,
yet resulting in informative feedback, exmples.

Here's the console output when running the tests:

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

*Main> equiv testEquivTrue_1 testEquivTrue_2
True
*Main> equiv testEquivTrue_1 testEquivFalse_1
False
*Main> equiv testEquivTrue_2 testEquivFalse_1
False


Testing results group 2

This tests cases - complete theorems were used to verify the correctness of the functions 
- whether the outcome is indeed what is expected to be.

The theorems have been proven before, so we use them to prove the validity of
our functions.

*Main> testEquiv
True

*Main> testEntails
True

*Main> testTautology
True

*Main> testContradiction
True


-}