-- Software verification and testing
-- Lab Assignment 2, Answer of Question 3, CNF

-- Group D2_1
-- Cigdem Aytekin 10463135,
-- Jorryt Jan Dijkstra 10462015,
-- Zarina Efendijeva 10628185, 
-- Lukasz Harezlak, 10630171

-- time spent on excercise: 8 hours
import Data.List
import Week2

cnf :: Form -> Form
cnf (Prop l) = Prop l
cnf (Cnj c) = Cnj (map (\x -> cnf x) c)
cnf (Dsj d) = distlist (map cnf d) -- dist over the complete disjunction list
cnf (f) = f

distlist :: [Form] -> Form
distlist [x] = x
distlist (x:xs) = dist x (distlist xs)

dist :: Form -> Form -> Form
dist (Cnj x) y = Cnj (map (\z -> dist z y) x)
dist x (Cnj y) = Cnj (map (\z -> dist x z) y)
dist x y = Dsj[x, y]

cnftests :: [(Form, Form)] -- [(input of cnf, output of cnf)]
cnftests = [(Dsj[Cnj[p,q],r], Cnj[Dsj[p,r],Dsj[q,r]]), (Dsj[p, Cnj[q, r]], Cnj[Dsj[p, q], Dsj[p, r]]), ( Cnj[Impl p q, Impl q r], Cnj [Dsj [Neg p, q], Dsj [Neg q, r]]), (Impl (Neg q) (Cnj [p, r]), Cnj [(Dsj [q, p]) , (Dsj [q, r])])] 

-- Taken from the example in the slides :)
testcnf = inputcnf cnftests

inputcnf :: [(Form, Form)] -> Bool
inputcnf (x:xs) = ((cnf (nnf (arrowfree (fst x)))) == (snd x)) && inputcnf xs
inputcnf [] = True


