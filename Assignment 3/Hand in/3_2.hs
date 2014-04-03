module Week3_2 where

import System.Random
import Data.List
import Week2



-- Software verification and testing
-- Assignment for week 3
-- Answer to the question 6
-- Group D2_1
-- Cigdem Aytekin 10463135,
-- Jorryt Jan Dijkstra 10462015,
-- Z.E. 10628185,
-- Lukasz Harezlak, 10630171




-- New file due to redefinition of Neg in Week3.hs
-- Random implementations from the slides, used for cnf test
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

getRandomIntInRange :: Int -> Int -> IO Int
getRandomIntInRange a b = getStdRandom (randomR (a,b))

getRandomF :: IO Form
getRandomF = do d <- getRandomInt 4
                getRandomForm d
getRandomForm :: Int -> IO Form
getRandomForm 0 = do m <- getRandomInt 20
                     return (Prop (m+1))
getRandomForm d = do n <- getRandomInt 3
                     case n of
                       0 -> do m <- getRandomInt 20
                               return (Prop (m+1))
                       1 -> do f <- getRandomForm (d-1)
                               return (Neg f)
                       2 -> do m  <- getRandomIntInRange 2 5
                               fs <- getRandomForms (d-1) m
                               return (Cnj fs)
                       3 -> do m  <- getRandomIntInRange 2 5
                               fs <- getRandomForms (d-1) m
                               return (Dsj fs)

getRandomFs :: Int ->  IO [Form]
getRandomFs n = do d <- getRandomInt 3
                   getRandomForms d n

getRandomForms :: Int -> Int -> IO [Form]
getRandomForms _ 0 = return []
getRandomForms d n = do
                     f <- getRandomForm d
                     fs <- getRandomForms d (n-1)
                     return (f:fs)

-- Unaltered cnf implementation of Week2
-- VVZ: why unaltered? where is my flattener for nested disjunctions/conjunctions?
cnf :: Form -> Form
cnf (Prop l) = Prop l
cnf (Neg n) = Neg $ cnf n
cnf (Cnj c) = Cnj (map cnf c)
cnf (Dsj d) = distlist (map cnf d) -- dist over the complete disjunction list
cnf (f) = f

distlist :: [Form] -> Form
distlist [x] = x
distlist [x,y] = dist x y
distlist (x:xs) = dist x (distlist xs)

dist :: Form -> Form -> Form
dist (Cnj x) y = Cnj (map (\z -> dist z y) x)
dist x (Cnj y) = Cnj (map (\z -> dist x z) y)
dist x y = Dsj[x, y]

-- tautology 
tautology :: Form -> Bool
tautology f = all (\ v -> eval v f) (allVals f)

-- logical equivalence
equiv :: Form -> Form -> Bool
equiv f1 f2 = tautology (Equiv f1 f2)

testcnf = do
          r <- getRandomFs 10 -- test 10 random formula's
          let c = map processToCnf r
          let equal = map (\x -> equiv (fst x) (snd x)) ([(x,y) | x <- r, y <- c]) -- test whether both formula's (cnf'ed and random forms) are equivalent
          let valid = map (validateElement) (c) -- validate whether the structure of the output is correct
          return (and(equal) && and(valid))

-- We have tested the cnf with random formula's. Our results are as follows: 
-- The cnf conversion takes as input a propositional logic statement
-- and produces an output which should be in CNF form.
-- What we see from the real data (tests with the random forms) is
-- that it produces a syntactically correct propositional logic statement
-- as output, however, the output is not logicaly equivalent to the
-- input form. We have tested cnf in the second week, with a few
-- manual input and there, the cnf worked correctly for our limited input data.
-- For the randomly generated forms, however, it is unfortunatley not the case.
-- We are looking for the reason behind this defect.  

-- VVZ: please keep me informed about this activity.
-- VVZ: some of the things to think about:
-- VVZ: (1) better reporting on cases - as in, "tested ... on ... failed/passed", etc, instead of "False"
-- VVZ: (2) making patterns exhaustive (how about Dsj []?)
-- VVZ: (3) implementing the flattener of nested conjunctions/disjunctions, just to be on the safe side
-- VVZ: (4) rewrite parts of the program that expect weird things like binary disjunction
-- VVZ: (5) come to me if you get stuck with any of these

processToCnf :: Form -> Form
processToCnf = cnf . nnf . arrowfree

validateElement :: Form -> Bool
validateElement (Cnj x) = length x > 1 -- conjunction should be over more than one formula's
-- VVZ: why?
validateElement (Prop x) = True
validateElement (Neg x) = True
validateElement (Dsj x) = length x == 2 -- An outer disjunction should have two elements max
-- VVZ: why?
validateElement (x) = False