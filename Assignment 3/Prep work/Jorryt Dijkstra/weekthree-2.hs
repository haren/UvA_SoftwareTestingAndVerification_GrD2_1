import System.Random
import Data.List
import Week2


-- New file due to redefinition of Neg in Week3.hs

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

getRandomIntWithRange :: Int -> Int -> IO Int
getRandomIntWithRange a b = getStdRandom (randomR (a,b))

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
                       2 -> do m  <- getRandomIntWithRange 2 5
                               fs <- getRandomForms (d-1) m
                               return (Cnj fs)
                       3 -> do m  <- getRandomIntWithRange 2 5
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
          let equal = map (\x -> equiv (fst x) (snd x)) ([(x,y) | x <- r, y <- c]) -- test whether the equivalence of both formula's (cnf'ed and random forms are the same)
          -- putStrLn ("equal: " ++ show equal ++ " of " ++ show ([(x,y) | x <- r, y <- c])) 
          let valid = map (validateElement) (c) -- validate whether the structure of the output is correct
          return (and(equal) && and(valid))

processToCnf :: Form -> Form
processToCnf = cnf . nnf . arrowfree

validateElement :: Form -> Bool
validateElement (Cnj x) = length x > 1 -- conjunction should be over more than one formula's
validateElement (Prop x) = True
validateElement (Neg x) = True
validateElement (Dsj x) = length x == 2 -- An outer disjunction should have two elements max
validateElement (x) = False

