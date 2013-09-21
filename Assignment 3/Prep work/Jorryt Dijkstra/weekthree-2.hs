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
cnf (Neg n) = Neg (cnf n)
cnf (Prop l) = Prop l
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

-- Taken from the example in the slides :)
testcnf = do
	r <- (getRandomInt 4)
	f <- (getRandomForms (r - 1) 10)
	let b = (inputcnf f)
	return b

inputcnf :: [Form] -> Bool
inputcnf (x:xs) = (validatecnj (cnf (nnf (arrowfree (x))))) && inputcnf xs
inputcnf [] = True

validatecnj (Cnj x) = True
validatecnj (Prop x) = True
validatecnj (Neg x) = True
validatecnj (x) = False

