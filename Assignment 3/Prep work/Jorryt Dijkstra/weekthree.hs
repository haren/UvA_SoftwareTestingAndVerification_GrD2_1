import System.Random
import Data.List
import Week3

getRandomInt :: IO Int
getRandomInt = getStdRandom(random)

getRandomIntInRange :: Int -> Int -> IO Int
getRandomIntInRange x y = getStdRandom(randomR(x,y))

getRandomInts :: Int -> IO [Int]
getRandomInts 0 = return []
getRandomInts n = do
	f <- getRandomInt
	fs <- getRandomInts (n-1)
	return (f : fs)

getRandomIntsWithRange :: Int -> Int -> Int -> IO [Int]
getRandomIntsWithRange 0 _ _ = return []
getRandomIntsWithRange n x y = do
	f <- getRandomIntInRange x y
	fs <- getRandomIntsWithRange (n-1) x y
	return (f : fs)

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = (length xs == length ys) && (null (deleteFirstsBy (==) xs ys))


testPermutations = inputPermutations 10000 -- amount of variations to test
inputPermutations :: Int -> IO Bool
inputPermutations 0 = return True
inputPermutations x = do
  i <- getRandomIntsWithRange 10 1 10000 -- generate a random list of 10 ints within boundaries
  r <- getRandomInt
  o <- inputPermutations (x-1)
  let s = isPermutation (sort i) i -- check if the sorted random list is a permutation of the unaltered random list
  let s' = (not) (isPermutation (drop 1 i) i)
  let s'' = (not) (isPermutation i (r : i))
  let selfPermutation = isPermutation i i -- should be a permutation of itself
  return (selfPermutation && s && s' && s'' && o) -- combine all permutation checks

terms = [x, y, z]
getRandomTerm :: IO Term
getRandomTerm = do
		r <- getRandomIntInRange 0 2
		return (terms !! r)

getRandomLowercaseLetter :: IO Char
getRandomLowercaseLetter = do
                    c <- randomRIO('a','z')
                    return c

getRandomUppercaseLetter :: IO Char
getRandomUppercaseLetter = do
                    c <- randomRIO('A','Z')
                    if (c == 'E' || c == 'A') -- These are reserved for printing exists and for all
                    then do x <- getRandomUppercaseLetter
                            return x
                    else return c

getRandomF :: IO Formula
getRandomF = do d <- getRandomIntInRange 0 3
                getRandomForm d

getRandomAtomFormWithBoundLetter :: Int -> Term -> IO Formula
getRandomAtomFormWithBoundLetter n c = do
                                          r <- getRelations (n-1)
                                          f <- getRandomUppercaseLetter
                                          p <- randomRIO(0, (length r) - 1)
                                          -- Randomly place the bound letter
                                          let (xs, ys) = splitAt p r
                                          return (Atom [f] (xs ++ [c] ++ ys))

getRandomAtomUnboundForm :: Int -> IO Formula
getRandomAtomUnboundForm n = do
                                r <- getRelations (n-1)
                                f <- getRandomUppercaseLetter
                                return (Atom [f] r)


getRelations :: Int -> IO [Term]
getRelations 0 = return []
getRelations i = do
                    x <- getRelations (i-1)
                    t <- getRandomTerm
                    return (t : x)

getRandomAtomForm :: Char -> IO Formula
getRandomAtomForm c = do
                        p <- getRandomIntInRange 2 4
                        n <- getRandomIntInRange 1 2
                        -- Generate bound and unbound forms with at least 1 to 3 relations
                        case n of
                          1 -> do f <- getRandomAtomFormWithBoundLetter p (V [c])
                                  return f
                          2 -> do f <- getRandomAtomUnboundForm p
                                  return f

getRandomForm :: Int -> IO Formula
getRandomForm 0 = do c <- getRandomUppercaseLetter
                     f <- getRandomAtomForm c
                     return (f)

getRandomForm d = do n <- getRandomIntInRange 0 5
                     case n of
                       0 -> do f <- getRandomForm 0
                               return f
                       1 -> do f <- getRandomForm (d-1)
                               return (Neg f)
                       2 -> do m <- getRandomIntInRange 2 4
                               fs <- getRandomForms (d-1) m
                               return (Conj fs)
                       3 -> do m  <- getRandomIntInRange 2 4
                               fs <- getRandomForms (d-1) m
                               return (Disj fs)
                       4 -> do c <- getRandomLowercaseLetter
                               f <- getRandomAtomForm c
                               return (Forall [c] f)
                       5 -> do c <- getRandomLowercaseLetter
                               f <- getRandomAtomForm c
                               return (Exists [c] f)

getRandomFs :: Int ->  IO [Formula]
getRandomFs n = do d <- getRandomIntInRange 0 5
                   getRandomForms d n

getRandomForms :: Int -> Int -> IO [Formula]
getRandomForms _ 0 = return []
getRandomForms d n = do
                     f <- getRandomForm d
                     fs <- getRandomForms d (n-1)
                     return (f:fs)

------
{--
data Token
      = TokenNeg
      | TokenConj
      | TokenDisj
      | TokenImpl
      | TokenEquiv
      | TokenInt Int
      | TokenAll Char
      | TokenExists Char
      | TokenOP
      | TokenCP
 deriving (Show,Eq)

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) | isSpace c = lexer cs
             | isDigit c = lexNum (c:cs)
lexer (’(’:cs) = TokenOP : lexer cs
lexer (’)’:cs) = TokenCP : lexer cs
lexer (’*’:cs) = TokenConj : lexer cs
lexer (’+’:cs) = TokenDisj : lexer cs
lexer (’-’:cs) = TokenNeg : lexer cs
lexer (’=’:’=’:’>’:cs) = TokenImpl : lexer cs
lexer (’<’:’=’:’>’:cs) = TokenEquiv : lexer cs
lexer ('A':x:xs) = TokenAll x : lexer xs
lexer ('E':x:xs) = TokenExists x : lexer xs
lexer (x:_) = error ("unknown token: " ++ [x])

lexNum cs = TokenInt (read num) : lexer rest
     where (num,rest) = span isDigit cs

type Parser a b = [a] -> [(b,[a])]
parseForm :: Parser Token Form
parseForm (TokenInt x : tokens) = [(Prop x,tokens)]
parseForm (TokenNeg : tokens) =
  [ (Neg f, rest) | (f,rest) <- parseForm tokens ]
parseForm (TokenConj : TokenOP : tokens) =
  [ (Cnj fs, rest) | (fs,rest) <- parseForms tokens ]
parseForm (TokenDisj : TokenOP : tokens) =
  [ (Dsj fs, rest) | (fs,rest) <- parseForms tokens ]
parseForm (TokenOP : tokens) =
  [ (Impl f1 f2, rest) | (f1,ys) <- parseForm tokens, (f2,rest) <- parseImpl ys ]
     ++
    [ (Equiv f1 f2, rest) | (f1,ys) <- parseForm tokens, (f2,rest) <- parseEquiv ys ]
parseForm tokens = []
--}