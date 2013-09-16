module Techniques where 

import Data.List
import Data.Char
import System.Random
import Week2

data Token 
      = TokenNeg
      | TokenCnj
      | TokenDsj
      | TokenImpl
      | TokenEquiv 
      | TokenInt Int 
      | TokenOP
      | TokenCP
 deriving (Show,Eq)

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) | isSpace c = lexer cs
             | isDigit c = lexNum (c:cs) 
lexer ('(':cs) = TokenOP : lexer cs
lexer (')':cs) = TokenCP : lexer cs
lexer ('*':cs) = TokenCnj : lexer cs
lexer ('+':cs) = TokenDsj : lexer cs
lexer ('-':cs) = TokenNeg : lexer cs 
lexer ('=':'=':'>':cs) = TokenImpl : lexer cs
lexer ('<':'=':'>':cs) = TokenEquiv : lexer cs
lexer (x:_) = error ("unknown token: " ++ [x])

lexNum cs = TokenInt (read num) : lexer rest
     where (num,rest) = span isDigit cs

type Parser a b = [a] -> [(b,[a])]

succeed :: b -> Parser a b
succeed x xs = [(x,xs)]

parseForm :: Parser Token Form 
parseForm (TokenInt x: tokens) = [(Prop x,tokens)]
parseForm (TokenNeg : tokens) =
  [ (Neg f, rest) | (f,rest) <- parseForm tokens ]
parseForm (TokenCnj : TokenOP : tokens) = 
  [ (Cnj fs, rest) | (fs,rest) <- parseForms tokens ]
parseForm (TokenDsj : TokenOP : tokens) = 
  [ (Dsj fs, rest) | (fs,rest) <- parseForms tokens ]
parseForm (TokenOP : tokens) = 
    [ (Impl f1 f2, rest) | (f1,ys) <- parseForm tokens,
                           (f2,rest) <- parseImpl ys ]
     ++
    [ (Equiv f1 f2, rest) | (f1,ys) <- parseForm tokens,
                            (f2,rest) <- parseEquiv ys ] 
parseForm tokens = []

parseForms :: Parser Token [Form] 
parseForms (TokenCP : tokens) = succeed [] tokens
parseForms tokens = 
   [(f:fs, rest) | (f,ys) <- parseForm tokens, 
                   (fs,rest) <- parseForms ys ]

parseImpl :: Parser Token Form
parseImpl (TokenImpl : tokens) = 
  [ (f,ys) | (f,y:ys) <- parseForm tokens, 
              y == TokenCP ]
parseImpl tokens = []

parseEquiv :: Parser Token Form
parseEquiv (TokenEquiv : tokens) = 
  [ (f,ys) | (f,y:ys) <- parseForm tokens, 
              y == TokenCP ]
parseEquiv tokens = []

parse :: String -> [Form]
parse s = [ f | (f,_) <- parseForm (lexer s) ]

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

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
                       2 -> do m  <- getRandomInt 5 
                               fs <- getRandomForms (d-1) m
                               return (Cnj fs)
                       3 -> do m  <- getRandomInt 5 
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

test :: Int -> (Form -> Bool) -> [Form] -> IO ()
test n _ [] = print (show n ++ " tests passed")
test n p (f:fs) = 
  if p f 
  then do print ("pass on:" ++ show f)
          test n p fs
  else error ("failed test on:" ++ show f)

testForms :: Int -> (Form -> Bool) -> IO ()
testForms n prop = do 
  fs <- getRandomFs n
  test n prop fs

testParser :: IO ()
testParser = testForms 100
   (\ f -> let [g] = parse (show f) in 
           show f == show g)

