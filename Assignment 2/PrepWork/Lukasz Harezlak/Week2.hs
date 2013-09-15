module Week2

where 

import Data.List
import Data.Char

data Coin = C Int

w :: Coin -> Float 
w (C n) = if n == lighter then 1 - 0.01
          else if n == heavier then 1 + 0.01
          else 1

weight :: [Coin] -> Float
weight = sum . (map w)

balance :: [Coin] -> [Coin] -> Ordering 
balance xs ys = 
  if weight xs < weight ys then LT
  else if weight xs > weight ys then GT
  else EQ

outcome :: (Float -> Bool) -> (Float, Bool -> Float) 
            -> Float
outcome accept (x,decide) = 
  if accept x then x + (decide True)
              else (1 - x) + (decide False)

jill :: Float -> Bool
jill = \ x -> x > 1/2

joe :: (Float, Bool -> Float)
joe = (2/3, \p -> if p then 1/100000 else 1/2)

jillVariations :: [Float -> Bool]
jillVariations = 
   [ \ x -> x >= y/100 | y <- [50..100] ]

joeVariations :: [(Float, Bool -> Float)]
joeVariations = 
  [(z/100,\ p -> if p then 1/100000000 else 1/2) 
                               | z <- [50..100] ]

testJill1 :: Float
testJill1 = minimum 
  [ outcome ji jo | ji <- jillVariations, 
                    jo <- joeVariations ]

testJill2 :: Float
testJill2 = minimum 
  [ outcome jill jo | jo <- joeVariations ]

testJoe1 :: Float
testJoe1 = 2 - maximum 
  [ outcome ji jo | ji <- jillVariations, 
                    jo <- joeVariations ]

testJoe2 :: Float
testJoe2 = 2 - maximum 
  [ outcome ji joe | ji <- jillVariations ]

run :: Integer -> [Integer]
run n | n < 1 = error "argument not positive"
      | n == 1 = [1]
      | even n = n: run (div n 2)
      | odd n  = n: run (3*n+1)

type Name = Int

data Form = Prop Name
          | Neg  Form
          | Cnj [Form]
          | Dsj [Form]
          | Impl Form Form 
          | Equiv Form Form 
          deriving Eq

instance Show Form where 
  show (Prop x)   = show x
  show (Neg f)    = '-' : show f 
  show (Cnj fs)     = "*(" ++ showLst fs ++ ")"
  show (Dsj fs)     = "+(" ++ showLst fs ++ ")"
  show (Impl f1 f2)  = "(" ++ show f1 ++ "==>" 
                           ++ show f2 ++ ")"
  show (Equiv f1 f2)  = "(" ++ show f1 ++ "<=>" 
                           ++ show f2 ++ ")"

showLst,showRest :: [Form] -> String
showLst [] = ""
showLst (f:fs) = show f ++ showRest fs
showRest [] = ""
showRest (f:fs) = ' ': show f ++ showRest fs

p = Prop 1
q = Prop 2
r = Prop 3

form1 = Equiv (Impl p q) (Impl (Neg q) (Neg p))
form2 = Equiv (Impl p q) (Impl (Neg p) (Neg q))
form3 = Impl (Cnj [Impl p q, Impl q r]) (Impl p r)

propNames :: Form -> [Name]
propNames = sort.nub.pnames where 
  pnames (Prop name) = [name]
  pnames (Neg f)  = pnames f
  pnames (Cnj fs) = concat (map pnames fs)
  pnames (Dsj fs) = concat (map pnames fs)
  pnames (Impl f1 f2) = concat (map pnames [f1,f2])
  pnames (Equiv f1 f2) = 
          concat (map pnames [f1,f2])

type Valuation = [(Name,Bool)]

-- all possible valuations for list of prop letters
genVals :: [Name] -> [Valuation]
genVals [] = [[]]
genVals (name:names) = 
  map ((name,True) :) (genVals names)
  ++ map ((name,False):) (genVals names)

-- generate all possible valuations for a formula
allVals :: Form -> [Valuation]
allVals = genVals . propNames

eval :: Valuation -> Form -> Bool
eval [] (Prop c)    = error ("no info: " ++ show c)
eval ((i,b):xs) (Prop c)
     | c == i    = b
     | otherwise = eval xs (Prop c)
eval xs (Neg f)  = not (eval xs f)
eval xs (Cnj fs) = all (eval xs) fs
eval xs (Dsj fs) = any (eval xs) fs
eval xs (Impl f1 f2) = 
     not (eval xs f1) || eval xs f2
eval xs (Equiv f1 f2) = eval xs f1 == eval xs f2

satisfiable :: Form -> Bool
satisfiable f = any (\ v -> eval v f) (allVals f)

-- no precondition: should work for any formula. 
arrowfree :: Form -> Form 
arrowfree (Prop x) = Prop x 
arrowfree (Neg f) = Neg (arrowfree f)
arrowfree (Cnj fs) = Cnj (map arrowfree fs)
arrowfree (Dsj fs) = Dsj (map arrowfree fs)
arrowfree (Impl f1 f2) = 
  Dsj [Neg (arrowfree f1), arrowfree f2]
arrowfree (Equiv f1 f2) = 
  Dsj [Cnj [f1', f2'], Cnj [Neg f1', Neg f2']]
  where f1' = arrowfree f1
        f2' = arrowfree f2

-- precondition: input is arrowfree
nnf :: Form -> Form 
nnf (Prop x) = Prop x
nnf (Neg (Prop x)) = Neg (Prop x)
nnf (Neg (Neg f)) = nnf f
nnf (Cnj fs) = Cnj (map nnf fs)
nnf (Dsj fs) = Dsj (map nnf fs)
nnf (Neg (Cnj fs)) = Dsj (map (nnf.Neg) fs)
nnf (Neg (Dsj fs)) = Cnj (map (nnf.Neg) fs)

lighter, heavier :: Int
lighter = 3
heavier = 0

