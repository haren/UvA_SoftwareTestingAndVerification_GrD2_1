module Week3

where 

import Data.List

type Name = String
data Term = V Name | F Name [Term] deriving (Eq,Ord)

instance Show Term where 
  show (V name)    = name
  show (F name []) = name 
  show (F name ts) = name ++ show ts

x, y, z :: Term 
x = V "x"
y = V "y"
z = V "z"

varsInTerm :: Term -> [Name]
varsInTerm (V name) = [name]
varsInTerm (F _ ts) = varsInTerms ts where 
  varsInTerms :: [Term] -> [Name]
  varsInTerms = nub . concat . map varsInTerm

subst :: Name -> Term -> Term -> Term
subst name t (V name') = 
  if name == name' then t else (V name') 
subst name t (F name' ts) = 
      F name' (map (subst name t) ts) 

data Formula = Atom Name [Term]
               | Eq Term Term
               | Neg  Formula 
               | Impl Formula Formula
               | Equi Formula Formula
               | Conj [Formula]
               | Disj [Formula] 
               | Forall Name Formula
               | Exists Name Formula
               deriving (Eq,Ord)

instance Show Formula where 
  show (Atom s [])   = s
  show (Atom s xs)   = s ++ show xs 
  show (Eq t1 t2)    = show t1 ++ "==" ++ show t2
  show (Neg form)    = '~' : (show form)
  show (Impl f1 f2)  = "(" ++ show f1 ++ "==>" 
                           ++ show f2 ++ ")"
  show (Equi f1 f2)  = "(" ++ show f1 ++ "<=>" 
                           ++ show f2 ++ ")"
  show (Conj [])     = "true" 
  show (Conj fs)     = "conj" ++ show fs 
  show (Disj [])     = "false" 
  show (Disj fs)     = "disj" ++ show fs 
  show (Forall v f)  = "A " ++  v ++ (' ' : show f)
  show (Exists v f)  = "E " ++  v ++ (' ' : show f)

r = Atom "R" 

formula1 = Forall "x" (r [x,x])
formula2 = Forall "x" 
            (Forall "y"
              (Impl (r [x,y]) (r [y,x])))

type Rint a = Name -> [a] -> Bool
type Fint a = Name -> [a] -> a

type Lookup a = Name -> a

termVal :: Lookup a -> Fint a -> Term -> a 
termVal g i (V name) = g name
termVal g i (F name ts) = 
   i name (map (termVal g i) ts) 

changeLookup :: Lookup a -> Name -> a -> Lookup a 
changeLookup g v d = \ v' -> 
             if v == v' then d else g v'

evalFOL :: Eq a => 
   [a] -> Lookup a -> Fint a -> Rint a -> Formula -> Bool
evalFOL domain g f i = evalFOL' g where 
  evalFOL' g (Atom name ts) = i name (map (termVal g f) ts)
  evalFOL' g (Eq t1 t2) = termVal g f t1 == termVal g f t2
  evalFOL' g (Neg form) = not (evalFOL' g form)
  evalFOL' g (Impl f1 f2) = not 
                    (evalFOL' g f1 && not (evalFOL' g f2))
  evalFOL' g (Equi f1 f2) = evalFOL' g f1 == evalFOL' g f2 
  evalFOL' g (Conj fs)    = and (map (evalFOL' g) fs)
  evalFOL' g (Disj fs)    = or  (map (evalFOL' g) fs)
  evalFOL' g (Forall v form) = 
    all (\ d -> evalFOL' (changeLookup g v d) form) domain 
  evalFOL' g (Exists v form) = 
    any (\ d -> evalFOL' (changeLookup g v d) form) domain 

f :: Fint Int
f "z" []    = 0 
f "s" [i]   = succ i 
f "p" [i,j] = i + j
f "t" [i,j] = i * j

i :: Rint Int 
i "R" [i,j] = i < j

zero = F "z" []

frm1 = Exists "x" (r [zero,x])
frm2 = Exists "x" (Exists "y" (r [x,y]))
frm3 = Forall "x" (Exists "y" (r [x,y]))

evalFOL1 = evalFOL [0..] (\ v -> 0) f i frm1
evalFOL2 = evalFOL [0..] (\ v -> 0) f i frm2
evalFOL3 = evalFOL [0..] (\ v -> 0) f i frm3
evalFOL4 = evalFOL [0..] (\ v -> 0) f i (Neg frm3)

