infixr 2 <+> 
(<+>) :: Bool -> Bool -> Bool 
x <+> y = x /= y


//that proves the correctness
*Main> :t (<+>)
(<+>) :: Bool -> Bool -> Bool
*Main> True <+> False
True
*Main> True <+> True
False
*Main> False <+> False
False
*Main> False <+> True
True