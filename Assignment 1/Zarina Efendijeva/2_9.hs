module Chapter_2_exercise2_9 where

infix 1 <+>
(<+>) :: Bool -> Bool -> Bool
(<+>) True False = True
(<+>) False True = True
(<+>) _ _ = False


infix 1 <=>
(<=>) :: Bool -> Bool -> Bool
x <=> y = x == y

valid2 :: (Bool -> Bool -> Bool) -> Bool
valid2 bf =   (bf True  True)  
           && (bf True  False) 
           && (bf False True) 
           && (bf False False)

exercise2_9 p q = ((p <+> q) <+> q) <=> p
