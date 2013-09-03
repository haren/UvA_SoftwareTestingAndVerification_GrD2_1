module Chapter_2 where

infix 1 <+>
(<+>) :: Bool -> Bool -> Bool
(<+>) x y | x == True && y == False = True
(<+>) x y | x==False && y==True = True
(<+>) x y |otherwise = False


infix 1 <=>
(<=>) :: Bool -> Bool -> Bool
x <=> y = x == y



valid2 :: (Bool -> Bool -> Bool) -> Bool
valid2 bf =   (bf True  True)  
           && (bf True  False) 
           && (bf False True) 
           && (bf False False)

exercise2_9 p q = ((p <+> q) <+> q) <=> p

logEquiv1 ::  (Bool -> Bool) -> (Bool -> Bool) -> Bool
logEquiv1 bf1 bf2 =  
    (bf1 True  <=> bf2 True) && (bf1 False <=> bf2 False) 

test = logEquiv1 id (\ p <=> (not p))	