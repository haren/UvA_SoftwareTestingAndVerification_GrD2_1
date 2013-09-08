module Chapter_2_exercise2_15 where

infix 1 <=>
(<=>) :: Bool -> Bool -> Bool
x <=> y = x == y

infix 1 ==>

(==>) :: Bool -> Bool -> Bool
x ==> y = (not x) || y


contraCheck ::  (Bool -> Bool) -> (Bool -> Bool) -> Bool
contraCheck bf1 bf2 = (bf1 True  <=> bf2 False) && (bf1 False <=> bf2 True) 

contraCheck2 :: (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> Bool
contraCheck2 bf1 bf2 = 
  and [(bf1 p q) <=> (bf2 p q)  |  p <- [True,False], 
                                   q <- [False,True]]
	
contraCheck3 :: (Bool -> Bool -> Bool -> Bool) ->
                 (Bool -> Bool -> Bool -> Bool) -> Bool
contraCheck3 bf1 bf2 = 
  and [(bf1 p q r) <=> (bf2 p q r) |  p <- [True,False], 
                                      q <- [True,False], 
                                      r <- [True,False]] 

									  
test1 = contraCheck (\  p -> p && not p  )(\  p -> False )
test2 = contraCheck2 (\ p q -> p && (q && not q))(\ p q  -> False) 
test3 = contraCheck3 (\ p q r -> ((p && r) || (q && not r)) <=>((not p && r) || (not q && not r )))(\ p q r -> False) 


