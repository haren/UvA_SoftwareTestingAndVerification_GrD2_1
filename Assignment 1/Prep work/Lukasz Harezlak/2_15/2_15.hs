(<=>) :: Bool -> Bool -> Bool
x <=> y = x == y

(==>) :: Bool -> Bool -> Bool
x ==> y = (not x) || y

contra ::  (Bool -> Bool) -> (Bool -> Bool) -> Bool
contra bf1 bf2 = (bf1 True <=> bf2 False) && (bf1 False <=> bf2 True) 

contra2 :: (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> Bool
contra2 bf1 bf2 = 
  and [(bf1 p q) <=> (bf2 p q)  |  p <- [True,False], 
                                   q <- [False,True]]
	
contra3 :: (Bool -> Bool -> Bool -> Bool) ->
                 (Bool -> Bool -> Bool -> Bool) -> Bool
contra3 bf1 bf2 = 
  and [(bf1 p q r) <=> (bf2 p q r) |  p <- [True,False], 
                                      q <- [True,False], 
                                      r <- [True,False]] 

									  
test1 = contra (\  p -> p && not p  )(\  p -> False )
test2 = contra2 (\ p q -> p && (q && not q))(\ p q  -> False) 
test3 = contra3 (\ p q r -> ((p && r) || (q && not r)) <=>((not p && r) || (not q && not r )))(\ p q r -> False) 


//based on zafinas