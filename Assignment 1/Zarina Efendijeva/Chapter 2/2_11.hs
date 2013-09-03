module Chapter2_exercise2_11 where

infix 1 ==>

(==>) :: Bool -> Bool -> Bool
x ==> y = (not x) || y

infix 1 <=>

(<=>) :: Bool -> Bool -> Bool
x <=> y = x == y 


logEquiv1 ::  (Bool -> Bool) -> (Bool -> Bool) -> Bool
logEquiv1 bf1 bf2 =  
    (bf1 True  <=> bf2 True) && (bf1 False <=> bf2 False) 

logEquiv2 :: (Bool -> Bool -> Bool) -> 
                    (Bool -> Bool -> Bool) -> Bool
logEquiv2 bf1 bf2 = 
  and [(bf1 p q) <=> (bf2 p q)  |  p <- [True,False], 
                                   q <- [True,False]]

logEquiv3 :: (Bool -> Bool -> Bool -> Bool) ->
                 (Bool -> Bool -> Bool -> Bool) -> Bool
logEquiv3 bf1 bf2 = 
  and [(bf1 p q r) <=> (bf2 p q r) |  p <- [True,False], 
                                      q <- [True,False], 
                                      r <- [True,False]] 


test1  = logEquiv1 id (\ p -> not (not p))
test2a = logEquiv1 id (\ p -> p && p) 
test2b = logEquiv1 id (\ p -> p || p) 
test3a = logEquiv2 (\ p q -> p ==> q) (\ p q -> not p || q)
test3b = logEquiv2 (\ p q -> not (p ==> q)) (\ p q -> p && not q)
test4a = logEquiv2 (\ p q -> not p ==> not q) (\ p q -> q ==> p)
test4b = logEquiv2 (\ p q -> p ==> not q) (\ p q -> q ==> not p)
test4c = logEquiv2 (\ p q -> not p ==> q) (\ p q -> not q ==> p)
test5a = logEquiv2 (\ p q -> p <=> q) 
                   (\ p q -> (p ==> q) && (q ==> p))
test5b = logEquiv2 (\ p q -> p <=> q) 
                   (\ p q -> (p && q) || (not p && not q))
test6a = logEquiv2 (\ p q -> p && q) (\ p q -> q && p)
test6b = logEquiv2 (\ p q -> p || q) (\ p q -> q || p)
test7a = logEquiv2 (\ p q -> not (p && q)) 
                   (\ p q -> not p || not q)
test7b = logEquiv2 (\ p q -> not (p || q)) 
                   (\ p q -> not p && not q)
test8a = logEquiv3 (\ p q r -> p && (q && r)) 
                   (\ p q r -> (p && q) && r)
test8b = logEquiv3 (\ p q r -> p || (q || r)) 
                   (\ p q r -> (p || q) || r)
test9a = logEquiv3 (\ p q r -> p && (q || r)) 
                   (\ p q r -> (p && q) || (p && r))
test9b = logEquiv3 (\ p q r ->  p || (q && r)) 
                   (\ p q r -> (p || q) && (p || r))

