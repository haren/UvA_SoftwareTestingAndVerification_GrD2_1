module Chapter_2_exercise2_13 where


infix 1 <=>
(<=>) :: Bool -> Bool -> Bool
x <=> y = x == y

infix 1 ==>

(==>) :: Bool -> Bool -> Bool
x ==> y = (not x) || y


logEquiv1 ::  (Bool -> Bool) -> (Bool -> Bool) -> Bool
logEquiv1 bf1 bf2 =  
    (bf1 True  <=> bf2 True) && (bf1 False <=> bf2 False) 
	
		
exercise2_13_1a = logEquiv1 (\  p -> not True )(\  p -> False )
exercise2_13_1b = logEquiv1 (\  p -> not False )(\  p -> True )

exercise2_13_2 = logEquiv1 (\  p -> p==> False )(\  p -> not p )

exercise2_13_3a = logEquiv1 (\  p -> p || True )(\  p -> True )
exercise2_13_3b = logEquiv1 (\  p -> p && False )(\  p -> False )

exercise2_13_4a = logEquiv1 (\  p -> p || False )(\  p -> p )
exercise2_13_4b = logEquiv1 (\  p -> p && True )(\  p -> p )

exercise2_13_5 = logEquiv1 (\  p -> p || not p )(\  p -> True )

exercise2_13_6 = logEquiv1 (\  p -> p && not p )(\  p -> False )
