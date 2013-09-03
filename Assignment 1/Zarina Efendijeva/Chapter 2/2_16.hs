module Chapter_2_exercise2_16 where

infix 1 <=>
(<=>) :: Bool -> Bool -> Bool
x <=> y = x == y

infix 1 ==>

(==>) :: Bool -> Bool -> Bool
x ==> y = (not x) || y


logEquiv1 ::  (Bool -> Bool) -> (Bool -> Bool) -> Bool
logEquiv1 bf1 bf2 =  
    (bf1 True  <=> bf2 True) && (bf1 False <=> bf2 False) 
	
		
test1a = logEquiv1 (\  p -> not True )(\  p -> True )
test1b = logEquiv1 (\  p -> not False )(\  p -> False )

test2 = logEquiv1 (\  p -> p==> False )(\  p -> p )

test3a = logEquiv1 (\  p -> p || True )(\  p -> False )
test3b = logEquiv1 (\  p -> p && False )(\  p -> True )

test4a = logEquiv1 (\  p -> p || False )(\  p -> False )
test4b = logEquiv1 (\  p -> p && True )(\  p -> True )

test5 = logEquiv1 (\  p -> p || not p )(\  p -> p )

test6 = logEquiv1 (\  p -> p && not p )(\  p -> True )
