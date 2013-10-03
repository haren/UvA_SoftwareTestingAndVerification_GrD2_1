module AnswerCnf

where


import Data.List
import Data.Char
import Week2


-- arrowfree :: Form -> Form 
-- arrowfree (Prop x) = Prop x 
-- arrowfree (Neg f) = Neg (arrowfree f)
-- arrowfree (Cnj fs) = Cnj (map arrowfree fs)
-- arrowfree (Dsj fs) = Dsj (map arrowfree fs)
-- arrowfree (Impl f1 f2) = 
--  Dsj [Neg (arrowfree f1), arrowfree f2]
--arrowfree (Equiv f1 f2) = 
--  Dsj [Cnj [f1', f2'], Cnj [Neg f1', Neg f2']]
--  where f1' = arrowfree f1
--        f2' = arrowfree f2
        
cnf :: Form -> Form
cnf (Prop x) = Prop x
cnf (Cnj fs) = Cnj (map cnf fs)
cnf (Dsj fs) = distList (map cnf fs)
cnf f = f


distList :: [Form] -> Form
distList [f] = f
distList (f:fs) = dist f (distList fs)
 

dist :: Form -> Form -> Form
dist (Cnj [f11, f12])  f2  = 
                Cnj [(dist f11 f2), (dist f12 f2)]
dist f1 (Cnj [f21, f22]) = 
                Cnj [ (dist f1 f21), (dist f1 f22)]
dist f1 f2 = Dsj [f1, f2]


testCnf :: Form -> Form
testCnf f =  cnf (nnf (arrowfree f))
 






 
 
 
 
 
 
 
 
 