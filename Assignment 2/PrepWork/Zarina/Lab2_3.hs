module Lab2_3 where
import Week2

cnf :: Form -> Form 
cnf (Prop x) = Prop x
cnf (Neg (Prop x)) = Neg (Prop x)
cnf (Cnj fs) = Cnj (map cnf fs)

cnf (Dsj f1:f2:fs) = dist (cnf f1) (cnf (Dsj(f2:fs)))

dist :: Form -> Form -> Form 
dist x y  
	| (x == Cnj [p, q]) = Cnj[dist p y, dist q y] -- based on Jorrits work
	| (y == Cnj [p, q]) = Cnj[dist x p, dist x q] -- based on Jorrits work
	| otherwise = Dsj[x, y]

