
module Module3 where
import Week2


cnf :: Form -> Form
cnf x
	| x == (Prop p) 	= Prop p
	| x == (Cnj [p, q]) = Cnj [cnf p, cnf q]
	| x == (Cnj [p, q]) = Cnj [cnf p, cnf q]
	| otherwise 		= error "This is an error!"


dist :: Form -> Form -> Form
dist x y
	| (x == Cnj [p, q]) = Cnj [dist p y, dist q y]
	| (y == Cnj [r, s]) = Cnj [dist x r, dist x s]
	| otherwise = Dsj [x, y]
	

