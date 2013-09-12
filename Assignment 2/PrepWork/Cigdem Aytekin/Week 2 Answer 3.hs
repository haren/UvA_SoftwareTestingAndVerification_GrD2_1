module AssignmentCNF where

import Week2

cnf :: Form -> Form
cnf (Prop x) 		= Prop x
cnf (Cnj fs) 		= Cnj (map cnf fs)
cnf (Dsj f) 		= dist fh (distList ft)  
					where
					 	  fh = head f		
						  ft = tail f

distList :: [Form] -> Form 
distList [f1, f2]		= dist f1 f2
distList (fh:ft)		= dist fh (distList ft) 



dist :: Form -> Form -> Form
dist f (Cnj g) = Cnj [dist f gh, dist f gs]
					where
					 	  gh = head g		
						  gs = last g
dist (Cnj f) g = Cnj [dist fh g, dist ft g]
					where
					 	  fh = head f		
						  ft = last f
dist f g 	  = Dsj [f, g]
	 

