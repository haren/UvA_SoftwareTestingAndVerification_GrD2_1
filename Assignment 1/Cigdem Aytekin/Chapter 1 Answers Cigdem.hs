
{- 	The answers of the exercises of Chapter 1 of the book
	the Haskell Road to Logic, Maths and Programming
	Cigdem Aytekin, student nr.: 104 63 135, Date: 6-9-2013
	Group: GR_D2_1
-}



{- Excercise 1.4: 	It will not make any difference to the meaning, it will still compute the same, 
					because it will still compute to n, the same value.
-}

divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0



ldf :: Integer -> Integer -> Integer
ldf k n 	| divides k n 	= k
			| k^2 > n 	= n
			| otherwise 	= ldf (k+1) n

ld :: Integer -> Integer
ld n = ldf 2 n

prime0 :: Integer -> Bool
prime0 n 	| n < 1		= error "not a positive integer" 
			| n == 1	= False
			| otherwise	= ld n == n

{- Excercise 1.6: 	It takes two Integer arguments and produces an integer argument. The type declaration:
					rem :: Integer -> Integer -> Integer

-}

{- Excercise 1.7:	divides 5 :: Integer -> Bool
					means that divides 5 takes an argument 5 (of type Integer) and produces a result of type 
					Integer -> Bool (a procedure that takes an argument of Integer and produces a boolean 
					result).
					
					divides 5 7 :: Bool
					means that divides 5 7 takes two arguments (both of type Integer) and produces a result of 
					type Bool.

-} 

maxInt :: [Int] -> Int
maxInt [] 		= error "List is empty!"
maxInt[x] 		= x
maxInt (x:xs) 	= max x (maxInt xs)

			
removeFst :: Int -> [Int] -> [Int]
removeFst m [] 		= error "List is empty!"
removeFst m [x] 	| x == m		= []
					| otherwise		= [x]
removeFst m (x:xs) 	|  x == m		= xs
					|  otherwise 	= [x] ++ removeFst m xs

					
mnmInt :: [Int] -> Int
mnmInt[] 		= error "List is empty!"
mnmInt[x]		= x
mnmInt (x:xs)	= min x (mnmInt xs)

srtInts 	:: [Int] ->	[Int]
srtInts [] 		= []
srtInts xs 		= m : (srtInts (removeFst m xs)) where m = mnmInt xs

srtInts2	::	[Int] -> [Int]
srtInts2 []		= []
srtInts2 xs		= let
					m = mnmInt xs
					in m: (srtInts2 (removeFst m xs))
				
sum2 :: [Int] -> Int
sum2 [] 	= 0
sum2 (x:xs)	= x + sum (xs)

length2 :: [a]	-> Int
length2 []		= 0
length2	(x:xs)	= 1 + length2 xs	


average	::	[Integer] -> Float
average []	= error "List is empty!"
average xs = fromInteger (sum xs) / fromIntegral (length xs)



count 	:: Char -> String -> Int
count a [] 		= 0
count a [x]		|	a == x		= 1
				|	otherwise 	= 0
count a (x:xs) 	= 	count a [x]	+	count a xs


{- This is the exercise which I couldn't make it work, I'll work on it...
-}	
blowup	::	String -> String
blowup	[]		= []
blowup	[x]		= [x]
blowup (x:xs)	= [x] ++ blowup xs

 	
				
removeFstString :: String -> [String] -> [String]
removeFstString m [] 		= error "List is empty!"
removeFstString m [x] 	| x == m		= []
						| otherwise		= [x]
removeFstString 	m (x:xs) 	|  x == m		= xs
								|  otherwise 	= [x] ++ removeFstString m xs

minStr :: String -> String -> String
minStr s1 s2	|	s1 < s2	= s1
				| 	otherwise	= s2
				
					
mnmString :: [String] -> String
mnmString[] 		= error "List is empty!"
mnmString[x]		= x
mnmString (x:xs)	= minStr x (mnmString xs)

srtStrings 	:: [String] ->	[String]
srtStrings [] 		= []
srtStrings xs 		= m : (srtStrings (removeFstString m xs)) 				where m = mnmString xs
					 

prefix :: String -> String -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x==y) && prefix xs ys

substring :: String -> String -> Bool
substring [] ys = True
substring (x:xs)[] = False
substring xs (y:ys') 	| prefix xs (y:ys') = True
						| otherwise 		=  substring xs ys'


{-
Excercise 1.18
	1. ["Cigdem", "Aytekin","Liselot"]
	2. (True, "Cigdem")
	3. [ (False, "Cigdem) , (True, "Aytekin") ]
	4. ( [True, 1 < 3, False, 2 < 3], "Cigdem")
	5. Any function of type Bool -> Bool
-}

{-
Excercise 1.19
	1. head :: [a] -> a
	2. last :: [a] -> a
	3. init :: [a] -> [a]
	4. fst :: (a, b) -> a
	5. (++) :: [a] -> [a] -> [a]
	6.flip :: (a -> b -> c) -> b -> a -> c
	7.flip (++) :: [a] -> [a] -> [a]
-}

factors :: Integer -> [Integer]

factors n 	| n < 1		= error "argument not poistive"
			| n == 1	= []
			| otherwise = p : factors (div n p) where p = ld n
			
lengths :: [[a]] -> [Int]
lengths xs	= 	map length xs

sumLengths :: [[a]] -> Int
sumLengths xs = sum (lengths xs)

primes1 :: [Integer] 
primes1 = 2 : filter prime [3..]

ldp :: Integer -> Integer
ldp n = ldpf primes1 n 

ldpf :: [Integer] -> Integer -> Integer
ldpf (p:ps) n	| rem n p == 0	= p
				| p^2 > n		= n
				| otherwise		= ldpf ps n

prime :: Integer -> Bool
prime n 	| n < 1 	= error "n is negative!"
			| n == 1	= False
			| otherwise = ldp n == n
			
{- Excercise 1.24
	It does not compile. It complains about too few arguments
-}

