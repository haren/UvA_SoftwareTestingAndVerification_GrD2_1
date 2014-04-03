-- Software verification and testing
-- Lab Assignment 1

-- Group D2_1
-- Cigdem Aytekin 10463135,
-- Jorryt Jan Dijkstra 10462015,
-- Z.E. 10628185, 
-- Lukasz Harezlak, 10630171

-- Chapter 1

-- 1.1 
-- Operators sorted by priority (highest first): ^, * and /, + and -

-- 1.3
divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0

-- 1.4
-- It wouldn't, because k^2 == n => n % k = 0, so the first condition would be met and second -- clause would never be hit by the execution flow.

-- 1.5
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


-- 1.6
-- It takes two Integer arguments and produces an integer argument. The type declaration:
-- rem :: Integer -> Integer -> Integer

-- 1.7
-- Basically a function with multiple parameters recursively results in a new function with a -- parameter (currying)

-- divides 5 :: Integer -> Bool
-- means that divides 5 takes an argument 5 (of type Integer) and produces a result of type -- Integer -> Bool (a procedure that takes an argument of Integer and produces a boolean result).
					
-- divides 5 7 :: Bool
-- means that divides 5 7 takes two arguments (both of type Integer) and produces a result of -- type Bool.

-- 1.9
max' :: [Int] -> Int 
max' [] = error "no maximum definable from an empty list"
max' [x] = x
max' (x:xs) = max x (max' xs)

-- 1.10
removeFst :: [Int] -> Int -> [Int] 
removeFst [] y = []
removeFst (x : xs) y
	| (x == y) = xs
	| otherwise = x : removeFst xs y

-- 1.13
count :: Char -> String -> Int
count c [] = 0
count c (x:xs)
		| (x == c) = 1 + i
		| otherwise = i
         	where i = count c xs

-- 1.14
blowup:: String -> String
blowup xs = blowup2 xs 1
    where blowup2 [] _ = []
          blowup2 (x:xs) n = take n (repeat x) ++ blowup2 xs (n+1)
-- VVZ: the take-repeat combo has a shorter notation: "replicate n x"

-- 1.15
removeFstString :: [String] -> String -> [String]
removeFstString [] y = error "empty list"
removeFstString (x : xs) y
				| (x == y) = xs
				| otherwise = x : (removeFstString xs y)
 
srtString :: [String] -> [String]
srtString [] = []
srtString [x] = [x]
srtString (xs) =
  		m : (srtString (removeFstString xs m))
  		where m = minimum xs

-- 1.17
substring :: String -> String -> Bool
-- VVZ: no need to name unneeded arguments
substring [] ys = True
-- VVZ: patterns are checked in the order they are written, so you know that the first argument won't be empty, no need to write it out
substring (x:xs) [] = False
-- VVZ: the same name in different places can be reused to mean different things
substring xs (y:ys') | (prefix xs (y:ys')) == True = True
		| (substring xs ys' == True) = True
		| otherwise = False
-- VVZ: when you check for truth, you can just leave it as it is: for all boolean p, "(p == True)" is the same as just "p"
-- VVZ: the last function could have been just:
substring' [] _ = True
substring' _ [] = False
substring' xs (y:ys) = prefix xs (y:ys) || substring xs ys

prefix :: String -> String -> Bool
prefix [] ys = True 
prefix (x:xs) [] = False 
prefix (x:xs) (y:ys) = (x==y) && prefix xs ys 

-- 1.19
-- head :: [a] -> a 
-- last :: [a] -> a 
-- init :: [a] -> [a]
-- fst :: (a,b) -> a
-- (++) :: [a] -> [a] -> [a]
-- flip :: (a -> b -> c) -> b -> a -> c
-- flip (++) :: [a] -> [a] -> [a]

-- 1.20
-- haskell in all its beauty
lengths = map length

-- 1.21
sumLengths = sum . map length

-- 1.24

-- It does not affect the behaviour of the function at all but is a shorter writing (writing a -- parameter that is passed on is redundant)

ldp :: Integer -> Integer 
ldp = ldpf primes1 

ldpf :: [Integer] -> Integer -> Integer 
ldpf (p:ps) n   | rem n p == 0 = p 
		| p^2 > n = n 
		| otherwise = ldpf ps n

primes1 :: [Integer] 
primes1 = 2 : filter prime[3..] 

prime :: Integer -> Bool 
prime n	| n<1 = error "not a positivei nteger" 
	| n==1 = False 
	| otherwise = ldp n==n