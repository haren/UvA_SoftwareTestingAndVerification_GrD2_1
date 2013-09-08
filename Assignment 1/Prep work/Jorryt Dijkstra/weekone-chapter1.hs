import TAMO
exercise1_1 a = 2^3 * 3 + 5 - 1 + 4 / 2 * a

-- Exercise1_2
divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0
ld :: Integer -> Integer
ld n = ldf 2 n

-- Equation guarding
ldf :: Integer -> Integer -> Integer
ldf k n | divides k n = k
		| k^2 >= n = n
		| otherwise = ldf (k+1) n

{--
Exercise 1.4:
	This would save one extra recursive function call in some cases, but the correctness (or meaning as stated in the exercise) of the program is maintained
--}

-- Exercise 1.5
prime0 :: Integer -> Bool
prime0 n | n < 1 = error "not a positive integer"
		 | n == 1 = False
		 | otherwise = ld n == n

{--
Exercise 1.6
Integer -> Integer -> Integer
Remainder only works on integer (according to the mathematical specification) and it takes two arguments that result in an integer (the actual remainder)
--}

{--
Exercise 1.7
Basically a function with multiple parameters recursively results in a new function with a parameter (currying), this is quite obvious from the example given in this exercise:

-	:t divides 5
	divides 5 :: Integer -> Bool
	divides 5 needs one more argument and will result in a bool

-	:t divides 5 7
	divides 5 7 :: Bool
	the composed function will result in a bool
--}

-- Exercise 1.9
max' :: [Int] -> Int
max' [] = error "no maximum definable from an empty list"
max' [x] = x
max' (x:xs) = max x (max' xs)

min' :: [Int] -> Int
min' [] = error "no minimum definable from an empty list"
min' [x] = x
min' (x:xs) = min x (min' xs)

-- Exercise 1.10
removeFst :: [Int] -> Int -> [Int]
removeFst [] y = error "empty list"
removeFst (x : xs) y
	| (x == y) = xs
	| otherwise = x : removeFst xs y

-- Exercise 1.13
count :: Char -> String -> Int
count c [] = 0
count c (x:xs)
		| (x == c) = 1 + i
		| otherwise = i
		where i = count c xs

-- Exercise 1.14
blowup :: String -> String
blowup [] = []
blowup xs = blowup' xs 0

blowup' :: String -> Int -> String
blowup' [] _ = [] -- TODO without nr parameter
blowup' (x:xs) n = (take (n+1) (repeat x)) ++ blowup' xs (n+1)


removeFstString :: [String] -> String -> [String]
removeFstString [] y = error "empty list"
removeFstString (x : xs) y 
			| (x == y) = xs
			| otherwise = x : (removeFstString xs y)

-- Exercise 1.15 
srtString :: [String] -> [String]
srtString [] = []
srtString [x] = [x]
srtString (xs) =
	m : (srtString (removeFstString xs m))
	where m = minimum xs

-- Exercise 1.17
checkSubstring :: String -> String -> Bool
checkSubstring [] y = True -- Logic, a set always contains an empty set
checkSubstring (x:xs) [] = False
checkSubstring [x] [y] = (x == y)
checkSubstring (x:xs) (y:ys)
		| (x == y && ys == xs) = True
		| (x == y) = checkSubstring xs ys	-- Check the subsequent chars
		| otherwise = checkSubstring (x:xs) ys

-- Exercise 1.18
{--
	[String] expressions: lines, words, unlines, unwords
	(Bool, String): ?
	[(Bool,String)]: ?
	([Bool],String): ?

	TODO
--}

-- Exercise 1.19
{--
	head :: [a] -> a: takes the first element of an array
	last :: [a] -> a: takes the last element of an array
	init :: [a] -> [a]: cuts the last element off of an array
	fst :: (a, b) -> a : returns the first item in a tuple
	(++) :: [a] -> [a] -> [a]: combines two lists
	flip :: (a -> b -> c) -> b -> a -> c: reverse parameter order of a function
	flip (++) :: [a] -> [a] -> [a]: combine lists in reverse
--}

-- prime factorization
factors :: Integer -> [Integer]
factors n
	| n < 1 = error "argument not positive" 
	| n == 1 = []
	| otherwise = p : factors (div n p) where p = ld n

-- Exercise 1.20
listLengths :: [[a]] -> [Int]
listLengths [] = [0]
listLengths xs = map (length) xs

-- Exercise 1.21
sumLengths :: [[a]] -> Int
sumLengths [] = 0
sumLengths xs = sum (listLengths xs)

ldp :: Integer -> Integer
ldp = ldpf primes1
ldpf :: [Integer] -> Integer -> Integer
ldpf (p:ps) n | rem n p == 0 = p
	| p^2 > n = n
	| otherwise = ldpf ps n

primes0 :: [Integer]
primes0 = filter prime0 [2..]

primes1 :: [Integer]
primes1 = 2 : filter prime [3..]

prime :: Integer -> Bool
prime n | n < 1 = error "not a positive integer"
		| n == 1 = False
		| otherwise = ldp n == n

-- Exercise 1.24
{--
	It does not affect the behaviour of the function at all but is a shorter writing (writing a parameter that is passed on is redundant)
--}




