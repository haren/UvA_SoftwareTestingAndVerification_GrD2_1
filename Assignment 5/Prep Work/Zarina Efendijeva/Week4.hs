module Week4

where

import Data.List

f :: Int -> Int
f y = f' y 0 0 

f' :: Int -> Int -> Int -> Int 
f' y n x = if n < y then 
             let 
               x' = x + 2*n + 1
               n' = n + 1
             in f' y n' x' 
             else x

fix :: (a -> a) -> a
fix f = f (fix f)

fib :: Integer -> Integer
fib n = fib2 0 1 n

fib2 :: Integer -> Integer -> Integer -> Integer
fib2 = fix (\ f x y n -> 
            if n == 0 then x
            else f y (x+y) (n-1))

run :: Integer -> [Integer]
run n = run1 [n]

run1 ::  [Integer] -> [Integer]
run1 = fix (\ f ns -> 
          let 
            n = head ns 
          in 
            if n == 1 then ns
            else if even n then f (div n 2:ns)
            else f (3*n+1:ns))

g0 :: (Int -> Int -> Int ->Int) 
           -> Int -> Int -> Int -> Int
g0 =  (\ f y n x -> 
           if n < y then 
           let 
             x' = x + 2*n + 1
             n' = n + 1
           in f y n' x' 
           else x)

f0 = fix g0

g x = if even x then 
      let 
        x' = x `div` 2
      in g x'
      else x

g' = fix (\ f x -> 
          if even x then 
          let 
            x' = x `div` 2
          in f x'
          else x)

p x = even x 
h x = x `div` 2

g1 x = if p x then g1 (h x) 
       else x

while1 = until . (not.)

g2 = while1 even (`div` 2)

g3 = while1 (\x -> even x) (\x -> x `div` 2)

g3' = while1 even (`div` 2)

lfp :: Eq a => (a -> a) -> a -> a
lfp f x | x == f x  = x
        | otherwise = lfp f (f x)

lfp' :: Eq a => (a -> a) -> a -> a
lfp' f = while1 (\x -> x /= f x) (\x -> f x)

while2 :: (a -> b -> Bool) 
       -> (a -> b -> (a,b)) 
       -> a -> b -> b
while2 p f x y 
  | p x y     = let (x',y') = f x y in 
                              while2 p f x' y'
  | otherwise = y 

euclidGCD :: Integer -> Integer -> Integer
euclidGCD = while2 
             (\ x y -> x /= y) 
             (\ x y -> if x > y 
                       then (x-y,y) 
                       else (x,y-x))

sqr :: Int -> Int
sqr y = let  
          n = 0 
          x = 0 
        in sqr' y n x 

sqr' y = while2 
          (\ n _ -> n < y) 
          (\ n x -> (n+1, x + 2*n + 1)) 

while3 :: (a -> b -> c -> Bool)
       -> (a -> b -> c -> (a,b,c))
       -> a -> b -> c -> c
while3 p f x y z 
  | p x y z   = let 
                  (x',y',z') = f x y z 
                in while3 p f x' y' z'
  | otherwise = z 

repeat1 :: (a -> a) -> (a -> Bool) -> a -> a
repeat1 f p = while1 (not.p) f . f

repeat2 :: (a -> b -> (a,b)) 
        -> (a -> b -> Bool) -> a -> b -> b
repeat2 f p x y = let 
     (x1,y1) = f x y 
     negp    = (\ x y -> not (p x y))
  in while2 negp f x1 y1

repeat3 :: (a -> b -> c -> (a,b,c)) 
        -> (a -> b -> c -> Bool) -> a -> b -> c -> c
repeat3 f p x y z = let 
     (x1,y1,z1) = f x y z
     negp       = (\ x y z -> not (p x y z))
  in while3 negp f x1 y1 z1

for :: [a] -> (a -> b -> b) -> b -> b
for [] f y = y
for (x:xs) f y = for xs f (f x y)

fact :: Integer -> Integer
fact n = let 
           t = 1 
         in fact' n t 

fact' :: Integer -> Integer -> Integer
fact' n = for [1..n] (\ i t -> i*t) 

factorial :: Integer -> Integer
factorial n = let 
                t = 1 
              in factorial' n t 

factorial' = while2 (\ n _ -> n /= 0) 
                    (\ n t ->  let 
                      t' = n*t
                      n' = n-1
                    in (n',t'))

fct :: Integer -> Integer
fct n = fct0 n 1

fct0 :: Integer -> Integer -> Integer
fct0 n x = if n == 0 then x else fct0 (n-1) (n*x)

fct' :: Integer -> Integer
fct' n = fct0' n 1

fct0' :: Integer -> Integer -> Integer
fct0' = fix (\ f n x -> 
               if n == 0 then x 
               else f (n-1) (n*x))

for2 :: [a] -> (a -> b -> c -> (b,c)) 
           -> b -> c -> c
for2 [] f _ z = z
for2 (x:xs) f y z = let 
    (y',z') = f x y z 
  in 
    for2 xs f y' z'

for3 :: [a] -> (a -> b -> c -> d -> (b,c,d)) 
           -> b -> c -> d -> d 
for3 [] f _ _ u = u
for3 (x:xs) f y z u = let 
    (y',z',u') = f x y z u
  in 
    for3 xs f y' z' u'

fordown :: [a] -> (a -> b -> b) -> b -> b
fordown = for . reverse

fordown2 :: [a] -> (a -> b -> c -> (b,c)) 
                -> b -> c -> c
fordown2 = for2 . reverse

fordown3 :: [a] -> (a -> b -> c -> d -> (b,c,d)) 
                      -> b -> c -> d -> d 
fordown3 = for3 . reverse

