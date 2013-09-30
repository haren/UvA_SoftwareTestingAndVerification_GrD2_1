module Week5

where

import Data.List
import Week4 

pre1 :: (a -> Bool) -> (a -> b) -> a -> b 
pre1 p f x = if p x then f x 
             else error "pre1"

post1 :: (b -> Bool) -> (a -> b) -> a -> b 
post1 p f x = if p (f x) then f x 
              else error "post1"

decomp :: Integer -> (Integer,Integer)
decomp n = decomp' (0,n) where
  decomp' = while1 (\ (_,m) -> even m)
                   (\ (k,m) -> (k+1,m `div` 2))

decompPost = post1 (\ (_,m) -> odd m) decomp

assert1 :: (a -> b -> Bool) -> (a -> b) -> a -> b 
assert1 p f x = if p x (f x) then f x 
                else error "assert1"

decompA = assert1 (\ n (k,m) -> n == 2^k*m) decomp

invar1 :: (a -> Bool) -> (a -> a) -> a -> a
invar1 p f x = 
  let 
    x' = f x 
  in
  if p x && not (p x') then error "invar1"
  else x'

gSign = invar1 (>0) (while1 even (`div` 2))

gSign' = invar1 (<0) (while1 even (`div` 2))

decompInvar :: Integer -> (Integer,Integer)
decompInvar n = decomp' (0,n) where
  decomp' = while1 (\ (_,m) -> even m)
                   (invar1 
                     (\ (i,j) -> 2^i*j == n)
                     (\ (k,m) -> (k+1,m `div` 2)))

infix 1 ==> 

(==>) :: Bool -> Bool -> Bool
p ==> q = (not p) || q

sortedProp :: Ord a => [a] -> [a] -> [a] -> Bool
sortedProp xs ys zs = 
  (sorted xs && sorted ys) ==> sorted zs

sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [_] = True 
sorted (x:y:zs) = x <= y && sorted (y:zs)

sublistProp :: Eq a => [a] -> [a] -> [a] -> Bool
sublistProp xs ys zs = 
  sublist xs zs && sublist ys zs

sublist :: Eq a => [a] -> [a] -> Bool
sublist [] _ = True
sublist (x:xs) ys = 
  elem x ys && sublist xs (ys \\ [x])

assert2 ::  (a -> b -> c -> Bool) 
             -> (a -> b -> c) -> a -> b -> c
assert2 p f x y = 
  if p x y (f x y) then f x y
  else error "assert2"

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = if x <= y 
                         then x : merge xs (y:ys) 
                         else y : merge (x:xs) ys

mergeA :: Ord a => [a] -> [a] -> [a]
mergeA = assert2 sortedProp 
            $ assert2 sublistProp merge

ext_gcd :: Integer -> Integer -> (Integer,Integer) 
ext_gcd a b = let 
   x = 0
   y = 1
   lastx = 1
   lasty = 0
  in ext_gcd' a b x y (lastx,lasty) 

ext_gcd' = while5 (\ _ b _ _ _ ->  b /= 0) 
                  (\ a b x y (lastx,lasty) -> let 
                    (q,r)   = quotRem a b 
                    (x',lastx') = (lastx-q*x,x)
                    (y',lasty') = (lasty-q*y,y)
                 in (b,r,x',y',(lastx',lasty')))

while5 :: (a -> b -> c -> d -> e -> Bool)
       -> (a -> b -> c -> d -> e -> (a,b,c,d,e))
       -> a -> b -> c -> d -> e -> e   
while5 p f x y z v w
  | p x y z v w = let 
                    (x',y',z',v',w') = f x y z v w
                  in while5 p f x' y' z' v' w'
  | otherwise = w

bezout :: Integer -> Integer 
          -> (Integer,Integer) -> Bool
bezout m n (x,y) = x*m + y*n == gcd m n 

ext_gcdA = assert2 bezout ext_gcd

fct_gcd :: Integer -> Integer -> (Integer,Integer) 
fct_gcd a b = 
  if b == 0 
  then (1,0) 
  else 
     let 
       (q,r) = quotRem a b
       (s,t) = fct_gcd b r 
     in (t, s - q*t)

divides :: Integer -> Integer -> Bool
divides n m = rem m n == 0

gcd_property :: Integer -> Integer -> (Integer,Integer) -> Bool
gcd_property = \ m n (x,y) -> let 
    d = x*m + y*n 
  in 
    divides d m && divides d n 

fct_gcdA = assert2 gcd_property fct_gcd

assert3 :: (a -> b -> c -> d -> Bool) -> 
           (a -> b -> c -> d) -> 
            a -> b -> c -> d
assert3 p f x y z = 
  if p x y z (f x y z) then f x y z
  else error "assert3"

invar3 :: (a -> b -> c -> Bool) -> 
          (a -> b -> c -> (a,b,c)) -> 
           a -> b -> c -> (a,b,c)
invar3 p f x y z = 
  let 
    (x',y',z') = f x y z
  in 
   if p x y z && not (p x' y' z') then error "invar3"
   else (x',y',z')

assert4 ::  (a -> b -> c -> d -> e -> Bool) 
             -> (a -> b -> c -> d -> e) 
             -> a -> b -> c -> d -> e
assert4 p f x y z u = 
  if p x y z u (f x y z u) then f x y z u
  else error "assert4"

invar4 :: (a -> b -> c -> d -> Bool) -> 
          (a -> b -> c -> d -> (a,b,c,d)) -> 
           a -> b -> c -> d -> (a,b,c,d)
invar4 p f x y z u = 
  let 
    (x',y',z',u') = f x y z u
  in 
   if p x y z u && not (p x' y' z' u')
   then error "invar4"
   else (x',y',z',u')

assert5 ::  (a -> b -> c -> d -> e -> f -> Bool) 
             -> (a -> b -> c -> d -> e -> f) 
             -> a -> b -> c -> d -> e -> f
assert5 p f x y z u v = 
  if p x y z u v (f x y z u v) then f x y z u v
  else error "assert5"

invar5 :: (a -> b -> c -> d -> e -> Bool) -> 
          (a -> b -> c -> d -> e -> (a,b,c,d,e)) -> 
           a -> b -> c -> d -> e -> (a,b,c,d,e)
invar5 p f x y z u v = 
  let 
    (x',y',z',u',v') = f x y z u v
  in 
   if p x y z u v && not (p x' y' z' u' v')
   then error "invar5"
   else (x',y',z',u',v')

type Row    = Int 
type Column = Int 
type Value  = Int
type Grid   = [[Value]]

positions, values :: [Int]
positions = [1..9]
values    = [1..9] 

blocks :: [[Int]]
blocks = [[1..3],[4..6],[7..9]]

showDgt :: Value -> String
showDgt 0 = " "
showDgt d = show d

showRow :: [Value] -> IO()
showRow [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
 do  putChar '|'         ; putChar ' '
     putStr (showDgt a1) ; putChar ' '
     putStr (showDgt a2) ; putChar ' '
     putStr (showDgt a3) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showDgt a4) ; putChar ' '
     putStr (showDgt a5) ; putChar ' '
     putStr (showDgt a6) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showDgt a7) ; putChar ' '
     putStr (showDgt a8) ; putChar ' '
     putStr (showDgt a9) ; putChar ' '
     putChar '|'         ; putChar '\n'

showGrid :: Grid -> IO()
showGrid [as,bs,cs,ds,es,fs,gs,hs,is] =
 do putStrLn ("+-------+-------+-------+")
    showRow as; showRow bs; showRow cs
    putStrLn ("+-------+-------+-------+")
    showRow ds; showRow es; showRow fs
    putStrLn ("+-------+-------+-------+")
    showRow gs; showRow hs; showRow is
    putStrLn ("+-------+-------+-------+")

type Sudoku = (Row,Column) -> Value

sud2grid :: Sudoku -> Grid
sud2grid s = 
  [ [ s (r,c) | c <- [1..9] ] | r <- [1..9] ] 

grid2sud :: Grid -> Sudoku
grid2sud gr = \ (r,c) -> pos gr (r,c) 
  where 
  pos :: [[a]] -> (Row,Column) -> a 
  pos gr (r,c) = (gr !! (r-1)) !! (c-1)

showSudoku :: Sudoku -> IO()
showSudoku = showGrid . sud2grid

bl :: Int -> [Int]
bl x = concat $ filter (elem x) blocks 

subGrid :: Sudoku -> (Row,Column) -> [Value]
subGrid s (r,c) = 
  [ s (r',c') | r' <- bl r, c' <- bl c ]

freeInSeq :: [Value] -> [Value]
freeInSeq seq = values \\ seq 

freeInRow :: Sudoku -> Row -> [Value]
freeInRow s r = 
  freeInSeq [ s (r,i) | i <- positions  ]

freeInColumn :: Sudoku -> Column -> [Value]
freeInColumn s c = 
  freeInSeq [ s (i,c) | i <- positions ]

freeInSubgrid :: Sudoku -> (Row,Column) -> [Value]
freeInSubgrid s (r,c) = freeInSeq (subGrid s (r,c))

freeAtPos :: Sudoku -> (Row,Column) -> [Value]
freeAtPos s (r,c) = 
  (freeInRow s r) 
   `intersect` (freeInColumn s c) 
   `intersect` (freeInSubgrid s (r,c)) 

injective :: Eq a => [a] -> Bool
injective xs = nub xs == xs

rowInjective :: Sudoku -> Row -> Bool
rowInjective s r = injective vs where 
   vs = filter (/= 0) [ s (r,i) | i <- positions ]

colInjective :: Sudoku -> Column -> Bool
colInjective s c = injective vs where 
   vs = filter (/= 0) [ s (i,c) | i <- positions ]

subgridInjective :: Sudoku -> (Row,Column) -> Bool
subgridInjective s (r,c) = injective vs where 
   vs = filter (/= 0) (subGrid s (r,c))

consistent :: Sudoku -> Bool
consistent s = and $
               [ rowInjective s r |  r <- positions ]
                ++
               [ colInjective s c |  c <- positions ]
                ++
               [ subgridInjective s (r,c) | 
                    r <- [1,4,7], c <- [1,4,7]]

extend :: Sudoku -> (Row,Column,Value) -> Sudoku
extend s (r,c,v) (i,j) | (i,j) == (r,c) = v
                       | otherwise      = s (i,j)

type Constraint = (Row,Column,[Value])

type Node = (Sudoku,[Constraint])

showNode :: Node -> IO()
showNode = showSudoku . fst

solved  :: Node -> Bool
solved = null . snd

extendNode :: Node -> Constraint -> [Node]
extendNode (s,constraints) (r,c,vs) = 
   [(extend s (r,c,v),
     sortBy length3rd $ 
         prune (r,c,v) constraints) | v <- vs ]

length3rd :: (a,b,[c]) -> (a,b,[c]) -> Ordering
length3rd (_,_,zs) (_,_,zs') = 
  compare (length zs) (length zs')

prune :: (Row,Column,Value) 
      -> [Constraint] -> [Constraint]
prune _ [] = []
prune (r,c,v) ((x,y,zs):rest)
  | r == x = (x,y,zs\\[v]) : prune (r,c,v) rest
  | c == y = (x,y,zs\\[v]) : prune (r,c,v) rest
  | sameblock (r,c) (x,y) = 
        (x,y,zs\\[v]) : prune (r,c,v) rest
  | otherwise = (x,y,zs) : prune (r,c,v) rest

sameblock :: (Row,Column) -> (Row,Column) -> Bool
sameblock (r,c) (x,y) = bl r == bl x && bl c == bl y 

initNode :: Grid -> [Node]
initNode gr = let s = grid2sud gr in 
              if (not . consistent) s then [] 
              else [(s, constraints s)]

openPositions :: Sudoku -> [(Row,Column)]
openPositions s = [ (r,c) | r <- positions,  
                            c <- positions, 
                            s (r,c) == 0 ]

constraints :: Sudoku -> [Constraint] 
constraints s = sortBy length3rd 
    [(r,c, freeAtPos s (r,c)) | 
                       (r,c) <- openPositions s ]

search :: (node -> [node]) 
       -> (node -> Bool) -> [node] -> [node]
search succ goal [] = []
search succ goal (x:xs) 
  | goal x    = x : search succ goal xs
  | otherwise = search succ goal ((succ x) ++ xs)

solveNs :: [Node] -> [Node]
solveNs = search succNode solved 

succNode :: Node -> [Node]
succNode (s,[]) = []
succNode (s,p:ps) = extendNode (s,ps) p 

solveAndShow :: Grid -> IO[()]
solveAndShow gr = solveShowNs (initNode gr)

solveShowNs :: [Node] -> IO[()]
solveShowNs ns = sequence $ fmap showNode (solveNs ns)

example1 :: Grid
example1 = [[5,3,0,0,7,0,0,0,0],
            [6,0,0,1,9,5,0,0,0],
            [0,9,8,0,0,0,0,6,0],
            [8,0,0,0,6,0,0,0,3],
            [4,0,0,8,0,3,0,0,1],
            [7,0,0,0,2,0,0,0,6],
            [0,6,0,0,0,0,2,8,0],
            [0,0,0,4,1,9,0,0,5],
            [0,0,0,0,8,0,0,7,9]]

example2 :: Grid
example2 = [[0,3,0,0,7,0,0,0,0],
            [6,0,0,1,9,5,0,0,0],
            [0,9,8,0,0,0,0,6,0],
            [8,0,0,0,6,0,0,0,3],
            [4,0,0,8,0,3,0,0,1],
            [7,0,0,0,2,0,0,0,6],
            [0,6,0,0,0,0,2,8,0],
            [0,0,0,4,1,9,0,0,5],
            [0,0,0,0,8,0,0,7,9]]

example3 :: Grid
example3 = [[1,0,0,0,3,0,5,0,4],
            [0,0,0,0,0,0,0,0,3],
            [0,0,2,0,0,5,0,9,8], 
            [0,0,9,0,0,0,0,3,0],
            [2,0,0,0,0,0,0,0,7],
            [8,0,3,0,9,1,0,6,0],
            [0,5,1,4,7,0,0,0,0],
            [0,0,0,3,0,0,0,0,0],
            [0,4,0,0,0,9,7,0,0]]

example4 :: Grid
example4 = [[1,2,3,4,5,6,7,8,9],
            [2,0,0,0,0,0,0,0,0],
            [3,0,0,0,0,0,0,0,0],
            [4,0,0,0,0,0,0,0,0],
            [5,0,0,0,0,0,0,0,0],
            [6,0,0,0,0,0,0,0,0],
            [7,0,0,0,0,0,0,0,0],
            [8,0,0,0,0,0,0,0,0],
            [9,0,0,0,0,0,0,0,0]]

example5 :: Grid
example5 = [[1,0,0,0,0,0,0,0,0],
            [0,2,0,0,0,0,0,0,0],
            [0,0,3,0,0,0,0,0,0],
            [0,0,0,4,0,0,0,0,0],
            [0,0,0,0,5,0,0,0,0],
            [0,0,0,0,0,6,0,0,0],
            [0,0,0,0,0,0,7,0,0],
            [0,0,0,0,0,0,0,8,0],
            [0,0,0,0,0,0,0,0,9]]

