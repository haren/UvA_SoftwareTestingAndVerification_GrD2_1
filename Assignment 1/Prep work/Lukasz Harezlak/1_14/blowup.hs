blowup:: String -> String

blowup xs = blowup2 xs 1
    where blowup2 [] _ = []
          blowup2 (x:xs) n = take n (repeat x) ++ blowup2 xs (n+1)