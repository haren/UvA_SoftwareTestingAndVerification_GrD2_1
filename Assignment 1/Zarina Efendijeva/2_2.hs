module Chapter_2_exercise2_2 where

infix 1 <+>
(<+>) :: Bool -> Bool -> Bool
(<+>) True False = True
(<+>) False True = True
(<+>) _ _ = False
