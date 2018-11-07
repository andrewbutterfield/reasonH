module List where

length [] = 0
length (_:xs) = 1 + length xs

[] ++ ys      =  ys
(x:xs) ++ ys  =  x:(xs ++ ys)
