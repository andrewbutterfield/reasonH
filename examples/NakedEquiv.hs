module NakedEquiv where

infix 3 === 

nakedEquiv xs ys  =   length (xs++ys) === (length xs) + (length ys)
