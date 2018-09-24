module NakedEquiv where

infix 3 ===

nakedEquiv  =  length (xs++ys) === (length xs) + (length ys)
