module Equiv where

infix 3 ===
x === y = x == y


equiv x y  = x == y  ===  (x + 1) == (y + 1)
