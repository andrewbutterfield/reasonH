module FPC3 where

data Parity = Even | Odd

pflop = Even:Odd:pflop

from n = n:(from (n+1))

parity n = if even n then Even else Odd

pflop = Even:pflop'
pflop' = Odd:pflop

data List = Nil | Cons Int List
