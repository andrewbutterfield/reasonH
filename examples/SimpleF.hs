module SimpleF where

f x  =  x + 1

prop_f0       =  f 0 == 1
prop_f1       =  f 1 == 2
prop_f' x     =  f x > x
prop_f_inv x  =  f (x-1) == x
