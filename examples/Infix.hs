module Infix where
infix  5  %
infixl 5 &%
infixr 5 %&
a = (1 %  2) % (3 %  4)
b =  1 &% 2 &%  3 &% 4
c =  1 %& 2 %&  3 %& 4
d =  1 %% 2 %%  3 %% 4
x %  y = x+y
x %& y = x+y
x &% y = x+y
x %% y = x+y
