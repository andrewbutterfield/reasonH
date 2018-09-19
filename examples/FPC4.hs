module FPC4 where

invSetList []  =  True
invSetList [x]  = True
invSetList (x:ys@(y:_)) = x < y && invSetList  ys

insert x [] = [x]
insert x ys@(y:zs)
 | x < y      =  x : ys           -- smallest at start
 | x > y      =  y : insert x zs  -- recurse in
 | otherwise  =  ys               -- already present

data BTree a = Null | Br (BTree a) a (BTree a)
insert x Null = Br Null x Null
insert x bt@(Br left y right)
 | x < y      =  Br (insert x left) y right
 | x > y      =  Br left y (insert x right)
 | otherwise  =  bt

inv (Br left x right)
 = inv left
   && smax left <= x && x <= smin right
   && inv right
inv Null = True

smin Null = maxBound -- see Bounded class
smin (Br left x right) = minimum [smin left,x,smin right]

smax Null = minBound
smax (Br left x right) = maximum [smax left,x,smax right]
