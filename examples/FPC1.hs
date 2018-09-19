module FPC1 where

[] ++ ys = ys
(x:xs) ++ ys = x:(xs++ys)
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]
length [] = 0
length (_:xs) = 1 + length xs

ins :: Ord t => t -> [t] -> [t]
ins x [] = [x]
ins x ys@(y:zs)
 | x < y      =  x : ys        -- smallest at start\pause
 | x > y      =  y : ins x zs  -- recurse in\pause
 | otherwise  =  ys            -- already present\pause

mbr :: Ord t => t -> [t] -> Bool
mbr _ []   =  False
mbr x (y:ys)
 | x < y      =  False
 | x > y      =  mbr x ys
 | otherwise  =  True
