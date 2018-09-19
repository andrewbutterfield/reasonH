module FPC5 where

rev []      =  []              -- def.rev.nil
rev (x:xs)  =  rev xs ++ [x]   -- def.rev.rec

[]     ++ ys  =  ys            -- def.++.nil
(x:xs) ++ ys  =  x:(xs ++ ys)  -- def.++.rec
