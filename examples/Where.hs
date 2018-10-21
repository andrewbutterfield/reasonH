module Where where

wf :: [Char] -> Char
wf [] = '?'
wf (x:xs)
 | even xnum  = 'e'
 | xnum < 60  = 's'
 | otherwise  = 'L'
 where
   xnum = ord x
