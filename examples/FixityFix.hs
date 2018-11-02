module FixityFix where

data E = A Int | B E String E deriving (Eq,Show)

split :: E -> ( [String], [E] )
split a@(A _) = ( [], [a] )
split (B e1 op e2) = ( ops ++ [op] , es ++ [e2]) where (ops,es) = split e1

wrap :: [String] -> [E] -> E
wrap [] [e] = e
wrap [op] [e1,e2] = B e1 op e2
wrap (op:ops) (e1:e2:es) = wrap ops (B e1 op e2 : es)
wrap ops es = error ("wrap not defined for "++show (ops,es))

-- infixl 9  !!
-- infixr 9  .
-- infixr 8  ^, ^^, **
-- infixl 7  *, /, `quot`, `rem`, `div`, `mod`
-- infixl 6  +, -
-- infixr 5  : ++
-- infix  4  ==, /=, <, <=, >=, >, `elem`, `notElem`
-- infixr 3  &&
-- infixr 2  ||
-- infixl 1  >>, >>=
-- infixr 1  =<<
-- infixr 0  $, $!, `seq`

prec :: String -> Int
prec "*" = 7
prec "+" = 6
prec ":" = 5
prec "==" = 4

data Assoc = A0 | AL | AR deriving (Eq, Show)

ass :: String -> Assoc
ass "*" = AL
ass "+" = AL
ass ":" = AR
ass "==" = A0

pfuse :: Int -> [String] -> [E] -> ([String],[E])
pfuse p [] [e] = ([],[e])
pfuse p [op] [e1,e2]
 | p == prec op  =  ([],[B e1 op e2])
 | otherwise  =  ([op],[e1,e2])
pfuse p (op:ops) (e1:e2:es)
 | p == prec op  =  pfuse p ops (B e1 op e2 : es)
 | otherwise  = (op:ops',e1:es') where (ops',es') = pfuse p ops (e2:es)


{- ======= Examples ========= -}
snoc = (B (B (B (A 3) ":" (A 2)) ":" (A 1)) ":" (A 0))

amalt =  (B (B (B (B (A 1) "+" (A 2)) "*" (A 3)) "+" (A 4)) "*" (A 5))

mix = wrap ["+",":","+",":","+",":","+",":"] $ map A $ reverse [0..8]
-- pfuse 8..5 $ split mix  --->   ((((8+7):(6+5)):(4+3)):(2+1)):0
