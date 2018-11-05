module FixityFix where

data E = A Int | B E String E deriving (Eq,Show)

split :: E -> ( [String], [E] )
split a@(A _) = ( [], [a] )
split (B e1 op e2) = ( ops ++ [op] , es ++ [e2]) where (ops,es) = split e1

data Assoc = A0 | AL | AR deriving (Eq, Show)

pfuse :: Int -> [String] -> [E] -> ([String],[E])
pfuse p [] [e] = ([],[e])
pfuse p [op] [e1,e2]
 | p == prec op  =  ([],[B e1 op e2])
 | otherwise  =  ([op],[e1,e2])
pfuse p (op:ops) (e1:e2:es)
 | p == prec op  =  pfuse p ops (B e1 op e2 : es)
 | otherwise  = (op:ops',e1:es') where (ops',es') = pfuse p ops (e2:es)

pfusing :: Int -> ([String],[E]) -> ([String],[E])
pfusing (-1) oes = oes
pfusing p (ops,es) = pfusing (p-1) $ pfuse p ops es

-- we assume everything is left-infix to start.
fixfixity :: E -> E
fixfixity a@(A _)  =  a
fixfixity (B e1 op e2) = fix' (B (fixfixity e1) op (fixfixity e2))


fix' (B b@(B e1 op1 e2) op2 e3)
 | ass op1 == AR && ass op2 == AR && prec op1 == prec op2
    = B e1 op1 (insSE op2 e2 e3 )
fix' e = e

insSE op2 (B e4 op3 e5) e3
 | ass op3 == AR && prec op2 == prec op3
   = B e4 op3 (insSE op2 e5 e3)
insSE op2 e2 e3 = B e2 op2 e3

fixit e = fixfixity $ head $ snd $ pfusing 9 $ split e



{- ======= Examples ========= -}

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

ass :: String -> Assoc
ass "*" = AL
ass "+" = AL
ass ":" = AR
ass "==" = A0

snoc = B (B (B (A 3) ":" (A 2)) ":" (A 1)) ":" (A 0) -- ((3:2):1):0

-- wanted  3:(2:(1:0))

-- fixit =  3:(2:(1:0))

amalt =  B (B (B (B (A 1) "+" (A 2)) "*" (A 3)) "+" (A 4)) "*" (A 5)

-- wanted (1+(2*3))+(4*5)

-- fixit = (1+(2*3))+(4*5)

wrap :: [String] -> [E] -> E
wrap [] [e] = e
wrap [op] [e1,e2] = B e1 op e2
wrap (op:ops) (e1:e2:es) = wrap ops (B e1 op e2 : es)
wrap ops es = error ("wrap not defined for "++show (ops,es))


mix = wrap ["+",":","+",":","+",":","+",":"] $ map A $ reverse [0..8]

-- wanted: (8+7):((6+5):((4+3):((2+1):0)))

-- fixit = (8+7):((6+5):((4+3):((2+1):0)))
