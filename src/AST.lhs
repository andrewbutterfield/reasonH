\section{Abstract Syntax Tree}
\begin{haskell}
Copyright  Andrew Buttefield (c) 2017--18

LICENSE: BSD3, see file LICENSE at reasonEq root
\end{haskell}
\begin{code}
module AST where

import Language.Haskell.Parser
import Language.Haskell.Pretty
import Language.Haskell.Syntax

import Debug.Trace
dbg msg x = trace (msg ++ show x) x
mdbg msg x = return $! dbg msg x
\end{code}

We need a simplified AST for haskell.
We don't need any source-locs,
and we really don't need to distinguish identifiers from symbols,
handle qualified names,
or treat patterns differently to general expressions.
Wildcard patterns (or even irrefutable ones)
can be handled using names.


\subsection{Simplified Haskell AST}

We simplify things dramatically.
First, expressions:
\begin{code}
data Expr
  = LBool Bool | LInt Int | LChar
  | Var String
  | App String [Expr]
  | GrdExpr [(Expr,Expr)]
  | Let [Decl] Expr
  deriving (Eq,Show)
\end{code}
Then, declarations:
\begin{code}
data Decl
  = Fun [ ( Expr, Expr, [Decl] ) ]
  | Bind String Expr
  deriving (Eq, Show)
\end{code}

\newpage
\subsection{Examples}

\texttt{"1+(2+3)"} parses as:
\begin{haskell}
HsInfixApp
  (HsLit (HsInt 1))
  (HsQVarOp (UnQual (HsSymbol "+")))
  (HsParen
    ( HsInfixApp (HsLit (HsInt 2))
                 (HsQVarOp (UnQual (HsSymbol "+")))
                 (HsLit (HsInt 3))
    )
  )
\end{haskell}
Suggested form:
\begin{haskell}
App "+" [ LInt 1, App "+" [ LInt 2, LInt 3] ]
\end{haskell}

\texttt{"1+2+3"} parses as
\begin{haskell}
HsInfixApp
  (HsInfixApp
    (HsLit (HsInt 1))
    (HsQVarOp (UnQual (HsSymbol "+")))
    (HsLit (HsInt 2))
  )
  (HsQVarOp (UnQual (HsSymbol "+")))
  (HsLit (HsInt 3))
\end{haskell}
Suggested form:
\begin{haskell}
App "+" [LInt 1, LInt 2, LInt 3]
\end{haskell}

\texttt{"(1+2)+3"} parses as
\begin{haskell}
HsInfixApp
  (HsParen
    (HsInfixApp
      (HsLit (HsInt 1))
      (HsQVarOp (UnQual (HsSymbol "+")))
      (HsLit (HsInt 2))
    )
  )
  (HsQVarOp (UnQual (HsSymbol "+")))
  (HsLit (HsInt 3))
\end{haskell}
Suggested form:
\begin{haskell}
App "+" [App "+" [LInt 1, LInt 2], LInt 3]
\end{haskell}

\texttt{"[] ++ ys = ys"} parses as:
\begin{haskell}
HsMatch (SrcLoc {...})
  (HsSymbol "++")
  [ HsPList []
  , HsPVar (HsIdent "ys") ]
  (HsUnGuardedRhs (HsVar (UnQual (HsIdent "ys"))))
  [] -- where-clauses
\end{haskell}
Suggested form:
\begin{haskell}
Match (App "++" [ LNull, Var "ys" ]) (Var "ys") []
\end{haskell}

\newpage
\texttt{"(x:xs) ++ ys = x:(xs++ys)"} parses as:
\begin{haskell}
HsMatch (SrcLoc {...})
  (HsSymbol "++")
  [ HsPParen ( HsPInfixApp (HsPVar (HsIdent "x"))
                           (Special HsCons)
                           (HsPVar (HsIdent "xs"))
             )
  , HsPVar (HsIdent "ys") ]
  ( HsUnGuardedRhs
      ( HsInfixApp
         (HsVar (UnQual (HsIdent "x")))
         (HsQConOp (Special HsCons))
         ( HsParen
           ( HsInfixApp (HsVar (UnQual (HsIdent "xs")))
                        (HsQVarOp (UnQual (HsSymbol "++")))
                        (HsVar (UnQual (HsIdent "ys")))
           )
         )
      )
  )
  []
\end{haskell}
Suggested form:
\begin{haskell}
Match (App "++" [App ":" [Var "x", Var "xs"], Var "ys"] )
      (App ":" [Var "x", App "++" [Var "xs", Var "ys"]])
      []
\end{haskell}

\texttt{"[] ++ ys = ys"}\\
\texttt{"(x:xs) ++ ys = x:(xs++ys)"} parses as:
\begin{haskell}
HsFunBind
  [ HsMatch (SrcLoc {...}  <as above>
  , HsMatch (SrcLoc {...}  <as above>
  ]
\end{haskell}
Suggested form:
\begin{haskell}
Fun [ Match <as above>, Match <as above> ]
\end{haskell}

\texttt{"reverse [] = []"} parses as:
\begin{haskell}
HsMatch (SrcLoc {...})
  (HsIdent "reverse")
  [HsPList []]
  (HsUnGuardedRhs (HsList []))
  []
\end{haskell}
Suggested form:
\begin{haskell}
Match (App "reverse" [LNull]) (LNull) []
\end{haskell}

\newpage
\texttt{"reverse (x:xs) = reverse xs ++ [x]"} parses as:
\begin{haskell}
HsMatch (SrcLoc {srcFilename = "FPC1.hs", srcLine = 7, srcColumn = 1})
  (HsIdent "reverse")
  [ HsPParen
      ( HsPInfixApp (HsPVar (HsIdent "x"))
                    (Special HsCons)
                    (HsPVar (HsIdent "xs")))
  ]
  ( HsUnGuardedRhs
     ( HsInfixApp
         ( HsApp (HsVar (UnQual (HsIdent "reverse")))
                 (HsVar (UnQual (HsIdent "xs")))
         )
         ( HsQVarOp (UnQual (HsSymbol "++")) )
         ( HsList [HsVar (UnQual (HsIdent "x"))] )
     )
  )
  []
\end{haskell}
Suggested form:
\begin{haskell}
Match (App "reverse" [App ":" [Var "x", Var "xs"]])
      (App "++" [App "reverse" [Var "xs"], List [Var "x"])
      []
\end{haskell}

\texttt{"length (\_:xs) = 1 + length xs"} parses as:
\begin{haskell}
HsMatch (SrcLoc {...})
  (HsIdent "length")
  [ HsPParen ( HsPInfixApp HsPWildCard
                           (Special HsCons) (HsPVar (HsIdent "xs"))
             )
  ]
  ( HsUnGuardedRhs
      ( HsInfixApp
          (HsLit (HsInt 1))
          (HsQVarOp (UnQual (HsSymbol "+")))
          ( HsApp (HsVar (UnQual (HsIdent "length")))
                  (HsVar (UnQual (HsIdent "xs")))
          )
      )
  )
  []
\end{haskell}
Suggested form:
\begin{haskell}
Match (App "length" [App ":" [Var "_", Var "xs"]])
      (App "+" [LInt 1,App "length" [Var "xs"]])
      []
\end{haskell}

\newpage
\texttt{"ins x ys@(y:zs)"}\\
\texttt{" | x < y      =  x : ys"}\\
\texttt{" | x > y      =  y : ins x zs"}\\
\texttt{" | otherwise  =  ys"} parses as:
\begin{haskell}
HsMatch (SrcLoc {srcFilename = "FPC1.hs", srcLine = 14, srcColumn = 1})
(HsIdent "ins")
[ HsPVar (HsIdent "x")
, HsPAsPat
    (HsIdent "ys")
    ( HsPParen
        ( HsPInfixApp
            (HsPVar (HsIdent "y"))
            (Special HsCons)
            (HsPVar (HsIdent "zs"))
        )
    )
]
( HsGuardedRhss
    [ HsGuardedRhs
        (SrcLoc {srcFilename = "FPC1.hs", srcLine = 15, srcColumn = 2})
        ( HsInfixApp
           (HsVar (UnQual (HsIdent "x")))
           (HsQVarOp (UnQual (HsSymbol "<")))
           (HsVar (UnQual (HsIdent "y")))
        )
        ( HsInfixApp
            (HsVar (UnQual (HsIdent "x")))
            (HsQConOp (Special HsCons))
            (HsVar (UnQual (HsIdent "ys")))
        )
    , HsGuardedRhs
        (SrcLoc {srcFilename = "FPC1.hs", srcLine = 16, srcColumn = 2})
        ( HsInfixApp
            (HsVar (UnQual (HsIdent "x")))
            (HsQVarOp (UnQual (HsSymbol ">")))
            (HsVar (UnQual (HsIdent "y")))
        )
        ( HsInfixApp
            (HsVar (UnQual (HsIdent "y")))
            (HsQConOp (Special HsCons))
            ( HsApp
                ( HsApp (HsVar (UnQual (HsIdent "ins")))
                        (HsVar (UnQual (HsIdent "x")))
                )
                (HsVar (UnQual (HsIdent "zs")))
            )
        )
    , HsGuardedRhs
        (SrcLoc {srcFilename = "FPC1.hs", srcLine = 17, srcColumn = 2})
        (HsVar (UnQual (HsIdent "otherwise")))
        (HsVar (UnQual (HsIdent "ys")))
    ])
[]
\end{haskell}
\newpage
Suggested form:
\begin{haskell}
Match (App "ins" [Var "x", App "@" [ys,App ":" [Var "y", Var "zs"]]])
      (GrdExpr [ ( App "<" [Var "x", Var "y"]
                 , App ":" [Var "x", Var "ys"] )
               , ( App ">" [Var "x", Var "y"]
                 , App ":"" [Var "y", App "ins" [Var "x", Var "zs"]] )
               , ( LTrue, Var "ys" )
               ]
      )
      []
\end{haskell}

\newpage
\texttt{"wf (x:xs)"}\\
\texttt{" | even xnum  = 'e'"}\\
\texttt{" | xnum < 60  = 's'"}\\
\texttt{" | otherwise  = 'L'"}\\
\texttt{" where"}\\
\texttt{"   xnum = ord x"}
parses as:
\begin{haskell}
HsMatch (SrcLoc {srcFilename = "Where.hs", srcLine = 5, srcColumn = 1})
  (HsIdent "wf")
  [ HsPParen
      ( HsPInfixApp (HsPVar (HsIdent "x"))
                    (Special HsCons)
                    (HsPVar (HsIdent "xs"))
      )
  ]
  ( HsGuardedRhss
      [ HsGuardedRhs
         (SrcLoc {srcFilename = "Where.hs", srcLine = 6, srcColumn = 2})
         (HsApp (HsVar (UnQual (HsIdent "even")))
                (HsVar (UnQual (HsIdent "xnum")))
         )
         (HsLit (HsChar 'e'))
      , HsGuardedRhs
         (SrcLoc {srcFilename = "Where.hs", srcLine = 7, srcColumn = 2})
         (HsInfixApp (HsVar (UnQual (HsIdent "xnum")))
                     (HsQVarOp (UnQual (HsSymbol "<")))
                     (HsLit (HsInt 60))
         )
         (HsLit (HsChar 's'))
      , HsGuardedRhs
         (SrcLoc {srcFilename = "Where.hs", srcLine = 8, srcColumn = 2})
         (HsVar (UnQual (HsIdent "otherwise")))
         (HsLit (HsChar 'L'))
      ]
  )
  [ HsPatBind
     (SrcLoc {srcFilename = "Where.hs", srcLine = 10, srcColumn = 4})
     ( HsPVar (HsIdent "xnum"))
     ( HsUnGuardedRhs
         ( HsApp (HsVar (UnQual (HsIdent "ord")))
                 (HsVar (UnQual (HsIdent "x")))
         )
     )
     []
   ]
\end{haskell}
Suggested form:
\begin{haskell}
Match (App "wf" [App ":" [Var "x", Var "xs"]])
      (GrdExpr [ ( App "even" [Var "xnum"]
                 , LChar 'e' )
               , ( App "<" [Var "xnum", LInt 60]
                 , LChar 's' )
               , ( LTrue, LChar 'L' )
               ]
      )
      [ Bind "xnum" (App "ord" [Var "xs"])]
\end{haskell}
%
% \texttt{"xxx"} parses as:
% \begin{haskell}
% xxx
% \end{haskell}
% Suggested form:
% \begin{haskell}
% xxx
% \end{haskell}

\newpage
Gathered forms:
\begin{haskell}
App "+" [ LInt 1, App "+" [ LInt 2, LInt 3] ]

App "+" [LInt 1, LInt 2, LInt 3]

App "+" [App "+" [LInt 1, LInt 2], LInt 3]

Match (App "++" [ LNull, Var "ys" ]) (Var "ys") []

Match (App "++" [App ":" [Var "x", Var "xs"], Var "ys"] )
      (App ":" [Var "x", App "++" [Var "xs", Var "ys"]])
      []

Fun [ Match <as above>, Match <as above> ]

Match (App "reverse" [LNull]) (LNull) []

Match (App "reverse" [App ":" [Var "x", Var "xs"]])
      (App "++" [App "reverse" [Var "xs"], List [Var "x"])
      []

Match (App "length" [App ":" [Var "_", Var "xs"]])
      (App "+" [LInt 1,App "length" [Var "xs"]])
      []

Match (App "ins" [Var "x", App "@" [ys,App ":" [Var "y", Var "zs"]]])
      (GrdExpr [ ( App "<" [Var "x", Var "y"]
                 , App ":" [Var "x", Var "ys"] )
               , ( App ">" [Var "x", Var "y"]
                 , App ":"" [Var "y", App "ins" [Var "x", Var "zs"]] )
               , ( LTrue, Var "ys" )
               ]
      )
      []

Match (App "wf" [App ":" [Var "x", Var "xs"]])
      (GrdExpr [ ( App "even" [Var "xnum"]
                 , LChar 'e' )
               , ( App "<" [Var "xnum", LInt 60]
                 , LChar 's' )
               , ( LTrue, LChar 'L' )
               ]
      )
      [ Bind "xnum" (App "ord" [Var "xs"])]
\end{haskell}
