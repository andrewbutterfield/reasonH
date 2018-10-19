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
For now we won't support ``as''-patterns.

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

\texttt{"xxx"} parses as:
\begin{haskell}
xxx
\end{haskell}
Suggested form:
\begin{haskell}
xxx
\end{haskell}

\texttt{"xxx"} parses as:
\begin{haskell}
xxx
\end{haskell}
Suggested form:
\begin{haskell}
xxx
\end{haskell}

\texttt{"xxx"} parses as:
\begin{haskell}
xxx
\end{haskell}
Suggested form:
\begin{haskell}
xxx
\end{haskell}

\texttt{"xxx"} parses as:
\begin{haskell}
xxx
\end{haskell}
Suggested form:
\begin{haskell}
xxx
\end{haskell}

\texttt{"xxx"} parses as:
\begin{haskell}
xxx
\end{haskell}
Suggested form:
\begin{haskell}
xxx
\end{haskell}
