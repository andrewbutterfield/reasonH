\section{Matching}
\begin{haskell}
Copyright  Andrew Buttefield (c) 2017--18

LICENSE: BSD3, see file LICENSE at reasonEq root
\end{haskell}
\begin{code}
module Matching
where

import AST

import Debug.Trace
dbg msg x = trace (msg ++ show x) x
mdbg msg x = return $! dbg msg x
\end{code}

\begin{verbatim}
data Expr
  = LBool Bool | LInt Int | LChar Char
  | Var String
  | App Expr Expr
  | If Expr Expr Expr
  | GrdExpr [(Expr,Expr)]
  | Let [Decl] Expr
  | PApp String [Expr]
  deriving (Eq,Show)
\end{verbatim}

\begin{verbatim}
data Match = Match { fname ::  String  -- function name
                   , lhspat :: [Expr]  -- LHS patterns
                   , rhs :: Expr    -- RHS outcome
                   , ldecls :: [Decl]  -- local declarations
                   }
           deriving (Eq, Show)
\end{verbatim}

\begin{verbatim}
data Decl
  = Fun [Match]
  | Bind Expr Expr [Decl]
  | Syntax -- not relevant to this tool !
  | Type String -- just noting name for now - to be addressed later
  deriving (Eq, Show)
\end{verbatim}
