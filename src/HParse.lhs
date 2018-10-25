\section{Haskell Parser}
\begin{verbatim}
Copyright  Andrew Buttefield (c) 2017--18

LICENSE: BSD3, see file LICENSE at reasonEq root
\end{verbatim}
\begin{code}
module HParse
( Line, Lines, Parser
, parseHModule
, parseExpr
, parseEquiv
, hs42
, ParseMode(..), ParseResult(..), SrcLoc(..), pFail
)
where

import Data.Char

import Language.Haskell.Parser
import Language.Haskell.Pretty
import Language.Haskell.Syntax

import Utilities
import AST

import Debug.Trace
dbg msg x = trace (msg ++ show x) x
mdbg msg x = return $! dbg msg x
\end{code}


\subsection{Monadic Failure}
A polymorphic, monadic parser type:
\begin{code}
type Line = (Int,String)
type Lines = [Line]
type Parser m a  = Lines -> m (a,Lines)
\end{code}
A \texttt{SrcLoc}-based monadic failure:
\begin{code}
pFail :: Monad m => ParseMode -> Int -> Int -> String -> m a
pFail pmode lno colno msg
  = fail (parseFilename pmode ++ ':':show lno++ ":"++show colno++" "++msg)
\end{code}

\subsection{Parser Top-Level}

\begin{code}
parseHModule :: Monad m => String -> String -> m Mdl
parseHModule fname modstr
 = case parseModuleWithMode pmode modstr of
     ParseFailed loc msg -> pFail pmode (srcLine loc) (srcColumn loc) msg
     ParseOk hsmod -> return $ hsModule2Mdl hsmod
 where pmode = ParseMode fname
\end{code}

\newpage
\subsection{Parsing Expressions}

\begin{code}
parseExpr :: Monad m => ParseMode -> Lines -> Parser m Expr
parseExpr pmode restlns [] = pFail pmode 0 0 "no expression!"
parseExpr pmode restlns chunk@((lno,_):_)
  = case parseModuleWithMode pmode (modstrf chunk) of
      ParseFailed _ msg  -> pFail pmode lno 1 msg
      ParseOk hsmod -> return (getNakedExpression $ dbg "pE.hsmod:\n" hsmod, restlns)
  where
    modstrf [(_,str)]
      = unlines [ "module NakedExpr where"
                , "infix 1 ==="
                , "nakedExpr = "++str ]
    modstrf chunk
      = unlines ( [ "module NakedExpr where"
                  , "infix 1 ==="
                  , "nakedExpr = " ]
                  ++ map snd chunk )
\end{code}

\begin{code}
getNakedExpression :: HsModule -> Expr
getNakedExpression
 (HsModule _ _ _ _ [ HsPatBind _ _ (HsUnGuardedRhs hsexp) [] ])
    = hsExp2Expr hsexp
getNakedExpression _ = hs42

hs42 = LInt 42
\end{code}

\subsection{Parsing Equivalences}

\begin{code}
parseEquiv :: Monad m => ParseMode -> Lines -> Parser m (Expr, Expr)
parseEquiv pmode restlns [] = pFail pmode 0 0 "no equivalence!"
parseEquiv pmode restlns chunk@((lno,_):_)
  = case parseModuleWithMode pmode (modstrf chunk) of
      ParseFailed _ msg  -> pFail pmode lno 1 msg
      ParseOk hsmod -> return (getNakedEquivalence hsmod, restlns)
  where
    modstrf [(_,str)]
      = unlines [ "module NakedExpr where"
                , "infix 1 === ; x === y = x == y"
                , "nakedExpr = "++str ]
    modstrf chunk
      = unlines ( [ "module NakedExpr where"
                  , "infix 1 === ; x === y = x == y"
                  , "nakedExpr = " ]
                  ++ map snd chunk )
\end{code}

\begin{code}
getNakedEquivalence :: HsModule -> (Expr,Expr)
getNakedEquivalence
 (HsModule _ _ _ _ [ _, HsPatBind _ _ (HsUnGuardedRhs hsexp) [] ])
   = case hsexp of
       (HsInfixApp e1 (HsQVarOp (UnQual (HsSymbol "==="))) e2)
          ->  (hsExp2Expr e1, hsExp2Expr e2)
       _               ->  (hs42,hs42)
getNakedEquivalence _  =   (hs42,hs42)
\end{code}
