\section{Checking}
\begin{haskell}
Copyright  Andrew Buttefield (c) 2017--18

LICENSE: BSD3, see file LICENSE at reasonEq root
\end{haskell}
\begin{code}
module Check
(Report, showReport, checkTheorem)
where

import AST
import Theory hiding (dbg)

import Debug.Trace
dbg msg x = trace (msg ++ show x) x
mdbg msg x = return $! dbg msg x
\end{code}

\begin{code}
type Report = [String]

showReport rep = putStrLn $ unlines rep
\end{code}

\begin{code}
checkTheorem :: Theorem -> Report
checkTheorem thm = ["checkTheorem NYI"]
\end{code}
