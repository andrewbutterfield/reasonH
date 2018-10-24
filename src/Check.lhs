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
import Theory

import Debug.Trace
dbg msg x = trace (msg ++ show x) x
mdbg msg x = return $! dbg msg x
\end{code}

\begin{code}
type Report = [String]

showReport rep = putStrLn $ unlines rep

rep :: String -> Report
rep str = lines str

rjoin :: Report -> Report -> Report
r1 `rjoin` r2 = r1 ++ r2
\end{code}

\begin{code}
checkTheorem :: Theorem -> Report
checkTheorem thm
  = rep ("Checking theorem '"++thmName thm++"'")
     `rjoin` (checkStrategy $ strategy thm)
\end{code}

\begin{code}
checkStrategy :: Strategy -> Report
checkStrategy (ReduceAll calc)
  = rep "Strategy: reduce all to True"
     `rjoin` checkCalc calc
     -- need to check last thing is True
checkStrategy (ReduceLHS calc)
  = rep "Strategy: reduce LHS to RHS"
     `rjoin` checkCalc calc
     -- need to check last LHS = RHS
checkStrategy (ReduceRHS calc)
  = rep "Strategy: reduce RHS to LHS"
     `rjoin` checkCalc calc
     -- need to check last RHS = LHS
checkStrategy (ReduceBoth cLHS cRHS)
  = rep "Strategy: reduce RHS and LHS to same"
     `rjoin` rep "Check LHS" `rjoin` checkCalc cLHS
     `rjoin` rep "Check RHS" `rjoin` checkCalc cRHS
     `rjoin` checkSame cLHS cRHS
checkStrategy _  = rep "checkStrategy: NYI for Induction"
\end{code}

\begin{code}
checkCalc :: Calculation -> Report
checkCalc calc = rep "checkCalc NYI"
\end{code}

\begin{code}
checkSame :: Calculation -> Calculation -> Report
checkSame cLHS cRHS = rep "checkSame NYI"
\end{code}
