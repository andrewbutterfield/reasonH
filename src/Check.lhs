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
     `rjoin` (checkStrategy (theorem thm) $ strategy thm)
\end{code}

\begin{code}
checkStrategy :: Expr -> Strategy -> Report

checkStrategy goal (ReduceAll calc)
  = rep "Strategy: reduce all to True"
     `rjoin` checkFirst calc goal
     `rjoin` checkCalc calc
     `rjoin` checkLast calc (LBool True)

checkStrategy goal (ReduceLHS calc)
  = rep "Strategy: reduce LHS to RHS"
     `rjoin` checkFirst calc (lhsOf goal)
     `rjoin` checkCalc calc
     `rjoin` checkLast calc (rhsOf goal)

checkStrategy goal (ReduceRHS calc)
  = rep "Strategy: reduce RHS to LHS"
     `rjoin` checkFirst calc (rhsOf goal)
     `rjoin` checkCalc calc
     `rjoin` checkLast calc (lhsOf goal)

checkStrategy goal (ReduceBoth cLHS cRHS)
  = rep "Strategy: reduce RHS and LHS to same"
     `rjoin` checkBothStart goal cLHS cRHS
     `rjoin` rep "Check LHS" `rjoin` checkCalc cLHS
     `rjoin` rep "Check RHS" `rjoin` checkCalc cRHS
     `rjoin` checkSameLast cLHS cRHS

checkStrategy _ _  = rep "checkStrategy: NYI for Induction"
\end{code}


\begin{code}
checkFirst :: Calculation -> Expr -> Report
checkFirst (CALC e0 _) e
  | e0 == e   =  rep "OK: correct first expression."
  | otherwise =  rep "!!: incorrect first expression."
\end{code}

\begin{code}
lastE :: Calculation -> Expr
lastE (CALC e [])  =  e
lastE (CALC _ steps)  =  snd $ last steps

checkLast :: Calculation -> Expr -> Report
checkLast calc e
  | lastE calc == e  =  rep "OK: correct last expression."
checkLast _ _        =  rep "!!: incorrect last expression."
\end{code}

\begin{code}
checkBothStart :: Expr -> Calculation -> Calculation -> Report
checkBothStart goal (CALC gLHS _) (CALC gRHS _)
  | goal == equal gLHS gRHS  = rep "OK: goal lhs/rhs"
  | otherwise                = rep "!!: (lhs = rhs) is not goal"
\end{code}

\begin{code}
checkSameLast :: Calculation -> Calculation -> Report
checkSameLast cLHS cRHS
 | lastE cLHS == lastE cRHS  =  rep "OK: last expressions are the same."
 | otherwise                 =  rep "!!: last expressions differ."
\end{code}

\begin{code}
equal :: Expr -> Expr -> Expr
equal e1 e2 = App (App eEq e1) e2
\end{code}

\begin{code}
lhsOf (App (App eq e1) _)
 | eq == eEq  =  e1
lhsOf e       =  e

rhsOf (App (App eq _) e2)
 | eq == eEq  =  e2
rhsOf e       =  e
\end{code}

Last because it is the biggie...
\begin{code}
checkCalc :: Calculation -> Report
checkCalc calc = rep "checkCalc NYI"
\end{code}
