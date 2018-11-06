\section{Checking}
\begin{haskell}
Copyright  Andrew Butterfield (c) 2017--18

LICENSE: BSD3, see file LICENSE at reasonEq root
\end{haskell}
\begin{code}
module Check
(Report, showReport, checkTheorem)
where

import AST
import Theory
import Matching

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
checkTheorem :: [Mdl] -> [Theory] -> Theorem -> Report
checkTheorem mdls thrys thm
  = rep ("Checking theorem '"++thmName thm++"'")
     `rjoin` (checkStrategy mdls thrys (theorem thm) $ strategy thm)
\end{code}

Induction and case-based strategies require a hypothesis to be passed to
the calculation checker,
while the variations of reduction don't.
In the latter case we pass in a dummy hypothesis:
\begin{code}
dummyH = Var "??"
\end{code}

\begin{code}
checkStrategy :: [Mdl] -> [Theory] -> Expr -> Strategy -> Report
\end{code}

\begin{code}
checkStrategy mdls thrys goal (ReduceAll calc)
  = rep "Strategy: reduce all to True"
     `rjoin` checkFirst calc goal
     `rjoin` checkCalc mdls thrys dummyH calc
     `rjoin` checkLast calc (LBool True)

checkStrategy mdls thrys goal (ReduceLHS calc)
  = rep "Strategy: reduce LHS to RHS"
     `rjoin` checkFirst calc (lhsOf goal)
     `rjoin` checkCalc mdls thrys dummyH calc
     `rjoin` checkLast calc (rhsOf goal)

checkStrategy mdls thrys goal (ReduceRHS calc)
  = rep "Strategy: reduce RHS to LHS"
     `rjoin` checkFirst calc (rhsOf goal)
     `rjoin` checkCalc mdls thrys dummyH calc
     `rjoin` checkLast calc (lhsOf goal)

checkStrategy mdls thrys goal (ReduceBoth cLHS cRHS)
  = rep "Strategy: reduce RHS and LHS to same"
     `rjoin` checkBothStart goal cLHS cRHS
     `rjoin` rep "Check LHS" `rjoin` checkCalc mdls thrys dummyH cLHS
     `rjoin` rep "Check RHS" `rjoin` checkCalc mdls thrys dummyH cRHS
     `rjoin` checkSameLast cLHS cRHS

checkStrategy _ _ _ _  = rep "checkStrategy: NYI for Induction"
\end{code}


\begin{code}
checkFirst :: Calculation -> Expr -> Report
checkFirst (CALC e0 _) e
  | e0 == e   =  rep "OK: correct first expression."
  | otherwise =  rep "!!: incorrect first expression."
\end{code}

\begin{code}
lastE :: Calculation -> Expr
lastE (CALC e [])     =  e
lastE (CALC _ steps)  =  snd $ last steps

checkLast :: Calculation -> Expr -> Report
checkLast calc e
  | (lastE calc) == e  =  rep "OK: correct last expression."
checkLast _ _          =  rep "!!: incorrect last expression."
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
eEq = Var "=="
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

We keep the best until last \dots
\begin{code}
checkCalc :: [Mdl] -> [Theory] -> Expr -> Calculation -> Report
checkCalc mdls thrys hyp (CALC goal [])  =  rep "OK: no steps to check"
checkCalc mdls thrys hyp (CALC goal steps)
  = checkSteps mdls thrys hyp goal steps
checkSteps _ _ _ _ []  = rep "check complete"
checkSteps mdls thrys hyp goal ((just,goal'):steps)
  = checkStep mdls thrys hyp goal just goal'
     `rjoin` checkSteps mdls thrys hyp goal' steps
\end{code}

This is where all the heavy lifting is done:
\begin{code}
checkStep :: [Mdl] -> [Theory] -> Expr -> Expr -> Justification -> Expr
          -> Report
checkStep mdls thrys hyp goal (BECAUSE _ (D dnm i) howused what) goal'
 = case searchMods mdls dnm i of
     Nothing -> rep ("!!: Can't find definition "++dnm++"."++show i)
     Just defn
       -> case findAndApplyDEFN (mdlsKnown mdls) defn goal Top {-what-} of
           Nothing -> rep ("!!: Failed to apply defn: "++show what)
           Just goal''
             -> if goal'' == goal'
                 then rep ("OK: use of "++dnm++"."++show i++" is correct.")
                 else rep ("!!: use of "++dnm++"."++show i++" differs.")
checkStep _ _ _ _ just _ = rep ("checkStep NYI for "++show (law just))
\end{code}

We need all names defined in imported haskell files:
\begin{code}
mdlsKnown = concat . map mdlKnown

mdlKnown mdl = getDefined $ topdecls mdl

getDefined [] = []
getDefined (Fun (m:_)        : tdcls)  = fname m : getDefined tdcls
getDefined (Bind (Var v) _ _ : tdcls)  = v       : getDefined tdcls
getDefined (_                : tdcls)  =           getDefined tdcls
\end{code}

\begin{code}
type Definition = (Expr,Expr,[Decl])

searchMods [] dnm i = Nothing
searchMods (mdl:mdls) dnm i
  = case searchDecls (topdecls mdl) dnm i of
      Nothing  ->  searchMods mdls dnm i
      jdefn    ->  jdefn
\end{code}

\begin{code}
searchDecls [] dnm i = Nothing
searchDecls (decl:decls) dnm i
  = case checkDecl dnm i decl of
      Nothing -> searchDecls decls dnm i
      jdefn -> jdefn
\end{code}

\begin{code}
checkDecl :: String -> Int -> Decl -> Maybe Definition

checkDecl dnm i (Bind v@(Var vnm) defn ldcls)
  | dnm == vnm && i < 2  =  Just (v,defn,ldcls)
  -- only do simple  v = e where ... binds for now

checkDecl dnm i (Fun [match])
  | dnm == fname match && i < 2
                         = Just (mkLHS dnm match,rhs match, ldecls match)
checkDecl dnm i (Fun matches)
  | i < 1  =  Nothing
  | i > length matches  =  Nothing
  | dnm == fname match  = Just (mkLHS dnm match,rhs match, ldecls match)
  where
    match = matches !! (i-1)
checkDecl _ _ _ = Nothing

mkLHS dnm match = mkApp (Var dnm) $ lhspat match

mkApp f [] = f
mkApp f (a:as)  = mkApp (App f a) as
\end{code}

This does an in-order traverse of the \texttt{goal} looking for
the sub-expression defined by \texttt{what}.
Once found, it will use \texttt{defn} to rewrite that sub-expression.
\begin{code}
findAndApplyDEFN :: [String] -> Definition -> Expr -> Focus -> Maybe Expr
findAndApplyDEFN knowns defn goal Top = applyDEFN knowns defn goal
findAndApplyDEFN knowns defn goal (At nm i)
  = case findAllNameUsage nm [] goal of
      [] -> Nothing
      paths
         -> case getIth i paths of
             Nothing -> Nothing
             Just path -> applyAtPathFocus
                              (map fst $ dropWhile isApp1 path)
                              (applyDEFN knowns defn)
                              goal
  where
    getIth _ []      =  Nothing
    getIth 1 (x:_)   =  Just x
    getIth n (_:xs)  =  getIth (n-1) xs
    isApp1 (1,AppB)  =  True
    isApp1 _         =  False
\end{code}

\begin{code}
applyDEFN :: [String] -> Definition -> Expr -> Maybe Expr
applyDEFN knowns (lhs,rhs,ldcls) expr
  = case eMatch knowns expr lhs of
      Nothing -> Nothing
      Just bind -> Just $ buildReplacement bind ldcls rhs
\end{code}


Consider we are looking for the $i$th occurrence of name \texttt{f}
in an expression, and it is found embedded somehere,
and is a function name applied to several arguments:
\texttt{.... f x y z ....}.
What we want returned is a pointer to that full application,
and not just to \texttt{f}.
However, this means that the location of \texttt{f}
can be arbitrarily deep down the lefthand branch of an \texttt{App},
as the above application will parse as $@ (@ (@~f~x)~y)~z$.
If the application has path $\rho$, then the path to the
occurrence of $f$ will be $\rho \cat \seqof{1,1,1}$.
So we can delete trailing ones to get up to the correct location in this case.
However if \texttt{f} occurs in an if-expression (say),
like \texttt{if f then x else y}, then if the if-expression has path $\rho$,
then $f$ has path $\rho\cat\seqof{1}$, but this last one needs to remain.
In effect we have to tag the indices to indicate if we are branching
through an application ($@$) or some other kind of node (e.g., $if$).


\begin{code}
-- we only care about App vs everything else right now
data ExprBranches = AppB | OtherB deriving (Eq, Show)
type Branch = (Int,ExprBranches)
type Path = [Branch] -- identify sub-expr by sequence of branch indices
findAllNameUsage :: String -> Path -> Expr -> [Path]
-- paths returned here are reversed, with deepest index first
findAllNameUsage nm currPath (App (Var v) e2)
  | nm == v  = currPath : findAllNameUsage nm ((2,AppB):currPath) e2
findAllNameUsage nm currPath (Var v) = if nm == v then [currPath] else []
findAllNameUsage nm currPath (App e1 e2)
  =  findAllNameUsage nm ((1,AppB):currPath) e1
  ++ findAllNameUsage nm ((2,AppB):currPath) e2
findAllNameUsage nm currPath (If e1 e2 e3)
  =  findAllNameUsage nm ((1,OtherB):currPath) e1
  ++ findAllNameUsage nm ((2,OtherB):currPath) e2
  ++ findAllNameUsage nm ((3,OtherB):currPath) e3
findAllNameUsage nm currPath (GrdExpr grds)
  = concat $ map (findGuardNameUsage nm currPath) $ zip [1..] grds
findAllNameUsage nm currPath e = error ("findAllNameUsage NYIf "++show e)
\end{code}

\begin{code}
findGuardNameUsage nm currPath (i,(grd,res))
  =    findAllNameUsage nm ((1,OtherB):cp') grd
    ++ findAllNameUsage nm ((2,OtherB):cp') res
  where cp' = (i,OtherB):currPath
\end{code}

\begin{code}
applyAtPathFocus :: [Int] -> (Expr -> Maybe Expr) -> Expr -> Maybe Expr
applyAtPathFocus path apply goal = Nothing
\end{code}
