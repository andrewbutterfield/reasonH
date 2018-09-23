\section{Theory}
\begin{verbatim}
Copyright  Andrew Buttefield (c) 2017--18

LICENSE: BSD3, see file LICENSE at reasonEq root
\end{verbatim}
\begin{code}
module Theory where

import Language.Haskell.Parser
import Language.Haskell.Pretty
import Language.Haskell.Syntax

import Utilities
\end{code}


\subsection{Theory Document Structure}

Typically a keyword at the start of a line introduces something.
We start with \texttt{THEORY} and zero or more imports:
\begin{verbatim}
THEORY <TheoryName>
IMPORT theory <Name>
IMPORT haskell <Name>
\end{verbatim}
We then have a number of ``one-liners'':
\begin{verbatim}
AXIOM <name> <br?> <equation>
DERIVED <name> <br?> <equation>
INDUCTION-BASE <Type> <value>
INDUCTION-STEP <Type>  <var> --> <expr>
INDUCTION-INJ  <Type> <br?> <equation>  ===  <equation>
\end{verbatim}
Here, \verb"<br?>" means that the following part may occupy
a number of subsequent lines. There can be a blank line before it,
and must be a blank line after it. The following part itself must
not have blank lines embedded in it.

\newpage
The following keywords are arranged to form larger structures
in which a theorem is stated and then proven.
\begin{verbatim}
THEOREM <name> <free-var1> ... <freevarN> <br?> <expr>
STRATEGY <strategy>
LHS <br!> <calculation>
RHS <br!> <calculation>
BASE <var1> = <val1> .. <varN> = <valN> <br!> <expr>
QED ( BASE | STEP | <name> )
STEP <var1> --> <expr1> .. <varN> --> <exprN>
ASSUME <br?> <expr>
SHOW <br?> <expr>
\end{verbatim}
Here, \verb"<br!>" is similar to \verb"<br?>",
except that a line break at this point is mandatory.

Strategies include:
\begin{verbatim}
reduce-all
reduce-lhs
reduce-rhs
reduce-both
induction <type1> <ind-var1> .. <typeN> <ind-varN>
\end{verbatim}

A calculation is a sequence of formul\ae\ seperated by justification lines,
which always start with an equal sign. Blank lines are allowed
around justification lines.
\begin{verbatim}
<expr1>
 = <justification1>
 ...
 = <justificationN>
<exprN+1>
\end{verbatim}

A justification is one of the following,
where both direction and location are optional.
A missing direction means the whole law matches,
while an explicit direction means the laws is an equation
and we have matched one side, to be replaced by the other.
A missing location means the whole current expression
is replaced.
\begin{verbatim}
 <funorlawname> [<direction>] [@ <location>]
 ASSUMPTION <direction>
 ASSUMPTION <direction> @ <location>
\end{verbatim}

Locations, identify the function name or value to be replaced
with a number used to identify which, if there is more than one.
\begin{verbatim}
<funnameorvalue>[.<number>]
\end{verbatim}

Directions: \verb"l2r" and \verb"r2l"

A theorem has the following top-level structure:
\begin{verbatim}
THEOREM <name> <free-var1> ... <freevarN> <br?> <expr> === <expr>
STRATEGY <strategy>
...
QED <name>
\end{verbatim}

\newpage
The choice of strategy will then determine the resulting structure:
\begin{description}
  \item [reduce-all]
    \begin{verbatim}
      <calculation>
    \end{verbatim}
  \item [reduce-lhs]
    \begin{verbatim}
      <calculation>
    \end{verbatim}
  \item [reduce-rhs]
    \begin{verbatim}
      <calculation>
    \end{verbatim}
  \item [reduce-both]~\\
    \begin{verbatim}
LHS
<calculation>
RHS
<calculation>
    \end{verbatim}
  \item [induction]~\\
    \begin{verbatim}
BASE <var1> = <val1> .. <varN> = <valN> <br!> <expr>
<one of the four strategies above>
QED BASE
STEP <var1> --> <expr1> .. <varN> --> <exprN>
ASSUME <br?> <expr>
SHOW <br?> <expr>
<one of the four strategies above>
QED STEP
    \end{verbatim}
\end{description}

\newpage
\subsection{Datatypes}

\begin{code}
data Theory
 = THEORY {
     theoryName :: String
   , thImports :: [Theory]
   , hkImports :: [HsModule]
   , thLaws :: [Law]
   , thInds :: [InductionSpec]
   , thTheorems :: [Theorem]
   }
 deriving Show
\end{code}

\begin{code}
data Law
 = LAW {
     lawName :: String
   , lawEqn :: HsExp
   }
 deriving Show
\end{code}

\begin{code}
data InductionSpec
 = IND {
     indType :: String
   , indBase :: HsExp
   , indStep :: (HsExp, HsExp)
   , indInj :: (HsExp,HsExp)
   }
 deriving Show
\end{code}

\begin{code}
data Theorem
 = THEOREM {
     thmName :: String
   , strategy :: Strategy
   }
 deriving Show
\end{code}

\begin{code}
data Strategy
 = ReduceAll Calculation
 | ReduceLHS Calculation
 | ReduceRHS Calculation
 | ReduceBoth Calculation Calculation
 | Induction {
     iVars :: [(String,String)] -- type var
   , baseVals :: [(String,HsExp)] -- var = value
   , baseStrategy :: Strategy
   , steps :: [(String,HsExp)] -- var --> expr
   , assume :: HsExp
   , iGoal :: HsExp
   , stepStrategy :: Strategy
   }
 deriving Show
\end{code}

\begin{code}
data Calculation
 = CALC {
     goal :: HsExp
   , calcs :: [(Justification,HsExp)]
   }
 deriving Show
\end{code}

\begin{code}
data Justification
 = BECAUSE {
     eqnName :: String -- fn definition or law, or ASSUMPTION
   , dir :: Maybe Direction
   , loc :: Maybe Location
   }
 deriving Show
\end{code}

\begin{code}
data Direction = L2R | R2L deriving (Eq, Show)
data Location
 = FNNAME String (Maybe Int)
 | VALUE HsExp (Maybe Int)
 deriving Show
\end{code}
\subsection{Parser}


\begin{code}
parseTheory :: ParseMode -> String -> ParseResult Theory
parseTheory pmode str
 = ParseFailed
     (SrcLoc (parseFilename pmode) 0 0)
     "Theory Parser not yet implemented"
-- ParseOk hsmod
\end{code}
