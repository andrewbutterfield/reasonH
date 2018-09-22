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

\subsection{Datatypes}

Theory:
\begin{code}
data Theory = Theory deriving Show
\end{code}

\subsection{Structure}

Typically a keyword at the start of a line introduces something

\begin{verbatim}
THEORY <TheoryName>
IMPORT theory <Name>
IMPORT haskell <Name>
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

The following keywords are arranged to form larger structures
in which a theorem is stated and then proven.
\begin{verbatim}
THEOREM <name> <free-var1> ... <freevarN> <br?> <expr> === <expr>
STRATEGY <strategy>
LHS <br!> <calculation>
RHS <br!> <calculation>
BASE <var1> = <val1> .. <varN> = <valN> <br!> <expr> === <expr>
QED ( BASE | STEP | <name> )
STEP <var1> --> <expr1> .. <varN> --> <exprN>
ASSUME <br?> <expr> === <expr>
SHOW <br?> <expr> === <expr>
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

A calculation is:
\begin{verbatim}
<expr1>
 = <justification1>
 ...
 = <justificationN>
<exprN+1>
\end{verbatim}

A justification is one of the following:
\begin{verbatim}
 defn.<number>
 defn.<number> @ <location>
 <lawname> <direction>
 <lawname> <direction> @ <location>
 <theoryname>.assume <direction> <location>
\end{verbatim}

Locations:
\begin{verbatim}
<funname> <number>
<funname>.<arg-number> <number>
\end{verbatim}

Directions: \verb"l2r" and \verb"r2l"

A theorem has the following top-level structure:
\begin{verbatim}
THEOREM <name> <free-var1> ... <freevarN> <br?> <expr> === <expr>
STRATEGY <strategy>
...
QED <name>
\end{verbatim}

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
BASE <var1> = <val1> .. <varN> = <valN> <br!> <expr> === <expr>
<one of the four strategies above>
QED BASE
STEP <var1> --> <expr1> .. <varN> --> <exprN>
ASSUME <br?> <expr> === <expr>
SHOW <br?> <expr> === <expr>
<one of the four strategies above>
QED STEP
    \end{verbatim}
\end{description}


\subsection{Parser}

Parse Theory:
\begin{code}
parseTheory :: String -> ParseResult Theory
parseTheory str = ParseOk Theory -- ParseFailed undefined "parseTheory NYI"

-- ParseFailed loc str
-- ParseOk hsmod
\end{code}
