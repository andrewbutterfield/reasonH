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

Theory:
\begin{code}
data Theory = Theory deriving Show
\end{code}

Parse Theory:
\begin{code}
parseTheory :: String -> ParseResult Theory
parseTheory str = ParseOk Theory -- ParseFailed undefined "parseTheory NYI"

-- ParseFailed loc str
-- ParseOk hsmod
\end{code}
