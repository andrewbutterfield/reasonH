\section{Theory}
\begin{verbatim}
Copyright  Andrew Buttefield (c) 2017--18

LICENSE: BSD3, see file LICENSE at reasonEq root
\end{verbatim}
\begin{code}
module Theory where

import Data.Char

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
IMPORT-THEORY <Name>
IMPORT-HASKELL <Name>
\end{verbatim}
These are followed by zero or more entries
that describe laws, induction schemes and theorems.

Laws are described by the following ``one-liner'' construct:
\begin{verbatim}
LAW <name> <br?> <expr>
\end{verbatim}
Here, \verb"<br?>" means that the following part
is either entirely on this line,
or else occupies a number of subsequent lines.
There can be a blank line before it,
and must be a blank line after it.
The following part itself must
not have blank lines embedded in it.

An induction-scheme is described by the following four lines:
\begin{verbatim}
INDUCTION-SCHEME <Type>
BASE <value>
STEP <var> --> <expr>
INJ  <br?> <expr>  ===  <expr>
\end{verbatim}

\newpage

A theorem has the following top-level structure:
\begin{verbatim}
THEOREM <name> <free-var1> ... <freevarN> <br?> <expr> === <expr>
STRATEGY <strategy>
...
QED <name>
\end{verbatim}

Strategies include:
\begin{verbatim}
reduce-all
reduce-lhs
reduce-rhs
reduce-both
induction <type1> <ind-var1> .. <typeN> <ind-varN>
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
BASE <var1> = <val1> .. <varN> = <valN> <br!> <expr>
<one of the four strategies above>
QED BASE
STEP <var1> --> <expr1> .. <varN> --> <exprN>
ASSUME <br?> <expr>
SHOW <br?> <expr>
<one of the four strategies above>
QED STEP
    \end{verbatim}
    Here, \verb"<br!>" is similar to \verb"<br?>",
    except that a line break at this point is mandatory.
\end{description}

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


\newpage
\subsection{Datatypes}

\begin{code}
data Theory
 = THEORY {
     theoryName  :: String
   , thImports   :: [String]  -- Theory Names
   , hkImports   :: [String]  -- Haskell Module names
   , thLaws      :: [Law]
   , thIndScheme :: [InductionScheme]
   , thTheorems  :: [Theorem]
   }
 deriving Show

thImports__ f thry = thry{ thImports = f $ thImports thry }
hkImports__ f thry = thry{ hkImports = f $ hkImports thry }
thLaws__    f thry = thry{ thLaws    = f $ thLaws    thry }
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
data InductionScheme
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

We start by adding in an ``empty'' theory as an accumulating
parameter,
breaking input into numbered lines
and starting the proper parsing.
\begin{code}
parseTheory :: ParseMode -> String -> ParseResult Theory
parseTheory pmode str = theoryParser pmode theory0 $ zip [1..] $ lines str

theory0 = THEORY { theoryName = "?", thImports = [], hkImports = []
                 , thLaws = [], thIndScheme = [], thTheorems = [] }
\end{code}

We start proper parsing by looking for \texttt{THEORY <TheoryName>}
on the first line:
\begin{code}
theoryParser :: ParseMode -> Theory -> [(Int,String)] -> ParseResult Theory
theoryParser pmode theory []
 = ParseFailed (SrcLoc (parseFilename pmode) 0 0) "Empty file"
theoryParser pmode theory ((lno,str):lns)
 | not gotKey    =  ParseFailed (SrcLoc (parseFilename pmode) lno 1)
                                "THEORY <TheoryName> expected"
 | otherwise     =  parseRest pmode theory' lns
 where
   (gotKey,keyedName) = parseKeyAndName "THEORY" str
   theory' = theory{theoryName = keyedName}
\end{code}


\begin{code}
parseRest pmode theory [] = ParseOk theory
parseRest pmode theory (ln@(lno,str):lns)
 | emptyLine str  =  parseRest pmode theory lns
 | gotImpTheory   =  parseRest pmode (thImports__ (++[thryName]) theory) lns
 | gotImpCode     =  parseRest pmode (hkImports__ (++[codeName]) theory) lns
 | gotLaw         =  parseLaw pmode theory lwName lno rest lns
 | otherwise      =  ParseFailed (SrcLoc (parseFilename pmode) lno 1)
                                   ("Unexpected keywords, etc.\n"++str)
 where
   (gotImpTheory, thryName) = parseKeyAndName "IMPORT-THEORY"  str
   (gotImpCode,   codeName) = parseKeyAndName "IMPORT-HASKELL" str
   (gotLaw, lwName, rest)   = parseOneLinerStart "LAW" str
\end{code}

\begin{code}
parseLaw pmode theory lwName lno rest lns
  = case parseExprChunk pmode lno rest lns of
      Nothing
        ->  ParseFailed (SrcLoc (parseFilename pmode) lno 1)
                                "Law expected"
      Just (expr, lns')
        ->  parseRest pmode (thLaws__ (++[LAW lwName expr]) theory) lns'
\end{code}

\begin{code}
parseExprChunk pmode lno rest lns
 | emptyLine rest  =  parseExpr pmode restlns chunk
 | otherwise       =  parseExpr pmode restlns [(lno,rest)]
 where (chunk,restlns) = getChunk lns
\end{code}

A chunk is zero or more empty lines,
followed by one or more non-empty lines,
followed by at least one empty line,
or the end of the list of lines.
\begin{code}
getChunk [] = ([],[])
getChunk (ln@(_,str):lns)
 | emptyLine str = getChunk lns
 | otherwise = getChunk' [ln] lns
getChunk' snl [] = (reverse snl, [])
getChunk' snl (ln@(_,str):lns)
 | emptyLine str = (reverse snl,lns)
 | otherwise     = getChunk' (ln:snl) lns
\end{code}

\begin{code}
parseExpr pmode restlns [] = Nothing
parseExpr pmode restlns chunk@((lno,_):_)
  = case parseModuleWithMode pmode (modstrf chunk) of
      ParseFailed _ _  -> Nothing
      ParseOk _ -> Just (HsLit (HsInt 42), restlns)
  where
    modstrf [(_,str)]
      = unlines [ "module NakedExpr where"
                , "nakedExpr = "++str ]
    modstrf chunk
      = unlines ( [ "module NakedExpr where"
                  , "nakedExpr = " ]
                  ++ map snd chunk )
\end{code}

\newpage
\subsubsection{``One-Liner'' Parsing}

\begin{code}
emptyLine = all isSpace
\end{code}

We return a boolean that is true if the parse suceeds.
\begin{code}
parseKeyAndName key str
  = case words str of
      [w1,w2] | w1 == key  ->  (True,  w2)
      _                    ->  (False, error "parseKeyAndName failed!")
\end{code}

\begin{code}
parseOneLinerStart key str
  = case words str of
      (w1:w2:rest) | w1 == key  ->  (True,  w2, unwords rest)
      _                         ->  ( False
                                    , error "parseOneLinerStart failed!"
                                    , str)
\end{code}
